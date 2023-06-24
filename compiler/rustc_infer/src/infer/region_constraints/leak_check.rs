use super::*;
use crate::infer::CombinedSnapshot;
use rustc_data_structures::{
    fx::FxIndexMap,
    graph::{scc::Sccs, vec_graph::VecGraph},
};
use rustc_index::Idx;
use rustc_middle::ty::error::TypeError;
use rustc_middle::ty::relate::RelateResult;

impl<'tcx> RegionConstraintCollector<'_, 'tcx> {
    /// Searches new universes created during `snapshot`, looking for
    /// placeholders that may "leak" out from the universes they are contained
    /// in. If any leaking placeholders are found, then an `Err` is returned
    /// (typically leading to the snapshot being reversed). This algorithm
    /// only looks at placeholders which cannot be named by `outer_universe`,
    /// as this is the universe we're currently checking for a leak.
    ///
    /// The leak check *used* to be the only way we had to handle higher-ranked
    /// obligations. Now that we have integrated universes into the region
    /// solvers, this is no longer the case, but we retain the leak check for
    /// backwards compatibility purposes. In particular, it lets us make "early"
    /// decisions about whether a region error will be reported that are used in
    /// coherence and elsewhere -- see #56105 and #59490 for more details. The
    /// eventual fate of the leak checker is not yet settled.
    ///
    /// The leak checker works by searching for the following error pattern:
    ///
    /// * P: R, where R is in some universe that cannot name P
    ///
    /// The idea here is that each of these patterns represents something that
    /// the region solver would eventually report as an error, so we can detect
    /// the error early. There is a fly in the ointment, though, in that this is
    /// not entirely true. In particular, in the future, we may extend the
    /// environment with implied bounds or other info about how placeholders
    /// relate to regions in outer universes. In that case, `P1: R` for example
    /// might become solvable.
    ///
    /// # Summary of the implementation
    ///
    /// The leak checks as follows. First, we construct a graph where `R2: R1`
    /// implies `R2 -> R1`, and we compute the SCCs.
    ///
    /// For each SCC S, we compute:
    ///
    /// * what placeholder P it must be equal to, if any
    ///   * if there are multiple placeholders that must be equal, we pick the one with the higher
    ///     universe. It will eventually be an error in the next step if the placeholders are in
    ///     different universes.
    /// * the minimum universe of its constituents
    ///
    /// Then we walk the SCCs in dependency order and compute
    ///
    /// * minimum universe U of all SCCs they must outlive
    ///   * if the SCC must also be equal to a placeholder P, and U cannot name P, report an error,
    ///     as that indicates `P: R` and `R` is in a universe that cannot name P
    ///
    /// To improve performance and for the old trait solver caching to be sound, this takes
    /// an optional snapshot in which case we only look at region constraints added in that
    /// snapshot. If we were to not do that the `leak_check` during evaluation can rely on
    /// region constraints added outside of that evaluation. As that is not reflected in the
    /// cache key this would be unsound.
    ///
    /// # Historical note
    ///
    /// Older variants of the leak check used to report errors for these
    /// patterns, but we no longer do:
    ///
    /// * R: P1, even if R cannot name P1, because R = 'static is a valid sol'n
    /// * R: P1, R: P2, as above
    /// * P1: P2, when P2 lives in a universe that *can* name P1.
    #[instrument(level = "debug", skip(self, tcx, only_consider_snapshot), ret)]
    pub fn leak_check(
        &mut self,
        tcx: TyCtxt<'tcx>,
        outer_universe: ty::UniverseIndex,
        max_universe: ty::UniverseIndex,
        only_consider_snapshot: Option<&CombinedSnapshot<'tcx>>,
    ) -> RelateResult<'tcx, ()> {
        if outer_universe == max_universe {
            return Ok(());
        }

        let mini_graph = &MiniGraph::new(tcx, &self, only_consider_snapshot);

        let mut leak_check = LeakCheck::new(mini_graph, self);
        leak_check.assign_placeholder_values();
        leak_check.propagate_scc_value()
    }
}

struct LeakCheck<'me, 'tcx> {
    mini_graph: &'me MiniGraph<'tcx>,
    rcc: &'me RegionConstraintCollector<'me, 'tcx>,

    // Initially, for each SCC S, stores a placeholder `P` such that `S = P`
    // must hold.
    scc_placeholders: IndexVec<LeakCheckScc, Option<ty::PlaceholderRegion>>,

    // For each SCC S, track the minimum universe that flows into it. Note that
    // this is both the minimum of the universes for every region that is a
    // member of the SCC, but also if you have `R1: R2`, then the universe of
    // `R2` must be less than the universe of `R1` (i.e., `R1` flows `R2`). To
    // see that, imagine that you have `P1: R` -- in that case, `R` must be
    // either the placeholder `P1` or the empty region in that same universe.
    //
    // To detect errors, we look for an SCC S where the values in
    // `scc_values[S]` (if any) cannot be stored into `scc_universes[S]`.
    scc_universes: IndexVec<LeakCheckScc, SccUniverse<'tcx>>,
}

impl<'me, 'tcx> LeakCheck<'me, 'tcx> {
    fn new(
        mini_graph: &'me MiniGraph<'tcx>,
        rcc: &'me RegionConstraintCollector<'me, 'tcx>,
    ) -> Self {
        let dummy_scc_universe = SccUniverse { universe: ty::UniverseIndex::MAX, region: None };
        Self {
            mini_graph,
            rcc,
            scc_placeholders: IndexVec::from_elem_n(None, mini_graph.sccs.num_sccs()),
            scc_universes: IndexVec::from_elem_n(dummy_scc_universe, mini_graph.sccs.num_sccs()),
        }
    }

    /// Compute what placeholders (if any) each SCC must be equal to.
    /// Also compute the minimum universe of all the regions in each SCC.
    fn assign_placeholder_values(&mut self) {
        // First walk: find each placeholder that is from a newly created universe.
        for (region, leak_check_node) in &self.mini_graph.nodes {
            let scc = self.mini_graph.sccs.scc(*leak_check_node);

            // Set the universe of each SCC to be the minimum of its constituent universes
            let universe = self.rcc.universe(*region);
            debug!(
                "assign_placeholder_values: scc={:?} universe={:?} region={:?}",
                scc, universe, region
            );
            self.scc_universes[scc].take_min(universe, *region);

            // Detect those SCCs that directly contain a placeholder
            if let ty::RePlaceholder(p1) = **region {
                let max_placeholder = match self.scc_placeholders[scc] {
                    Some(p2) => std::cmp::max_by_key(p1, p2, |p| p.universe),
                    None => p1,
                };
                self.scc_placeholders[scc] = Some(max_placeholder);
            }
        }
    }

    /// For each SCC S, iterate over each successor S1 where `S: S1`:
    ///
    /// * Compute
    /// Iterate over each SCC `S` and ensure that, for each `S1` where `S1: S`,
    /// `universe(S) <= universe(S1)`. This executes after
    /// `assign_placeholder_values`, so `universe(S)` is already the minimum
    /// universe of any of its direct constituents.
    fn propagate_scc_value(&mut self) -> RelateResult<'tcx, ()> {
        // Loop invariants:
        //
        // On start of the loop iteration for `scc1`:
        //
        // * `scc_universes[scc1]` contains the minimum universe of the
        //   constituents of `scc1`
        // * `scc_placeholder[scc1]` stores the placeholder that `scc1` must
        //   be equal to (if any)
        //
        // For each successor `scc2` where `scc1: scc2`:
        //
        // * `scc_universes[scc2]` contains the minimum universe of the
        //   constituents of `scc2` and any of its successors
        for scc1 in self.mini_graph.sccs.all_sccs() {
            debug!(
                "propagate_scc_value: scc={:?} with universe {:?}",
                scc1, self.scc_universes[scc1]
            );

            // Walk over each `scc2` such that `scc1: scc2` and compute:
            //
            // * `scc1_universe`: the minimum universe of `scc2` and the constituents of `scc1`
            let mut scc1_universe = self.scc_universes[scc1];
            for &scc2 in self.mini_graph.sccs.successors(scc1) {
                let SccUniverse { universe: scc2_universe, region: scc2_region } =
                    self.scc_universes[scc2];

                scc1_universe.take_min(scc2_universe, scc2_region.unwrap());
            }

            // Update minimum universe of scc1.
            self.scc_universes[scc1] = scc1_universe;

            // At this point, `scc_placeholders[scc1]` stores the placeholder that
            // `scc1` must be equal to, if any.
            if let Some(scc1_placeholder) = self.scc_placeholders[scc1] {
                debug!(
                    "propagate_scc_value: scc1={:?} placeholder={:?} scc1_universe={:?}",
                    scc1, scc1_placeholder, scc1_universe
                );

                // Check if `P1: R` for some `R` in a universe that cannot name
                // P1. That's an error.
                if scc1_universe.universe.cannot_name(scc1_placeholder.universe) {
                    return Err(self.error(scc1_placeholder, scc1_universe.region.unwrap()));
                }
            }
        }
        Ok(())
    }

    fn error(
        &self,
        placeholder: ty::PlaceholderRegion,
        other_region: ty::Region<'tcx>,
    ) -> TypeError<'tcx> {
        debug!("error: placeholder={:?}, other_region={:?}", placeholder, other_region);
        TypeError::RegionsInsufficientlyPolymorphic(placeholder.bound.kind, other_region)
    }
}

// States we need to distinguish:
//
// * must be equal to a placeholder (i.e., a placeholder is in the SCC)
//     * it could conflict with some other regions in the SCC in different universes
//     * or a different placeholder
// * `P1: S` and `S` must be equal to a placeholder
// * `P1: S` and `S` is in an incompatible universe
//
// So if we
//
// (a) compute which placeholder (if any) each SCC must be equal to
// (b) compute its minimum universe
// (c) compute *some* placeholder where `S: P1` (any one will do)
//
// then we get an error if:
//
// - it must be equal to a placeholder `P1` and minimum universe cannot name `P1`
// - `S: P1` and minimum universe cannot name `P1`
// - `S: P1` and we must be equal to `P2`
//
// So we want to track:
//
// * Equal placeholder (if any)
// * Some bounding placeholder (if any)
// * Minimum universe
//
// * We compute equal placeholder + minimum universe of constituents in first pass
// * Then we walk in order and compute from our dependencies `S1` where `S: S1` (`S -> S1`)
//   * bounding placeholder (if any)
//   * minimum universe
// * And if we must be equal to a placeholder then we check it against
//   * minimum universe
//   * no bounding placeholder

/// Tracks the "minimum universe" for each SCC, along with some region that
/// caused it to change.
#[derive(Copy, Clone, Debug)]
struct SccUniverse<'tcx> {
    /// For some SCC S, the minimum universe of:
    ///
    /// * each region R in S
    /// * each SCC S1 such that S: S1
    universe: ty::UniverseIndex,

    /// Some region that caused `universe` to be what it is.
    region: Option<ty::Region<'tcx>>,
}

impl<'tcx> SccUniverse<'tcx> {
    /// If `universe` is less than our current universe, then update
    /// `self.universe` and `self.region`.
    fn take_min(&mut self, universe: ty::UniverseIndex, region: ty::Region<'tcx>) {
        if universe < self.universe || self.region.is_none() {
            self.universe = universe;
            self.region = Some(region);
        }
    }
}

rustc_index::newtype_index! {
    #[debug_format = "LeakCheckNode({})"]
    struct LeakCheckNode {}
}

rustc_index::newtype_index! {
    #[debug_format = "LeakCheckScc({})"]
    struct LeakCheckScc {}
}

/// Represents the graph of constraints. For each `R1: R2` constraint we create
/// an edge `R1 -> R2` in the graph.
struct MiniGraph<'tcx> {
    /// Map from a region to the index of the node in the graph.
    nodes: FxIndexMap<ty::Region<'tcx>, LeakCheckNode>,

    /// Map from node index to SCC, and stores the successors of each SCC. All
    /// the regions in the same SCC are equal to one another, and if `S1 -> S2`,
    /// then `S1: S2`.
    sccs: Sccs<LeakCheckNode, LeakCheckScc>,
}

impl<'tcx> MiniGraph<'tcx> {
    fn new(
        tcx: TyCtxt<'tcx>,
        region_constraints: &RegionConstraintCollector<'_, 'tcx>,
        only_consider_snapshot: Option<&CombinedSnapshot<'tcx>>,
    ) -> Self {
        let mut nodes = FxIndexMap::default();
        let mut edges = Vec::new();

        // Note that if `R2: R1`, we get a callback `r1, r2`, so `target` is first parameter.
        Self::iterate_region_constraints(
            tcx,
            region_constraints,
            only_consider_snapshot,
            |target, source| {
                let source_node = Self::add_node(&mut nodes, source);
                let target_node = Self::add_node(&mut nodes, target);
                edges.push((source_node, target_node));
            },
        );
        let graph = VecGraph::new(nodes.len(), edges);
        let sccs = Sccs::new(&graph);
        Self { nodes, sccs }
    }

    /// Invokes `each_edge(R1, R2)` for each edge where `R2: R1`
    fn iterate_region_constraints(
        tcx: TyCtxt<'tcx>,
        region_constraints: &RegionConstraintCollector<'_, 'tcx>,
        only_consider_snapshot: Option<&CombinedSnapshot<'tcx>>,
        mut each_edge: impl FnMut(ty::Region<'tcx>, ty::Region<'tcx>),
    ) {
        let mut each_constraint = |constraint| match constraint {
            &Constraint::VarSubVar(a, b) => {
                each_edge(ty::Region::new_var(tcx, a), ty::Region::new_var(tcx, b));
            }
            &Constraint::RegSubVar(a, b) => {
                each_edge(a, ty::Region::new_var(tcx, b));
            }
            &Constraint::VarSubReg(a, b) => {
                each_edge(ty::Region::new_var(tcx, a), b);
            }
            &Constraint::RegSubReg(a, b) => {
                each_edge(a, b);
            }
        };

        if let Some(snapshot) = only_consider_snapshot {
            for undo_entry in
                region_constraints.undo_log.region_constraints_in_snapshot(&snapshot.undo_snapshot)
            {
                match undo_entry {
                    AddConstraint(constraint) => {
                        each_constraint(constraint);
                    }
                    &AddVerify(i) => span_bug!(
                        region_constraints.data().verifys[i].origin.span(),
                        "we never add verifications while doing higher-ranked things",
                    ),
                    &AddCombination(..) | &AddVar(..) => {}
                }
            }
        } else {
            for (constraint, _origin) in &region_constraints.data().constraints {
                each_constraint(constraint)
            }
        }
    }

    fn add_node(
        nodes: &mut FxIndexMap<ty::Region<'tcx>, LeakCheckNode>,
        r: ty::Region<'tcx>,
    ) -> LeakCheckNode {
        let l = nodes.len();
        *nodes.entry(r).or_insert(LeakCheckNode::new(l))
    }
}
