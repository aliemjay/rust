use rustc_hir::{def::DefKind, def_id::DefId};
use rustc_middle::ty::{self, Ty, TyCtxt, TypeFoldable, TypeVisitableExt as _};
use rustc_span::DUMMY_SP;

pub fn provide(providers: &mut ty::query::Providers) {
    *providers = ty::query::Providers { assumed_wf_types, ..*providers };
}

fn assumed_wf_types<'tcx>(tcx: TyCtxt<'tcx>, def_id: DefId) -> &'tcx ty::List<Ty<'tcx>> {
    match tcx.def_kind(def_id) {
        DefKind::Fn => tcx.mk_type_list_from_iter(self::from_fn_sig(tcx, def_id)),
        DefKind::AssocFn => {
            let from_impl = tcx.assumed_wf_types(tcx.parent(def_id));
            let from_sig = self::from_fn_sig(tcx, def_id);
            tcx.mk_type_list_from_iter(from_impl.into_iter().chain(from_sig))
        }
        DefKind::Impl { .. } => {
            let unnormalized = match tcx.impl_trait_ref(def_id) {
                Some(trait_ref) => {
                    tcx.mk_type_list_from_iter(trait_ref.subst_identity().substs.types())
                }
                // Only the impl self type
                None => tcx.mk_type_list(&[tcx.type_of(def_id).subst_identity()]),
            };

            // FIXME(@lcnr): rustc currently does not check wf for types
            // pre-normalization, meaning that implied bounds from unnormalized types
            // are sometimes incorrect. See #100910 for more details.
            //
            // Not adding the unnormalized types here mostly fixes that, except
            // that there are projections which are still ambiguous in the item definition
            // but do normalize successfully when using the item, see #98543.
            self::normalize(tcx, tcx.param_env(def_id), unnormalized)
        }
        DefKind::AssocConst | DefKind::AssocTy => tcx.assumed_wf_types(tcx.parent(def_id)),
        DefKind::OpaqueTy => match tcx.def_kind(tcx.parent(def_id)) {
            DefKind::TyAlias => ty::List::empty(),
            DefKind::AssocTy => tcx.assumed_wf_types(tcx.parent(def_id)),
            // Nested opaque types only occur in associated types:
            // ` type Opaque<T> = impl Trait<&'static T, AssocTy = impl Nested>; `
            // assumed_wf_types should include those of `Opaque<T>`, `Opaque<T>` itself
            // and `&'static T`.
            DefKind::OpaqueTy => bug!("unimplemented implied bounds for neseted opaque types"),
            def_kind @ _ => {
                bug!("unimplemented implied bounds for opaque types with parent {def_kind:?}")
            }
        },
        DefKind::Mod
        | DefKind::Struct
        | DefKind::Union
        | DefKind::Enum
        | DefKind::Variant
        | DefKind::Trait
        | DefKind::TyAlias
        | DefKind::ForeignTy
        | DefKind::TraitAlias
        | DefKind::TyParam
        | DefKind::Const
        | DefKind::ConstParam
        | DefKind::Static(_)
        | DefKind::Ctor(_, _)
        | DefKind::Macro(_)
        | DefKind::ExternCrate
        | DefKind::Use
        | DefKind::ForeignMod
        | DefKind::AnonConst
        | DefKind::InlineConst
        | DefKind::ImplTraitPlaceholder
        | DefKind::Field
        | DefKind::LifetimeParam
        | DefKind::GlobalAsm
        | DefKind::Closure
        | DefKind::Generator => ty::List::empty(),
    }
}

fn from_fn_sig<'tcx>(tcx: TyCtxt<'tcx>, def_id: DefId) -> impl Iterator<Item = Ty<'tcx>> {
    let unnormalized = tcx.fn_sig(def_id).subst_identity().inputs_and_output();
    let unnormalized = tcx.liberate_late_bound_regions(def_id, unnormalized);
    let normalized = self::normalize(tcx, tcx.param_env(def_id), unnormalized);

    // FIXME(#105948): Use unnormalized types for implied bounds as well.
    std::iter::zip(normalized, unnormalized)
        .flat_map(|(ty1, ty2)| match ty1 == ty2 {
            true => [Some(ty1), None],
            false => [Some(ty1), Some(ty2)],
        })
        .flatten()
}

fn normalize<'tcx, T>(tcx: TyCtxt<'tcx>, param_env: ty::ParamEnv<'tcx>, value: T) -> T
where
    T: TypeFoldable<TyCtxt<'tcx>> + Copy,
{
    use rustc_infer::infer::resolve::OpportunisticRegionResolver;
    use rustc_infer::infer::TyCtxtInferExt as _;
    use rustc_trait_selection::traits::{ObligationCause, ObligationCtxt};

    let infcx = tcx.infer_ctxt().build();
    let ocx = ObligationCtxt::new(&infcx);
    let result = ocx.normalize(&ObligationCause::dummy(), param_env, value);
    if !ocx.select_all_or_error().is_empty() {
        tcx.sess.delay_span_bug(DUMMY_SP, format!("can't normalize {value:?}"));
        return value;
    }
    let result = infcx.resolve_vars_if_possible(result);
    let result = result.fold_with(&mut OpportunisticRegionResolver::new(&infcx));
    if result.has_infer() {
        tcx.sess.delay_span_bug(DUMMY_SP, format!("can't normalize {value:?}"));
        return value;
    }
    result
}
