# Manuscript analysis map

This map connects the supplement to **Populations to Social Networks: Identifying structural effects of suicidal ideation on adolescent friendship networks**. The manuscript is maintained separately. Analysis selection is based on its methods, captions, and the existing paper-table manifest.

| Manuscript result | Configuration / source | Reproduction output |
| --- | --- | --- |
| Table 1 and appendix descriptives | `01_data_prep.R`, `02_descriptives.R`; saved `all_communities_descriptives_network_directed_full.csv` | Descriptive tables in the results report |
| Figure 1, community 408 network | `02_descriptives.R`, `utils/plot_helpers.R` | Saved `reference_results/community_408_network_w1.png`; regenerated in full mode |
| Tables 2–5, ERGM Models 1–3 | `m14a`, `m14c`, `m14d`, respectively; communities 406, 408, 428, 477 | `outputs/tables_paper/ergm_ergm_community_<id>.*` |
| Figure 2a–c, fully adjusted ERGM | `m14d`, community ordering | `outputs/figures_paper/ergm_m14d_by_id_part*.{pdf,png}` |
| Figure 3a–c, final formation model | `m7f4d3`, formation, community ordering | `outputs/figures_paper/stergm_m7f4d3_formation_by_id_part*.{pdf,png}` |
| Figure 4a–b, final persistence model | `m7f3d3`, dissolution component (persistence parameterization) | `outputs/figures_paper/stergm_m7f3d3_dissolution_by_id_part*.{pdf,png}` |
| Supplemental formation/persistence sequence | `m7f1d1`, `m7f2d2`, `m7f3d3`, `m7f4d3` | Results report and STERGM community tables |
| Goodness of fit and estimation diagnostics | Selected-model `*_fit_info.csv`, `*_gof.csv`, `*_coef_corr.csv`, saved GOF PNGs | Results report |
| Exploratory pooled estimates and sensitivity checks | `utils/meta_trust.R`, selected-model coefficients and community covariates | Results report; new full-run `*_meta*.csv`, `*_selection.csv` |

`m7f4d3` adds squared CES-D sender and receiver terms to formation; its persistence formula is identical to `m7f3d3`. Both saved model IDs are retained so the existing fit diagnostics and manuscript results remain traceable. Positive persistence coefficients mean a tie is more likely to persist, despite the historical `dissolution` label in filenames.

The unused ERGM structural-only intermediate `m14b`, integrated-model experiment, old directed/undirected model scripts, influence models, RSiena models, mortality models, old sensitivity scripts, duplicated workbooks/text bundles, review copies and overlapping generated figure sets are outside the publication workflow. Historical `m14b` rows and images were removed from the retained reference bundle; coefficients for the selected model IDs were not recalculated during cleanup.

The RTF tables and figure exports reproduce statistical content; they do not rewrite or automatically assemble the Word manuscript. The appendix's manual panel layout and any hand edits in Word must be assessed separately from numeric reproduction.
