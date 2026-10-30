# Reproducibility record

## Environment and provenance

The reference estimates came from `report_bundle_2026_10_26/`, renamed `reference_results/`. The retained `*_models_run.csv` files record their fit dates and formulas. `checksums.csv` records byte-level MD5 digests after selecting the manuscript analyses; it detects later accidental changes, not scientific correctness. Git preserves reference-file bytes across platforms.

The original fitting environment was not recorded. The reconstruction environment uses R 4.3.1 and the 2024-03-01 package snapshot, with ergm 4.6.0 and tergm 4.2.0 as reported in the manuscript. `renv.lock` pins the complete installed dependency set. New runs write session information. Exact stochastic agreement across operating systems, compiler versions, BLAS implementations, or time-limited jobs is not guaranteed by fixed seeds alone.

## Changes made during supplement preparation

- Restricted the published workflow to the manuscript's selected ERGMs, temporal sequence, descriptives, diagnostics, pooling and sensitivity checks.
- Added a single runner, package restoration, input validation, reference checksums, an analysis map and automated verification.
- Anchored `here` to this supplement rather than the enclosing AddHealth Git repository. Removed an extra `R/` prefix from the network-structure helper path.
- Restored explicit `paper_name` selections so the three manuscript ERGM columns are actually exported. Temporal model tables also export the retained sequence.
- Removed the unnecessary dependency on a private `deaths.RData` file and its unrelated descriptive column.
- Joined all 33 in-school club items before computing club counts; the previous slim join only carried age and the no-club item. This fixes missing-column failures when those club items are absent from the in-home file. The model's binary shared-club definition is unchanged.
- Generated network diagrams by default in a common figure directory so a fresh full run includes the diagrams promised in the manuscript.
- Included data, code, settings, R and relevant package versions in cache fingerprints; missing inputs fail explicitly. Old coefficient files are cleared when a community is refitted so a failed fit cannot silently retain old estimates.
- Rejected ambiguous result directories containing duplicate CSV basenames. The report, tables and figures read one explicitly selected source.
- Added a reader mapping for the presentation headings in the archived descriptive CSV and support for its mixed UTF-8/Windows-1252 exports. The saved reference files remain byte-for-byte unchanged by the reader. Corrected the `rase_asian_pi` column spelling for new descriptive exports.

## Verification performed

- Restored the project library against `renv.lock`; the installed environment is synchronized.
- Verified the reference checksums and exact configured formula matches for all retained model IDs.
- Rebuilt the paper tables and compared all four ERGM table CSVs with their pre-cleanup exports; their formatted cells match.
- Ran artificial XPT inputs through preparation, both descriptive stages, and ERGM/STERGM workers including goodness of fit. Checked directed arcs, club overlap matrices, suppression boundaries, persistence labels, and cache changes when settings change. These checks use artificial people only.
- Rendered the complete saved-result HTML report successfully with Pandoc, including community diagnostics and coefficient figures.

Generated reference exports are written under `outputs/`; validation does not overwrite `reference_results/`.

Preparation fixes can change a new fit if the original analysis machine used different preparation code or releases. Existing saved coefficients are retained as reference evidence; matching those coefficients requires a licensed-data comparison, not an assumption that the fixes are numerically neutral.

## Limits of verification

The licensed XPT files and original fitted model objects are not in this folder. A raw-data rebuild, MCMC chain inspection for the original fits, and confirmation of every manuscript narrative statistic therefore remain unverified. The new workflow must be run on an authorized analysis machine before claiming complete raw-to-paper reproduction. The report preserves the existing fit-status flags and uncertainty rather than interpreting all saved coefficients as trustworthy fits.

The existing disclosure suppression logic is retained for community descriptives; reproducing an export is not a new determination of permission to release restricted-data derivatives.
