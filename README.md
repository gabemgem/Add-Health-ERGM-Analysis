# Suicidal ideation and adolescent friendship networks

Online analysis supplement for **Populations to Social Networks: Identifying structural effects of suicidal ideation on adolescent friendship networks**. The manuscript is maintained separately from this repository.

This supplement contains the final directed ERGM and STERGM analyses, community descriptives, goodness-of-fit diagnostics, and exploratory pooling and sensitivity checks. Friendship nominations retain their direction: A → B means A nominated B. Model formulas are recorded in `config/`; the mapping to manuscript tables and figures is in [docs/analysis-map.md](docs/analysis-map.md).

## Reproduce the saved results

Use **R 4.3.1**, an internet connection for the initial package installation, and Pandoc (included with RStudio, or installed separately). Run these commands in a terminal **from this folder**, or open `AddHealthSupplement.Rproj` first:

```sh
Rscript scripts/setup.R
Rscript run_analysis.R --mode=reference
```

The setup script restores `renv.lock` into the ignored `.r-library/` directory. The package repository is frozen at 2024-03-01 and includes **ergm 4.6.0** and **tergm 4.2.0**, the versions stated in the manuscript. On systems without compatible binary packages, a compiler toolchain and system libraries required by those R packages are needed. `Rscript` must be on PATH; otherwise use its full installation path.

Reference mode verifies the saved result checksums and model definitions, then regenerates:

- `outputs/results_report.html`: community estimates, model diagnostics, descriptives, pooling and sensitivity analyses.
- `outputs/tables_paper/`: editable RTF, HTML and CSV tables for the illustrative communities.
- `outputs/figures_paper/`: PDF and PNG caterpillar plots.
- `outputs/sessionInfo_reference.txt`: the environment used for the build.

These commands need no licensed data. `reference_results/` contains the retained aggregate estimates and images from the 2026-10-26 result bundle; it is never overwritten by a reproduction run. Use `--no-report` to build just tables and figures when Pandoc is unavailable.

## Refit from licensed data

Obtain the restricted-use inputs independently; see [data/README.md](data/README.md). Copy `.Renviron.template` to `.Renviron` and set `RAW_DATA_ROOT`, using forward slashes on Windows. Then run:

```sh
Rscript run_analysis.R --mode=full
```

Full mode checks all six input files before starting, prepares the data, computes descriptives and directed-network structure, fits the selected ERGMs and STERGMs, pools estimates, creates a fresh report bundle, and builds tables, figures and the report from **the new fits**. Outputs go to `data/processed/`, `results/`, and `outputs/`; these are excluded from Git. It does not substitute saved reference estimates if a fit fails. Failed, degenerate and timed-out fits remain visible in the diagnostics.

Model fitting can take many hours or days. By default, one community is fitted at a time. Set `SUPPLEMENT_WORKERS` in `.Renviron` to increase concurrency if memory permits. Individual fits have time limits of six hours for ERGMs and five hours for STERGMs. Completed fits are reused only when their model definition, prepared data, settings, utility code, R version and relevant package versions match. Rerunning full mode rebuilds preparation and descriptives, then resumes compatible model fits. For a forced refit, set `force_rerun <- TRUE` in the applicable fitting script.

## Checks

```sh
Rscript run_analysis.R --mode=check
Rscript scripts/test_pipeline.R
```

The first checks dependencies, reference checksums and formulas. The second exercises preprocessing, directed-network construction, shared-club matrices, disclosure rules, cache invalidation, saved table reproduction, and small synthetic model fits. GitHub Actions runs these checks and rebuilds the saved-result supplement without licensed inputs.

## Reproducibility status

The original fitting machine's lockfile and session information were not retained. The new lockfile defines a repeatable reconstruction environment, **not a recovered record of that original environment**. Saved coefficients allow the paper's tables and coefficient plots to be rebuilt without refitting. The licensed inputs are not present here, so a full raw-data reproduction and numerical agreement with the manuscript cannot be certified from this folder alone. See [docs/reproducibility.md](docs/reproducibility.md) for checks performed, preparation fixes, and remaining limitations.

## Files and publication

The numbered R scripts implement the analysis; `utils/` holds shared code; `config/` records formulas and display labels; `reference_results/` stores selected existing results. `run_analysis.R` is the supported entry point. The Git ignore rules exclude licensed data, network/model objects, environment paths, local libraries and generated outputs. Older analyses and redundant outputs are outside the publication files; locally retained cleanup materials are ignored under `.local_cleanup/`.

## License

The analysis code is licensed under the GNU General Public License, version 3. See [LICENSE](LICENSE) for the full terms.

Add Health inputs remain subject to their own access terms; the code license does not grant access to or permission to redistribute those data. The retained aggregate exports and network images have not received a new institutional disclosure review during this cleanup.

# Add Health Acknowledgement

Data collection and dissemination for Wave VI of Add Health was funded by cooperative agreements U01 AG071448 (PI: Robert A. Hummer) and U01 AG071450 (MPIs: Robert A. Hummer and Allison E. Aiello) from the National Institute on Aging to the University of North Carolina at Chapel Hill, with cooperative funding from five other institutes and offices at the National Institutes of Health: the Eunice Kennedy Shriver National Institute of Child Health and Human Development, the National Institute on Minority Health and Health Disparities, the National Institute on Drug Abuse, the Office of Disease Prevention, and the Office of Behavioral and Social Science Research. Waves I-V of Add Health were funded by grant P01 HD31921 from the Eunice Kennedy Shriver National Institute of Child Health and Human Development, with cooperative funding from 23 other federal agencies and foundations. Add Health is currently directed by Robert A. Hummer at the University of North Carolina at Chapel Hill. Add Health was designed by J. Richard Udry, Peter S. Bearman, and Kathleen Mullan Harris at the University of North Carolina at Chapel Hill. No direct funds from U01 AG071448, U01 AG071450, or P01 HD31921 were used in producing this research. Any viewpoints or perspectives expressed in this research are the author’s and do not necessarily represent those of the institutes or agencies that funded Add Health data collection or those of the University of North Carolina at Chapel Hill. The restricted-use Add Health data analyzed in this study were accessed and used under Add Health Restricted-Use Data Contract No. 120925002. 
