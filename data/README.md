# Licensed inputs

Raw and respondent-level data are not included. Full reproduction requires authorized access to the same Add Health restricted-use releases used by the author. Set `RAW_DATA_ROOT` to a directory containing these case-sensitive relative paths:

```text
Standard/Survey/W1/allwave1.xpt
Standard/Survey/W2/wave2.xpt
Standard/InSchool/Inschool.xpt
Standard/InSchool/Schinfo.xpt
Standard/Friendship/hfriend1.xpt
Standard/Friendship/hfriend2.xpt
```

The original input release identifiers and checksums were not retained. A new full run records its input checksums in `data/processed/input_checksums.csv`. They identify that run's files, rather than proving that the original files were identical.

`01_data_prep.R` uses `SAT_SCHL == 1` in school information to select saturated schools and uses `COMMID` as the community ID. It joins respondents by `AID`, constructs survey composites, restricts nominations to the community's retained Wave I respondents, and saves directed network and covariate RDS files in `data/processed/`.

Both wave networks retain the same baseline vertex set. `in_wave2` is defined by a nonmissing `H2SU1`; the structure summaries restrict Wave II to this observed subset, while the temporal fits use the prepared baseline vertex set. This is the existing analysis convention, not a new attrition correction. Consult the scripts when interpreting Wave II summaries.

CES-D sums 19 items after reversing items 4, 8, 11 and 15, and requires all items. Social support averages eight items after reversing item 6, and requires all items. Model fitting imputes missing binary attributes as zero, numeric attributes by community mean, and categorical attributes by mode when possible; uninformative terms are dropped and logged. Shared clubs are coded as at least one common activity across S44A1–S44A33; missing club responses are treated as zero at fit time. The same baseline covariates are used in both temporal components.

No death-record file is required. Mortality and influence analyses are outside the manuscript's final analysis and were removed from the publication workflow. Unsuppressed intermediate descriptives remain under the ignored processed-data directory. Do not commit raw files, prepared networks, respondent attributes, or model objects containing data.
