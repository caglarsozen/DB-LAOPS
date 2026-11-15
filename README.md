# DB-LAOPS

Reproducible R implementation accompanying **Drawdown-Budgeted Learning-Augmented Online Portfolio Selection under Market Regime Shifts and Transaction Costs**.

Release: **v9.0.1**, 15 November 2026. This publication cleanup retains all active numerical function bodies, formals and parameter values from the computed v9.0 revision. Seven superseded helper definitions were removed and figure labels/fonts were improved. The manuscript contains the full method specification and parameters in its appendices.

## Run

Use R 4.4 or later. From this repository's root:

```r
install.packages(c("quantmod", "xts", "zoo", "quadprog", "lpSolve"),
                 repos = "https://cloud.r-project.org")
source("DB_LAOPS_v90_MAJOR_REVISION.R")
result <- db_main("full", root = getwd(), download = FALSE)
```

The `data/source_cache` directory contains the frozen inputs used for the archived results. With `download = FALSE`, missing data stop execution. Setting `download = TRUE` downloads missing inputs only; existing cached observations are retained. No real-data run substitutes synthetic data after a retrieval failure.

- `full`: two asset universes; 0/5/10/25 bp costs; 4,999 bootstrap draws; block lengths 5/20/60; annual past-only budget selection; extra execution delay; component comparisons.
- `review`: shorter real-data check with 999 draws and 10 bp costs. Its p-value resolution is insufficient for some 72-test Holm thresholds.
- `smoke`: synthetic software validation only. These observations cannot be reported as empirical market results.

Full replication can be lengthy. Completed engine scenarios are cached using the input, settings and source checksums. A changed source file invalidates the previous cache. The final output directory is `outputs_full`; completion is indicated by its `RUN_STATUS.txt`.

```r
source("tests/test_revision.R")
```

## Archived results and scope

`results/` contains the frozen result tables, software-validation records, input checksums and session information. Simulation covers 5 January 2016 through 31 December 2025. The main retrospective evaluation covers 2 January 2020 through 31 December 2025 (1,508 daily observations). The older design had already examined this historical period, so this is not an untouched holdout.

The corrected evidence does **not** establish general superiority. At the baseline thresholds, CVaR screening makes the separate drawdown screen redundant throughout the evaluation; the contextual-advice CE5 effect is economically negligible. Cost-aware mean-variance remains competitive. Historical risk budgets do not guarantee future drawdown limits. The manuscript reports adverse and inconclusive comparisons.

The archived financial calculations used the v9.0 functions. The v9.0.1 source cleanup was checked for identical active numerical definitions, passed the 20 mechanical checks again, and was exercised through its synthetic end-to-end workflow. It did not generate a new market experiment or change the reported estimates.

## Data and software license

The software is licensed under the MIT License in `LICENSE`. The license does not grant additional rights to Yahoo Finance, FRED or other third-party market data; those inputs retain the providers' terms and attribution. Retrieval metadata are in `data/source_cache/download_manifest.json`. CSV line endings are normalized to LF for the public repository, with all numerical observations unchanged. `results/input_checksums.csv` matches these public bytes; `input_checksums_v90_original.csv` preserves the hashes of the original experiment cache.

The authors are Mervenur Sözen, Onur Şeyranlıoğlu and Çağlar Sözen. This repository is a research reproduction resource, not a guarantee of future investment performance.
