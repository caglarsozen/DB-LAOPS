# DB-LAOPS

Reproducible R code for **Drawdown-Budgeted Learning-Augmented Online Portfolio Selection: Component Activity and Economic Equivalence under Transaction Costs**.

Revision package **v9.1**, 16 November 2026. The allocation engine remains v9.0.1 with unchanged numerical rules. This revision adds component-activity diagnostics, the reduction protocol and CE5 equivalence study.

## Run the component study

From the repository root, with R 4.4 or later:

    install.packages(c("quantmod", "xts", "zoo", "quadprog", "lpSolve"))
    source("RUN_COMPONENT_STUDY.R")

The self-contained component_study folder also runs with Rscript component_study/RUN_CONTRIBUTION.R. Two R workers are used by default; set Sys.setenv(DB_LAOPS_CORES=1) to use one. Frozen market data are included. Full mode computes twelve core scenarios (two universes, three costs 0/10/25 bp and contextual/uniform probabilities), then replays the reduced selectors and runs the diagnostics, inference and output checks.

R1 removes drawdown screening/penalty and equally weights the selected leading candidates. R2 additionally replaces contextual expert probabilities by uniform probabilities. CVaR screening, other online rules, stress and trading controls remain. The primary annual CE5 equivalence margin is +/-25 bp; +/-10 and +/-50 bp are fixed sensitivities. All twelve comparisons meet the primary and 10-bp sensitivity margins using the reported twelve-comparison-adjusted intervals. This is CE5 equivalence to the full rule, not superiority to external portfolios or equivalence of every risk measure.

The component_study/results folder contains completed summary results. Core caches and large daily result paths are regenerated locally rather than stored in Git. If daily paths already exist from a full run, set Sys.setenv(DB_LAOPS_VERIFY_ONLY="TRUE") before the same entry point to recompute equivalence, analytical fixtures and output checks without rerunning the market engine. This mode requires the existing daily files; it does not fabricate them.

The local protocol was written before reduction paths were computed, after the historical interval had already informed earlier development. It is not an independent preregistration or untouched holdout. Basic paired-block intervals are approximate and conditional on fixed realized strategy paths; multiplicity adjustment does not remove earlier model-search bias.

Compact archived DB paths and the published cost table provide validation references. The original research run also checked all 43 returned core rules against the full archive (108,059 daily matches per main-cost universe); its dated record is preserved. Independent external comparators are omitted only during core-cache construction because they do not feed core decisions. No omitted comparator is imputed or reported as computed.

## Original comparisons and software checks

    source("RUN_FULL.R")
    source("tests/test_revision.R")

The full original workflow covers both universes, 0/5/10/25 bp costs, original superiority and component families, annual historical budget selection and extra execution delay. Its archived results remain in results/. The new equivalence study uses separate seeds and comparison families. Additional analytical and output checks run in the component workflow.

## Scope and license

Simulation covers 2016--2025; evaluation contains 1,508 daily observations in 2020--2025. General economic superiority is not established. The cost-aware mean-variance comparator remains competitive. Historical screening does not guarantee future drawdown protection. The complete method is specified in the manuscript, independently of Markdown documentation.

The R software is MIT-licensed. Market data retain their providers' terms. Authors: Mervenur Sözen, Onur Şeyranlıoğlu and Çağlar Sözen.
