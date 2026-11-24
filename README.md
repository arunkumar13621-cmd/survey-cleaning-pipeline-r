# Survey Data Cleaning and Validation Pipeline (R)

A pipeline in R that takes a messy raw survey file, checks it against documented rules, fixes or flags problems, adds weights and estimates employment rates. All data is synthetic: 12 fictional training institutions and about 2,700 fictional graduates.

**Run:** `Rscript gen_data.R` (makes the raw file and the answer key), then `Rscript run_pipeline.R`. **Test:** `Rscript -e "testthat::test_dir('tests/testthat')"` (7 tests, 26 checks).

## What it does
1. Standardises Yes/No answers written in different ways (Yes, yes, Y, 1).
2. Removes duplicate submissions and keeps the latest one per respondent.
3. Checks six rules (age range, wage range, wage or training-relatedness given by someone not employed, missing completion status, identical answers on every rating question). Thresholds live in one `CONFIG` list.
4. Fixes impossible values by setting them to missing (never guessing) and keeps the row. A log of every flagged row is written to `output/dq_log.csv`.
5. Weights each respondent by institution (graduates at the institution divided by respondents) and estimates employment rates with confidence intervals using the `survey` package.

## Results (from `output/analysis_output.txt`)
* 2,768 raw rows, 40 duplicate submissions removed, 2,728 respondents kept.
* All planted problems were found: 25 bad ages, 30 bad wages, 35 wages from non-employed people, 20 relatedness answers from non-employed people, 60 missing completion values.
* The straight-lining rule flagged 54 rows against 45 planted, so about 9 are respondents who gave identical answers by chance. It is flag-only for that reason.
* Employment rate: unweighted 63.3%, weighted 63.2% (95% CI 61.2% to 65.2%), true population rate 63.5%. Weighting changed the estimate very little here because employment rates are similar across institutions. In real data the effect depends on how response rates and outcomes relate.

## Limits
Synthetic data with simple, planted problems. Real survey data has less obvious problems. The wage limits are illustrative, not official rules. Weighting by institution only corrects for differences between institutions.
