library(testthat); library(jsonlite)
source("../../R/pipeline.R")
raw   <- read.csv("../../data/raw_survey.csv", stringsAsFactors = FALSE, na.strings = c("", "NA"))
pop   <- read.csv("../../data/population_sizes.csv", stringsAsFactors = FALSE)
truth <- read_json("../../data/ground_truth.json", simplifyVector = TRUE)
res   <- clean_survey(raw)

test_that("yes/no spellings are standardised", {
  expect_equal(recode_yes_no(c("Yes","yes","Y","1","No","no","N","0"," YES ")), c(rep("Yes",4), rep("No",4), "Yes"))
  expect_true(is.na(recode_yes_no("maybe")))
})
test_that("duplicate submissions are removed and the latest one kept", {
  expect_equal(nrow(res$removed), 40)
  expect_equal(anyDuplicated(res$data$resp_id), 0)
  expect_equal(nrow(res$data), truth$n_respondents)
  id <- truth$truth$duplicate_resp_ids[1]
  both <- raw[raw$resp_id == id, ]
  expect_equal(res$data$submitted_at[res$data$resp_id == id], max(both$submitted_at))
})
test_that("every planted problem is flagged (recall 100%)", {
  f <- res$flags
  hit <- function(rule) unique(f$resp_id[f$rule == rule])
  expect_true(all(truth$truth$bad_age %in% hit("AGE_RANGE")))
  expect_true(all(truth$truth$bad_wage %in% hit("WAGE_RANGE")))
  expect_true(all(truth$truth$wage_when_not_employed %in% hit("SKIP_WAGE")))
  expect_true(all(truth$truth$related_when_not_employed %in% hit("SKIP_RELATED")))
  expect_true(all(truth$truth$missing_completed %in% hit("MISSING_COMPLETED")))
  expect_true(all(truth$truth$straightliners %in% hit("STRAIGHTLINE")))
})
test_that("rules flag only what they should (precision for the exact-rule problems)", {
  f <- res$flags
  expect_equal(sum(f$rule == "AGE_RANGE"), 25)
  expect_equal(sum(f$rule == "WAGE_RANGE"), 30)
  expect_equal(sum(f$rule == "SKIP_WAGE"), 35)
  expect_equal(sum(f$rule == "MISSING_COMPLETED"), 60)
  # straightline can catch real coincidences, so it is allowed to flag a few extra rows
  expect_gte(sum(f$rule == "STRAIGHTLINE"), 45)
  expect_lt(sum(f$rule == "STRAIGHTLINE"), 70)
})
test_that("after fixes no impossible value remains and rows are kept", {
  d <- res$data
  expect_false(any(d$age < CONFIG$age_min | d$age > CONFIG$age_max, na.rm = TRUE))
  expect_false(any(d$hourly_wage < CONFIG$wage_min | d$hourly_wage > CONFIG$wage_max, na.rm = TRUE))
  expect_true(all(is.na(d$hourly_wage[d$employed_6mo == "No"])))
  expect_equal(nrow(d), truth$n_respondents)
})
test_that("weights add back up to the institution populations", {
  cl <- add_weights(res$data, pop)
  w  <- tapply(cl$weight, cl$institution_id, sum)
  expect_equal(as.numeric(w[pop$institution_id]), as.numeric(pop$n_graduates), tolerance = 1e-8)
})
test_that("weighted employment rate is close to the true population rate", {
  cl <- add_weights(res$data, pop)
  r <- employment_rate(cl)
  expect_lt(abs(r$rate - truth$population_employment_rate), 0.03)
  expect_lt(r$lo, r$rate); expect_gt(r$hi, r$rate)
})
