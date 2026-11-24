# Survey cleaning and validation pipeline (functions). Data are synthetic.
suppressPackageStartupMessages({library(dplyr); library(survey)})

# Standard thresholds, kept in one place so they can be changed without touching the logic
CONFIG <- list(age_min = 16, age_max = 90, wage_min = 10, wage_max = 200)

recode_yes_no <- function(x) {
  v <- tolower(trimws(as.character(x)))
  ifelse(v %in% c("yes", "y", "1", "true"), "Yes",
  ifelse(v %in% c("no", "n", "0", "false"), "No", NA_character_))
}

# Keep the most recent submission for each respondent. Returns the data and the removed rows.
remove_duplicates <- function(df) {
  df <- df |> arrange(resp_id, desc(as.Date(submitted_at)))
  keep <- !duplicated(df$resp_id)
  list(data = df[keep, ], removed = df[!keep, ])
}

# Each rule returns TRUE for rows that break it. Rules are documented in one table.
rules <- function(cfg = CONFIG) {
  list(
    AGE_RANGE        = list(desc = "Age outside plausible range",
                            test = function(d) !is.na(d$age) & (d$age < cfg$age_min | d$age > cfg$age_max)),
    WAGE_RANGE       = list(desc = "Hourly wage outside plausible range",
                            test = function(d) !is.na(d$hourly_wage) & (d$hourly_wage < cfg$wage_min | d$hourly_wage > cfg$wage_max)),
    SKIP_WAGE        = list(desc = "Wage given although respondent is not employed",
                            test = function(d) !is.na(d$employed_6mo) & d$employed_6mo == "No" & !is.na(d$hourly_wage)),
    SKIP_RELATED     = list(desc = "Training-relatedness answered although not employed",
                            test = function(d) !is.na(d$employed_6mo) & d$employed_6mo == "No" & !is.na(d$related_to_training)),
    MISSING_COMPLETED= list(desc = "Completion status missing",
                            test = function(d) is.na(d$completed)),
    STRAIGHTLINE     = list(desc = "Identical answers on all rating questions (flag only)",
                            test = function(d) { q <- d[, c("q_quality","q_support","q_facilities","q_value")]
                              !is.na(rowSums(q)) & apply(q, 1, function(r) length(unique(r)) == 1) & d$recommend %in% c(0, 10) })
  )
}

validate <- function(df, cfg = CONFIG) {
  rl <- rules(cfg)
  flags <- lapply(names(rl), function(id) {
    hit <- which(rl[[id]]$test(df))
    if (length(hit) == 0) return(NULL)
    data.frame(resp_id = df$resp_id[hit], rule = id, description = rl[[id]]$desc, stringsAsFactors = FALSE)
  })
  do.call(rbind, flags)
}

# Fixes: impossible values become missing (not guessed), and the row is kept. STRAIGHTLINE is flag only.
apply_fixes <- function(df, cfg = CONFIG) {
  df$age[!is.na(df$age) & (df$age < cfg$age_min | df$age > cfg$age_max)] <- NA
  bad_wage <- !is.na(df$hourly_wage) & (df$hourly_wage < cfg$wage_min | df$hourly_wage > cfg$wage_max)
  df$hourly_wage[bad_wage] <- NA
  skip <- !is.na(df$employed_6mo) & df$employed_6mo == "No"
  df$hourly_wage[skip] <- NA
  df$related_to_training[skip] <- NA
  df
}

clean_survey <- function(raw, cfg = CONFIG) {
  d <- raw
  d$completed    <- recode_yes_no(d$completed)
  d$employed_6mo <- recode_yes_no(d$employed_6mo)
  dd <- remove_duplicates(d)
  flags <- validate(dd$data, cfg)
  d2 <- apply_fixes(dd$data, cfg)
  d2$flag_straightline <- d2$resp_id %in% flags$resp_id[flags$rule == "STRAIGHTLINE"]
  list(data = d2, flags = flags, removed = dd$removed)
}

# Weight each respondent by (graduates at institution) / (respondents at institution)
add_weights <- function(df, pop) {
  n_resp <- df |> count(institution_id, name = "n_resp")
  df |> left_join(pop, by = "institution_id") |> left_join(n_resp, by = "institution_id") |>
    mutate(weight = n_graduates / n_resp) |> select(-n_graduates, -n_resp)
}

employment_rate <- function(df, by = NULL) {
  df$emp <- as.numeric(df$employed_6mo == "Yes")
  des <- svydesign(ids = ~1, weights = ~weight, data = df[!is.na(df$emp), ])
  if (is.null(by)) {
    m <- svymean(~emp, des); ci <- confint(m)
    data.frame(group = "All", rate = unname(coef(m)), lo = ci[1], hi = ci[2], n = nrow(des$variables))
  } else {
    r <- svyby(~emp, as.formula(paste0("~", by)), des, svymean, vartype = "ci")
    n <- table(des$variables[[by]])
    data.frame(group = r[[by]], rate = r$emp, lo = r$ci_l, hi = r$ci_u, n = as.integer(n[as.character(r[[by]])]))
  }
}
