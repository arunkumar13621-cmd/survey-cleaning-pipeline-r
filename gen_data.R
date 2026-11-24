# Generate a synthetic graduate-outcomes survey (fictional institutions) with planted data problems.
suppressPackageStartupMessages({library(dplyr); library(jsonlite)})
set.seed(2027)
inst <- data.frame(
  institution_id = sprintf("PTI%02d", 1:12),
  n_graduates    = c(900, 650, 800, 500, 700, 450, 600, 850, 400, 550, 750, 500),
  resp_rate      = c(.55, .20, .45, .30, .50, .15, .40, .25, .60, .35, .22, .48),
  inst_effect    = round(rnorm(12, 0, .35), 3), stringsAsFactors = FALSE)
programs <- c("Trades","Health Care","Information Technology","Business","Hospitality","Design")
prog_eff <- c(.50, .40, .30, .00, -.20, -.35); names(prog_eff) <- programs
prog_wage <- c(Trades=30, `Health Care`=28, `Information Technology`=33, Business=26, Hospitality=20, Design=24)

# full population of graduates (truth is known because we simulate everyone)
pop <- inst |> rowwise() |> do(data.frame(institution_id=.$institution_id, idx=seq_len(.$n_graduates))) |> ungroup()
pop <- pop |> left_join(inst, by="institution_id")
n <- nrow(pop)
pop$program_area <- sample(programs, n, TRUE, prob=c(.22,.2,.15,.18,.15,.10))
pop$completed <- rbinom(n, 1, .78)
pop$age <- pmin(pmax(round(rnorm(n, 29, 8)), 17), 62)
lin <- -0.2 + 1.0*pop$completed + prog_eff[pop$program_area] + pop$inst_effect - 0.012*(pop$age-29)
pop$employed <- rbinom(n, 1, plogis(lin))
pop$wage <- ifelse(pop$employed==1, round(prog_wage[pop$program_area]*exp(rnorm(n,0,.18)) + 2*pop$completed, 2), NA)
pop$related <- ifelse(pop$employed==1, sample(c("Yes","Partly","No"), n, TRUE, prob=c(.55,.25,.20)), NA)
truth_rate <- mean(pop$employed)
truth_by_prog <- tapply(pop$employed, pop$program_area, mean)

# respondents: response depends on institution
pop$responded <- rbinom(n, 1, pop$resp_rate)
s <- pop[pop$responded==1, ]
m <- nrow(s)
s$resp_id <- sprintf("R%05d", sample(10000:99999, m))
lik <- function(k) pmin(pmax(round(rnorm(m, 3.6 + 0.3*(s$employed - .5)*0 + k, 1)), 1), 5)
s$q_quality <- lik(0); s$q_support <- lik(-.1); s$q_facilities <- lik(.1); s$q_value <- lik(-.2)
s$recommend <- pmin(pmax(round(rnorm(m, 7.2, 2)), 0), 10)
s$submitted_at <- as.character(as.Date("2026-09-01") + sample(0:60, m, TRUE))
s$grad_year <- sample(2023:2025, m, TRUE)
# raw coding (messy): Yes/No in several spellings
yn <- function(x) { base <- ifelse(x==1,"Yes","No"); k <- sample(1:4, length(x), TRUE, prob=c(.7,.12,.1,.08))
  ifelse(k==1, base, ifelse(k==2, tolower(base), ifelse(k==3, ifelse(x==1,"Y","N"), ifelse(x==1,"1","0")))) }
raw <- data.frame(resp_id=s$resp_id, institution_id=s$institution_id, program_area=s$program_area,
  grad_year=s$grad_year, age=s$age, completed=yn(s$completed), employed_6mo=yn(s$employed),
  hourly_wage=s$wage, related_to_training=s$related, q_quality=s$q_quality, q_support=s$q_support,
  q_facilities=s$q_facilities, q_value=s$q_value, recommend=s$recommend, submitted_at=s$submitted_at,
  stringsAsFactors=FALSE)
truth <- list()
# planted problems
pick <- function(k, exclude=integer()) sample(setdiff(seq_len(nrow(raw)), exclude), k)
used <- integer()
i <- pick(25, used); used <- c(used,i); raw$age[i] <- sample(c(3,9,12,14,102,115,130), 25, TRUE); truth$bad_age <- raw$resp_id[i]
i <- pick(30, used); used <- c(used,i); emp <- i[!is.na(raw$hourly_wage[i])]
i <- sample(which(!is.na(raw$hourly_wage)), 30); used <- c(used,i)
raw$hourly_wage[i] <- sample(c(0.5,2.5,4,6.75,310,450,1200,2300), 30, TRUE); truth$bad_wage <- raw$resp_id[i]
nonemp <- which(is.na(raw$hourly_wage) & is.na(raw$related_to_training))
i <- sample(nonemp, 35); used <- c(used,i); raw$hourly_wage[i] <- round(runif(35, 15, 40), 2); truth$wage_when_not_employed <- raw$resp_id[i]
i <- sample(setdiff(nonemp, i), 20); used <- c(used,i); raw$related_to_training[i] <- "Yes"; truth$related_when_not_employed <- raw$resp_id[i]
i <- sample(setdiff(seq_len(nrow(raw)), used), 60); raw$completed[i] <- NA; truth$missing_completed <- raw$resp_id[i]
i <- sample(setdiff(seq_len(nrow(raw)), used), 45); raw[i, c("q_quality","q_support","q_facilities","q_value")] <- 5; raw$recommend[i] <- 10; truth$straightliners <- raw$resp_id[i]
# duplicates: re-submissions, later date
d <- raw[sample(nrow(raw), 40), ]; d$submitted_at <- as.character(as.Date(d$submitted_at) + sample(1:5, 40, TRUE)); truth$duplicate_resp_ids <- d$resp_id
raw <- rbind(raw, d); raw <- raw[sample(nrow(raw)), ]
dir.create("data", showWarnings=FALSE)
write.csv(raw, "data/raw_survey.csv", row.names=FALSE, na="")
write.csv(inst[, c("institution_id","n_graduates")], "data/population_sizes.csv", row.names=FALSE)
write_json(list(truth=truth, population_employment_rate=truth_rate, by_program=as.list(truth_by_prog), n_respondents=m, n_raw_rows=nrow(raw)), "data/ground_truth.json", auto_unbox=TRUE, pretty=TRUE)
cat("raw rows:", nrow(raw), " unique respondents:", m, " population rate:", round(truth_rate,4), "\n")
print(lengths(truth))
