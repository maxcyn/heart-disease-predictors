# Shared helpers: data loading, recoding, the train/validation/test split and metrics.
# Every script sources this file and must be run from the repository root.

DATA_FILE <- "heart_disease_indicators_dataset.csv"
OUT_DIR <- "outputs"
FIG_DIR <- file.path(OUT_DIR, "figures")
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)

TARGET <- "HeartDiseaseorAttack"
SPLIT_SEED <- 2026

load_raw <- function() read.csv(DATA_FILE)

QUESTIONS <- c("HighBP", "HighChol", "CholCheck", "BMI", "Smoker", "Stroke", "Diabetes",
               "PhysActivity", "Fruits", "Veggies", "HvyAlcoholConsump", "AnyHealthcare",
               "NoDocbcCost", "GenHlth", "MentHlth", "PhysHlth", "DiffWalk", "Sex", "Age",
               "Education", "Income")

QUESTION_TEXT <- c(
  HighBP = "Ever told you have high blood pressure?",
  HighChol = "Ever told you have high cholesterol?",
  CholCheck = "Cholesterol checked in the past 5 years?",
  BMI = "Body mass index",
  Smoker = "Smoked at least 100 cigarettes in your life?",
  Stroke = "Ever told you had a stroke?",
  Diabetes = "Ever told you have diabetes?",
  PhysActivity = "Any physical activity in the past 30 days (outside work)?",
  Fruits = "Eat fruit at least once a day?",
  Veggies = "Eat vegetables at least once a day?",
  HvyAlcoholConsump = "Heavy drinker (men >14, women >7 drinks a week)?",
  AnyHealthcare = "Any health care coverage?",
  NoDocbcCost = "Needed a doctor in the past year but could not because of cost?",
  GenHlth = "Would you say your general health is...",
  MentHlth = "Days of poor mental health in the past 30",
  PhysHlth = "Days of poor physical health in the past 30",
  DiffWalk = "Serious difficulty walking or climbing stairs?",
  Sex = "Sex",
  Age = "Age",
  Education = "Highest level of education",
  Income = "Annual household income"
)

# Every question becomes a factor, so no question is forced to be linear on the log-odds scale.
recode <- function(d) {
  out <- data.frame(y = as.integer(d[[TARGET]]))
  yes_no <- c("HighBP", "HighChol", "CholCheck", "Smoker", "Stroke", "PhysActivity", "Fruits",
              "Veggies", "HvyAlcoholConsump", "AnyHealthcare", "NoDocbcCost", "DiffWalk")
  for (v in yes_no) out[[v]] <- factor(d[[v]], levels = c(0, 1), labels = c("No", "Yes"))
  out$Sex <- factor(d$Sex, levels = c(0, 1), labels = c("Female", "Male"))
  out$Diabetes <- factor(d$Diabetes, levels = 0:2, labels = c("No", "Prediabetes", "Diabetes"))
  out$GenHlth <- factor(d$GenHlth, levels = 1:5,
                        labels = c("Excellent", "Very good", "Good", "Fair", "Poor"))
  out$Age <- factor(d$Age, levels = 1:13,
                    labels = c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54",
                               "55-59", "60-64", "65-69", "70-74", "75-79", "80+"))
  out$Education <- factor(pmax(d$Education, 2), levels = 2:6,
                          labels = c("Elementary or less", "Some high school",
                                     "High school graduate", "Some college", "College graduate"))
  out$Income <- factor(d$Income, levels = 1:8,
                       labels = c("<$10k", "$10-15k", "$15-20k", "$20-25k", "$25-35k",
                                  "$35-50k", "$50-75k", "$75k+"))
  out$BMI <- cut(d$BMI, breaks = c(0, 18.5, 25, 30, 35, 40, Inf), right = FALSE,
                 labels = c("<18.5", "18.5-24.9", "25-29.9", "30-34.9", "35-39.9", "40+"))
  days <- function(x) cut(x, breaks = c(-Inf, 0, 13, Inf), labels = c("0", "1-13", "14-30"))
  out$MentHlth <- days(d$MentHlth)
  out$PhysHlth <- days(d$PhysHlth)
  stopifnot(!anyNA(out))
  out[, c("y", QUESTIONS)]
}

# 60/20/20 split, stratified on the outcome. Deterministic for a given seed.
make_split <- function(y, seed = SPLIT_SEED) {
  set.seed(seed)
  split <- character(length(y))
  for (cls in c(0, 1)) {
    idx <- which(y == cls)
    idx <- idx[sample.int(length(idx))]
    n <- length(idx); n_tr <- floor(0.6 * n); n_va <- floor(0.2 * n)
    split[idx[1:n_tr]] <- "train"
    split[idx[(n_tr + 1):(n_tr + n_va)]] <- "valid"
    split[idx[(n_tr + n_va + 1):n]] <- "test"
  }
  split
}

# One dummy-variable block per question, so selection adds or removes whole questions.
question_blocks <- function(df, questions = QUESTIONS) {
  lapply(setNames(questions, questions), function(q) {
    m <- model.matrix(~ x, data.frame(x = df[[q]]))[, -1, drop = FALSE]
    colnames(m) <- paste0(q, ":", levels(df[[q]])[-1])
    m
  })
}

fit_logit <- function(X, y) {
  f <- glm.fit(cbind(`(Intercept)` = 1, X), y, family = binomial())
  stopifnot(f$converged, !anyNA(f$coefficients))
  f
}
predict_logit <- function(fit, X) plogis(drop(cbind(1, X) %*% fit$coefficients))

# AUC by the rank (Mann-Whitney) formula; ties get average ranks.
auc <- function(y, p) {
  r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
logloss <- function(y, p) {
  p <- pmin(pmax(p, 1e-12), 1 - 1e-12)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}
flag_metrics <- function(y, flag) {
  tp <- sum(flag & y == 1); fp <- sum(flag & y == 0)
  fn <- sum(!flag & y == 1); tn <- sum(!flag & y == 0)
  data.frame(n = length(y), cases = tp + fn, flagged = tp + fp, TP = tp, FP = fp, FN = fn, TN = tn,
             flagged_pct = 100 * (tp + fp) / length(y),
             sensitivity = tp / (tp + fn), specificity = tn / (tn + fp),
             ppv = tp / (tp + fp), npv = tn / (tn + fn))
}

# Greedy forward selection of whole questions by training AIC.
forward_select <- function(blocks, y, max_k = length(blocks)) {
  selected <- character(0); remaining <- names(blocks)
  aics <- numeric(0); fits <- list()
  for (k in seq_len(max_k)) {
    base <- if (length(selected)) do.call(cbind, blocks[selected]) else NULL
    cand <- lapply(remaining, function(q) fit_logit(cbind(base, blocks[[q]]), y))
    cand_aic <- vapply(cand, function(f) f$aic, numeric(1))
    best <- which.min(cand_aic)
    selected <- c(selected, remaining[best]); remaining <- remaining[-best]
    aics <- c(aics, cand_aic[best]); fits[[k]] <- cand[[best]]
  }
  list(order = selected, aic = aics, fits = fits)
}
