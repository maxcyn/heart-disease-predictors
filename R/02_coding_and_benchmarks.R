# Validation-set comparison of predictor coding and of benchmark models. The test set is not used.
source("R/00_utils.R")
raw <- load_raw(); df <- recode(raw); y <- df$y
split <- make_split(y); tr <- split == "train"; va <- split == "valid"
blocks <- question_blocks(df)

rows <- list()
add <- function(model, n_questions, p) {
  rows[[length(rows) + 1]] <<- data.frame(model = model, questions = n_questions,
                                           valid_auc = auc(y[va], p), valid_logloss = logloss(y[va], p))
}

add("No model (predict the training prevalence)", 0, rep(mean(y[tr]), sum(va)))

X_age <- blocks[["Age"]]
add("Age only (13 bands)", 1, predict_logit(fit_logit(X_age[tr, ], y[tr]), X_age[va, ]))

X_num <- as.matrix(raw[, QUESTIONS])
add("Logistic, all 21, numeric-linear coding (as in the original)", 21,
    predict_logit(fit_logit(X_num[tr, ], y[tr]), X_num[va, ]))

X_fac <- do.call(cbind, blocks)
add("Logistic, all 21, categorical coding", 21,
    predict_logit(fit_logit(X_fac[tr, ], y[tr]), X_fac[va, ]))

tab <- do.call(rbind, rows)
write.csv(tab, file.path(OUT_DIR, "benchmarks_validation.csv"), row.names = FALSE)
print(tab, row.names = FALSE)
cat("split sizes:\n"); print(table(split, y))
