# Final model, nurse point score, operating cut-off (all fixed on train/validation), then a single
# evaluation on the untouched test set.
source("R/00_utils.R")
SCORE_AUC_TOLERANCE <- 0.001  # validation AUC the integer score may lose against its own model
TARGET_SENSITIVITY <- 0.80    # share of people with heart disease the cut-off must flag (validation)
N_BOOT <- 1000

sel <- readRDS(file.path(OUT_DIR, "selection.rds"))
raw <- load_raw(); df <- recode(raw); y <- df$y
split <- make_split(y); tr <- split == "train"; va <- split == "valid"; te <- split == "test"
blocks <- question_blocks(df)
chosen <- sel$order[1:sel$k_star]

X_c <- do.call(cbind, blocks[chosen]); X_f <- do.call(cbind, blocks[QUESTIONS])
fit_c <- fit_logit(X_c[tr, ], y[tr]); fit_f <- fit_logit(X_f[tr, ], y[tr])
p_c <- predict_logit(fit_c, X_c); p_f <- predict_logit(fit_f, X_f)
p_age <- predict_logit(fit_logit(blocks[["Age"]][tr, ], y[tr]), blocks[["Age"]])

# ---- Point score: shift each question so its lowest-risk answer scores 0, then round ----
level_coefs <- lapply(setNames(chosen, chosen), function(q) {
  lv <- levels(df[[q]])
  b <- c(0, fit_c$coefficients[paste0(q, ":", lv[-1])]); names(b) <- lv
  b - min(b)
})
score_for <- function(B) {
  pts <- lapply(level_coefs, function(b) round(b / B))
  s <- Reduce(`+`, lapply(chosen, function(q) unname(pts[[q]][as.integer(df[[q]])])))
  list(points = pts, score = s)
}
grid <- c(1, 0.75, 0.5, 0.4, 0.3, 0.25, 0.2)   # log-odds per point, coarsest first
grid_tab <- do.call(rbind, lapply(grid, function(B) {
  s <- score_for(B)
  data.frame(log_odds_per_point = B, max_score = sum(vapply(s$points, max, numeric(1))),
             valid_auc_score = auc(y[va], s$score[va]), valid_auc_model = auc(y[va], p_c[va]))
}))
grid_tab$loss <- grid_tab$valid_auc_model - grid_tab$valid_auc_score
ok <- which(grid_tab$loss <= SCORE_AUC_TOLERANCE)
B <- grid[if (length(ok)) ok[1] else length(grid)]
write.csv(grid_tab, file.path(OUT_DIR, "score_scaling_validation.csv"), row.names = FALSE)
print(grid_tab, row.names = FALSE); cat("Chosen log-odds per point:", B, "\n")
sc <- score_for(B); score <- sc$score

# ---- Cut-offs fixed on validation ----
cuts <- sort(unique(score))
sens_va <- vapply(cuts, function(c) mean(score[va & y == 1] >= c), numeric(1))
cut_star <- max(cuts[sens_va >= TARGET_SENSITIVITY])
thr_full <- unname(quantile(p_f[va & y == 1], 1 - TARGET_SENSITIVITY))
# Second threshold for the 21-question model: flag the same share of validation rows as the score
# does, so the two rules can be compared at equal workload.
thr_full_matched <- unname(quantile(p_f[va], 1 - mean(score[va] >= cut_star)))
cat("Score cut-off:", cut_star, " validation sensitivity:", sens_va[cuts == cut_star], "\n")

# =========================== TEST SET ===========================
yt <- y[te]
preds <- list("Age only" = p_age[te],
              "Point score (chosen questions)" = score[te],
              "Logistic, chosen questions" = p_c[te],
              "Logistic, all 21 questions" = p_f[te])

set.seed(99)
boot <- t(replicate(N_BOOT, {
  i <- sample.int(length(yt), replace = TRUE)
  vapply(preds, function(p) auc(yt[i], p[i]), numeric(1))
}))
ci <- function(x) quantile(x, c(0.025, 0.975))
auc_tab <- data.frame(model = names(preds), test_auc = vapply(preds, function(p) auc(yt, p), numeric(1)),
                      ci_low = apply(boot, 2, function(x) ci(x)[1]), ci_high = apply(boot, 2, function(x) ci(x)[2]))
write.csv(auc_tab, file.path(OUT_DIR, "test_auc.csv"), row.names = FALSE)
print(auc_tab, row.names = FALSE)

pairs <- list(c("Point score (chosen questions)", "Logistic, all 21 questions"),
              c("Logistic, chosen questions", "Logistic, all 21 questions"),
              c("Point score (chosen questions)", "Logistic, chosen questions"),
              c("Point score (chosen questions)", "Age only"))
diff_tab <- do.call(rbind, lapply(pairs, function(pr) {
  d <- boot[, pr[1]] - boot[, pr[2]]
  data.frame(model_a = pr[1], model_b = pr[2],
             auc_a_minus_b = auc(yt, preds[[pr[1]]]) - auc(yt, preds[[pr[2]]]),
             ci_low = ci(d)[1], ci_high = ci(d)[2])
}))
write.csv(diff_tab, file.path(OUT_DIR, "test_auc_differences.csv"), row.names = FALSE)
print(diff_tab, row.names = FALSE)

# Every possible cut-off on the test set, so a clinic can pick a different trade-off.
cut_tab <- do.call(rbind, lapply(cuts, function(c) cbind(cutoff = c, chosen = c == cut_star, flag_metrics(yt, score[te] >= c))))
write.csv(cut_tab, file.path(OUT_DIR, "test_cutoff_table.csv"), row.names = FALSE)
print(cut_tab[, c("cutoff", "chosen", "flagged_pct", "sensitivity", "specificity", "ppv", "npv")], row.names = FALSE)

# Chosen rule against the 21-question model. Flag agreement uses the equal-workload threshold.
flag_s <- score[te] >= cut_star; flag_f <- p_f[te] >= thr_full_matched
op_tab <- rbind(cbind(rule = sprintf("Point score >= %d", cut_star), flag_metrics(yt, flag_s)),
                cbind(rule = "Logistic, all 21 questions, same share flagged", flag_metrics(yt, flag_f)),
                cbind(rule = "Logistic, all 21 questions, 80% sensitivity target", flag_metrics(yt, p_f[te] >= thr_full)))
write.csv(op_tab, file.path(OUT_DIR, "test_operating_point.csv"), row.names = FALSE)
print(op_tab, row.names = FALSE)
agree <- data.frame(both_flag = sum(flag_s & flag_f), score_only = sum(flag_s & !flag_f),
                    full_only = sum(!flag_s & flag_f), neither = sum(!flag_s & !flag_f),
                    agreement_pct = 100 * mean(flag_s == flag_f),
                    cases_score_only = sum(flag_s & !flag_f & yt == 1),
                    cases_full_only = sum(!flag_s & flag_f & yt == 1))
write.csv(agree, file.path(OUT_DIR, "test_flag_agreement.csv"), row.names = FALSE)
print(agree, row.names = FALSE)

# Subgroups.
age_grp <- cut(raw$Age, breaks = c(0, 6, 9, 13), labels = c("18-49", "50-64", "65+"))
groups <- list(All = rep(TRUE, length(y)), Female = df$Sex == "Female", Male = df$Sex == "Male",
               "Age 18-49" = age_grp == "18-49", "Age 50-64" = age_grp == "50-64", "Age 65+" = age_grp == "65+")
sub_tab <- do.call(rbind, lapply(names(groups), function(g) {
  i <- te & groups[[g]]
  cbind(group = g, auc_score = auc(y[i], score[i]), prevalence_pct = 100 * mean(y[i]), flag_metrics(y[i], score[i] >= cut_star))
}))
write.csv(sub_tab, file.path(OUT_DIR, "test_subgroups.csv"), row.names = FALSE)
print(sub_tab[, c("group", "n", "cases", "prevalence_pct", "auc_score", "flagged_pct", "sensitivity", "specificity", "ppv")], row.names = FALSE)

# Calibration of the chosen-question logistic model by tenths of predicted risk.
dec <- cut(p_c[te], breaks = unique(quantile(p_c[te], 0:10 / 10)), include.lowest = TRUE)
cal_tab <- data.frame(n = as.vector(table(dec)), mean_predicted = tapply(p_c[te], dec, mean), observed = tapply(yt, dec, mean))
write.csv(cal_tab, file.path(OUT_DIR, "test_calibration.csv"), row.names = FALSE)
print(cal_tab, row.names = FALSE)

# Observed heart disease rate at each score: training rows (used for the sheet) against test rows.
risk_tab <- data.frame(score = cuts,
                       train_n = vapply(cuts, function(c) sum(score[tr] == c), numeric(1)),
                       train_rate_pct = vapply(cuts, function(c) 100 * mean(y[tr][score[tr] == c]), numeric(1)),
                       test_n = vapply(cuts, function(c) sum(score[te] == c), numeric(1)),
                       test_rate_pct = vapply(cuts, function(c) 100 * mean(yt[score[te] == c]), numeric(1)))
write.csv(risk_tab, file.path(OUT_DIR, "risk_by_score.csv"), row.names = FALSE)
print(risk_tab, row.names = FALSE)

# ---- Figures ----
roc_xy <- function(y, p) { o <- order(-p); list(fpr = c(0, cumsum(y[o] == 0) / sum(y == 0)), tpr = c(0, cumsum(y[o] == 1) / sum(y == 1))) }
png(file.path(FIG_DIR, "roc_test.png"), width = 800, height = 750, res = 110)
cols <- c("grey60", "red", "orange", "steelblue")[seq_along(preds)]
plot(0:1, 0:1, type = "l", lty = 3, xlab = "1 - specificity", ylab = "Sensitivity", main = "Test set ROC curves")
for (j in seq_along(preds)) { r <- roc_xy(yt, preds[[j]]); lines(r$fpr, r$tpr, col = cols[j], lwd = 2) }
m <- flag_metrics(yt, flag_s); points(1 - m$specificity, m$sensitivity, pch = 19, col = "red", cex = 1.5)
legend("bottomright", bty = "n", lwd = 2, col = cols, legend = sprintf("%s (AUC %.3f)", names(preds), auc_tab$test_auc))
dev.off()

png(file.path(FIG_DIR, "calibration_test.png"), width = 750, height = 700, res = 110)
lim <- c(0, max(cal_tab$mean_predicted, cal_tab$observed))
plot(cal_tab$mean_predicted, cal_tab$observed, pch = 19, xlim = lim, ylim = lim, xlab = "Mean predicted risk",
     ylab = "Observed rate", main = "Calibration on the test set (tenths of predicted risk)")
abline(0, 1, lty = 2)
dev.off()

png(file.path(FIG_DIR, "risk_by_score.png"), width = 850, height = 600, res = 110)
keep <- risk_tab$test_n >= 50
plot(risk_tab$score[keep], risk_tab$test_rate_pct[keep], type = "b", pch = 19, xlab = "Point score",
     ylab = "Observed heart disease rate, test set (%)", main = "Observed rate by point score (scores with at least 50 test rows)")
abline(v = cut_star - 0.5, lty = 2, col = "red")
dev.off()

# ---- Score sheet ----
sheet <- c("# Heart disease triage score sheet", "",
           sprintf("Ask the %d questions below, add up the points, and flag anyone scoring **%d or more**.", length(chosen), cut_star), "")
for (q in chosen) {
  sheet <- c(sheet, sprintf("**%s**", QUESTION_TEXT[[q]]), "", "| Answer | Points |", "|---|---|",
             sprintf("| %s | %d |", names(sc$points[[q]]), as.integer(sc$points[[q]])), "")
}
m <- op_tab[1, ]
sheet <- c(sheet, sprintf("Maximum score: %d.", sum(vapply(sc$points, max, numeric(1)))), "",
           sprintf("On %s held-out survey respondents, flagging at %d or more flagged %.1f%% of people, caught %.1f%% of those reporting heart disease, and %.1f%% of flagged people reported heart disease.",
                   format(m$n, big.mark = ","), cut_star, m$flagged_pct, 100 * m$sensitivity, 100 * m$ppv), "",
           "The outcome is self-reported history of coronary heart disease or heart attack in a US telephone survey. The sheet has not been validated in a clinic or against clinical diagnosis.")
writeLines(sheet, file.path(OUT_DIR, "score_sheet.md"))
saveRDS(list(chosen = chosen, B = B, cut = cut_star, points = sc$points, thr_full = thr_full,
             coefficients = fit_c$coefficients), file.path(OUT_DIR, "final_model.rds"))
