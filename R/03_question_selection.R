# Cost-of-complexity curve: order questions by forward selection on the training set, score every
# model size on the validation set, and pick the smallest size within a pre-set margin of the
# 21-question model. Also checks whether the chosen questions are stable across training subsets.
source("R/00_utils.R")
AUC_MARGIN <- 0.01   # largest validation AUC loss tolerated against the 21-question model

raw <- load_raw(); df <- recode(raw); y <- df$y
split <- make_split(y); tr <- split == "train"; va <- split == "valid"
blocks <- question_blocks(df)
blocks_tr <- lapply(blocks, function(b) b[tr, , drop = FALSE])

fs <- forward_select(blocks_tr, y[tr])
curve <- do.call(rbind, lapply(seq_along(fs$order), function(k) {
  X <- do.call(cbind, blocks[fs$order[1:k]])[va, , drop = FALSE]
  p <- predict_logit(fs$fits[[k]], X)
  data.frame(k = k, question_added = fs$order[k], parameters = ncol(X), train_aic = fs$aic[k],
             valid_auc = auc(y[va], p), valid_logloss = logloss(y[va], p))
}))
full_auc <- curve$valid_auc[nrow(curve)]
curve$auc_loss_vs_all_21 <- full_auc - curve$valid_auc
k_star <- min(curve$k[curve$auc_loss_vs_all_21 <= AUC_MARGIN])
write.csv(curve, file.path(OUT_DIR, "selection_curve_validation.csv"), row.names = FALSE)
print(curve, row.names = FALSE)
cat("Chosen number of questions:", k_star, "->", paste(fs$order[1:k_star], collapse = ", "), "\n")
saveRDS(list(order = fs$order, k_star = k_star, margin = AUC_MARGIN), file.path(OUT_DIR, "selection.rds"))

png(file.path(FIG_DIR, "cost_of_complexity.png"), width = 900, height = 600, res = 110)
plot(curve$k, curve$valid_auc, type = "b", pch = 19, xlab = "Number of questions asked",
     ylab = "Validation AUC", main = "What each extra question buys", xaxt = "n",
     ylim = c(min(curve$valid_auc) - 0.005, full_auc + 0.01))
axis(1, at = curve$k)
abline(h = full_auc, lty = 2); abline(h = full_auc - AUC_MARGIN, lty = 3, col = "red")
points(k_star, curve$valid_auc[k_star], pch = 19, cex = 1.8, col = "red")
text(curve$k[1:k_star], curve$valid_auc[1:k_star], curve$question_added[1:k_star], pos = 1, cex = 0.7)
legend("bottomright", bty = "n", lty = c(2, 3, NA), pch = c(NA, NA, 19), col = c("black", "red", "red"),
       legend = c("all 21 questions", sprintf("all 21 minus %.2f (tolerance)", AUC_MARGIN), "chosen model"))
dev.off()

# Stability: repeat the selection on 5 disjoint fifths of the training set.
set.seed(7)
tr_idx <- which(tr)
fold <- sample(rep(1:5, length.out = length(tr_idx)))
max_k <- min(length(QUESTIONS), k_star + 3)
orders <- sapply(1:5, function(f) {
  rows <- tr_idx[fold == f]
  forward_select(lapply(blocks, function(b) b[rows, , drop = FALSE]), y[rows], max_k)$order
})
colnames(orders) <- paste0("fold", 1:5)
stab <- data.frame(step = 1:max_k, full_training_set = fs$order[1:max_k], orders)
write.csv(stab, file.path(OUT_DIR, "selection_stability_orders.csv"), row.names = FALSE)
print(stab, row.names = FALSE)
top <- data.frame(
  question = fs$order[1:k_star],
  folds_in_top_k_of_5 = vapply(fs$order[1:k_star], function(q)
    sum(apply(orders[1:k_star, , drop = FALSE], 2, function(o) q %in% o)), numeric(1)))
write.csv(top, file.path(OUT_DIR, "selection_stability_topk.csv"), row.names = FALSE)
print(top, row.names = FALSE)
