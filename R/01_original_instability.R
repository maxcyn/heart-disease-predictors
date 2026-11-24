# How much does the original procedure (4,000-row sample + backward stepwise AIC) depend on
# which 4,000 rows were drawn? Repeats it on 50 different random samples.
source("R/00_utils.R")
raw <- load_raw()

run_original <- function(seed) {
  set.seed(seed)
  d <- raw[sample(nrow(raw), 4000), ]
  full <- glm(HeartDiseaseorAttack ~ ., data = d, family = "binomial")
  red <- step(full, direction = "backward", trace = 0)
  attr(terms(red), "term.labels")
}

original <- run_original(123)
seeds <- 1:50
kept <- lapply(seeds, run_original)

freq <- data.frame(
  variable = QUESTIONS,
  kept_in_original_seed123 = QUESTIONS %in% original,
  times_kept_of_50 = vapply(QUESTIONS, function(v) sum(vapply(kept, function(k) v %in% k, logical(1))), numeric(1))
)
freq <- freq[order(-freq$times_kept_of_50), ]
write.csv(freq, file.path(OUT_DIR, "original_instability_by_variable.csv"), row.names = FALSE)

sizes <- vapply(kept, length, numeric(1))
same_as_original <- vapply(kept, function(k) setequal(k, original), logical(1))
distinct_sets <- length(unique(vapply(kept, function(k) paste(sort(k), collapse = "+"), character(1))))
summary_tab <- data.frame(
  samples = length(seeds), min_kept = min(sizes), median_kept = median(sizes), max_kept = max(sizes),
  distinct_variable_sets = distinct_sets, identical_to_seed123 = sum(same_as_original),
  always_kept = sum(freq$times_kept_of_50 == 50), never_kept = sum(freq$times_kept_of_50 == 0)
)
write.csv(summary_tab, file.path(OUT_DIR, "original_instability_summary.csv"), row.names = FALSE)
print(freq, row.names = FALSE); print(summary_tab, row.names = FALSE)

png(file.path(FIG_DIR, "original_instability.png"), width = 900, height = 650, res = 110)
par(mar = c(4.5, 9, 3, 1))
o <- freq[order(freq$times_kept_of_50), ]
barplot(o$times_kept_of_50, names.arg = o$variable, horiz = TRUE, las = 1,
        col = ifelse(o$kept_in_original_seed123, "steelblue", "grey75"),
        xlab = "Times kept out of 50 random 4,000-row samples",
        main = "Original method: which variables survive depends on the sample")
legend("bottomright", fill = c("steelblue", "grey75"), bty = "n",
       legend = c("kept in the original run (seed 123)", "dropped in the original run"))
dev.off()
