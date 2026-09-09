# ---------------------------------------------------------------------------
# 08_across_studies_check.R -- numerical check of the claim made in the Methods
# subsection "Which inferential problem is being addressed", setting (iii):
# if a field runs trials of sizes n_1 < n_2 < ... and tests each at its own
# level n_k^-p, only finitely many false positives are ever published, provided
# the sizes grow at least linearly in k.
#
# A field runs true-null trials in sequence. Trial k has total size n_k and is
# tested once at level alpha_k. Counted: how many false positives the field
# publishes, against the theoretical expectation sum_k alpha_k.
#
#   (a) sizes grow linearly, almost sure rule   -> count settles
#   (b) sizes grow linearly, fixed 0.05         -> count grows linearly
#   (c) sizes never grow, almost sure rule      -> count grows linearly
#                                                  (the growth condition bites)
#
# Output: out/across_studies_check.txt
# ---------------------------------------------------------------------------

source("R/01_asht_core.R")
set.seed(20260829)

K     <- 20000        # trials in the field
REPS  <- 400          # independent fields
P     <- 1.2
C     <- 4            # n_k = ceiling(C * k)

k   <- 1:K
n_k <- ceiling(C * k)

scenarios <- list(
  "linear growth, almost sure p=1.2" = asht_alpha(n_k, P),
  "linear growth, fixed 0.05"        = rep(0.05, K),
  "no growth (all n=40), p=1.2"      = rep(asht_alpha(40, P), K)
)

sink("out/across_studies_check.txt", split = TRUE)
cat("A field of", K, "true-null trials, each tested once;", REPS, "independent fields.\n")
cat("Sizes n_k = ceiling(", C, "* k) where they grow.\n\n", sep = "")

for (nm in names(scenarios)) {
  a <- scenarios[[nm]]
  cnt <- replicate(REPS, sum(runif(K) < a))
  cat(sprintf("--- %s ---\n", nm))
  cat(sprintf("  theoretical expectation  sum_k alpha_k = %s\n",
              ifelse(sum(a) > 1e4, sprintf("%.0f", sum(a)), sprintf("%.3f", sum(a)))))
  cat(sprintf("  simulated mean count                   = %.3f\n", mean(cnt)))
  cat(sprintf("  simulated median / max                 = %.0f / %.0f\n",
              median(cnt), max(cnt)))
  cat(sprintf("  fields with zero false positives       = %.0f%%\n", 100 * mean(cnt == 0)))
  cat("\n")
}

cat("Series limit for the growing-size case, as K -> infinity:\n")
cat(sprintf("  sum_{k=1}^inf (C k)^-p = C^-p * zeta(p) = %.3f  (p = %.1f, C = %d)\n",
            C^(-P) * zeta_p(P), P, C))
cat(sprintf("  partial sum to K = %d is %.3f, so the horizon is long enough to see it settle.\n",
            K, sum(asht_alpha(n_k, P))))

cat("\nGrowth of the cumulative count with K, almost sure rule, linear sizes:\n")
a <- asht_alpha(n_k, P)
for (kk in c(100, 1000, 5000, 20000))
  cat(sprintf("  K = %5d : expected total = %.3f\n", kk, sum(a[1:kk])))
cat("\nSame for the fixed level, which is 0.05*K and diverges:\n")
for (kk in c(100, 1000, 5000, 20000))
  cat(sprintf("  K = %5d : expected total = %.0f\n", kk, 0.05 * kk))
sink()
