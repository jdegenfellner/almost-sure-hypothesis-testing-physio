# ===========================================================================
# Almost sure hypothesis testing: simulating the core claim (Borel-Cantelli).
#
# Under a TRUE null (mu = 0), test at every sample size n with statistic
# Z_n = sqrt(n)*xbar_n.  Reject when |Z_n| exceeds the bar.
#
#   FIXED bar 1.96 : per-test rejection rate = P(|Z_n|>1.96) = 0.05 for EVERY n.
#                    Sum of rates = 0.05*n -> infinity  => infinitely many
#                    false rejections, with probability one (limsup, LIL).
#   ASHT  bar c_n  = qnorm(1 - n^-p / 2):  per-test rate = n^-p, which decays.
#                    Sum of rates = sum n^-p = zeta(p) < infinity for p>1
#                    => only finitely many false rejections, w.p. 1.
#
# LEFT  panel: per-test rejection rate vs n (the summands).
# RIGHT panel: cumulative # false rejections vs n (the sums). Sim vs theory.
# ===========================================================================

set.seed(42)

n_max <- 1e6          # horizon
N     <- 150          # independent paths
P     <- c(1.2, 2)    # ASHT decay exponents

nn        <- 1:n_max
fixed_bar <- 1.96
asht_bar  <- sapply(P, function(p) qnorm(1 - nn^(-p) / 2))   # n_max x length(P)

cum_fixed <- numeric(n_max)
cum_asht  <- matrix(0, n_max, length(P))
last_asht <- matrix(0, N, length(P))

for (i in 1:N) {
  Z    <- abs(cumsum(rnorm(n_max)) / sqrt(nn))
  cum_fixed <- cum_fixed + cumsum(Z > fixed_bar)
  for (j in seq_along(P)) {
    cr_a <- Z > asht_bar[, j]
    cum_asht[, j]   <- cum_asht[, j] + cumsum(cr_a)
    last_asht[i, j] <- if (any(cr_a)) max(which(cr_a)) else NA
  }
}
mean_fixed <- cum_fixed / N
mean_asht  <- cum_asht  / N

# ---- empirical per-test rejection rate, binned in log(n) ------------------
bb   <- unique(round(exp(seq(log(2), log(n_max), length.out = 19))))
mid  <- sqrt(head(bb, -1) * tail(bb, -1))
rate <- function(cm) diff(cm[bb]) / diff(bb)
rate_f  <- rate(mean_fixed)
rate_a1 <- rate(mean_asht[, 1]); rate_a2 <- rate(mean_asht[, 2])

# ---- console summary ------------------------------------------------------
cat(sprintf("\nHorizon n_max = %.0e, paths N = %d\n", n_max, N))
cat(sprintf("Mean total false rejections | fixed 1.96 : %8.0f  (theory 0.05*n = %.0f)\n",
            mean_fixed[n_max], 0.05 * n_max))
for (j in seq_along(P))
  cat(sprintf("                            | ASHT p=%.1f : %8.2f  (theory zeta(p) ~ %.2f)\n",
              P[j], mean_asht[n_max, j], sum(nn^(-P[j]))))
for (j in seq_along(P))
  cat(sprintf("Fraction of paths with NO ASHT(p=%.1f) rejection beyond n=1000: %.0f%%\n",
              P[j], 100 * mean(last_asht[, j] < 1000, na.rm = TRUE)))

# ---- figure ---------------------------------------------------------------
gi   <- unique(round(exp(seq(log(2), log(n_max), length.out = 1200))))
cols <- c(fixed = "firebrick", a1 = "steelblue", a2 = "darkgreen")

png("figures/asht_infinitely_often.png", width = 3214, height = 1371, res = 300)
par(mfrow = c(1, 2), mar = c(4.6, 4.6, 3.2, 1), cex.lab = 1.04)

## -- LEFT: per-test rejection rate (the summands)
plot(gi, rep(0.05, length(gi)), type = "l", log = "xy", col = cols["fixed"], lwd = 2.4,
     ylim = c(1e-7, 1), xlab = "n  (log scale)",
     ylab = "per-test false-rejection rate  (log scale)",
     main = "Rejection rate at each sample size")
lines(gi, gi^(-P[1]), col = cols["a1"], lwd = 2.4)
lines(gi, gi^(-P[2]), col = cols["a2"], lwd = 2.4)
# drop empty bins (observed rate = 0) instead of pinning them to the axis floor
naz <- function(x) replace(x, !(x > 0), NA)
points(mid, naz(rate_f ), pch = 16, col = cols["fixed"], cex = 0.7)
points(mid, naz(rate_a1), pch = 16, col = cols["a1"],    cex = 0.7)
points(mid, naz(rate_a2), pch = 16, col = cols["a2"],    cex = 0.7)
legend("bottomleft", bty = "n", cex = 0.82,
       legend = c("fixed 1.96  = 0.05  (sum = 0.05 n -> infinity)",
                  expression("ASHT p=1.2  = n"^-1.2 * "  (sum < infinity)"),
                  expression("ASHT p=2  = n"^-2 * "  (sum < infinity)"),
                  "dots = simulated rate"),
       lwd = c(2.4, 2.4, 2.4, NA), pch = c(NA, NA, NA, 16),
       lty = c(1, 1, 1, NA), col = c(cols["fixed"], cols["a1"], cols["a2"], "grey40"))

## -- RIGHT: cumulative false rejections (the sums), sim vs theory
plot(gi, mean_fixed[gi], type = "l", log = "xy", col = cols["fixed"], lwd = 2.4,
     xlab = "n  (log scale)", ylab = "mean cumulative # false rejections  (log scale)",
     main = "Cumulative false rejections", ylim = c(0.5, 1e5))
lines(gi, 0.05 * gi, lty = 2, col = cols["fixed"])
lines(gi, mean_asht[gi, 1], col = cols["a1"], lwd = 2.4)
lines(gi, cumsum(nn^(-P[1]))[gi], lty = 2, col = cols["a1"])
lines(gi, mean_asht[gi, 2], col = cols["a2"], lwd = 2.4)
lines(gi, cumsum(nn^(-P[2]))[gi], lty = 2, col = cols["a2"])
legend("topleft", bty = "n", cex = 0.82,
       legend = c("fixed 1.96  (sim)", "  theory  0.05 n  -> infinity",
                  "ASHT p=1.2  (sim)", "ASHT p=2  (sim)",
                  expression("  theory  " * Sigma * " n"^-p * " = zeta(p) < " * infinity)),
       lwd = c(2.4, 1, 2.4, 2.4, 1), lty = c(1, 2, 1, 1, 2),
       col = c(cols["fixed"], cols["fixed"], cols["a1"], cols["a2"], "grey30"))
dev.off()
message("wrote figures/asht_infinitely_often.png")
