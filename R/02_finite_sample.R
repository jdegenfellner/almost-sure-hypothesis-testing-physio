# ---------------------------------------------------------------------------
# 02_finite_sample.R -- finite-sample operating characteristics of the
# almost sure rule at the sample sizes physiotherapy trials actually reach.
#
# Addresses the reviewers' central objection: the almost sure guarantee is
# asymptotic, so what does the rule DO at n = 40, 140, 500?  At a given n it
# is an ordinary two-sided test of exact size alpha_n = n^{-p}, so its size
# and power are available in closed form and need no asymptotics at all.
#
# Outputs: out/finite_sample_power.csv, out/required_n.csv,
#          out/simulation_check.csv, figures/asht_power.png
# ---------------------------------------------------------------------------

source("R/01_asht_core.R")
set.seed(20260829)
dir.create("out", showWarnings = FALSE)

PS     <- c(1.1, 1.2, 1.5, 2)         # decay exponents reported throughout
DELTAS <- c(0.2, 0.5, 0.8)            # Cohen's small / medium / large SMD

# --- 1. exact size and power at fixed n ------------------------------------
grid <- expand.grid(n = c(20, 40, 100, 140, 200, 500, 1000, 2000, 4000),
                    delta = DELTAS)
grid$power_fixed <- power_fixed(grid$delta, grid$n)
for (p in PS) {
  grid[[sprintf("alpha_p%s", p)]] <- asht_alpha(grid$n, p)
  grid[[sprintf("power_p%s", p)]] <- power_asht(grid$delta, grid$n, p)
}
write.csv(grid, "out/finite_sample_power.csv", row.names = FALSE)

cat("=== exact power, two-sided, equal arms (total n) ===\n")
for (dl in DELTAS) {
  cat(sprintf("\n-- SMD delta = %.1f --\n", dl))
  g <- subset(grid, delta == dl)
  cat(sprintf("%6s %10s %10s %10s %10s %10s\n", "n", "alpha=.05",
              "p=1.1", "p=1.2", "p=1.5", "p=2"))
  for (i in seq_len(nrow(g)))
    cat(sprintf("%6d %10.3f %10.3f %10.3f %10.3f %10.3f\n", g$n[i],
                g$power_fixed[i], g$power_p1.1[i], g$power_p1.2[i],
                g$power_p1.5[i], g$power_p2[i]))
}

# --- 2. sample size needed for 80% power -----------------------------------
req <- data.frame(delta = DELTAS)
req$n_fixed <- sapply(DELTAS, n_required, rule = list(kind = "fixed", alpha = 0.05))
for (p in PS) {
  req[[sprintf("n_p%s", p)]]      <- sapply(DELTAS, n_required,
                                            rule = list(kind = "asht", p = p))
  req[[sprintf("factor_p%s", p)]] <- req[[sprintf("n_p%s", p)]] / req$n_fixed
}
write.csv(req, "out/required_n.csv", row.names = FALSE)

cat("\n\n=== total sample size for 80% power, and inflation over alpha=0.05 ===\n")
cat(sprintf("%6s %9s %9s %7s %9s %7s %9s %7s %9s %7s\n", "delta", "a=.05",
            "p=1.1", "x", "p=1.2", "x", "p=1.5", "x", "p=2", "x"))
for (i in seq_len(nrow(req)))
  cat(sprintf("%6.1f %9d %9d %7.2f %9d %7.2f %9d %7.2f %9d %7.2f\n",
      req$delta[i], req$n_fixed[i],
      req$n_p1.1[i], req$factor_p1.1[i], req$n_p1.2[i], req$factor_p1.2[i],
      req$n_p1.5[i], req$factor_p1.5[i], req$n_p2[i],   req$factor_p2[i]))

# --- 3. simulation check: the exact size really is alpha_n -----------------
# Two-sample t test on normal data; the rule is applied on the p-value scale
# (reject when P < alpha_n), which is how it would be used in practice.
B <- 2e5
sim <- do.call(rbind, lapply(c(40, 140, 500), function(n) {
  na <- n / 2
  x  <- matrix(rnorm(B * na), B, na); y <- matrix(rnorm(B * na), B, na)
  pv <- 2 * pt(-abs((rowMeans(x) - rowMeans(y)) /
        sqrt((apply(x, 1, var) + apply(y, 1, var)) / na)), df = n - 2)
  out <- data.frame(n = n, rule = c("fixed 0.05", sprintf("p=%s", PS)),
                    nominal = c(0.05, asht_alpha(n, PS)))
  out$observed <- c(mean(pv < 0.05), sapply(PS, function(p) mean(pv < asht_alpha(n, p))))
  out$mcse <- sqrt(out$nominal * (1 - out$nominal) / B)
  out
}))
write.csv(sim, "out/simulation_check.csv", row.names = FALSE)
sim$dev_mcse <- abs(sim$observed - sim$nominal) / sim$mcse
cat("\n\n=== simulated type I error under a true null (B = 2e5 t tests) ===\n")
print(sim, row.names = FALSE, digits = 4)

# --- 4. power figure --------------------------------------------------------
png("figures/asht_power.png", width = 3000, height = 1150, res = 300)
par(mfrow = c(1, 3), mar = c(4.4, 4.4, 3.0, 0.8), cex.lab = 1.05)
cols <- c("firebrick", "#3B7DBF", "#2E8B57", "#8B5A2B", "grey30")
nn   <- round(exp(seq(log(10), log(4000), length.out = 400)))
for (dl in DELTAS) {
  plot(nn, power_fixed(dl, nn), type = "l", log = "x", lwd = 2.4, col = cols[1],
       ylim = c(0, 1), xlab = "total sample size n  (log scale)", ylab = "power",
       main = bquote("SMD" ~ delta == .(dl)))
  for (j in seq_along(PS))
    lines(nn, power_asht(dl, nn, PS[j]), lwd = 2.2, col = cols[j + 1])
  abline(h = 0.8, lty = 3, col = "grey50")
  if (dl == DELTAS[1])
    legend("topleft", bty = "n", cex = 0.85, lwd = 2.2, col = cols[1:5],
           legend = c(expression(alpha == 0.05), "almost sure, p = 1.1",
                      "p = 1.2", "p = 1.5", "p = 2"))
}
dev.off()
message("wrote figures/asht_power.png")
