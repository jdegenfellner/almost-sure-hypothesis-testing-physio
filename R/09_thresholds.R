# ---------------------------------------------------------------------------
# 09_thresholds.R -- Table 1: the rejection threshold on |z| under the fixed
# rule and under almost sure testing, across the sample sizes physiotherapy
# trials span.
#
# At p = 1.2 this reproduces Naaman's own Table 1 (Electron J Stat
# 2016;10:1526-50), whose Eq. (3.5) is the same exponent: 2.88 at n = 100 and
# 3.66 at n = 1000.
# ---------------------------------------------------------------------------

source("R/01_asht_core.R")

NS <- c(20, 40, 100, 200, 1000, 4000)
PS <- c(1.2, 2)

tab <- data.frame(n = NS, z_classical = qnorm(0.975))
for (p in PS) {
  tab[[sprintf("alpha_p%s", p)]] <- asht_alpha(NS, p)
  tab[[sprintf("zbar_p%s",  p)]] <- asht_crit(NS, p)
}
write.csv(tab, "out/thresholds.csv", row.names = FALSE)

sink("out/thresholds.txt")
cat("=== Table 1: rejection threshold on |z| by total sample size ===\n\n")
cat(sprintf("%8s %10s %12s %8s %12s %8s\n",
            "n", "classical", "alpha p=1.2", "z p=1.2", "alpha p=2", "z p=2"))
for (i in seq_len(nrow(tab)))
  cat(sprintf("%8d %10.2f %12.3g %8.2f %12.3g %8.2f\n", tab$n[i], tab$z_classical[i],
              tab$alpha_p1.2[i], tab$zbar_p1.2[i], tab$alpha_p2[i], tab$zbar_p2[i]))

cat("\n--- check against Naaman's Table 1, two-sided, Eq. (3.5) = n^(-6/5) ---\n")
nm <- data.frame(n = c(10, 100, 1000, 1e6), naaman = c(1.86, 2.88, 3.66, 5.41))
nm$ours <- asht_crit(nm$n, 1.2)
for (i in seq_len(nrow(nm)))
  cat(sprintf("n = %7g   Naaman %.2f   recomputed %.2f   %s\n", nm$n[i], nm$naaman[i],
              nm$ours[i], ifelse(abs(nm$naaman[i] - nm$ours[i]) < 0.005, "match", "DIFFERS")))

cat("\n--- where the almost sure level crosses the fixed 0.05 and 0.005 ---\n")
for (p in c(1.1, 1.2, 1.5, 2))
  cat(sprintf("p = %-3s: exceeds 0.05 below n = %5.1f;  falls below 0.005 above n = %6.1f\n",
              p, asht_crossover(0.05, p), asht_crossover(0.005, p)))
sink()
cat("wrote out/thresholds.csv and out/thresholds.txt\n")
