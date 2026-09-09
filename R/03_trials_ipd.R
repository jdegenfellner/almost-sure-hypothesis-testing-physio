# ---------------------------------------------------------------------------
# 03_trials_ipd.R -- the almost sure rule applied to 21 randomised trials in
# physiotherapy, rehabilitation and exercise science whose INDIVIDUAL
# PARTICIPANT DATA are openly deposited.
#
# For each trial one pre-specified reference analysis of the primary outcome
# (see data/ipd_reference_specs.csv and 00_export_ipd_reference_specs.R) gives
# an estimate, a standard error and a two-sided P value.  The rule is applied
# on the P value scale: reject when P < alpha_n = n^{-p}.  We also report the
# critical exponent p* = log(1/P)/log(n), the largest p at which the finding
# would still be rejected, which summarises the verdict without fixing p.
#
# Outputs: out/ipd_verdicts.csv, out/ipd_summary.txt
# ---------------------------------------------------------------------------

source("R/01_asht_core.R")
PS <- c(1.1, 1.2, 1.5, 2)

d <- read.csv("data/ipd_reference_specs.csv", stringsAsFactors = FALSE)
d <- d[order(d$n), ]
d$label <- sprintf("%s (%s)", sub(" .*", "", sub(":.*", "", d$title)), d$year)

d$z    <- d$est / d$se
d$crit <- crit_exponent(d$p, d$n)
d$sig_fixed <- d$p < 0.05
for (p in PS) d[[sprintf("sig_p%s", p)]] <- d$p < asht_alpha(d$n, p)
d$sig_005 <- d$p < 0.005          # the "redefine significance" comparator

out <- d[, c("pmcid", "year", "n", "est", "se", "z", "p", "crit",
             "sig_fixed", "sig_005", sprintf("sig_p%s", PS))]
write.csv(out, "out/ipd_verdicts.csv", row.names = FALSE)

sink("out/ipd_summary.txt", split = TRUE)
cat("=== 21 trials with openly deposited individual participant data ===\n")
cat(sprintf("total n: median %.0f, range %d to %d\n\n",
            median(d$n), min(d$n), max(d$n)))
cat(sprintf("%-12s %5s %8s %9s %7s  %-5s %-5s %-5s %-5s %-5s %-5s\n",
            "PMCID", "n", "z", "P", "p*",
            ".05", ".005", "1.1", "1.2", "1.5", "2"))
yn <- function(x) ifelse(x, "yes", "-")
for (i in seq_len(nrow(d)))
  cat(sprintf("%-12s %5d %8.2f %9.2g %7.2f  %-5s %-5s %-5s %-5s %-5s %-5s\n",
      d$pmcid[i], d$n[i], d$z[i], d$p[i], d$crit[i],
      yn(d$sig_fixed[i]), yn(d$sig_005[i]), yn(d$sig_p1.1[i]),
      yn(d$sig_p1.2[i]), yn(d$sig_p1.5[i]), yn(d$sig_p2[i])))

cat("\n--- counts of rejected primary outcomes out of 21 ---\n")
cat(sprintf("fixed alpha = 0.05 : %d\n", sum(d$sig_fixed)))
cat(sprintf("fixed alpha = 0.005: %d\n", sum(d$sig_005)))
for (p in PS)
  cat(sprintf("almost sure p = %-3s: %d\n", p, sum(d[[sprintf("sig_p%s", p)]])))

cat("\n--- of the ", sum(d$sig_fixed), " findings significant at 0.05 ---\n", sep = "")
s <- subset(d, sig_fixed)
for (p in PS)
  cat(sprintf("survive at p = %-3s: %d of %d (%.0f%%)\n", p,
      sum(s[[sprintf("sig_p%s", p)]]), nrow(s),
      100 * mean(s[[sprintf("sig_p%s", p)]])))
cat(sprintf("critical exponent p* of these: median %.2f, range %.2f to %.2f\n",
            median(s$crit), min(s$crit), max(s$crit)))
cat(sprintf("number with p* <= 1 (survive under NO admissible rule): %d of %d\n",
            sum(s$crit <= 1), nrow(s)))

cat("\n--- agreement between the almost sure rule and a fixed 0.005 ---\n")
for (p in PS) {
  a <- d$sig_005; b <- d[[sprintf("sig_p%s", p)]]
  cat(sprintf("p = %-3s: identical verdict in %d of 21 trials (%.0f%%)\n",
              p, sum(a == b), 100 * mean(a == b)))
}
sink()
