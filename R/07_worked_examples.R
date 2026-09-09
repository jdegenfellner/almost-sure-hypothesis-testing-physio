# ---------------------------------------------------------------------------
# 07_worked_examples.R -- worked examples spanning the whole size range of
# physiotherapy trials, from n = 140 to n = 2249, recomputed from the summary
# statistics reported in the open-access source articles.
#
#  Cui 2023 (npj Digit Med 6:121), n = 140, NCT04808141
#  UK BEAM Trial Team 2004 (BMJ 329:1377), n = 1334, ISRCTN32683578
#  Salisbury 2013, PhysioDirect (BMJ 346:f43), n = 2249, ISRCTN55666618
#
# Where a 95% confidence interval is reported, the standard error follows as
# (upper - lower)/(2 * 1.96) and the z statistic as estimate/SE.
#
# Outputs: out/worked_examples.csv, out/worked_examples.txt
# ---------------------------------------------------------------------------

source("R/01_asht_core.R")
PS <- c(1.1, 1.2, 1.5, 2)

se_from_ci <- function(lo, hi) (hi - lo) / (2 * qnorm(0.975))

ex <- list(
  # The ODI interval reported by Cui and colleagues is not symmetric about
  # the median difference, so no standard error is recoverable from it; the
  # reported P value is used directly.  For dropout the difference in
  # proportions and its pooled standard error are recomputed here (see
  # verify_correia.R), giving z = 2.54; the trial reported P = 0.019 from a
  # continuity-corrected chi-square, which is the value adjudicated below.
  list(trial = "Cui 2023, digital vs conventional physiotherapy",
       outcome = "ODI change, 8 weeks (primary)",
       n = 140, est = -0.55, lo = NA, hi = NA, se = NA, p = 0.412),
  list(trial = "Cui 2023, digital vs conventional physiotherapy",
       outcome = "difference in dropout (secondary)",
       n = 140, est = 0.186, lo = NA, hi = NA, se = 0.0732, p = 0.019),
  list(trial = "UK BEAM 2004, exercise vs best care",
       outcome = "Roland Morris, 3 months (primary)",
       n = 1334, est = 1.4, lo = 0.6, hi = 2.1, p = NA),
  list(trial = "UK BEAM 2004, manipulation vs best care",
       outcome = "Roland Morris, 12 months (primary)",
       n = 1334, est = 1.0, lo = 0.2, hi = 1.8, p = NA),
  list(trial = "UK BEAM 2004, manipulation then exercise vs best care",
       outcome = "Roland Morris, 3 months (primary)",
       n = 1334, est = 1.9, lo = 1.2, hi = 2.6, p = NA),
  list(trial = "PhysioDirect 2013, telephone vs usual care",
       outcome = "SF-36v2 physical component, 6 months (primary)",
       n = 2249, est = -0.01, lo = -0.80, hi = 0.79, p = NA),
  list(trial = "PhysioDirect 2013, telephone vs usual care",
       outcome = "overall satisfaction, 6 months (secondary)",
       n = 2249, est = -3.8, lo = -7.3, hi = -0.3, p = NA)
)

d <- do.call(rbind, lapply(ex, function(e) {
  se <- if (!is.null(e$se)) e$se else if (is.na(e$lo)) NA else se_from_ci(e$lo, e$hi)
  pv <- if (!is.na(e$p)) e$p else 2 * pnorm(-abs(e$est / se))
  z  <- if (!is.na(se)) e$est / se else qnorm(1 - pv / 2)
  data.frame(trial = e$trial, outcome = e$outcome, n = e$n, est = e$est,
             se = se, z = z, p = pv, stringsAsFactors = FALSE)
}))

d$crit      <- crit_exponent(d$p, d$n)
d$sig_fixed <- d$p < 0.05
d$sig_005   <- d$p < 0.005
for (p in PS) d[[sprintf("bar_p%s", p)]] <- asht_crit(d$n, p)
for (p in PS) d[[sprintf("sig_p%s", p)]] <- d$p < asht_alpha(d$n, p)

write.csv(d, "out/worked_examples.csv", row.names = FALSE)

sink("out/worked_examples.txt", split = TRUE)
yn <- function(x) ifelse(x, "reject", "-")
for (i in seq_len(nrow(d))) {
  cat(sprintf("\n%s\n  %s\n", d$trial[i], d$outcome[i]))
  cat(sprintf("  n = %d   estimate = %.3f   SE = %s   |z| = %.2f   P = %.4g\n",
      d$n[i], d$est[i], ifelse(is.na(d$se[i]), "-", sprintf("%.3f", d$se[i])),
      abs(d$z[i]), d$p[i]))
  cat(sprintf("  critical exponent p* = %.2f  -> %s\n", d$crit[i],
      ifelse(d$crit[i] > 1, "survives some admissible rule",
                            "survives NO admissible rule")))
  cat(sprintf("  bars on |z|: classical 1.96"))
  for (p in PS) cat(sprintf(" | p=%s: %.2f", p, d[[sprintf("bar_p%s", p)]][i]))
  cat(sprintf("\n  verdicts:    %s", yn(d$sig_fixed[i])))
  for (p in PS) cat(sprintf(" | %s", yn(d[[sprintf("sig_p%s", p)]][i])))
  cat("\n")
}
cat("\n\nThe two large trials show the other edge of the rule: at n = 1334 and\n")
cat("n = 2249 the classical bar certifies differences of one point on a\n")
cat("24-point disability scale and 3.8 percentage points of satisfaction,\n")
cat("while the almost sure bar, which has risen to |z| ~ 3.6 to 4.0, does not.\n")
sink()
