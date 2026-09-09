# ---------------------------------------------------------------------------
# 05_choosing_p.R -- the quantities that fix the exponent p before seeing
# the data, and the implicit error trade-off the choice commits one to.
#
# Outputs: out/choosing_p.txt
# ---------------------------------------------------------------------------

source("R/01_asht_core.R")
PS <- c(1.1, 1.2, 1.5, 2)

sink("out/choosing_p.txt", split = TRUE)

cat("=== (1) expected total number of false rejections, zeta(p) ===\n")
cat("Under a true null the whole sequence of tests n = 1, 2, ... produces\n")
cat("sum_n alpha_n = zeta(p) false rejections in expectation.\n\n")
cat(sprintf("%6s %10s %14s\n", "p", "zeta(p)", "partial to 1e6"))
for (p in PS) cat(sprintf("%6.1f %10.3f %14.3f\n", p, zeta_p(p), zeta_partial(p, 1e6)))

cat("\n=== (2) the level the rule implies at a given sample size (Table 1) ===\n")
cat("alpha_n = n^{-p}.  Conversely, to hit a target level at a planned n,\n")
cat("set p = log(1/alpha_target) / log(n).\n\n")
cat(sprintf("%6s %10s %10s %10s %10s %10s\n", "n", "p=1.1", "p=1.2", "p=1.5", "p=2", "p for .005"))
for (n in c(20, 40, 100, 140, 200, 500, 1000, 4000))
  cat(sprintf("%6d %10.2g %10.2g %10.2g %10.2g %10.2f\n", n,
      asht_alpha(n, 1.1), asht_alpha(n, 1.2), asht_alpha(n, 1.5),
      asht_alpha(n, 2), log(1 / 0.005) / log(n)))

cat("\n=== (3) the implicit cost of a type I relative to a type II error ===\n")
cat("A level alpha minimises w*alpha + beta(alpha) when w = phi(c-ncp)/(2*phi(c)),\n")
cat("with c the critical value and ncp = delta*sqrt(n)/2.  Reading that identity\n")
cat("backwards gives the relative cost each rule commits one to.\n\n")
implied_w <- function(alpha, delta, n) {
  c0 <- qnorm(1 - alpha / 2); ncp <- delta * sqrt(n) / 2
  dnorm(c0 - ncp) / (2 * dnorm(c0))
}
cat(sprintf("%6s %7s %10s %10s %10s %10s %10s\n",
            "n", "delta", "a=.05", "p=1.1", "p=1.2", "p=1.5", "p=2"))
for (n in c(40, 140, 500, 1000)) for (dl in c(0.3, 0.5)) {
  cat(sprintf("%6d %7.1f %10.1f %10.1f %10.1f %10.1f %10.1f\n", n, dl,
      implied_w(0.05, dl, n),
      implied_w(asht_alpha(n, 1.1), dl, n), implied_w(asht_alpha(n, 1.2), dl, n),
      implied_w(asht_alpha(n, 1.5), dl, n), implied_w(asht_alpha(n, 2),   dl, n)))
}

cat("\n=== (4) sample size cost, Table 3 (see out/required_n.csv) ===\n")
req <- read.csv("out/required_n.csv")
print(req, row.names = FALSE, digits = 3)

sink()
