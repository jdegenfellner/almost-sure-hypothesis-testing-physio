# ---------------------------------------------------------------------------
# 01_asht_core.R -- shared helpers for the almost sure test.
#
# The rule (Naaman 2016, Theorem 2.1): test at level alpha_n = n^{-p}, p > 1,
# instead of at a fixed alpha.  At a GIVEN n this is an ordinary fixed-sample
# test whose exact size is alpha_n; the almost sure property is a statement
# about the whole sequence of tests indexed by n.
# ---------------------------------------------------------------------------

asht_alpha <- function(n, p) n^(-p)                       # level at size n
asht_crit  <- function(n, p) qnorm(1 - n^(-p) / 2)        # two-sided |z| bar

## Critical exponent: the largest p at which a result with two-sided P value
## `pval` observed at sample size n is still rejected, i.e. the solution of
## P = n^{-p}.  Reporting p* replaces the choice of a single p by a
## continuous, threshold-free summary; the finding survives some admissible
## rule exactly when p* > 1.
# crit_exponent: derived convenience, retained as a column in the output
# CSVs. It is not used in the manuscript.
crit_exponent <- function(pval, n) log(1 / pval) / log(n)

## Sample size at which alpha_n crosses a fixed level (e.g. 0.05):
## below it the almost sure rule is the MORE permissive of the two.
asht_crossover <- function(alpha_fixed, p) alpha_fixed^(-1 / p)

## Power of the two-sided level-alpha z test for a standardised mean
## difference delta, total sample size n, equal arms.
power_z <- function(delta, n, alpha) {
  ncp <- delta * sqrt(n) / 2                 # se of the difference = 2/sqrt(n)
  cc  <- qnorm(1 - alpha / 2)
  pnorm(-cc + ncp) + pnorm(-cc - ncp)
}
power_fixed <- function(delta, n, alpha = 0.05) power_z(delta, n, alpha)
power_asht  <- function(delta, n, p)           power_z(delta, n, asht_alpha(n, p))

## Total sample size required for a target power.  `rule` is either
## list(kind="fixed", alpha=) or list(kind="asht", p=).
n_required <- function(delta, target = 0.80, rule, nmax = 1e7) {
  f <- function(n) {
    a <- if (rule$kind == "fixed") rule$alpha else asht_alpha(n, rule$p)
    power_z(delta, n, a) - target
  }
  if (f(4) > 0) return(4)
  if (f(nmax) < 0) return(NA_real_)
  ceiling(uniroot(f, c(4, nmax))$root)
}

## Expected total number of false rejections over the sequence n = 1, 2, ...
## under a true null:  sum_n alpha_n = zeta(p).  Reported both as the true
## series limit and as the partial sum a simulation of finite horizon shows.
## Expected total number of false rejections over the sequence n = 1, 2, ...
## under a true null:  sum_n alpha_n = zeta(p), the Riemann zeta function.
## Euler-Maclaurin, accurate to ~1e-12 for the p used here; `zeta_partial`
## is the truncated sum a simulation with a finite horizon can actually show.
zeta_p <- function(p, N = 1e5) {
  n <- 1:(N - 1)
  sum(n^(-p)) + N^(1 - p) / (p - 1) + N^(-p) / 2 + p * N^(-p - 1) / 12
}
zeta_partial <- function(p, nmax) sum((1:nmax)^(-p))
