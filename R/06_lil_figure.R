# ---------------------------------------------------------------------------
# 06_lil_figure.R -- replacement for Figure 1.
#
# The first version showed ONE path of Z_n, which happened never to cross
# 1.96 by n = 10^6, inviting exactly the misreading a reviewer anticipated.
# Panel A now shows an ensemble, marks the crossings, and reports how wildly
# the realised number of crossings varies around its expectation, because the
# Z_n along a path are strongly dependent.
#
# Outputs: figures/asht_lil.png, out/lil_crossings.txt
# ---------------------------------------------------------------------------

n_max <- 1e6
n     <- 1:n_max
idx   <- unique(round(exp(seq(log(3), log(n_max), length.out = 3000))))
NPATH <- 150

set.seed(1)                                   # the path shown in version 1
Z1 <- cumsum(rnorm(n_max)) / sqrt(n)

set.seed(42)
paths <- vector("list", NPATH); cnt <- exc <- numeric(NPATH)
for (i in 1:NPATH) {
  z <- cumsum(rnorm(n_max)) / sqrt(n)
  cr <- abs(z) > 1.96
  cnt[i] <- sum(cr); exc[i] <- sum(diff(c(FALSE, cr)) == 1)
  if (i <= 24) paths[[i]] <- z[idx]
}
busy <- which.max(cnt)                        # a path that crosses often
set.seed(42); for (i in 1:busy) zb <- cumsum(rnorm(n_max)) / sqrt(n)

sink("out/lil_crossings.txt", split = TRUE)
cat("Crossings of the fixed line |Z_n| > 1.96, horizon n = 10^6, 150 paths\n\n")
cat(sprintf("expected number of n with |Z_n| > 1.96 : %.0f  (= 0.05 n)\n", 0.05 * n_max))
cat(sprintf("realised, mean over 150 paths          : %.0f\n", mean(cnt)))
cat(sprintf("realised, median                       : %.0f\n", median(cnt)))
cat(sprintf("realised, range                        : %.0f to %.0f\n", min(cnt), max(cnt)))
cat(sprintf("paths with NO crossing at all          : %d of %d\n", sum(cnt == 0), NPATH))
cat(sprintf("separate excursions above the line     : median %.0f, range %.0f to %.0f\n",
            median(exc), min(exc), max(exc)))
cat(sprintf("\nthe single path of the first version (seed 1): %d crossings, max |Z_n| = %.2f\n",
            sum(abs(Z1) > 1.96), max(abs(Z1))))
sink()

env <- sqrt(2 * log(log(n))); env[!is.finite(env)] <- NA

png("figures/asht_lil.png", width = 3214, height = 1200, res = 300)
par(mfrow = c(1, 2), mar = c(4.5, 4.6, 2.8, 1), cex.lab = 1.05)

## Panel A -- an ensemble, not one path
plot(idx, Z1[idx], type = "n", log = "x", ylim = c(-5.0, 3.4), yaxt = "n",
     xlab = "n  (log scale)", ylab = expression(Z[n]),
     main = "A fixed line is crossed infinitely often")
axis(2, at = -3:3)
abline(h = 0, col = "grey90")
for (i in seq_along(paths)) if (!is.null(paths[[i]]))
  lines(idx, paths[[i]], col = adjustcolor("grey70", 0.45), lwd = 0.5)
lines(idx, zb[idx], col = "#2E6DA4", lwd = 0.9)
cr <- idx[abs(zb[idx]) > 1.96]
points(cr, zb[cr], pch = 16, cex = 0.30, col = "#D95F02")
lines(idx, Z1[idx], col = "grey25", lwd = 0.9)
abline(h = c(-1.96, 1.96), col = "steelblue", lty = 2)
lines(idx,  env[idx], col = "firebrick", lwd = 2.4)
lines(idx, -env[idx], col = "firebrick", lwd = 2.4)
legend("bottom", bty = "n", cex = 0.72, y.intersp = 0.95,
       legend = c("the single path shown previously (never crosses)",
                  "a path that crosses repeatedly (crossings marked)",
                  "22 further paths",
                  "envelope  +/- sqrt(2 log log n)",
                  "fixed line  +/- 1.96"),
       col = c("grey25", "#2E6DA4", "grey70", "firebrick", "steelblue"),
       lwd = c(0.9, 0.9, 0.5, 2.4, 1), lty = c(1, 1, 1, 1, 2))

## Panel B -- the three growth scales (unchanged)
nn <- 10^seq(1, 12, by = 0.1)
plot(nn, sqrt(nn), type = "l", log = "xy", col = "grey25", lwd = 2.4,
     xlab = "n  (log scale)", ylab = "growth rate  (log scale)",
     main = "Threshold between noise and signal")
lines(nn, sqrt(2 * 2 * log(nn)),  col = "steelblue", lwd = 2.4)
lines(nn, sqrt(2 * log(log(nn))), col = "firebrick", lwd = 2.4)
legend("topleft", bty = "n", cex = 0.78,
       legend = c("signal  ~ sqrt(n)",
                  "almost sure threshold  c_n ~ sqrt(2p log n)",
                  "null fluctuations  ~ sqrt(2 log log n)"),
       col = c("grey25", "steelblue", "firebrick"), lwd = 2.4)
dev.off()
message("wrote figures/asht_lil.png")
