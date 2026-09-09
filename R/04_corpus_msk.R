# ---------------------------------------------------------------------------
# 04_corpus_msk.R -- the almost sure rule applied to a whole field.
#
# Bleakley et al. (Sports Med Open 2025;11:134) report, in their open
# supplementary file 3, the two arm sizes and the observed standardised mean
# difference for each of 266 randomised trials of conservative musculoskeletal
# management, drawn from 41 meta-analyses.  From (n1, n2, SMD) the z statistic
# of the comparison follows, and with it the verdict under any threshold rule.
#
# Caveat carried into the paper: the SMD tabulated is the effect the trial
# contributed to its meta-analysis, which is not always the trial's own
# pre-specified primary outcome, and it is reported as an absolute value, so
# only |z| is recoverable.  The corpus therefore describes the field's
# distribution of evidence, not a re-adjudication of 266 primary endpoints.
#
# Outputs: out/corpus_verdicts.csv, out/corpus_summary.txt,
#          figures/asht_corpus.png
# ---------------------------------------------------------------------------

source("R/01_asht_core.R")
library(readxl)
PS <- c(1.1, 1.2, 1.5, 2)

## The supplement is not redistributed here: it is licensed CC BY-NC-ND by the
## publisher, so the script fetches it from the article's own media store and
## caches it locally (the cache is git-ignored).
XLSX <- "data/bleakley2025_S3.xlsx"
if (!file.exists(XLSX)) {
  url <- paste0("https://static-content.springer.com/esm/",
                "art%3A10.1186%2Fs40798-025-00908-8/MediaObjects/",
                "40798_2025_908_MOESM3_ESM.xlsx")
  message("downloading Bleakley et al. 2025 supplementary file 3 ...")
  utils::download.file(url, XLSX, mode = "wb", quiet = TRUE)
}

d <- read_excel(XLSX,
                sheet = "Constituent RCTs (no duplicate)", skip = 1,
                .name_repair = "minimal")
names(d)[c(1, 2, 3, 4, 5, 13)] <- c("author", "year", "n1", "n2", "n", "smd")
d <- data.frame(lapply(d[, c("author", "year", "n1", "n2", "n", "smd")],
                       function(x) if (is.character(x)) x else as.numeric(x)),
                stringsAsFactors = FALSE)
d$n1 <- as.numeric(d$n1); d$n2 <- as.numeric(d$n2)
d$n  <- as.numeric(d$n);  d$smd <- as.numeric(d$smd)
d <- d[complete.cases(d[, c("n1", "n2", "smd")]) & d$smd > 0, ]

## standard error of Hedges' g / Cohen's d, and the resulting |z|
d$se_smd <- sqrt(1 / d$n1 + 1 / d$n2 + d$smd^2 / (2 * (d$n1 + d$n2)))
d$z      <- d$smd / d$se_smd
d$p      <- 2 * pnorm(-abs(d$z))
d$crit   <- crit_exponent(d$p, d$n)

d$sig_fixed <- d$p < 0.05
d$sig_005   <- d$p < 0.005
for (p in PS) d[[sprintf("sig_p%s", p)]] <- d$p < asht_alpha(d$n, p)

write.csv(d, "out/corpus_verdicts.csv", row.names = FALSE)

sink("out/corpus_summary.txt", split = TRUE)
cat("=== 266 musculoskeletal RCTs (Bleakley et al. 2025, open supplement) ===\n")
cat(sprintf("usable trials: %d\n", nrow(d)))
cat(sprintf("total n: median %.1f, IQR %.0f to %.0f, range %d to %d\n",
            median(d$n), quantile(d$n, .25), quantile(d$n, .75),
            min(d$n), max(d$n)))
cat(sprintf("|SMD|: median %.2f, IQR %.2f to %.2f\n\n",
            median(d$smd), quantile(d$smd, .25), quantile(d$smd, .75)))

cat("--- how many of the ", nrow(d), " comparisons are rejected ---\n", sep = "")
cat(sprintf("fixed alpha = 0.05 : %3d (%.0f%%)\n",
            sum(d$sig_fixed), 100 * mean(d$sig_fixed)))
cat(sprintf("fixed alpha = 0.005: %3d (%.0f%%)\n",
            sum(d$sig_005), 100 * mean(d$sig_005)))
for (p in PS)
  cat(sprintf("almost sure p = %-3s: %3d (%.0f%%)\n", p,
              sum(d[[sprintf("sig_p%s", p)]]), 100 * mean(d[[sprintf("sig_p%s", p)]])))

s <- subset(d, sig_fixed)
cat(sprintf("\n--- of the %d significant at 0.05, how many survive ---\n", nrow(s)))
for (p in PS)
  cat(sprintf("p = %-3s: %3d of %d (%.0f%%)\n", p,
      sum(s[[sprintf("sig_p%s", p)]]), nrow(s), 100 * mean(s[[sprintf("sig_p%s", p)]])))
cat(sprintf("critical exponent p* of these: median %.2f, IQR %.2f to %.2f\n",
            median(s$crit), quantile(s$crit, .25), quantile(s$crit, .75)))
cat(sprintf("with p* <= 1 (survive under NO admissible almost sure rule): %d of %d (%.0f%%)\n",
            sum(s$crit <= 1), nrow(s), 100 * mean(s$crit <= 1)))

cat("\n--- P values of the comparisons the rule removes vs keeps ---\n")
for (p in PS) {
  keep <- s[[sprintf("sig_p%s", p)]]
  cat(sprintf("p = %-3s: removed median P %.4f (n=%d), kept median P %.5f (n=%d)\n",
      p, median(s$p[!keep]), sum(!keep), median(s$p[keep]), sum(keep)))
}

cat("\n--- survival by trial size (of those significant at 0.05, p = 1.2) ---\n")
br <- cut(s$n, c(0, 30, 50, 100, Inf), labels = c("<30", "30-50", "50-100", ">100"))
tb <- table(br, s$sig_p1.2)
for (lv in levels(br)) {
  k <- sum(br == lv)
  cat(sprintf("n %-7s: %2d of %2d survive (%.0f%%)\n", lv,
              sum(br == lv & s$sig_p1.2), k, 100 * mean(s$sig_p1.2[br == lv])))
}

cat("\n--- is the almost sure rule just a fixed lower alpha? ---\n")
for (p in PS) {
  a <- d$sig_005; b <- d[[sprintf("sig_p%s", p)]]
  cat(sprintf("p = %-3s vs fixed 0.005: identical verdict in %3d of %d (%.0f%%)\n",
              p, sum(a == b), nrow(d), 100 * mean(a == b)))
}
cat("\nWhere the two rules disagree, the almost sure rule is the stricter one\n")
cat("in the larger trials and the more permissive one in the smallest:\n")
for (p in PS) {
  dis <- which(d$sig_005 != d[[sprintf("sig_p%s", p)]])
  if (length(dis)) {
    stricter <- d$sig_005[dis] & !d[[sprintf("sig_p%s", p)]][dis]
    cat(sprintf("p = %-3s: %d disagreements, median n where almost sure is stricter %s, more permissive %s\n",
        p, length(dis),
        ifelse(any(stricter),  sprintf("%.0f", median(d$n[dis][stricter])),  "-"),
        ifelse(any(!stricter), sprintf("%.0f", median(d$n[dis][!stricter])), "-")))
  }
}
## ---------------------------------------------------------------------------
## What the rule does to a literature whose trial sizes are bounded.
## Convergence of sum_k n_k^-p needs the sizes to grow.  Real fields cannot
## grow them without bound, so the level is bounded below, the sum diverges,
## and false positives accumulate linearly -- only at a lower rate.  The rate
## is the mean level over the field's own size distribution.
## ---------------------------------------------------------------------------
cat("\n--- mean level over the corpus size distribution (bounded sizes) ---\n")
cat(sprintf("fixed 0.05         : %.5f  -> %5.1f per 1000 true nulls\n", 0.05, 50))
for (p in PS) {
  r <- mean(d$n^-p)
  cat(sprintf("almost sure p = %-3s: %.5f  -> %5.1f per 1000 true nulls (%.1f times slower)\n",
              p, r, 1000 * r, 0.05 / r))
}

sink()

## --- figure: every trial in the field against the two bars -----------------
png("figures/asht_corpus.png", width = 3000, height = 1300, res = 300)
par(mfrow = c(1, 2), mar = c(4.5, 4.6, 3.0, 1.0), cex.lab = 1.03)

nn <- round(exp(seq(log(10), log(4000), length.out = 300)))
plot(d$n, d$z, log = "xy", pch = 16, col = adjustcolor("grey25", 0.55), cex = 0.6,
     xlim = c(10, 4000), ylim = c(0.05, 30),
     xlab = "total sample size n  (log scale)", ylab = "|z|  (log scale)",
     main = sprintf("%d musculoskeletal trials against each bar", nrow(d)))
abline(h = 1.96, col = "firebrick", lwd = 2.2)
abline(h = qnorm(1 - 0.005 / 2), col = "grey40", lwd = 1.8, lty = 3)
lines(nn, asht_crit(nn, 1.2), col = "#3B7DBF", lwd = 2.4)
lines(nn, asht_crit(nn, 2),   col = "#2E8B57", lwd = 2.4)
legend("bottomright", bty = "n", cex = 0.78,
       legend = c("fixed 1.96", "fixed 0.005", "almost sure p = 1.2", "p = 2"),
       lwd = c(2.2, 1.8, 2.4, 2.4), lty = c(1, 3, 1, 1),
       col = c("firebrick", "grey40", "#3B7DBF", "#2E8B57"))

h <- hist(log10(d$n), breaks = 22, plot = FALSE)
plot(h, col = "grey85", border = "white", axes = FALSE,
     xlab = "total sample size n  (log scale)", ylab = "number of trials",
     main = sprintf("Trial size in the corpus, median n = %.1f", median(d$n)))
ax <- c(10, 30, 100, 300, 1000)
axis(1, at = log10(ax), labels = ax); axis(2)
abline(v = log10(median(d$n)), col = "firebrick", lwd = 2, lty = 2)
dev.off()
message("wrote figures/asht_corpus.png")
