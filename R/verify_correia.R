# verify_correia.R -- reanalysis of the one "significant" between-group finding
# in Cui et al. 2023 (npj Digital Medicine 6:121, n=140, 70/group): dropout
# 11/70 (digital) vs 24/70 (conventional), reported P=0.019.
# Checks (a) the difference-in-proportions test by several methods, (b) the
# almost sure verdict, and (c) what a multiplicity correction would do, since
# the trial applied none (all outcomes tested two-sided at a fixed alpha=0.05).

asht_crit <- function(n, p) qnorm(1 - n^(-p)/2)     # almost sure threshold on |z|

x1 <- 11; n1 <- 70          # digital
x2 <- 24; n2 <- 70          # conventional
N  <- n1 + n2
p1 <- x1/n1; p2 <- x2/n2; diff <- p2 - p1
ct <- matrix(c(x1, n1 - x1, x2, n2 - x2), 2, byrow = TRUE)

cat(sprintf("p1(digital)=%.4f  p2(conv)=%.4f  diff=%.4f (%.1f pts)\n\n",
            p1, p2, diff, 100 * diff))

## (a) difference in proportions, several ways -------------------------------
pp <- (x1 + x2)/(n1 + n2); se_pool <- sqrt(pp*(1-pp)*(1/n1 + 1/n2))
z_pool <- diff/se_pool
se_un  <- sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2); z_un <- diff/se_un
cat(sprintf("pooled z-test:     z=%.3f  two-sided p=%.4f\n",
            z_pool, 2*pnorm(-abs(z_pool))))
cat(sprintf("unpooled Wald z:   z=%.3f  two-sided p=%.4f\n",
            z_un, 2*pnorm(-abs(z_un))))
cat(sprintf("chi-square, no cc: X2=%.3f  p=%.4f\n",
            chisq.test(ct, correct = FALSE)$statistic,
            chisq.test(ct, correct = FALSE)$p.value))
cat(sprintf("chi-square +Yates: X2=%.3f  p=%.4f   <- matches reported 0.019\n",
            chisq.test(ct, correct = TRUE)$statistic,
            chisq.test(ct, correct = TRUE)$p.value))
cat(sprintf("Fisher exact:                 p=%.4f\n", fisher.test(ct)$p.value))
pt <- prop.test(c(x2, x1), c(n2, n1), correct = TRUE)
cat(sprintf("prop.test diff CI: %.4f  95%% CI [%.4f, %.4f]\n\n",
            diff, pt$conf.int[1], pt$conf.int[2]))

## (b) almost sure verdict at n=140 ------------------------------------------
z <- z_pool
for (p in c(1.1, 1.2, 1.5, 2)) {
  c_n <- asht_crit(N, p)
  cat(sprintf("ASHT p=%-3s : alpha_n=%.5g  crit=%.3f  reject(|z|=%.2f)? %s\n",
              p, N^(-p), c_n, z, abs(z) > c_n))
}
cat(sprintf("fixed alpha=.05 crit=1.96  reject? %s\n\n", abs(z) > 1.96))

## (c) multiplicity: the trial made NO correction ----------------------------
# ODI was the single pre-specified primary outcome; dropout is one of a dozen-plus
# secondary/engagement between-group comparisons, all tested at a fixed 0.05.
p_reported <- 0.019
for (m in c(5, 10, 13)) {
  cat(sprintf("Bonferroni, m=%2d tests: thresh=%.4f  dropout p=0.019 %s  (adj p=%.3f)\n",
              m, 0.05/m, ifelse(p_reported < 0.05/m, "clears", "FAILS"),
              min(1, m * p_reported)))
}
