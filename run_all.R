# ---------------------------------------------------------------------------
# run_all.R -- reproduces every number, table and figure of revision 1.
# Run from the repository root:  Rscript run_all.R
# ---------------------------------------------------------------------------

steps <- c(
  "R/09_thresholds.R",                  # Table 1
  "R/06_lil_figure.R",                  # Figure 1
  "R/asht_simulation.R",                # Figure 2 (slow: 150 paths to n = 1e6)
  "R/02_finite_sample.R",               # Table 2, Table 3, Figure 3
  "R/07_worked_examples.R",             # Table 4
  "R/03_trials_ipd.R",                  # Table 5
  "R/04_corpus_msk.R",                  # Figure 4
  "R/05_choosing_p.R",                  # out/choosing_p.txt
  "R/08_across_studies_check.R",        # checks the Methods claim, setting (iii)
  "R/verify_correia.R"                  # checks on the Cui dropout comparison
)

for (s in steps) {
  message("\n==== ", s, " ====")
  source(s, echo = FALSE)
}
message("\nAll steps complete. See out/ and figures/.")
