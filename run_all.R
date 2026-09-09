# ---------------------------------------------------------------------------
# run_all.R -- reproduces every number, table and figure of revision 1.
# Run from the repository root:  Rscript run_all.R
#
# Step 00 refits the 21 trials from their raw deposits and needs those files;
# set IPD_DIR to where they are held.  Its output, data/ipd_reference_specs.csv,
# is committed here, so every later step runs without it and reproduces every
# number, table and figure.
# ---------------------------------------------------------------------------

steps <- c(
  "R/00_export_ipd_reference_specs.R",  # optional; refits the IPD from raw deposits
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
  ok <- tryCatch({ source(s, echo = FALSE); TRUE },
                 error = function(e) { message("SKIPPED (expected unless the raw deposits are present): ",
                                    conditionMessage(e)); FALSE })
  if (!ok && !grepl("00_export", s)) stop("failed at ", s)
}
message("\nAll steps complete. See out/ and figures/.")
