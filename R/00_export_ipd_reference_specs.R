# ---------------------------------------------------------------------------
# 00_export_ipd_reference_specs.R
#
# Exports one pre-specified reference analysis per trial from the open-data
# reanalysis, so that the present paper's reanalysis is
# reproducible from a single flat file.
#
# Source of the individual participant data: 21 randomised trials in
# physiotherapy, rehabilitation and exercise science whose individual
# participant data are openly deposited (Zenodo, Dryad, journal supplements).
# The reference specification for each trial is: ANCOVA on the primary
# endpoint adjusting for its baseline value where the deposit provides one,
# otherwise the endpoint alone; the covariates adjusted for in the published
# analysis; no outlier handling; a random intercept where the trial was
# cluster randomised; model-based standard errors.
#
# Run where the deposited trial data are held. The exported flat file is what
# the present paper ships, so this script is documentation of provenance and
# is not needed to reproduce any number in the paper.
# ---------------------------------------------------------------------------

MV  <- Sys.getenv("IPD_DIR", file.path("..", "ipd-reanalysis"))   # where the deposited data live
OUT <- file.path("data", "ipd_reference_specs.csv")

d  <- read.csv(file.path(MV, "output", "reference_analyses.csv"), stringsAsFactors = FALSE)
reg <- read.csv(file.path(MV, "config", "registry.csv"), stringsAsFactors = FALSE)
sl  <- read.csv(file.path(MV, "candidates", "shortlist_with_deposited_data.csv"),
                stringsAsFactors = FALSE)

pr <- subset(d, grid_type == "principled")

ref <- do.call(rbind, lapply(split(pr, pr$study_id), function(s) {
  bl <- if ("ancova"   %in% s$baseline) "ancova"   else "endpoint"
  mo <- if ("mixed_ri" %in% s$model)    "mixed_ri" else "lm"
  cv <- if ("published" %in% s$covars)  "published" else "none"
  r  <- subset(s, baseline == bl & covars == cv & outlier == "none" &
                  model == mo & inference == "model")
  stopifnot(nrow(r) == 1)
  r
}))

ref$pmcid <- sub(".*_(PMC[0-9]+)$", "\\1", ref$study_id)
ref <- merge(ref, sl[, c("pmcid", "year", "journal", "title", "doi", "data_urls")],
             by = "pmcid", all.x = TRUE)
ref <- merge(ref, reg[, c("study_id", "outcome_var", "notes")],
             by = "study_id", all.x = TRUE)

ref$z <- ref$est / ref$se
ref   <- ref[order(ref$n), ]

keep <- c("study_id", "pmcid", "year", "journal", "title", "doi", "data_urls",
          "outcome_var", "baseline", "covars", "model",
          "n", "est", "se", "z", "p", "d")
write.csv(ref[, keep], OUT, row.names = FALSE)
cat("wrote", OUT, "with", nrow(ref), "trials\n")
