# Almost sure hypothesis testing in physiotherapy

Code and derived data reproducing every number, table and figure of

> Degenfellner J. *Moving the goalposts on purpose: almost sure hypothesis
> testing and what it would do to physiotherapy trials.*
> Submitted to BMC Medical Research Methodology.

Almost sure hypothesis testing lets the significance level shrink with the
sample size, `α_n = n^(-p)` with `p > 1`, so that with probability one only
finitely many errors of either kind occur (Naaman, *Electron J Stat*
2016;10:1526–50). This repository asks what that rule does to real
physiotherapy trials: three open-access trials spanning *n* = 140 to 2249, the
primary outcome of 21 trials with openly deposited individual participant data,
and 256 musculoskeletal trials reanalysed from published summary statistics.

## Reproducing everything

```sh
Rscript run_all.R
```

Runs in about 15 seconds and writes `figures/` and `out/`. Beyond base R only
`readxl` is required. Developed under R 4.6.0.

Step `00` refits the 21 trials from their raw deposits and needs those files,
which are not redistributed here; it is skipped automatically when they are
absent. Its output, `data/ipd_reference_specs.csv`, is committed, so every later
step runs without it and reproduces every number in the paper.

## Layout

```
R/          analysis scripts, numbered in run order
data/       derived inputs (see provenance below)
figures/    the four figures, as submitted
out/        every table and console summary the paper quotes
run_all.R   reproduces all of the above
```

## Scripts

| script | produces |
|---|---|
| `R/01_asht_core.R` | shared helpers: `asht_alpha`, `asht_crit`, power, required *n*, ζ(p) |
| `R/06_lil_figure.R` | Figure 1 and `out/lil_crossings.txt` |
| `R/asht_simulation.R` | Figure 2 (slow: 150 paths to *n* = 10⁶) |
| `R/02_finite_sample.R` | Tables 2 and 3, Figure 3, `out/simulation_check.csv` |
| `R/07_worked_examples.R` | Table 4 |
| `R/00_export_ipd_reference_specs.R` | `data/ipd_reference_specs.csv` (needs the raw deposits; output is committed) |
| `R/03_trials_ipd.R` | Table 5 |
| `R/04_corpus_msk.R` | Figure 4, `out/corpus_*` |
| `R/05_choosing_p.R` | `out/choosing_p.txt` |
| `R/08_across_studies_check.R` | checks the Methods claim for setting (iii) |
| `R/verify_correia.R` | checks on the Cui dropout comparison |

Table 1 is computed inline from `R/01_asht_core.R`.

## Data provenance

Everything this paper uses is already public.

- **Worked examples** use summary statistics reported in the open-access
  articles: Cui et al., *npj Digit Med* 2023;6:121 (NCT04808141); UK BEAM Trial
  Team, *BMJ* 2004;329:1377 (ISRCTN32683578); Salisbury et al., *BMJ*
  2013;346:f43 (ISRCTN55666618). The numbers are transcribed in
  `R/07_worked_examples.R`.
- **The 256-trial corpus** is supplementary file 3 of Bleakley et al.,
  *Sports Med Open* 2025;11:134. It is **not** redistributed here — that article
  is licensed CC BY-NC-ND — so `R/04_corpus_msk.R` downloads it from the
  publisher on first run and caches it under `data/` (git-ignored).
- **The 21 trials with individual participant data** were deposited by their
  original investigators in public repositories (Zenodo, Dryad, the Open Science
  Framework). This repository ships only the derived
  `data/ipd_reference_specs.csv`: one row per trial, giving the PMC identifier,
  the deposit DOI or URL, the reference specification fitted, and the resulting
  estimate, standard error and *P* value. The raw deposits are not
  redistributed, since ten of the twenty-one carry no licence permitting it;
  each row names where to obtain them.

## Licence

MIT, as in `LICENSE`. Third-party data retain the licences of their sources.
