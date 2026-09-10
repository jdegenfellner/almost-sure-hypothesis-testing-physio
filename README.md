# Almost sure hypothesis testing in physiotherapy — revision 1

Code, derived data and manuscript sources for revision 1 of

> Degenfellner J. *Moving the goalposts on purpose: almost sure hypothesis
> testing and what it would do to physiotherapy trials.*
> BMC Medical Research Methodology, submission ID
> `6eb71280-e95a-49d0-9495-40901c6d15f2`, major revision of 29 August 2026.

The code archive the manuscript's availability statement points to is
[`almost-sure-hypothesis-testing-physio`](https://github.com/jdegenfellner/almost-sure-hypothesis-testing-physio),
public since 9 September 2026. It holds the code, the derived data and the
figures, without the manuscript sources. The first submission's repository
content is preserved locally as
`bmc_revision_1/github_repo_july2026_backup.bundle`.

## What changed, in one paragraph

Four reviewers asked for the same four things: say which inferential problem the
rule solves, evaluate it at finite sample sizes rather than asymptotically, apply
it to more than one trial, and stop overstating the Jeffreys–Lindley claim. The
revision does all four. It adds a finite-sample evaluation (exact size, power,
required sample size, implicit error weighting), a new statistic — the critical
exponent `p* = log(1/P)/log(n)`, the largest exponent at which a finding is still
rejected — and three levels of empirical application in place of the single
worked example: three open-access trials spanning *n* = 140 to 2249, 21 trials
reanalysed from openly deposited individual participant data, and 256
musculoskeletal trials from published summary statistics.

## Layout

```
paper_rev1_clean.tex     revised manuscript, no change marking  → submit this
paper_rev1_marked.tex    same content, additions printed in green → "related file"
preamble.tex             shared preamble; \markuptrue selects the marked build
body.tex                 the manuscript itself (single source for both builds)
response_to_reviewers.tex  point-by-point response  → submit as PDF

R/                       analysis scripts, numbered in run order
data/                    derived inputs (see provenance below)
figures/                 the five figures, as published
out/                     every table and console summary the paper quotes
run_all.R                reproduces all of the above
```

Build the two manuscript versions and the response with

```sh
pdflatex paper_rev1_clean.tex   && pdflatex paper_rev1_clean.tex
pdflatex paper_rev1_marked.tex  && pdflatex paper_rev1_marked.tex
pdflatex response_to_reviewers.tex && pdflatex response_to_reviewers.tex
```

Both manuscript builds `\input` the same `body.tex`, so the clean and marked
versions cannot drift apart.

## Scripts

| script | produces |
|---|---|
| `R/01_asht_core.R` | shared helpers: `asht_alpha`, `asht_crit`, `crit_exponent`, power, required *n*, ζ(p) |
| `R/06_lil_figure.R` | Figure 1 and `out/lil_crossings.txt` |
| `R/asht_simulation.R` | Figure 2 (unchanged from the first submission; slow) |
| `R/02_finite_sample.R` | Tables 2 and 3, Figure 3, `out/simulation_check.csv` |
| `R/07_worked_examples.R` | Table 4 |
| `R/03_trials_ipd.R` | Table 5 |
| `R/04_corpus_msk.R` | Figure 4, `out/corpus_*` |
| `R/05_choosing_p.R` | `out/choosing_p.txt` |
| `R/verify_correia.R` | checks on the Cui dropout comparison (unchanged) |

`Rscript run_all.R` runs them in order. Only `readxl` is needed beyond base R.

## Data provenance

Everything this paper uses is already public.

- **Worked examples** use summary statistics reported in the open-access
  articles: Cui et al., *npj Digit Med* 2023;6:121 (NCT04808141); UK BEAM Trial
  Team, *BMJ* 2004;329:1377 (ISRCTN32683578); Salisbury et al., *BMJ* 2013;346:f43
  (ISRCTN55666618). The numbers are transcribed in `R/07_worked_examples.R`.
- **The 256-trial corpus** is supplementary file 3 of Bleakley et al.,
  *Sports Med Open* 2025;11:134. It is **not** redistributed here — that article
  is licensed CC BY-NC-ND — so `R/04_corpus_msk.R` downloads it from the
  publisher on first run and caches it under `data/` (git-ignored).
- **The 21 trials with individual participant data** were deposited by their
  original investigators in public repositories (Zenodo, Dryad, journal
  supplements). This repository ships only the derived
  `data/ipd_reference_specs.csv`: one row per trial, giving the PMC identifier,
  the deposit DOI, the reference specification fitted, and the resulting
  estimate, standard error and *P* value. The raw deposits are not
  redistributed; each row names where to get them.


## Publishing the code archive

The public archive is built from this repository by copying `R/`, `data/`,
`figures/`, `out/`, `run_all.R`, `LICENSE` and `.gitignore` into a clean
directory with its own single commit, so that the manuscript sources and the
point-by-point response never enter its history. Verified by cloning the public
repository anonymously and running `Rscript run_all.R`: all twelve files in
`out/` come back bit-identical.

## Licence

MIT, as in `LICENSE`. Third-party data retain the licences of their sources.
