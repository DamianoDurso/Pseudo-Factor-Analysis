# Semantic Structure Analysis: reproducibility pack

Code and data that reproduce every table, figure and result quoted in

Guenole, N., D'Urso, E. D., Samo, A., Sun, T., & Haslbeck, J. M. B. (2026). Enhancing scale development: Semantic structure analysis of language embedding similarity matrices. *Educational and Psychological Measurement*. https://doi.org/10.1177/00131644261487937

Two studies: the IPIP-NEO-300 (30 facets, five factors) and the IPIP HEXACO (24 facets, six factors). Facet similarity matrices are built from sentence embeddings, factor-analysed with a theoretical target rotation, labelled with the DAAL rule, and compared with published empirical loadings (Johnson, 2014; Lee & Ashton, 2004).

This pack replaces the materials previously posted on OSF. It corrects transcription errors in the empirical benchmark loadings (Johnson, 2014; Lee & Ashton, 2004), incorrect HEXACO item keys, and indexing and data-entry bugs in the analysis.

## Running it

Run from this folder, in order:

| Step | Command | Does |
|---|---|---|
| 1 | `python3 01_build_items.py` | Item and key files from the downloaded sources |
| 2 | `HF_HUB_OFFLINE=1 TOKENIZERS_PARALLELISM=false python3 02_embed.py` | Embeddings and facet similarity matrices (checks the saved ones; writes missing ones); robustness-check matrices |
| 3 | `Rscript 03_fit.R` | Target-rotated factor analyses, DAAL, robustness check, residuals |
| 4 | `python3 04_tables.py` | `Results.xlsx` and `Manuscript Tables.xlsx` |
| 5 | `Rscript 05_figures.R` | `Figure 2.png`, `Figure 3.png` |

Leave out `HF_HUB_OFFLINE=1` the first time, so the models can be downloaded (each is pinned to a fixed revision in `02_embed.py`). Versions used: Python 3.11.8 with `requirements.txt`; R 4.3.1 with psych 2.4.6.26, GPArotation 2024.2-1, esemComp 0.2 (GitHub: MateusPsi/esemComp, commit 5b5b9f067aad2d40310a01adb59722bda9e8fe51), ggplot2 3.5.2 and gridExtra 2.3.

## Files

| Path | Content |
|---|---|
| `NEO/`, `HEXACO/` | Per study: downloaded sources, item and key file, empirical benchmark (`benchmark_*.csv`), `matrices/`, `results/` |
| `*/matrices/matrix_concatenated_{item_,item_rev_,}<model>.csv` | Facet similarity matrices: atomic, atomic reversed, macro encoding |
| `*/matrices/<study>_<step>_avg.csv` | Robustness check: averaged-transformer matrices as each factor is replaced by noise |
| `*/results/Results_target_*_labeled.csv` | Loadings of all solutions, columns labelled by DAAL |
| `*/results/DAAL.csv` | DAAL calculation for every factor of every solution |
| `*/results/robustness.csv`, `residuals.csv` | Robustness check (Tables 7, 8); residual diagnostics |
| `Results.xlsx` | One tab per study: alignment, loadings, congruence, correlations, robustness |
| `Manuscript Tables.xlsx` | Tables 1–8, supplementary Tables S1–S4, and the numbers quoted in the text |
| `Figure 2.png`, `Figure 3.png` | Figures 2 and 3 |

In both workbooks, summary cells (proportions, means, M, Overall M, totals) are Excel formulas over the cells they summarise; open them in Excel or LibreOffice to see the values (previews that do not calculate formulas show them empty).

The published source PDFs (Johnson, 2014; Lee & Ashton, 2004) are not included (copyright); the benchmark CSVs give their page and table.

## Notes on reproduction

- **Similarity matrices.** The main analyses fit the similarity matrices saved by the original analysis. `02_embed.py` regenerates them from the item text and checks that they agree to 0.00001 (they agree to 0.0000014). The saved matrices are used because a few rotations (see below) amplify floating-point differences of this size into changes of .01 in printed values. The HEXACO atomic-reversed matrices are new: they use the official IPIP keys.
- **Rotation.** As in the paper, target rotation runs with the default iteration limit from a single start. Five rotations do not converge within that limit (NEO atomic MiniLM, NEO macro T5, HEXACO atomic MiniLM, HEXACO atomic T5, HEXACO atomic-reversed MiniLM); they are flagged in `DAAL.csv` (`rotation_converged`) and in `Results.xlsx`, and reported as published.
- **Robustness check.** Its matrices are regenerated from scratch with seeded noise; the results are identical to the original analysis at every step.
