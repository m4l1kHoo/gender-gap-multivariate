# Gender Gap: A Multivariate Country Analysis

How do countries differ across economic participation, education, health, and political representation? This course project explores the 2017 Global Gender Gap indicators through descriptive statistics, principal component analysis (PCA), and K-means clustering.

The analysis covers **144 countries and 14 detailed indicators**. It also compares continent and income groups and describes associations with GDP per capita, human capital, and population sex ratio. The historical World series supplied with the course data provides context.

## Main findings

- Education and health scores are relatively high in this dataset; political empowerment has the lowest mean and greatest cross-country variation among the four dimensions.
- Four principal components explain **65.37%** of the standardized variance.
- The four-group solution contains **27, 81, 14, and 22 countries**. Two groups achieve the highest average silhouette among the candidate cluster counts, so four groups are an interpretive choice rather than a uniquely supported answer.
- The four-group partition is consistent across the ten tested initialization seeds. Restricting the analysis to 90 countries with complete detailed data changes the partition substantially: adjusted Rand index **0.450** on the shared countries.

These are descriptive, country-level results. They do not establish causal effects, individual-level differences, or definitive country types.

## Run the analysis

Use R 4.4 or later. The workflow was verified with R 4.4.2; exact observed package versions are in [dependencies.csv](dependencies.csv). No Python or Pandoc is required for the new analysis.

If packages are missing, install them in your preferred R library:

```r
install.packages(c("readr", "dplyr", "tidyr", "ggplot2", "cluster",
                  "jsonlite", "digest", "knitr", "scales", "ragg", "systemfonts"))
```

A self-contained **synthetic demonstration** is included. It generates artificial data; it does not reproduce the country findings above.

```sh
Rscript --vanilla tests/run_tests.R --demo
Rscript --vanilla generate_demo.R --out demo/data
Rscript --vanilla run.R --data demo/data --out results/demo
```

Open `results/demo/report.html`. The run writes CSV tables, PNG figures, configuration and input fingerprints, and R environment details. Choose a new or empty output directory for each run.

The verified course-data findings are available as [aggregate result snapshots](examples/course-results/README.md). Raw course CSVs and original reports are not redistributed. To repeat the empirical analysis, supply the three documented CSV files in `data/`; [data/README.md](data/README.md) describes their fields and provenance limitations, and `data/manifest.json` identifies the exact inputs used.

```sh
Rscript --vanilla tests/run_tests.R
Rscript --vanilla run.R --out results/course-run
```

When the course CSVs are absent, the test command uses an artificial fixture. It does not silently replace the empirical analysis inputs. The `run.R` command requires an explicit valid data directory.

## Inspect individual steps

```sh
Rscript --vanilla run.R --data demo/data --stage data    --out results/data-check
Rscript --vanilla run.R --data demo/data --stage pca     --out results/pca-check
Rscript --vanilla run.R --data demo/data --stage cluster --out results/cluster-check
```

Each stage runs its prerequisites from the CSV inputs and exports intermediate tables. It does not depend on a previous interactive R session. The default `all` stage adds statistical tests, sensitivity checks, plots, and the report. Parameters live in [config.json](config.json); `--config` and `--data` accept alternative locations. Changing inputs requires an accurate replacement manifest, or an input directory without a manifest, which is recorded as unverified.

## Implementation

| File | Purpose |
|---|---|
| [analysis.R](analysis.R) | Input checks, preprocessing, PCA, clustering, statistical analysis and sensitivity checks |
| [report.R](report.R) | Result tables, figures, historical comparisons and HTML report |
| [run.R](run.R) | Command-line workflow and run metadata |
| [generate_demo.R](generate_demo.R) | Generate deterministic artificial inputs |
| [tests/run_tests.R](tests/run_tests.R) | Analytical examples, invariants and failure cases |
| [docs/METHODS.md](docs/METHODS.md) | Mathematical definitions and their implementation |
| [docs/PROVENANCE.md](docs/PROVENANCE.md) | Original materials, authorship and changes |
| [docs/VALIDATION.md](docs/VALIDATION.md) | Executed checks, numerical comparisons and remaining limitations |

This is a course project with a subsequently reconstructed workflow. The author completed the original data preparation, R implementation, statistical analysis and report. AI assistance in the original materials and in the reconstruction is documented in the provenance record. Original reports and reference materials remain local.
