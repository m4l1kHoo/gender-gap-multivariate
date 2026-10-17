#!/usr/bin/env Rscript
# Run the unchanged primary R Markdown in a disposable copy, never in the source tree.
args <- commandArgs(trailingOnly = FALSE)
root <- dirname(normalizePath(sub("^--file=", "", args[grepl("^--file=", args)][1])))
work <- file.path(root, ".cache", "historical-source")
out <- file.path(root, "results", "historical")
input <- file.path(root, "private_originals")
if (!dir.exists(input)) stop("Historical reproduction requires local private_originals; these materials are not distributed.")
dir.create(work, recursive = TRUE, showWarnings = FALSE)
for (name in c("gender_gap_analysis.Rmd", "gender_gap_analysis_knit_ready_original_names.Rmd")) {
  stopifnot(file.copy(file.path(input, name), file.path(work, name), overwrite = TRUE))
}
relative_data <- file.path("性别差异", "性别差异")
dir.create(file.path(work, relative_data), recursive = TRUE, showWarnings = FALSE)
stopifnot(all(file.copy(file.path(input, relative_data, c("gggr.csv", "gggr-detail.csv", "country.csv")),
  file.path(work, relative_data), overwrite = TRUE)))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
if (nzchar(Sys.which("pandoc"))) Sys.setenv(RSTUDIO_PANDOC = dirname(Sys.which("pandoc")))
packages <- c("readr", "dplyr", "tidyr", "stringr", "ggplot2", "cluster", "knitr", "rmarkdown", "jsonlite")
stopifnot(all(vapply(packages, requireNamespace, logical(1), quietly = TRUE)))
env <- new.env(parent = globalenv())
rmarkdown::render(file.path(work, "gender_gap_analysis.Rmd"),
  output_format = "html_document", output_file = "original_recomputed.html",
  output_dir = out, intermediates_dir = work, knit_root_dir = work, envir = env,
  quiet = TRUE)
file.copy(file.path(work, "outputs", "gender_gap", c("tables", "figures")), out,
  recursive = TRUE, overwrite = TRUE)
alternate <- file.path(work, "gender_gap_analysis_knit_ready_original_names.Rmd")
script <- file.path(work, "alternate_extracted.R")
knitr::purl(alternate, output = script, quiet = TRUE)
parse_status <- tryCatch({parse(script); "valid"}, error = function(e) conditionMessage(e))
writeLines(parse_status, file.path(out, "alternate_parse.txt"))
capture.output(sessionInfo(), file = file.path(out, "sessionInfo.txt"))
capture.output(summary(env$anova_continent), summary(env$anova_income),
  summary(env$manova_income, test = "Wilks"), TukeyHSD(env$anova_income),
  file = file.path(out, "statistical_tests.txt"))
jsonlite::write_json(list(primary = "gender_gap_analysis.Rmd", unchanged_source = TRUE,
  countries = nrow(env$full_2017), pca_first_four = sum(env$pca_fit$sdev[1:4]^2)/sum(env$pca_fit$sdev^2),
  cluster_sizes = as.list(table(env$km4$cluster)), alternate_parse = parse_status,
  generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE)),
  file.path(out, "run.json"), pretty = TRUE, auto_unbox = TRUE, digits = 16)
cat("Original reproduction complete:", out, "\n")
