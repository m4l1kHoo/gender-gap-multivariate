#!/usr/bin/env Rscript
argv <- commandArgs(trailingOnly = FALSE)
root <- dirname(normalizePath(sub("^--file=", "", argv[grepl("^--file=", argv)][1])))
run_warnings <- character()
capture_warnings <- function(expr) withCallingHandlers(expr, warning = function(w) {
  run_warnings <<- unique(c(run_warnings, conditionMessage(w)))
  invokeRestart("muffleWarning")
})
capture_warnings(source(file.path(root, "analysis.R")))
source(file.path(root, "report.R"))
args <- commandArgs(trailingOnly = TRUE)
options <- list(data = file.path(root, "data"), config = file.path(root, "config.json"),
  out = file.path(root, "results", "full"), stage = "all")
if ("--help" %in% args) {
  cat("Rscript --vanilla run.R [--data DIR] [--config FILE] [--out DIR] [--stage data|pca|cluster|all]\nOutputs must be new or empty. Each stage runs its prerequisites.\n"); quit(status = 0)
}
assert(length(args) %% 2 == 0, "Arguments must be --name value pairs; use --help")
if (length(args)) for (i in seq(1, length(args), by = 2)) {
  key <- sub("^--", "", args[i]); assert(key %in% names(options) && startsWith(args[i], "--"), paste("Unknown option:", args[i]))
  options[[key]] <- args[i + 1]
}
assert(options$stage %in% c("data", "pca", "cluster", "all"), "Unknown stage")
cfg <- jsonlite::read_json(options$config, simplifyVector = TRUE); validate_config(cfg)
assert(!dir.exists(options$out) || length(list.files(options$out, all.files = TRUE, no.. = TRUE)) == 0,
  "Output directory must be new or empty; choose --out results/another-run")
files <- file.path(options$data, c("gggr.csv", "gggr-detail.csv", "country.csv"))
assert(all(file.exists(files)), "Missing input CSV")
hashes <- setNames(vapply(files, digest::digest, character(1), algo = "sha256", file = TRUE), basename(files))
manifest_path <- file.path(options$data, "manifest.json")
verified <- FALSE
if (file.exists(manifest_path)) {
  expected <- unlist(jsonlite::read_json(manifest_path))
  assert(all(names(hashes) %in% names(expected)) && all(hashes == expected[names(hashes)]), "Input files do not match their manifest")
  verified <- TRUE
}
reference <- unlist(jsonlite::read_json(file.path(root, "data", "manifest.json")))
is_course <- all(names(hashes) %in% names(reference)) && all(hashes == reference[names(hashes)])
info_path <- file.path(options$data, "dataset.json")
is_synthetic <- file.exists(info_path) && identical(jsonlite::read_json(info_path)$kind, "synthetic")
dataset_kind <- if (is_course) "course" else if (is_synthetic) "synthetic" else "user_supplied"
dat <- prepare_data(options$data, cfg$year)
result <- capture_warnings(run_analysis(dat, cfg, options$stage))
result$dataset_kind <- dataset_kind
dir.create(options$out, recursive = TRUE, showWarnings = FALSE)
write_tables(result, options$out)
comparison <- if (is_course && !is.null(result$main) && cfg$year == 2017 && cfg$components == 4 && cfg$clusters == 4)
  compare_history(result, root, options$out) else data.frame()
if (options$stage == "all") capture_warnings({make_plots(result, options$out); render_report(result, options$out, comparison)})
capture.output(sessionInfo(), file = file.path(options$out, "sessionInfo.txt"))
writeLines(run_warnings, file.path(options$out, "warnings.txt"))
code_paths <- file.path(root, c("analysis.R", "report.R", "run.R", "config.json"))
metadata <- list(generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE), stage = options$stage, dataset_kind = dataset_kind, config = cfg,
  configuration_sha256 = digest::digest(options$config, algo = "sha256", file = TRUE),
  rng_kind = RNGkind(), warnings = run_warnings,
  input_sha256 = as.list(hashes), manifest_verified = verified, raw_rows = as.list(dat$raw_rows),
  source_sha256 = as.list(setNames(vapply(code_paths, digest::digest, character(1), algo = "sha256", file = TRUE), basename(code_paths))),
  countries = nrow(dat$full), missing_detail_values = sum(dat$missing$missing_n),
  main_cluster_warnings = if (!is.null(result$main)) result$main$warnings else NULL,
  complete_case_warnings = if (!is.null(result$complete_case)) result$complete_case$fit$warnings else NULL,
  main_ifault = if (!is.null(result$main)) result$main$model$ifault else NULL,
  complete_case_ifault = if (!is.null(result$complete_case)) result$complete_case$fit$model$ifault else NULL)
jsonlite::write_json(metadata, file.path(options$out, "run.json"), pretty = TRUE, auto_unbox = TRUE, digits = 16)
cat("Completed stage:", options$stage, "\nCountries:", nrow(dat$full), "\nOutput:", normalizePath(options$out), "\n")
