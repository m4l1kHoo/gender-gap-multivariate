#!/usr/bin/env Rscript
argv <- commandArgs(trailingOnly = FALSE)
root <- dirname(dirname(normalizePath(sub("^--file=", "", argv[grepl("^--file=", argv)][1]))))
source(file.path(root, "analysis.R"))
passed <- 0L; failed <- character()
test <- function(name, body) {
  tryCatch({force(body); passed <<- passed + 1L; cat("PASS", name, "\n")}, error = function(e) {
    failed <<- c(failed, name); cat("FAIL", name, ":", conditionMessage(e), "\n")
  })
}
expect_error <- function(expr, pattern) {
  err <- tryCatch({force(expr); NULL}, error = identity)
  stopifnot(inherits(err, "error"), grepl(pattern, conditionMessage(err)))
}
cfg <- jsonlite::read_json(file.path(root, "config.json"), simplifyVector = TRUE)
data_dir <- file.path(root, "data")
course <- !"--demo" %in% commandArgs(trailingOnly=TRUE) && all(file.exists(file.path(data_dir, c("gggr.csv", "gggr-detail.csv", "country.csv"))))
if (!course) {
  source(file.path(root, "generate_demo.R"))
  data_dir <- tempfile("mva-synthetic-")
  generate_demo(data_dir)
}
cat("Test input:", if (course) "local course data" else "synthetic fixture", "\n")
d <- prepare_data(data_dir, cfg$year)
r <- run_analysis(d, cfg, "cluster")
test("known median preserves observed values and logs each missing cell", {
  x <- cbind(a = c(1, NA, 9, 3), b = c(2, 4, 6, NA)); z <- median_impute(x)
  stopifnot(z$x[2, 1] == 3, z$x[4, 2] == 4, all(z$x[!is.na(x)] == x[!is.na(x)]), nrow(z$positions) == 2)
})
test("entirely missing columns fail", expect_error(median_impute(cbind(c(1, 2), c(NA_real_, NA_real_))), "entirely missing"))
test("infinite features fail", expect_error(median_impute(cbind(c(1, 2), c(Inf, 3))), "Nonfinite"))
test("constant features fail before scaling", expect_error(fit_pca(cbind(1:5, rep(2, 5)), 2), "constant"))
test("PCA matches known perfectly correlated example", {
  f <- fit_pca(cbind(x = 1:5, y = 2*(1:5)), 2)
  stopifnot(abs(f$sdev[1]^2 - 2) < 1e-12, f$sdev[2]^2 < 1e-20)
})
test("PCA reconstructs standardized inputs with all components", {
  x <- median_impute(d$full[d$vars])$x
  stopifnot(max(abs(scale(x) - r$pca$x %*% t(r$pca$rotation))) < 1e-12)
})
test("PCA total eigenvalue equals count of standardized features", stopifnot(abs(sum(r$pca$sdev^2) - 14) < 1e-10))
test("principal component sign changes preserve distances", {
  x <- r$pca$x[, 1:4]; flipped <- sweep(x, 2, c(-1, 1, -1, 1), "*")
  stopifnot(max(abs(as.matrix(dist(x)) - as.matrix(dist(flipped)))) < 1e-12)
})
test("ARI invariant to arbitrary label permutations", stopifnot(adjusted_rand(c(1,1,2,2,3), c("z","z","a","a","m")) == 1))
test("ARI matches hand computed crossed partition", stopifnot(abs(adjusted_rand(c(1,1,2,2), c(1,2,1,2)) + .5) < 1e-12))
test("ARI degenerate identical partitions return one", stopifnot(adjusted_rand(rep(1, 4), rep(7, 4)) == 1))
test("invalid ARI inputs fail", expect_error(adjusted_rand(c(1,2), c(1)), "Invalid partitions"))
test("known separated points recover analytic partition and WSS", {
  x <- matrix(c(0, 1, 10, 11), ncol = 1)
  f <- fit_clusters(x, 2, 29, 20, 10)
  stopifnot(adjusted_rand(f$model$cluster, c(1,1,2,2)) == 1, abs(f$model$tot.withinss - 1) < 1e-12)
})
test("cluster profile labeling does not depend on numeric IDs", {
  a <- cluster_labels(c(1,1,2,2), c(.1,.2,.8,.9)); b <- cluster_labels(c(9,9,3,3), c(.1,.2,.8,.9))
  stopifnot(identical(a, b), identical(a, c("G2","G2","G1","G1")))
})
test("supplied dataset coverage is accounted for", stopifnot(nrow(d$full) == if (course) 144 else 96, length(d$vars) == 14,
  sum(d$missing$missing_n) == if (course) 85 else 28, sum(d$coverage$missing_n == 0) == if (course) 90 else 68, nrow(r$imputation) == if (course) 85 else 28))
test("missing cell log maps precisely to input NA positions", {
  for (i in seq_len(nrow(r$imputation))) {
    row <- r$imputation[i, ]; stopifnot(is.na(d$full[[row$indicator]][match(row$country, d$full$country)]))
  }
})
test("WSS agrees with squared distance to assigned center", {
  f <- r$main$model; x <- r$pca$x[, 1:4]
  stopifnot(abs(sum((x - f$centers[f$cluster, ])^2) - f$tot.withinss) < 1e-10)
})
test("selection silhouette and WSS refer to same recorded memberships", {
  x <- r$pca$x[, 1:4]
  for (k in cfg$candidate_k) {
    labels <- r$candidate_memberships$group[r$candidate_memberships$k == k]
    f <- factor(labels); cen <- rowsum(x, f) / as.numeric(table(f))
    wss <- sum((x - cen[as.character(f), ])^2)
    sil <- mean(cluster::silhouette(as.integer(f), dist(x))[, 3])
    ref <- r$choices[r$choices$k == k, ]
    stopifnot(abs(wss - ref$within_ss) < 1e-9, abs(sil - ref$silhouette) < 1e-12)
  }
})
test("country join retains exact key order without multiplying rows", {
  b <- read_input(file.path(data_dir, "gggr.csv"))
  expected <- unique(b$country[b$year == 2017 & b$country != "World"])
  stopifnot(identical(expected, d$full$country), !anyDuplicated(d$full$country))
})
test("duplicate selected-year keys fail instead of silently aggregating", {
  scratch <- tempfile("mva-data-"); dir.create(scratch)
  file.copy(file.path(data_dir, c("gggr.csv", "gggr-detail.csv", "country.csv")), scratch)
  b <- read_input(file.path(scratch, "gggr.csv")); b <- bind_rows(b, b[b$year == 2017 & b$country != "World", ][1, ])
  readr::write_csv(b, file.path(scratch, "gggr.csv"))
  expect_error(prepare_data(scratch, 2017), "duplicate keys")
  unlink(scratch, recursive = TRUE)
})
test("other-year detail rows cannot leak into current analysis", {
  scratch <- tempfile("mva-year-"); dir.create(scratch)
  file.copy(file.path(data_dir, c("gggr.csv", "gggr-detail.csv", "country.csv")), scratch)
  z <- read_input(file.path(scratch, "gggr-detail.csv")); extra <- z[1, ]; extra$year <- 2016; extra$value <- 999
  readr::write_csv(bind_rows(z, extra), file.path(scratch, "gggr-detail.csv"))
  actual <- prepare_data(scratch, 2017)
  stopifnot(isTRUE(all.equal(actual$full, d$full)))
  unlink(scratch, recursive = TRUE)
})
test("invalid configuration fails before computation", {
  bad <- cfg; bad$components <- -1; expect_error(validate_config(bad), "components")
  bad <- cfg; bad$candidate_k <- c(2, 3, 4.5); expect_error(validate_config(bad), "candidate_k")
})
test("ANOVA agrees with manually computed sums of squares", {
  s <- statistical_analysis(d$full)
  y <- d$full$global; g <- d$full$IncomeLevel; grand <- mean(y)
  means <- tapply(y, g, mean); ns <- table(g)
  between <- sum(ns * (means - grand)^2); within <- sum((y - means[g])^2)
  f <- (between/(length(ns)-1)) / (within/(length(y)-length(ns)))
  stopifnot(abs(f - s$tests$statistic[s$tests$grouping == "IncomeLevel" & s$tests$test == "ANOVA"]) < 1e-10,
    all(s$correlations$n <= nrow(d$full)), s$correlations$n[s$correlations$covariate == "HumanCapitalIndex"] < nrow(d$full))
})
cat("\n", passed, "passed;", length(failed), "failed\n")
if (!course) unlink(data_dir, recursive=TRUE)
if (length(failed)) quit(status = 1)
