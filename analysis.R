# Statistical functions. Sources and changes are documented in docs/METHODS.md.
required_packages <- c("readr", "dplyr", "tidyr", "ggplot2", "cluster", "jsonlite", "digest", "knitr", "scales")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Missing R package: ", pkg, call. = FALSE)
}
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(ggplot2)})

broad_names <- c(global = "Global Index", economic = "Economic Participation and Opportunity",
  education = "Education Attainment", health = "Health and Survival", political = "Political Empowerment")

assert <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}
assert_columns <- function(x, columns, label) {
  assert(all(columns %in% names(x)), paste(label, "missing columns:", paste(setdiff(columns, names(x)), collapse = ", ")))
}
assert_unique <- function(x, keys, label) {
  assert(!anyNA(x[keys]), paste(label, "has missing keys"))
  assert(!any(duplicated(x[keys])), paste(label, "has duplicate keys:", paste(keys, collapse = ", ")))
}
read_input <- function(path) {
  x <- readr::read_csv(path, show_col_types = FALSE, trim_ws = TRUE, progress = FALSE)
  assert(nrow(readr::problems(x)) == 0, paste("CSV parsing problems:", path))
  names(x) <- trimws(names(x))
  x
}
validate_config <- function(cfg) {
  positive_integer <- function(x) is.numeric(x) && length(x) == 1 && is.finite(x) && x >= 1 && x == floor(x)
  for (key in c("year", "seed", "components", "clusters", "selection_nstart", "main_nstart", "iter_max")) {
    assert(positive_integer(cfg[[key]]), paste("Invalid config:", key))
  }
  assert(cfg$clusters >= 2, "clusters must be >= 2")
  assert(length(cfg$candidate_k) > 0 && all(is.finite(cfg$candidate_k)) && all(cfg$candidate_k >= 2) &&
    all(cfg$candidate_k == floor(cfg$candidate_k)) && !anyDuplicated(cfg$candidate_k), "Invalid candidate_k")
  assert(all(c(2, 3, cfg$clusters) %in% cfg$candidate_k), "candidate_k must include 2, 3 and the main k")
  assert(length(cfg$sensitivity_seeds) >= 2 && !anyDuplicated(cfg$sensitivity_seeds) &&
    all(is.finite(cfg$sensitivity_seeds)) && all(cfg$sensitivity_seeds >= 1) &&
    all(cfg$sensitivity_seeds == floor(cfg$sensitivity_seeds)), "Invalid sensitivity seeds")
  invisible(cfg)
}
prepare_data <- function(data_dir, year) {
  broad <- read_input(file.path(data_dir, "gggr.csv"))
  detail <- read_input(file.path(data_dir, "gggr-detail.csv"))
  country <- read_input(file.path(data_dir, "country.csv"))
  assert_columns(broad, c("country", "year", "IndexName", "value", "rank"), "gggr")
  assert_columns(detail, c("country", "year", "DetailIndexName", "value"), "detail")
  assert_columns(country, c("country", "ChineseCountryName", "Continent", "IncomeLevel", "GDPperCapita",
    "PopulationSexRatio", "HumanCapitalIndex"), "country")
  assert_unique(country, "country", "country attributes")
  history_duplicates <- broad |> group_by(year, country, IndexName) |> filter(n() > 1) |> ungroup()
  b <- broad |> filter(.data$year == .env$year, country != "World")
  d <- detail |> filter(.data$year == .env$year, country != "World")
  d$DetailIndexName[d$DetailIndexName == "Legislators, senior o?cials and managers"] <- "Legislators, senior officials and managers"
  assert(nrow(b) > 0 && nrow(d) > 0, "No broad/detail data for configured year")
  assert_unique(b, c("country", "IndexName"), "selected-year broad data")
  assert_unique(d, c("country", "DetailIndexName"), "selected-year detail data")
  assert(setequal(unique(b$IndexName), unname(broad_names)), "Expected five broad indicators")
  vars <- unique(d$DetailIndexName)
  assert(length(vars) == 14, "Expected 14 detail indicators")
  assert(setequal(unique(b$country), unique(d$country)), "Broad/detail country sets differ")
  assert(all(unique(b$country) %in% country$country), "Missing country attributes")
  assert(nrow(d) == length(unique(d$country)) * length(vars), "Missing detail records (distinct from explicit NA values)")
  wide <- b |> select(country, IndexName, value) |> pivot_wider(names_from = IndexName, values_from = value)
  names(wide)[match(unname(broad_names), names(wide))] <- names(broad_names)
  detail_wide <- d |> select(country, DetailIndexName, value) |> pivot_wider(names_from = DetailIndexName, values_from = value)
  full <- wide |> left_join(b |> filter(IndexName == "Global Index") |> select(country, rank), by = "country") |>
    left_join(detail_wide, by = "country") |> left_join(country, by = "country")
  assert(nrow(full) == nrow(wide), "Join changed the country count")
  assert(all(vapply(full[c(names(broad_names), vars)], is.numeric, logical(1))), "Indicators must be numeric")
  assert(all(is.finite(as.matrix(full[names(broad_names)]))), "Missing/nonfinite broad score")
  assert(!anyNA(full[c("Continent", "IncomeLevel")]), "Missing group labels")
  assert(all(is.finite(as.matrix(full[vars])) | is.na(as.matrix(full[vars]))), "Infinite detail value")
  assert(all(full$GDPperCapita[!is.na(full$GDPperCapita)] > 0), "Nonpositive GDP cannot be log transformed")
  world <- broad |> filter(country == "World", !is.na(value)) |> arrange(IndexName, year)
  assert_unique(world, c("year", "IndexName"), "World trend")
  missing <- data.frame(indicator = vars, missing_n = colSums(is.na(full[vars]))) |> arrange(desc(missing_n))
  coverage <- data.frame(country = full$country, missing_n = rowSums(is.na(full[vars])))
  list(full = full, vars = vars, world = world, missing = missing, coverage = coverage,
    history_duplicates = history_duplicates, extra_country_attributes = setdiff(country$country, full$country),
    raw_rows = c(gggr = nrow(broad), detail = nrow(detail), country = nrow(country)))
}

median_impute <- function(x) {
  x <- as.matrix(x)
  assert(is.numeric(x), "PCA inputs must be numeric")
  assert(all(is.finite(x) | is.na(x)), "Nonfinite PCA inputs")
  assert(!any(colSums(!is.na(x)) == 0), "Cannot impute an entirely missing indicator")
  medians <- apply(x, 2, median, na.rm = TRUE)
  positions <- which(is.na(x), arr.ind = TRUE)
  if (nrow(positions)) x[positions] <- medians[positions[, 2]]
  list(x = x, medians = medians, positions = positions)
}
fit_pca <- function(x, components) {
  x <- as.matrix(x)
  assert(all(is.finite(x)), "PCA requires finite imputed inputs")
  assert(nrow(x) > components && ncol(x) >= components, "Insufficient dimensions for requested PCA")
  assert(all(apply(x, 2, sd) > 0), "PCA cannot scale a constant indicator")
  stats::prcomp(x, center = TRUE, scale. = TRUE)
}
adjusted_rand <- function(a, b) {
  assert(length(a) == length(b) && length(a) > 1 && !anyNA(a) && !anyNA(b), "Invalid partitions for ARI")
  choose2 <- function(z) z * (z - 1) / 2
  tab <- table(a, b)
  pair <- sum(choose2(tab)); ra <- sum(choose2(rowSums(tab))); cb <- sum(choose2(colSums(tab)))
  expected <- ra * cb / choose2(length(a)); upper <- (ra + cb) / 2
  if (abs(upper - expected) < .Machine$double.eps) return(1)
  (pair - expected) / (upper - expected)
}
fit_clusters <- function(scores, k, seed, nstart, iter_max) {
  assert(k >= 2 && k < nrow(scores), "Invalid k for sample size")
  warnings <- character()
  set.seed(seed)
  model <- withCallingHandlers(kmeans(scores, centers = k, nstart = nstart, iter.max = iter_max,
      algorithm = "Hartigan-Wong"), warning = function(w) {warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning")})
  list(model = model, silhouette = mean(cluster::silhouette(model$cluster, dist(scores))[, 3]),
    warnings = unique(warnings), k = k, seed = seed)
}
cluster_labels <- function(cluster, global) {
  # Presentation only: the global score is NOT a clustering feature.
  means <- tapply(global, cluster, mean)
  order_ids <- names(sort(means, decreasing = TRUE))
  map <- setNames(paste0("G", seq_along(order_ids)), order_ids)
  unname(map[as.character(cluster)])
}
summarize_clusters <- function(full, cluster) {
  full |> mutate(group = cluster_labels(cluster, global)) |> group_by(group) |>
    summarise(n = n(), across(all_of(names(broad_names)), mean), .groups = "drop")
}
descriptive_analysis <- function(dat) {
  f <- dat$full
  broad <- bind_rows(lapply(names(broad_names), function(v) {
    x <- f[[v]]
    data.frame(indicator = v, n = length(x), mean = mean(x), sd = sd(x), min = min(x),
      q1 = unname(quantile(x, .25)), median = median(x), q3 = unname(quantile(x, .75)), max = max(x))
  }))
  groups <- lapply(c("Continent", "IncomeLevel"), function(g) f |> group_by(.data[[g]]) |>
    summarise(n = n(), across(all_of(names(broad_names)), mean), .groups = "drop") |> arrange(desc(global)))
  names(groups) <- c("continent", "income")
  world_change <- dat$world |> group_by(IndexName) |> summarise(first_year = first(year), last_year = last(year),
    first_value = first(value), last_value = last(value), change = last(value) - first(value), .groups = "drop")
  list(broad = broad, groups = groups, world_change = world_change)
}
statistical_analysis <- function(full) {
  tests <- list(); diagnostics <- list(); residuals <- list(); group_variances <- list()
  for (g in c("Continent", "IncomeLevel")) {
    z <- full[complete.cases(full[c("global", g)]), ]
    model <- aov(reformulate(g, "global"), z)
    s <- summary(model)[[1]]
    welch <- oneway.test(reformulate(g, "global"), z)
    tests[[g]] <- data.frame(grouping = g, test = c("ANOVA", "Welch"), n = nrow(z),
      statistic = c(s[1, "F value"], unname(welch$statistic)), df1 = c(s[1, "Df"], welch$parameter[1]),
      df2 = c(s[2, "Df"], welch$parameter[2]), p = c(s[1, "Pr(>F)"], welch$p.value))
    z$deviation <- abs(z$global - ave(z$global, z[[g]], FUN = median))
    bf <- summary(aov(reformulate(g, "deviation"), z))[[1]]
    sh <- shapiro.test(residuals(model))
    diagnostics[[g]] <- data.frame(grouping = g, test = c("Brown-Forsythe", "Shapiro-Wilk residuals"),
      statistic = c(bf[1, "F value"], unname(sh$statistic)), p = c(bf[1, "Pr(>F)"], sh$p.value))
    residuals[[g]] <- data.frame(grouping = g, fitted = fitted(model), residual = residuals(model))
    group_variances[[g]] <- z |> group_by(.data[[g]]) |> summarise(n = n(), variance = var(global), .groups = "drop")
  }
  income_model <- aov(global ~ IncomeLevel, full)
  tukey <- as.data.frame(TukeyHSD(income_model)$IncomeLevel)
  tukey$comparison <- rownames(tukey); rownames(tukey) <- NULL
  m <- manova(cbind(economic, education, health, political) ~ IncomeLevel, full)
  ms <- summary(m, test = "Wilks")$stats
  manova_table <- data.frame(test = "Wilks", n = nobs(m), wilks = ms[1, "Wilks"], statistic = ms[1, "approx F"],
    df1 = ms[1, "num Df"], df2 = ms[1, "den Df"], p = ms[1, "Pr(>F)"])
  correlations <- bind_rows(lapply(c("GDPperCapita", "HumanCapitalIndex", "PopulationSexRatio"), function(v) {
    x <- full[[v]]; if (v == "GDPperCapita") x <- log10(x)
    ok <- complete.cases(x, full$global)
    data.frame(covariate = v, transformation = ifelse(v == "GDPperCapita", "log10", "none"),
      n = sum(ok), pearson_r = cor(x[ok], full$global[ok]))
  }))
  list(tests = bind_rows(tests), diagnostics = bind_rows(diagnostics), tukey = tukey,
    manova = manova_table, correlations = correlations, residuals = bind_rows(residuals), variances = group_variances,
    manova_covariance_eigenvalues = eigen(cov(residuals(m)), symmetric = TRUE)$values)
}

run_analysis <- function(dat, cfg, stage = "all") {
  out <- list(data = dat, config = cfg, descriptive = descriptive_analysis(dat))
  if (stage == "data") return(out)
  imputed <- median_impute(dat$full[dat$vars]); pca <- fit_pca(imputed$x, cfg$components)
  positions <- imputed$positions
  out$imputation <- data.frame(country = dat$full$country[positions[, 1]], indicator = dat$vars[positions[, 2]],
    imputed_value = imputed$medians[positions[, 2]])
  out$medians <- data.frame(indicator = names(imputed$medians), median = imputed$medians)
  out$pca <- pca
  out$variance <- data.frame(component = seq_along(pca$sdev), eigenvalue = pca$sdev^2,
    variance = pca$sdev^2 / sum(pca$sdev^2), cumulative = cumsum(pca$sdev^2 / sum(pca$sdev^2)))
  if (stage == "pca") return(out)
  scores <- pca$x[, seq_len(cfg$components), drop = FALSE]
  fits <- lapply(cfg$candidate_k, function(k) fit_clusters(scores, k, cfg$seed, cfg$selection_nstart, cfg$iter_max))
  names(fits) <- as.character(cfg$candidate_k)
  out$choices <- bind_rows(lapply(fits, function(f) data.frame(k = f$k, within_ss = f$model$tot.withinss,
    silhouette = f$silhouette, iterations = f$model$iter, ifault = f$model$ifault,
    warnings = paste(f$warnings, collapse = "; "))))
  main <- fit_clusters(scores, cfg$clusters, cfg$seed, cfg$main_nstart, cfg$iter_max)
  out$main <- main
  out$clustered <- dat$full |> mutate(group = cluster_labels(main$model$cluster, global)) |> bind_cols(as.data.frame(scores))
  out$cluster_summary <- summarize_clusters(dat$full, main$model$cluster)
  out$profiles <- bind_rows(lapply(fits[as.character(unique(c(2, 3, cfg$clusters)))], function(f)
    summarize_clusters(dat$full, f$model$cluster) |> mutate(k = f$k)))
  out$candidate_memberships <- bind_rows(lapply(fits, function(f) data.frame(k = f$k, country = dat$full$country,
    group = cluster_labels(f$model$cluster, dat$full$global))))
  if (stage == "cluster") return(out)
  out$stats <- statistical_analysis(dat$full)
  out$stability <- bind_rows(lapply(unique(c(2, 3, cfg$clusters)), function(k) {
    reference <- if (k == cfg$clusters) main else fit_clusters(scores, k, cfg$seed, cfg$main_nstart, cfg$iter_max)
    bind_rows(lapply(cfg$sensitivity_seeds, function(seed) {
      f <- fit_clusters(scores, k, seed, cfg$main_nstart, cfg$iter_max)
      data.frame(k = k, seed = seed, ari = adjusted_rand(reference$model$cluster, f$model$cluster),
        within_ss = f$model$tot.withinss, silhouette = f$silhouette,
        warnings = paste(f$warnings, collapse = "; "), ifault = f$model$ifault)
    }))
  }))
  complete <- complete.cases(dat$full[dat$vars])
  cpca <- fit_pca(dat$full[complete, dat$vars], cfg$components)
  cscores <- cpca$x[, seq_len(cfg$components), drop = FALSE]
  cf <- fit_clusters(cscores, cfg$clusters, cfg$seed, cfg$main_nstart, cfg$iter_max)
  out$complete_case <- list(n = sum(complete), pca_variance = sum(cpca$sdev[1:cfg$components]^2) / sum(cpca$sdev^2),
    ari_shared_countries = adjusted_rand(main$model$cluster[complete], cf$model$cluster), fit = cf,
    summary = summarize_clusters(dat$full[complete, ], cf$model$cluster),
    membership = data.frame(country = dat$full$country[complete],
      full_sample_group = out$clustered$group[complete], complete_case_group = cluster_labels(cf$model$cluster, dat$full$global[complete])))
  out
}
