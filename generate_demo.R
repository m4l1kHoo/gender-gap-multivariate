# Artificial fixture for checking the workflow; no country data are resampled.
generate_demo <- function(out) {
  stopifnot(!dir.exists(out) || length(list.files(out, all.files = TRUE, no.. = TRUE)) == 0)
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  set.seed(314159)
  n <- 96L
  countries <- sprintf("Demo-%03d", seq_len(n))
  indicators <- c("Labour force participation", "Wage equality for similar work (survey)",
    "Estimated earned income (PPP, US$)", "Legislators, senior officials and managers",
    "Professional and technical workers", "Literacy rate", "Enrolment in primary  education",
    "Enrolment in secondary education", "Enrolment in tertiary education", "Sex ratio at birth",
    "Healthy life expectancy", "Women in parliament", "Women in ministerial positions",
    "Years with female head of state (last 50)")
  indexes <- c("Economic Participation and Opportunity", "Education Attainment", "Health and Survival", "Political Empowerment")
  domain <- c(rep(1, 5), rep(2, 4), rep(3, 2), rep(4, 3))
  latent <- matrix(rnorm(n*4), ncol = 4)
  values <- sapply(seq_along(indicators), function(j) plogis(.4 + .8*latent[, domain[j]] + rnorm(n, sd=.35)))
  broad <- sapply(1:4, function(j) rowMeans(values[, domain == j, drop=FALSE]))
  broad <- cbind(rowMeans(broad), broad)
  index_names <- c("Global Index", indexes)
  rows <- do.call(rbind, lapply(seq_along(index_names), function(j) data.frame(year=2017, country=countries,
    IndexName=index_names[j], value=broad[, j], rank=rank(-broad[, j], ties.method="first"))))
  world <- do.call(rbind, lapply(seq_along(index_names), function(j) data.frame(year=2015:2017,
    country="World", IndexName=index_names[j], value=mean(broad[, j])+c(-.02,-.01,0), rank=NA_real_)))
  values[cbind(1:28, rep(1:14, 2))] <- NA_real_
  detail <- do.call(rbind, lapply(seq_along(indicators), function(j) data.frame(year=2017, country=countries,
    IndexName=indexes[domain[j]], DetailIndexName=indicators[j], value=values[, j], female=values[, j], male=1)))
  attributes <- data.frame(country=countries, ChineseCountryName=paste0("示例", seq_len(n)),
    Continent=rep(paste0("Region ", LETTERS[1:6]), length.out=n), IncomeLevel=rep(paste0("Income ", LETTERS[1:4]), each=24),
    GDPperCapita=exp(rnorm(n, 10, .6)), HumanCapitalIndex=runif(n, .3, .9), PopulationSexRatio=runif(n, .95, 1.05))
  attributes$HumanCapitalIndex[1:10] <- NA_real_
  write.csv(rbind(rows, world), file.path(out,"gggr.csv"), row.names=FALSE, na="")
  write.csv(detail, file.path(out,"gggr-detail.csv"), row.names=FALSE, na="")
  write.csv(attributes, file.path(out,"country.csv"), row.names=FALSE, na="", fileEncoding="UTF-8")
  jsonlite::write_json(list(kind="synthetic", label="Artificial workflow demonstration; no empirical country claims",
    seed=314159, countries=n, missing_detail_values=28), file.path(out,"dataset.json"), pretty=TRUE, auto_unbox=TRUE)
  hashes <- setNames(lapply(c("gggr.csv","gggr-detail.csv","country.csv"), function(f)
    digest::digest(file.path(out,f), file=TRUE, algo="sha256")), c("gggr.csv","gggr-detail.csv","country.csv"))
  jsonlite::write_json(hashes, file.path(out,"manifest.json"), pretty=TRUE, auto_unbox=TRUE)
  invisible(out)
}
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly=TRUE)
  if (length(args) != 2L || args[1] != "--out") stop("Usage: Rscript generate_demo.R --out demo/data")
  generate_demo(args[2]); cat("Generated synthetic data:", args[2], "\n")
}
