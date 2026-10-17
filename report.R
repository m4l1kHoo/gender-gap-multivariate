write_tables <- function(result, out) {
  dir.create(file.path(out, "tables"), recursive = TRUE, showWarnings = FALSE)
  save <- function(x, name) readr::write_csv(x, file.path(out, "tables", paste0(name, ".csv")), na = "")
  d <- result$data
  save(d$full, "analysis_input"); save(d$missing, "missing_detail"); save(d$coverage, "country_missingness")
  save(d$history_duplicates, "historical_duplicate_records")
  save(data.frame(country = d$extra_country_attributes), "unused_country_attributes")
  save(result$descriptive$broad, "broad_summary"); save(result$descriptive$world_change, "world_change")
  save(d$world, "world_trend")
  for (g in names(result$descriptive$groups)) save(result$descriptive$groups[[g]], paste0(g, "_summary"))
  if (is.null(result$pca)) return(invisible(NULL))
  save(result$imputation, "imputed_cells"); save(result$medians, "imputation_medians")
  save(data.frame(country = d$full$country, median_impute(d$full[d$vars])$x, check.names = FALSE), "pca_input")
  save(result$variance, "pca_variance")
  save(data.frame(indicator = rownames(result$pca$rotation), result$pca$rotation, row.names = NULL), "pca_loadings")
  save(data.frame(country = d$full$country, result$pca$x), "pca_scores")
  save(data.frame(indicator = names(result$pca$center), center = result$pca$center, scale = result$pca$scale), "pca_scaling")
  if (is.null(result$main)) return(invisible(NULL))
  save(result$choices, "cluster_choice"); save(result$clustered, "clustered_countries")
  save(result$cluster_summary, "cluster_summary"); save(result$profiles, "cluster_profiles_k2_k3_main")
  save(result$candidate_memberships, "candidate_memberships")
  if (is.null(result$stats)) return(invisible(NULL))
  for (name in c("tests", "diagnostics", "tukey", "manova", "correlations", "residuals")) save(result$stats[[name]], name)
  for (name in names(result$stats$variances)) save(result$stats$variances[[name]], paste0(name, "_variances"))
  save(data.frame(eigenvalue = result$stats$manova_covariance_eigenvalues), "manova_residual_covariance_eigenvalues")
  save(result$stability, "initialization_stability")
  save(result$complete_case$summary, "complete_case_summary")
  save(result$complete_case$membership, "complete_case_membership")
  save(data.frame(n = result$complete_case$n, variance_first_components = result$complete_case$pca_variance,
    ari_shared_countries = result$complete_case$ari_shared_countries), "complete_case_comparison")
}

compare_history <- function(result, project_root, out) {
  rows <- list()
  add <- function(comparison, metric, value, tolerance, pass) {
    rows[[length(rows) + 1L]] <<- data.frame(comparison, metric, value, tolerance, pass)
  }
  for (label in c("saved_original", "recomputed_original")) {
    path <- if (label == "saved_original") file.path(project_root, "private_originals", "outputs", "gender_gap", "tables") else
      file.path(project_root, "results", "historical", "tables")
    if (!dir.exists(path)) next
    oldv <- read_input(file.path(path, "pca_variance.csv"))
    delta <- max(abs(result$variance$eigenvalue - oldv$eigenvalue))
    add(label, "max_abs_eigenvalue_error", delta, 1e-10, delta < 1e-10)
    oldl <- read_input(file.path(path, "pca_loadings.csv"))
    a <- as.matrix(oldl[match(rownames(result$pca$rotation), oldl$indicator), paste0("PC", 1:4)])
    b <- result$pca$rotation[, 1:4]; signs <- sign(colSums(a * b)); signs[signs == 0] <- 1
    delta <- max(abs(a - sweep(b, 2, signs, "*")))
    add(label, "max_abs_loading_error_after_sign_alignment", delta, 1e-10, delta < 1e-10)
    oldc <- read_input(file.path(path, "clustered_countries.csv"))
    matched <- match(result$clustered$country, oldc$country)
    assert(!anyNA(matched), "Historical country comparison is incomplete")
    ari <- adjusted_rand(result$clustered$group, oldc$cluster[matched])
    add(label, "cluster_ARI", ari, 1, abs(ari - 1) < 1e-12)
    delta <- max(abs(as.matrix(result$clustered[names(broad_names)]) - as.matrix(oldc[matched, names(broad_names)])))
    add(label, "max_abs_country_broad_score_error", delta, 1e-12, delta < 1e-12)
  }
  historical <- file.path(project_root, "results", "historical", "tables")
  saved <- file.path(project_root, "private_originals", "outputs", "gender_gap", "tables")
  archival <- list()
  if (dir.exists(historical) && dir.exists(saved)) {
    for (file in list.files(saved, pattern = "[.]csv$")) {
      a <- read_input(file.path(saved, file)); b <- read_input(file.path(historical, file))
      ac <- names(a)[vapply(a, is.numeric, logical(1))]; bc <- names(b)[vapply(b, is.numeric, logical(1))]
      same_shape <- identical(dim(a), dim(b)) && identical(ac, bc)
      delta <- if (same_shape && length(ac)) max(abs(as.matrix(a[ac]) - as.matrix(b[bc])), na.rm = TRUE) else NA_real_
      text_cols <- intersect(names(a)[!vapply(a, is.numeric, logical(1))], names(b))
      same_text <- same_shape && all(vapply(text_cols, function(n) identical(a[[n]], b[[n]]), logical(1)))
      archival[[file]] <- data.frame(file, same_shape, numeric_max_abs_error = delta,
        text_columns_identical = same_text, method = "Same saved row order; all numeric columns; text checked separately")
    }
    readr::write_csv(bind_rows(archival), file.path(out, "tables", "original_reproduction_tables.csv"))
  }
  checks <- bind_rows(rows)
  if (nrow(checks)) readr::write_csv(checks, file.path(out, "tables", "historical_comparison.csv"))
  checks
}

make_plots <- function(result, out) {
  d <- result$data; cfg <- result$config
  dir.create(file.path(out, "figures"), showWarnings = FALSE)
  families <- if (requireNamespace("systemfonts", quietly = TRUE)) systemfonts::system_fonts()$family else character()
  family <- if ("Arial Unicode MS" %in% families) "Arial Unicode MS" else "sans"
  theme_set(theme_minimal(base_size = 12, base_family = family) + theme(panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", margin = margin(b = 14)), legend.position = "bottom"))
  palette <- setNames(c("#256d85", "#b85b37", "#718a36", "#845c9c", "#ad546a", "#785d46", "#337c68", "#7d7187"), paste0("G", 1:8))
  if (cfg$clusters > 8) palette <- setNames(grDevices::hcl.colors(cfg$clusters, "Dark 3"), paste0("G", seq_len(cfg$clusters)))
  save <- function(plot, name, width = 9, height = 5.3) {
    device <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png"
    ggsave(file.path(out, "figures", paste0(name, ".png")), plot, width = width, height = height, dpi = 160, device = device, bg = "white")
  }
  save(ggplot(d$world, aes(year, value, color = IndexName)) + geom_line(linewidth = .8) + geom_point(size = 1.5) +
    labs(title = "Historical World series supplied with the dataset", x = "Report year", y = "Index score", color = NULL) +
    guides(color = guide_legend(nrow = 3)), "world_trend", height = 6)
  long <- d$full |> select(all_of(names(broad_names))) |> pivot_longer(everything())
  save(ggplot(long, aes(name, value, fill = name)) + geom_boxplot(width = .5, outlier.alpha = .5) +
    guides(fill = "none") + labs(title = paste(cfg$year, "country-level distributions"), x = NULL, y = "Index score"), "broad_boxplot")
  for (g in names(result$descriptive$groups)) {
    s <- result$descriptive$groups[[g]]; category <- names(s)[1]
    save(ggplot(s, aes(reorder(.data[[category]], global), global)) + geom_col(fill = "#277b87", width = .65) +
      coord_flip() + labs(title = paste("Mean overall score by", g), x = NULL, y = "Mean overall index"), paste0(g, "global"))
  }
  save(ggplot(result$variance, aes(component, variance)) + geom_col(fill = "#277b87", width = .65) +
    geom_line(aes(y = cumulative), color = "#b85b37", linewidth = .8) + geom_point(aes(y = cumulative), color = "#b85b37") +
    scale_x_continuous(breaks = 1:14) + scale_y_continuous(labels = scales::percent) +
    labs(title = "PCA variance explained", subtitle = "Bars: individual components / line: cumulative", x = "Component", y = "Proportion"), "pca_scree")
  save(ggplot(result$clustered, aes(PC1, PC2, color = Continent)) + geom_point(alpha = .8, size = 2.2) +
    labs(title = "Countries in the first two principal components", color = "Continent"), "pca_scores")
  c <- result$choices |> select(k, within_ss, silhouette) |> pivot_longer(-k)
  save(ggplot(c, aes(k, value)) + geom_line(color = "#277b87") + geom_point(color = "#277b87", size = 2) +
    facet_wrap(~name, scales = "free_y", ncol = 1) + scale_x_continuous(breaks = cfg$candidate_k) +
    labs(title = "Cluster count diagnostics", subtitle = "Both metrics come from the same fit for each k", x = "k", y = NULL), "cluster_choice", height = 6.5)
  save(ggplot(result$clustered, aes(PC1, PC2, color = group)) + geom_point(alpha = .85, size = 2.4) +
    scale_color_manual(values = palette) +
    labs(title = paste0("K-means on the first ", cfg$components, " PCs: k = ", cfg$clusters), color = "Group"), "cluster_pca")
  profile <- result$profiles |> select(k, group, economic, education, health, political) |> pivot_longer(-c(k, group))
  save(ggplot(profile, aes(name, value, group = group, color = group)) + geom_point() + geom_line() +
    scale_color_manual(values = palette) +
    facet_wrap(~k, labeller = label_both) + labs(title = "Group profiles under alternative cluster counts", x = NULL, y = "Mean index score", color = "Group") +
    theme(axis.text.x = element_text(angle = 35, hjust = 1)), "cluster_profiles", width = 11)
  save(ggplot(result$stats$residuals, aes(fitted, residual)) + geom_hline(yintercept = 0, color = "grey60") +
    geom_point(alpha = .55, color = "#277b87") + facet_wrap(~grouping) +
    labs(title = "ANOVA residuals against group means", x = "Fitted value", y = "Residual"), "anova_residuals")
  save(ggplot(result$stats$residuals, aes(sample = residual)) + stat_qq(color = "#277b87", alpha = .6) + stat_qq_line() +
    facet_wrap(~grouping) + labs(title = "ANOVA residual normal Q-Q plots"), "anova_qq")
}

html_escape <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE); gsub(">", "&gt;", x, fixed = TRUE)
}
render_example_report <- function(r, out) {
  synthetic <- identical(r$dataset_kind, "synthetic")
  title <- if (synthetic) "Synthetic workflow demonstration" else "Analysis of user-supplied inputs"
  description <- if (synthetic) "All inputs are artificial. These outputs exercise the software and are not evidence about countries or the historical course dataset." else
    "These inputs differ from the verified course-data fingerprints. Inspect their provenance and interpretation independently."
  table_html <- function(x) paste0('<div class="table">', knitr::kable(x, format="html", digits=5, row.names=FALSE, escape=TRUE), '</div>')
  files <- list.files(file.path(out,"tables"), pattern="[.]csv$")
  figs <- list.files(file.path(out,"figures"), pattern="[.]png$")
  content <- c('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
    paste0('<title>', title, '</title>'),
    '<style>body{max-width:1000px;margin:40px auto;padding:0 20px;font:16px/1.7 system-ui;color:#24353c}h1{line-height:1.3}.notice{padding:18px;border-left:4px solid #ba603f;background:#fff6ed}.table{overflow:auto}table{border-collapse:collapse;width:100%;font-size:14px}td,th{padding:8px;border-bottom:1px solid #ddd;white-space:nowrap}img{width:100%;height:auto}a{color:#227181}</style></head><body>',
    paste0('<h1>', title, '</h1><p class="notice">', description, '</p>'),
    paste0('<p>Rows: ', nrow(r$data$full), '; detail indicators: ', length(r$data$vars), '; missing cells: ', sum(r$data$missing$missing_n),
      '; first ', r$config$components, ' PCs explain ', sprintf('%.2f%%',100*r$variance$cumulative[r$config$components]), ' of standardized variance.</p>'),
    '<h2>Cluster diagnostics</h2>', table_html(r$choices), '<h2>Main group summaries</h2>', table_html(r$cluster_summary),
    '<h2>Initialization checks</h2>', table_html(r$stability),
    paste0('<h2>Complete-case sensitivity</h2><p>Complete rows: ',r$complete_case$n, '; shared-row ARI: ',sprintf('%.5f',r$complete_case$ari_shared_countries),
      '. This comparison changes both sample composition and preprocessing.</p>'),
    '<h2>Figures</h2>', paste0('<figure><img src="figures/',figs,'" alt="',figs,'"><figcaption>',figs,'</figcaption></figure>'),
    '<h2>All result tables</h2><ul>',paste0('<li><a href="tables/',files,'">',files,'</a></li>'),
    '</ul><p><a href="run.json">Configuration and fingerprints</a> · <a href="sessionInfo.txt">R environment</a></p></body></html>')
  writeLines(content, file.path(out,"report.html"), useBytes=TRUE)
}
render_report <- function(r, out, comparison) {
  if (!identical(r$dataset_kind, "course")) return(render_example_report(r, out))
  table_html <- function(x, digits = 4) {
    for (col in intersect(c("p", "p adj"), names(x))) x[[col]] <- format.pval(x[[col]], digits = 3, eps = 1e-16)
    paste0('<div class="table-wrap">', knitr::kable(x, format = "html", digits = digits, row.names = FALSE, escape = TRUE), '</div>')
  }
  fig <- function(name, caption) paste0('<figure><img src="figures/', name, '.png" alt="', html_escape(caption), '"><figcaption>', caption, '</figcaption></figure>')
  p <- function(...) paste0("<p>", paste0(..., collapse = ""), "</p>")
  cfg <- r$config; n <- nrow(r$data$full); pct <- sprintf("%.2f%%", 100 * r$variance$cumulative[cfg$components])
  winner <- r$choices$k[which.max(r$choices$silhouette)]
  stability <- r$stability |> group_by(k) |> summarise(seeds = n(), min_ARI = min(ari), max_ARI = max(ari), .groups = "drop")
  m <- r$stats$manova
  cluster_details <- r$clustered |> select(country, ChineseCountryName, group, global, economic, education, health, political)
  files <- list.files(file.path(out, "tables"), pattern = "[.]csv$")
  links <- paste0('<li><a href="tables/', files, '">', files, '</a></li>', collapse = "")
  header <- '<!doctype html><html lang="zh-CN"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>全球性别差异 · 多元统计分析</title><style>
    :root{color-scheme:light}*{box-sizing:border-box}body{margin:0;background:#f4f5f2;color:#203239;font:16px/1.85 -apple-system,BlinkMacSystemFont,"PingFang SC",sans-serif}main{max-width:1100px;margin:auto;padding:52px 30px 80px}header{border-top:5px solid #277b87;padding-top:24px}h1{font-size:38px;line-height:1.3;margin:12px 0}h2{margin-top:48px;font-size:25px}h3{font-size:19px;margin-top:28px}a{color:#206e7c}nav{display:flex;flex-wrap:wrap;gap:20px;border-bottom:1px solid #ccd6d4;padding:20px 0}.eyebrow{color:#47716e;letter-spacing:2px;font-size:13px}.lede{font-size:18px;max-width:850px}.cards{display:grid;grid-template-columns:repeat(4,1fr);gap:14px;margin:28px 0}.card{background:white;border:1px solid #dce2de;padding:18px}.card b{display:block;font-size:29px;color:#226f7b}.card span{font-size:14px;color:#52676b}.note{border-left:4px solid #b85b37;background:#fffaf2;padding:14px 20px;margin:22px 0}.table-wrap{overflow:auto;margin:20px 0}table{border-collapse:collapse;width:100%;font-size:14px;background:white}th,td{padding:10px 13px;border-bottom:1px solid #e0e5e1;white-space:nowrap}th{background:#e9efeb;text-align:left}figure{margin:24px 0;background:white;border:1px solid #dde4df;padding:16px}img{display:block;width:100%;height:auto}figcaption{font-size:14px;color:#566968;padding:8px 10px}details{background:white;border:1px solid #dae1dc;margin:20px 0;padding:15px 18px}summary{cursor:pointer;font-weight:600}code{font-size:14px;background:#e7eeea;padding:2px 5px}.small,footer{font-size:14px;color:#5a6a6a}footer{margin-top:50px;border-top:1px solid #ccd6d4;padding-top:18px}@media(max-width:700px){main{padding:24px 16px}.cards{grid-template-columns:repeat(2,1fr)}h1{font-size:29px}}
    </style></head><body><main>'
  body <- c(header,
    '<header><div class="eyebrow">MULTIVARIATE ANALYSIS · COURSE PROJECT</div><h1>全球性别差异的多元统计分析</h1>',
    p('以 ', cfg$year, ' 年国家层面的性别差异指标为对象，比较经济、教育、健康与政治四个维度，使用主成分分析和聚类探索国家间的相似性。'), '</header>',
    '<nav><a href="#data">数据</a><a href="#overview">总体特征</a><a href="#pca">主成分</a><a href="#clusters">国家分组</a><a href="#stats">组间比较</a><a href="#limits">敏感性与局限</a><a href="#trace">复算与数据表</a></nav>',
    paste0('<div class="cards"><div class="card"><b>', n, '</b><span>国家 / 地区</span></div><div class="card"><b>14</b><span>细分指标</span></div><div class="card"><b>', pct, '</b><span>前 ', cfg$components, ' 个主成分解释方差</span></div><div class="card"><b>', cfg$clusters, '</b><span>主要分组方案</span></div></div>'),
    '<h2 id="data">数据与分析路径</h2>',
    p('输入为课程材料中的 <code>gggr.csv</code>、<code>gggr-detail.csv</code> 和 <code>country.csv</code>。主分析按国家名称连接 ', cfg$year, ' 年的主指标、14 个细分指标与国家属性；历史图使用文件直接提供的 World 序列。'),
    p('主指标无缺失。细分指标共有 ', sum(r$data$missing$missing_n), ' 个缺失值，涉及 ', sum(r$data$coverage$missing_n > 0), ' 个国家；逐指标中位数填补后中心化、标准化。PCA 使用细分指标的 <code>value</code>，不以 female、male 原始量、总体指数、收入或洲别作为降维输入。'),
    '<p><strong>流程：</strong>原始 CSV → 年份筛选与连接检查 → 缺失记录 / 中位数填补 → 标准化 PCA → 主成分上的 K-means → 分组特征与敏感性分析。组间检验直接使用原始主指标。</p>',
    '<details><summary>查看逐指标缺失数</summary>', table_html(r$data$missing, 0), '</details>',
    p('全部年份的历史主指标发现 ', nrow(r$data$history_duplicates), ' 条重复键相关记录，详见数据表；所选年份的分析键唯一。重复历史国家记录未用于重新计算 World 序列。'),
    '<h2 id="overview">总体特征</h2>', table_html(r$descriptive$broad),
    p('在这批国家及本套指标下，教育与健康的平均得分较高；政治赋权得分最低，国家间的标准差也最大。指标衡量的是各维度的性别差距，不能据此判断完整的生活质量或绝对发展水平。'),
    fig('broad_boxplot', '各国原始主指标分布；未删除长尾观测，也未裁剪指标。'),
    fig('world_trend', '数据提供的 World 历史序列；不同年份国家覆盖及指标口径的可比性未另行核验。'),
    '<details><summary>洲别与收入组均值</summary>', table_html(r$descriptive$groups$continent), fig('continentglobal', '按洲别分组的国家均值。'), table_html(r$descriptive$groups$income), fig('incomeglobal', '按收入分组的国家均值。'), '</details>',
    '<h2 id="pca">从 14 个指标到主成分</h2>',
    p('PCA 对标准化数据作奇异值分解。前 ', cfg$components, ' 个主成分保留 ', pct, ' 的标准化总方差，延续原课程项目的降维方案。主成分方向的正负号可以整体翻转；图中保留计算输出的符号，不将某一个方向直接命名为“综合平等水平”。'),
    fig('pca_scree', '所有 14 个主成分的方差解释比例。'), fig('pca_scores', '仅显示前两个主成分；聚类实际使用的维数见上文。'),
    '<details><summary>前几个主成分的载荷</summary>', table_html(data.frame(indicator = rownames(r$pca$rotation), r$pca$rotation[, seq_len(cfg$components)], row.names = NULL)), '</details>',
    '<h2 id="clusters">国家间的相似性</h2>',
    p('主要保留 k = ', cfg$clusters, ' 的方案以便与课程分析比较。候选方案中，平均轮廓系数最高的是 k = ', winner, '；因此主要方案是解释性的分组选择，不能声称数据唯一支持该类别数。'),
    fig('cluster_choice', '每个 k 的两个评价指标来自同一次拟合。'),
    table_html(r$cluster_summary),
    p('G1、G2 等仅为展示编号，按组内总体指数均值从高到低排列；总体指数没有参与聚类。类别解释来自当前特征表，不能沿用某次随机编号的含义。各组是国家层面的统计概括，组内仍存在差异。'),
    fig('cluster_pca', '各国在前两个主成分上的投影及所属组。'),
    fig('cluster_profiles', '替代类别数下的组内均值；不同 k 的同名 G 编号不代表同一个固定类别。'),
    '<details><summary>查看所有国家的分组与主指标</summary>', table_html(cluster_details), '</details>',
    '<h2 id="stats">洲别、收入与协变量</h2>', table_html(r$stats$tests, 7),
    p('普通单因素 ANOVA 保留原分析；Welch 检验是本次新增的方差不齐稳健性补充。二者均显示这批国家中存在组间差异。检验结果不能说明洲别或收入造成了差异。'),
    p('收入组的四维 MANOVA：Wilks λ = ', sprintf('%.5f', m$wilks), '，近似 F = ', sprintf('%.3f', m$statistic), '，p = ', format(m$p, scientific = TRUE, digits = 3), '。'),
    '<details><summary>Tukey 收入组两两比较</summary>', table_html(r$stats$tukey, 7), p('p adj 已作 Tukey 家族内调整；并非所有收入组组合都存在显著差异。'), '</details>',
    '<details><summary>残差与方差诊断</summary>', table_html(r$stats$diagnostics, 6), fig('anova_residuals', '检查各组残差范围与离群观测。'), fig('anova_qq', '残差与正态分位数对照。'),
    p('Brown–Forsythe 使用组内中位数中心化的绝对偏差。诊断未检出偏离不等于假设成立；国家间独立性、空间相关、MANOVA 的多元正态与协方差齐性未得到完整验证。'), '</details>',
    table_html(r$stats$correlations), p('Pearson 相关系数使用各变量的完整配对观测，因此样本量不同。相关关系仅作探索，不能解释为因果效应或推广到个人。'),
    '<h2 id="limits">稳定性与解释边界</h2>',
    p('在相同输入及参数下，针对 k = 2、3 和主要方案分别使用 ', length(cfg$sensitivity_seeds), ' 个预先列出的种子重拟合。ARI 比较分区的一致性，不受类别编号置换影响；1 表示一致。'), table_html(stability),
    paste0('<div class="note">仅保留 ', r$complete_case$n, ' 个 14 项指标均无缺失的国家，并重新标准化、拟合 PCA 与聚类后，与全样本方案在共同国家上的 ARI 为 <strong>', sprintf('%.3f', r$complete_case$ari_shared_countries), '</strong>。初始化稳定并不能消除对缺失处理及样本构成的敏感性。</div>'),
    p('这一对照同时改变了样本覆盖、中心化与缩放参数以及主成分方向，不能把全部变化单独归因于中位数填补。填补不确定性未被传播到聚类或检验；未进行外部验证，也未将国家分组视为预测模型。'),
    '<details><summary>完整观测样本的分组特征</summary>', table_html(r$complete_case$summary), '</details>',
    p('国家属性的来源记录不完整，原说明对部分属性仅写“来自网络”。本报告使用现有课程材料，没有重新核验所有数据与原始发布表的一致性。跨国家检验属于探索性分析，数据不是明确概率抽样；本报告不提出政策因果结论。'),
    '<h2 id="trace">复算、来源与可检查输出</h2>',
    p('原项目的数据整理、R 编程、统计分析和报告均由项目作者完成（本人确认）。原报告已有 AI 辅助代码与写作说明；本次整理另有 AI 协助，新增数据检查、编号修正、同次拟合评价、敏感性分析和自动报告。'),
    if (nrow(comparison)) c(p('与历史保存结果及原主 Rmd 复算结果的关键对照如下；载荷允许符号翻转，聚类比较不依赖编号。'), table_html(comparison, 12)) else p('本次运行未提供历史对照目录。'),
    p('另一份文件名含 knit_ready 的 Rmd 存在嵌套反引号语法错误，保留作历史材料；未将文件名作为版本可靠性的依据。原报告与参考文献仅保存在本地，不作为新报告的生成输入。'),
    '<details><summary>下载全部结果表</summary><ul>', links, '</ul></details>',
    '<p><a href="run.json">运行配置与输入哈希</a> · <a href="sessionInfo.txt">实际 R 环境</a> · <a href="../../docs/METHODS.md">方法与实现对应</a></p>',
    '<footer>这是根据现有课程数据重新计算的分析报告。图、表与数值由同一次运行生成；本页不代表已经核验或复现全部历史报告文字。</footer></main></body></html>')
  writeLines(paste(body, collapse = "\n"), file.path(out, "report.html"), useBytes = TRUE)
}
