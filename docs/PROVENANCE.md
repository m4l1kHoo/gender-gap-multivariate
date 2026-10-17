# 原始材料与本次工作

## 作者与来源

用户确认原项目为课程项目，数据整理、R 代码、统计分析和报告均由本人完成。原报告已有由作者主导、AI 辅助代码与文字的说明；“全部负责”不改写成未使用辅助工具。本次代码重构、检查及报告也由 AI 协助完成。

原始项目为 `MVA_coursework`，本次工作在独立目录进行。原目录所有 58 个文件的路径、大小与 SHA-256 记录于仅在本地保留的 `source_manifest.json`；本地 `private_originals/` 保留逐字节副本，包括历史报告、参考材料、输出和辅助文件。没有向原目录写入缓存、改动文件或运行输出。

参考作业、论文与其他课题数据不归作作者成果，不参与新分析。没有伪造旧 Git 历史、实验日期或研究贡献。公开仓库只记录本次真实的整理与发布历史。

## 新旧核心材料对应

| 原材料 / 单元 | 新实现 | 分类与含义 |
|---|---|---|
| `gender_gap_analysis.Rmd` / read-data、missing-summary | `analysis.R::prepare_data` | 重构并加保护：显式年份、唯一键、字段类型、连接覆盖；保留原拼写修复。 |
| world-trend、world-change、broad-summary、group-summary | `descriptive_analysis` | 原统计含义保留，World 与国家横截面分开。 |
| pca | `median_impute`、`fit_pca` | 原中位数填补及标准化 PCA 保留；新增填补日志、无效列拒绝。 |
| cluster-selection | `fit_clusters`、`run_analysis` | 修复：一次拟合同时算 WSS 与 silhouette；每个 k 独立设置相同起始 seed，避免顺序改变随机流。 |
| cluster-model、country-lists | `cluster_labels`、`summarize_clusters`、完整成员表 | 主四组参数保留；替换硬编码编号解释，完整国家名单可核查。 |
| group-tests | `statistical_analysis` | 保留 ANOVA、Tukey 和 Wilks MANOVA；补充有效样本量。 |
| covariate-correlation | `statistical_analysis` | 保留三个相关变量及 log10 GDP 变换；增加完整配对 n。 |
| 八张历史图及硬编码文字 | `report.R` | 新报告从结果取数；保留相应统计图，增加三张方案/诊断图。 |
| 无原对应实验 | 初始化种子、完整观测、Welch 与诊断 | 本次新增分析；不是原课程时已完成的实验。 |
| knit_ready 另一 Rmd | 私有保存 + `alternate_parse.txt` | 静态抽取可定位语法错误；不运行损坏版本，不凭文件名选择最终版本。 |
| 原三份 CSV | `data/` 及 `manifest.json` | 原字节复制，不修写原数据。 |

没有替换研究问题或人为调整参数来贴合旧图。主要四组仍沿用课程设置；k=2 的轮廓优势与完整观测敏感性如实报告。

## 历史复算路径

`reproduce_original.R` 从 `private_originals/` 复制主 Rmd、另一 Rmd 和三份 CSV 到 `.cache/historical-source/`，在副本目录使用 `rmarkdown::render`，保留原计算语句。输出到 `results/historical/`；原 Rmd 内相对输出先在缓存副本生成，再复制至该结果目录。需要 `rmarkdown`、`stringr` 和 Pandoc，以及新分析已列的 R 包。Pandoc 需在 PATH 或 RSTUDIO_PANDOC 指定的位置可用；此历史入口需要私有原件，公开示例不依赖它。

```sh
Rscript --vanilla reproduce_original.R
```

历史复算 HTML 的日期按原 Rmd 的 `Sys.Date()` 动态生成，因此它是本次复算产物，不是旧提交记录。原脚本的图形警告隐藏设置和默认绘图仍属原样执行；数值复算验证不等于确认历史报告每一句叙述正确。

`results/full/tables/original_reproduction_tables.csv` 对比所有 12 张原 CSV 与复算 CSV 的数值列和文字列。`historical_comparison.csv` 对照新主分析与两组历史证据的特征值、符号对齐载荷、国家主指标及分区 ARI。

原 `broad_summary.csv` 使用完整指标名称，主 Rmd 复算使用短名；全部数值相同。其余 11 张表数值与文字均一致。这支持主 Rmd 的计算与保存结果一致，但没有额外证据证明哪个文件就是当时最终提交版。

## 公开范围

公开内容包括 R 源码、数学与运行说明、合成数据生成器、验证记录，以及 `examples/course-results/` 中的聚合结果。原始 CSV、逐国家分组表、旧报告、外部参考、原目录完整文件名清单与本地缓存不上传。

原数据及部分属性的来源、许可仍不完整；没有把现有文件当作已经取得再分发权。`generate_demo.R` 使用固定随机数生成完全人工的数据，不从原国家数据采样，明确用于验证软件流程。合成结果单独标注，不用于支持 README 的国家层面发现。
