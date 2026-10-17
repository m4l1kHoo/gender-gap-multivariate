# 方法与代码对应

本项目对已有课程数据进行描述性多元统计分析。主要问题是不同国家在这套指标上的差异及相似性，不建立个体层面的结论，也不估计收入、洲别或制度的因果效应。

## 完整执行顺序

| 步骤 | 实现 | 输入 → 输出 | 条件与操作 |
|---|---|---|---|
| 配置与校验 | `run.R`、`validate_config` | JSON、3 个 CSV、可选清单 → 已核对的路径与哈希 | 使用现有数据无需手工改路径；可用命令行指定。输出目录须为空。 |
| 筛选与连接 | `prepare_data` | 所选年主指标、细分指标、国家属性 → `analysis_input.csv` | 以 country 连接；国家—指标键唯一；排除 World；保留原国家顺序。 |
| 描述统计 | `descriptive_analysis` | 原始主指标与独立 World 序列 → 均值、分位数、趋势等表 | 主指标不填补；World 不从国家行重算。 |
| 缺失处理 | `median_impute` | 14 个细分指标 → `pca_input.csv`、中位数与逐单元格填补日志 | 不剔除观测长尾；中位数由当前全样本已观测值计算。 |
| 降维 | `fit_pca` | 填补后矩阵 → 载荷、得分、解释方差、缩放参数 | `prcomp(center=TRUE, scale.=TRUE)`。 |
| 类别数比较 | `fit_clusters` | 默认前 4 个 PC → k=2…8 的分组及评价 | 每个 k 重设 seed=2026，100 次初始化；同一拟合产出两种指标。 |
| 主要分组 | `fit_clusters`、`summarize_clusters` | 同一得分矩阵 → 默认 k=4，200 次初始化 | 不用总体指数选簇或算距离；其组内均值仅用于展示编号排序。 |
| 组间分析 | `statistical_analysis` | 原始总体/四维主指标及属性 → ANOVA、Tukey、MANOVA、相关、诊断 | 不使用填补后的细分指标进行这些检验。 |
| 敏感性 | `run_analysis` | 固定种子列表 / 完整观测样本 → 稳定性和完整观测对照 | 本次新增，不冒充历史实验。 |
| 输出 | `report.R` | 同一内存结果对象 → CSV、PNG、HTML | 无 Notebook 状态；表图和主要报告数值自动生成。 |

`--stage data` 到数据与描述统计为止；`pca` 再完成填补和 PCA；`cluster` 再完成候选和主要聚类；`all` 包括统计检验、敏感性与报告。每次都从原始 CSV 执行依赖步骤，不读取之前运行保存的内存对象。

## PCA

令填补后的矩阵为 X（n=144，p=14）。对每列用样本均值和样本标准差计算 Zᵢⱼ=(Xᵢⱼ−μⱼ)/sⱼ，其中 s 的分母为 n−1。SVD 为 Z=UDVᵀ，载荷为 V，得分 T=ZV，特征值 λⱼ=Dⱼⱼ²/(n−1)。解释比例 λⱼ/Σλ，默认保留前四列 T。

对应 `fit_pca` 与 `run_analysis`。没有对白化的主成分再标准化，故各 PC 在聚类距离中的权重仍取决于保留方差。载荷向量的整体正负号不唯一，历史对照先进行符号对齐，再比较误差；图不赋予正负方向固定价值含义。[R 的 prcomp 文档](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/prcomp.html)说明了 SVD、缩放和符号约定。

中位数填补会改变相关结构并压缩部分变异。这里延续原分析，不将它表述为经过缺失机制验证的最优处理。完整观测对照同时改变样本、缩放参数和主成分，不能单独识别填补的影响。

## K-means 与评价

在保留的得分空间最小化 Σᵢ‖Tᵢ−c₍gᵢ₎‖²，即组内平方和。调用 R 的 Hartigan–Wong 算法，`iter.max=10` 与原默认值一致；候选与主要方案的 `nstart` 分别为 100、200。记录迭代数、ifault 与警告。随机算法不能保证全局最优。[R 的 kmeans 文档](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/kmeans.html)给出参数和输出定义。

轮廓系数按 sᵢ=(bᵢ−aᵢ)/max(aᵢ,bᵢ) 计算，其中 aᵢ 为同组平均欧氏距离，bᵢ 为到其他各组平均距离的最小值；孤立单点按软件定义为 0。报告使用所有国家的 sᵢ 均值。[cluster::silhouette 文档](https://stat.ethz.ch/R-manual/R-devel/library/cluster/html/silhouette.html)提供定义。

`cluster_labels` 按各组原始总体指数均值降序赋 G1…Gk，仅用于排序显示，不改变所属分区。不同 k 或不同样本的 G1 不能当作同一固定类别。主要四组在本批数据中的特征分别表现为：G1 总体和政治均值较高；G2 教育、健康均值较高而政治较低；G3 教育均值明显较低；G4 经济参与均值最低。它们不是规范性国家评级。

`adjusted_rand` 根据两个分区的列联表计算调整兰德指数：观察同组点对数减去固定组规模下的随机期望，再除以平均可同组点对数减去该期望。标签置换不影响结果。测试同时覆盖手算的 −0.5 情形和一致分区，避免把“编号相等率”误当作稳定性。

## 检验与相关

- `aov(global ~ Continent)` 和 `aov(global ~ IncomeLevel)` 延续原总体指数单因素 ANOVA；Tukey 只对收入组作六个两两比较，并采用其家族内调整 p 值。
- `manova(cbind(economic, education, health, political) ~ IncomeLevel)` 延续原四维检验，报告 Wilks λ 及其近似 F。[R 的 summary.manova 文档](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/summary.manova.html)描述输出。
- 本次新增 `oneway.test(var.equal=FALSE)` 的 Welch 检验，用于对照不要求各组方差相等的均值比较。[R 的 oneway.test 文档](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/oneway.test.html)说明其近似检验。
- 新增 Brown–Forsythe：先算 |yᵢ−median(y 所在组)|，再对绝对偏差作单因素 ANOVA；同时给出残差 Shapiro–Wilk、Q-Q 图和组内方差。未拒绝诊断原假设不证明模型假设成立；多元协方差齐性、国家间独立性和空间依赖尚未充分评估。
- 总体指数分别与 log10(GDPperCapita)、HumanCapitalIndex、PopulationSexRatio 计算 Pearson r。每项使用完整配对，并输出 n。无预测性能声明。

国家是给定课程数据的分析对象，不是有明确抽样设计的随机样本。p 值是模型条件下的探索结果，不能自动推广至超总体，也不能代替实际效应大小、领域解释或因果识别。

## 配置与边界

默认年份 2017、随机种子 2026、4 个 PC、4 组，均来自原主要分析。10 个敏感性种子显式写在配置中。当前只验证提供的数据与默认主配置；其他年份若没有细分数据会直接拒绝运行。更换数据或维数不代表沿用本报告已验证的结论。

世界曲线直接绘制提供的汇总值；没有验证跨年度样本覆盖的一致性，不把曲线变动解释为固定国家面板的变化。全流程不使用网络下载、外部服务、付费计算或历史工作区状态。
