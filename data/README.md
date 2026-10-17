# Input data

The three CSV files are byte-identical copies of the course inputs under the original `MVA_coursework/性别差异/性别差异` directory. `manifest.json` records their SHA-256 digests. `source_readme.txt` is the original GB18030 explanation converted to UTF-8; this conversion applies only to the explanation, not to the CSV files.

| File | Rows | Columns | Role |
|---|---:|---:|---|
| `gggr.csv` | 8,170 | 5 | Country and World overall/subindex scores, 2006–2017 |
| `gggr-detail.csv` | 2,016 | 7 | 144 countries × 14 detailed indicators, all 2017 |
| `country.csv` | 150 | 13 | Country names, continent, income group and contextual attributes |

The course explanation attributes the gender-gap indicators to the Global Gender Gap Reports. It attributes income group, GDP and related attributes to the 2017 report, but describes some other country attributes only as coming from the internet. No original retrieval URLs, extraction script or complete provenance trail were found. Values have not been re-extracted from external reports. No redistribution license is established by these local files.

## Fields used

- Broad scores: `year`, `country`, `IndexName`, `value`, `rank`. `World` is excluded from the country analysis and retained for the historical chart.
- Detailed input: `year`, `country`, `DetailIndexName`, `value`. `female` and `male` are retained in the source file and are not the PCA features.
- Attributes: `ChineseCountryName`, `Continent`, `IncomeLevel`, `GDPperCapita`, `HumanCapitalIndex`, `PopulationSexRatio`. Other attributes are preserved in the joined table but do not enter PCA, clustering or the reported correlations.
- According to the course explanation, GDP per capita is in US dollars, human capital is the supplied index, and population sex ratio is female population / male population. These are not newly verified measurements.

The detail variable `Enrolment in primary  education` contains two spaces; that original spelling is retained. The corrupted string `Legislators, senior o?cials and managers` is explicitly mapped to `Legislators, senior officials and managers`, as in the original Rmd. Whitespace around country-table column headers is removed.

## Coverage and anomalies

The 2017 joins retain all 144 countries with complete broad scores, continent and income labels. Six rows in the attribute file do not belong to the selected country set. There are 85 missing detailed values in 54 countries; only 90 countries have all 14 observations. Human capital correlations use 121 complete pairs, while the other two reported correlations use 144.

`gggr.csv` contains two 2006 Algeria / Political Empowerment records with different values and ranks. They are recorded in `historical_duplicate_records.csv`, not deduplicated or used in the 2017 analysis. World values are read directly from their supplied records, not calculated from those country rows.

The workflow rejects duplicate keys in the selected year, unmatched countries, missing detailed records, nonnumeric scores, infinite values, all-missing columns and constant PCA features. A missing *record* is distinguished from an explicit missing *value*. It does not delete observed outliers or clip indicator values.
