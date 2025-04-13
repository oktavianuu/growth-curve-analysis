# WHO-Style Growth Curve Modeling in R

This project models weight-for-age growth curves for boys using WHO-recommended LMS methods via the GAMLSS framework. It includes statistical validation, diagnostic visualizations, and publication-quality centile plots.

## 📊 Methods

- **Data**: 213 observations of boys aged 1–70 months
- **Model**: GAMLSS with BCPE distribution
- **Validation**: AIC, GAIC, and worm plot diagnostics
- **Output**:
  - Z-score centile table
  - WHO-style growth plot with publication-level formatting

## 🔧 Tools

- R
- `gamlss`, `ggplot2`, `dplyr`, `tidyr`
- R Markdown

## Key Insight

Although the BCCG model yielded the lowest AIC/GAIC, worm plots showed systematic deviations in the tails. The BCPE model produced a better overall fit with negligible AIC difference, justifying its selection.

## Author's Goal

To explore statistical modeling for real-world applications in health, improve understanding of GAMLSS, and produce high-quality, reproducible research workflows.

## 📍 Data Source

The sample data used in this project originates from a research study conducted by my lecturer, involving children from **Desa Jungkat, Kecamatan Jongkat, Pontianak Province, West Kalimantan, Indonesia**.

The dataset consists of 213 observations of boys aged 1–60 months, with variables including body weight (kg) and age (months). It was collected for the purpose of evaluating growth patterns and generating local reference curves.

*Note: All identifiable information has been removed to ensure data privacy.*

---

### 📎 Reference

Stasinopoulos, M., Rigby, R., & Akantziliotou, C. (2008). *Flexible Regression and Smoothing Using GAMLSS in R*. Chapman & Hall/CRC.
