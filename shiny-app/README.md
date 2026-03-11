# MCAnalysis Shiny App

Interactive web application for analyzing health outcomes across the menstrual cycle.

## Requirements

Install required R packages:

```r
install.packages(c("shiny", "bslib", "DT", "ggplot2", "patchwork"))

# Install mcanalysis package from parent directory
devtools::install("../r-package/mcanalysis")
```

## Running the App

```r
# From R console
shiny::runApp("shiny-app")

# Or from command line
Rscript -e "shiny::runApp('shiny-app')"
```

## Data Format

### Period Dates (required)
CSV with columns:
- `userid`: User identifier
- `period_date`: Date of period start (YYYY-MM-DD)

### Outcome Data (required)
CSV with columns:
- `userid`: User identifier
- `date`: Date of measurement (YYYY-MM-DD)
- `outcome`: Numeric outcome value

### Confounders (optional)
CSV with columns:
- `userid`: User identifier
- Additional numeric columns for confounders (e.g., `age`, `mean_steps`)

## Features

- **Data Preview**: View uploaded data before analysis
- **Exploratory Analysis**: Distribution plots, cycle length analysis, data quality checks
- **Cycle Effect Visualization**: GAM-based analysis with confidence intervals
- **Phase Analysis**: Linear models for each cycle phase
- **Confounder Analysis**: Test effects of confounding variables
- **Downloads**: Export plots, processed data, and statistical results
