# MCAnalysis

Analyze the effect of menstrual cycle phase on health, physiology or other outcomes using GAM models.

Available for both **R** and **Python**.

## Features

- Process period dates and label cycle days (Day 0 = period start)
- Filter cycles by length (default 21-35 days)
- Filters for minimum of 5 observations in each assumed phase for each user.
- Normalize outcomes to percentage of individual mean
- Fit cyclic GAM using Fourier basis
- Identify turning points (peaks/troughs)
- Fit linear phase models between turning points
- Analyze confounders
- Publication-ready visualizations

## Installation

### Python

```bash
pip install git+https://github.com/kyradelray/mcanalysis.git#subdirectory=python-package
```

### R

```r
# Install devtools if needed
install.packages("devtools")

# Install mcanalysis
devtools::install_github("kyradelray/mcanalysis", subdir = "r-package/mcanalysis")
```

## Quick Start

### Python

```python
import pandas as pd
from mcanalysis import MCAnalysis

# Load your data
periods = pd.read_csv("period_dates.csv")
outcomes = pd.read_csv("outcomes.csv")

# Run analysis
mc = MCAnalysis(
    period_dates=periods,
    outcome_data=outcomes,
    id_col='user_id',
    date_col='period_date',
    outcome_col='outcome',
    outcome_date_col='date'
)

results = mc.run(n_splines=2)

# View results
print(mc.summary())

# Plot
mc.plot(title='Menstrual Cycle Effect')
```

### R

```r
library(mcanalysis)

# Load your data
periods <- read.csv("period_dates.csv")
outcomes <- read.csv("outcomes.csv")

# Run analysis
results <- mc_analyze(
    period_dates = periods,
    outcome_data = outcomes,
    id_col = "user_id",
    date_col = "period_date",
    outcome_col = "outcome",
    outcome_date_col = "date",
    k = 2
)

# View results
mc_summary(results)

# Plot
mc_plot(results, title = "Menstrual Cycle Effect")
```

## Data Format

### Period Dates
| user_id | period_date |
|---------|-------------|
| 1       | 2023-01-15  |
| 1       | 2023-02-12  |
| 2       | 2023-01-20  |

### Outcome Data
| user_id | date       | outcome |
|---------|------------|---------|
| 1       | 2023-01-10 | 72.5    |
| 1       | 2023-01-11 | 73.2    |
| 2       | 2023-01-15 | 68.1    |

### Confounder Data (Optional)
| user_id | age | bmi  |
|---------|-----|------|
| 1       | 28  | 22.5 |
| 2       | 32  | 24.1 |

## Plot Customization

Both R and Python support extensive plot customization:

```python
mc.plot(
    show_linear_models=True,    # Show/hide linear phase lines
    show_inflection_points=True, # Show/hide turning point markers
    show_raw_data=True,          # Show daily means
    show_phases=True,            # Show phase shading
    gam_color='steelblue',       # GAM curve color
    linear_colors=['red', 'green']  # Phase line colors
)
```

## License

MIT License - Kyra Delray, University of Oxford

## Citation

If you use this package in your research, please cite:

```
Delray, K. (2026). mcanalysis: Menstrual Cycle Analysis for R and Python.
https://github.com/kyradelray/mcanalysis
```
