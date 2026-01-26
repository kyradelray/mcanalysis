# Data Preparation Guide for mcanalysis

This guide explains how to prepare your data for use with the `mcanalysis` package.

## Required Datasets

You need **two required** datasets and one **optional** dataset:

| Dataset | Required | Description |
|---------|----------|-------------|
| Period dates | Yes | When each person's periods started |
| Outcome data | Yes | Daily measurements of your outcome variable |
| Confounder data | No | Person-level variables (age, BMI, etc.) |

---

## 1. Period Dates

A table with one row per period (menstrual bleed) per person.

### Required columns:
| Column | Description | Example |
|--------|-------------|---------|
| User ID | Unique identifier for each person | `"user_001"` or `12345` |
| Period date | Date the period started | `"2023-01-15"` |

### Example:
```
user_id,period_date
user_001,2023-01-15
user_001,2023-02-12
user_001,2023-03-14
user_002,2023-01-20
user_002,2023-02-18
```

### Tips:
- Each row = one period start date
- Multiple rows per person (one per cycle)
- Date format: `YYYY-MM-DD` recommended
- Don't include period end dates (only start dates needed)
- Need at least 2 periods per person to define a cycle

---

## 2. Outcome Data

A table with daily measurements of your outcome variable.

### Required columns:
| Column | Description | Example |
|--------|-------------|---------|
| User ID | Same IDs as period dates | `"user_001"` |
| Date | Date of measurement | `"2023-01-10"` |
| Outcome | Numeric value to analyze | `72.5` |

### Example:
```
user_id,date,outcome
user_001,2023-01-10,72.5
user_001,2023-01-11,73.2
user_001,2023-01-12,71.8
user_002,2023-01-15,68.1
user_002,2023-01-16,67.9
```

### Tips:
- One row per person per day
- Dates should overlap with period date ranges
- Outcome must be numeric (not categories)
- Missing days are okay (will be excluded)
- Works with: heart rate, HRV, temperature, mood scores, sleep duration, etc.

---

## 3. Confounder Data (Optional)

A table with one row per person containing potential confounding variables.

### Required columns:
| Column | Description | Example |
|--------|-------------|---------|
| User ID | Same IDs as other datasets | `"user_001"` |
| (any confounders) | Numeric variables | `28`, `22.5` |

### Example:
```
user_id,age,bmi,avg_sleep_hours
user_001,28,22.5,7.2
user_002,32,24.1,6.8
user_003,25,21.0,7.5
```

### Tips:
- One row per person (not per day)
- All confounder columns must be numeric
- Common confounders: age, BMI, weight, average activity level
- Missing values are handled (users with missing values excluded from that analysis)

---

## Data Cleaning Checklist

Before running mcanalysis, ensure:

- [ ] **User IDs match** across all datasets
- [ ] **Dates are valid** and parseable (YYYY-MM-DD format recommended)
- [ ] **Outcome is numeric** (convert scales like "1-10" to numbers)
- [ ] **No duplicate rows** (one outcome per person per day)
- [ ] **Sufficient data**: At least 2 periods per person, ~10+ outcome observations per person

---

## Common Data Sources & How to Prepare Them

### From Wearables (Fitbit, Apple Watch, Oura, etc.)

**Raw export typically has:**
- Daily summaries with date and metrics

**To prepare:**
```python
import pandas as pd

# Load your export
df = pd.read_csv("fitbit_export.csv")

# Create outcome dataset
outcomes = df[['user_id', 'date', 'resting_heart_rate']].copy()
outcomes.columns = ['user_id', 'date', 'outcome']
outcomes = outcomes.dropna(subset=['outcome'])
outcomes.to_csv("outcomes.csv", index=False)
```

### From Period Tracking Apps (Clue, Flo, etc.)

**Raw export typically has:**
- Cycle data with period start/end dates

**To prepare:**
```python
import pandas as pd

# Load your export
df = pd.read_csv("clue_export.csv")

# Create period dates (keep only period start dates)
periods = df[['user_id', 'period_start_date']].copy()
periods.columns = ['user_id', 'period_date']
periods = periods.dropna()
periods.to_csv("periods.csv", index=False)
```

### From Survey/Questionnaire Data

**Raw data typically has:**
- Daily entries with multiple variables

**To prepare:**
```python
import pandas as pd

# Load survey data
df = pd.read_csv("daily_survey.csv")

# Create outcome dataset (e.g., mood score)
outcomes = df[['participant_id', 'survey_date', 'mood_score']].copy()
outcomes.columns = ['user_id', 'date', 'outcome']
outcomes.to_csv("outcomes.csv", index=False)

# Create confounders (baseline survey)
baseline = df.groupby('participant_id').first().reset_index()
confounders = baseline[['participant_id', 'age', 'bmi']].copy()
confounders.columns = ['user_id', 'age', 'bmi']
confounders.to_csv("confounders.csv", index=False)
```

---

## Example: Complete Workflow

```python
import pandas as pd
from mcanalysis import MCAnalysis

# Load your prepared data
periods = pd.read_csv("periods.csv")
outcomes = pd.read_csv("outcomes.csv")
confounders = pd.read_csv("confounders.csv")  # optional

# Check your data
print("Periods:", periods.shape)
print("Outcomes:", outcomes.shape)
print("Unique users in periods:", periods['user_id'].nunique())
print("Unique users in outcomes:", outcomes['user_id'].nunique())

# Run analysis
mc = MCAnalysis(
    period_dates=periods,
    outcome_data=outcomes,
    confounder_data=confounders,  # optional
    id_col='user_id',           # your ID column name
    date_col='period_date',     # your period date column name
    outcome_col='outcome',      # your outcome column name
    outcome_date_col='date'     # your outcome date column name
)

results = mc.run(n_splines=2)
print(mc.summary())
mc.plot(title='My Analysis')
```

---

## Troubleshooting

| Error | Cause | Solution |
|-------|-------|----------|
| "No data after processing" | User IDs don't match between datasets | Check ID column names and values match exactly |
| "No users meet criteria" | Too few observations per person | Lower `min_negative_obs` and `min_positive_obs` in `run()` |
| Few users in output | Cycles outside 21-35 days filtered out | Adjust `min_cycle_length` and `max_cycle_length` |
| NaN in results | Missing values in outcome | Clean outcome data before running |

---

## Need Help?

- GitHub Issues: https://github.com/kyradelray/mcanalysis/issues
- Full documentation: See README.md
