# mcanalysis

Menstrual Cycle Analysis - Analyze the effect of menstrual cycle phase on outcomes using GAM models.

## Installation

```bash
pip install -e .
```

## Quick Start

```python
import pandas as pd
from mcanalysis import MCAnalysis

# Load data
periods = pd.read_csv("period_dates.csv")
outcomes = pd.read_csv("outcomes.csv")

# Run analysis
mc = MCAnalysis(periods, outcomes)
results = mc.run()

# View and plot
print(mc.summary())
mc.plot(save_path="cycle_effect.png")
```

See main README for full documentation.
