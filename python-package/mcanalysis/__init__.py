"""
mcanalysis - Menstrual Cycle Analysis Package

Analyze the effect of menstrual cycle phase on outcomes using GAM models.
"""

from .processing import process_periods, filter_cycles, filter_users
from .models import normalize_outcome, fit_gam, analyze_confounders, find_turning_points, fit_phase_models
from .visualization import plot_cycle_effect
from .analysis import MCAnalysis
from .exploratory import (
    plot_outcome_distribution,
    plot_cycle_lengths,
    plot_confounder_distributions,
    plot_observations_per_user,
    data_summary,
    run_exploratory_analysis
)

__version__ = "0.1.0"
__all__ = [
    "process_periods",
    "filter_cycles",
    "filter_users",
    "normalize_outcome",
    "fit_gam",
    "analyze_confounders",
    "find_turning_points",
    "fit_phase_models",
    "plot_cycle_effect",
    "MCAnalysis",
    # Exploratory analysis
    "plot_outcome_distribution",
    "plot_cycle_lengths",
    "plot_confounder_distributions",
    "plot_observations_per_user",
    "data_summary",
    "run_exploratory_analysis",
]
