"""
mcanalysis - Menstrual Cycle Analysis Package

Analyze the effect of menstrual cycle phase on outcomes using GAM models.
"""

from .processing import process_periods, filter_cycles, filter_users
from .models import normalize_outcome, fit_gam, analyze_confounders, find_inflections, fit_phase_models
from .visualization import plot_cycle_effect
from .analysis import MCAnalysis

__version__ = "0.1.0"
__all__ = [
    "process_periods",
    "filter_cycles",
    "filter_users",
    "normalize_outcome",
    "fit_gam",
    "analyze_confounders",
    "find_inflections",
    "fit_phase_models",
    "plot_cycle_effect",
    "MCAnalysis",
]
