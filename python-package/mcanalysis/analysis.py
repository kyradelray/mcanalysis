"""
Main analysis class for menstrual cycle analysis.
"""

import pandas as pd
import numpy as np
from typing import Dict, List, Optional, Any
from dataclasses import dataclass

from .processing import process_periods, filter_cycles, filter_users, add_dummy_day
from .models import (
    normalize_outcome, fit_gam, analyze_confounders,
    find_turning_points, fit_phase_models,
    GAMResult, PhaseModel, ConfounderResult
)
from .visualization import plot_cycle_effect, plot_confounder_effects


@dataclass
class MCAnalysisResult:
    """Container for complete analysis results."""
    processed_data: pd.DataFrame
    gam_result: GAMResult
    turning_points: List[float]
    phase_models: List[PhaseModel]
    confounder_results: Optional[List[ConfounderResult]]
    n_users: int
    n_observations: int
    n_cycles: int


class MCAnalysis:
    """
    Menstrual Cycle Analysis

    Analyze the effect of menstrual cycle phase on outcomes using GAM models.

    Parameters
    ----------
    period_dates : pd.DataFrame
        DataFrame with columns for user ID and period start dates
    outcome_data : pd.DataFrame
        DataFrame with columns for user ID, date, and outcome values
    confounder_data : pd.DataFrame, optional
        DataFrame with columns for user ID and confounder variables
    id_col : str
        Column name for user ID (default "id")
    date_col : str
        Column name for period dates (default "period_date")
    outcome_col : str
        Column name for outcome variable (default "outcome")
    outcome_date_col : str
        Column name for outcome dates (default "date")

    Examples
    --------
    >>> import pandas as pd
    >>> from mcanalysis import MCAnalysis
    >>>
    >>> # Load your data
    >>> periods = pd.read_csv("period_dates.csv")
    >>> outcomes = pd.read_csv("outcomes.csv")
    >>>
    >>> # Run analysis
    >>> mc = MCAnalysis(periods, outcomes)
    >>> results = mc.run()
    >>>
    >>> # View results
    >>> print(results.gam_result.summary)
    >>> mc.plot()
    """

    def __init__(
        self,
        period_dates: pd.DataFrame,
        outcome_data: pd.DataFrame,
        confounder_data: Optional[pd.DataFrame] = None,
        id_col: str = "id",
        date_col: str = "period_date",
        outcome_col: str = "outcome",
        outcome_date_col: str = "date"
    ):
        self.period_dates = period_dates
        self.outcome_data = outcome_data
        self.confounder_data = confounder_data
        self.id_col = id_col
        self.date_col = date_col
        self.outcome_col = outcome_col
        self.outcome_date_col = outcome_date_col

        self.results: Optional[MCAnalysisResult] = None

    def run(
        self,
        min_cycle_length: int = 21,
        max_cycle_length: int = 35,
        min_negative_obs: int = 5,
        min_positive_obs: int = 5,
        n_splines: int = 10,
        confounder_cols: Optional[List[str]] = None
    ) -> MCAnalysisResult:
        """
        Run the complete analysis pipeline.

        Parameters
        ----------
        min_cycle_length : int
            Minimum cycle length to include (default 21)
        max_cycle_length : int
            Maximum cycle length to include (default 35)
        min_negative_obs : int
            Minimum observations with negative cycle days per user (default 5)
        min_positive_obs : int
            Minimum observations with positive cycle days per user (default 5)
        n_splines : int
            Number of splines for GAM (default 10)
        confounder_cols : List[str], optional
            Specific confounder columns to analyze

        Returns
        -------
        MCAnalysisResult
            Complete analysis results
        """
        # Step 1: Process periods and label cycle days
        print("Processing period dates...")
        df = process_periods(
            self.period_dates,
            self.outcome_data,
            id_col=self.id_col,
            date_col=self.date_col,
            outcome_col=self.outcome_col,
            outcome_date_col=self.outcome_date_col
        )

        if len(df) == 0:
            raise ValueError("No data after processing. Check your input data.")

        # Step 2: Filter cycles by length
        print("Filtering cycles by length...")
        df = filter_cycles(df, min_cycle_length, max_cycle_length)

        # Step 3: Filter users by observation count
        print("Filtering users by observation count...")
        df = filter_users(df, min_negative_obs, min_positive_obs)

        if len(df) == 0:
            raise ValueError("No users meet the observation criteria.")

        # Step 4: Add dummy day 21
        print("Adding dummy day for cyclical analysis...")
        df = add_dummy_day(df)

        # Step 5: Normalize outcome
        print("Normalizing outcome to percentage of individual mean...")
        df = normalize_outcome(df, outcome_col="outcome")

        # Step 6: Fit GAM with cyclic Fourier basis
        print("Fitting cyclic GAM model...")
        gam_result = fit_gam(df, outcome_col="a", n_splines=n_splines, cyclic=True, n_harmonics=n_splines)

        # Step 7: Find turning points (where curve changes direction)
        print("Finding turning points...")
        turning_points = find_turning_points(
            gam_result,
            find_peaks_troughs=True,
            round_to_whole_days=True
        )

        # Step 8: Fit phase models from GAM predictions at turning points
        print("Fitting phase models...")
        phase_models = fit_phase_models(df, turning_points, gam_result=gam_result)

        # Step 9: Analyze confounders if provided
        confounder_results = None
        if self.confounder_data is not None:
            print("Analyzing confounders...")
            # Rename confounder id column to match processed data
            conf_data = self.confounder_data.copy()
            if self.id_col in conf_data.columns and "user_id" not in conf_data.columns:
                conf_data = conf_data.rename(columns={self.id_col: "user_id"})

            confounder_results = analyze_confounders(
                df,
                conf_data,
                id_col="user_id",
                confounder_cols=confounder_cols
            )

        # Compile results
        n_users = df["user_id"].nunique()
        n_observations = len(df[~df.get("is_dummy", False)])
        n_cycles = df.groupby("user_id")["cycle_id"].nunique().sum()

        self.results = MCAnalysisResult(
            processed_data=df,
            gam_result=gam_result,
            turning_points=turning_points,
            phase_models=phase_models,
            confounder_results=confounder_results,
            n_users=n_users,
            n_observations=n_observations,
            n_cycles=n_cycles
        )

        print(f"\nAnalysis complete!")
        print(f"  Users: {n_users}")
        print(f"  Observations: {n_observations}")
        print(f"  Cycles: {n_cycles}")
        print(f"  Turning points found: {len(turning_points)}")
        print(f"\n{gam_result.summary}")

        return self.results

    def plot(
        self,
        title: str = "Menstrual Cycle Effect on Outcome",
        save_path: Optional[str] = None,
        **kwargs
    ):
        """
        Plot the GAM results.

        Parameters
        ----------
        title : str
            Plot title
        save_path : str, optional
            Path to save the figure
        **kwargs
            Additional arguments passed to plot_cycle_effect()

        Returns
        -------
        matplotlib.figure.Figure
        """
        if self.results is None:
            raise ValueError("Must run analysis first. Call .run()")

        return plot_cycle_effect(
            self.results.gam_result,
            turning_points=self.results.turning_points,
            phase_models=self.results.phase_models,
            raw_data=self.results.processed_data,
            title=title,
            save_path=save_path,
            **kwargs
        )

    def plot_confounders(
        self,
        title: str = "Confounder Effects",
        save_path: Optional[str] = None,
        **kwargs
    ):
        """
        Plot confounder analysis results.

        Parameters
        ----------
        title : str
            Plot title
        save_path : str, optional
            Path to save the figure
        **kwargs
            Additional arguments

        Returns
        -------
        matplotlib.figure.Figure
        """
        if self.results is None:
            raise ValueError("Must run analysis first. Call .run()")

        if self.results.confounder_results is None:
            raise ValueError("No confounder analysis was performed.")

        return plot_confounder_effects(
            self.results.confounder_results,
            title=title,
            save_path=save_path,
            **kwargs
        )

    def summary(self) -> str:
        """
        Generate a text summary of the analysis results.

        Returns
        -------
        str
            Summary text
        """
        if self.results is None:
            return "No results. Run .run() first."

        r = self.results
        g = r.gam_result

        text = f"""
================================================================================
                     MENSTRUAL CYCLE ANALYSIS SUMMARY
================================================================================

DATA SUMMARY
------------
  Users analyzed: {r.n_users}
  Total observations: {r.n_observations}
  Total cycles: {r.n_cycles}

GAM MODEL RESULTS
-----------------
  Effective degrees of freedom: {g.edf:.2f}
  Deviance explained: {g.deviance_explained * 100:.1f}%
  P-value for cycle effect: {g.p_value:.2e} {'***' if g.p_value < 0.001 else '**' if g.p_value < 0.01 else '*' if g.p_value < 0.05 else '(not significant)'}

TURNING POINTS
--------------
  Number found: {len(r.turning_points)}
  Days: {', '.join([f'{tp:.1f}' for tp in r.turning_points]) if r.turning_points else 'None'}

PHASE MODELS (Daily Change)
---------------------------
"""
        for pm in r.phase_models:
            sig = "***" if pm.p_value < 0.001 else "**" if pm.p_value < 0.01 else "*" if pm.p_value < 0.05 else ""
            text += f"  Phase {pm.phase} (Day {pm.start_day:.0f} to {pm.end_day:.0f}):\n"
            text += f"    Slope: {pm.slope:.4f} ± {pm.slope_se:.4f} (p={pm.p_value:.3e}) {sig}\n"
            text += f"    R²: {pm.r_squared:.3f}\n"

        if r.confounder_results:
            text += "\nCONFOUNDER ANALYSIS\n-------------------\n"
            for cr in r.confounder_results:
                sig = "***" if cr.p_value < 0.001 else "**" if cr.p_value < 0.01 else "*" if cr.p_value < 0.05 else ""
                text += f"  {cr.confounder}:\n"
                text += f"    Coefficient: {cr.coefficient:.4f} [{cr.ci_lower:.4f}, {cr.ci_upper:.4f}]\n"
                text += f"    P-value: {cr.p_value:.3e} {sig}\n"

        text += "\n================================================================================\n"

        return text
