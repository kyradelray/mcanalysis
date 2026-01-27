"""
Statistical modeling functions for menstrual cycle analysis.
"""

import pandas as pd
import numpy as np
from typing import Dict, List, Tuple, Optional, Any
from dataclasses import dataclass
import warnings


@dataclass
class GAMResult:
    """Container for GAM results."""
    model: Any
    predictions: pd.DataFrame
    summary: str
    p_value: float
    edf: float  # Effective degrees of freedom
    deviance_explained: float


@dataclass
class PhaseModel:
    """Container for phase linear model results."""
    phase: int
    start_day: float
    end_day: float
    slope: float
    intercept: float
    slope_se: float
    p_value: float
    r_squared: float


@dataclass
class ConfounderResult:
    """Container for confounder analysis results."""
    confounder: str
    coefficient: float
    std_error: float
    p_value: float
    ci_lower: float
    ci_upper: float


def normalize_outcome(
    df: pd.DataFrame,
    outcome_col: str = "outcome"
) -> pd.DataFrame:
    """
    Normalize outcome to percentage of each user's mean.

    Parameters
    ----------
    df : pd.DataFrame
        Dataset with outcome variable
    outcome_col : str
        Name of outcome column

    Returns
    -------
    pd.DataFrame
        Dataset with normalized outcome 'a' added
    """
    df = df.copy()

    # Calculate each user's mean outcome
    user_means = df.groupby("user_id")[outcome_col].transform("mean")

    # Express outcome as percentage of user's mean
    df["a"] = (df[outcome_col] / user_means) * 100

    # Store the user means for reference
    df["user_mean"] = user_means

    return df


def fit_gam(
    df: pd.DataFrame,
    outcome_col: str = "a",
    day_col: str = "cycle_day",
    n_splines: int = 10,
    cyclic: bool = True,
    n_harmonics: int = 2,
    cycle_length: int = 28
) -> GAMResult:
    """
    Fit a Generalized Additive Model of cycle day on normalized outcome.

    Parameters
    ----------
    df : pd.DataFrame
        Dataset with normalized outcome
    outcome_col : str
        Name of outcome column (default 'a')
    day_col : str
        Name of cycle day column
    n_splines : int
        Number of splines for smooth term (used if cyclic=False)
    cyclic : bool
        Whether to use cyclic Fourier basis (True) or standard splines (False)
    n_harmonics : int
        Number of Fourier harmonics for cyclic fit (default 2)
    cycle_length : int
        Expected cycle length in days for Fourier basis (default 28)

    Returns
    -------
    GAMResult
        GAM results including model, predictions, and statistics
    """
    import statsmodels.api as sm

    # Prepare data - remove NaN values
    clean_df = df.dropna(subset=[day_col, outcome_col])
    X_days = clean_df[day_col].values
    y = clean_df[outcome_col].values

    if cyclic:
        # Use Fourier basis for cyclic pattern
        # This naturally enforces periodicity
        X_design = _create_fourier_basis(X_days, n_harmonics, cycle_length)

        # Fit OLS with Fourier terms
        model = sm.OLS(y, X_design).fit()

        # Generate predictions
        day_range = np.linspace(-14, 13, 100)
        X_pred = _create_fourier_basis(day_range, n_harmonics, cycle_length)
        predictions = model.predict(X_pred)

        # Bootstrap for confidence intervals
        n_boot = 200
        boot_preds = np.zeros((n_boot, len(day_range)))
        for i in range(n_boot):
            idx = np.random.choice(len(y), len(y), replace=True)
            boot_model = sm.OLS(y[idx], X_design[idx]).fit()
            boot_preds[i] = boot_model.predict(X_pred)

        ci_lower = np.percentile(boot_preds, 2.5, axis=0)
        ci_upper = np.percentile(boot_preds, 97.5, axis=0)

        # Get statistics
        # F-test for overall significance of Fourier terms
        p_value = model.f_pvalue
        edf = n_harmonics * 2  # Each harmonic has sin and cos
        deviance_explained = model.rsquared

    else:
        # Use pygam splines (non-cyclic)
        try:
            from pygam import LinearGAM, s
        except ImportError:
            raise ImportError("pygam is required for non-cyclic GAM")

        X = X_days.reshape(-1, 1)
        gam = LinearGAM(s(0, n_splines=n_splines, spline_order=3))
        gam.fit(X, y)

        day_range = np.linspace(df[day_col].min(), df[day_col].max(), 100)
        predictions = gam.predict(day_range.reshape(-1, 1))
        confidence_intervals = gam.confidence_intervals(day_range.reshape(-1, 1))
        ci_lower = confidence_intervals[:, 0]
        ci_upper = confidence_intervals[:, 1]

        p_values = gam.statistics_["p_values"]
        p_value = p_values[0] if hasattr(p_values, '__len__') else p_values
        edf = gam.statistics_["edof"]
        deviance_explained = gam.statistics_["pseudo_r2"]["explained_deviance"]
        model = gam

    pred_df = pd.DataFrame({
        "cycle_day": day_range,
        "predicted": predictions,
        "ci_lower": ci_lower,
        "ci_upper": ci_upper
    })

    summary = f"""
GAM Summary
===========
Effective degrees of freedom: {edf:.2f}
P-value for cycle day effect: {p_value:.4e}
Deviance explained: {deviance_explained * 100:.1f}%
N observations: {len(df)}
"""

    return GAMResult(
        model=model,
        predictions=pred_df,
        summary=summary,
        p_value=p_value,
        edf=edf,
        deviance_explained=deviance_explained
    )


def _create_fourier_basis(days: np.ndarray, n_harmonics: int, cycle_length: int) -> np.ndarray:
    """Create Fourier basis matrix for cyclic regression."""
    n = len(days)
    # Convert days to radians (full cycle = 2*pi)
    radians = 2 * np.pi * days / cycle_length

    # Build design matrix: intercept + sin/cos pairs for each harmonic
    X = np.ones((n, 1 + 2 * n_harmonics))
    for h in range(1, n_harmonics + 1):
        X[:, 2*h - 1] = np.sin(h * radians)
        X[:, 2*h] = np.cos(h * radians)

    return X


def analyze_confounders(
    df: pd.DataFrame,
    confounder_data: pd.DataFrame,
    outcome_col: str = "a",
    day_col: str = "cycle_day",
    id_col: str = "user_id",
    confounder_cols: Optional[List[str]] = None
) -> List[ConfounderResult]:
    """
    Analyze effect of confounders on the cycle-outcome relationship.

    Parameters
    ----------
    df : pd.DataFrame
        Main dataset with normalized outcome
    confounder_data : pd.DataFrame
        DataFrame with confounders (must have id column matching df)
    outcome_col : str
        Name of outcome column
    day_col : str
        Name of cycle day column
    id_col : str
        Column name for user ID
    confounder_cols : List[str], optional
        List of confounder column names. If None, uses all non-ID columns.

    Returns
    -------
    List[ConfounderResult]
        Results for each confounder
    """
    import statsmodels.api as sm

    # Merge confounder data
    merged = df.merge(confounder_data, on=id_col, how="left")

    if confounder_cols is None:
        confounder_cols = [c for c in confounder_data.columns if c != id_col]

    results = []

    for conf in confounder_cols:
        if conf not in merged.columns:
            continue

        # Drop rows with missing or infinite confounder values
        subset = merged.dropna(subset=[conf, outcome_col, day_col])
        # Also remove infinite values
        subset = subset[np.isfinite(subset[conf])]

        if len(subset) < 10:
            warnings.warn(f"Skipping {conf}: only {len(subset)} valid observations")
            continue

        # Fit model with Fourier basis for cycle day + linear confounder
        X_days = subset[day_col].values
        X_conf = subset[conf].values
        y = subset[outcome_col].values

        try:
            # Create Fourier basis for cycle day (2 harmonics)
            X_fourier = _create_fourier_basis(X_days, n_harmonics=2, cycle_length=28)
            # Add confounder as additional column
            X_full = np.column_stack([X_fourier, X_conf])

            model = sm.OLS(y, X_full).fit()

            # Confounder coefficient is the last one
            coef = model.params[-1]
            se = model.bse[-1]
            p_val = model.pvalues[-1]
            conf_int = model.conf_int()
            ci_lower = conf_int[-1, 0]
            ci_upper = conf_int[-1, 1]

            results.append(ConfounderResult(
                confounder=conf,
                coefficient=coef,
                std_error=se,
                p_value=p_val,
                ci_lower=ci_lower,
                ci_upper=ci_upper
            ))
        except Exception as e:
            warnings.warn(f"Could not analyze confounder {conf}: {e}")

    return results


def find_turning_points(
    gam_result: GAMResult,
    day_col: str = "cycle_day",
    find_peaks_troughs: bool = True,
    round_to_whole_days: bool = True,
    day_range: tuple = (-14, 13)
) -> List[float]:
    """
    Find turning points (peaks/troughs) of the GAM curve.

    Parameters
    ----------
    gam_result : GAMResult
        Fitted GAM result
    day_col : str
        Name of cycle day column in predictions
    find_peaks_troughs : bool
        If True, find turning points (peaks/troughs where curve changes direction).
        If False, find points where curvature changes sign.
    round_to_whole_days : bool
        If True, round results to nearest whole day.
    day_range : tuple
        Only return turning points within this range (default -14 to 13).

    Returns
    -------
    List[float]
        Cycle days where turning points occur
    """
    pred_df = gam_result.predictions

    # Get predictions
    days = pred_df[day_col].values
    y = pred_df["predicted"].values

    # Calculate first derivative
    dy = np.gradient(y, days)

    if find_peaks_troughs:
        # Find turning points: where first derivative changes sign (peaks/troughs)
        sign_changes = np.where(np.diff(np.sign(dy)))[0]
        derivative = dy
    else:
        # Find points where second derivative changes sign
        d2y = np.gradient(dy, days)
        sign_changes = np.where(np.diff(np.sign(d2y)))[0]
        derivative = d2y

    result_days = []
    for idx in sign_changes:
        # Interpolate to find crossing point
        if idx + 1 < len(days):
            x1, x2 = days[idx], days[idx + 1]
            y1, y2 = derivative[idx], derivative[idx + 1]
            if y2 != y1:
                crossing_day = x1 - y1 * (x2 - x1) / (y2 - y1)
                # Skip if result is NaN or infinite
                if np.isnan(crossing_day) or np.isinf(crossing_day):
                    continue
                if round_to_whole_days:
                    crossing_day = int(round(crossing_day))
                # Only include if within specified range
                if day_range[0] <= crossing_day <= day_range[1]:
                    result_days.append(crossing_day)

    # Remove duplicates if rounding
    if round_to_whole_days:
        result_days = sorted(list(set(result_days)))
    else:
        result_days = sorted(result_days)

    return result_days


def fit_phase_models(
    df: pd.DataFrame,
    turning_points: List[float],
    gam_result: Optional[GAMResult] = None,
    outcome_col: str = "a",
    day_col: str = "cycle_day"
) -> List[PhaseModel]:
    """
    Fit linear models for each phase defined by turning points.

    For a cyclical model with N turning points, fits N linear models.
    If gam_result is provided, slopes are calculated from GAM predictions
    at the turning points.

    Parameters
    ----------
    df : pd.DataFrame
        Dataset with normalized outcome
    turning_points : List[float]
        Turning points from find_turning_points()
    gam_result : GAMResult, optional
        If provided, use GAM predictions at turning points to define slopes
    outcome_col : str
        Name of outcome column
    day_col : str
        Name of cycle day column

    Returns
    -------
    List[PhaseModel]
        Linear model results for each phase
    """
    try:
        import statsmodels.api as sm
    except ImportError:
        raise ImportError("statsmodels is required")

    if len(turning_points) < 2:
        warnings.warn("Need at least 2 turning points for phase models")
        return []

    # Sort turning points
    turns = sorted(turning_points)

    # Get GAM predictions at turning points if provided
    gam_values = {}
    gam_ci_lower = {}
    gam_ci_upper = {}
    if gam_result is not None:
        pred_df = gam_result.predictions
        for ip in turns:
            closest_idx = (pred_df["cycle_day"] - ip).abs().idxmin()
            gam_values[ip] = pred_df.loc[closest_idx, "predicted"]
            gam_ci_lower[ip] = pred_df.loc[closest_idx, "ci_lower"]
            gam_ci_upper[ip] = pred_df.loc[closest_idx, "ci_upper"]

    # Get day range
    min_day = df[day_col].min()
    max_day = df[day_col].max()

    results = []

    # For cyclical model, create phases that wrap around
    n_phases = len(turns)

    for i in range(n_phases):
        start = turns[i]
        end = turns[(i + 1) % n_phases]

        # Handle wraparound for cyclical model
        if end <= start:
            # Phase spans the boundary
            mask = (df[day_col] >= start) | (df[day_col] < end)
            # Calculate effective day span for wraparound
            day_span = (max_day - start) + (end - min_day)
        else:
            mask = (df[day_col] >= start) & (df[day_col] < end)
            day_span = end - start

        phase_data = df[mask].copy()

        if len(phase_data) < 3:
            continue

        if gam_result is not None and start in gam_values and end in gam_values:
            # Calculate slope from GAM predictions at turning points
            y_start = gam_values[start]
            y_end = gam_values[end]

            if day_span != 0:
                slope = (y_end - y_start) / day_span
            else:
                slope = 0.0

            intercept = y_start - slope * start

            # Estimate SE from GAM confidence intervals using delta method
            # SE of slope ≈ sqrt(SE_y1^2 + SE_y2^2) / day_span
            se_start = (gam_values[start] - gam_ci_lower[start]) / 1.96
            se_end = (gam_values[end] - gam_ci_lower[end]) / 1.96
            slope_se = np.sqrt(se_start**2 + se_end**2) / day_span if day_span != 0 else 0

            # Calculate t-statistic and p-value
            if slope_se > 0:
                t_stat = slope / slope_se
                # Two-tailed p-value with large df approximation
                from scipy import stats
                p_value = 2 * (1 - stats.t.cdf(abs(t_stat), df=len(phase_data) - 2))
            else:
                p_value = 1.0

            # R-squared: fit line through data and calculate
            X = phase_data[day_col].values
            if end <= start:
                X = np.where(X < start, X + (max_day - min_day + 1), X)
            y_pred = intercept + slope * X
            y_actual = phase_data[outcome_col].values
            ss_res = np.sum((y_actual - y_pred) ** 2)
            ss_tot = np.sum((y_actual - np.mean(y_actual)) ** 2)
            r_squared = 1 - (ss_res / ss_tot) if ss_tot > 0 else 0

            results.append(PhaseModel(
                phase=i + 1,
                start_day=start,
                end_day=end,
                slope=slope,
                intercept=intercept,
                slope_se=slope_se,
                p_value=p_value,
                r_squared=r_squared
            ))
        else:
            # Fall back to OLS on raw data
            X = phase_data[day_col].values
            if end <= start:
                X = np.where(X < start, X + (max_day - min_day + 1), X)

            X = sm.add_constant(X)
            y = phase_data[outcome_col].values

            try:
                model = sm.OLS(y, X).fit()

                results.append(PhaseModel(
                    phase=i + 1,
                    start_day=start,
                    end_day=end,
                    slope=model.params[1],
                    intercept=model.params[0],
                    slope_se=model.bse[1],
                    p_value=model.pvalues[1],
                    r_squared=model.rsquared
                ))
            except Exception as e:
                warnings.warn(f"Could not fit phase {i + 1} model: {e}")

    return results
