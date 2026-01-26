"""
Visualization functions for menstrual cycle analysis.
"""

import pandas as pd
import numpy as np
from typing import List, Optional, Tuple
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches

from .models import GAMResult, PhaseModel


def plot_cycle_effect(
    gam_result: GAMResult,
    inflection_points: Optional[List[float]] = None,
    phase_models: Optional[List[PhaseModel]] = None,
    raw_data: Optional[pd.DataFrame] = None,
    title: str = "Menstrual Cycle Effect on Outcome",
    xlabel: str = "Cycle Day (Day 0 = Period Start)",
    ylabel: str = "% Change from Individual Mean",
    show_significance: bool = True,
    show_phases: bool = True,
    show_linear_models: bool = True,
    show_inflection_points: bool = True,
    show_ci: bool = True,
    show_period_line: bool = True,
    show_raw_data: bool = False,
    day_range: Tuple[int, int] = (-14, 13),
    figsize: Tuple[int, int] = (12, 8),
    gam_color: str = "steelblue",
    gam_linewidth: float = 2.5,
    ci_alpha: float = 0.3,
    raw_data_alpha: float = 0.1,
    raw_data_size: float = 10,
    raw_data_color: str = "gray",
    linear_colors: Optional[List[str]] = None,
    save_path: Optional[str] = None
) -> plt.Figure:
    """
    Plot GAM results with confidence intervals and optional phase information.

    Parameters
    ----------
    gam_result : GAMResult
        Fitted GAM result
    inflection_points : List[float], optional
        Inflection points to mark on plot
    phase_models : List[PhaseModel], optional
        Phase model results to display
    raw_data : pd.DataFrame, optional
        Processed data with 'cycle_day' and 'a' columns for plotting raw observations
    title : str
        Plot title
    xlabel : str
        X-axis label
    ylabel : str
        Y-axis label
    show_significance : bool
        Whether to show significance annotation
    show_phases : bool
        Whether to shade phases differently
    show_linear_models : bool
        Whether to show linear phase model lines (default True)
    show_inflection_points : bool
        Whether to mark inflection points (default True)
    show_ci : bool
        Whether to show confidence interval (default True)
    show_period_line : bool
        Whether to show vertical line at day 0 (default True)
    show_raw_data : bool
        Whether to show raw data points (default False)
    day_range : Tuple[int, int]
        Range of days to display (default -14 to 13)
    figsize : Tuple[int, int]
        Figure size
    gam_color : str
        Color of GAM curve (default "steelblue")
    gam_linewidth : float
        Line width of GAM curve (default 2.5)
    ci_alpha : float
        Transparency of confidence interval (default 0.3)
    raw_data_alpha : float
        Transparency of raw data points (default 0.1)
    raw_data_size : float
        Size of raw data points (default 10)
    raw_data_color : str
        Color of raw data points (default "gray")
    linear_colors : List[str], optional
        Colors for linear phase models (default red and green)
    save_path : str, optional
        Path to save figure

    Returns
    -------
    plt.Figure
        Matplotlib figure object
    """
    if linear_colors is None:
        linear_colors = ["#E74C3C", "#27AE60"]
    pred_df = gam_result.predictions

    # Filter to day range
    mask = (pred_df["cycle_day"] >= day_range[0]) & (pred_df["cycle_day"] <= day_range[1])
    plot_data = pred_df[mask].copy()

    # Convert to percentage change from mean (subtract 100)
    plot_data["predicted_pct"] = plot_data["predicted"] - 100
    plot_data["ci_lower_pct"] = plot_data["ci_lower"] - 100
    plot_data["ci_upper_pct"] = plot_data["ci_upper"] - 100

    fig, ax = plt.subplots(figsize=figsize)

    # Plot daily means as line graph (behind GAM curve)
    if show_raw_data and raw_data is not None:
        # Filter to day range and exclude dummy observations
        raw_mask = (
            (raw_data["cycle_day"] >= day_range[0]) &
            (raw_data["cycle_day"] <= day_range[1]) &
            (~raw_data.get("is_dummy", pd.Series([False] * len(raw_data))).fillna(False))
        )
        raw_plot = raw_data[raw_mask].copy()
        # Convert to percentage change
        raw_plot["a_pct"] = raw_plot["a"] - 100
        # Calculate daily means
        daily_means = raw_plot.groupby("cycle_day")["a_pct"].mean().reset_index()
        daily_means = daily_means.sort_values("cycle_day")

        ax.plot(
            daily_means["cycle_day"],
            daily_means["a_pct"],
            alpha=raw_data_alpha,
            linewidth=raw_data_size / 5,
            color=raw_data_color,
            label="Daily Mean",
            zorder=1
        )

    # Plot confidence interval
    if show_ci:
        ax.fill_between(
            plot_data["cycle_day"],
            plot_data["ci_lower_pct"],
            plot_data["ci_upper_pct"],
            alpha=ci_alpha,
            color=gam_color,
            label="95% CI"
        )

    # Plot GAM curve
    ax.plot(
        plot_data["cycle_day"],
        plot_data["predicted_pct"],
        color=gam_color,
        linewidth=gam_linewidth,
        label="GAM Fit"
    )

    # Add reference line at 0 (mean)
    ax.axhline(y=0, color="gray", linestyle="--", alpha=0.5, label="Mean (0%)")

    # Add vertical line at day 0 (period start)
    if show_period_line:
        ax.axvline(x=0, color="red", linestyle=":", alpha=0.7, label="Period Start")

    # Mark inflection points
    if show_inflection_points and inflection_points:
        for i, ip in enumerate(inflection_points):
            if day_range[0] <= ip <= day_range[1]:
                # Get y value at inflection point (percentage change)
                closest_idx = (plot_data["cycle_day"] - ip).abs().idxmin()
                y_val = plot_data.loc[closest_idx, "predicted_pct"]

                ax.axvline(x=ip, color="orange", linestyle="--", alpha=0.6)
                ax.scatter([ip], [y_val], color="orange", s=100, zorder=5,
                          marker="o", edgecolor="black", linewidth=1)

                # Label
                ax.annotate(
                    f"IP {i+1}\n(Day {ip:.1f})",
                    xy=(ip, y_val),
                    xytext=(10, 10),
                    textcoords="offset points",
                    fontsize=9,
                    bbox=dict(boxstyle="round,pad=0.3", facecolor="white", alpha=0.8)
                )

    # Shade phases if provided
    if show_phases and phase_models and inflection_points:
        colors = plt.cm.Set3(np.linspace(0, 1, len(phase_models)))
        for i, (pm, color) in enumerate(zip(phase_models, colors)):
            if pm.end_day <= pm.start_day:
                # Phase wraps around - shade two regions
                # Region 1: from start_day to day_range[1]
                ax.axvspan(pm.start_day, day_range[1], alpha=0.1, color=color)
                # Region 2: from day_range[0] to end_day
                ax.axvspan(day_range[0], pm.end_day, alpha=0.1, color=color)
            else:
                # Normal phase within range
                start = max(pm.start_day, day_range[0])
                end = min(pm.end_day, day_range[1])
                ax.axvspan(start, end, alpha=0.1, color=color)

    # Add significance annotation
    if show_significance:
        p_val = gam_result.p_value
        sig_text = f"Cycle Effect p-value: {p_val:.2e}"
        if p_val < 0.001:
            sig_text += " ***"
        elif p_val < 0.01:
            sig_text += " **"
        elif p_val < 0.05:
            sig_text += " *"
        else:
            sig_text += " (ns)"

        ax.text(
            0.02, 0.98, sig_text,
            transform=ax.transAxes,
            fontsize=11,
            verticalalignment="top",
            bbox=dict(boxstyle="round,pad=0.5", facecolor="white", edgecolor="gray", alpha=0.9)
        )

        # Add deviance explained
        dev_text = f"Deviance Explained: {gam_result.deviance_explained * 100:.1f}%"
        ax.text(
            0.02, 0.90, dev_text,
            transform=ax.transAxes,
            fontsize=10,
            verticalalignment="top",
            bbox=dict(boxstyle="round,pad=0.5", facecolor="white", edgecolor="gray", alpha=0.9)
        )

    # Add phase model summary and linear model lines if provided
    if phase_models and inflection_points:
        # Get GAM predicted values at turning points (percentage change)
        turning_point_values = {}
        for ip in inflection_points:
            closest_idx = (plot_data["cycle_day"] - ip).abs().idxmin()
            turning_point_values[ip] = plot_data.loc[closest_idx, "predicted_pct"]

        # Plot linear model lines connecting turning points
        if show_linear_models:
            for i, pm in enumerate(phase_models):
                color = linear_colors[i % len(linear_colors)]

                start_y = turning_point_values.get(pm.start_day)
                end_y = turning_point_values.get(pm.end_day)

                if start_y is None or end_y is None:
                    continue

                # Handle cyclical wraparound
                if pm.end_day <= pm.start_day:
                    # Phase wraps around - plot two segments
                    # Segment 1: from start_day to day_range[1]
                    x1_start = pm.start_day
                    x1_end = day_range[1]
                    # Interpolate y values
                    total_span = (day_range[1] - pm.start_day) + (pm.end_day - day_range[0])
                    frac1 = (x1_end - x1_start) / total_span if total_span > 0 else 0
                    y1_end = start_y + frac1 * (end_y - start_y)
                    ax.plot([x1_start, x1_end], [start_y, y1_end], color=color,
                           linewidth=2, linestyle="--", label=f"Phase {pm.phase} Linear", alpha=0.8)

                    # Segment 2: from day_range[0] to end_day
                    x2_start = day_range[0]
                    x2_end = pm.end_day
                    y2_start = y1_end  # Continue from where segment 1 ended
                    ax.plot([x2_start, x2_end], [y2_start, end_y], color=color,
                           linewidth=2, linestyle="--", alpha=0.8)
                else:
                    # Normal phase within range - draw line from turning point to turning point
                    ax.plot([pm.start_day, pm.end_day], [start_y, end_y], color=color,
                           linewidth=2, linestyle="--", label=f"Phase {pm.phase} Linear", alpha=0.8)

        # Add phase model summary text
        phase_text = "Phase Slopes:\n"
        for pm in phase_models:
            sig = "***" if pm.p_value < 0.001 else "**" if pm.p_value < 0.01 else "*" if pm.p_value < 0.05 else ""
            phase_text += f"  Phase {pm.phase} (Day {pm.start_day:.0f} to {pm.end_day:.0f}): "
            phase_text += f"{pm.slope:.3f} ± {pm.slope_se:.3f} {sig}\n"

        ax.text(
            0.98, 0.02, phase_text,
            transform=ax.transAxes,
            fontsize=9,
            verticalalignment="bottom",
            horizontalalignment="right",
            family="monospace",
            bbox=dict(boxstyle="round,pad=0.5", facecolor="white", edgecolor="gray", alpha=0.9)
        )

    # Formatting
    ax.set_xlabel(xlabel, fontsize=12)
    ax.set_ylabel(ylabel, fontsize=12)
    ax.set_title(title, fontsize=14, fontweight="bold")

    ax.set_xlim(day_range[0] - 0.5, day_range[1] + 0.5)
    ax.set_xticks(range(day_range[0], day_range[1] + 1, 2))

    ax.legend(loc="upper right")
    ax.grid(True, alpha=0.3)

    plt.tight_layout()

    if save_path:
        plt.savefig(save_path, dpi=300, bbox_inches="tight")

    return fig


def plot_confounder_effects(
    confounder_results: List,
    title: str = "Confounder Effects on Cycle-Outcome Relationship",
    figsize: Tuple[int, int] = (10, 6),
    save_path: Optional[str] = None
) -> plt.Figure:
    """
    Plot forest plot of confounder effects.

    Parameters
    ----------
    confounder_results : List[ConfounderResult]
        Results from analyze_confounders()
    title : str
        Plot title
    figsize : Tuple[int, int]
        Figure size
    save_path : str, optional
        Path to save figure

    Returns
    -------
    plt.Figure
        Matplotlib figure object
    """
    if not confounder_results:
        fig, ax = plt.subplots(figsize=figsize)
        ax.text(0.5, 0.5, "No confounder results to display",
                ha="center", va="center", transform=ax.transAxes)
        return fig

    fig, ax = plt.subplots(figsize=figsize)

    confounders = [r.confounder for r in confounder_results]
    coefs = [r.coefficient for r in confounder_results]
    ci_lower = [r.ci_lower for r in confounder_results]
    ci_upper = [r.ci_upper for r in confounder_results]
    p_values = [r.p_value for r in confounder_results]

    y_pos = np.arange(len(confounders))

    # Colors based on significance
    colors = ["red" if p < 0.05 else "gray" for p in p_values]

    # Plot
    ax.errorbar(
        coefs, y_pos,
        xerr=[np.array(coefs) - np.array(ci_lower), np.array(ci_upper) - np.array(coefs)],
        fmt="o",
        capsize=5,
        capthick=2,
        color="steelblue",
        ecolor="steelblue",
        markersize=8
    )

    # Add significance markers
    for i, (coef, p) in enumerate(zip(coefs, p_values)):
        sig = "***" if p < 0.001 else "**" if p < 0.01 else "*" if p < 0.05 else ""
        ax.annotate(sig, (coef, i), xytext=(5, 0), textcoords="offset points",
                   fontsize=12, fontweight="bold", color="red")

    ax.axvline(x=0, color="gray", linestyle="--", alpha=0.5)
    ax.set_yticks(y_pos)
    ax.set_yticklabels(confounders)
    ax.set_xlabel("Coefficient (effect on normalized outcome)")
    ax.set_title(title, fontsize=14, fontweight="bold")
    ax.grid(True, alpha=0.3, axis="x")

    plt.tight_layout()

    if save_path:
        plt.savefig(save_path, dpi=300, bbox_inches="tight")

    return fig
