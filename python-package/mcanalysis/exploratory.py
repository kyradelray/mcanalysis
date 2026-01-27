"""
Exploratory data analysis functions for menstrual cycle data.
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from typing import Optional, List, Tuple


def plot_outcome_distribution(
    outcome_data: pd.DataFrame,
    outcome_col: str = "outcome",
    title: str = "Outcome Distribution",
    figsize: Tuple[int, int] = (10, 4),
    color: str = "steelblue",
    bins: int = 50,
    save_path: Optional[str] = None
) -> plt.Figure:
    """
    Plot the distribution of the outcome variable.

    Parameters
    ----------
    outcome_data : pd.DataFrame
        DataFrame containing outcome values
    outcome_col : str
        Name of outcome column
    title : str
        Plot title
    figsize : tuple
        Figure size (width, height)
    color : str
        Histogram color
    bins : int
        Number of histogram bins
    save_path : str, optional
        Path to save figure

    Returns
    -------
    matplotlib.figure.Figure
    """
    fig, axes = plt.subplots(1, 2, figsize=figsize)

    values = outcome_data[outcome_col].dropna()

    # Histogram
    axes[0].hist(values, bins=bins, color=color, edgecolor='white', alpha=0.7)
    axes[0].axvline(values.mean(), color='red', linestyle='--', label=f'Mean: {values.mean():.2f}')
    axes[0].axvline(values.median(), color='orange', linestyle='--', label=f'Median: {values.median():.2f}')
    axes[0].set_xlabel(outcome_col)
    axes[0].set_ylabel('Frequency')
    axes[0].set_title('Distribution')
    axes[0].legend()

    # Box plot
    axes[1].boxplot(values, orientation='vertical')
    axes[1].set_ylabel(outcome_col)
    axes[1].set_title('Box Plot')

    # Add stats text
    stats_text = f"N: {len(values):,}\nMean: {values.mean():.2f}\nSD: {values.std():.2f}\nMin: {values.min():.2f}\nMax: {values.max():.2f}"
    axes[1].text(1.3, values.median(), stats_text, fontsize=9, verticalalignment='center',
                 bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5))

    fig.suptitle(title, fontsize=14, fontweight='bold')
    plt.tight_layout()

    if save_path:
        plt.savefig(save_path, dpi=150, bbox_inches='tight')
        print(f"Saved to {save_path}")

    return fig


def plot_cycle_lengths(
    period_dates: pd.DataFrame,
    id_col: str = "user_id",
    date_col: str = "period_date",
    title: str = "Cycle Length Distribution",
    figsize: Tuple[int, int] = (10, 4),
    color: str = "coral",
    valid_range: Tuple[int, int] = (21, 35),
    save_path: Optional[str] = None
) -> Tuple[plt.Figure, pd.DataFrame]:
    """
    Plot the distribution of menstrual cycle lengths.

    Parameters
    ----------
    period_dates : pd.DataFrame
        DataFrame with period start dates
    id_col : str
        Name of user ID column
    date_col : str
        Name of date column
    title : str
        Plot title
    figsize : tuple
        Figure size
    color : str
        Histogram color
    valid_range : tuple
        Range of valid cycle lengths to highlight (min, max)
    save_path : str, optional
        Path to save figure

    Returns
    -------
    tuple
        (matplotlib.figure.Figure, pd.DataFrame with cycle lengths)
    """
    # Calculate cycle lengths
    df = period_dates.copy()
    df[date_col] = pd.to_datetime(df[date_col])
    df = df.sort_values([id_col, date_col])

    # Calculate days between periods for each user
    df['cycle_length'] = df.groupby(id_col)[date_col].diff().dt.days
    cycle_lengths = df['cycle_length'].dropna()

    fig, axes = plt.subplots(1, 2, figsize=figsize)

    # Histogram with valid range highlighted
    n, bins_edges, patches = axes[0].hist(cycle_lengths, bins=range(10, 60),
                                           color='lightgray', edgecolor='white')

    # Color valid range
    for i, (left, right) in enumerate(zip(bins_edges[:-1], bins_edges[1:])):
        if valid_range[0] <= left < valid_range[1]:
            patches[i].set_facecolor(color)

    axes[0].axvline(valid_range[0], color='green', linestyle='--', alpha=0.7, label=f'Valid range: {valid_range[0]}-{valid_range[1]} days')
    axes[0].axvline(valid_range[1], color='green', linestyle='--', alpha=0.7)
    axes[0].axvline(cycle_lengths.mean(), color='red', linestyle='-', label=f'Mean: {cycle_lengths.mean():.1f} days')
    axes[0].set_xlabel('Cycle Length (days)')
    axes[0].set_ylabel('Frequency')
    axes[0].set_title('All Cycles')
    axes[0].legend(fontsize=8)

    # Stats for valid vs invalid
    valid_cycles = cycle_lengths[(cycle_lengths >= valid_range[0]) & (cycle_lengths <= valid_range[1])]
    invalid_cycles = cycle_lengths[(cycle_lengths < valid_range[0]) | (cycle_lengths > valid_range[1])]

    # Pie chart of valid vs invalid
    sizes = [len(valid_cycles), len(invalid_cycles)]
    labels = [f'Valid ({valid_range[0]}-{valid_range[1]}d)\n{len(valid_cycles):,} ({100*len(valid_cycles)/len(cycle_lengths):.1f}%)',
              f'Outside range\n{len(invalid_cycles):,} ({100*len(invalid_cycles)/len(cycle_lengths):.1f}%)']
    colors_pie = [color, 'lightgray']
    axes[1].pie(sizes, labels=labels, colors=colors_pie, autopct='', startangle=90)
    axes[1].set_title('Valid vs Invalid Cycles')

    fig.suptitle(title, fontsize=14, fontweight='bold')
    plt.tight_layout()

    if save_path:
        plt.savefig(save_path, dpi=150, bbox_inches='tight')
        print(f"Saved to {save_path}")

    # Return summary dataframe
    summary = pd.DataFrame({
        'metric': ['Total cycles', 'Valid cycles', 'Invalid cycles',
                   'Mean length', 'Median length', 'SD', 'Min', 'Max'],
        'value': [len(cycle_lengths), len(valid_cycles), len(invalid_cycles),
                  cycle_lengths.mean(), cycle_lengths.median(), cycle_lengths.std(),
                  cycle_lengths.min(), cycle_lengths.max()]
    })

    return fig, summary


def plot_confounder_distributions(
    confounder_data: pd.DataFrame,
    id_col: str = "user_id",
    confounder_cols: Optional[List[str]] = None,
    title: str = "Confounder Distributions",
    figsize: Optional[Tuple[int, int]] = None,
    color: str = "mediumpurple",
    save_path: Optional[str] = None
) -> plt.Figure:
    """
    Plot distributions of confounder variables.

    Parameters
    ----------
    confounder_data : pd.DataFrame
        DataFrame with confounder variables
    id_col : str
        Name of user ID column (excluded from plots)
    confounder_cols : list, optional
        List of confounder columns to plot. If None, plots all numeric columns.
    title : str
        Plot title
    figsize : tuple, optional
        Figure size. If None, auto-calculated based on number of confounders.
    color : str
        Histogram color
    save_path : str, optional
        Path to save figure

    Returns
    -------
    matplotlib.figure.Figure
    """
    if confounder_cols is None:
        confounder_cols = [c for c in confounder_data.columns
                          if c != id_col and confounder_data[c].dtype in ['int64', 'float64']]

    n_conf = len(confounder_cols)
    if n_conf == 0:
        print("No numeric confounder columns found")
        return None

    # Calculate grid size
    n_cols = min(3, n_conf)
    n_rows = (n_conf + n_cols - 1) // n_cols

    if figsize is None:
        figsize = (4 * n_cols, 3 * n_rows)

    fig, axes = plt.subplots(n_rows, n_cols, figsize=figsize)
    if n_conf == 1:
        axes = np.array([axes])
    axes = axes.flatten()

    for i, col in enumerate(confounder_cols):
        values = confounder_data[col].dropna()

        axes[i].hist(values, bins=30, color=color, edgecolor='white', alpha=0.7)
        axes[i].axvline(values.mean(), color='red', linestyle='--', linewidth=1)
        axes[i].set_xlabel(col)
        axes[i].set_ylabel('Frequency')

        # Add stats
        stats_text = f"N={len(values):,}\nμ={values.mean():.1f}\nσ={values.std():.1f}"
        axes[i].text(0.95, 0.95, stats_text, transform=axes[i].transAxes,
                     fontsize=8, verticalalignment='top', horizontalalignment='right',
                     bbox=dict(boxstyle='round', facecolor='white', alpha=0.8))

    # Hide unused axes
    for i in range(n_conf, len(axes)):
        axes[i].set_visible(False)

    fig.suptitle(title, fontsize=14, fontweight='bold')
    plt.tight_layout()

    if save_path:
        plt.savefig(save_path, dpi=150, bbox_inches='tight')
        print(f"Saved to {save_path}")

    return fig


def plot_observations_per_user(
    outcome_data: pd.DataFrame,
    id_col: str = "user_id",
    title: str = "Observations per User",
    figsize: Tuple[int, int] = (10, 4),
    color: str = "teal",
    save_path: Optional[str] = None
) -> Tuple[plt.Figure, pd.DataFrame]:
    """
    Plot distribution of number of observations per user.

    Parameters
    ----------
    outcome_data : pd.DataFrame
        DataFrame with outcome data
    id_col : str
        Name of user ID column
    title : str
        Plot title
    figsize : tuple
        Figure size
    color : str
        Histogram color
    save_path : str, optional
        Path to save figure

    Returns
    -------
    tuple
        (matplotlib.figure.Figure, pd.DataFrame with summary)
    """
    obs_per_user = outcome_data.groupby(id_col).size()

    fig, axes = plt.subplots(1, 2, figsize=figsize)

    # Histogram
    axes[0].hist(obs_per_user, bins=50, color=color, edgecolor='white', alpha=0.7)
    axes[0].axvline(obs_per_user.mean(), color='red', linestyle='--', label=f'Mean: {obs_per_user.mean():.1f}')
    axes[0].axvline(obs_per_user.median(), color='orange', linestyle='--', label=f'Median: {obs_per_user.median():.1f}')
    axes[0].set_xlabel('Number of observations')
    axes[0].set_ylabel('Number of users')
    axes[0].set_title('Distribution')
    axes[0].legend()

    # Cumulative
    sorted_obs = np.sort(obs_per_user)
    cumulative = np.arange(1, len(sorted_obs) + 1) / len(sorted_obs)
    axes[1].plot(sorted_obs, cumulative, color=color, linewidth=2)
    axes[1].axhline(0.5, color='gray', linestyle='--', alpha=0.5)
    axes[1].axvline(obs_per_user.median(), color='orange', linestyle='--', alpha=0.5)
    axes[1].set_xlabel('Number of observations')
    axes[1].set_ylabel('Cumulative proportion of users')
    axes[1].set_title('Cumulative Distribution')
    axes[1].grid(True, alpha=0.3)

    fig.suptitle(title, fontsize=14, fontweight='bold')
    plt.tight_layout()

    if save_path:
        plt.savefig(save_path, dpi=150, bbox_inches='tight')
        print(f"Saved to {save_path}")

    summary = pd.DataFrame({
        'metric': ['Total users', 'Total observations', 'Mean obs/user',
                   'Median obs/user', 'Min obs/user', 'Max obs/user'],
        'value': [len(obs_per_user), obs_per_user.sum(), obs_per_user.mean(),
                  obs_per_user.median(), obs_per_user.min(), obs_per_user.max()]
    })

    return fig, summary


def data_summary(
    period_dates: pd.DataFrame,
    outcome_data: pd.DataFrame,
    confounder_data: Optional[pd.DataFrame] = None,
    id_col: str = "user_id",
    date_col: str = "period_date",
    outcome_col: str = "outcome",
    outcome_date_col: str = "date"
) -> pd.DataFrame:
    """
    Generate a summary table of the input data.

    Parameters
    ----------
    period_dates : pd.DataFrame
        Period dates data
    outcome_data : pd.DataFrame
        Outcome data
    confounder_data : pd.DataFrame, optional
        Confounder data
    id_col : str
        User ID column name
    date_col : str
        Period date column name
    outcome_col : str
        Outcome column name
    outcome_date_col : str
        Outcome date column name

    Returns
    -------
    pd.DataFrame
        Summary statistics table
    """
    # Period data stats
    period_users = period_dates[id_col].nunique()
    total_periods = len(period_dates)

    # Calculate cycle lengths
    df = period_dates.copy()
    df[date_col] = pd.to_datetime(df[date_col])
    df = df.sort_values([id_col, date_col])
    df['cycle_length'] = df.groupby(id_col)[date_col].diff().dt.days
    cycle_lengths = df['cycle_length'].dropna()

    # Outcome data stats
    outcome_users = outcome_data[id_col].nunique()
    total_outcomes = len(outcome_data)
    outcome_values = outcome_data[outcome_col].dropna()

    # Users in common
    common_users = set(period_dates[id_col]) & set(outcome_data[id_col])

    rows = [
        ('PERIOD DATA', '', ''),
        ('  Users with period data', period_users, ''),
        ('  Total period records', total_periods, ''),
        ('  Periods per user', f"{total_periods/period_users:.1f}", 'mean'),
        ('  Cycle length (days)', f"{cycle_lengths.mean():.1f} ± {cycle_lengths.std():.1f}", 'mean ± SD'),
        ('  Cycle length range', f"{cycle_lengths.min():.0f} - {cycle_lengths.max():.0f}", 'min - max'),
        ('', '', ''),
        ('OUTCOME DATA', '', ''),
        ('  Users with outcome data', outcome_users, ''),
        ('  Total outcome records', total_outcomes, ''),
        ('  Observations per user', f"{total_outcomes/outcome_users:.1f}", 'mean'),
        ('  Outcome value', f"{outcome_values.mean():.2f} ± {outcome_values.std():.2f}", 'mean ± SD'),
        ('  Outcome range', f"{outcome_values.min():.2f} - {outcome_values.max():.2f}", 'min - max'),
        ('', '', ''),
        ('OVERLAP', '', ''),
        ('  Users in both datasets', len(common_users), ''),
    ]

    if confounder_data is not None:
        conf_users = confounder_data[id_col].nunique()
        conf_cols = [c for c in confounder_data.columns if c != id_col]
        rows.append(('', '', ''))
        rows.append(('CONFOUNDER DATA', '', ''))
        rows.append(('  Users with confounders', conf_users, ''))
        rows.append(('  Confounder variables', len(conf_cols), ''))
        for col in conf_cols:
            values = confounder_data[col].dropna()
            if len(values) > 0:
                rows.append((f'    {col}', f"{values.mean():.2f} ± {values.std():.2f}", 'mean ± SD'))

    summary_df = pd.DataFrame(rows, columns=['Metric', 'Value', 'Note'])
    return summary_df


def run_exploratory_analysis(
    period_dates: pd.DataFrame,
    outcome_data: pd.DataFrame,
    confounder_data: Optional[pd.DataFrame] = None,
    id_col: str = "user_id",
    date_col: str = "period_date",
    outcome_col: str = "outcome",
    outcome_date_col: str = "date",
    confounder_cols: Optional[List[str]] = None,
    save_dir: Optional[str] = None,
    show_plots: bool = True
) -> dict:
    """
    Run complete exploratory data analysis.

    Parameters
    ----------
    period_dates : pd.DataFrame
        Period dates data
    outcome_data : pd.DataFrame
        Outcome data
    confounder_data : pd.DataFrame, optional
        Confounder data
    id_col : str
        User ID column name
    date_col : str
        Period date column name
    outcome_col : str
        Outcome column name
    outcome_date_col : str
        Outcome date column name
    confounder_cols : list, optional
        Specific confounder columns to analyze
    save_dir : str, optional
        Directory to save figures
    show_plots : bool
        Whether to display plots

    Returns
    -------
    dict
        Dictionary containing all figures and summary tables
    """
    import os

    results = {}

    # Data summary
    print("=" * 60)
    print("EXPLORATORY DATA ANALYSIS")
    print("=" * 60)

    summary = data_summary(
        period_dates, outcome_data, confounder_data,
        id_col, date_col, outcome_col, outcome_date_col
    )
    print("\nDATA SUMMARY:")
    print("-" * 40)
    for _, row in summary.iterrows():
        if row['Metric']:
            print(f"{row['Metric']}: {row['Value']}")
    results['summary'] = summary

    # Outcome distribution
    print("\n" + "-" * 40)
    print("Generating outcome distribution plot...")
    save_path = os.path.join(save_dir, 'eda_outcome_distribution.png') if save_dir else None
    fig_outcome = plot_outcome_distribution(
        outcome_data, outcome_col=outcome_col,
        title=f'Distribution of {outcome_col}',
        save_path=save_path
    )
    results['fig_outcome'] = fig_outcome

    # Cycle lengths
    print("Generating cycle length distribution plot...")
    save_path = os.path.join(save_dir, 'eda_cycle_lengths.png') if save_dir else None
    fig_cycles, cycle_summary = plot_cycle_lengths(
        period_dates, id_col=id_col, date_col=date_col,
        save_path=save_path
    )
    results['fig_cycles'] = fig_cycles
    results['cycle_summary'] = cycle_summary

    # Observations per user
    print("Generating observations per user plot...")
    save_path = os.path.join(save_dir, 'eda_obs_per_user.png') if save_dir else None
    fig_obs, obs_summary = plot_observations_per_user(
        outcome_data, id_col=id_col,
        save_path=save_path
    )
    results['fig_obs_per_user'] = fig_obs
    results['obs_summary'] = obs_summary

    # Confounder distributions
    if confounder_data is not None:
        print("Generating confounder distribution plots...")
        save_path = os.path.join(save_dir, 'eda_confounders.png') if save_dir else None
        fig_conf = plot_confounder_distributions(
            confounder_data, id_col=id_col,
            confounder_cols=confounder_cols,
            save_path=save_path
        )
        results['fig_confounders'] = fig_conf

    print("\n" + "=" * 60)
    print("Exploratory analysis complete!")
    if save_dir:
        print(f"Figures saved to: {save_dir}")
    print("=" * 60)

    if show_plots:
        plt.show()

    return results
