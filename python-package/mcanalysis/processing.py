"""
Data processing functions for menstrual cycle analysis.
"""

import pandas as pd
import numpy as np
from typing import Tuple


def process_periods(
    period_dates: pd.DataFrame,
    outcome_data: pd.DataFrame,
    id_col: str = "id",
    date_col: str = "period_date",
    outcome_col: str = "outcome",
    outcome_date_col: str = "date"
) -> pd.DataFrame:
    """
    Process period dates and outcome data to create cycle-day labeled dataset.

    Parameters
    ----------
    period_dates : pd.DataFrame
        DataFrame with user ID and period start dates
    outcome_data : pd.DataFrame
        DataFrame with user ID, date, and outcome values
    id_col : str
        Column name for user ID
    date_col : str
        Column name for period dates
    outcome_col : str
        Column name for outcome variable
    outcome_date_col : str
        Column name for outcome dates

    Returns
    -------
    pd.DataFrame
        Dataset with cycle day labels (-14 to 20, plus day 21 dummy)
    """
    # Ensure dates are datetime
    period_dates = period_dates.copy()
    outcome_data = outcome_data.copy()
    period_dates[date_col] = pd.to_datetime(period_dates[date_col])
    outcome_data[outcome_date_col] = pd.to_datetime(outcome_data[outcome_date_col])

    # Sort period dates
    period_dates = period_dates.sort_values([id_col, date_col])

    results = []

    for user_id in period_dates[id_col].unique():
        user_periods = period_dates[period_dates[id_col] == user_id][date_col].values
        user_outcomes = outcome_data[outcome_data[id_col] == user_id].copy()

        if len(user_periods) == 0 or len(user_outcomes) == 0:
            continue

        for i, period_start in enumerate(user_periods):
            period_start = pd.Timestamp(period_start)

            # Determine cycle end (next period or open-ended)
            if i < len(user_periods) - 1:
                next_period = pd.Timestamp(user_periods[i + 1])
                cycle_length = (next_period - period_start).days
                is_open = False
            else:
                # Open-ended cycle - use 35 days as max
                next_period = period_start + pd.Timedelta(days=35)
                cycle_length = None
                is_open = True

            # Get outcomes within this cycle window
            # Day 0 is period start, we want days -14 to 20
            cycle_start = period_start - pd.Timedelta(days=14)
            cycle_end = period_start + pd.Timedelta(days=20)

            mask = (user_outcomes[outcome_date_col] >= cycle_start) & \
                   (user_outcomes[outcome_date_col] <= cycle_end)

            cycle_outcomes = user_outcomes[mask].copy()

            if len(cycle_outcomes) == 0:
                continue

            # Calculate cycle day (day 0 = period start)
            cycle_outcomes["cycle_day"] = (
                cycle_outcomes[outcome_date_col] - period_start
            ).dt.days

            cycle_outcomes["cycle_id"] = i
            cycle_outcomes["cycle_length"] = cycle_length
            cycle_outcomes["is_open_cycle"] = is_open
            cycle_outcomes["user_id"] = user_id

            results.append(cycle_outcomes)

    if not results:
        return pd.DataFrame()

    df = pd.concat(results, ignore_index=True)

    # Rename columns for consistency
    df = df.rename(columns={outcome_col: "outcome", outcome_date_col: "date"})

    return df


def filter_cycles(
    df: pd.DataFrame,
    min_length: int = 21,
    max_length: int = 35
) -> pd.DataFrame:
    """
    Filter to cycles with length between min_length and max_length days.
    Keep open-ended cycles.

    Parameters
    ----------
    df : pd.DataFrame
        Processed cycle data from process_periods()
    min_length : int
        Minimum cycle length (default 21)
    max_length : int
        Maximum cycle length (default 35)

    Returns
    -------
    pd.DataFrame
        Filtered dataset
    """
    mask = (
        df["is_open_cycle"] |  # Keep open cycles
        ((df["cycle_length"] >= min_length) & (df["cycle_length"] <= max_length))
    )

    return df[mask].copy()


def filter_users(
    df: pd.DataFrame,
    min_negative_obs: int = 5,
    min_positive_obs: int = 5
) -> pd.DataFrame:
    """
    Filter to users with sufficient observations in both negative and positive cycle days.

    Parameters
    ----------
    df : pd.DataFrame
        Processed cycle data
    min_negative_obs : int
        Minimum observations with cycle_day < 0 (default 5)
    min_positive_obs : int
        Minimum observations with cycle_day >= 0 (default 5)

    Returns
    -------
    pd.DataFrame
        Filtered dataset with only qualifying users
    """
    # Count negative and positive day observations per user
    user_stats = df.groupby("user_id").apply(
        lambda x: pd.Series({
            "neg_obs": (x["cycle_day"] < 0).sum(),
            "pos_obs": (x["cycle_day"] >= 0).sum()
        })
    ).reset_index()

    # Filter to qualifying users
    qualifying_users = user_stats[
        (user_stats["neg_obs"] >= min_negative_obs) &
        (user_stats["pos_obs"] >= min_positive_obs)
    ]["user_id"]

    return df[df["user_id"].isin(qualifying_users)].copy()


def add_dummy_day(df: pd.DataFrame) -> pd.DataFrame:
    """
    Add day 21 as a dummy variable representing day -14 for cyclical continuity.

    Parameters
    ----------
    df : pd.DataFrame
        Processed cycle data

    Returns
    -------
    pd.DataFrame
        Dataset with day 21 dummy observations added
    """
    # Get observations from day -14
    day_neg14 = df[df["cycle_day"] == -14].copy()

    if len(day_neg14) == 0:
        return df

    # Create dummy day 21 entries
    day_neg14["cycle_day"] = 21
    day_neg14["is_dummy"] = True

    # Mark original data
    df["is_dummy"] = False

    return pd.concat([df, day_neg14], ignore_index=True)
