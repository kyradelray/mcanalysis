#' Process Period Dates
#'
#' Convert period dates and outcome data to cycle-day labeled dataset.
#'
#' @param period_dates Data frame with user ID and period start dates
#' @param outcome_data Data frame with user ID, date, and outcome values
#' @param id_col Column name for user ID (default "id")
#' @param date_col Column name for period dates (default "period_date")
#' @param outcome_col Column name for outcome variable (default "outcome")
#' @param outcome_date_col Column name for outcome dates (default "date")
#'
#' @return Data frame with cycle day labels (-14 to 20)
#' @export
#'
#' @examples
#' \dontrun{
#' df <- process_periods(period_dates, outcome_data)
#' }
process_periods <- function(period_dates,
                            outcome_data,
                            id_col = "id",
                            date_col = "period_date",
                            outcome_col = "outcome",
                            outcome_date_col = "date") {

  # Ensure dates are Date type
  period_dates[[date_col]] <- as.Date(period_dates[[date_col]])
  outcome_data[[outcome_date_col]] <- as.Date(outcome_data[[outcome_date_col]])

  # Sort period dates
  period_dates <- period_dates[order(period_dates[[id_col]], period_dates[[date_col]]), ]

  results <- list()

  for (user_id in unique(period_dates[[id_col]])) {
    user_periods <- period_dates[period_dates[[id_col]] == user_id, date_col, drop = TRUE]
    user_outcomes <- outcome_data[outcome_data[[id_col]] == user_id, ]

    if (length(user_periods) == 0 || nrow(user_outcomes) == 0) next

    for (i in seq_along(user_periods)) {
      period_start <- user_periods[i]

      # Determine cycle end
      if (i < length(user_periods)) {
        next_period <- user_periods[i + 1]
        cycle_length <- as.numeric(next_period - period_start)
        is_open <- FALSE
      } else {
        next_period <- period_start + 35
        cycle_length <- NA
        is_open <- TRUE
      }

      # Get outcomes within cycle window (day -14 to 20)
      cycle_start <- period_start - 14
      cycle_end <- period_start + 20

      mask <- user_outcomes[[outcome_date_col]] >= cycle_start &
              user_outcomes[[outcome_date_col]] <= cycle_end

      cycle_outcomes <- user_outcomes[mask, ]

      if (nrow(cycle_outcomes) == 0) next

      # Calculate cycle day
      cycle_outcomes$cycle_day <- as.numeric(
        cycle_outcomes[[outcome_date_col]] - period_start
      )
      cycle_outcomes$cycle_id <- i
      cycle_outcomes$cycle_length <- cycle_length
      cycle_outcomes$is_open_cycle <- is_open
      cycle_outcomes$user_id <- user_id

      results[[length(results) + 1]] <- cycle_outcomes
    }
  }

  if (length(results) == 0) {
    return(data.frame())
  }

  df <- do.call(rbind, results)

  # Rename columns for consistency
  names(df)[names(df) == outcome_col] <- "outcome"
  names(df)[names(df) == outcome_date_col] <- "date"

  return(df)
}


#' Filter Cycles by Length
#'
#' Filter to cycles with length between min and max days. Keep open-ended cycles.
#'
#' @param df Processed cycle data from process_periods()
#' @param min_length Minimum cycle length (default 21)
#' @param max_length Maximum cycle length (default 35)
#'
#' @return Filtered data frame
#' @export
filter_cycles <- function(df, min_length = 21, max_length = 35) {
  mask <- df$is_open_cycle |
          (!is.na(df$cycle_length) &
           df$cycle_length >= min_length &
           df$cycle_length <= max_length)

  return(df[mask, ])
}


#' Filter Users by Observation Count
#'
#' Keep only users with sufficient observations in both negative and positive cycle days.
#'
#' @param df Processed cycle data
#' @param min_negative_obs Minimum observations with cycle_day < 0 (default 5)
#' @param min_positive_obs Minimum observations with cycle_day >= 0 (default 5)
#'
#' @return Filtered data frame
#' @export
filter_users <- function(df, min_negative_obs = 5, min_positive_obs = 5) {
  # Count observations per user
  user_stats <- aggregate(
    cycle_day ~ user_id,
    data = df,
    FUN = function(x) c(
      neg_obs = sum(x < 0),
      pos_obs = sum(x >= 0)
    )
  )

  user_stats <- cbind(
    user_stats["user_id"],
    as.data.frame(user_stats$cycle_day)
  )

  # Filter qualifying users
  qualifying_users <- user_stats$user_id[
    user_stats$neg_obs >= min_negative_obs &
    user_stats$pos_obs >= min_positive_obs
  ]

  return(df[df$user_id %in% qualifying_users, ])
}


#' Add Dummy Day 21
#'
#' Add day 21 as a dummy variable representing day -14 for cyclical continuity.
#'
#' @param df Processed cycle data
#'
#' @return Data frame with day 21 dummy observations added
#' @export
add_dummy_day <- function(df) {
  # Get observations from day -14
  day_neg14 <- df[df$cycle_day == -14, ]

  if (nrow(day_neg14) == 0) {
    df$is_dummy <- FALSE
    return(df)
  }

  # Create dummy day 21 entries
  day_neg14$cycle_day <- 21
  day_neg14$is_dummy <- TRUE

  # Mark original data
  df$is_dummy <- FALSE

  return(rbind(df, day_neg14))
}
