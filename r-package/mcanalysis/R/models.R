#' Normalize Outcome
#'
#' Normalize outcome to percentage of each user's mean.
#'
#' @param df Dataset with outcome variable
#' @param outcome_col Name of outcome column (default "outcome")
#'
#' @return Data frame with normalized outcome 'a' added
#' @export
normalize_outcome <- function(df, outcome_col = "outcome") {
  # Calculate each user's mean outcome
  user_means <- tapply(df[[outcome_col]], df$user_id, mean, na.rm = TRUE)

  # Express outcome as percentage of user's mean
  df$user_mean <- user_means[as.character(df$user_id)]
  df$a <- (df[[outcome_col]] / df$user_mean) * 100

  return(df)
}


#' Fit GAM Model
#'
#' Fit a Generalized Additive Model of cycle day on normalized outcome.
#' Uses Fourier basis for cyclic patterns to match Python implementation.
#'
#' @param df Dataset with normalized outcome
#' @param outcome_col Name of outcome column (default "a")
#' @param day_col Name of cycle day column (default "cycle_day")
#' @param k Number of Fourier harmonics (default 10, maps to n_splines in Python)
#' @param cyclic Whether to use cyclic Fourier basis (default TRUE)
#' @param cycle_length Expected cycle length in days (default 28)
#'
#' @return List with model, predictions, summary, and statistics
#' @export
fit_gam <- function(df, outcome_col = "a", day_col = "cycle_day",
                    k = 10, cyclic = TRUE, cycle_length = 28) {

  X_days <- df[[day_col]]
  y <- df[[outcome_col]]

  if (cyclic) {
    # Use Fourier basis for cyclic pattern (matches Python)
    n_harmonics <- k
    X_design <- create_fourier_basis(X_days, n_harmonics, cycle_length)

    # Fit OLS with Fourier terms
    model <- lm(y ~ X_design - 1)  # -1 because X_design includes intercept

    # Generate predictions
    day_range <- seq(-14, 13, length.out = 100)
    X_pred <- create_fourier_basis(day_range, n_harmonics, cycle_length)
    pred_fit <- as.vector(X_pred %*% coef(model))

    # Bootstrap for confidence intervals
    n_boot <- 200
    boot_preds <- matrix(0, nrow = n_boot, ncol = length(day_range))
    set.seed(42)  # For reproducibility
    for (i in seq_len(n_boot)) {
      idx <- sample(length(y), length(y), replace = TRUE)
      boot_model <- lm(y[idx] ~ X_design[idx, ] - 1)
      boot_preds[i, ] <- as.vector(X_pred %*% coef(boot_model))
    }

    ci_lower <- apply(boot_preds, 2, quantile, probs = 0.025)
    ci_upper <- apply(boot_preds, 2, quantile, probs = 0.975)

    # Get statistics
    model_summary <- summary(model)
    p_value <- pf(model_summary$fstatistic[1],
                  model_summary$fstatistic[2],
                  model_summary$fstatistic[3],
                  lower.tail = FALSE)
    edf <- n_harmonics * 2  # Each harmonic has sin and cos
    dev_explained <- model_summary$r.squared

    predictions <- data.frame(
      cycle_day = day_range,
      predicted = pred_fit,
      ci_lower = ci_lower,
      ci_upper = ci_upper
    )

  } else {
    # Use mgcv splines (non-cyclic)
    formula <- as.formula(paste0(outcome_col, " ~ s(", day_col, ", k=", k, ")"))
    model <- mgcv::gam(formula, data = df)

    day_range <- seq(min(df[[day_col]]), max(df[[day_col]]), length.out = 100)
    pred_data <- data.frame(cycle_day = day_range)
    names(pred_data) <- day_col

    pred <- predict(model, newdata = pred_data, se.fit = TRUE)

    predictions <- data.frame(
      cycle_day = day_range,
      predicted = pred$fit,
      ci_lower = pred$fit - 1.96 * pred$se.fit,
      ci_upper = pred$fit + 1.96 * pred$se.fit
    )

    model_summary <- summary(model)
    p_value <- model_summary$s.table[1, "p-value"]
    edf <- model_summary$s.table[1, "edf"]
    dev_explained <- model_summary$dev.expl
  }

  result <- list(
    model = model,
    predictions = predictions,
    summary = model_summary,
    p_value = p_value,
    edf = edf,
    deviance_explained = dev_explained,
    n_obs = nrow(df)
  )

  class(result) <- c("mcgam", "list")

  return(result)
}


#' Create Fourier Basis Matrix
#'
#' Create Fourier basis matrix for cyclic regression.
#'
#' @param days Vector of cycle days
#' @param n_harmonics Number of Fourier harmonics
#' @param cycle_length Expected cycle length in days
#'
#' @return Design matrix with intercept and sin/cos terms
#' @keywords internal
create_fourier_basis <- function(days, n_harmonics, cycle_length) {
  n <- length(days)
  # Convert days to radians (full cycle = 2*pi)
  radians <- 2 * pi * days / cycle_length

  # Build design matrix: intercept + sin/cos pairs for each harmonic
  X <- matrix(1, nrow = n, ncol = 1 + 2 * n_harmonics)
  for (h in seq_len(n_harmonics)) {
    X[, 2 * h] <- sin(h * radians)
    X[, 2 * h + 1] <- cos(h * radians)
  }

  return(X)
}


#' Analyze Confounders
#'
#' Analyze effect of confounders on the cycle-outcome relationship.
#'
#' @param df Main dataset with normalized outcome
#' @param confounder_data Data frame with confounders
#' @param outcome_col Name of outcome column (default "a")
#' @param day_col Name of cycle day column (default "cycle_day")
#' @param id_col Column name for user ID (default "user_id")
#' @param confounder_cols Vector of confounder column names (default NULL uses all)
#'
#' @return Data frame with confounder results
#' @export
#'
#' @importFrom mgcv gam s
analyze_confounders <- function(df, confounder_data,
                                 outcome_col = "a",
                                 day_col = "cycle_day",
                                 id_col = "user_id",
                                 confounder_id_col = NULL,
                                 confounder_cols = NULL) {

  # Handle ID column renaming for confounder data
  conf_data <- confounder_data

  # Find the ID column in confounder data
  if (is.null(confounder_id_col)) {
    # Try to find a matching column
    if (id_col %in% names(conf_data)) {
      confounder_id_col <- id_col
    } else {
      # Look for common ID column names
      possible_ids <- c("userid_anon", "user_id", "id", "ID", "subject_id")
      found <- intersect(possible_ids, names(conf_data))
      if (length(found) > 0) {
        confounder_id_col <- found[1]
      } else {
        stop("Could not find ID column in confounder data")
      }
    }
  }

  # Rename confounder ID column to match
  if (confounder_id_col != id_col) {
    names(conf_data)[names(conf_data) == confounder_id_col] <- id_col
  }

  # Merge confounder data
  merged <- merge(df, conf_data, by = id_col, all.x = TRUE)

  if (is.null(confounder_cols)) {
    confounder_cols <- setdiff(names(confounder_data), id_col)
  }

  results <- list()

  for (conf in confounder_cols) {
    if (!(conf %in% names(merged))) next

    # Drop rows with missing values
    subset_df <- merged[!is.na(merged[[conf]]) & !is.na(merged[[outcome_col]]), ]

    if (nrow(subset_df) == 0) next

    tryCatch({
      # Fit model with smooth cycle day + linear confounder
      formula <- as.formula(paste0(outcome_col, " ~ s(", day_col, ") + ", conf))
      model <- mgcv::gam(formula, data = subset_df)

      model_summary <- summary(model)

      # Get confounder coefficient
      coef_idx <- which(names(model$coefficients) == conf)
      if (length(coef_idx) == 0) next

      coef_val <- model$coefficients[coef_idx]
      se_val <- sqrt(diag(model$Vp)[coef_idx])
      p_val <- model_summary$p.table[conf, "Pr(>|t|)"]

      results[[length(results) + 1]] <- data.frame(
        confounder = conf,
        coefficient = coef_val,
        std_error = se_val,
        p_value = p_val,
        ci_lower = coef_val - 1.96 * se_val,
        ci_upper = coef_val + 1.96 * se_val
      )
    }, error = function(e) {
      warning(paste("Could not analyze confounder", conf, ":", e$message))
    })
  }

  if (length(results) == 0) {
    return(data.frame())
  }

  return(do.call(rbind, results))
}


#' Find Turning Points
#'
#' Find turning points (peaks/troughs) of the GAM curve.
#'
#' @param gam_result Result from fit_gam()
#' @param find_peaks_troughs If TRUE, find turning points (first derivative sign change).
#'   If FALSE, find points where curvature changes sign. Default TRUE.
#' @param round_to_whole_days If TRUE, round results to nearest whole day. Default TRUE.
#' @param day_range Only return points within this range. Default c(-14, 13).
#'
#' @return Vector of cycle days where turning points occur
#' @export
find_turning_points <- function(gam_result,
                              find_peaks_troughs = TRUE,
                              round_to_whole_days = TRUE,
                              day_range = c(-14, 13)) {
  pred_df <- gam_result$predictions

  days <- pred_df$cycle_day
  y <- pred_df$predicted

  # Calculate first derivative using finite differences
  dy <- diff(y) / diff(days)
  days_dy <- (days[-1] + days[-length(days)]) / 2

  if (find_peaks_troughs) {
    # Find turning points: where first derivative changes sign (peaks/troughs)
    derivative <- dy
    days_d <- days_dy
  } else {
    # Find points where second derivative changes sign
    d2y <- diff(dy) / diff(days_dy)
    days_d <- (days_dy[-1] + days_dy[-length(days_dy)]) / 2
    derivative <- d2y
  }

  # Find sign changes
  sign_changes <- which(diff(sign(derivative)) != 0)

  result_days <- numeric()

  for (idx in sign_changes) {
    if (idx + 1 <= length(days_d)) {
      # Linear interpolation to find crossing point
      x1 <- days_d[idx]
      x2 <- days_d[idx + 1]
      y1 <- derivative[idx]
      y2 <- derivative[idx + 1]

      if (y2 != y1) {
        crossing_day <- x1 - y1 * (x2 - x1) / (y2 - y1)

        if (round_to_whole_days) {
          crossing_day <- round(crossing_day)
        }

        # Only include if within specified range
        if (crossing_day >= day_range[1] && crossing_day <= day_range[2]) {
          result_days <- c(result_days, crossing_day)
        }
      }
    }
  }

  # Remove duplicates if rounding
  if (round_to_whole_days) {
    result_days <- sort(unique(result_days))
  } else {
    result_days <- sort(result_days)
  }

  return(result_days)
}


#' Fit Phase Models
#'
#' Fit linear models for each phase defined by turning points.
#' Slopes are calculated from the GAM predictions at the turning points.
#'
#' @param df Dataset with normalized outcome
#' @param turning_points Turning points from find_turning_points()
#' @param gam_result Optional GAM result to use for slope calculation
#' @param outcome_col Name of outcome column (default "a")
#' @param day_col Name of cycle day column (default "cycle_day")
#'
#' @return Data frame with linear model results for each phase
#' @export
fit_phase_models <- function(df, turning_points,
                              gam_result = NULL,
                              outcome_col = "a",
                              day_col = "cycle_day") {

  if (length(turning_points) < 2) {
    warning("Need at least 2 turning points for phase models")
    return(data.frame())
  }

  # Sort turning points
  turns <- sort(turning_points)

  min_day <- min(df[[day_col]])
  max_day <- max(df[[day_col]])

  # Get GAM predictions at turning points if available
  gam_values <- NULL
  gam_ci_lower <- NULL
  gam_ci_upper <- NULL
  if (!is.null(gam_result)) {
    pred_df <- gam_result$predictions
    gam_values <- list()
    gam_ci_lower <- list()
    gam_ci_upper <- list()
    for (ip in turns) {
      closest_idx <- which.min(abs(pred_df$cycle_day - ip))
      gam_values[[as.character(ip)]] <- pred_df$predicted[closest_idx]
      gam_ci_lower[[as.character(ip)]] <- pred_df$ci_lower[closest_idx]
      gam_ci_upper[[as.character(ip)]] <- pred_df$ci_upper[closest_idx]
    }
  }

  results <- list()
  n_phases <- length(turns)

  for (i in seq_len(n_phases)) {
    start <- turns[i]
    end <- turns[((i %% n_phases) + 1)]

    # Handle wraparound for cyclical model
    if (end <= start) {
      mask <- df[[day_col]] >= start | df[[day_col]] < end
      day_span <- (max_day - start) + (end - min_day)
    } else {
      mask <- df[[day_col]] >= start & df[[day_col]] < end
      day_span <- end - start
    }

    phase_data <- df[mask, ]

    if (nrow(phase_data) < 3) next

    tryCatch({
      if (!is.null(gam_values)) {
        # Calculate slope from GAM predictions at turning points
        y_start <- gam_values[[as.character(start)]]
        y_end <- gam_values[[as.character(end)]]

        if (day_span != 0) {
          slope <- (y_end - y_start) / day_span
        } else {
          slope <- 0
        }

        intercept <- y_start - slope * start

        # Estimate SE from GAM confidence intervals
        se_start <- (gam_values[[as.character(start)]] - gam_ci_lower[[as.character(start)]]) / 1.96
        se_end <- (gam_values[[as.character(end)]] - gam_ci_lower[[as.character(end)]]) / 1.96
        slope_se <- sqrt(se_start^2 + se_end^2) / day_span

        # Calculate t-statistic and p-value
        if (slope_se > 0) {
          t_stat <- slope / slope_se
          p_value <- 2 * pt(abs(t_stat), df = nrow(phase_data) - 2, lower.tail = FALSE)
        } else {
          p_value <- 1
        }

        # R-squared from predicted line vs actual data
        x <- phase_data[[day_col]]
        if (end <= start) {
          x <- ifelse(x < start, x + (max_day - min_day + 1), x)
        }
        y_pred <- intercept + slope * x
        y_actual <- phase_data[[outcome_col]]
        ss_res <- sum((y_actual - y_pred)^2)
        ss_tot <- sum((y_actual - mean(y_actual))^2)
        r_squared <- ifelse(ss_tot > 0, 1 - (ss_res / ss_tot), 0)

      } else {
        # Fall back to OLS on raw data
        x <- phase_data[[day_col]]
        if (end <= start) {
          x <- ifelse(x < start, x + (max_day - min_day + 1), x)
        }

        model <- lm(phase_data[[outcome_col]] ~ x)
        model_summary <- summary(model)

        slope <- coef(model)[2]
        intercept <- coef(model)[1]
        slope_se <- model_summary$coefficients[2, "Std. Error"]
        p_value <- model_summary$coefficients[2, "Pr(>|t|)"]
        r_squared <- model_summary$r.squared
      }

      results[[length(results) + 1]] <- data.frame(
        phase = i,
        start_day = start,
        end_day = end,
        slope = slope,
        intercept = intercept,
        slope_se = slope_se,
        p_value = p_value,
        r_squared = r_squared
      )
    }, error = function(e) {
      warning(paste("Could not fit phase", i, "model:", e$message))
    })
  }

  if (length(results) == 0) {
    return(data.frame())
  }

  return(do.call(rbind, results))
}
