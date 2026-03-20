#' Plot Cycle Effect
#'
#' Plot GAM results with confidence intervals and optional phase information.
#'
#' @param gam_result Result from fit_gam()
#' @param turning_points Optional turning points to mark
#' @param phase_models Optional phase model results
#' @param raw_data Optional processed data for showing daily means
#' @param title Plot title
#' @param xlabel X-axis label
#' @param ylabel Y-axis label
#' @param show_significance Whether to show significance annotation (default TRUE)
#' @param show_phases Whether to shade phases (default TRUE)
#' @param show_linear_models Whether to show linear phase lines (default TRUE)
#' @param show_turning_points Whether to mark turning points (default TRUE)
#' @param show_ci Whether to show confidence interval (default TRUE)
#' @param show_period_line Whether to show vertical line at day 0 (default TRUE)
#' @param show_raw_data Whether to show daily means line (default FALSE)
#' @param day_range Range of days to display (default c(-14, 13))
#' @param gam_color Color of GAM curve (default "steelblue")
#' @param gam_linewidth Line width of GAM curve (default 1)
#' @param ci_alpha Transparency of CI (default 0.3)
#' @param linear_colors Colors for linear phase models (default c("#E74C3C", "#27AE60"))
#' @param raw_data_alpha Transparency of daily means line (default 0.5)
#' @param raw_data_color Color of daily means line (default "gray50")
#'
#' @return ggplot2 object
#' @export
#'
#' @import ggplot2
plot_cycle_effect <- function(gam_result,
                               turning_points = NULL,
                               phase_models = NULL,
                               raw_data = NULL,
                               title = "Menstrual Cycle Effect on Outcome",
                               xlabel = "Cycle Day (Day 0 = Period Start)",
                               ylabel = NULL,
                               show_significance = TRUE,
                               show_phases = TRUE,
                               show_linear_models = TRUE,
                               show_turning_points = TRUE,
                               show_ci = TRUE,
                               show_period_line = TRUE,
                               show_raw_data = FALSE,
                               day_range = c(-14, 13),
                               gam_color = "steelblue",
                               gam_linewidth = 1,
                               ci_alpha = 0.3,
                               linear_colors = c("#E74C3C", "#27AE60"),
                               raw_data_alpha = 0.5,
                               raw_data_color = "gray50") {

  pred_df <- gam_result$predictions
  is_binary <- isTRUE(gam_result$is_binary)

  # Filter to day range
  plot_data <- pred_df[pred_df$cycle_day >= day_range[1] &
                        pred_df$cycle_day <= day_range[2], ]

  if (is_binary) {
    # For binary outcomes: use log odds
    if (is.null(ylabel)) ylabel <- "Log Odds"

    # Center log odds around mean for plotting
    mean_logit <- mean(plot_data$log_odds)
    plot_data$predicted_pct <- plot_data$log_odds - mean_logit

    # CI from bootstrap on log odds scale (if available) or from probability
    if ("or_ci_lower" %in% names(plot_data)) {
      plot_data$ci_lower_pct <- log(plot_data$or_ci_lower)
      plot_data$ci_upper_pct <- log(plot_data$or_ci_upper)
    } else {
      # Approximate CI on log odds from probability CI
      eps <- 1e-6
      plot_data$ci_lower_pct <- log(pmax(plot_data$ci_lower, eps) / pmax(1 - plot_data$ci_lower, eps)) - mean_logit
      plot_data$ci_upper_pct <- log(pmax(plot_data$ci_upper, eps) / pmax(1 - plot_data$ci_upper, eps)) - mean_logit
    }
  } else {
    # For continuous outcomes: percentage change from 100
    if (is.null(ylabel)) ylabel <- "% Change from Personal Average"

    plot_data$predicted_pct <- plot_data$predicted - 100
    plot_data$ci_lower_pct <- plot_data$ci_lower - 100
    plot_data$ci_upper_pct <- plot_data$ci_upper - 100
  }

  # Calculate y-axis range
  y_min <- min(plot_data$ci_lower_pct, na.rm = TRUE)
  y_max <- max(plot_data$ci_upper_pct, na.rm = TRUE)
  y_range <- y_max - y_min

  # Base plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = cycle_day))

  # Add daily means line if requested
  if (show_raw_data && !is.null(raw_data)) {
    raw_filtered <- raw_data[raw_data$cycle_day >= day_range[1] &
                              raw_data$cycle_day <= day_range[2] &
                              !raw_data$is_dummy, ]
    daily_means <- stats::aggregate(a ~ cycle_day, data = raw_filtered, FUN = mean)
    daily_means$a_pct <- daily_means$a - 100

    p <- p +
      ggplot2::geom_line(
        data = daily_means,
        ggplot2::aes(x = cycle_day, y = a_pct),
        color = raw_data_color,
        alpha = raw_data_alpha,
        linewidth = 0.8
      )
  }

  # Confidence interval (no legend mapping)
  if (show_ci) {
    p <- p +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = ci_lower_pct, ymax = ci_upper_pct),
        alpha = ci_alpha,
        fill = gam_color
      )
  }

  # GAM curve (no legend mapping)
  p <- p +
    ggplot2::geom_line(
      ggplot2::aes(y = predicted_pct),
      color = gam_color,
      linewidth = gam_linewidth
    )

  # Reference line at 0 (mean)
  p <- p +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.6)

  # Period start line
  if (show_period_line) {
    p <- p +
      ggplot2::geom_vline(xintercept = 0, linetype = "dotted", color = "red", linewidth = 0.8)
  }

  # Add phase shading
  if (show_phases && !is.null(phase_models) && nrow(phase_models) > 0) {
    phase_colors <- c("#E8F4F8", "#FFF3CD")
    for (i in seq_len(nrow(phase_models))) {
      pm <- phase_models[i, ]
      fill_color <- phase_colors[(i - 1) %% length(phase_colors) + 1]

      if (pm$end_day <= pm$start_day) {
        p <- p +
          ggplot2::annotate("rect",
            xmin = pm$start_day, xmax = day_range[2],
            ymin = -Inf, ymax = Inf,
            alpha = 0.1, fill = fill_color) +
          ggplot2::annotate("rect",
            xmin = day_range[1], xmax = pm$end_day,
            ymin = -Inf, ymax = Inf,
            alpha = 0.1, fill = fill_color)
      } else {
        p <- p +
          ggplot2::annotate("rect",
            xmin = pm$start_day, xmax = pm$end_day,
            ymin = -Inf, ymax = Inf,
            alpha = 0.1, fill = fill_color)
      }
    }
  }

  # Add linear model lines
  if (show_linear_models && !is.null(phase_models) && nrow(phase_models) > 0 && !is.null(turning_points)) {
    for (i in seq_len(nrow(phase_models))) {
      pm <- phase_models[i, ]
      line_color <- linear_colors[(i - 1) %% length(linear_colors) + 1]

      start_idx <- which.min(abs(plot_data$cycle_day - pm$start_day))
      end_idx <- which.min(abs(plot_data$cycle_day - pm$end_day))
      start_y <- plot_data$predicted_pct[start_idx]
      end_y <- plot_data$predicted_pct[end_idx]

      if (pm$end_day <= pm$start_day) {
        total_span <- (day_range[2] - pm$start_day) + (pm$end_day - day_range[1])
        frac1 <- (day_range[2] - pm$start_day) / total_span
        y_at_edge <- start_y + frac1 * (end_y - start_y)

        p <- p +
          ggplot2::annotate("segment",
            x = pm$start_day, xend = day_range[2],
            y = start_y, yend = y_at_edge,
            color = line_color, linetype = "dashed", linewidth = 0.8, alpha = 0.8
          ) +
          ggplot2::annotate("segment",
            x = day_range[1], xend = pm$end_day,
            y = y_at_edge, yend = end_y,
            color = line_color, linetype = "dashed", linewidth = 0.8, alpha = 0.8
          )
      } else {
        p <- p +
          ggplot2::annotate("segment",
            x = pm$start_day, xend = pm$end_day,
            y = start_y, yend = end_y,
            color = line_color, linetype = "dashed", linewidth = 0.8, alpha = 0.8
          )
      }
    }
  }

  # Define legend position for collision detection
  legend_x_min <- day_range[2] - 6
  legend_x_max <- day_range[2]
  legend_y_max <- y_max
  legend_y_min <- y_max - y_range * 0.18

  # Add turning points with collision detection
  if (show_turning_points && !is.null(turning_points)) {
    for (i in seq_along(turning_points)) {
      tp <- turning_points[i]
      if (tp >= day_range[1] && tp <= day_range[2]) {
        closest_idx <- which.min(abs(plot_data$cycle_day - tp))
        y_val <- plot_data$predicted_pct[closest_idx]

        # Check if label would collide with legend (top right area)
        in_legend_x <- tp > legend_x_min
        in_legend_y_above <- y_val > legend_y_min

        # Determine label position - avoid legend collision and stay in bounds
        if (in_legend_x && in_legend_y_above) {
          # Would collide with legend - put label below point
          label_vjust <- 1.5
        } else if (y_val > (y_max - y_range * 0.12)) {
          # Near top but not in legend area - put label below
          label_vjust <- 1.5
        } else {
          # Safe to put label above
          label_vjust <- -0.5
        }

        p <- p +
          ggplot2::geom_vline(xintercept = tp, linetype = "dashed",
                              color = "orange", alpha = 0.6) +
          ggplot2::geom_point(
            data = data.frame(x = tp, y = y_val),
            ggplot2::aes(x = x, y = y),
            color = "orange", size = 4
          ) +
          ggplot2::annotate(
            "label",
            x = tp, y = y_val,
            label = paste0("TP ", i, " (Day ", round(tp, 1), ")"),
            size = 3.5,
            vjust = label_vjust,
            label.padding = ggplot2::unit(0.25, "lines"),
            label.r = ggplot2::unit(0.2, "lines"),
            fill = "white",
            color = "gray30"
          )
      }
    }
  }

  # Add significance annotation as subtitle
  if (show_significance) {
    p_val <- gam_result$p_value
    sig_stars <- if (p_val < 0.001) "***" else if (p_val < 0.01) "**" else if (p_val < 0.05) "*" else "(ns)"

    if (is_binary) {
      # Binary outcome: show pseudo R² and peak-to-trough OR
      or_text <- if (!is.null(gam_result$peak_trough_or)) {
        sprintf("  |  Peak-Trough OR: %.2f", gam_result$peak_trough_or)
      } else ""
      sig_text <- paste0("Cycle Effect p-value: ", format(p_val, scientific = TRUE, digits = 2), " ", sig_stars,
                         "  |  Pseudo R²: ", round(gam_result$deviance_explained * 100, 1), "%",
                         or_text)
    } else {
      sig_text <- paste0("Cycle Effect p-value: ", format(p_val, scientific = TRUE, digits = 2), " ", sig_stars,
                         "  |  Deviance Explained: ", round(gam_result$deviance_explained * 100, 1), "%")
    }

    p <- p + ggplot2::labs(subtitle = sig_text)
  }

  # === LEGEND (top right corner) ===
  ci_symbol <- "\u2588"
  gam_symbol <- "\u2014"
  legend_text <- paste0(ci_symbol, "  95% CI\n", gam_symbol, "  GAM Fit")

  p <- p +
    ggplot2::annotate(
      "label",
      x = day_range[2],
      y = y_max,
      label = legend_text,
      hjust = 1, vjust = 1,
      size = 4,
      fontface = "bold",
      color = gam_color,
      fill = "white",
      label.padding = ggplot2::unit(0.4, "lines"),
      label.r = ggplot2::unit(0.3, "lines")
    )

  # === PHASE SLOPES (bottom right corner) ===
  if (!is.null(phase_models) && nrow(phase_models) > 0) {
    phase_text <- "Phase Slopes:\n"
    for (i in seq_len(nrow(phase_models))) {
      pm <- phase_models[i, ]
      sig <- if (pm$p_value < 0.001) "***" else if (pm$p_value < 0.01) "**" else if (pm$p_value < 0.05) "*" else ""
      phase_text <- paste0(phase_text,
        sprintf("Phase %d (Day %.0f to %.0f): %.3f +/- %.3f %s\n",
                pm$phase, pm$start_day, pm$end_day,
                pm$slope, pm$slope_se, sig))
    }

    p <- p +
      ggplot2::annotate(
        "label",
        x = day_range[2],
        y = y_min,
        label = phase_text,
        hjust = 1, vjust = 0,
        size = 3.5,
        fontface = "bold",
        color = "gray30",
        fill = "white",
        label.padding = ggplot2::unit(0.4, "lines"),
        label.r = ggplot2::unit(0.3, "lines")
      )
  }

  # === WATERMARK (bottom left corner) ===
  p <- p +
    ggplot2::annotate(
      "text",
      x = day_range[1],
      y = y_min,
      label = "Produced by MCAnalysis",
      hjust = 0, vjust = 0,
      size = 3.5,
      fontface = "italic",
      color = "gray50"
    )

  # Formatting with consistent font
  p <- p +
    ggplot2::scale_x_continuous(breaks = seq(day_range[1], day_range[2], by = 2)) +
    ggplot2::labs(
      title = title,
      x = xlabel,
      y = ylabel
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      text = ggplot2::element_text(size = 12),
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 11),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 11),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "none"
    )

  return(p)
}


#' Plot Confounder Effects
#'
#' Plot forest plot of confounder effects.
#'
#' @param confounder_results Results from analyze_confounders()
#' @param title Plot title
#'
#' @return ggplot2 object
#' @export
#'
#' @import ggplot2
plot_confounder_effects <- function(confounder_results,
                                     title = "Confounder Effects on Cycle-Outcome Relationship") {

  if (is.null(confounder_results) || nrow(confounder_results) == 0) {
    message("No confounder results to display")
    return(NULL)
  }

  # Add significance indicator
  confounder_results$significant <- confounder_results$p_value < 0.05

  p <- ggplot2::ggplot(
    confounder_results,
    ggplot2::aes(x = coefficient, y = stats::reorder(confounder, coefficient))
  ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = ci_lower, xmax = ci_upper),
      height = 0.2,
      color = "steelblue"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(color = significant),
      size = 3
    ) +
    ggplot2::scale_color_manual(
      values = c("TRUE" = "red", "FALSE" = "steelblue"),
      guide = "none"
    ) +
    ggplot2::labs(
      title = title,
      x = "Coefficient (effect on normalized outcome)",
      y = ""
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      text = ggplot2::element_text(size = 12),
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 11)
    )

  return(p)
}
