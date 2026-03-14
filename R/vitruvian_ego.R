#' @title Vitruvian plot — EGO layout (one panel per treatment)
#'
#' @description Creates a circular polar chart in the style of Ostinelli et al.
#'   (\url{https://github.com/EGOstinelli/Vitruvian-plot}), with
#'   \strong{one panel per treatment} and \strong{outcomes as radial sectors}.
#'   Bar height = absolute event rate (\%); bar colour = CINeMA confidence
#'   rating (when \code{cinema} is supplied) or \code{bar_color} (default).
#'
#'   Design elements (spider-web grid, outer coloured band, arc-following
#'   sector labels, angle-aware badges) are borrowed from
#'   \code{\link{vitruvian}()}.  Only the colour logic differs: bars are
#'   coloured by CINeMA rating rather than by p-value gradient.
#'
#' @param outcomes List of outcome specifications.  Each element is a named
#'   list:
#'   \describe{
#'     \item{\code{x}}{A \code{netmeta} object or path to an \code{.rds} file.}
#'     \item{\code{name}}{Internal identifier.}
#'     \item{\code{label}}{Display label (defaults to \code{name}).}
#'     \item{\code{small_values}}{\code{"desirable"} or \code{"undesirable"}.}
#'     \item{\code{cer}}{Control event rate (0–1); \code{NULL}/\code{"metaprop"}
#'       = auto-estimate; \code{"simple"} = naive pooled proportion.}
#'     \item{\code{cinema}}{Per-outcome CINeMA CSV or data frame (overrides
#'       top-level \code{cinema}).}
#'     \item{\code{reference}}{Per-outcome reference override.}
#'     \item{\code{group}}{Optional group name for outer-band colouring
#'       (e.g.\ \code{"Efficacy"} / \code{"Tolerability"}).}
#'   }
#' @param reference Reference treatment name.
#' @param common Logical. Use common-effects model? Default \code{FALSE}.
#' @param digits Decimal places for badge labels.  Default \code{1}.
#' @param ncol Number of facet columns.  Default \code{3}.
#' @param cinema Global CINeMA CSV path or data frame.
#' @param palette Named character vector: \code{c("High"=..., "Moderate"=...,
#'   "Low"=..., "Very low"=...)}.  Standard green–red scheme by default.
#' @param bar_color Default bar fill when no CINeMA rating is available.
#'   Default \code{"#4EA6DD"}.
#' @param ref_color Fill for the reference event-rate area and reference-panel
#'   bars.  Default \code{"#aec6cf"}.
#' @param ref_alpha Transparency of the reference area overlay.
#'   Default \code{0.35}.
#' @param group_colors Named vector mapping group names to hex colours
#'   (overrides the default pastel palette for the outer band).
#' @param strip_color Background of treatment-name strip.
#'   Default \code{"#2c3e50"}.
#' @param file Output path (\code{.png}, \code{.pdf}, \code{.svg}).
#'   \code{NULL} renders to Viewer at fixed size.
#' @param width Plot width in inches.  Default \code{8}.
#' @param height Plot height in inches.  Default \code{8}.
#'
#' @return A \code{ggplot} object (invisibly when saved to file / Viewer).
#' @export
#'
#' @examples
#' \dontrun{
#' net     <- build_w2i_netmeta("remission_lt")
#' net_dlt <- build_w2i_netmeta("dropout_lt")
#' cinema_path <- system.file("extdata", "w2i_cinema.csv", package = "netmetaviz")
#'
#' vitruvian_ego(
#'   outcomes = list(
#'     list(x = net,     name = "remission", label = "Remission",
#'          small_values = "undesirable", cer = 0.28, group = "Efficacy"),
#'     list(x = net_dlt, name = "dropout",   label = "Dropout",
#'          small_values = "desirable",   cer = 0.20, group = "Tolerability")
#'   ),
#'   reference = "Pharmacotherapy",
#'   cinema    = cinema_path,
#'   ncol      = 3,
#'   file      = "vitruvian_ego.png",
#'   width = 12, height = 5
#' )
#' }
vitruvian_ego <- function(outcomes,
                          reference,
                          common       = FALSE,
                          digits       = 1,
                          ncol         = 3,
                          cinema       = NULL,
                          palette      = NULL,
                          bar_color    = "#4EA6DD",
                          ref_color    = "#aec6cf",
                          ref_alpha    = 0.35,
                          group_colors = NULL,
                          strip_color  = "#2c3e50",
                          file         = NULL,
                          width        = 8,
                          height       = 8) {

  # ---- 1. Default CINeMA palette ----
  if (is.null(palette)) {
    palette <- c(
      "High"     = "#92D050",
      "Moderate" = "#FFFF00",
      "Low"      = "#FF9900",
      "Very low" = "#FF0000"
    )
  }

  # ---- 2. Load netmeta objects ----
  outcomes <- lapply(outcomes, function(oc) {
    if (is.character(oc$x)) oc$x <- readRDS(oc$x)
    oc
  })

  # ---- 3. Parse global CINeMA ----
  global_cinema_df <- if (!is.null(cinema)) parse_cinema(cinema) else NULL

  # ---- 4. Outcome metadata ----
  outcome_names  <- vapply(outcomes, function(oc) oc$name, character(1))
  outcome_labels <- vapply(outcomes,
    function(oc) if (!is.null(oc$label)) oc$label else oc$name,
    character(1))
  outcome_groups <- vapply(outcomes,
    function(oc) if (!is.null(oc$group)) as.character(oc$group) else NA_character_,
    character(1))
  n_oc <- length(outcomes)

  # ---- 5. Treatment list (union across outcomes; reference first) ----
  all_trts   <- unique(unlist(lapply(outcomes, function(oc) oc$x$trts)))
  treatments <- c(reference, sort(setdiff(all_trts, reference)))

  fmt <- paste0("%.", digits, "f%%")

  # ---- 6. Compute per-treatment × per-outcome values ----
  rows <- list()

  for (k in seq_along(outcomes)) {
    oc  <- outcomes[[k]]
    nx  <- oc$x
    ref <- if (!is.null(oc$reference)) oc$reference else reference

    is_binary <- nx$sm %in% c("OR", "RR", "RD")

    cer <- oc$cer
    if (is_binary) {
      if (is.null(cer) || identical(cer, "metaprop")) {
        cer <- .estimate_cer(nx, ref, method = "metaprop")
      } else if (identical(cer, "simple")) {
        cer <- .estimate_cer(nx, ref, method = "simple")
      }
    }
    cer_pct <- if (is.numeric(cer)) cer * 100 else NA_real_

    te_mat <- .nm_mat(nx, "TE", common)

    oc_cinema_df <- if (!is.null(oc$cinema)) {
      parse_cinema(oc$cinema)
    } else {
      global_cinema_df
    }

    for (trt in treatments) {

      if (trt == ref) {
        abs_rate      <- cer_pct
        cinema_rating <- NA_character_
        is_ref_trt    <- TRUE

      } else if (!trt %in% rownames(te_mat) || !ref %in% rownames(te_mat)) {
        abs_rate      <- NA_real_
        cinema_rating <- NA_character_
        is_ref_trt    <- FALSE

      } else {
        te <- te_mat[trt, ref]

        if (nx$sm == "OR") {
          ar <- .or_to_ar(exp(te), cer)
        } else if (nx$sm == "RR") {
          ar <- cer * exp(te)
        } else if (nx$sm == "RD") {
          ar <- cer + te
        } else {
          ar <- te
        }

        abs_rate      <- if (is_binary) ar * 100 else ar
        cinema_rating <- if (!is.null(oc_cinema_df)) {
          get_cinema_rating(trt, ref, oc_cinema_df)
        } else {
          NA_character_
        }
        is_ref_trt <- FALSE
      }

      rows[[length(rows) + 1L]] <- data.frame(
        treatment    = trt,
        outcome_idx  = k,
        abs_rate     = abs_rate,
        cer_pct      = cer_pct,
        is_ref       = is_ref_trt,
        cinema       = cinema_rating,
        stringsAsFactors = FALSE
      )
    }
  }

  plot_df <- do.call(rbind, rows)

  # ---- 7. Treatment factor ordering ----
  plot_df$treatment <- factor(plot_df$treatment, levels = treatments)

  # ---- 8. Fill colours (CINeMA-based) ----
  plot_df$fill_color <- ifelse(
    plot_df$is_ref,
    ref_color,
    ifelse(
      !is.na(plot_df$cinema) & plot_df$cinema %in% names(palette),
      palette[plot_df$cinema],
      bar_color
    )
  )

  # ---- 9. Value labels for badges ----
  plot_df$val_label <- ifelse(
    !plot_df$is_ref & !is.na(plot_df$abs_rate),
    sprintf(fmt, plot_df$abs_rate),
    NA_character_
  )
  plot_df$ref_val_label <- ifelse(
    !is.na(plot_df$cer_pct) & plot_df$cer_pct > 0,
    sprintf(fmt, plot_df$cer_pct),
    NA_character_
  )

  # ---- 10. Y-scale (round to nearest 10) ----
  y_max <- max(c(plot_df$abs_rate, plot_df$cer_pct), na.rm = TRUE)
  y_max <- ceiling(y_max / 10) * 10
  if (!is.finite(y_max) || y_max == 0) y_max <- 50

  major_at <- seq(10, y_max, by = 10)
  minor_at <- seq(5,  y_max, by = 5)

  # ---- 11. Angle-aware badge positions (same formula as vitruvian()) ----
  badge_center_y <- y_max * 0.55
  vd    <- y_max * 0.09
  sq2   <- sqrt(2)
  alpha <- badge_center_y * 2 * pi / n_oc

  k_idx <- plot_df$outcome_idx
  phi_k <- 2 * pi * (k_idx - 0.5) / n_oc

  eer_dx <- (cos(phi_k) * (-vd/sq2) - sin(phi_k) * ( vd/sq2)) / alpha
  eer_dy <-  sin(phi_k) * (-vd/sq2) + cos(phi_k) * ( vd/sq2)
  cer_dx <- (cos(phi_k) * ( vd/sq2) - sin(phi_k) * (-vd/sq2)) / alpha
  cer_dy <-  sin(phi_k) * ( vd/sq2) + cos(phi_k) * (-vd/sq2)

  plot_df$eer_x <- k_idx + eer_dx
  plot_df$eer_y <- ifelse(!plot_df$is_ref & !is.na(plot_df$val_label),
                          badge_center_y + eer_dy, NA_real_)
  plot_df$cer_x <- k_idx + cer_dx
  plot_df$cer_y <- ifelse(!is.na(plot_df$ref_val_label),
                          badge_center_y + cer_dy, NA_real_)

  # ---- 12. Arc-following outcome labels ----
  label_arc_df <- do.call(rbind, lapply(seq_len(n_oc), function(k) {
    n_pts <- 60
    data.frame(
      x       = seq(k - 0.49, k + 0.49, length.out = n_pts),
      y       = rep(y_max * 1.07, n_pts),
      arc_lab = rep(outcome_labels[k], n_pts),
      arc_grp = rep(as.character(k), n_pts),
      stringsAsFactors = FALSE
    )
  }))

  # ---- 13. Group structure (optional) ----
  has_groups <- !all(is.na(outcome_groups))

  group_palette_colors <- c(
    "#A8C4E0", "#F5CBA7", "#C5E0B4", "#FFE5A0",
    "#D6C0E8", "#A0DCF0", "#F5B7B1", "#C8DDD0",
    "#FFC8C8", "#C0B4D6"
  )
  grp_colors     <- NULL
  group_bounds_x <- numeric(0)
  group_arc_df   <- NULL

  if (has_groups) {
    unique_grps <- unique(outcome_groups[!is.na(outcome_groups)])
    grp_colors  <- stats::setNames(
      group_palette_colors[seq_along(unique_grps)], unique_grps
    )
    if (!is.null(group_colors)) {
      cmn <- intersect(names(group_colors), unique_grps)
      grp_colors[cmn] <- group_colors[cmn]
    }

    for (i in seq_len(n_oc)) {
      ni <- if (i == n_oc) 1L else i + 1L
      if (!is.na(outcome_groups[i]) && !is.na(outcome_groups[ni]) &&
          outcome_groups[i] != outcome_groups[ni])
        group_bounds_x <- c(group_bounds_x, i + 0.5)
    }

    group_arc_df <- do.call(rbind, lapply(unique_grps, function(grp) {
      idx   <- which(outcome_groups == grp)
      n_pts <- max(60, (max(idx) - min(idx) + 1) * 20)
      data.frame(
        x       = seq(min(idx) - 0.49, max(idx) + 0.49, length.out = n_pts),
        y       = rep(y_max * 1.14, n_pts),
        arc_lab = rep(grp, n_pts),
        arc_grp = rep(paste0("grp_", grp), n_pts),
        stringsAsFactors = FALSE
      )
    }))
  }

  # ---- 14. Outer label band (coloured annular ring) ----
  band_top <- y_max * 1.25

  band_color_vec <- if (has_groups) {
    vapply(outcome_groups, function(g) {
      if (is.na(g)) "#DCDCDC" else grp_colors[[g]]
    }, character(1))
  } else {
    rep("#DCDCDC", n_oc)
  }

  ring_df <- do.call(rbind, lapply(seq_len(n_oc), function(i) {
    data.frame(
      x        = seq(i - 0.5, i + 0.5, length.out = 30),
      ymin_val = y_max,
      ymax_val = band_top,
      fill_col = band_color_vec[i],
      sector   = i,
      stringsAsFactors = FALSE
    )
  }))

  y_scale_top <- y_max * if (has_groups) 1.27 else 1.22

  # ---- 15. Build ggplot ----
  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = outcome_idx, y = abs_rate)
  ) +
    # Outer label band
    ggplot2::geom_ribbon(
      data = ring_df,
      ggplot2::aes(x = x, ymin = ymin_val, ymax = ymax_val,
                   fill = fill_col, group = sector),
      colour = NA, inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_identity() +
    # Sector boundary radial lines
    ggplot2::annotate(
      "segment",
      x      = c(0.5 + 1e-3, seq(1.5, n_oc - 0.5, by = 1), n_oc + 0.5 - 1e-3),
      xend   = c(0.5 + 1e-3, seq(1.5, n_oc - 0.5, by = 1), n_oc + 0.5 - 1e-3),
      y      = 0,
      yend   = band_top,
      colour = "grey60", linewidth = 0.3
    ) +
    # Reference event-rate area (semi-transparent)
    ggplot2::geom_col(
      ggplot2::aes(y = cer_pct),
      fill = ref_color, alpha = ref_alpha,
      width = 0.9, colour = NA
    ) +
    # Main bars (CINeMA colours)
    ggplot2::geom_col(
      ggplot2::aes(fill = I(fill_color)),
      width = 0.85, colour = "white", linewidth = 0.25
    ) +
    # Major concentric circles (solid, every 10%)
    ggplot2::geom_hline(
      yintercept = major_at,
      colour = "grey65", linewidth = 0.35, linetype = "solid"
    ) +
    # Minor concentric circles (dashed, every 5%)
    ggplot2::geom_hline(
      yintercept = minor_at[minor_at %% 10 != 0],
      colour = "grey82", linewidth = 0.2, linetype = "dashed"
    ) +
    # Outer circle at y_max
    ggplot2::annotate("path",
      x = seq(0.5, n_oc + 0.5, length.out = 360),
      y = y_max,
      colour = "grey30", linewidth = 0.5
    ) +
    # Outer band boundary at band_top
    ggplot2::annotate("path",
      x = seq(0.5, n_oc + 0.5, length.out = 360),
      y = band_top,
      colour = "grey30", linewidth = 0.3
    ) +
    # Arc-following outcome labels
    geomtextpath::geom_textpath(
      data = label_arc_df,
      ggplot2::aes(x = x, y = y, label = arc_lab, group = arc_grp),
      colour = "grey10", fontface = "bold", size = 2.5,
      hjust = 0.5, linecolour = NA, inherit.aes = FALSE
    ) +
    # Group boundary lines (if groups defined)
    (if (has_groups && length(group_bounds_x) > 0)
      ggplot2::geom_segment(
        data = data.frame(x = group_bounds_x, xend = group_bounds_x,
                          y = 0, yend = band_top),
        ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
        colour = "grey30", linewidth = 0.7, inherit.aes = FALSE
      )
    else NULL) +
    # Group arc labels (if groups defined)
    (if (has_groups && !is.null(group_arc_df))
      geomtextpath::geom_textpath(
        data = group_arc_df,
        ggplot2::aes(x = x, y = y, label = arc_lab, group = arc_grp),
        colour = "grey10", fontface = "bold", size = 3.0,
        hjust = 0.5, linecolour = NA, inherit.aes = FALSE
      )
    else NULL) +
    # EER badge (grey rectangle)
    ggplot2::geom_label(
      ggplot2::aes(x = eer_x, y = eer_y, label = val_label),
      inherit.aes = FALSE,
      fill = "#d4d4d4", colour = "grey20",
      size = 3.0, fontface = "bold",
      label.padding = ggplot2::unit(c(0.10, 0.28, 0.10, 0.28), "lines"),
      label.r = ggplot2::unit(0, "lines"),
      label.size = 0, na.rm = TRUE
    ) +
    # CER badge (light blue rounded)
    ggplot2::geom_label(
      ggplot2::aes(x = cer_x, y = cer_y, label = ref_val_label),
      inherit.aes = FALSE,
      fill = "#d4ecf7", colour = "grey20",
      size = 3.0, fontface = "bold",
      label.padding = ggplot2::unit(c(0.10, 0.28, 0.10, 0.28), "lines"),
      label.r = ggplot2::unit(0.25, "lines"),
      label.size = 0, na.rm = TRUE
    ) +
    ggplot2::coord_polar(theta = "x", start = 0, clip = "off") +
    ggplot2::scale_y_continuous(
      limits = c(0, y_scale_top),
      breaks = major_at,
      expand = c(0, 0)
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq_len(n_oc),
      labels = outcome_labels,
      limits = c(0.5, n_oc + 0.5),
      expand = c(0, 0)
    ) +
    ggplot2::facet_wrap(~ treatment, ncol = ncol) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      axis.text.x        = ggplot2::element_blank(),
      axis.text.y        = ggplot2::element_blank(),
      axis.ticks         = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.background   = ggplot2::element_rect(fill = "white", colour = NA),
      plot.background    = ggplot2::element_rect(fill = "white", colour = NA),
      strip.text         = ggplot2::element_text(
        size = 10, face = "bold", colour = "white",
        margin = ggplot2::margin(t = 4, r = 4, b = 4, l = 4, unit = "pt")
      ),
      strip.background   = ggplot2::element_rect(
        fill = strip_color, colour = strip_color, linewidth = 0,
        inherit.blank = FALSE
      ),
      panel.spacing      = ggplot2::unit(0.1, "cm"),
      plot.margin        = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), "cm")
    )

  # ---- 16. Save or interactive view (same as vitruvian()) ----
  if (!is.null(file)) {
    ext <- tolower(tools::file_ext(file))
    if (!ext %in% c("pdf", "png", "svg"))
      stop("Unsupported file extension: ", ext, ". Use .pdf, .png, or .svg")
    ggplot2::ggsave(file, plot = p, width = width, height = height,
                    units = "in", device = ext)
    message("Saved Vitruvian EGO plot: ", file)
    return(invisible(p))
  }

  if (interactive()) {
    tmp <- tempfile(fileext = ".png")
    ggplot2::ggsave(tmp, plot = p, width = width, height = height,
                    units = "in", dpi = 150)
    viewer <- getOption("viewer")
    if (!is.null(viewer)) viewer(tmp) else utils::browseURL(tmp)
    return(invisible(p))
  }

  p
}
