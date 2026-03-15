#' Plot a parallax point cloud
#'
#' @param x A \code{px_cloud} object.
#' @param color_by Character — attribute to color by: "z", "intensity",
#'   "classification", "rgb", or "none".
#' @param point_size Numeric — rendering point size.
#' @param backend Character: "rgl" (interactive 3D) or "ggplot" (2D projection).
#' @param ... Additional arguments passed to the plotting backend.
#'
#' @return Invisibly returns the plot object (ggplot or rgl scene ID).
#'
#' @export
plot.px_cloud <- function(x, color_by = "z", point_size = 1, backend = "ggplot", ...) {
  .assert_px_cloud(x, "x")
  backend <- match.arg(backend, c("ggplot", "rgl"))
  point_size <- .validate_scalar_numeric(point_size, "point_size")

  if (point_size <= 0) {
    rlang::abort("`point_size` must be greater than 0.")
  }

  color_spec <- .plot_color_spec(x, color_by)

  if (backend == "ggplot") {
    rlang::check_installed("ggplot2")
    plot_data <- data.frame(
      x = x$xyz[, 1L],
      y = x$xyz[, 2L],
      color = color_spec$values
    )

    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = rlang::.data$x, y = rlang::.data$y, color = rlang::.data$color)
    ) +
      ggplot2::geom_point(size = point_size, alpha = 0.8) +
      ggplot2::coord_equal() +
      ggplot2::labs(x = "X", y = "Y", color = color_spec$label) +
      ggplot2::theme_minimal()

    if (identical(color_spec$scale, "identity")) {
      p <- p + ggplot2::scale_color_identity()
    } else if (identical(color_spec$scale, "continuous")) {
      p <- p + ggplot2::scale_color_viridis_c()
    }

    return(invisible(p))
  }

  .require_rgl()
  color_values <- .plot_color_vector(color_spec)
  scene <- .rgl_get("plot3d")(
    x$xyz[, 1L],
    x$xyz[, 2L],
    x$xyz[, 3L],
    col = color_values,
    size = point_size,
    type = "p",
    xlab = "X",
    ylab = "Y",
    zlab = "Z",
    ...
  )

  invisible(scene)
}


#' Visualize a registration result
#'
#' @param source A \code{px_cloud}.
#' @param target A \code{px_cloud}.
#' @param result An \code{icp_result}.
#'
#' @return Invisibly returns a ggplot object.
#'
#' @export
px_plot_registration <- function(source, target, result) {
  .assert_px_cloud(source, "source")
  .assert_px_cloud(target, "target")

  if (!is.list(result) || !is.matrix(result$transformation) || !identical(dim(result$transformation), c(4L, 4L))) {
    rlang::abort("`result` must contain a 4x4 `transformation` matrix.")
  }

  rlang::check_installed("ggplot2")

  transformed_source <- .apply_transform(source$xyz, result$transformation)
  plot_data <- rbind(
    data.frame(x = transformed_source[, 1L], y = transformed_source[, 2L], cloud = "source"),
    data.frame(x = target$xyz[, 1L], y = target$xyz[, 2L], cloud = "target")
  )

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = rlang::.data$x, y = rlang::.data$y, color = rlang::.data$cloud)
  ) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = "X", y = "Y", color = "Cloud") +
    ggplot2::theme_minimal()

  invisible(p)
}


#' Visualize segmentation results
#'
#' @param cloud A \code{px_cloud}.
#' @param result A \code{segmentation_result}.
#'
#' @return Invisibly returns a ggplot object.
#'
#' @export
px_plot_segmentation <- function(cloud, result) {
  .assert_px_cloud(cloud)

  if (!inherits(result, "segmentation_result") || length(result$labels) != nrow(cloud$xyz)) {
    rlang::abort("`result` must be a `segmentation_result` matching the cloud size.")
  }

  rlang::check_installed("ggplot2")

  labels <- ifelse(result$labels < 0L, "noise", paste0("segment_", result$labels))
  plot_data <- data.frame(
    x = cloud$xyz[, 1L],
    y = cloud$xyz[, 2L],
    segment = factor(labels)
  )

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = rlang::.data$x, y = rlang::.data$y, color = rlang::.data$segment)
  ) +
    ggplot2::geom_point(alpha = 0.8) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = "X", y = "Y", color = "Segment") +
    ggplot2::theme_minimal()

  invisible(p)
}

.plot_color_spec <- function(cloud, color_by) {
  color_by <- match.arg(color_by, c("z", "intensity", "classification", "rgb", "none"))

  switch(
    color_by,
    z = list(values = cloud$xyz[, 3L], scale = "continuous", label = "Z"),
    intensity = {
      if (is.null(cloud$intensity)) {
        rlang::abort("`cloud` does not contain intensity values.")
      }
      list(values = cloud$intensity, scale = "continuous", label = "Intensity")
    },
    classification = {
      if (is.null(cloud$classification)) {
        rlang::abort("`cloud` does not contain classification values.")
      }
      list(values = factor(cloud$classification), scale = "discrete", label = "Classification")
    },
    rgb = {
      if (is.null(cloud$rgb)) {
        rlang::abort("`cloud` does not contain RGB values.")
      }
      list(values = grDevices::rgb(cloud$rgb[, 1L], cloud$rgb[, 2L], cloud$rgb[, 3L], maxColorValue = 255), scale = "identity", label = "RGB")
    },
    none = list(values = rep("point cloud", nrow(cloud$xyz)), scale = "discrete", label = "Cloud")
  )
}

.plot_color_vector <- function(color_spec) {
  if (identical(color_spec$scale, "identity")) {
    return(color_spec$values)
  }

  if (identical(color_spec$scale, "continuous")) {
    palette <- grDevices::colorRampPalette(c("#1f5f8b", "#f4d35e", "#d1495b"))(64)
    bins <- cut(color_spec$values, breaks = 64, include.lowest = TRUE, labels = FALSE)
    return(palette[bins])
  }

  labels <- unique(as.character(color_spec$values))
  palette <- stats::setNames(grDevices::hcl.colors(length(labels), "Set 2"), labels)
  unname(palette[as.character(color_spec$values)])
}

.require_rgl <- function() {
  ok <- tryCatch(requireNamespace("rgl", quietly = TRUE), error = function(...) FALSE)
  if (!ok) {
    rlang::abort("The `rgl` package is required for `backend = \"rgl\"` and must be loadable on this system.")
  }
}

.rgl_get <- function(name) {
  getExportedValue("rgl", name)
}
