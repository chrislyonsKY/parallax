#' Print a parallax point cloud
#'
#' @param x A \code{px_cloud} object.
#' @param ... Ignored.
#'
#' @return Invisibly returns \code{x}.
#'
#' @export
print.px_cloud <- function(x, ...) {
  .assert_px_cloud(x, "x")

  n_points <- nrow(x$xyz)
  z_range <- if (n_points > 0L) range(x$xyz[, 3L]) else c(NA_real_, NA_real_)

  cli::cli_h1("parallax point cloud")
  cli::cli_li("Class: {.val {paste(class(x), collapse = ', ')}}")
  cli::cli_li("Platform: {.val {x$metadata$platform %||% 'unknown'}}")
  cli::cli_li("Points: {format(n_points, big.mark = ',')}")
  cli::cli_li("Z range: {format(z_range[1], digits = 4)} to {format(z_range[2], digits = 4)}")
  cli::cli_li("Normals: {if (!is.null(x$normals)) 'present' else 'absent'}")

  invisible(x)
}

#' @export
print.px_aerial <- function(x, ...) {
  NextMethod()

  if (!is.null(x$metadata$flight_altitude_m)) {
    cli::cli_li("Flight altitude (m): {format(x$metadata$flight_altitude_m, digits = 4)}")
  }

  invisible(x)
}

#' @export
print.px_terrestrial <- function(x, ...) {
  NextMethod()
  scan_positions <- x$metadata$scan_positions %||% list()
  cli::cli_li("Scan positions: {length(scan_positions)}")
  invisible(x)
}

#' @export
print.px_uav <- function(x, ...) {
  NextMethod()
  cli::cli_li("Photogrammetric: {isTRUE(x$metadata$is_photogrammetric)}")
  invisible(x)
}

#' @export
print.icp_result <- function(x, ...) {
  cli::cli_h1("ICP registration result")
  cli::cli_li("Converged: {isTRUE(x$converged)}")
  cli::cli_li("Fitness: {format(x$fitness, digits = 4)}")
  cli::cli_li("Inlier RMSE: {format(x$inlier_rmse, digits = 4)}")
  cli::cli_li("Iterations: {x$iterations}")
  invisible(x)
}

#' @export
print.ground_result <- function(x, ...) {
  cli::cli_h1("Ground classification result")
  cli::cli_li("Algorithm: {.val {x$algorithm}}")
  cli::cli_li("Ground points: {x$n_ground}")
  cli::cli_li("Non-ground points: {x$n_nonground}")
  invisible(x)
}

#' @export
print.segmentation_result <- function(x, ...) {
  cli::cli_h1("Segmentation result")
  cli::cli_li("Segments: {x$n_segments}")
  cli::cli_li("Noise points: {sum(x$labels < 0L, na.rm = TRUE)}")
  invisible(x)
}

#' @export
print.mesh_result <- function(x, ...) {
  cli::cli_h1("Mesh reconstruction result")
  cli::cli_li("Vertices: {x$n_vertices}")
  cli::cli_li("Faces: {x$n_faces}")
  invisible(x)
}
