#' Compute point cloud metrics
#'
#' Summary statistics including point count, density, elevation distribution,
#' and intensity statistics.
#'
#' @param cloud A \code{px_cloud}.
#'
#' @return A tibble with one row containing summary statistics.
#'
#' @export
px_metrics <- function(cloud) {
  .assert_px_cloud(cloud)

  n_points <- nrow(cloud$xyz)

  if (n_points == 0L) {
    return(tibble::tibble(
      n_points = 0L,
      x_min = NA_real_,
      x_max = NA_real_,
      y_min = NA_real_,
      y_max = NA_real_,
      z_min = NA_real_,
      z_max = NA_real_,
      x_mean = NA_real_,
      y_mean = NA_real_,
      z_mean = NA_real_,
      z_sd = NA_real_,
      z_q05 = NA_real_,
      z_q25 = NA_real_,
      z_q50 = NA_real_,
      z_q75 = NA_real_,
      z_q95 = NA_real_,
      point_density = NA_real_,
      intensity_mean = NA_real_
    ))
  }

  bounds <- apply(cloud$xyz, 2L, range)
  centroid <- colMeans(cloud$xyz)
  xy_area <- diff(bounds[, 1L]) * diff(bounds[, 2L])
  point_density <- if (xy_area <= 0) NA_real_ else n_points / xy_area
  z_quantiles <- stats::quantile(cloud$xyz[, 3L], probs = c(0.05, 0.25, 0.5, 0.75, 0.95), names = FALSE)

  tibble::tibble(
    n_points = n_points,
    x_min = bounds[1L, 1L],
    x_max = bounds[2L, 1L],
    y_min = bounds[1L, 2L],
    y_max = bounds[2L, 2L],
    z_min = bounds[1L, 3L],
    z_max = bounds[2L, 3L],
    x_mean = centroid[1L],
    y_mean = centroid[2L],
    z_mean = centroid[3L],
    z_sd = stats::sd(cloud$xyz[, 3L]),
    z_q05 = z_quantiles[1L],
    z_q25 = z_quantiles[2L],
    z_q50 = z_quantiles[3L],
    z_q75 = z_quantiles[4L],
    z_q95 = z_quantiles[5L],
    point_density = point_density,
    intensity_mean = if (is.null(cloud$intensity)) NA_real_ else mean(cloud$intensity)
  )
}


#' Generate a canopy height model
#'
#' Creates a gridded CHM from classified aerial or UAV point clouds.
#'
#' @param cloud A \code{px_cloud} with ground classification.
#' @param resolution Numeric — grid cell size in meters.
#' @param ground_class Integer — ASPRS code for ground (default 2).
#'
#' @return A list with components:
#'   \describe{
#'     \item{chm}{Numeric matrix — height values}
#'     \item{x_edges}{Numeric vector — x bin edges}
#'     \item{y_edges}{Numeric vector — y bin edges}
#'     \item{resolution}{Numeric — cell size used}
#'   }
#'
#' @export
px_canopy_height_model <- function(cloud, resolution = 1.0, ground_class = 2L) {
  .assert_px_cloud(cloud)
  resolution <- .validate_scalar_numeric(resolution, "resolution")
  ground_class <- .validate_neighbor_count(ground_class, "ground_class")

  if (is.null(cloud$classification)) {
    rlang::abort("`cloud` must contain `classification` values for CHM generation.")
  }

  grid <- .grid_index(cloud$xyz, resolution)
  ground_mask <- cloud$classification == ground_class
  canopy_mask <- !ground_mask

  ground_z <- .aggregate_grid_stat(grid, cloud$xyz[, 3L], ground_mask, min)
  canopy_z <- .aggregate_grid_stat(grid, cloud$xyz[, 3L], canopy_mask, max)
  chm <- canopy_z - ground_z

  list(
    chm = chm,
    x_edges = grid$x_edges,
    y_edges = grid$y_edges,
    resolution = resolution
  )
}


#' Generate a point density map
#'
#' @param cloud A \code{px_cloud}.
#' @param resolution Numeric — grid cell size in meters.
#'
#' @return A list with components:
#'   \describe{
#'     \item{density}{Numeric matrix — points per cell}
#'     \item{x_edges}{Numeric vector}
#'     \item{y_edges}{Numeric vector}
#'   }
#'
#' @export
px_density_map <- function(cloud, resolution = 1.0) {
  .assert_px_cloud(cloud)
  resolution <- .validate_scalar_numeric(resolution, "resolution")

  grid <- .grid_index(cloud$xyz, resolution)
  density <- matrix(0, nrow = length(grid$y_edges) - 1L, ncol = length(grid$x_edges) - 1L)

  for (i in seq_len(nrow(cloud$xyz))) {
    density[grid$y_bin[i], grid$x_bin[i]] <- density[grid$y_bin[i], grid$x_bin[i]] + 1
  }

  list(
    density = density,
    x_edges = grid$x_edges,
    y_edges = grid$y_edges
  )
}

.grid_index <- function(xyz, resolution) {
  if (resolution <= 0) {
    rlang::abort("`resolution` must be greater than 0.")
  }

  if (nrow(xyz) == 0L) {
    x_edges <- c(0, resolution)
    y_edges <- c(0, resolution)

    return(list(
      x_edges = x_edges,
      y_edges = y_edges,
      x_bin = integer(),
      y_bin = integer()
    ))
  }

  x_range <- range(xyz[, 1L])
  y_range <- range(xyz[, 2L])
  x_edges <- seq(x_range[1L], x_range[2L] + resolution, by = resolution)
  y_edges <- seq(y_range[1L], y_range[2L] + resolution, by = resolution)

  if (length(x_edges) < 2L) {
    x_edges <- c(x_range[1L], x_range[1L] + resolution)
  }

  if (length(y_edges) < 2L) {
    y_edges <- c(y_range[1L], y_range[1L] + resolution)
  }

  x_bin <- pmin(findInterval(xyz[, 1L], x_edges, all.inside = TRUE), length(x_edges) - 1L)
  y_bin <- pmin(findInterval(xyz[, 2L], y_edges, all.inside = TRUE), length(y_edges) - 1L)

  list(
    x_edges = x_edges,
    y_edges = y_edges,
    x_bin = x_bin,
    y_bin = y_bin
  )
}

.aggregate_grid_stat <- function(grid, values, mask, fn) {
  out <- matrix(NA_real_, nrow = length(grid$y_edges) - 1L, ncol = length(grid$x_edges) - 1L)

  if (!any(mask)) {
    return(out)
  }

  idx <- which(mask)
  split_idx <- split(idx, paste(grid$y_bin[idx], grid$x_bin[idx], sep = ":"))

  for (key in names(split_idx)) {
    bins <- as.integer(strsplit(key, ":", fixed = TRUE)[[1L]])
    out[bins[1L], bins[2L]] <- fn(values[split_idx[[key]]])
  }

  out
}
