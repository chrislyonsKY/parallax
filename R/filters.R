#' Voxel grid downsampling
#'
#' @param cloud A \code{px_cloud}.
#' @param voxel_size Numeric — voxel edge length in coordinate units.
#'
#' @return A \code{px_cloud} — downsampled.
#'
#' @export
px_voxel_downsample <- function(cloud, voxel_size = 0.05) {
  .assert_px_cloud(cloud)
  voxel_size <- .validate_scalar_numeric(voxel_size, "voxel_size")

  if (voxel_size <= 0) {
    rlang::abort("`voxel_size` must be greater than 0.")
  }

  if (nrow(cloud$xyz) == 0L) {
    return(cloud)
  }

  native <- .parallax_call("wrap__parallax_voxel_downsample", cloud$xyz, voxel_size)
  if (is.list(native) && !is.null(native$xyz) && !is.null(native$index)) {
    out <- .subset_px_cloud(cloud, native$index)
    out$xyz <- unname(native$xyz)

    if (!is.null(out$normals)) {
      out$normals <- t(apply(out$normals, 1L, .normalize_vector))
    }

    return(out)
  }

  mins <- apply(cloud$xyz, 2L, min)
  voxel_index <- floor((cloud$xyz - matrix(mins, nrow(cloud$xyz), 3L, byrow = TRUE)) / voxel_size)
  groups <- split(seq_len(nrow(cloud$xyz)), interaction(voxel_index[, 1L], voxel_index[, 2L], voxel_index[, 3L], drop = TRUE))

  xyz <- do.call(
    rbind,
    lapply(groups, function(idx) colMeans(cloud$xyz[idx, , drop = FALSE]))
  )

  out <- .subset_px_cloud(cloud, vapply(groups, function(idx) idx[1L], integer(1)))
  out$xyz <- unname(xyz)

  if (!is.null(cloud$intensity)) {
    out$intensity <- unname(vapply(groups, function(idx) mean(cloud$intensity[idx]), numeric(1)))
  }

  if (!is.null(cloud$classification)) {
    out$classification <- unname(vapply(groups, function(idx) .majority_value(cloud$classification[idx]), integer(1)))
  }

  if (!is.null(cloud$rgb)) {
    out$rgb <- t(vapply(groups, function(idx) colMeans(cloud$rgb[idx, , drop = FALSE]), numeric(3)))
    storage.mode(out$rgb) <- "integer"
  }

  if (!is.null(cloud$normals)) {
    out$normals <- t(vapply(groups, function(idx) .normalize_vector(colMeans(cloud$normals[idx, , drop = FALSE])), numeric(3)))
  }

  if (!is.null(cloud$return_number)) {
    out$return_number <- unname(vapply(groups, function(idx) .majority_value(cloud$return_number[idx]), integer(1)))
  }

  if (!is.null(cloud$number_of_returns)) {
    out$number_of_returns <- unname(vapply(groups, function(idx) .majority_value(cloud$number_of_returns[idx]), integer(1)))
  }

  out
}


#' Statistical outlier removal
#'
#' Removes points whose mean distance to k nearest neighbors exceeds
#' the global threshold (mean + std_ratio * std).
#'
#' @param cloud A \code{px_cloud}.
#' @param k Integer — number of nearest neighbors.
#' @param std_ratio Numeric — standard deviation multiplier.
#'
#' @return A \code{px_cloud} — filtered (outliers removed).
#'
#' @export
px_filter_statistical <- function(cloud, k = 20L, std_ratio = 2.0) {
  .assert_px_cloud(cloud)
  k <- .validate_neighbor_count(k, "k")
  std_ratio <- .validate_scalar_numeric(std_ratio, "std_ratio")

  n_points <- nrow(cloud$xyz)
  if (n_points <= 1L) {
    return(cloud)
  }

  native_keep <- .parallax_call("wrap__parallax_sor", cloud$xyz, as.integer(k), std_ratio)
  keep <- if (is.integer(native_keep)) {
    as.integer(native_keep)
  } else {
    k <- min(k, n_points - 1L)
    dmat <- .pairwise_distances(cloud$xyz)
    sorted <- t(apply(dmat, 1L, sort, partial = seq_len(k + 1L)))
    mean_neighbor_distance <- rowMeans(sorted[, seq.int(2L, k + 1L), drop = FALSE])
    threshold <- mean(mean_neighbor_distance) + stats::sd(mean_neighbor_distance) * std_ratio
    which(mean_neighbor_distance <= threshold)
  }

  .subset_px_cloud(cloud, keep)
}


#' Radius outlier removal
#'
#' Removes points with fewer than \code{min_neighbors} within \code{radius}.
#'
#' @param cloud A \code{px_cloud}.
#' @param radius Numeric — search radius.
#' @param min_neighbors Integer — minimum neighbor count to keep.
#'
#' @return A \code{px_cloud} — filtered.
#'
#' @export
px_filter_radius <- function(cloud, radius = 0.5, min_neighbors = 5L) {
  .assert_px_cloud(cloud)
  radius <- .validate_scalar_numeric(radius, "radius")
  min_neighbors <- .validate_neighbor_count(min_neighbors, "min_neighbors")

  if (radius <= 0) {
    rlang::abort("`radius` must be greater than 0.")
  }

  if (nrow(cloud$xyz) == 0L) {
    return(cloud)
  }

  native_keep <- .parallax_call("wrap__parallax_radius_outlier", cloud$xyz, radius, as.integer(min_neighbors))
  keep <- if (is.integer(native_keep)) {
    as.integer(native_keep)
  } else {
    dmat <- .pairwise_distances(cloud$xyz)
    neighbor_counts <- rowSums(dmat <= radius) - 1L
    which(neighbor_counts >= min_neighbors)
  }

  .subset_px_cloud(cloud, keep)
}


#' Crop to bounding box
#'
#' @param cloud A \code{px_cloud}.
#' @param bbox Numeric vector of length 6: (xmin, ymin, zmin, xmax, ymax, zmax).
#'
#' @return A \code{px_cloud} — cropped.
#'
#' @export
px_crop <- function(cloud, bbox) {
  .assert_px_cloud(cloud)

  if (!is.numeric(bbox) || length(bbox) != 6L || any(!is.finite(bbox))) {
    rlang::abort("`bbox` must be a numeric vector of length 6.")
  }

  if (bbox[1L] > bbox[4L] || bbox[2L] > bbox[5L] || bbox[3L] > bbox[6L]) {
    rlang::abort("`bbox` must be ordered as xmin, ymin, zmin, xmax, ymax, zmax.")
  }

  keep <- which(
    cloud$xyz[, 1L] >= bbox[1L] &
      cloud$xyz[, 1L] <= bbox[4L] &
      cloud$xyz[, 2L] >= bbox[2L] &
      cloud$xyz[, 2L] <= bbox[5L] &
      cloud$xyz[, 3L] >= bbox[3L] &
      cloud$xyz[, 3L] <= bbox[6L]
  )

  .subset_px_cloud(cloud, keep)
}


#' Random downsampling
#'
#' @param cloud A \code{px_cloud}.
#' @param fraction Numeric between 0 and 1 — fraction to keep.
#' @param seed Integer or \code{NULL} — random seed.
#'
#' @return A \code{px_cloud} — downsampled.
#'
#' @export
px_random_downsample <- function(cloud, fraction = 0.5, seed = NULL) {
  .assert_px_cloud(cloud)
  fraction <- .validate_scalar_numeric(fraction, "fraction")

  if (fraction < 0 || fraction > 1) {
    rlang::abort("`fraction` must be between 0 and 1.")
  }

  if (!is.null(seed)) {
    seed <- .validate_neighbor_count(seed, "seed")
    old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
    on.exit({
      if (is.null(old_seed)) {
        rm(".Random.seed", envir = .GlobalEnv)
      } else {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      }
    }, add = TRUE)
    set.seed(seed)
  }

  n_points <- nrow(cloud$xyz)
  n_keep <- floor(n_points * fraction)

  if (fraction > 0 && n_keep == 0L && n_points > 0L) {
    n_keep <- 1L
  }

  keep <- if (n_keep == 0L) integer() else sort(sample.int(n_points, size = n_keep, replace = FALSE))
  .subset_px_cloud(cloud, keep)
}

.pairwise_distances <- function(xyz) {
  as.matrix(stats::dist(xyz))
}

.validate_neighbor_count <- function(x, arg) {
  x <- .validate_scalar_numeric(x, arg)

  if (x < 1 || x != as.integer(x)) {
    rlang::abort(sprintf("`%s` must be a positive integer.", arg))
  }

  as.integer(x)
}

.majority_value <- function(x) {
  tab <- table(x)
  as.integer(names(tab)[which.max(tab)])
}

.normalize_vector <- function(x) {
  norm <- sqrt(sum(x ^ 2))
  if (norm == 0) {
    return(rep(0, length(x)))
  }

  as.numeric(x / norm)
}
