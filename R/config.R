#' Platform default parameters
#'
#' Returns a named list of default processing parameters for a given platform.
#' Used internally by px_classify_ground, px_segment_trees, and other
#' platform-aware functions.
#'
#' @param platform Character: "aerial", "terrestrial", "uav", or "unknown".
#'
#' @return Named list of defaults.
#'
#' @keywords internal
.platform_defaults <- function(platform) {
  switch(platform,
    aerial = list(
      ground_algorithm = "csf",
      ground_cloth_resolution = 2.0,
      ground_max_angle = 15.0,
      sor_k = 20L,
      sor_std_ratio = 2.0,
      voxel_size = 0.1
    ),
    terrestrial = list(
      ground_algorithm = "csf",
      ground_cloth_resolution = 0.5,
      ground_max_angle = 25.0,
      sor_k = 30L,
      sor_std_ratio = 1.5,
      voxel_size = 0.02
    ),
    uav = list(
      ground_algorithm = "csf",
      ground_cloth_resolution = 1.5,
      ground_max_angle = 20.0,
      sor_k = 20L,
      sor_std_ratio = 2.0,
      voxel_size = 0.05
    ),
    # unknown / default
    list(
      ground_algorithm = "csf",
      ground_cloth_resolution = 2.0,
      ground_max_angle = 15.0,
      sor_k = 20L,
      sor_std_ratio = 2.0,
      voxel_size = 0.05
    )
  )
}

.parallax_voxel_downsample <- function(xyz, voxel_size) {
  tryCatch(
    .Call("wrap__parallax_voxel_downsample", xyz, voxel_size, PACKAGE = "parallax"),
    error = function(cnd) NULL
  )
}

.parallax_sor <- function(xyz, k, std_ratio) {
  tryCatch(
    .Call("wrap__parallax_sor", xyz, k, std_ratio, PACKAGE = "parallax"),
    error = function(cnd) NULL
  )
}

.parallax_radius_outlier <- function(xyz, radius, min_neighbors) {
  tryCatch(
    .Call("wrap__parallax_radius_outlier", xyz, radius, min_neighbors, PACKAGE = "parallax"),
    error = function(cnd) NULL
  )
}

.parallax_estimate_normals <- function(xyz, k_neighbors, radius) {
  tryCatch(
    .Call("wrap__parallax_estimate_normals", xyz, k_neighbors, radius, PACKAGE = "parallax"),
    error = function(cnd) NULL
  )
}

.parallax_dbscan <- function(xyz, eps, min_points) {
  tryCatch(
    .Call("wrap__parallax_dbscan", xyz, eps, min_points, PACKAGE = "parallax"),
    error = function(cnd) NULL
  )
}
