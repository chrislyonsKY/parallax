#' Create a point cloud object
#'
#' Base constructor for a platform-agnostic point cloud. Use
#' \code{\link{px_aerial}}, \code{\link{px_terrestrial}}, or
#' \code{\link{px_uav}} for platform-specific subtypes with smart defaults.
#'
#' @param xyz Numeric matrix with 3 columns (x, y, z).
#' @param intensity Numeric vector of intensity values, or \code{NULL}.
#' @param classification Integer vector of ASPRS classification codes, or \code{NULL}.
#' @param rgb Integer matrix with 3 columns (R, G, B), or \code{NULL}.
#' @param normals Numeric matrix with 3 columns (nx, ny, nz), or \code{NULL}.
#' @param return_number Integer vector of return numbers, or \code{NULL}.
#' @param number_of_returns Integer vector of total returns, or \code{NULL}.
#' @param metadata Named list of acquisition metadata.
#'
#' @return An object of class \code{px_cloud}.
#'
#' @examples
#' xyz <- matrix(rnorm(300), ncol = 3)
#' cloud <- px_cloud(xyz)
#' print(cloud)
#'
#' @export
px_cloud <- function(xyz,
                     intensity = NULL,
                     classification = NULL,
                     rgb = NULL,
                     normals = NULL,
                     return_number = NULL,
                     number_of_returns = NULL,
                     metadata = list()) {
  xyz <- .validate_xyz_matrix(xyz)
  n_points <- nrow(xyz)

  intensity <- .validate_optional_vector(intensity, n_points, "intensity", mode = "numeric")
  classification <- .validate_optional_vector(classification, n_points, "classification", mode = "integer")
  rgb <- .validate_optional_matrix(rgb, n_points, "rgb", mode = "integer")
  normals <- .validate_optional_matrix(normals, n_points, "normals", mode = "numeric")
  return_number <- .validate_optional_vector(return_number, n_points, "return_number", mode = "integer")
  number_of_returns <- .validate_optional_vector(number_of_returns, n_points, "number_of_returns", mode = "integer")

  metadata <- .validate_metadata(metadata)
  metadata$platform <- .validate_platform(metadata$platform %||% "unknown")

  structure(
    list(
      xyz = xyz,
      intensity = intensity,
      classification = classification,
      rgb = rgb,
      normals = normals,
      return_number = return_number,
      number_of_returns = number_of_returns,
      metadata = metadata
    ),
    class = "px_cloud"
  )
}


#' Create an aerial LiDAR point cloud
#'
#' Constructor for aerial LiDAR data. Sets platform to "aerial" and provides
#' appropriate defaults for ground classification, tree segmentation, and
#' metrics computation.
#'
#' @inheritParams px_cloud
#' @param flight_altitude_m Numeric scalar, flight altitude in meters, or \code{NULL}.
#'
#' @return An object of class \code{c("px_aerial", "px_cloud")}.
#'
#' @examples
#' xyz <- matrix(rnorm(300), ncol = 3)
#' cloud <- px_aerial(xyz)
#'
#' @export
px_aerial <- function(xyz,
                      intensity = NULL,
                      classification = NULL,
                      rgb = NULL,
                      normals = NULL,
                      return_number = NULL,
                      number_of_returns = NULL,
                      flight_altitude_m = NULL,
                      metadata = list()) {
  metadata <- .validate_metadata(metadata)
  metadata$platform <- "aerial"

  if (!is.null(flight_altitude_m)) {
    metadata$flight_altitude_m <- .validate_scalar_numeric(flight_altitude_m, "flight_altitude_m")
  }

  cloud <- px_cloud(
    xyz = xyz,
    intensity = intensity,
    classification = classification,
    rgb = rgb,
    normals = normals,
    return_number = return_number,
    number_of_returns = number_of_returns,
    metadata = metadata
  )

  class(cloud) <- c("px_aerial", class(cloud))
  cloud
}


#' Create a terrestrial laser scanning point cloud
#'
#' Constructor for TLS data. Sets platform to "terrestrial" and provides
#' appropriate defaults for registration, ground classification, and
#' feature extraction. Scan positions are first-class metadata.
#'
#' @inheritParams px_cloud
#' @param scan_positions List of \code{px_scan_position} objects, or \code{NULL}.
#'
#' @return An object of class \code{c("px_terrestrial", "px_cloud")}.
#'
#' @examples
#' xyz <- matrix(rnorm(300), ncol = 3)
#' pos <- px_scan_position(0, 0, 1.5, scan_id = "setup1")
#' cloud <- px_terrestrial(xyz, scan_positions = list(pos))
#'
#' @export
px_terrestrial <- function(xyz,
                           intensity = NULL,
                           classification = NULL,
                           rgb = NULL,
                           normals = NULL,
                           return_number = NULL,
                           number_of_returns = NULL,
                           scan_positions = NULL,
                           metadata = list()) {
  metadata <- .validate_metadata(metadata)
  metadata$platform <- "terrestrial"

  if (!is.null(scan_positions)) {
    if (!is.list(scan_positions) || !all(vapply(scan_positions, inherits, logical(1), "px_scan_position"))) {
      rlang::abort("`scan_positions` must be a list of `px_scan_position` objects.")
    }

    metadata$scan_positions <- scan_positions
  }

  cloud <- px_cloud(
    xyz = xyz,
    intensity = intensity,
    classification = classification,
    rgb = rgb,
    normals = normals,
    return_number = return_number,
    number_of_returns = number_of_returns,
    metadata = metadata
  )

  class(cloud) <- c("px_terrestrial", class(cloud))
  cloud
}


#' Create a UAV point cloud
#'
#' Constructor for UAV LiDAR or photogrammetric point clouds. Sets platform
#' to "uav" with defaults appropriate for oblique perspectives and variable
#' point density.
#'
#' @inheritParams px_cloud
#' @param is_photogrammetric Logical, \code{TRUE} if derived from SfM/photogrammetry
#'   rather than LiDAR. Affects noise model defaults.
#'
#' @return An object of class \code{c("px_uav", "px_cloud")}.
#'
#' @examples
#' xyz <- matrix(rnorm(300), ncol = 3)
#' cloud <- px_uav(xyz, is_photogrammetric = TRUE)
#'
#' @export
px_uav <- function(xyz,
                   intensity = NULL,
                   classification = NULL,
                   rgb = NULL,
                   normals = NULL,
                   return_number = NULL,
                   number_of_returns = NULL,
                   is_photogrammetric = FALSE,
                   metadata = list()) {
  metadata <- .validate_metadata(metadata)
  metadata$platform <- "uav"
  metadata$is_photogrammetric <- .validate_scalar_logical(is_photogrammetric, "is_photogrammetric")

  cloud <- px_cloud(
    xyz = xyz,
    intensity = intensity,
    classification = classification,
    rgb = rgb,
    normals = normals,
    return_number = return_number,
    number_of_returns = number_of_returns,
    metadata = metadata
  )

  class(cloud) <- c("px_uav", class(cloud))
  cloud
}


#' Create a scan position object
#'
#' Represents the position and orientation of a scanner setup, primarily
#' used for terrestrial and mobile scanning.
#'
#' @param x,y,z Numeric scalars — scanner position coordinates.
#' @param roll,pitch,yaw Numeric scalars — scanner orientation in degrees.
#' @param scan_id Character string identifying this setup.
#'
#' @return An object of class \code{px_scan_position}.
#'
#' @export
px_scan_position <- function(x, y, z, roll = 0, pitch = 0, yaw = 0, scan_id = "") {
  scan_id <- .validate_scalar_character(scan_id, "scan_id")

  structure(
    list(
      x = .validate_scalar_numeric(x, "x"),
      y = .validate_scalar_numeric(y, "y"),
      z = .validate_scalar_numeric(z, "z"),
      roll = .validate_scalar_numeric(roll, "roll"),
      pitch = .validate_scalar_numeric(pitch, "pitch"),
      yaw = .validate_scalar_numeric(yaw, "yaw"),
      scan_id = scan_id
    ),
    class = "px_scan_position"
  )
}

.subset_px_cloud <- function(cloud, index) {
  .assert_px_cloud(cloud)

  index <- as.integer(index)
  if (length(index) == 0) {
    index <- integer()
  }

  out <- cloud
  out$xyz <- cloud$xyz[index, , drop = FALSE]

  for (field in c("intensity", "classification", "return_number", "number_of_returns")) {
    if (!is.null(cloud[[field]])) {
      out[[field]] <- cloud[[field]][index]
    }
  }

  for (field in c("rgb", "normals")) {
    if (!is.null(cloud[[field]])) {
      out[[field]] <- cloud[[field]][index, , drop = FALSE]
    }
  }

  out
}

.assert_px_cloud <- function(cloud, arg = "cloud") {
  if (!inherits(cloud, "px_cloud")) {
    rlang::abort(sprintf("`%s` must be a `px_cloud`.", arg))
  }
}

.validate_xyz_matrix <- function(xyz) {
  if (is.data.frame(xyz)) {
    xyz <- as.matrix(xyz)
  }

  if (!is.matrix(xyz) || !is.numeric(xyz) || ncol(xyz) != 3L) {
    rlang::abort("`xyz` must be a numeric matrix with 3 columns.")
  }

  if (any(!is.finite(xyz))) {
    rlang::abort("`xyz` must contain only finite values.")
  }

  unname(matrix(as.numeric(xyz), ncol = 3L, dimnames = list(NULL, colnames(xyz))))
}

.validate_optional_vector <- function(x, n_points, arg, mode = c("numeric", "integer")) {
  mode <- match.arg(mode)

  if (is.null(x)) {
    return(NULL)
  }

  if (!is.atomic(x) || length(dim(x)) > 1L || length(x) != n_points) {
    rlang::abort(sprintf("`%s` must be a vector with length %d.", arg, n_points))
  }

  if (mode == "numeric" && !is.numeric(x)) {
    rlang::abort(sprintf("`%s` must be numeric.", arg))
  }

  if (mode == "integer" && !is.numeric(x)) {
    rlang::abort(sprintf("`%s` must be integer-like numeric data.", arg))
  }

  if (any(!is.finite(x))) {
    rlang::abort(sprintf("`%s` must contain only finite values.", arg))
  }

  if (mode == "integer") {
    return(as.integer(x))
  }

  as.numeric(x)
}

.validate_optional_matrix <- function(x, n_points, arg, mode = c("numeric", "integer")) {
  mode <- match.arg(mode)

  if (is.null(x)) {
    return(NULL)
  }

  if (!is.matrix(x) || ncol(x) != 3L || nrow(x) != n_points) {
    rlang::abort(sprintf("`%s` must be a %d x 3 matrix.", arg, n_points))
  }

  if (mode == "numeric" && !is.numeric(x)) {
    rlang::abort(sprintf("`%s` must be numeric.", arg))
  }

  if (mode == "integer" && !is.numeric(x)) {
    rlang::abort(sprintf("`%s` must be integer-like numeric data.", arg))
  }

  if (any(!is.finite(x))) {
    rlang::abort(sprintf("`%s` must contain only finite values.", arg))
  }

  if (mode == "integer") {
    return(matrix(as.integer(x), ncol = 3L, dimnames = dimnames(x)))
  }

  matrix(as.numeric(x), ncol = 3L, dimnames = dimnames(x))
}

.validate_metadata <- function(metadata) {
  if (!is.list(metadata)) {
    rlang::abort("`metadata` must be a named list.")
  }

  if (length(metadata) > 0L) {
    metadata_names <- names(metadata)
    if (is.null(metadata_names) || any(is.na(metadata_names)) || any(metadata_names == "")) {
      rlang::abort("`metadata` must be a named list.")
    }
  }

  metadata
}

.validate_platform <- function(platform) {
  platform <- .validate_scalar_character(platform, "platform")
  valid <- c("unknown", "aerial", "terrestrial", "uav")

  if (!platform %in% valid) {
    rlang::abort("`platform` must be one of \"unknown\", \"aerial\", \"terrestrial\", or \"uav\".")
  }

  platform
}

.validate_scalar_numeric <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x)) {
    rlang::abort(sprintf("`%s` must be a single finite numeric value.", arg))
  }

  as.numeric(x)
}

.validate_scalar_character <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    rlang::abort(sprintf("`%s` must be a single character string.", arg))
  }

  x
}

.validate_scalar_logical <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    rlang::abort(sprintf("`%s` must be `TRUE` or `FALSE`.", arg))
  }

  x
}

.platform_constructor <- function(platform) {
  switch(
    .validate_platform(platform),
    aerial = px_aerial,
    terrestrial = px_terrestrial,
    uav = px_uav,
    unknown = px_cloud
  )
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
