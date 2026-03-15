#' Read a point cloud file
#'
#' Reads LAS, LAZ, PLY, or XYZ files and returns the appropriate
#' platform-specific subtype based on the \code{platform} argument.
#'
#' @param path Character string — path to the point cloud file.
#' @param platform Character string: "unknown", "aerial", "terrestrial", or "uav".
#'   Controls which subtype is returned and which default parameters apply
#'   to subsequent processing.
#' @param subsample Numeric between 0 and 1, or \code{NULL}. If provided,
#'   randomly subsample to this fraction of points.
#'
#' @return A \code{px_cloud} (or platform subtype).
#'
#' @examples
#' \dontrun{
#' cloud <- px_read("survey.laz", platform = "aerial")
#' scan <- px_read("bridge.laz", platform = "terrestrial")
#' }
#'
#' @export
px_read <- function(path, platform = "unknown", subsample = NULL) {
  path <- .validate_scalar_character(path, "path")
  platform <- .validate_platform(platform)

  if (!file.exists(path)) {
    rlang::abort(sprintf("File does not exist: %s", path))
  }

  ext <- tolower(tools::file_ext(path))

  cloud <- switch(
    ext,
    xyz = .read_xyz(path, platform = platform),
    txt = .read_xyz(path, platform = platform),
    csv = .read_xyz(path, platform = platform),
    las = .read_las(path, platform = platform),
    laz = .read_las(path, platform = platform),
    rlang::abort(sprintf("Unsupported point cloud format: .%s", ext))
  )

  if (!is.null(subsample)) {
    subsample <- .validate_scalar_numeric(subsample, "subsample")

    if (subsample <= 0 || subsample > 1) {
      rlang::abort("`subsample` must be > 0 and <= 1.")
    }

    cloud <- px_random_downsample(cloud, fraction = subsample)
  }

  cloud
}


#' Write a point cloud to file
#'
#' @param cloud A \code{px_cloud} object.
#' @param path Character string — output file path. Format inferred from extension.
#' @param compress Logical or \code{NULL}. For LAS output, whether to compress
#'   to LAZ. If \code{NULL}, inferred from extension.
#'
#' @return Invisibly returns the output path.
#'
#' @examples
#' \dontrun{
#' px_write(cloud, "output.laz")
#' }
#'
#' @export
px_write <- function(cloud, path, compress = NULL) {
  .assert_px_cloud(cloud)
  path <- .validate_scalar_character(path, "path")
  ext <- tolower(tools::file_ext(path))

  switch(
    ext,
    xyz = .write_xyz(cloud, path),
    txt = .write_xyz(cloud, path),
    csv = .write_xyz(cloud, path),
    las = .write_las(cloud, path),
    laz = .write_las(cloud, path),
    rlang::abort(sprintf("Unsupported point cloud format: .%s", ext))
  )

  invisible(path)
}

.read_xyz <- function(path, platform = "unknown") {
  ext <- tolower(tools::file_ext(path))
  sep <- if (ext == "csv") "," else ""

  data <- utils::read.table(
    file = path,
    header = FALSE,
    sep = sep,
    stringsAsFactors = FALSE
  )

  if (ncol(data) < 3L) {
    rlang::abort("XYZ files must contain at least 3 columns.")
  }

  xyz <- as.matrix(data[, 1:3, drop = FALSE])
  constructor <- .platform_constructor(platform)
  do.call(constructor, list(xyz = xyz))
}

.write_xyz <- function(cloud, path) {
  utils::write.table(
    cloud$xyz,
    file = path,
    sep = if (tolower(tools::file_ext(path)) == "csv") "," else " ",
    row.names = FALSE,
    col.names = FALSE
  )
}

.read_las <- function(path, platform = "unknown") {
  rlang::check_installed("lidR")
  las <- lidR::readLAS(path)

  if (lidR::is.empty(las)) {
    rlang::abort(sprintf("No points could be read from %s.", path))
  }

  from_lidr(las, platform = platform)
}

.write_las <- function(cloud, path) {
  rlang::check_installed("lidR")
  las <- to_lidr(cloud)

  if (tolower(tools::file_ext(path)) == "laz") {
    rlang::check_installed("rlas")
  }

  lidR::writeLAS(las, path)
}
