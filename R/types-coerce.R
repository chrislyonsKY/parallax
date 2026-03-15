#' Convert an object to a parallax point cloud
#'
#' Generic converter. Methods exist for matrices, data frames, and lidR LAS objects.
#'
#' @param x Object to convert.
#' @param platform Character string: "unknown", "aerial", "terrestrial", or "uav".
#' @param ... Additional arguments passed to the specific method.
#'
#' @return A \code{px_cloud} (or platform subtype).
#'
#' @export
as_px_cloud <- function(x, platform = "unknown", ...) {
  constructor <- .platform_constructor(platform)

  if (inherits(x, "px_cloud")) {
    if (identical(platform, "unknown")) {
      return(x)
    }

    return(do.call(
      constructor,
      c(
        list(
          xyz = x$xyz,
          intensity = x$intensity,
          classification = x$classification,
          rgb = x$rgb,
          normals = x$normals,
          return_number = x$return_number,
          number_of_returns = x$number_of_returns,
          metadata = utils::modifyList(x$metadata, list(platform = platform))
        ),
        list(...)
      )
    ))
  }

  if (is.matrix(x)) {
    return(do.call(constructor, c(list(xyz = x), list(...))))
  }

  if (is.data.frame(x)) {
    lower_names <- tolower(names(x))
    xyz_cols <- match(c("x", "y", "z"), lower_names)

    if (anyNA(xyz_cols)) {
      rlang::abort("Data frames must contain `x`, `y`, and `z` columns.")
    }

    xyz <- as.matrix(x[, xyz_cols, drop = FALSE])
    extras <- list()

    if ("intensity" %in% lower_names) {
      extras$intensity <- x[[which(lower_names == "intensity")[1L]]]
    }

    if ("classification" %in% lower_names) {
      extras$classification <- x[[which(lower_names == "classification")[1L]]]
    }

    return(do.call(constructor, c(list(xyz = xyz), extras, list(...))))
  }

  if (inherits(x, "LAS")) {
    return(from_lidr(x, platform = platform, ...))
  }

  rlang::abort("`x` must be a matrix, data frame, `LAS`, or `px_cloud`.")
}


#' Convert a lidR LAS object to parallax
#'
#' @param las A \code{LAS} object from the \code{lidR} package.
#' @param platform Character string: "aerial", "terrestrial", "uav", or "unknown".
#'
#' @return A \code{px_cloud} (or platform subtype).
#'
#' @export
from_lidr <- function(las, platform = "aerial") {
  rlang::check_installed("lidR")

  if (!inherits(las, "LAS")) {
    rlang::abort("`las` must be a `lidR::LAS` object.")
  }

  data <- las@data
  xyz <- as.matrix(data[, c("X", "Y", "Z"), drop = FALSE])

  metadata <- list(
    platform = .validate_platform(platform),
    source = "lidR"
  )

  constructor <- .platform_constructor(platform)

  do.call(
    constructor,
    list(
      xyz = xyz,
      intensity = data[["Intensity"]] %||% NULL,
      classification = data[["Classification"]] %||% NULL,
      return_number = data[["ReturnNumber"]] %||% NULL,
      number_of_returns = data[["NumberOfReturns"]] %||% NULL,
      metadata = metadata
    )
  )
}


#' Convert a parallax point cloud to lidR LAS
#'
#' @param cloud A \code{px_cloud} object.
#'
#' @return A \code{LAS} object.
#'
#' @export
to_lidr <- function(cloud) {
  .assert_px_cloud(cloud)
  rlang::check_installed("lidR")

  data <- data.frame(
    X = cloud$xyz[, 1L],
    Y = cloud$xyz[, 2L],
    Z = cloud$xyz[, 3L]
  )

  if (!is.null(cloud$intensity)) {
    data$Intensity <- cloud$intensity
  }

  if (!is.null(cloud$classification)) {
    data$Classification <- cloud$classification
  }

  if (!is.null(cloud$return_number)) {
    data$ReturnNumber <- cloud$return_number
  }

  if (!is.null(cloud$number_of_returns)) {
    data$NumberOfReturns <- cloud$number_of_returns
  }

  lidR::LAS(data)
}
