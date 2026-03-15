#' Detect planar surfaces
#'
#' Sequential RANSAC plane detection. Iteratively finds the largest plane,
#' removes inliers, and repeats.
#'
#' @param cloud A \code{px_cloud}.
#' @param distance_threshold Numeric — max distance to plane for inlier.
#' @param max_planes Integer — maximum planes to detect.
#' @param min_points Integer — minimum inliers for a valid plane.
#' @param num_iterations Integer — RANSAC iterations per plane.
#'
#' @return List of \code{plane_result} objects, each with:
#'   \describe{
#'     \item{equation}{Numeric vector (a, b, c, d) — plane equation}
#'     \item{normal}{Numeric vector (3) — plane normal}
#'     \item{inlier_mask}{Logical vector}
#'     \item{n_inliers}{Integer}
#'     \item{rmse}{Numeric — RMS inlier distance}
#'   }
#'
#' @export
px_detect_planes <- function(cloud,
                             distance_threshold = 0.02,
                             max_planes = 10L,
                             min_points = 100L,
                             num_iterations = 1000L) {
  .assert_px_cloud(cloud)
  distance_threshold <- .validate_scalar_numeric(distance_threshold, "distance_threshold")
  max_planes <- .validate_neighbor_count(max_planes, "max_planes")
  min_points <- .validate_neighbor_count(min_points, "min_points")
  num_iterations <- .validate_neighbor_count(num_iterations, "num_iterations")

  if (distance_threshold <= 0) {
    rlang::abort("`distance_threshold` must be greater than 0.")
  }

  remaining <- seq_len(nrow(cloud$xyz))
  planes <- list()

  while (length(planes) < max_planes && length(remaining) >= min_points && length(remaining) >= 3L) {
    points <- cloud$xyz[remaining, , drop = FALSE]
    best_inliers <- integer()
    best_plane <- NULL

    for (iter in seq_len(num_iterations)) {
      sample_idx <- sample.int(nrow(points), 3L, replace = FALSE)
      plane <- .plane_from_three_points(points[sample_idx, , drop = FALSE])

      if (is.null(plane)) {
        next
      }

      distances <- .point_plane_distances(points, plane$equation)
      inliers <- which(distances <= distance_threshold)

      if (length(inliers) > length(best_inliers)) {
        best_inliers <- inliers
        best_plane <- plane
      }
    }

    if (length(best_inliers) < min_points || is.null(best_plane)) {
      break
    }

    refined <- .fit_plane_pca(points[best_inliers, , drop = FALSE])
    distances <- .point_plane_distances(points, refined$equation)
    inliers <- which(distances <= distance_threshold)

    if (length(inliers) < min_points) {
      break
    }

    full_mask <- rep(FALSE, nrow(cloud$xyz))
    full_mask[remaining[inliers]] <- TRUE
    planes[[length(planes) + 1L]] <- structure(
      list(
        equation = refined$equation,
        normal = refined$normal,
        inlier_mask = full_mask,
        n_inliers = length(inliers),
        rmse = sqrt(mean(distances[inliers] ^ 2))
      ),
      class = "plane_result"
    )

    remaining <- remaining[-inliers]
  }

  planes
}


#' Detect cylindrical surfaces
#'
#' RANSAC cylinder detection. Useful for pipes, poles, tree trunks.
#'
#' @param cloud A \code{px_cloud} (normals recommended).
#' @param distance_threshold Numeric — max distance to cylinder surface.
#' @param radius_range Numeric vector of length 2 — min/max radius.
#' @param min_points Integer — minimum inliers.
#' @param num_iterations Integer — RANSAC iterations.
#'
#' @return List of \code{cylinder_result} objects, each with:
#'   \describe{
#'     \item{axis_point}{Numeric vector (3)}
#'     \item{axis_direction}{Numeric vector (3)}
#'     \item{radius}{Numeric}
#'     \item{inlier_mask}{Logical vector}
#'     \item{n_inliers}{Integer}
#'     \item{rmse}{Numeric}
#'   }
#'
#' @export
px_detect_cylinders <- function(cloud,
                                distance_threshold = 0.02,
                                radius_range = c(0.01, 2.0),
                                min_points = 100L,
                                num_iterations = 5000L) {
  .assert_px_cloud(cloud)
  distance_threshold <- .validate_scalar_numeric(distance_threshold, "distance_threshold")
  min_points <- .validate_neighbor_count(min_points, "min_points")
  num_iterations <- .validate_neighbor_count(num_iterations, "num_iterations")

  if (!is.numeric(radius_range) || length(radius_range) != 2L || any(!is.finite(radius_range))) {
    rlang::abort("`radius_range` must be a numeric vector of length 2.")
  }

  radius_range <- sort(as.numeric(radius_range))

  if (nrow(cloud$xyz) < max(min_points, 5L)) {
    return(list())
  }

  cyl <- .fit_cylinder_pca(cloud$xyz)

  if (is.null(cyl) || cyl$radius < radius_range[1L] || cyl$radius > radius_range[2L]) {
    return(list())
  }

  inlier_mask <- abs(cyl$radial_distance - cyl$radius) <= distance_threshold

  if (sum(inlier_mask) < min_points) {
    return(list())
  }

  list(
    structure(
      list(
        axis_point = cyl$axis_point,
        axis_direction = cyl$axis_direction,
        radius = cyl$radius,
        inlier_mask = inlier_mask,
        n_inliers = sum(inlier_mask),
        rmse = sqrt(mean((cyl$radial_distance[inlier_mask] - cyl$radius) ^ 2))
      ),
      class = "cylinder_result"
    )
  )
}


#' Detect edges and boundary points
#'
#' Uses normal variation and local surface curvature to identify edge points.
#'
#' @param cloud A \code{px_cloud} with normals.
#' @param k_neighbors Integer — neighbors for local analysis.
#' @param angle_threshold Numeric — angle threshold in degrees.
#'
#' @return An \code{edge_result} object with:
#'   \describe{
#'     \item{edge_mask}{Logical vector}
#'     \item{n_edges}{Integer}
#'   }
#'
#' @export
px_detect_edges <- function(cloud, k_neighbors = 30L, angle_threshold = 60.0) {
  .assert_px_cloud(cloud)

  if (is.null(cloud$normals)) {
    rlang::abort("`cloud` must contain normals.")
  }

  k_neighbors <- .validate_neighbor_count(k_neighbors, "k_neighbors")
  angle_threshold <- .validate_scalar_numeric(angle_threshold, "angle_threshold")

  if (nrow(cloud$xyz) == 0L) {
    return(structure(list(edge_mask = logical(), n_edges = 0L), class = "edge_result"))
  }

  dmat <- .pairwise_distances(cloud$xyz)
  neighbor_k <- pmin(k_neighbors, nrow(cloud$xyz) - 1L)
  edge_mask <- vapply(seq_len(nrow(cloud$xyz)), function(i) {
    if (neighbor_k < 1L) {
      return(FALSE)
    }

    neighbors <- order(dmat[i, ])[seq.int(2L, neighbor_k + 1L)]
    mean_normal <- .normalize_vector(colMeans(cloud$normals[neighbors, , drop = FALSE]))
    angle <- acos(pmin(1, abs(sum(.normalize_vector(cloud$normals[i, ]) * mean_normal)))) * 180 / pi
    is.finite(angle) && angle >= angle_threshold
  }, logical(1))

  structure(
    list(
      edge_mask = edge_mask,
      n_edges = sum(edge_mask)
    ),
    class = "edge_result"
  )
}

.plane_from_three_points <- function(points) {
  a <- points[1L, ]
  b <- points[2L, ]
  c <- points[3L, ]
  normal <- .cross3(b - a, c - a)
  normal <- .normalize_vector(normal)

  if (all(normal == 0)) {
    return(NULL)
  }

  equation <- c(normal, -sum(normal * a))
  list(normal = normal, equation = equation)
}

.fit_plane_pca <- function(points) {
  center <- colMeans(points)
  centered <- sweep(points, 2L, center, FUN = "-")
  eig <- eigen(crossprod(centered), symmetric = TRUE)
  normal <- .normalize_vector(eig$vectors[, ncol(eig$vectors)])
  equation <- c(normal, -sum(normal * center))
  list(normal = normal, equation = equation)
}

.point_plane_distances <- function(points, equation) {
  abs(points %*% equation[1:3] + equation[4L])
}

.cross3 <- function(a, b) {
  c(
    a[2L] * b[3L] - a[3L] * b[2L],
    a[3L] * b[1L] - a[1L] * b[3L],
    a[1L] * b[2L] - a[2L] * b[1L]
  )
}

.fit_cylinder_pca <- function(points) {
  center <- colMeans(points)
  centered <- sweep(points, 2L, center, FUN = "-")
  sv <- svd(centered)
  axis_direction <- .normalize_vector(sv$v[, 1L])

  if (all(axis_direction == 0)) {
    return(NULL)
  }

  projection_length <- as.numeric(centered %*% axis_direction)
  radial_vectors <- centered - projection_length %o% axis_direction
  radial_distance <- sqrt(rowSums(radial_vectors ^ 2))

  list(
    axis_point = center,
    axis_direction = axis_direction,
    radius = stats::median(radial_distance),
    radial_distance = radial_distance
  )
}
