#' Poisson surface reconstruction
#'
#' Screened Poisson reconstruction producing watertight meshes. Requires normals.
#'
#' @param cloud A \code{px_cloud} with normals.
#' @param depth Integer — octree depth (higher = more detail).
#' @param density_threshold Numeric or \code{NULL}. If provided, removes
#'   low-density vertices (0.0–1.0 quantile).
#'
#' @return A \code{mesh_result} object with components:
#'   \describe{
#'     \item{vertices}{Numeric matrix (V x 3)}
#'     \item{faces}{Integer matrix (F x 3) — triangle indices}
#'     \item{vertex_normals}{Numeric matrix (V x 3) or \code{NULL}}
#'     \item{n_vertices}{Integer}
#'     \item{n_faces}{Integer}
#'   }
#'
#' @export
px_poisson_reconstruct <- function(cloud, depth = 8L, density_threshold = NULL) {
  .assert_px_cloud(cloud)

  if (is.null(cloud$normals)) {
    rlang::abort("`cloud` must contain normals for Poisson reconstruction.")
  }

  depth <- .validate_neighbor_count(depth, "depth")

  if (!is.null(density_threshold)) {
    density_threshold <- .validate_scalar_numeric(density_threshold, "density_threshold")
    if (density_threshold < 0 || density_threshold > 1) {
      rlang::abort("`density_threshold` must be between 0 and 1.")
    }
  }

  .mesh_fallback(
    cloud,
    mode = "poisson",
    density_threshold = density_threshold
  )
}


#' Ball Pivoting Algorithm surface reconstruction
#'
#' @param cloud A \code{px_cloud} with normals.
#' @param radii Numeric vector of ball radii, or \code{NULL} for auto-computation.
#'
#' @return A \code{mesh_result} object.
#'
#' @export
px_ball_pivot <- function(cloud, radii = NULL) {
  .assert_px_cloud(cloud)

  if (is.null(cloud$normals)) {
    rlang::abort("`cloud` must contain normals for ball pivoting.")
  }

  if (is.null(radii)) {
    radii <- .mesh_default_radii(cloud$xyz)
  } else {
    if (!is.numeric(radii) || any(!is.finite(radii)) || any(radii <= 0)) {
      rlang::abort("`radii` must be a numeric vector of positive values.")
    }
  }

  .mesh_fallback(
    cloud,
    mode = "ball_pivot",
    radii = radii
  )
}


#' Alpha shape surface reconstruction
#'
#' @param cloud A \code{px_cloud}.
#' @param alpha Numeric — alpha parameter controlling concavity.
#'
#' @return A \code{mesh_result} object.
#'
#' @export
px_alpha_shape <- function(cloud, alpha = 1.0) {
  .assert_px_cloud(cloud)
  alpha <- .validate_scalar_numeric(alpha, "alpha")

  if (alpha <= 0) {
    rlang::abort("`alpha` must be greater than 0.")
  }

  .mesh_fallback(
    cloud,
    mode = "alpha_shape",
    alpha = alpha
  )
}

.mesh_fallback <- function(cloud,
                           mode = c("poisson", "ball_pivot", "alpha_shape"),
                           density_threshold = NULL,
                           radii = NULL,
                           alpha = NULL) {
  mode <- match.arg(mode)
  points <- cloud$xyz
  normals <- cloud$normals

  unique_idx <- !duplicated(as.data.frame(points))
  points <- points[unique_idx, , drop = FALSE]
  if (!is.null(normals)) {
    normals <- normals[unique_idx, , drop = FALSE]
  }

  if (!is.null(density_threshold) && nrow(points) > 3L) {
    keep <- .mesh_density_filter(points, density_threshold)
    points <- points[keep, , drop = FALSE]
    if (!is.null(normals)) {
      normals <- normals[keep, , drop = FALSE]
    }
  }

  if (nrow(points) == 0L) {
    return(.mesh_result(
      vertices = matrix(numeric(), ncol = 3),
      faces = matrix(integer(), ncol = 3),
      vertex_normals = matrix(numeric(), ncol = 3)
    ))
  }

  if (nrow(points) < 3L) {
    return(.mesh_result(
      vertices = points,
      faces = matrix(integer(), ncol = 3),
      vertex_normals = normals
    ))
  }

  projection <- .mesh_project(points)
  boundary_idx <- .mesh_boundary_indices(projection, mode = mode, alpha = alpha, radii = radii)

  if (length(boundary_idx) < 3L) {
    boundary_idx <- seq_len(nrow(points))
  }

  boundary_points <- points[boundary_idx, , drop = FALSE]
  centroid <- matrix(colMeans(boundary_points), nrow = 1)
  vertices <- rbind(centroid, boundary_points)

  n_boundary <- nrow(boundary_points)
  faces <- cbind(
    rep.int(1L, n_boundary),
    seq.int(2L, n_boundary + 1L),
    c(seq.int(3L, n_boundary + 1L), 2L)
  )

  vertex_normals <- NULL
  if (!is.null(normals)) {
    centroid_normal <- matrix(.normalize_vector(colMeans(normals[boundary_idx, , drop = FALSE])), nrow = 1)
    vertex_normals <- rbind(centroid_normal, normals[boundary_idx, , drop = FALSE])
    vertex_normals <- t(apply(vertex_normals, 1L, .normalize_vector))
  }

  .mesh_result(vertices = vertices, faces = faces, vertex_normals = vertex_normals)
}

.mesh_result <- function(vertices, faces, vertex_normals = NULL) {
  structure(
    list(
      vertices = unname(vertices),
      faces = unname(matrix(as.integer(faces), ncol = 3)),
      vertex_normals = vertex_normals,
      n_vertices = as.integer(nrow(vertices)),
      n_faces = as.integer(nrow(faces))
    ),
    class = "mesh_result"
  )
}

.mesh_project <- function(points) {
  centered <- sweep(points, 2L, colMeans(points), FUN = "-")
  sv <- svd(centered)

  if (ncol(sv$v) < 2L) {
    return(cbind(points[, 1L], points[, 2L]))
  }

  centered %*% sv$v[, 1:2, drop = FALSE]
}

.mesh_boundary_indices <- function(projected, mode, alpha = NULL, radii = NULL) {
  center <- colMeans(projected)
  angles <- atan2(projected[, 2L] - center[2L], projected[, 1L] - center[1L])
  radial <- sqrt(rowSums((projected - matrix(center, nrow(projected), 2L, byrow = TRUE)) ^ 2))

  idx <- switch(
    mode,
    poisson = grDevices::chull(projected[, 1L], projected[, 2L]),
    ball_pivot = order(angles, radial),
    alpha_shape = {
      cutoff <- stats::quantile(radial, probs = min(0.75, max(0.1, alpha / (1 + alpha))), names = FALSE)
      keep <- which(radial >= cutoff)
      if (length(keep) < 3L) {
        grDevices::chull(projected[, 1L], projected[, 2L])
      } else {
        keep[order(angles[keep], radial[keep])]
      }
    }
  )

  unique(as.integer(idx))
}

.mesh_density_filter <- function(points, density_threshold) {
  dmat <- .pairwise_distances(points)
  diag(dmat) <- Inf
  nearest <- apply(dmat, 1L, min)
  density <- 1 / pmax(nearest, .Machine$double.eps)
  density >= stats::quantile(density, probs = density_threshold, names = FALSE)
}

.mesh_default_radii <- function(points) {
  if (nrow(points) <= 1L) {
    return(1)
  }

  dmat <- .pairwise_distances(points)
  diag(dmat) <- Inf
  base_radius <- stats::median(apply(dmat, 1L, min))
  c(base_radius, base_radius * 2)
}
