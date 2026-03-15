#' Estimate surface normals
#'
#' PCA-based normal estimation using k nearest neighbors.
#'
#' @param cloud A \code{px_cloud}.
#' @param k_neighbors Integer — neighbors for local PCA.
#' @param radius Numeric or \code{NULL} — radius search instead of k-NN.
#' @param orient Logical — consistently orient normals after estimation.
#'
#' @return The input cloud with \code{normals} field populated.
#'
#' @export
px_estimate_normals <- function(cloud, k_neighbors = 30L, radius = NULL, orient = TRUE) {
  .assert_px_cloud(cloud)
  k_neighbors <- .validate_neighbor_count(k_neighbors, "k_neighbors")
  orient <- .validate_scalar_logical(orient, "orient")

  if (!is.null(radius)) {
    radius <- .validate_scalar_numeric(radius, "radius")
    if (radius <= 0) {
      rlang::abort("`radius` must be greater than 0.")
    }
  }

  n_points <- nrow(cloud$xyz)
  if (n_points == 0L) {
    cloud$normals <- matrix(numeric(), ncol = 3)
    return(cloud)
  }

  native_normals <- .parallax_estimate_normals(
    cloud$xyz,
    as.integer(k_neighbors),
    if (is.null(radius)) NaN else radius
  )

  normals <- if (is.matrix(native_normals) && identical(dim(native_normals), c(n_points, 3L))) {
    native_normals
  } else {
    dmat <- .pairwise_distances(cloud$xyz)
    t(vapply(seq_len(n_points), function(i) {
      idx <- .normal_neighbors(dmat, i, k_neighbors = k_neighbors, radius = radius)
      .estimate_point_normal(cloud$xyz[idx, , drop = FALSE])
    }, numeric(3)))
  }

  cloud$normals <- unname(normals)

  if (orient) {
    cloud <- px_orient_normals(cloud)
  }

  cloud
}


#' Orient normals consistently
#'
#' @param cloud A \code{px_cloud} with normals.
#' @param viewpoint Numeric vector of length 3, or \code{NULL}. If provided,
#'   orients normals toward this point.
#' @param method Character: "mst" (minimum spanning tree) or "viewpoint".
#'
#' @return The cloud with consistently oriented normals.
#'
#' @export
px_orient_normals <- function(cloud, viewpoint = NULL, method = "mst") {
  .assert_px_cloud(cloud)

  if (is.null(cloud$normals)) {
    rlang::abort("`cloud` must contain normals.")
  }

  method <- match.arg(method, c("mst", "viewpoint"))

  if (!is.null(viewpoint)) {
    viewpoint <- as.numeric(viewpoint)
    if (length(viewpoint) != 3L || any(!is.finite(viewpoint))) {
      rlang::abort("`viewpoint` must be a numeric vector of length 3.")
    }

    aligned <- cloud
    for (i in seq_len(nrow(aligned$xyz))) {
      toward_view <- viewpoint - aligned$xyz[i, ]
      if (sum(aligned$normals[i, ] * toward_view) < 0) {
        aligned$normals[i, ] <- -aligned$normals[i, ]
      }
    }

    return(aligned)
  }

  if (method == "viewpoint") {
    return(px_orient_normals(cloud, viewpoint = colMeans(cloud$xyz) + c(0, 0, max(stats::sd(cloud$xyz[, 3]), 1))))
  }

  aligned <- cloud
  n_points <- nrow(aligned$xyz)

  if (n_points <= 1L) {
    return(aligned)
  }

  dmat <- .pairwise_distances(aligned$xyz)
  neighbor_count <- min(8L, n_points - 1L)
  neighbor_order <- lapply(seq_len(n_points), function(i) {
    order(dmat[i, ])[seq.int(2L, neighbor_count + 1L)]
  })

  visited <- rep(FALSE, n_points)
  pending <- which.max(aligned$xyz[, 3])
  queue <- pending
  visited[pending] <- TRUE

  while (any(!visited) || length(queue) > 0L) {
    if (length(queue) == 0L) {
      pending <- which(!visited)[1L]
      visited[pending] <- TRUE
      queue <- pending
    }

    current <- queue[1L]
    queue <- queue[-1L]

    for (neighbor in neighbor_order[[current]]) {
      if (!visited[neighbor]) {
        if (sum(aligned$normals[current, ] * aligned$normals[neighbor, ]) < 0) {
          aligned$normals[neighbor, ] <- -aligned$normals[neighbor, ]
        }

        visited[neighbor] <- TRUE
        queue <- c(queue, neighbor)
      }
    }
  }

  aligned
}

.normal_neighbors <- function(dmat, i, k_neighbors, radius = NULL) {
  order_idx <- order(dmat[i, ])
  order_idx <- order_idx[order_idx != i]

  if (!is.null(radius)) {
    radius_idx <- order_idx[dmat[i, order_idx] <= radius]
    if (length(radius_idx) >= 3L) {
      return(c(i, radius_idx))
    }
  }

  k <- min(k_neighbors, length(order_idx))
  c(i, order_idx[seq_len(max(k, min(2L, length(order_idx))))])
}

.estimate_point_normal <- function(points) {
  if (nrow(points) < 3L) {
    return(c(0, 0, 1))
  }

  centered <- sweep(points, 2L, colMeans(points), FUN = "-")
  cov_mat <- crossprod(centered) / max(nrow(points) - 1L, 1L)
  eig <- eigen(cov_mat, symmetric = TRUE)
  normal <- eig$vectors[, ncol(eig$vectors)]
  normal <- .normalize_vector(normal)

  if (all(normal == 0)) {
    c(0, 0, 1)
  } else {
    as.numeric(normal)
  }
}
