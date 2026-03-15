#' Classify ground points
#'
#' Platform-aware ground classification using Cloth Simulation Filter (CSF) or
#' Progressive Morphological Filter (PMF). When \code{algorithm = "auto"},
#' selects based on the cloud's platform.
#'
#' @param cloud A \code{px_cloud} object.
#' @param algorithm Character: "auto", "csf", or "pmf".
#' @param cloth_resolution Numeric or \code{NULL}. CSF cloth resolution in meters.
#'   If \code{NULL}, uses platform default (aerial: 2.0, terrestrial: 0.5, uav: 1.5).
#' @param max_angle Numeric or \code{NULL}. Maximum terrain angle in degrees.
#' @param iterations Integer. Number of CSF simulation iterations.
#'
#' @return A \code{ground_result} object with components:
#'   \describe{
#'     \item{ground_mask}{Logical vector — \code{TRUE} for ground points}
#'     \item{n_ground}{Integer count of ground points}
#'     \item{n_nonground}{Integer count of non-ground points}
#'     \item{algorithm}{Character — algorithm used}
#'   }
#'
#' @examples
#' \dontrun{
#' ground <- px_classify_ground(aerial_cloud)
#' ground <- px_classify_ground(tls_cloud, cloth_resolution = 0.3)
#' }
#'
#' @export
px_classify_ground <- function(cloud,
                               algorithm = "auto",
                               cloth_resolution = NULL,
                               max_angle = NULL,
                               iterations = 500L) {
  .assert_px_cloud(cloud)

  algorithm <- match.arg(algorithm, c("auto", "csf", "pmf"))
  defaults <- .platform_defaults(cloud$metadata$platform %||% "unknown")

  if (algorithm == "auto") {
    algorithm <- defaults$ground_algorithm
  }

  cloth_resolution <- cloth_resolution %||% defaults$ground_cloth_resolution
  max_angle <- max_angle %||% defaults$ground_max_angle

  cloth_resolution <- .validate_scalar_numeric(cloth_resolution, "cloth_resolution")
  max_angle <- .validate_scalar_numeric(max_angle, "max_angle")
  iterations <- .validate_neighbor_count(iterations, "iterations")

  if (cloth_resolution <= 0) {
    rlang::abort("`cloth_resolution` must be greater than 0.")
  }

  ground_z <- .ground_surface_estimate(cloud$xyz, resolution = cloth_resolution)
  threshold <- .ground_height_threshold(cloth_resolution, max_angle, algorithm, iterations)
  ground_mask <- (cloud$xyz[, 3L] - ground_z) <= threshold

  structure(
    list(
      ground_mask = ground_mask,
      n_ground = sum(ground_mask),
      n_nonground = length(ground_mask) - sum(ground_mask),
      algorithm = algorithm
    ),
    class = "ground_result"
  )
}


#' Segment individual objects
#'
#' Segments non-ground points into individual objects using DBSCAN,
#' region growing, or connected components.
#'
#' @param cloud A \code{px_cloud} (typically with ground removed).
#' @param method Character: "dbscan", "region_growing", or "connected_components".
#' @param eps Numeric — neighborhood radius for DBSCAN.
#' @param min_points Integer — minimum points per segment.
#' @param use_normals Logical — incorporate normals in segmentation.
#'
#' @return A \code{segmentation_result} object with components:
#'   \describe{
#'     \item{labels}{Integer vector — per-point segment IDs (-1 = noise)}
#'     \item{n_segments}{Integer — number of segments}
#'     \item{segment_sizes}{Named integer vector — points per segment}
#'   }
#'
#' @export
px_segment_objects <- function(cloud,
                               method = "dbscan",
                               eps = 0.5,
                               min_points = 50L,
                               use_normals = FALSE) {
  .assert_px_cloud(cloud)
  method <- match.arg(method, c("dbscan", "region_growing", "connected_components"))
  eps <- .validate_scalar_numeric(eps, "eps")
  min_points <- .validate_neighbor_count(min_points, "min_points")
  use_normals <- .validate_scalar_logical(use_normals, "use_normals")

  if (eps <= 0) {
    rlang::abort("`eps` must be greater than 0.")
  }

  if (use_normals && is.null(cloud$normals)) {
    rlang::abort("`cloud` must contain normals when `use_normals = TRUE`.")
  }

  n_points <- nrow(cloud$xyz)
  if (n_points == 0L) {
    return(.build_segmentation_result(integer(), method = method))
  }

  dmat <- .pairwise_distances(cloud$xyz)
  adjacency <- dmat <= eps & dmat > 0

  if ((method == "region_growing" || use_normals) && !is.null(cloud$normals)) {
    adjacency <- adjacency & .normal_similarity_mask(cloud$normals, max_angle = 30)
  }

  labels <- switch(
    method,
    dbscan = {
      native_labels <- if (!use_normals) {
        .parallax_dbscan(cloud$xyz, eps, as.integer(min_points))
      } else {
        NULL
      }

      if (is.integer(native_labels) && length(native_labels) == n_points) {
        as.integer(native_labels)
      } else {
        .dbscan_labels(adjacency, min_points = min_points)
      }
    },
    region_growing = .drop_small_segments(.connected_component_labels(adjacency), min_points = min_points),
    connected_components = .drop_small_segments(.connected_component_labels(adjacency), min_points = min_points)
  )

  .build_segmentation_result(labels, method = method)
}


#' Segment individual trees
#'
#' Delineates individual tree crowns from aerial or UAV point clouds.
#' Typically applied to non-ground points.
#'
#' @param cloud A \code{px_cloud} (non-ground points preferred).
#' @param method Character: "watershed", "li2012", or "dalponte".
#' @param min_height Numeric — minimum tree height in meters.
#' @param crown_threshold Numeric — crown delineation threshold in meters.
#'
#' @return A \code{segmentation_result} object with per-point tree labels.
#'
#' @export
px_segment_trees <- function(cloud,
                             method = "watershed",
                             min_height = 2.0,
                             crown_threshold = 1.0) {
  .assert_px_cloud(cloud)
  method <- match.arg(method, c("watershed", "li2012", "dalponte"))
  min_height <- .validate_scalar_numeric(min_height, "min_height")
  crown_threshold <- .validate_scalar_numeric(crown_threshold, "crown_threshold")

  if (min_height < 0) {
    rlang::abort("`min_height` must be greater than or equal to 0.")
  }

  if (crown_threshold <= 0) {
    rlang::abort("`crown_threshold` must be greater than 0.")
  }

  labels <- rep.int(-1L, nrow(cloud$xyz))
  tree_mask <- cloud$xyz[, 3L] >= min_height

  if (any(tree_mask)) {
    crown_eps <- crown_threshold * switch(
      method,
      watershed = 1,
      li2012 = 1.25,
      dalponte = 0.75
    )

    tree_cloud <- .subset_px_cloud(cloud, which(tree_mask))
    tree_segments <- px_segment_objects(
      tree_cloud,
      method = "dbscan",
      eps = crown_eps,
      min_points = 2L
    )
    labels[tree_mask] <- tree_segments$labels
  }

  .build_segmentation_result(labels, method = method)
}

.ground_surface_estimate <- function(xyz, resolution) {
  keys <- .grid_keys(xyz[, 1L], xyz[, 2L], resolution = resolution)
  cell_min <- tapply(xyz[, 3L], keys, min)
  as.numeric(cell_min[keys])
}

.ground_height_threshold <- function(cloth_resolution, max_angle, algorithm, iterations) {
  base <- tan(max_angle * pi / 180) * cloth_resolution
  algo_term <- if (identical(algorithm, "pmf")) cloth_resolution * 0.75 else cloth_resolution * 0.5
  iter_term <- min(iterations / 1000, 1) * 0.05
  max(0.1, base + algo_term + iter_term)
}

.grid_keys <- function(x, y, resolution) {
  x_bin <- floor((x - min(x)) / resolution)
  y_bin <- floor((y - min(y)) / resolution)
  paste(x_bin, y_bin, sep = ":")
}

.build_segmentation_result <- function(labels, method = NULL) {
  labels <- as.integer(labels)
  positive <- sort(unique(labels[labels > 0L]))

  segment_sizes <- if (length(positive) == 0L) {
    integer()
  } else {
    tab <- table(labels[labels > 0L])
    stats::setNames(as.integer(tab), names(tab))
  }

  structure(
    list(
      labels = labels,
      n_segments = length(positive),
      segment_sizes = segment_sizes,
      method = method
    ),
    class = "segmentation_result"
  )
}

.normal_similarity_mask <- function(normals, max_angle = 30) {
  normalized <- t(apply(normals, 1L, .normalize_vector))
  similarity <- abs(normalized %*% t(normalized))
  similarity >= cos(max_angle * pi / 180)
}

.connected_component_labels <- function(adjacency) {
  n_points <- nrow(adjacency)
  labels <- rep.int(0L, n_points)
  current <- 0L

  for (i in seq_len(n_points)) {
    if (labels[i] != 0L) {
      next
    }

    current <- current + 1L
    queue <- i
    labels[i] <- current

    while (length(queue) > 0L) {
      node <- queue[1L]
      queue <- queue[-1L]
      neighbors <- which(adjacency[node, ])
      new_nodes <- neighbors[labels[neighbors] == 0L]

      if (length(new_nodes) > 0L) {
        labels[new_nodes] <- current
        queue <- unique(c(queue, new_nodes))
      }
    }
  }

  labels
}

.drop_small_segments <- function(labels, min_points) {
  if (length(labels) == 0L) {
    return(labels)
  }

  tab <- table(labels)
  small <- as.integer(names(tab)[tab < min_points])
  labels[labels %in% small] <- -1L

  positive <- sort(unique(labels[labels > 0L]))
  if (length(positive) > 0L) {
    for (i in seq_along(positive)) {
      labels[labels == positive[i]] <- i
    }
  }

  as.integer(labels)
}

.dbscan_labels <- function(adjacency, min_points) {
  n_points <- nrow(adjacency)
  labels <- rep.int(0L, n_points)
  visited <- rep(FALSE, n_points)
  cluster_id <- 0L

  for (i in seq_len(n_points)) {
    if (visited[i]) {
      next
    }

    visited[i] <- TRUE
    neighbors <- which(adjacency[i, ])

    if ((length(neighbors) + 1L) < min_points) {
      labels[i] <- -1L
      next
    }

    cluster_id <- cluster_id + 1L
    labels[i] <- cluster_id
    seeds <- unique(c(i, neighbors))

    while (length(seeds) > 0L) {
      point <- seeds[1L]
      seeds <- seeds[-1L]

      if (!visited[point]) {
        visited[point] <- TRUE
        point_neighbors <- which(adjacency[point, ])
        if ((length(point_neighbors) + 1L) >= min_points) {
          seeds <- unique(c(seeds, point_neighbors))
        }
      }

      if (labels[point] <= 0L) {
        labels[point] <- cluster_id
      }
    }
  }

  as.integer(labels)
}
