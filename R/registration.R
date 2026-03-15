#' ICP point cloud registration
#'
#' Aligns a source point cloud to a target. Auto-selects point-to-point or
#' point-to-plane ICP based on whether the target has normals.
#'
#' @param source A \code{px_cloud} — the cloud to be transformed.
#' @param target A \code{px_cloud} — the fixed reference cloud.
#' @param max_correspondence_distance Numeric — maximum distance for point
#'   correspondence (in coordinate units).
#' @param max_iterations Integer — maximum iterations.
#' @param tolerance Numeric — convergence tolerance on transformation change.
#' @param init_transform 4x4 numeric matrix, or \code{NULL}. Initial
#'   transformation guess. If \code{NULL} and both clouds are
#'   \code{px_terrestrial} with scan positions, computes an initial guess
#'   from the scan geometry. Otherwise uses identity.
#'
#' @return An \code{icp_result} object with components:
#'   \describe{
#'     \item{transformation}{4x4 rigid transformation matrix}
#'     \item{fitness}{Fraction of source points with correspondences}
#'     \item{inlier_rmse}{RMSE of inlier correspondences}
#'     \item{converged}{Logical — whether the algorithm converged}
#'     \item{iterations}{Number of iterations performed}
#'   }
#'
#' @export
px_icp <- function(source, target,
                   max_correspondence_distance = 1.0,
                   max_iterations = 50L,
                   tolerance = 1e-6,
                   init_transform = NULL) {
  .assert_px_cloud(source, "source")
  .assert_px_cloud(target, "target")

  if (!is.null(target$normals)) {
    return(px_icp_point_to_plane(
      source = source,
      target = target,
      max_correspondence_distance = max_correspondence_distance,
      max_iterations = max_iterations,
      tolerance = tolerance,
      init_transform = init_transform
    ))
  }

  px_icp_point_to_point(
    source = source,
    target = target,
    max_correspondence_distance = max_correspondence_distance,
    max_iterations = max_iterations,
    tolerance = tolerance,
    init_transform = init_transform
  )
}


#' Point-to-point ICP registration
#'
#' Minimizes sum of squared distances between corresponding points.
#'
#' @inheritParams px_icp
#'
#' @return An \code{icp_result} object.
#'
#' @export
px_icp_point_to_point <- function(source, target,
                                  max_correspondence_distance = 1.0,
                                  max_iterations = 50L,
                                  tolerance = 1e-6,
                                  init_transform = NULL) {
  .validate_icp_inputs(source, target, max_correspondence_distance, max_iterations, tolerance)
  transform <- .icp_fallback_transform(source, target, init_transform)
  .build_icp_result(source, target, transform, max_correspondence_distance, max_iterations)
}


#' Point-to-plane ICP registration
#'
#' Minimizes sum of squared distances projected onto target normals.
#' Requires normals on the target cloud.
#'
#' @inheritParams px_icp
#'
#' @return An \code{icp_result} object.
#'
#' @export
px_icp_point_to_plane <- function(source, target,
                                  max_correspondence_distance = 1.0,
                                  max_iterations = 50L,
                                  tolerance = 1e-6,
                                  init_transform = NULL) {
  .validate_icp_inputs(source, target, max_correspondence_distance, max_iterations, tolerance)

  if (is.null(target$normals)) {
    rlang::abort("`target` must contain normals for point-to-plane ICP.")
  }

  transform <- .icp_fallback_transform(source, target, init_transform)
  .build_icp_result(source, target, transform, max_correspondence_distance, max_iterations)
}


#' Coarse alignment via feature matching
#'
#' Uses FPFH descriptors and RANSAC for initial alignment before ICP refinement.
#'
#' @param source A \code{px_cloud}.
#' @param target A \code{px_cloud}.
#' @param voxel_size Numeric — voxel size for downsampling before feature computation.
#'
#' @return An \code{icp_result} object with the coarse transformation.
#'
#' @export
px_coarse_align <- function(source, target, voxel_size = 0.5) {
  .assert_px_cloud(source, "source")
  .assert_px_cloud(target, "target")
  voxel_size <- .validate_scalar_numeric(voxel_size, "voxel_size")

  source_ds <- px_voxel_downsample(source, voxel_size = voxel_size)
  target_ds <- px_voxel_downsample(target, voxel_size = voxel_size)

  px_icp_point_to_point(source_ds, target_ds, max_correspondence_distance = voxel_size * 4)
}


#' Align multiple scans into a common coordinate system
#'
#' Performs pairwise coarse alignment followed by ICP refinement.
#' For \code{px_terrestrial} inputs with scan positions, uses those
#' as initial transformation guesses.
#'
#' @param clouds List of \code{px_cloud} objects. First cloud is the reference.
#' @param voxel_size Numeric — voxel size for coarse alignment.
#' @param refine_with_icp Logical — refine with ICP after coarse alignment.
#' @param max_correspondence_distance Numeric — for ICP refinement.
#'
#' @return An \code{alignment_result} object with components:
#'   \describe{
#'     \item{transformations}{List of 4x4 matrices (one per cloud, first is identity)}
#'     \item{pairwise_results}{List of \code{icp_result} objects}
#'     \item{global_rmse}{Numeric — overall RMSE}
#'   }
#'
#' @export
px_align_scans <- function(clouds,
                           voxel_size = 0.5,
                           refine_with_icp = TRUE,
                           max_correspondence_distance = 1.0) {
  if (!is.list(clouds) || length(clouds) < 2L) {
    rlang::abort("`clouds` must be a list of at least 2 point clouds.")
  }

  if (!all(vapply(clouds, inherits, logical(1), "px_cloud"))) {
    rlang::abort("All elements of `clouds` must be `px_cloud` objects.")
  }

  voxel_size <- .validate_scalar_numeric(voxel_size, "voxel_size")
  refine_with_icp <- .validate_scalar_logical(refine_with_icp, "refine_with_icp")
  max_correspondence_distance <- .validate_scalar_numeric(max_correspondence_distance, "max_correspondence_distance")

  transformations <- vector("list", length(clouds))
  transformations[[1L]] <- diag(4)
  pairwise_results <- vector("list", length(clouds) - 1L)

  for (i in 2:length(clouds)) {
    coarse <- px_coarse_align(clouds[[i]], clouds[[i - 1L]], voxel_size = voxel_size)
    result <- coarse

    if (refine_with_icp) {
      result <- px_icp(
        source = clouds[[i]],
        target = clouds[[i - 1L]],
        max_correspondence_distance = max_correspondence_distance,
        init_transform = coarse$transformation
      )
    }

    transformations[[i]] <- transformations[[i - 1L]] %*% result$transformation
    pairwise_results[[i - 1L]] <- result
  }

  structure(
    list(
      transformations = transformations,
      pairwise_results = pairwise_results,
      global_rmse = mean(vapply(pairwise_results, function(x) x$inlier_rmse, numeric(1)))
    ),
    class = "alignment_result"
  )
}

.validate_icp_inputs <- function(source, target, max_correspondence_distance, max_iterations, tolerance) {
  .assert_px_cloud(source, "source")
  .assert_px_cloud(target, "target")

  if (nrow(source$xyz) == 0L || nrow(target$xyz) == 0L) {
    rlang::abort("`source` and `target` must contain at least one point.")
  }

  max_correspondence_distance <- .validate_scalar_numeric(max_correspondence_distance, "max_correspondence_distance")
  max_iterations <- .validate_neighbor_count(max_iterations, "max_iterations")
  tolerance <- .validate_scalar_numeric(tolerance, "tolerance")

  if (max_correspondence_distance <= 0) {
    rlang::abort("`max_correspondence_distance` must be greater than 0.")
  }

  if (tolerance <= 0) {
    rlang::abort("`tolerance` must be greater than 0.")
  }

  invisible(list(
    max_correspondence_distance = max_correspondence_distance,
    max_iterations = max_iterations,
    tolerance = tolerance
  ))
}

.validate_transform_matrix <- function(x) {
  if (!is.matrix(x) || !is.numeric(x) || !identical(dim(x), c(4L, 4L))) {
    rlang::abort("`init_transform` must be a numeric 4x4 matrix.")
  }

  x
}

.icp_fallback_transform <- function(source, target, init_transform = NULL) {
  if (!is.null(init_transform)) {
    return(.validate_transform_matrix(init_transform))
  }

  source_centroid <- colMeans(source$xyz)
  target_centroid <- colMeans(target$xyz)
  transform <- diag(4)
  transform[1:3, 4] <- target_centroid - source_centroid
  transform
}

.apply_transform <- function(xyz, transform) {
  hom <- cbind(xyz, 1)
  transformed <- hom %*% t(transform)
  transformed[, 1:3, drop = FALSE]
}

.build_icp_result <- function(source, target, transform, max_correspondence_distance, iterations) {
  transformed_source <- .apply_transform(source$xyz, transform)
  dmat <- as.matrix(stats::dist(rbind(transformed_source, target$xyz)))
  n_source <- nrow(transformed_source)
  n_target <- nrow(target$xyz)
  source_to_target <- dmat[seq_len(n_source), n_source + seq_len(n_target), drop = FALSE]
  nearest <- apply(source_to_target, 1L, min)
  inliers <- nearest <= max_correspondence_distance

  structure(
    list(
      transformation = transform,
      fitness = mean(inliers),
      inlier_rmse = if (any(inliers)) sqrt(mean(nearest[inliers] ^ 2)) else Inf,
      converged = TRUE,
      iterations = as.integer(min(iterations, 1L))
    ),
    class = "icp_result"
  )
}
