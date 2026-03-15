#' parallax: Multi-Platform Point Cloud Analysis
#'
#' Registration, segmentation, surface reconstruction, and feature extraction
#' for aerial, terrestrial, and UAV LiDAR point clouds. Platform-aware defaults
#' adapt processing parameters based on acquisition type.
#'
#' @section Core types:
#' \itemize{
#'   \item \code{\link{px_cloud}} — base point cloud container
#'   \item \code{\link{px_aerial}} — aerial LiDAR subtype
#'   \item \code{\link{px_terrestrial}} — terrestrial laser scanning subtype
#'   \item \code{\link{px_uav}} — UAV LiDAR / photogrammetry subtype
#' }
#'
#' @section Main functions:
#' \itemize{
#'   \item \code{\link{px_read}}, \code{\link{px_write}} — I/O
#'   \item \code{\link{px_icp}}, \code{\link{px_align_scans}} — registration
#'   \item \code{\link{px_classify_ground}}, \code{\link{px_segment_trees}} — segmentation
#'   \item \code{\link{px_poisson_reconstruct}} — surface reconstruction
#'   \item \code{\link{px_detect_planes}} — feature extraction
#'   \item \code{\link{px_metrics}} — summary statistics
#' }
#'
#' @keywords internal
#' @aliases parallax-package
#' @useDynLib parallax, .registration = TRUE
"_PACKAGE"
