test_that("px_icp_point_to_plane requires normals on target", {
  source <- px_cloud(matrix(rnorm(300), ncol = 3))
  target <- px_cloud(matrix(rnorm(300), ncol = 3))

  expect_error(px_icp_point_to_plane(source, target), "normals")
})

test_that("px_align_scans requires at least 2 clouds", {
  cloud <- px_cloud(matrix(rnorm(300), ncol = 3))
  expect_error(px_align_scans(list(cloud)), "at least 2")
})

test_that("point-to-point fallback returns an icp_result", {
  source <- px_cloud(matrix(c(0, 0, 0, 1, 1, 1), ncol = 3, byrow = TRUE))
  target <- px_cloud(matrix(c(1, 1, 1, 2, 2, 2), ncol = 3, byrow = TRUE))

  result <- px_icp_point_to_point(source, target, max_correspondence_distance = 2)

  expect_s3_class(result, "icp_result")
  expect_equal(dim(result$transformation), c(4, 4))
  expect_true(result$converged)
})
