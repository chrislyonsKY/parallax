test_that("mesh fallbacks return structured mesh results", {
  xy <- expand.grid(x = seq(0, 1, length.out = 4), y = seq(0, 1, length.out = 4))
  xyz <- as.matrix(cbind(xy$x, xy$y, 0.2 * xy$x + 0.1 * xy$y))
  normals <- matrix(rep(c(0, 0, 1), nrow(xyz)), ncol = 3, byrow = TRUE)
  cloud <- px_cloud(xyz, normals = normals)

  poisson <- px_poisson_reconstruct(cloud, depth = 6, density_threshold = 0.1)
  bpa <- px_ball_pivot(cloud)
  alpha <- px_alpha_shape(px_cloud(xyz), alpha = 1)

  expect_s3_class(poisson, "mesh_result")
  expect_true(poisson$n_vertices >= 3)
  expect_true(poisson$n_faces >= 1)

  expect_s3_class(bpa, "mesh_result")
  expect_true(bpa$n_faces >= 1)

  expect_s3_class(alpha, "mesh_result")
  expect_true(alpha$n_faces >= 1)
})

test_that("ggplot-based visualization helpers return plot objects", {
  skip_if_not_installed("ggplot2")

  xyz <- matrix(
    c(
      0, 0, 0,
      1, 0, 0.1,
      0, 1, 0.2,
      1, 1, 0.3
    ),
    ncol = 3,
    byrow = TRUE
  )
  cloud <- px_cloud(
    xyz,
    classification = c(2, 2, 5, 5),
    rgb = matrix(c(
      255, 0, 0,
      255, 0, 0,
      0, 0, 255,
      0, 0, 255
    ), ncol = 3, byrow = TRUE)
  )

  result <- structure(
    list(
      transformation = diag(4),
      fitness = 1,
      inlier_rmse = 0,
      converged = TRUE,
      iterations = 1L
    ),
    class = "icp_result"
  )
  segmentation <- structure(
    list(
      labels = c(1L, 1L, 2L, 2L),
      n_segments = 2L,
      segment_sizes = c(`1` = 2L, `2` = 2L)
    ),
    class = "segmentation_result"
  )

  cloud_plot <- plot(cloud, color_by = "rgb", backend = "ggplot")
  reg_plot <- px_plot_registration(cloud, cloud, result)
  seg_plot <- px_plot_segmentation(cloud, segmentation)

  expect_s3_class(cloud_plot, "ggplot")
  expect_s3_class(reg_plot, "ggplot")
  expect_s3_class(seg_plot, "ggplot")
})
