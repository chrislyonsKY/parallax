test_that("crop and random downsample preserve px_cloud structure", {
  xyz <- matrix(
    c(
      0, 0, 0,
      1, 1, 1,
      2, 2, 2,
      3, 3, 3
    ),
    ncol = 3,
    byrow = TRUE
  )
  cloud <- px_aerial(xyz, intensity = c(10, 20, 30, 40))

  cropped <- px_crop(cloud, c(0, 0, 0, 2.1, 2.1, 2.1))
  sampled <- px_random_downsample(cloud, fraction = 0.5, seed = 42)

  expect_s3_class(cropped, c("px_aerial", "px_cloud"))
  expect_equal(nrow(cropped$xyz), 3)
  expect_s3_class(sampled, c("px_aerial", "px_cloud"))
  expect_equal(nrow(sampled$xyz), 2)
})

test_that("voxel downsample merges points by voxel", {
  xyz <- matrix(
    c(
      0.00, 0.00, 0.0,
      0.02, 0.01, 0.02,
      1.00, 1.00, 1.0
    ),
    ncol = 3,
    byrow = TRUE
  )
  cloud <- px_cloud(xyz, classification = c(2, 2, 5))

  downsampled <- px_voxel_downsample(cloud, voxel_size = 0.1)

  expect_equal(nrow(downsampled$xyz), 2)
  expect_equal(downsampled$classification, c(2L, 5L))
})

test_that("metrics and density products return expected shapes", {
  xyz <- matrix(
    c(
      0, 0, 0,
      1, 0, 2,
      0, 1, 4,
      1, 1, 6
    ),
    ncol = 3,
    byrow = TRUE
  )
  cloud <- px_cloud(xyz, intensity = c(1, 2, 3, 4), classification = c(2, 1, 1, 1))

  metrics <- px_metrics(cloud)
  density <- px_density_map(cloud, resolution = 1)
  chm <- px_canopy_height_model(cloud, resolution = 1, ground_class = 2)

  expect_equal(metrics$n_points, 4)
  expect_equal(metrics$intensity_mean, 2.5)
  expect_equal(dim(density$density), c(2, 2))
  expect_equal(dim(chm$chm), c(2, 2))
  expect_equal(chm$resolution, 1)
})
