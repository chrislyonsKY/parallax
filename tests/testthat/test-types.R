test_that("px_cloud creates a valid object", {
  xyz <- matrix(seq_len(12), ncol = 3)
  cloud <- px_cloud(
    xyz = xyz,
    intensity = seq_len(4),
    classification = c(1, 2, 2, 3),
    rgb = matrix(rep(1:3, 4), ncol = 3, byrow = TRUE),
    normals = matrix(rep(c(0, 0, 1), 4), ncol = 3, byrow = TRUE),
    return_number = c(1, 1, 2, 1),
    number_of_returns = c(2, 2, 2, 1)
  )

  expect_s3_class(cloud, "px_cloud")
  expect_equal(nrow(cloud$xyz), 4)
  expect_equal(cloud$metadata$platform, "unknown")
  expect_equal(cloud$classification, c(1L, 2L, 2L, 3L))
})

test_that("px_cloud rejects invalid xyz dimensions", {
  expect_error(px_cloud(matrix(1:10, ncol = 2)), "3 columns")
  expect_error(px_cloud(1:10), "3 columns")
  expect_error(px_cloud(matrix(c(1, 2, NA), ncol = 3)), "finite")
})

test_that("px_aerial sets platform to aerial", {
  xyz <- matrix(rnorm(300), ncol = 3)
  cloud <- px_aerial(xyz, flight_altitude_m = 120)

  expect_s3_class(cloud, c("px_aerial", "px_cloud"))
  expect_equal(cloud$metadata$platform, "aerial")
  expect_equal(cloud$metadata$flight_altitude_m, 120)
})

test_that("px_terrestrial stores scan positions", {
  xyz <- matrix(rnorm(300), ncol = 3)
  pos <- px_scan_position(0, 0, 1.5, scan_id = "s1")
  cloud <- px_terrestrial(xyz, scan_positions = list(pos))

  expect_s3_class(cloud, c("px_terrestrial", "px_cloud"))
  expect_length(cloud$metadata$scan_positions, 1)
  expect_equal(cloud$metadata$scan_positions[[1]]$scan_id, "s1")
})

test_that("px_uav tracks photogrammetric flag", {
  xyz <- matrix(rnorm(300), ncol = 3)
  cloud <- px_uav(xyz, is_photogrammetric = TRUE)

  expect_s3_class(cloud, c("px_uav", "px_cloud"))
  expect_true(cloud$metadata$is_photogrammetric)
})

test_that("px_scan_position creates valid object", {
  pos <- px_scan_position(1, 2, 3, scan_id = "test")

  expect_s3_class(pos, "px_scan_position")
  expect_equal(pos$x, 1)
  expect_equal(pos$scan_id, "test")
})
