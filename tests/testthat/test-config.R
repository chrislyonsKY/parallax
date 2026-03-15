test_that("platform defaults return correct cloth resolution", {
  aerial <- .platform_defaults("aerial")
  expect_equal(aerial$ground_cloth_resolution, 2.0)

  terrestrial <- .platform_defaults("terrestrial")
  expect_equal(terrestrial$ground_cloth_resolution, 0.5)

  uav <- .platform_defaults("uav")
  expect_equal(uav$ground_cloth_resolution, 1.5)

  unknown <- .platform_defaults("unknown")
  expect_equal(unknown$ground_cloth_resolution, 2.0)
})

test_that("platform defaults return all required fields", {
  required <- c("ground_algorithm", "ground_cloth_resolution",
                 "ground_max_angle", "sor_k", "sor_std_ratio", "voxel_size")

  for (platform in c("aerial", "terrestrial", "uav", "unknown")) {
    defaults <- .platform_defaults(platform)
    expect_true(all(required %in% names(defaults)),
                info = paste("Missing fields for platform:", platform))
  }
})
