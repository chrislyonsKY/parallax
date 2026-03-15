test_that("normals can be estimated and oriented", {
  xy <- expand.grid(x = 0:2, y = 0:2)
  xyz <- as.matrix(cbind(xy$x, xy$y, 0))
  cloud <- px_cloud(xyz)

  estimated <- px_estimate_normals(cloud, k_neighbors = 4, orient = FALSE)
  expect_equal(dim(estimated$normals), c(9, 3))
  expect_true(all(abs(estimated$normals[, 3]) > 0.9))

  cloud$normals <- matrix(rep(c(0, 0, -1), 9), ncol = 3, byrow = TRUE)
  oriented <- px_orient_normals(cloud, viewpoint = c(1, 1, 10))
  expect_true(all(oriented$normals[, 3] > 0))
})

test_that("ground classification separates low and high surfaces", {
  xyz <- rbind(
    c(0, 0, 0),
    c(1, 0, 0.05),
    c(0, 1, 0),
    c(1, 1, 0.08),
    c(0, 0, 3),
    c(1, 0, 3.1),
    c(0, 1, 2.9),
    c(1, 1, 3.2)
  )
  cloud <- px_aerial(xyz)

  result <- px_classify_ground(cloud, cloth_resolution = 1, max_angle = 10)

  expect_s3_class(result, "ground_result")
  expect_equal(result$n_ground, 4)
  expect_equal(result$n_nonground, 4)
})

test_that("object and tree segmentation identify separate clusters", {
  xyz <- rbind(
    c(0, 0, 0.2),
    c(0.1, 0, 0.1),
    c(0, 0.1, 0.15),
    c(5, 5, 2.0),
    c(5.1, 5, 2.1),
    c(5, 5.1, 2.2),
    c(8, 8, 2.5),
    c(8.1, 8, 2.6),
    c(8, 8.1, 2.4)
  )
  cloud <- px_cloud(xyz)

  objects <- px_segment_objects(cloud, eps = 0.25, min_points = 2)
  trees <- px_segment_trees(cloud, min_height = 1.5, crown_threshold = 0.3)

  expect_s3_class(objects, "segmentation_result")
  expect_equal(objects$n_segments, 3)
  expect_equal(trees$n_segments, 2)
  expect_true(all(trees$labels[1:3] == -1L))
})

test_that("plane and cylinder detection return structured results", {
  plane_xy <- expand.grid(x = seq(0, 1, length.out = 4), y = seq(0, 1, length.out = 3))
  plane_xyz <- as.matrix(cbind(plane_xy$x, plane_xy$y, 0.5 * plane_xy$x + 0.1 * plane_xy$y + 1))
  plane_cloud <- px_cloud(rbind(plane_xyz, c(3, 3, 7), c(-2, -1, -4)))

  set.seed(123)
  planes <- px_detect_planes(
    plane_cloud,
    distance_threshold = 0.05,
    max_planes = 1,
    min_points = 10,
    num_iterations = 300
  )

  expect_length(planes, 1)
  expect_true(planes[[1]]$n_inliers >= 10)

  theta <- seq(0, 2 * pi, length.out = 13)[-13]
  z_levels <- c(0, 2, 4)
  cyl_xyz <- do.call(rbind, lapply(z_levels, function(z) cbind(cos(theta), sin(theta), z)))
  cyl_cloud <- px_cloud(cyl_xyz)
  cylinders <- px_detect_cylinders(
    cyl_cloud,
    distance_threshold = 0.2,
    radius_range = c(0.8, 1.2),
    min_points = 8
  )

  expect_length(cylinders, 1)
  expect_equal(cylinders[[1]]$radius, 1, tolerance = 0.15)
})

test_that("edge detection flags abrupt normal changes", {
  xyz <- rbind(
    c(0, 0, 0),
    c(1, 0, 0),
    c(0, 1, 0),
    c(1, 1, 0),
    c(0.5, 0.5, 0)
  )
  normals <- rbind(
    c(0, 0, 1),
    c(0, 0, 1),
    c(0, 0, 1),
    c(0, 0, 1),
    c(1, 0, 0)
  )
  cloud <- px_cloud(xyz, normals = normals)

  edges <- px_detect_edges(cloud, k_neighbors = 3, angle_threshold = 45)

  expect_equal(edges$n_edges >= 1, TRUE)
  expect_true(edges$edge_mask[5])
})
