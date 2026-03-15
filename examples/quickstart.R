library(parallax)

aerial_path <- system.file("extdata", "aerial-demo.csv", package = "parallax")
terrestrial_path <- system.file("extdata", "terrestrial-demo.csv", package = "parallax")

aerial <- px_read(aerial_path, platform = "aerial")
terrestrial <- px_read(terrestrial_path, platform = "terrestrial")

ground <- px_classify_ground(aerial)
aerial$classification <- ifelse(ground$ground_mask, 2L, 1L)

filtered <- px_filter_statistical(aerial, k = 6, std_ratio = 1.5)
trees <- px_segment_trees(filtered, min_height = 2.0, crown_threshold = 0.75)
metrics <- px_metrics(filtered)
density <- px_density_map(filtered, resolution = 1.0)
chm <- px_canopy_height_model(aerial, resolution = 1.0)

plane_candidates <- px_detect_planes(
  terrestrial,
  distance_threshold = 0.08,
  min_points = 6L,
  num_iterations = 100L
)

list(
  ground = ground,
  trees = trees,
  metrics = metrics,
  density = density,
  chm = chm,
  plane_candidates = plane_candidates
)
