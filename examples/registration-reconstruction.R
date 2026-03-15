library(parallax)

source_path <- system.file("extdata", "registration-source.csv", package = "parallax")
target_path <- system.file("extdata", "registration-target.csv", package = "parallax")

source <- px_read(source_path, platform = "terrestrial")
target <- px_read(target_path, platform = "terrestrial")

icp <- px_icp(source, target, max_correspondence_distance = 2.0)
target_with_normals <- px_estimate_normals(target, k_neighbors = 4L)
mesh <- px_poisson_reconstruct(target_with_normals, depth = 4L)

list(
  transformation = icp$transformation,
  rmse = icp$inlier_rmse,
  mesh_vertices = mesh$n_vertices,
  mesh_faces = mesh$n_faces
)
