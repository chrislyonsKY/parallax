// Geometric feature extraction via RANSAC.
//
// - Plane fitting (3-point RANSAC)
// - Cylinder fitting (6-point RANSAC, requires normals)
//
// Sequential RANSAC: detect best primitive, remove inliers, repeat.

// TODO: implement RANSAC plane detection
// Algorithm:
// 1. Sample 3 random points
// 2. Fit plane: normal = cross product of two edge vectors, d = -dot(normal, point)
// 3. Count inliers: points within distance_threshold of plane
// 4. Repeat for num_iterations, keep best
// 5. Remove inliers, repeat for next plane up to max_planes

// TODO: implement RANSAC cylinder detection
// Algorithm:
// 1. Sample 2 points with normals
// 2. Axis direction = cross product of normals (approximate)
// 3. Project points onto plane perpendicular to axis
// 4. Fit circle in 2D → radius and center
// 5. Count inliers: points within distance_threshold of cylinder surface
// 6. Requires more iterations than plane (6-DOF vs 4-DOF)
