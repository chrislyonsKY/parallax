// Point cloud registration algorithms.
//
// - ICP point-to-point: minimize sum of squared distances
// - ICP point-to-plane: minimize distances projected onto target normals
// - FPFH descriptor computation for coarse alignment
// - RANSAC rigid transformation estimation
//
// All functions operate on raw f64 slices (xyz as column-major from R matrices)
// and return transformation matrices + quality metrics.

// TODO: implement ICP point-to-point
// Algorithm:
// 1. Build k-d tree on target
// 2. For each iteration:
//    a. Find closest point in target for each source point (within max_distance)
//    b. Compute rigid transformation (SVD of cross-covariance)
//    c. Apply transformation to source
//    d. Check convergence (change in transformation < tolerance)
// 3. Return 4x4 transformation, fitness, RMSE, converged flag, iteration count

// TODO: implement ICP point-to-plane
// Same as above but step 2b minimizes point-to-tangent-plane distance
// using linearized least squares (requires target normals)

// TODO: implement FPFH descriptor computation
// Fast Point Feature Histogram:
// 1. Compute SPFH (Simplified PFH) for each point using k neighbors
// 2. Re-weight SPFH of neighbors to get FPFH
// 3. Return descriptor matrix (N x 33)

// TODO: implement RANSAC transformation estimation
// 1. Sample ransac_n point correspondences (matched by FPFH descriptors)
// 2. Compute rigid transformation from sample
// 3. Count inliers within distance threshold
// 4. Repeat for max_iterations, keep best
