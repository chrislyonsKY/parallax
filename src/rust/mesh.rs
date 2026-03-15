// Surface reconstruction algorithms.
//
// - Screened Poisson reconstruction (Kazhdan & Hoppe 2013)
// - Ball Pivoting Algorithm (Bernardini et al. 1999)
// - Alpha shapes (Edelsbrunner & Mucke 1994)
//
// All functions take xyz + normals as f64 slices and return
// vertex matrix + face index matrix.

// TODO: implement Screened Poisson reconstruction
// This is the most complex algorithm in the package.
// Consider wrapping the reference C++ implementation (Kazhdan's code)
// or implementing from scratch:
// 1. Build octree from oriented points
// 2. Compute vector field from point normals
// 3. Solve Poisson equation (sparse linear system)
// 4. Extract iso-surface via marching cubes
// 5. Optional: trim low-density regions

// TODO: implement Ball Pivoting Algorithm
// Algorithm:
// 1. Seed triangle from three mutually closest points
// 2. For each boundary edge:
//    a. Pivot a ball of radius r around the edge
//    b. Find the first point the ball touches
//    c. Create a new triangle
//    d. Update boundary edges
// 3. Repeat with increasing radii for multi-scale reconstruction

// TODO: implement alpha shapes
// Algorithm:
// 1. Compute Delaunay tetrahedralization
// 2. Remove tetrahedra with circumradius > 1/alpha
// 3. Extract boundary triangles as the alpha shape surface
