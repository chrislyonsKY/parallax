// Normal estimation and orientation.
//
// - PCA-based normal estimation from k nearest neighbors
// - MST-based consistent orientation
// - Viewpoint-based orientation

use extendr_api::prelude::*;

use crate::utils::{
    covariance, knn, points_to_rmatrix, radius_search, rmatrix_to_points, smallest_eigenvector_symmetric,
};

/// Estimate per-point normals from local neighborhoods.
#[extendr]
pub fn parallax_estimate_normals(xyz: RMatrix<f64>, k_neighbors: i32, radius: f64) -> RMatrix<f64> {
    let points = rmatrix_to_points(&xyz);
    let k_neighbors = k_neighbors.max(1) as usize;
    let radius = if radius.is_finite() && radius > 0.0 {
        Some(radius)
    } else {
        None
    };

    let normals: Vec<[f64; 3]> = points
        .iter()
        .enumerate()
        .map(|(idx, point)| {
            let neighbor_idx = neighborhood_indices(&points, idx, point, k_neighbors, radius);
            if neighbor_idx.len() < 3 {
                return [0.0, 0.0, 1.0];
            }

            let neighborhood: Vec<[f64; 3]> = neighbor_idx.iter().map(|i| points[*i]).collect();
            let cov = covariance(&neighborhood);
            let normal = smallest_eigenvector_symmetric(cov);
            if normal == [0.0, 0.0, 0.0] {
                [0.0, 0.0, 1.0]
            } else {
                normal
            }
        })
        .collect();

    points_to_rmatrix(&normals)
}

fn neighborhood_indices(
    points: &[[f64; 3]],
    idx: usize,
    point: &[f64; 3],
    k_neighbors: usize,
    radius: Option<f64>,
) -> Vec<usize> {
    if let Some(radius) = radius {
        let result = radius_search(points, point, radius, Some(idx));
        if result.len() >= 3 {
            let mut indices = vec![idx];
            indices.extend(result.into_iter().map(|(neighbor_idx, _)| neighbor_idx));
            return indices;
        }
    }

    let result = knn(points, point, k_neighbors.max(2), Some(idx));
    let mut indices = vec![idx];
    indices.extend(result.into_iter().map(|(neighbor_idx, _)| neighbor_idx));
    indices
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn estimated_plane_normals_are_vertical() {
        let points = vec![
            [0.0, 0.0, 0.0],
            [1.0, 0.0, 0.0],
            [0.0, 1.0, 0.0],
            [1.0, 1.0, 0.0],
            [0.5, 0.5, 0.0],
        ];
        let normal = {
            let neighborhood: Vec<[f64; 3]> = points.clone();
            smallest_eigenvector_symmetric(covariance(&neighborhood))
        };
        assert!(normal[2].abs() > 0.9);
    }
}

extendr_module! {
    mod normals;
    fn parallax_estimate_normals;
}
