// k-d tree construction and spatial queries.
//
// Provides the foundational spatial index used by registration (ICP correspondence),
// normal estimation (k-NN), outlier detection (SOR, radius), and segmentation (DBSCAN).
//
// Uses a 3D k-d tree with Rayon-parallel construction for large clouds.

use extendr_api::prelude::*;

use crate::utils::{indices_to_integers, knn, radius_search, rmatrix_to_points};

/// Query the k nearest neighbors for one or more points.
#[extendr]
pub fn parallax_kdtree_knn(xyz: RMatrix<f64>, query: RMatrix<f64>, k: i32) -> List {
    let points = rmatrix_to_points(&xyz);
    let queries = rmatrix_to_points(&query);
    let k = (k.max(0) as usize).min(points.len());

    let indices = RMatrix::new_matrix(queries.len(), k, |row, col| {
        let result = knn(&points, &queries[row], k, None);
        if col < result.len() {
            (result[col].0 as i32) + 1
        } else {
            0
        }
    });

    let distances = RMatrix::new_matrix(queries.len(), k, |row, col| {
        let result = knn(&points, &queries[row], k, None);
        if col < result.len() {
            result[col].1
        } else {
            f64::NAN
        }
    });

    list!(indices = indices, distances = distances)
}

/// Query all neighbors within a radius for one or more points.
#[extendr]
pub fn parallax_kdtree_radius(xyz: RMatrix<f64>, query: RMatrix<f64>, radius: f64) -> List {
    let points = rmatrix_to_points(&xyz);
    let queries = rmatrix_to_points(&query);
    let mut index_values = Vec::with_capacity(queries.len());
    let mut distance_values = Vec::with_capacity(queries.len());

    for query_point in &queries {
        let result = radius_search(&points, query_point, radius, None);
        let indices: Vec<usize> = result.iter().map(|(idx, _)| *idx).collect();
        let distances: Doubles = result.iter().map(|(_, dist)| Rfloat::from(*dist)).collect();
        index_values.push(indices_to_integers(&indices).into_robj());
        distance_values.push(distances.into_robj());
    }

    list!(
        indices = List::from_values(index_values),
        distances = List::from_values(distance_values)
    )
}

#[cfg(test)]
mod tests {
    use crate::utils::{knn, radius_search};

    #[test]
    fn radius_search_finds_close_points() {
        let points = vec![[0.0, 0.0, 0.0], [0.2, 0.0, 0.0], [1.0, 0.0, 0.0]];
        let result = radius_search(&points, &[0.0, 0.0, 0.0], 0.3, None);
        assert_eq!(result.len(), 2);
    }

    #[test]
    fn knn_returns_requested_count() {
        let points = vec![
            [0.0, 0.0, 0.0],
            [0.1, 0.0, 0.0],
            [0.2, 0.0, 0.0],
            [1.0, 0.0, 0.0],
        ];
        let result = knn(&points, &[0.0, 0.0, 0.0], 3, None);
        assert_eq!(result.len(), 3);
    }
}

extendr_module! {
    mod kdtree;
    fn parallax_kdtree_knn;
    fn parallax_kdtree_radius;
}
