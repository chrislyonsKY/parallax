// Point cloud filtering algorithms.
//
// - Voxel grid downsampling
// - Statistical Outlier Removal (SOR)
// - Radius outlier removal

use std::collections::BTreeMap;

use extendr_api::prelude::*;

use crate::utils::{indices_to_integers, knn, points_to_rmatrix, radius_search, rmatrix_to_points};

/// Downsample a point cloud using voxel centroids and return representative indices.
#[extendr]
pub fn parallax_voxel_downsample(xyz: RMatrix<f64>, voxel_size: f64) -> List {
    let points = rmatrix_to_points(&xyz);
    if points.is_empty() || voxel_size <= 0.0 {
        return list!(xyz = points_to_rmatrix(&[]), index = Integers::new(0));
    }

    let mins = [
        points.iter().map(|p| p[0]).fold(f64::INFINITY, f64::min),
        points.iter().map(|p| p[1]).fold(f64::INFINITY, f64::min),
        points.iter().map(|p| p[2]).fold(f64::INFINITY, f64::min),
    ];

    let mut voxels: BTreeMap<(i64, i64, i64), ([f64; 3], usize, usize)> = BTreeMap::new();

    for (idx, point) in points.iter().enumerate() {
        let key = (
            ((point[0] - mins[0]) / voxel_size).floor() as i64,
            ((point[1] - mins[1]) / voxel_size).floor() as i64,
            ((point[2] - mins[2]) / voxel_size).floor() as i64,
        );

        let entry = voxels.entry(key).or_insert(([0.0, 0.0, 0.0], 0usize, idx));
        entry.0[0] += point[0];
        entry.0[1] += point[1];
        entry.0[2] += point[2];
        entry.1 += 1;
    }

    let downsampled: Vec<[f64; 3]> = voxels
        .values()
        .map(|(sum, count, _)| [sum[0] / *count as f64, sum[1] / *count as f64, sum[2] / *count as f64])
        .collect();
    let indices: Vec<usize> = voxels.values().map(|(_, _, first_idx)| *first_idx).collect();

    list!(xyz = points_to_rmatrix(&downsampled), index = indices_to_integers(&indices))
}

/// Return 1-based indices for points kept by statistical outlier removal.
#[extendr]
pub fn parallax_sor(xyz: RMatrix<f64>, k: i32, std_ratio: f64) -> Integers {
    let points = rmatrix_to_points(&xyz);
    if points.len() <= 1 {
        return indices_to_integers(&(0..points.len()).collect::<Vec<_>>());
    }

    let k = (k.max(1) as usize).min(points.len() - 1);
    let mean_distances: Vec<f64> = points
        .iter()
        .enumerate()
        .map(|(idx, point)| {
            let neighbors = knn(&points, point, k, Some(idx));
            neighbors.iter().map(|(_, dist)| *dist).sum::<f64>() / (neighbors.len() as f64)
        })
        .collect();

    let mean = mean_distances.iter().sum::<f64>() / (mean_distances.len() as f64);
    let variance = mean_distances
        .iter()
        .map(|dist| {
            let centered = dist - mean;
            centered * centered
        })
        .sum::<f64>()
        / (mean_distances.len() as f64);
    let threshold = mean + std_ratio * variance.sqrt();

    let keep: Vec<usize> = mean_distances
        .iter()
        .enumerate()
        .filter_map(|(idx, dist)| if *dist <= threshold { Some(idx) } else { None })
        .collect();

    indices_to_integers(&keep)
}

/// Return 1-based indices for points kept by radius outlier removal.
#[extendr]
pub fn parallax_radius_outlier(xyz: RMatrix<f64>, radius: f64, min_neighbors: i32) -> Integers {
    let points = rmatrix_to_points(&xyz);
    if points.is_empty() {
        return Integers::new(0);
    }

    let min_neighbors = min_neighbors.max(1) as usize;
    let keep: Vec<usize> = points
        .iter()
        .enumerate()
        .filter_map(|(idx, point)| {
            let neighbors = radius_search(&points, point, radius, Some(idx));
            if neighbors.len() >= min_neighbors {
                Some(idx)
            } else {
                None
            }
        })
        .collect();

    indices_to_integers(&keep)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn voxel_groups_points() {
        let points = vec![[0.0, 0.0, 0.0], [0.02, 0.02, 0.02], [1.0, 1.0, 1.0]];
        let mins = [0.0, 0.0, 0.0];
        let mut voxels: BTreeMap<(i64, i64, i64), usize> = BTreeMap::new();
        for point in &points {
            let key = (
                ((point[0] - mins[0]) / 0.1_f64).floor() as i64,
                ((point[1] - mins[1]) / 0.1_f64).floor() as i64,
                ((point[2] - mins[2]) / 0.1_f64).floor() as i64,
            );
            *voxels.entry(key).or_insert(0) += 1;
        }
        assert_eq!(voxels.len(), 2);
    }

    #[test]
    fn sor_rejects_far_outlier() {
        let points = vec![
            [0.0, 0.0, 0.0],
            [0.1, 0.0, 0.0],
            [0.0, 0.1, 0.0],
            [10.0, 10.0, 10.0],
        ];
        let mean_distances: Vec<f64> = points
            .iter()
            .enumerate()
            .map(|(idx, point)| {
                let neighbors = crate::utils::knn(&points, point, 2, Some(idx));
                neighbors.iter().map(|(_, dist)| *dist).sum::<f64>() / (neighbors.len() as f64)
            })
            .collect();
        assert!(mean_distances[3] > mean_distances[0]);
    }
}

extendr_module! {
    mod filters;
    fn parallax_voxel_downsample;
    fn parallax_sor;
    fn parallax_radius_outlier;
}
