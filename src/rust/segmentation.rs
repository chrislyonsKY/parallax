// Ground classification and clustering algorithms.
//
// - CSF (Cloth Simulation Filter): Zhang et al. 2016
// - PMF (Progressive Morphological Filter): Zhang et al. 2003
// - DBSCAN for object clustering
//
// CSF is the primary ground classification algorithm. It simulates a cloth
// draped over an inverted point cloud; points near the cloth are ground.

use extendr_api::prelude::*;

use crate::utils::{radius_search, rmatrix_to_points};

/// Cluster points using a native DBSCAN implementation.
#[extendr]
pub fn parallax_dbscan(xyz: RMatrix<f64>, eps: f64, min_points: i32) -> Integers {
    let points = rmatrix_to_points(&xyz);
    let labels = dbscan(&points, eps, min_points.max(1) as usize);
    Integers::from_values(labels)
}

fn dbscan(points: &[[f64; 3]], eps: f64, min_points: usize) -> Vec<i32> {
    let mut labels = vec![0_i32; points.len()];
    let mut visited = vec![false; points.len()];
    let mut cluster_id = 0_i32;

    for idx in 0..points.len() {
        if visited[idx] {
            continue;
        }

        visited[idx] = true;
        let seed_neighbors = neighbors(points, idx, eps);
        if seed_neighbors.len() + 1 < min_points {
            labels[idx] = -1;
            continue;
        }

        cluster_id += 1;
        labels[idx] = cluster_id;
        let mut seeds = seed_neighbors;
        let mut cursor = 0usize;

        while cursor < seeds.len() {
            let point_idx = seeds[cursor];
            if !visited[point_idx] {
                visited[point_idx] = true;
                let point_neighbors = neighbors(points, point_idx, eps);
                if point_neighbors.len() + 1 >= min_points {
                    for neighbor_idx in point_neighbors {
                        if !seeds.contains(&neighbor_idx) {
                            seeds.push(neighbor_idx);
                        }
                    }
                }
            }

            if labels[point_idx] <= 0 {
                labels[point_idx] = cluster_id;
            }
            cursor += 1;
        }
    }

    labels
}

fn neighbors(points: &[[f64; 3]], idx: usize, eps: f64) -> Vec<usize> {
    radius_search(points, &points[idx], eps, Some(idx))
        .into_iter()
        .map(|(neighbor_idx, _)| neighbor_idx)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dbscan_finds_two_clusters() {
        let points = vec![
            [0.0, 0.0, 0.0],
            [0.1, 0.0, 0.0],
            [0.0, 0.1, 0.0],
            [5.0, 5.0, 0.0],
            [5.1, 5.0, 0.0],
            [5.0, 5.1, 0.0],
        ];
        let labels = dbscan(&points, 0.25, 2);
        assert_eq!(labels[0], labels[1]);
        assert_eq!(labels[3], labels[4]);
        assert_ne!(labels[0], labels[3]);
    }
}

extendr_module! {
    mod segmentation;
    fn parallax_dbscan;
}
