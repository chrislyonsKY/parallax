use extendr_api::prelude::*;

pub(crate) fn rmatrix_to_points(xyz: &RMatrix<f64>) -> Vec<[f64; 3]> {
    (0..xyz.nrows())
        .map(|row| [xyz[[row, 0]], xyz[[row, 1]], xyz[[row, 2]]])
        .collect()
}

pub(crate) fn points_to_rmatrix(points: &[[f64; 3]]) -> RMatrix<f64> {
    RMatrix::new_matrix(points.len(), 3, |row, col| points[row][col])
}

pub(crate) fn indices_to_integers(indices: &[usize]) -> Integers {
    Integers::from_values(indices.iter().map(|idx| (*idx as i32) + 1))
}

pub(crate) fn squared_distance(a: &[f64; 3], b: &[f64; 3]) -> f64 {
    let dx = a[0] - b[0];
    let dy = a[1] - b[1];
    let dz = a[2] - b[2];
    dx * dx + dy * dy + dz * dz
}

pub(crate) fn distance(a: &[f64; 3], b: &[f64; 3]) -> f64 {
    squared_distance(a, b).sqrt()
}

pub(crate) fn centroid(points: &[[f64; 3]]) -> [f64; 3] {
    if points.is_empty() {
        return [0.0, 0.0, 0.0];
    }

    let sum = points.iter().fold([0.0, 0.0, 0.0], |mut acc, point| {
        acc[0] += point[0];
        acc[1] += point[1];
        acc[2] += point[2];
        acc
    });

    let n = points.len() as f64;
    [sum[0] / n, sum[1] / n, sum[2] / n]
}

pub(crate) fn normalize(mut vector: [f64; 3]) -> [f64; 3] {
    let norm = (vector[0] * vector[0] + vector[1] * vector[1] + vector[2] * vector[2]).sqrt();
    if norm == 0.0 {
        return [0.0, 0.0, 0.0];
    }

    vector[0] /= norm;
    vector[1] /= norm;
    vector[2] /= norm;
    vector
}

pub(crate) fn covariance(points: &[[f64; 3]]) -> [[f64; 3]; 3] {
    let center = centroid(points);
    let mut cov = [[0.0; 3]; 3];

    if points.len() <= 1 {
        return cov;
    }

    for point in points {
        let x = point[0] - center[0];
        let y = point[1] - center[1];
        let z = point[2] - center[2];

        cov[0][0] += x * x;
        cov[0][1] += x * y;
        cov[0][2] += x * z;
        cov[1][0] += y * x;
        cov[1][1] += y * y;
        cov[1][2] += y * z;
        cov[2][0] += z * x;
        cov[2][1] += z * y;
        cov[2][2] += z * z;
    }

    let scale = 1.0 / ((points.len() - 1) as f64);
    for row in &mut cov {
        for value in row {
            *value *= scale;
        }
    }

    cov
}

pub(crate) fn smallest_eigenvector_symmetric(mut matrix: [[f64; 3]; 3]) -> [f64; 3] {
    let mut eigenvectors = [[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]];

    for _ in 0..24 {
        let mut p = 0usize;
        let mut q = 1usize;
        let mut max_off_diag = matrix[0][1].abs();

        for i in 0..3 {
            for j in (i + 1)..3 {
                if matrix[i][j].abs() > max_off_diag {
                    max_off_diag = matrix[i][j].abs();
                    p = i;
                    q = j;
                }
            }
        }

        if max_off_diag < 1e-12 {
            break;
        }

        let theta = (matrix[q][q] - matrix[p][p]) / (2.0 * matrix[p][q]);
        let t = if theta >= 0.0 {
            1.0 / (theta + (theta * theta + 1.0).sqrt())
        } else {
            -1.0 / (-theta + (theta * theta + 1.0).sqrt())
        };
        let c = 1.0 / (1.0 + t * t).sqrt();
        let s = t * c;

        let app = matrix[p][p];
        let aqq = matrix[q][q];
        let apq = matrix[p][q];

        matrix[p][p] = app - t * apq;
        matrix[q][q] = aqq + t * apq;
        matrix[p][q] = 0.0;
        matrix[q][p] = 0.0;

        for r in 0..3 {
            if r != p && r != q {
                let arp = matrix[r][p];
                let arq = matrix[r][q];
                matrix[r][p] = c * arp - s * arq;
                matrix[p][r] = matrix[r][p];
                matrix[r][q] = c * arq + s * arp;
                matrix[q][r] = matrix[r][q];
            }
        }

        for row in &mut eigenvectors {
            let vrp = row[p];
            let vrq = row[q];
            row[p] = c * vrp - s * vrq;
            row[q] = c * vrq + s * vrp;
        }
    }

    let mut smallest = 0usize;
    if matrix[1][1] < matrix[smallest][smallest] {
        smallest = 1;
    }
    if matrix[2][2] < matrix[smallest][smallest] {
        smallest = 2;
    }

    normalize([
        eigenvectors[0][smallest],
        eigenvectors[1][smallest],
        eigenvectors[2][smallest],
    ])
}

pub(crate) fn knn(points: &[[f64; 3]], query: &[f64; 3], k: usize, exclude: Option<usize>) -> Vec<(usize, f64)> {
    let mut distances: Vec<(usize, f64)> = points
        .iter()
        .enumerate()
        .filter(|(idx, _)| exclude.map_or(true, |skip| *idx != skip))
        .map(|(idx, point)| (idx, distance(point, query)))
        .collect();

    distances.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap_or(std::cmp::Ordering::Equal));
    distances.truncate(k.min(distances.len()));
    distances
}

pub(crate) fn radius_search(
    points: &[[f64; 3]],
    query: &[f64; 3],
    radius: f64,
    exclude: Option<usize>,
) -> Vec<(usize, f64)> {
    points
        .iter()
        .enumerate()
        .filter(|(idx, _)| exclude.map_or(true, |skip| *idx != skip))
        .map(|(idx, point)| (idx, distance(point, query)))
        .filter(|(_, dist)| *dist <= radius)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn smallest_eigenvector_finds_plane_normal() {
        let points = vec![
            [0.0, 0.0, 0.0],
            [1.0, 0.0, 0.0],
            [0.0, 1.0, 0.0],
            [1.0, 1.0, 0.0],
        ];
        let cov = covariance(&points);
        let normal = smallest_eigenvector_symmetric(cov);
        assert!(normal[2].abs() > 0.9);
    }

    #[test]
    fn knn_orders_neighbors_by_distance() {
        let points = vec![[0.0, 0.0, 0.0], [1.0, 0.0, 0.0], [2.0, 0.0, 0.0]];
        let result = knn(&points, &[0.1, 0.0, 0.0], 2, None);
        assert_eq!(result.len(), 2);
        assert_eq!(result[0].0, 0);
        assert_eq!(result[1].0, 1);
    }
}
