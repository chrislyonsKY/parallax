use extendr_api::prelude::*;

mod kdtree;
mod registration;
mod segmentation;
mod mesh;
mod features;
mod normals;
mod filters;
mod utils;

// TODO: register all Rust functions exposed to R
// Each module exports functions via #[extendr] that are callable from R
// via .Call(parallax_<function_name>, ...)

extendr_module! {
    mod parallax;

    use kdtree;
    use segmentation;
    use normals;
    use filters;
}
