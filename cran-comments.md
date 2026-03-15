## Test environments

- Local macOS 26.3.1 (arm64), R 4.5.2
- GitHub Actions:
  - ubuntu-latest (release, devel)
  - macos-latest (release)
  - windows-latest (release)

## R CMD check results

Local `R CMD check --as-cran` on `parallax_0.1.0.tar.gz` currently reports:

- 0 ERROR
- 1 WARNING
- 5 NOTE

The remaining package-owned NOTE is from the current `extendr-api` /
`libR-sys` toolchain:

- compiled code note about non-API R entry points
  (`BODY`, `CLOENV`, `DATAPTR`, `ENCLOS`, `FORMALS`)

The remaining WARNING/NOTEs are environment-specific on the local machine:

- `checkbashisms` is not installed locally
- internet access is unavailable for CRAN incoming and URL checks
- local HTML Tidy is too old for HTML manual validation
- macOS temp detritus note for `xcrun_db`

## Resubmission

Initial submission.

## Notes

- Rust dependencies are vendored in `src/rust/vendor.tar.xz` for offline
  source builds.
- The package includes `configure` and `configure.win` scripts to locate
  `cargo` and `rustc` before compilation.
