test_that("px_read errors on missing file", {
  expect_error(px_read("nonexistent.las"), "does not exist")
})

test_that("px_read errors on unsupported format", {
  tmp <- tempfile(fileext = ".docx")
  file.create(tmp)
  expect_error(px_read(tmp), "Unsupported")
  unlink(tmp)
})

test_that("px_read returns platform-specific subtype", {
  tmp <- tempfile(fileext = ".xyz")
  writeLines(c("0 0 0", "1 1 1", "2 2 2"), tmp)

  cloud <- px_read(tmp, platform = "aerial", subsample = 1)

  expect_s3_class(cloud, c("px_aerial", "px_cloud"))
  expect_equal(nrow(cloud$xyz), 3)

  unlink(tmp)
})
