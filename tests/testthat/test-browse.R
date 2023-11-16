test_that("browse_here() returns root", {
  local_mocked_bindings(
    browseURL = function(...) {
      invisible(NULL)
    }
  )
  expect_equal(browse_here(), here::here())
})
