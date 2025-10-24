test_that("browse_here() returns root", {
  local_mocked_bindings(
    browseURL = function(...) {
      invisible(NULL)
    }
  )
  expect_equal(browse_here(), here::here())
})

test_that("browse_repo() returns url of this github repo", {
  local_mocked_bindings(
    browseURL = function(...) {
      invisible(NULL)
    }
  )
  local_mocked_bindings(
    repo_tree = function(path = ".") {
      list(username = "jonthegeek", repo = "andthis")
    }
  )
  expect_equal(
    browse_repo(here::here()),
    "https://github.com/jonthegeek/andthis"
  )
})

test_that("browse_temp() returns tempdir", {
  local_mocked_bindings(
    browseURL = function(...) {
      invisible(NULL)
    }
  )
  expect_equal(
    browse_temp(),
    tempdir()
  )
})
