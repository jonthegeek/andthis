test_that("repo_url returns this repo url", {
  local_mocked_bindings(
    repo_tree = function(path = ".") {
      list(username = "jonthegeek", repo = "andthis")
    }
  )
  expect_equal(
    repo_url(),
    "https://github.com/jonthegeek/andthis"
  )
})

test_that("repo_url() errors for bad repo", {
  local_mocked_bindings(
    repo_tree = function(...) {
      NULL
    }
  )
  expect_error(
    browse_repo(here::here()),
    "No GitHub remote found for",
    class = "andthis-error-no_github_remote"
  )
})
