#' Install the current repo
#'
#' Install from GitHub to make sure things like renv understand where this
#' package is coming from.
#'
#' @return The result of the installation, invisibly.
#' @export
install_this <- function() {
  # nocov start
  remotes <- usethis::git_remotes()
  target <- remotes$upstream %||% remotes$origin
  if (is.null(target)) {
    cli::cli_abort("No github remotes found.")
  }
  cli::cli_inform("Installing from {target}.")
  pak::pak(target)
  # nocov end
}

#' Return the URL for a GitHub repo
#'
#' Return the URL for the GitHub repository for the project at `path`.
#'
#' @param path Path to the root of a project that has a GitHub remote.
#'
#' @returns The URL of the GitHub repository.
#' @export
repo_url <- function(path = ".") {
  path <- fs::path_abs(path)
  remote <- repo_tree(path)
  if (length(remote)) {
    return(paste0("https://github.com/", paste(remote, collapse = "/")))
  }
  cli::cli_abort(
    "No GitHub remote found for {.path {path}}.",
    class = "andthis-error-no_github_remote"
  )
}

#' Wrapper of gh_tree_remote for testing
#'
#' @inheritParams gh::gh_tree_remote
#' @inherit gh::gh_tree_remote return
#' @keywords internal
repo_tree <- function(path = ".") {
  gh::gh_tree_remote(path) # nocov
}
