#' Open this project's root directory
#'
#' Open the root directory of the current project in the system's file browser.
#'
#' @return The root directory (invisibly).
#' @export
browse_here <- function() {
  root <- here::here()
  browseURL(root)
  invisible(root)
}

#' Open this project's GitHub repo in browser
#'
#' Open the remote GitHub repository for the project at `path` in the system's
#' web browser.
#'
#' @param path Path to the root of the project.
#'
#' @returns The URL of the GitHub repository (invisibly).
#' @export
browse_repo <- function(path = ".") {
  repo_url <- repo_url(path)
  browseURL(repo_url)
  invisible(repo_url)
}

#' Open the current tempdir
#'
#' Open the current R session's tempdir in the system's file browser.
#'
#' @returns The path to the tempdir (invisibly).
#' @export
browse_temp <- function() {
  tmp <- tempdir()
  browseURL(tmp)
  invisible(tmp)
}
