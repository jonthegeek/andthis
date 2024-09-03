#' Install the current repo
#'
#' Install from GitHub to make sure things like renv understand where this
#' package is coming from.
#'
#' @return The result of the installation, invisibly.
#' @export
install_this <- function() {
  remotes <- usethis::git_remotes()
  target <- remotes$upstream %||% remotes$origin
  if (is.null(target)) {
    cli::cli_abort("No github remotes found.")
  }
  cli::cli_inform("Installing from {target}.")
  pak::pak(target)
}
