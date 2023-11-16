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
