#' Open an RStudio project
#'
#' Open an RStudio project by name. If the project does not exist, this function
#' will throw an error.
#'
#' @param project The name of an existing project.
#' @param new_session Whether to open the project in a new RStudio session.
#'
#' @return Called for side effects. Probably something invisibly.
#' @export
open_proj <- function(project, new_session = TRUE) {
  path <- fs::path(
    getOption("usethis.destdir", default = fs::path_home_r()),
    project
  )
  if (fs::dir_exists(path)) {
    return(rstudioapi::openProject(path, newSession = new_session))
  }
  cli::cli_abort("Project {project} does not exist.")
}

#' Open the RStudio project for a book club
#'
#' @param club The abbreviation for the club.
#' @inheritParams open_proj
#'
#' @inherit open_proj return
#' @export
club <- function(club, new_session = TRUE) {
  open_proj(glue::glue("bookclub-{club}"), new_session)
}
