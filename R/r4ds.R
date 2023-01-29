#' Create an R4DS book club
#'
#' @param book_abbr The short name for the book. Should match the channel.
#'
#' @return The path to the new project, invisibly.
#' @export
create_club <- function(book_abbr) {
  repo <- paste0(
    "r4ds/bookclub-",
    book_abbr
  )
  create_from_github(repo)
}
