#' Create an R4DS book club
#'
#' @param book_abbr The short name for the book. Should match the channel.
#'
#' @return The path to the new project, invisibly.
#' @export
create_club <- function(book_abbr) {
  # Get the data first (to make sure this book is ready).
  book_data <- .get_book_data(book_abbr)

  path <- .create_club_from_gh(book_abbr)

  # Set the new project as the active project until this function completes.
  local_project(path)

  # From here on out I'm writing within a directory I just created, so
  # overwriting is ok.
  withr::local_options(list(usethis.overwrite = TRUE))

  .apply_book_data(book_data)

  ### Send it all to github.
  gert::git_add(gert::git_status(FALSE)$file)
  gert::git_commit_all("Finish setup.")
  gert::git_push()

  ### Don't let anything else be done at main.
  protect_main()

  pages_url <- cli::style_hyperlink(
    "GitHub pages",
    "https://github.com/r4ds/bookclub-{book_abbr}/settings/pages"
  )
  ui_warn(
    "Be sure to set up {pages_url}!"
  )

  return(invisible(proj_path()))
}

.get_book_data <- function(book_abbr) {
  # Make sure the data is ready for this club.
  book_data <- googlesheets4::read_sheet(
    .club_gs4_sheet_id,
    sheet = "Approved Books",
    col_types = "c"
  ) |>
    dplyr::filter(
      .data$book_abbr == .env$book_abbr,
      .data$club_notes_needed == "y"
    ) |>
    # Get rid of columns that are for bookkeeping in the sheet.
    dplyr::select(
      -"request_timestamp",
      -"Amazon Search",
      -"sign_up_url",
      -"club_notes_needed",
      -"gh_pages_setting_done",
      -"redirect_done",
      -"club_notes_url"
    ) |>
    # Now that we have the columns we expect, we can apply formats.
    dplyr::mutate(
      print = as.logical(.data$print),
      cohort01_start_date = lubridate::as_date(.data$cohort01_start_date)
    )

  if (!nrow(book_data) || is.na(book_data$club_notes_done)) {
    spreadsheet_url <- cli::style_hyperlink(
      "spreadsheet",
      .club_gs4_sheet_approved
    )
    cli::cli_abort(
      c(
        x = "Data not ready for club creation.",
        i = "Visit the {spreadsheet_url} for details."
      )
    )
  }
  if (book_data$club_notes_done == "y") {
    cli::cli_abort(
      c(
        v = "Club site already ready.",
        i = cli::style_hyperlink(
          "shared slides",
          glue::glue("https://r4ds.io/{book_abbr}")
        ),
        i = cli::style_hyperlink(
          "repo",
          glue::glue("https://github.com/r4ds/bookclub-{book_abbr}")
        )
      )
    )
  }

  return(as.list(book_data))
}

.create_club_from_gh <- function(book_abbr) {
  full_name <- glue::glue("bookclub-{book_abbr}")
  gh_short_url <- glue::glue("r4ds/{full_name}")
  path <- fs::path(getOption("usethis.destdir"), full_name)

  # Eventually we should create the repo from the template automatically (if it
  # doesn't already exist).

  usethis::create_from_github(
    repo_spec = gh_short_url,
    open = FALSE
  )

  return(path)
}

.apply_book_data <- function(book_data) {
  fs::file_move(
    proj_path("bookclub-template.Rproj"),
    proj_path(
      glue::glue("bookclub-{book_data$book_abbr}.Rproj")
    )
  )

  purrr::walk(
    fs::dir_ls(proj_path()),
    .update_club_file,
    book_data = book_data
  )

  # Right now that misses .gitignore, so let's do that one specifically.
  .update_club_file(proj_path(".gitignore"), book_data)
}

.update_club_file <- function(path, book_data) {
  if (fs::path_file(path) == "99999.Rmd") {
    return(FALSE)
  }

  updated_contents <- strsplit(
    whisker::whisker.render(
      readLines(path, encoding = "UTF-8", warn = FALSE),
      book_data
    ),
    "\n"
  )[[1]]

  write_over(path, updated_contents, quiet = TRUE)
}

#' Copy File Template for Chapters
#'
#' Create a separate Rmd for each chapter, and apply the basic template
#' information.
#'
#' @param chapters A character vector of chapter titles. For now the assumption
#'   is that the vector contains numbered chapters starting with chapter 1.
#'
#' @return NULL (invisibly)
#' @export
create_club_chapter_rmds <- function(chapters) {
  # Again, I only try to write over files I create, and I explicitly don't
  # create files if they already exist.
  withr::local_options(list(usethis.overwrite = TRUE))

  if (fs::file_exists(here::here("99999.Rmd"))) {
    purrr::walk2(
      seq_along(chapters), chapters,
      .create_club_chapter_rmd_single
    )
  } else {
    cli::cli_abort(
      c(
        x = "Cannot find chapter source file 99999.Rmd.",
        i = "Are you in the book's project?"
      )
    )
  }
}

.create_club_chapter_rmd_single <- function(chapter_number, chapter_title) {
  formatted_chapter_number <- stringr::str_pad(chapter_number, 2, "left", "0")
  formatted_chapter_title <- tolower(.as_filename(chapter_title))
  chapter_rmd_path <- here::here(
    glue::glue("{formatted_chapter_number}_{formatted_chapter_title}.Rmd")
  )

  # Make the file. This will error if the file already exists.
  fs::file_copy(
    here::here("99999.Rmd"),
    chapter_rmd_path
  )

  # Add the data to the template.
  updated_contents <- strsplit(
    whisker::whisker.render(
      readLines(chapter_rmd_path, encoding = "UTF-8", warn = FALSE),
      data = list(chapter_title = chapter_title)
    ),
    "\n"
  )[[1]]

  write_over(chapter_rmd_path, updated_contents, quiet = TRUE)
}

.as_filename <- function(string) {
  # This is messy. Accented character and the like will be removed. But for now
  # it's good enough.
  string |>
    stringr::str_replace_all(" ", "-") |>
    stringr::str_remove_all("[^A-Za-z0-9-]")
}
