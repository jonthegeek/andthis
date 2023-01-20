#' Create a Jon Package
#'
#' @param name The name of the package
#' @param ... Additional parameters passed on to [usethis::create_package()].
#'
#' @inherit usethis::create_package return
#' @export
create_package <- function(name, ...) {
  path <- fs::path(getOption("usethis.destdir"), name)
  ui_todo("Run `finish_package_setup()` in the new project.")
  usethis::create_package(path, ...)
}

#' Run the Rest of the Things
#'
#' This must execute in a package directory. It assumes a lot.
#'
#' @param r4ds Whether to create the package under the r4ds organization
#'   (default `TRUE`).
#'
#' @return The path to the package, probably.
#' @export
finish_package_setup <- function(r4ds = TRUE) {
  use_testthat()
  use_mit_license()
  use_tidy_description()
  use_package_doc(open = FALSE)
  ui_line("\n\nuse_git() -----------------------------------------------------")
  ui_info("It's ok to commit, but don't restart!")
  use_git()
  ui_line("\n\nuse_github() --------------------------------------------------")
  ui_info("Say yes.")

  organization <- NULL
  if (r4ds) {
    organization <- "r4ds"
  }
  use_github(organisation = organization)

  # Ok, NOW we can create the README.
  use_readme_rmd(open = FALSE)
  use_lifecycle_badge("experimental")
  use_cran_badge()
  use_cran_comments(open = FALSE)

  ### This block semi-replicates usethis::use_tidy_github() ------------------

  use_directory(".github", ignore = TRUE)
  use_git_ignore("*.html", directory = ".github")

  # Get info about the project so far. We'll probably redo this occasionally.
  base_path <- proj_get()
  desc <- desc::description$new(base_path)
  desc <- as.list(desc$get(desc$fields()))
  pkg_name <- desc$Package
  gh_short_url <- paste(c(organization, pkg_name), collapse = "/")
  pkg_data <- list(Package = pkg_name, github_spec = gh_short_url)

  use_template(
    template = "contributing.md",
    save_as = fs::path(".github", "CONTRIBUTING.md"),
    data = pkg_data,
    open = FALSE,
    package = "andthis"
  )

  use_directory(fs::path(".github", "ISSUE_TEMPLATE"))
  use_template(
    template = "issue_template.md",
    save_as = fs::path(".github", "ISSUE_TEMPLATE", "issue_template.md"),
    open = FALSE,
    package = "andthis"
  )

  use_template(
    template = "support.md",
    save_as = fs::path(".github", "SUPPORT.md"),
    data = pkg_data,
    open = FALSE,
    package = "andthis"
  )

  use_code_of_conduct(contact = "jonthegeek@gmail.com", path = ".github")

  ### This block semi-replicates usethis::use_tidy_github_actions() ------------
  use_coverage()
  use_github_action_check_standard()
  use_github_action_pr_commands()
  use_github_action("pkgdown")
  use_github_action("test-coverage")

  ### Run the last couple calls.
  use_tidy_github_labels()
  use_pkgdown_github_pages()

  ### Clean up files that have been touched.
  use_tidy_description()
  devtools::document()
  devtools::build_readme()

  ui_todo("Run `protect_main()` when you've checked this all in.")
  ui_todo("The code of conduct isn't added to the README automatically.")
  ui_todo("Fix that manually, or delete the COC file and rerun.")
  ui_todo("A restart of RStudio is required to activate the Git pane")
  if (ui_yeah("Restart now?")) {
    rstudioapi::openProject(proj_get())
  }
}

#' Stop yourself from checking into the main branch
#'
#' @return `NULL` (invisibly).
#' @export
protect_main <- function() {
  no_main <- paste(
    "#!/bin/sh",
    'branch="$(git rev-parse --abbrev-ref HEAD)"',
    'if [ "$branch" = "main" -o "$branch" = "master" ]; then',
    '  echo "Do not commit directly to the main branch"',
    '  exit 1',
    'fi',
    sep = "\n"
  )
  use_git_hook("pre-commit", no_main)
}

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
