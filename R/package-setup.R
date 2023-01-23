#' Create a Jon Package
#'
#' @param pkg_name The name of the package
#' @param title What the package does in title case.
#' @param description A one-paragraph description of the package. Will be used
#'   to initially populate both the DESCRIPION and the README.
#' @param r4ds Whether to create the package under the r4ds organization
#'   (default `TRUE`).
#'
#' @return `NULL`, invisibly.
#' @export
create_package <- function(pkg_name,
                           title = "What the Package Does",
                           description = "The goal of {pkg_name} is to...",
                           r4ds = TRUE) {
  path <- fs::path(getOption("usethis.destdir"), pkg_name)
  title <- glue::glue(title)
  description <- glue::glue(description)
  usethis::create_package(
    path,
    fields = list(
      Title = title,
      Description = description
    ),
    open = FALSE
  )

  # Set the new project as the active project until this function completes.
  local_project(path)

  organization <- NULL
  if (r4ds) {
    organization <- "r4ds"
  }

  # Figure out where the project will be stored. Hard-coded and messy for
  # personal use.
  gh_root <- organization %||% "jonthegeek"

  gh_short_url <- paste(gh_root, pkg_name, sep = "/")

  # Set up a data object to use with various templates.
  pkg_data <- list(
    Package = pkg_name,
    github_spec = gh_short_url,
    description = description,
    gh_root = gh_root,
    Rmd = TRUE
  )

  use_template(
    template = "package-README",
    save_as = "README.Rmd",
    data = pkg_data,
    package = "andthis"
  )

  use_testthat()
  use_mit_license()
  use_tidy_description()
  use_package_doc(open = FALSE)
  ui_line("\n\nuse_git() -----------------------------------------------------")
  ui_info("It's ok to commit, but don't restart!")
  use_git()
  protect_readme()

  ui_line("\n\nuse_github() --------------------------------------------------")
  ui_info("Say yes.")
  use_github(organisation = organization)

  use_lifecycle_badge("experimental")
  use_cran_badge()
  use_cran_comments(open = FALSE)

  ### This block semi-replicates usethis::use_tidy_github() ------------------

  use_directory(".github", ignore = TRUE)
  use_git_ignore("*.html", directory = ".github")

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

  ### Send it all to github.
  gert::git_add(gert::git_status(FALSE)$file)
  gert::git_commit_all("Finish setup.")
  gert::git_push()

  ### Don't let anything else be done at main.
  protect_main()

  ui_line("\n\n")
  ui_warn("GitHub actions will fail until you have actual tests.")

  rstudioapi::openProject(proj_get(), newSession = TRUE)
}

#' Make sure README.md is rendered
#'
#' @return `NULL` (invisibly).
#' @export
protect_readme <- function() {
  hook <- c(
    "README=($(git diff --cached --name-only | grep -Ei '^README\\.[R]?md$'))",
    "MSG=\"use 'git commit --no-verify' to override this check\"",
    "", "if [[ ${#README[@]} == 0 ]]; then", "  exit 0", "fi", "",
    "if [[ README.Rmd -nt README.md ]]; then",
    "  echo -e \"README.md is out of date; please re-knit README.Rmd\\n$MSG\"",
    "  exit 1", "elif [[ ${#README[@]} -lt 2 ]]; then",
    "  echo -e \"README.Rmd and README.md should be both staged\\n$MSG\"",
    "  exit 1", "fi"
  )
  use_precommit_hook(hook)
}

#' Add something to the precommit hook
#'
#' Unlike [usethis::use_git_hook()], this function checks to see if you already
#' have a pre-commit hook, and attempts to add to it while avoiding duplication.
#' The checks are imperfect, which is probably why this isn't in usethis this
#' way.
#'
#' @param hook Character. The hook to add, with one line of output per element
#'   of the vector.
#'
#' @return `NULL` (invisibly).
#' @export
use_precommit_hook <- function(hook) {
  if (fs::file_exists(".git/hooks/pre-commit")) {
    existing_pre_commit <- readLines(".git/hooks/pre-commit")
    existing_pre_commit <- setdiff(
      existing_pre_commit,
      "#!/bin/bash"
    )
    existing_pre_commit <- setdiff(
      existing_pre_commit,
      "#!/bin/sh"
    )

    if (all(hook %in% existing_pre_commit)) {
      return(invisible(TRUE))
    } else {
      hook <- c(existing_pre_commit, hook)
    }
  }

  hook <- c("#!/bin/bash", hook)

  use_git_hook("pre-commit", hook)
}

#' Stop yourself from checking into the main branch
#'
#' @return `NULL` (invisibly).
#' @export
protect_main <- function() {
  hook <- c(
    'branch="$(git rev-parse --abbrev-ref HEAD)"',
    'if [ "$branch" = "main" -o "$branch" = "master" ]; then',
    '  echo "Do not commit directly to the main branch"',
    '  exit 1',
    'fi'
  )
  use_precommit_hook(hook)
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
