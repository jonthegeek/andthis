#' Create a Jon Package
#'
#' @param pkg_name The name of the package
#' @param title What the package does in title case.
#' @param description A one-paragraph description of the package. Will be used
#'   to initially populate both the DESCRIPION and the README.
#' @param organization The GitHub organization to create the package under
#'   (leave as `NULL` to create under `jonthegeek`).
#'
#' @return `NULL`, invisibly.
#' @export
create_package <- function(pkg_name,
                           title = "What the Package Does",
                           description = "The goal of {pkg_name} is to...",
                           organization = NULL) {
  path <- fs::path(getOption("usethis.destdir"), pkg_name)
  title <- glue::glue(title)
  description <- glue::glue(description)
  usethis::create_package(
    path,
    fields = list(
      Title = title,
      Description = description,
      Language = "en-US"
    ),
    open = FALSE
  )

  # From here on out I'm writing within a directory I just created, so
  # overwriting is ok.
  withr::local_options(list(usethis.overwrite = TRUE))

  # Set the new project as the active project until this function completes.
  local_project(path)

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
  usethis::use_build_ignore("README.Rmd")

  use_testthat()
  use_mit_license()
  use_tidy_description()
  use_package_doc(open = FALSE)
  ui_line("\n\nuse_git() -----------------------------------------------------")
  ui_info("It's ok to commit.")
  use_git()
  protect_readme()

  ui_line("\n\nuse_github() --------------------------------------------------")
  ui_info("Say yes.")
  use_github(organisation = organization)

  # There's nothing else that's necessary to interact with, so make it think we
  # aren't interactive.
  old_interactive <- options(rlang_interactive = FALSE)
  withr::defer(options(old_interactive))

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
  use_github_action("check-standard")
  use_github_action("pr-commands")
  use_github_action("pkgdown")
  use_github_action("test-coverage")

  ### Run the last couple calls.
  use_tidy_github_labels() # TODO: Update these to my own list.
  use_pkgdown_github_pages()

  ### Update _pkgdown.yml
  use_pkgdown_lang("en-US")
  use_pkgdown_dev_auto()

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
    "if [[ ${#README[@]} != 0 ]]; then",
    "  if [[ README.Rmd -nt README.md ]]; then",
    '    echo -e "README.md is out of date; please re-knit README.Rmd\\n$NOVERIFYMSG"',
    "    exit 1",
    "  elif [[ ${#README[@]} -lt 2 ]]; then",
    '    echo -e "README.Rmd and README.md should both be staged.\\n$NOVERIFYMSG"',
    "    # No exit; sometimes you need to do this to fix things.",
    "  fi",
    "fi"
  )

  use_precommit_hook(
    hook = hook,
    priority = -1000 # Needs to be last.
  )
}

#' Add something to the precommit hook
#'
#' Unlike [usethis::use_git_hook()], this function checks to see if you already
#' have a pre-commit hook, and attempts to add to it while avoiding duplication.
#' The checks are imperfect, which is probably why this isn't in usethis this
#' way. It will also fail if something exits early on success before everything
#' has ran.
#'
#' @param hook Character. The hook to add, with one line of output per element
#'   of the vector.
#' @param priority Where to put the hook. By default the hook is added at the
#'   end. Higher-priority hooks will be added higher up.
#'
#' @return `NULL` (invisibly).
#' @export
use_precommit_hook <- function(hook, priority = 0) {
  if (any(stringr::str_starts(hook, "NOVERIFYMSG="))) {
    cli::cli_abort(
      "Hooks cannot define a variable named NOVERIFYMSG."
    )
  }
  prepared_hooks <- list(.prepare_hook(hook, priority))

  if (fs::file_exists(".git/hooks/pre-commit")) {
    existing_hooks <- .parse_existing_pre_commit()

    if (prepared_hooks %in% existing_hooks) {
      return(invisible(TRUE))
    }

    prepared_hooks <- c(existing_hooks, prepared_hooks)
  }



  hook <- c(
    "#!/bin/bash",
    "NOVERIFYMSG='use `git commit --no-verify` to override this check'",
    .finalize_hooks(prepared_hooks)
  )

  use_git_hook("pre-commit", hook)
}

#' Clean up a hook and prep it for use
#'
#' @param hook The hook as a character vector, with one line per element of the
#'   vector.
#' @param priority The integer priority of the hook. Lower-priority hooks will
#'   be placed later in the script.
#'
#' @return A character vector with class "hook," with the priority as an
#'   attribute.
#' @keywords internal
.prepare_hook <- function(hook, priority = 0) {
  # Get rid of starting boilerplate.
  hook <- setdiff(
    hook,
    "#!/bin/bash"
  )
  hook <- setdiff(
    hook,
    "#!/bin/sh"
  )
  if (hook[[1]] == "") {
    hook <- hook[-1]
  }

  return(
    structure(
      c(
        # Include a blank line to make things lay out nicely.
        "",
        "# HOOKSTART",
        glue::glue("# HOOKPRIORITY {priority}"),
        hook,
        "# HOOKEND"
      ),
      class = "hook",
      priority = priority
    )
  )
}

#' Load existing pre-commit hooks and prep for recombining
#'
#' @return A list of hooks.
#' @keywords internal
.parse_existing_pre_commit <- function() {
  existing_pre_commit <- readLines(
    proj_path(".git/hooks/pre-commit")
  )

  # Remove things that we'll add back later.
  existing_pre_commit <- existing_pre_commit[
    stringr::str_starts(existing_pre_commit, "NOVERIFYMSG=", TRUE)
  ]

  # Don't use setdiff for these! It gets rid of duplicates!
  existing_pre_commit <- existing_pre_commit[
    !(existing_pre_commit %in% c("#!/bin/bash", "#!/bin/sh"))
  ]

  if (length(existing_pre_commit) && existing_pre_commit[[1]] == "") {
    existing_pre_commit <- existing_pre_commit[-1]
  }

  if (length(existing_pre_commit)) {
    # Parse labeled blocks. At least for now, I assume they're well-formed.
    return(.prepare_existing_hooks(existing_pre_commit))
  } else {
    return(list())
  }
}

#' Prep existing hooks for recombining
#'
#' @param existing_pre_commit A character vector with one element per row of the
#'   existing pre-commit hook, minus the boilerplate at the start.
#'
#' @return A list of hooks.
#' @keywords internal
.prepare_existing_hooks <- function(existing_pre_commit) {
  parsed_parts <- .extract_precommit_blocks(existing_pre_commit)

  prepared_hooks <- parsed_parts$prepared_hooks

  # Remove rows that were extracted and see if anything is left over.
  uncovered <- setdiff(
    seq_along(existing_pre_commit),
    unlist(parsed_parts$covered_rows)
  )
  uncovered <- existing_pre_commit[uncovered]

  if (length(uncovered) && uncovered[[1]] == "") {
    uncovered <- uncovered[-1]
  }

  if (length(uncovered)) {
    # Treat this as a single hook with default priority.
    uncovered_hook <- list(.prepare_hook(uncovered))
    prepared_hooks <- c(prepared_hooks, uncovered_hook)
  }

  return(prepared_hooks)
}

#' Parse formatted hooks into a list of hooks
#'
#' @inheritParams .prepare_existing_hooks
#'
#' @return A list of hooks.
#' @keywords internal
.extract_precommit_blocks <- function(existing_pre_commit) {
  return(
    tibble::tibble(
      starts = which(
        stringr::str_starts(existing_pre_commit, "# HOOKSTART")
      ),
      ends = which(
        stringr::str_starts(existing_pre_commit, "# HOOKEND")
      )
    ) |>
      dplyr::mutate(
        hook = purrr::map2(
          .data$starts, .data$ends,
          \(start, end) {
            this_hook <- existing_pre_commit[start:(end - 1)]
            this_hook <- this_hook[-1]
            this_hook <- this_hook[-1]
            return(this_hook)
          }
        ),
        priority = stringr::str_subset(
          .env$existing_pre_commit, "^# HOOKPRIORITY"
        ) |>
          stringr::str_remove("^# HOOKPRIORITY") |>
          stringr::str_squish() |>
          as.integer(),
        prepared_hooks = purrr::map2(
          .data$hook, .data$priority,
          .prepare_hook
        ),
        covered_rows = purrr::map2(
          .data$starts, .data$ends,
          `:`
        )
      )
  )
}

#' Sort hooks and combine
#'
#' @param prepared_hooks A list of hooks.
#'
#' @return A character vector with the hook contents, in priority order.
#' @keywords internal
.finalize_hooks <- function(prepared_hooks) {
  priorities <- purrr::map_int(
    prepared_hooks,
    attr,
    "priority",
    exact = TRUE
  )

  return(
    unlist(prepared_hooks[sort.list(priorities, decreasing = TRUE)])
  )
}

#' Stop yourself from checking into the main branch
#'
#' @return `NULL` (invisibly).
#' @export
protect_main <- function() {
  hook <- c(
    'branch="$(git rev-parse --abbrev-ref HEAD)"',
    'if [ "$branch" = "main" -o "$branch" = "master" ]; then',
    '  echo -e "Do not commit directly to the main branch\\n$NOVERIFYMSG"',
    "  exit 1",
    "fi"
  )
  use_precommit_hook(
    hook = hook,
    priority = 1000
  )
}

use_pkgdown_lang <- function(lang = "en-US") {
  .add_pkgdown_yaml(lang = lang)
}

use_pkgdown_dev_auto <- function() {
  .add_pkgdown_yaml(development = list(mode = "auto"))
}

.add_pkgdown_yaml <- function(...) {
  pkgdown_yml <- fs::path("_pkgdown.yml")
  if (!fs::file_exists(pkgdown_yml)) {
    cli::cli_abort(
      "No _pkgdown.yml file found. Run `use_pkgdown_github_pages()` first."
    )
  }
  pkgdown_yml_contents <- yaml::read_yaml(pkgdown_yml)
  new_fields <- list(...)
  for (field in names(new_fields)) {
    pkgdown_yml_contents[[field]] <- new_fields[[field]]
  }
  yaml::write_yaml(pkgdown_yml_contents, pkgdown_yml)
}
