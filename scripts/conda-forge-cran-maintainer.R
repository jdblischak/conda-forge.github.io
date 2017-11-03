#!/usr/bin/env Rscript

# Maintain conda-forge R packages from CRAN.
#
# Requirements:
#   conda-build 2
#   conda-smithy
#   GITHUB_PAT
#   SSH keys ~/.ssh
#
# To do:
#
# * Install separate conda environment like bioconda does (https://github.com/bioconda/bioconda-recipes/blob/master/simulate-travis.py)
# * Check for already existing Pull Request
# * Add a filter for maintainers

# Packages ---------------------------------------------------------------------

suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("gh"))
suppressPackageStartupMessages(library("git2r"))
suppressPackageStartupMessages(library("jsonlite"))
suppressPackageStartupMessages(library("purrr"))
suppressPackageStartupMessages(library("stringr"))
suppressPackageStartupMessages(library("whisker"))
suppressPackageStartupMessages(library("yaml"))

# Functions --------------------------------------------------------------------

obtain_cran_packages <- function(repos = "https://cran.rstudio.com/", ...) {
  cran <- available.packages(repos = repos, ...) %>%
    as.data.frame %>%
    select(package = Package, version = Version) %>%
    mutate(package = paste0("r-", str_to_lower(package)),
           version = as.numeric_version(version))
  return(cran)
}

obtain_anaconda_packages <- function() {
  # conda-search currently doesn't respect --override-channels. Packages in
  # defaults or the channels in .condarc are always included:
  # https://github.com/conda/conda/issues/5717
  cmd <- "conda search --json --override-channels -c conda-forge ^r-"
  anaconda <- fromJSON(system(cmd, intern = TRUE)) %>%
    map_df(`[`, c("name", "version", "build", "channel")) %>%
    filter(channel == "conda-forge") %>%
    mutate(version = str_replace_all(version, "_", "-")) %>%
    group_by(name) %>%
    summarize(version = max(as.numeric_version(version))) %>%
    rename(package = name) %>%
    as.data.frame()
  stopifnot(nrow(anaconda) == length(unique(anaconda$package)))
  return(anaconda)
}

# https://developer.github.com/v3/repos/forks/#create-a-fork
fork_repo <- function(repo, owner = "conda-forge") {
  fork <- gh("POST /repos/:owner/:repo/forks", owner = owner, repo = repo)
  return(fork)
}

create_feature_branch <- function(repo, name = "update") {
  stopifnot(class(repo) == "git_repository")
  git_log <- commits(r)
  b <- branch_create(commit = git_log[[1]], name = name)
  checkout(b)
  return(b)
}

rerender_feedstock <- function(path, max_attempts = 3) {
  if (Sys.which("conda-smithy") == "") {
    stop("You need to install conda-smithy to rerender the feedstock.\n",
         "    conda install -c conda-forge conda-smithy")
  }
  # Sometimes conda-smithy times out when downloading data from Anaconda Cloud
  attempts <- 0
  while(attempts < max_attempts) {
    out <- system2(command = "conda",
                   args = c("smithy", "rerender", "--commit", "auto",
                            "--feedstock_directory", path),
                   stdout = TRUE, stderr = TRUE)
    if (any(grepl("remote server error", out))) {
      attempts <- attempts + 1
    } else {
      return(out)
    }
  }
  stop(out, "\n\nconda-smithy failed when trying to rerender the feedstock")
}

create_cran_recipe <- function(pkg, path) {
  out <- system2("conda",
          args = c("skeleton", "cran", "--output-dir", path, pkg),
          stdout = TRUE, stderr = TRUE)
  recipe_path <- file.path(path, pkg, "meta.yaml")
  return(recipe_path)
}

read_recipe_yaml <- function(meta_lines) {
  valid_yaml <- paste(meta_lines[!str_detect(meta_lines, "\\{")],
                      collapse = "\n")
  list_yaml <- yaml.load(valid_yaml)
  return(list_yaml)
}

get_recipe_info <- function(recipe) {
  meta_lines <- readLines(recipe)
  list_yaml <- read_recipe_yaml(meta_lines)
  set_version <- meta_lines[str_detect(meta_lines, "set version")]
  sha256 <- meta_lines[str_detect(meta_lines, "sha256")]
  build_num <- list_yaml$build$number
  reqs_build <- list_yaml$requirements$build
  reqs_run <- list_yaml$requirements$run
  reqs_system <- meta_lines[str_detect(meta_lines, "SystemRequirements")]
  maintainers <- list_yaml$extra$`recipe-maintainers`
  return(list(set_version = set_version,
              sha256 = sha256,
              build_num = build_num,
              reqs_build = reqs_build,
              reqs_run = reqs_run,
              reqs_system = reqs_system,
              maintainers = maintainers))
}

update_cran_recipe <- function(recipe, old_info, new_info) {
  updated_lines <- readLines(recipe)
  updated_lines[str_which(updated_lines, "set version")] <- new_info$set_version
  updated_lines[str_which(updated_lines, "sha256")] <- new_info$sha256
  if (old_info$build_num != 0 && old_info$set_version != new_info$set_version) {
    updated_lines[str_which(updated_lines, "number: ")] <- "  number: 0"
  }
  writeLines(updated_lines, con = recipe)
  return(invisible(recipe))
}

commit_cran_recipe <- function(repo, recipe_string = "recipe/meta.yaml") {
  if (recipe_string %in% unlist(status(repo)$unstaged)) {
    add(repo, recipe_string)
    commit(repo, sprintf("MNT: Update version."))
    return(invisible(commits(repo)[[1]]))
  } else {
    return(invisible())
  }
}

create_pr_message <- function(rerender, update, old_info, new_info = NULL) {
  if (is.null(new_info)) {
    reqs_build_lost <- character(0)
    reqs_build_gained <- character(0)
    reqs_run_lost <- character(0)
    reqs_run_gained <- character(0)
    reqs_system_note <- character(0)
  } else {
    reqs_build_lost <- setdiff(old_info$reqs_build, new_info$reqs_build)
    reqs_build_gained <- setdiff(new_info$reqs_build, old_info$reqs_build)
    reqs_run_lost <- setdiff(old_info$reqs_run, new_info$reqs_run)
    reqs_run_gained <- setdiff(new_info$reqs_run, old_info$reqs_run)
    reqs_system_note <- new_info$reqs_system
  }
  template <-
"cc maintainer(s): {{maintainers}}

This is an automated PR generated by [conda-forge-cran-maintainer.R](https://github.com/jdblischak/conda-forge.github.io/blob/conda-forge-cran-maintainer/scripts/conda-forge-cran-maintainer.R)

Notes:

{{#rerender}}
* The feedstock was rerendered with conda-smithy (if there is no conda-smithy commit, then the feedstock was already up-to-date)
{{/rerender}}
{{#update}}
* The recipe was updated to the most recent version on CRAN
{{/update}}
{{#reqs_build_lost_flag}}
* The following build requirements from the current recipe were not included in the latest recipe created by `conda skeleton cran`: {{reqs_build_lost}}
{{/reqs_build_lost_flag}}
{{#reqs_build_gained_flag}}
* The following build requirements are not in the current recipe but were included in the recipe created by `conda skeleton cran`: {{reqs_build_gained}}
{{/reqs_build_gained_flag}}
{{#reqs_run_lost_flag}}
* The following run requirements from the current recipe were not included in the latest recipe created by `conda skeleton cran`: {{reqs_run_lost}}
{{/reqs_run_lost_flag}}
{{#reqs_run_gained_flag}}
* The following run requirements are not in the current recipe but were included in the recipe created by `conda skeleton cran`: {{reqs_run_gained}}
{{/reqs_run_gained_flag}}
{{#reqs_systems_note_flag}}
* The CRAN entry notes the following system requirements: {{reqs_system_note}}
{{/reqs_systems_note_flag}}
"
  data <- list(maintainers = paste0(c("m1", "m2", "m3"), collapse = ", "), #paste0("@", old_info$maintainers),
               rerender = rerender,
               update = update,
               reqs_build_lost =  paste0(reqs_build_lost, collapse = ", "),
               reqs_build_gained =  paste0(reqs_build_gained, collapse = ", "),
               reqs_run_lost =  paste0(reqs_run_lost, collapse = ", "),
               reqs_run_gained =  paste0(reqs_run_gained, collapse = ", "),
               reqs_system_note =  reqs_system_note,
               reqs_build_lost_flag = length(reqs_build_lost) > 0,
               reqs_build_gained_flag = length(reqs_build_gained) > 0,
               reqs_run_lost_flag = length(reqs_run_lost) > 0,
               reqs_run_gained_flag = length(reqs_run_gained) > 0,
               reqs_system_note_flag = length(reqs_system_note) > 0)
  text <- whisker.render(template, data)
  return(text)
}

submit_pull_request <- function(owner, repo, title, head, base, body,
                                maintainer_can_modify = TRUE) {
  pr <- gh("POST /repos/:owner/:repo/pulls", owner = owner, repo = repo,
           title = title, head = head, base = base, body = body,
           maintainer_can_modify = maintainer_can_modify)
  return(pr)
}

# Arguments --------------------------------------------------------------------

packages <- "r-anomalydetection"
maintainers <- "jdblischak"
all <- FALSE
limit <- 10
dry_run <- TRUE
cb2_path <- "/tmp/cb2"
local_path <- NULL
rerender <- TRUE

# Script -----------------------------------------------------------------------

cran <- obtain_cran_packages()
anaconda <- obtain_anaconda_packages()
conda_forge <- merge(anaconda, cran, by = "package",
                     suffixes = c(".conda", ".cran")) %>%
  mutate(outdated = version.conda < version.cran)
cat(sprintf("%d of %d CRAN R packages on conda-forge are outdated.\n",
            sum(conda_forge$outdated), nrow(conda_forge)))

login <- gh_whoami()

if (all) {
  packages <- sort(unique(c(packages, conda_forge$package[conda_forge$outdated])))
}
if (is.null(local_path)) {
  local_path <- tempdir()
}
packages <- packages[seq_len(min(length(packages), limit))]

for (pkg in packages) {
  cat(sprintf("Processing %s\n", pkg))
  feedstock <- paste0(pkg, "-feedstock")
  fork <- fork_repo(feedstock)
  r <- clone(fork$ssh_url, local_path = file.path(local_path, fork$name))
  b <- create_feature_branch(r)
  if (rerender) {
    conda_smithy <- rerender_feedstock(workdir(r))
  }
  recipe_old <- file.path(workdir(r), "recipe", "meta.yaml")
  recipe_old_info <- get_recipe_info(recipe_old)
  if (conda_forge$outdated[conda_forge$package == pkg]) {
    recipe_new <- create_cran_recipe(pkg, local_path)
    recipe_new_info <- get_recipe_info(recipe_new)
    update_cran_recipe(recipe_old, recipe_old_info, recipe_new_info)
    commit_cran_recipe(r)
    update <- TRUE
  } else {
    update <- FALSE
  }
  # Only push and pull request if new commits have been made
  if (branch_target(b) != branch_target(branches(r)$master)) {
    push(r, name = "origin", refspec = paste0("refs/heads/", b@name))
    if (update) {
      pr_message <- create_pr_message(rerender = rerender, update = update,
                                      old_info = recipe_old_info,
                                      new_info = recipe_new_info)
    } else {
      pr_message <- create_pr_message(rerender = rerender, update = FALSE,
                                      old_info = recipe_old_info)
    }
    pr <- submit_pull_request(owner = "jdblischak", repo = feedstock,
                              title = "MNT: Update package version",
                              head = paste(login$login, b@name, sep = ":"),
                              base = "master",
                              body = pr_message)
  }
}

# if (!interactive()) {
#   args <- commandArgs(trailingOnly = TRUE)
#   main(args)
# }
