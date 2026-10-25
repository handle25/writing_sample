# 00_load_workspace.R
# Install missing packages, then load them in the original order.
# Run with source(...) or Rscript from a terminal.

workspace_packages <- c(
  "tigris", "sf", "ggplot2", "data.table", "collapse", "modelsummary",
  "readxl", "fixest", "haven", "janitor", "future", "future.apply",
  "dplyr", "xtable", "lehdr", "bit64", "ivreg", "censusapi",
  "tidycensus", "sandwich", "patchwork", "lmtest"
)

# Create R's version-specific user library so installation does not need admin access.
workspace_user_library <- path.expand(strsplit(
  Sys.getenv("R_LIBS_USER"), .Platform$path.sep, fixed = TRUE
)[[1]][1])
if (is.na(workspace_user_library) || !nzchar(workspace_user_library)) {
  stop("R_LIBS_USER is not set. Set it to a writable R package library.")
}
if (!dir.exists(workspace_user_library)) {
  dir.create(workspace_user_library, recursive = TRUE, showWarnings = FALSE)
}
if (!dir.exists(workspace_user_library)) {
  stop("Cannot create R user library: ", workspace_user_library)
}
.libPaths(c(workspace_user_library, .libPaths()))

# A fixed CRAN URL avoids interactive mirror selection in terminal sessions.
workspace_repos <- getOption("repos")
if (!"CRAN" %in% names(workspace_repos) ||
    is.na(workspace_repos[["CRAN"]]) ||
    workspace_repos[["CRAN"]] %in% c("", "@CRAN@")) {
  workspace_repos["CRAN"] <- "https://cloud.r-project.org"
}

workspace_missing <- workspace_packages[!vapply(
  workspace_packages, requireNamespace, logical(1), quietly = TRUE
)]
if (length(workspace_missing)) {
  message("Installing missing packages: ", paste(workspace_missing, collapse = ", "))
  install.packages(
    workspace_missing,
    lib = workspace_user_library,
    repos = workspace_repos,
    dependencies = NA
  )
}

workspace_unavailable <- workspace_packages[!vapply(
  workspace_packages, requireNamespace, logical(1), quietly = TRUE
)]
if (length(workspace_unavailable)) {
  stop(
    "These packages could not be installed or loaded: ",
    paste(workspace_unavailable, collapse = ", "),
    ". See the installation messages above."
  )
}

for (workspace_package in workspace_packages) {
  library(workspace_package, character.only = TRUE)
}
