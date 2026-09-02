#' Mock Package Data Structure
#'
#' Defines a mock package structure for test dependency trees.
#'
#' @param name Character string; package name (must be >= 2 characters).
#' @param version Character string; package version (e.g. "1.0.0").
#' @param depends Character vector or list of mock_package objects.
#' @param imports Character vector or list of mock_package objects.
#' @param suggests Character vector or list of mock_package objects.
#'
#' @return A list of class "mock_package".
mock_package <- function(name,
                         version = "1.0.0",
                         depends = character(0L),
                         imports = character(0L),
                         suggests = character(0L)) {
  structure(
    list(
      name = name,
      version = version,
      depends = depends,
      imports = imports,
      suggests = suggests
    ),
    class = "mock_package"
  )
}

#' Helper to extract dependency strings from character vectors or mock objects
.format_dep_field <- function(dep_list) {
  if (is.null(dep_list) || length(dep_list) == 0L) {
    return(NULL)
  }

  entries <- vapply(dep_list, function(item) {
    if (inherits(item, "mock_package")) {
      item$name
    } else if (is.character(item)) {
      item
    } else {
      stop("Dependency item must be a character string or mock_package")
    }
  }, character(1L))

  paste(entries, collapse = ", ")
}

#' Create a Mock Package Library on Disk
#'
#' Instantiates a temporary library directory containing the specified mock
#' packages and builds lightweight installed package skeletons.
#'
#' @param ... One or more mock_package objects (or lists of mock_package).
#' @param lib_dir Optional directory path. Defaults to a temporary directory.
#'
#' @return A list of class "mock_library" containing:
#'   \itemize{
#'     \item \code{lib_dir}: Directory path to the mock library.
#'     \item \code{packages}: Named list of mock_package objects created.
#'     \item \code{cleanup}: Function to clean up the temporary directory.
#'   }
create_mock_library <- function(..., lib_dir = NULL) {
  raw_pkgs <- list(...)
  pkg_list <- list()

  flatten_pkgs <- function(items) {
    for (item in items) {
      if (inherits(item, "mock_package")) {
        pkg_list[[item$name]] <<- item
        for (fld in c("depends", "imports", "suggests")) {
          nested <- item[[fld]]
          if (is.list(nested)) {
            flatten_pkgs(nested)
          }
        }
      } else if (is.list(item)) {
        flatten_pkgs(item)
      }
    }
  }
  flatten_pkgs(raw_pkgs)

  if (is.null(lib_dir)) {
    lib_dir <- tempfile("mock_lib_")
  }
  if (!dir.exists(lib_dir)) {
    dir.create(lib_dir, recursive = TRUE)
  }

  src_dir <- tempfile("mock_src_")
  dir.create(src_dir, recursive = TRUE)

  # Sort packages topologically so dependencies are installed before dependents
  deps_graph <- list()
  for (pkg in pkg_list) {
    dep_str <- .format_dep_field(c(pkg$depends, pkg$imports))
    if (!is.null(dep_str) && nzchar(trimws(dep_str))) {
      parsed <- tools:::.split_dependencies(dep_str)
      deps_graph[[pkg$name]] <- setdiff(
        vapply(parsed, function(x) x$name, character(1L)),
        "R"
      )
    } else {
      deps_graph[[pkg$name]] <- character(0L)
    }
  }

  install_order <- multiLibrary:::.topological_sort(
    names(pkg_list),
    deps_graph
  )

  # Write source packages and install them into lib_dir in topological order
  for (pkg_name in install_order) {
    pkg <- pkg_list[[pkg_name]]
    p_src <- file.path(src_dir, pkg$name)
    dir.create(p_src, recursive = TRUE)
    dir.create(file.path(p_src, "R"), recursive = TRUE)

    desc_lines <- c(
      paste0("Package: ", pkg$name),
      paste0("Version: ", pkg$version),
      paste0("Title: Mock Package ", pkg$name),
      "Description: Mock package for testing dependency resolution.",
      "License: GPL-3"
    )

    dep_str <- .format_dep_field(pkg$depends)
    if (!is.null(dep_str)) {
      desc_lines <- c(desc_lines, paste0("Depends: ", dep_str))
    }

    imp_str <- .format_dep_field(pkg$imports)
    if (!is.null(imp_str)) {
      desc_lines <- c(desc_lines, paste0("Imports: ", imp_str))
    }

    sug_str <- .format_dep_field(pkg$suggests)
    if (!is.null(sug_str)) {
      desc_lines <- c(desc_lines, paste0("Suggests: ", sug_str))
    }

    writeLines(desc_lines, file.path(p_src, "DESCRIPTION"))
    writeLines(
      paste0("export(", pkg$name, "_fn)"),
      file.path(p_src, "NAMESPACE")
    )
    writeLines(
      sprintf("%s_fn <- function() '%s'", pkg$name, pkg$name),
      file.path(p_src, "R", "code.R")
    )

    # Install into lib_dir with R_LIBS pointing to lib_dir
    install_args <- c(
      "CMD", "INSTALL",
      "--no-docs",
      "--no-multiarch",
      "--no-test-load",
      "-l", lib_dir,
      p_src
    )
    system2(
      file.path(R.home("bin"), "R"),
      install_args,
      env = paste0("R_LIBS=", lib_dir),
      stdout = FALSE,
      stderr = FALSE
    )

    # Guarantee package metadata directory exists for stateless inspection
    p_dest <- file.path(lib_dir, pkg$name)
    if (!dir.exists(p_dest)) {
      dir.create(p_dest, recursive = TRUE)
      writeLines(desc_lines, file.path(p_dest, "DESCRIPTION"))
      writeLines(
        paste0("export(", pkg$name, "_fn)"),
        file.path(p_dest, "NAMESPACE")
      )
    }
  }

  # Clean up temporary source directory
  unlink(src_dir, recursive = TRUE)

  cleanup_fn <- function() {
    unlink(lib_dir, recursive = TRUE)
  }

  structure(
    list(
      lib_dir = normalizePath(lib_dir, mustWork = FALSE),
      packages = pkg_list,
      cleanup = cleanup_fn
    ),
    class = "mock_library"
  )
}

#' Compare Search Paths between library() and multiLibrary()
#'
#' Spawns isolated clean R sessions to verify whether standard library()
#' sequential loading produces an identical search() path to multiLibrary().
#'
#' @param target_pkgs Character vector of target package names.
#' @param mock_lib A mock_library object returned by create_mock_library().
#'
#' @return Logical TRUE if search paths are identical.
compare_search_paths <- function(target_pkgs, mock_lib) {
  r_bin <- file.path(R.home("bin"), "Rscript")

  # Standard sequential library calls
  lib_calls <- paste(
    sprintf("library(%s);", target_pkgs),
    collapse = " "
  )
  cmd_std_expr <- sprintf(
    ".libPaths(c(%s, .libPaths())); %s cat(paste(search(), collapse = ':::'))",
    shQuote(mock_lib$lib_dir),
    lib_calls
  )
  out_std <- system2(r_bin, c("-e", shQuote(cmd_std_expr)), stdout = TRUE)
  search_std <- strsplit(out_std[length(out_std)], ":::")[[1L]]

  # multiLibrary call
  multi_args <- paste(sprintf('"%s"', target_pkgs), collapse = ", ")
  cmd_multi_expr <- sprintf(
    paste0(
      ".libPaths(c(%s, .libPaths())); library(multiLibrary); ",
      "multiLibrary(%s, quietly = TRUE); ",
      "cat(paste(search(), collapse = ':::'))"
    ),
    shQuote(mock_lib$lib_dir),
    multi_args
  )
  out_multi <- system2(r_bin, c("-e", shQuote(cmd_multi_expr)), stdout = TRUE)
  search_multi <- strsplit(out_multi[length(out_multi)], ":::")[[1L]]

  # Remove multiLibrary itself from the search path comparison
  search_multi_clean <- setdiff(search_multi, "package:multiLibrary")

  identical(search_std, search_multi_clean)
}
