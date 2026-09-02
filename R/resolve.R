#' @keywords internal
.find_and_read_desc <- function(pkg, lib.loc, cache = NULL) {
  if (!is.null(cache) && exists(pkg, envir = cache, inherits = FALSE)) {
    return(get(pkg, envir = cache, inherits = FALSE))
  }

  # Find installed package directory (errors if not installed)
  pkg_path <- find.package(pkg, lib.loc = lib.loc, verbose = FALSE)

  desc <- utils::packageDescription(pkg, lib.loc = dirname(pkg_path))
  if (!inherits(desc, "packageDescription") || is.na(desc$Package)) {
    stop(sprintf("Corrupted or unreadable DESCRIPTION for package '%s'", pkg))
  }

  info <- list(
    desc = desc,
    version = numeric_version(desc$Version),
    path = normalizePath(pkg_path, mustWork = FALSE),
    lib = dirname(pkg_path)
  )

  # Cache this result so we don't need to lookup again in the future
  if (!is.null(cache)) {
    assign(pkg, info, envir = cache)
  }

  info
}


#' @keywords internal
.process_dependency_field <- function(raw_deps,
                                      type_label,
                                      parent_pkg,
                                      types_map,
                                      required_by_map,
                                      deps_graph,
                                      constraints_map,
                                      visited,
                                      in_queue,
                                      add_to_graph = TRUE) {
  if (is.null(raw_deps) || !nzchar(trimws(raw_deps))) {
    return(character(0L))
  }

  parsed <- tools:::.split_dependencies(raw_deps)
  new_pkgs <- character(0L)

  for (dep_entry in parsed) {
    dep_name <- dep_entry$name
    # The `Depends` field can contain the R version requirement itself
    if (dep_name == "R") {
      next
    }

    types_map[[dep_name]] <- unique(c(types_map[[dep_name]], type_label))
    required_by_map[[dep_name]] <- unique(
      c(required_by_map[[dep_name]], parent_pkg)
    )

    if (add_to_graph) {
      deps_graph[[parent_pkg]] <- unique(c(deps_graph[[parent_pkg]], dep_name))
    }

    if (!is.null(dep_entry$version)) {
      op_str <- dep_entry$op %||% ">="
      constraints_map[[dep_name]] <- c(
        constraints_map[[dep_name]],
        list(list(
          op = op_str,
          version = numeric_version(dep_entry$version),
          by = parent_pkg
        ))
      )
    }

    if (is.null(visited[[dep_name]]) && is.null(in_queue[[dep_name]])) {
      new_pkgs <- c(new_pkgs, dep_name)
      in_queue[[dep_name]] <- TRUE
    }
  }

  new_pkgs
}

#' @keywords internal
.topological_sort <- function(nodes, deps_graph) {
  in_degree <- list()
  for (p in nodes) {
    in_degree[[p]] <- 0L
  }
  for (parent in names(deps_graph)) {
    for (child in deps_graph[[parent]]) {
      if (child %in% names(in_degree)) {
        in_degree[[parent]] <- in_degree[[parent]] + 1L
      }
    }
  }

  sorted_order <- character(0L)
  temp_in_degree <- in_degree

  while (length(temp_in_degree) > 0L) {
    zero_deg <- names(temp_in_degree)[
      vapply(temp_in_degree, function(d) d == 0L, logical(1L))
    ]
    if (length(zero_deg) == 0L) {
      # Cycle detected; append remaining nodes
      sorted_order <- c(sorted_order, names(temp_in_degree))
      break
    }

    # Preserve original discovery sequence from nodes for tie-breaking
    zero_deg <- intersect(nodes, zero_deg)
    sorted_order <- c(sorted_order, zero_deg)

    temp_in_degree[zero_deg] <- NULL
    for (z in zero_deg) {
      for (parent in names(deps_graph)) {
        if (z %in% deps_graph[[parent]] && parent %in% names(temp_in_degree)) {
          temp_in_degree[[parent]] <- max(0L, temp_in_degree[[parent]] - 1L)
        }
      }
    }
  }

  sorted_order
}

#' Resolve Package Dependencies and Version Constraints
#'
#' Inspects package DESCRIPTION metadata using \code{packageDescription} without
#' loading namespaces or modifying the search path. Computes the maximum
#' required version for each package across all dependency trees, checks
#" installed versions, and determines the path and load order.
#'
#' @param packages Character vector of package names to resolve.
#' @param lib.loc Character vector of library paths. Defaults to
#'   \code{.libPaths()}.
#' @param include_suggests Logical; whether to include \code{Suggests}
#'   dependencies. Defaults to \code{FALSE}.
#'
#' @return A named list of class \code{"resolved_dependencies"}, where each
#'   element is a list containing metadata for that package:
#' \itemize{
#'   \item \code{package}: Package name
#'   \item \code{version_required}: Maximum version required by any dependent
#'   \item \code{version_installed}: Actual version found in the library path
#'   \item \code{path}: File path to the installed package directory
#'   \item \code{type}: Type of dependency (\code{"Target"}, \code{"Depends"},
#'     or \code{"Imports"})
#'   \item \code{required_by}: Character vector of packages that depend on this
#'     package
#'   \item \code{load_order}: Topological load order
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' res <- resolve_dependencies(c("stats", "tools"))
#' }
resolve_dependencies <- function(packages,
                                 lib.loc = .libPaths(),
                                 include_suggests = FALSE) {
  if (!is.character(packages) || length(packages) == 0L) {
    stop("'packages' must be a non-empty character vector")
  }

  visited <- new.env(hash = TRUE, parent = emptyenv())
  in_queue <- new.env(hash = TRUE, parent = emptyenv())

  queue <- unique(packages)
  for (p in queue) {
    in_queue[[p]] <- TRUE
  }

  # Dependency tracking (environments for by-reference updates and O(1) lookups)
  required_by_map <- new.env(hash = TRUE, parent = emptyenv())
  deps_graph <- new.env(hash = TRUE, parent = emptyenv())
  constraints_map <- new.env(hash = TRUE, parent = emptyenv())
  types_map <- new.env(hash = TRUE, parent = emptyenv())

  for (p in packages) {
    types_map[[p]] <- "Target"
  }

  desc_cache <- new.env(parent = emptyenv())

  # Traverse dependency tree
  while (length(queue) > 0L) {
    curr <- queue[1L]
    queue <- queue[-1L]
    in_queue[[curr]] <- NULL

    if (!is.null(visited[[curr]])) {
      next
    }
    visited[[curr]] <- TRUE

    pkg_info <- .find_and_read_desc(curr, lib.loc = lib.loc, cache = desc_cache)
    desc <- pkg_info$desc
    deps_graph[[curr]] <- character(0L)

    process_field <- function(raw_deps, type_label, add_to_graph = TRUE) {
      .process_dependency_field(
        raw_deps = raw_deps,
        type_label = type_label,
        parent_pkg = curr,
        types_map = types_map,
        required_by_map = required_by_map,
        deps_graph = deps_graph,
        constraints_map = constraints_map,
        visited = visited,
        in_queue = in_queue,
        add_to_graph = add_to_graph
      )
    }

    ## Add new Depends, Imports, and (optionally) Suggests to queue
    queue <- c(queue, process_field(desc$Depends, "Depends"))
    queue <- c(queue, process_field(desc$Imports, "Imports"))
    if (include_suggests) {
      queue <- c(
        queue,
        process_field(desc$Suggests, "Suggests", add_to_graph = FALSE)
      )
    }
  }

  # Topological Sort
  sorted_order <- .topological_sort(names(visited), deps_graph)

  # Assemble resolved dependencies output
  resolved_list <- list()

  for (p in sorted_order) {
    pkg_info <- .find_and_read_desc(p, lib.loc = lib.loc, cache = desc_cache)
    installed_ver <- pkg_info$version

    # Calculate maximum required version constraint
    c_list <- constraints_map[[p]]
    max_ver_req <- numeric_version("0.0.0")

    if (!is.null(c_list) && length(c_list) > 0L) {
      for (c_item in c_list) {
        op <- c_item$op
        ver <- c_item$version

        if (op %in% c(">=", ">", "==") && ver > max_ver_req) {
          max_ver_req <- ver
        }

        # Check compatibility against installed version
        if (!do.call(op, list(installed_ver, ver))) {
          stop(sprintf(
            paste0(
              "Version conflict: Package '%s' (%s) required by '%s' ",
              "with (%s %s) is not satisfied"
            ),
            p, installed_ver, c_item$by, op, ver
          ))
        }
      }
    }

    # Get the highest precedence type for this package
    type_precedence <- c("Target", "Depends", "Imports", "Suggests")
    primary_type <- intersect(type_precedence, types_map[[p]])[1L]

    # Direct Depends packages sorted by topological load order
    raw_dep_entries <- tools:::.split_dependencies(desc$Depends)
    dep_names <- setdiff(
      vapply(raw_dep_entries, function(x) x$name, character(1L)),
      "R"
    )
    dep_names <- dep_names[order(match(dep_names, sorted_order))]

    resolved_list[[p]] <- list(
      package = p,
      version_required = ifelse(
        max_ver_req == numeric_version("0.0.0"),
        "any",
        as.character(max_ver_req)
      ),
      version_installed = as.character(installed_ver),
      path = pkg_info$path,
      type = primary_type,
      depends = dep_names,
      required_by = required_by_map[[p]] %||% character(0L),
      load_order = match(p, sorted_order)
    )
  }

  class(resolved_list) <- c("resolved_dependencies", "list")
  resolved_list
}

#' @export
print.resolved_dependencies <- function(x, ...) {
  cat(sprintf("Resolved Dependencies (%d packages):\n\n", length(x)))
  df <- do.call(rbind, lapply(x, function(item) {
    data.frame(
      Order = item$load_order,
      Package = item$package,
      Required = item$version_required,
      Installed = item$version_installed,
      Type = item$type,
      RequiredBy = paste(item$required_by, collapse = ", "),
      Path = item$path,
      stringsAsFactors = FALSE
    )
  }))
  print(df, row.names = FALSE)
  invisible(x)
}
