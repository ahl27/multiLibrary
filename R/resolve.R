#' @keywords internal
.find_candidates <- function(pkg, lib.loc) {
  search_paths <- lib.loc %||% .libPaths()
  candidates <- list()
  seen_paths <- character(0L)

  # Scan each library directory using find.package(quiet = TRUE)
  for (lp in search_paths) {
    pkg_path <- find.package(pkg, lib.loc = lp, quiet = TRUE, verbose = FALSE)
    if (length(pkg_path) > 0L) {
      norm_path <- normalizePath(pkg_path, mustWork = FALSE)
      if (norm_path %in% seen_paths) {
        next
      }
      seen_paths <- c(seen_paths, norm_path)

      desc <- utils::packageDescription(pkg, lib.loc = dirname(norm_path))
      if (!inherits(desc, "packageDescription") || is.na(desc$Package)) {
        stop(sprintf(
          "Corrupted or unreadable DESCRIPTION for package '%s' at '%s'",
          pkg,
          norm_path
        ))
      }

      candidates[[length(candidates) + 1L]] <- list(
        desc = desc,
        version = numeric_version(desc$Version),
        path = norm_path,
        lib = dirname(norm_path)
      )
    }
  }

  # If not found anywhere, let find.package generate the standard error
  if (length(candidates) == 0L) {
    find.package(pkg, lib.loc = lib.loc, verbose = FALSE)
  }

  candidates
}


#' @keywords internal
.find_and_read_desc <- function(pkg,
                                lib.loc,
                                cache = NULL,
                                constraints = NULL) {
  # Return cached result if no constraints filtering is needed
  if (is.null(constraints) && !is.null(cache) &&
      exists(pkg, envir = cache, inherits = FALSE)) {
    return(get(pkg, envir = cache, inherits = FALSE))
  }

  # Find all candidate installations across library search paths
  candidates <- .find_candidates(pkg, lib.loc)

  # Filter candidates that satisfy all accumulated version constraints
  if (!is.null(constraints) && length(constraints) > 0L) {
    valid_cands <- Filter(function(cand) {
      all(vapply(constraints, function(con) {
        do.call(con$op, list(cand$version, con$version))
      }, logical(1L)))
    }, candidates)

    if (length(valid_cands) > 0L) {
      info <- valid_cands[[1L]]
      if (!is.null(cache)) {
        assign(pkg, info, envir = cache)
      }
      return(info)
    }
  }

  # Fall back to first candidate in search path priority
  info <- candidates[[1L]]
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
                                      reach_graph,
                                      constraints_map,
                                      selected_info,
                                      in_queue,
                                      add_to_graph = TRUE) {
  if (is.null(raw_deps) || !nzchar(trimws(raw_deps))) {
    return(character(0L))
  }

  parsed <- tools:::.split_dependencies(raw_deps)
  new_pkgs <- character(0L)

  for (dep_entry in parsed) {
    dep_name <- dep_entry$name
    if (dep_name == "R") {
      next
    }

    types_map[[dep_name]] <- unique(c(types_map[[dep_name]], type_label))
    required_by_map[[dep_name]] <- unique(
      c(required_by_map[[dep_name]], parent_pkg)
    )

    reach_graph[[parent_pkg]] <- unique(
      c(reach_graph[[parent_pkg]], dep_name)
    )
    if (add_to_graph) {
      deps_graph[[parent_pkg]] <- unique(
        c(deps_graph[[parent_pkg]], dep_name)
      )
    }

    # Record version constraint
    if (!is.null(dep_entry$version)) {
      op_str <- dep_entry$op %||% ">="
      con_entry <- list(
        op = op_str,
        version = numeric_version(dep_entry$version),
        by = parent_pkg
      )
      constraints_map[[dep_name]] <- c(
        constraints_map[[dep_name]],
        list(con_entry)
      )

      # Re-enqueue dep_name if already resolved and invalid under new constraint
      if (!is.null(selected_info[[dep_name]]) &&
          !do.call(op_str, list(selected_info[[dep_name]]$version,
                                con_entry$version)) &&
          is.null(in_queue[[dep_name]])) {
        new_pkgs <- c(new_pkgs, dep_name)
        in_queue[[dep_name]] <- TRUE
      }
    }

    # Enqueue dep_name if not yet resolved or queued
    if (is.null(selected_info[[dep_name]]) && is.null(in_queue[[dep_name]])) {
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
#' installed versions, and determines the path and load order.
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

  constraints_map <- new.env(hash = TRUE, parent = emptyenv())
  selected_info <- new.env(hash = TRUE, parent = emptyenv())
  types_map <- new.env(hash = TRUE, parent = emptyenv())
  required_by_map <- new.env(hash = TRUE, parent = emptyenv())
  deps_graph <- new.env(hash = TRUE, parent = emptyenv())
  reach_graph <- new.env(hash = TRUE, parent = emptyenv())

  for (p in packages) {
    types_map[[p]] <- "Target"
  }

  queue <- unique(packages)
  in_queue <- new.env(hash = TRUE, parent = emptyenv())
  for (p in queue) {
    in_queue[[p]] <- TRUE
  }

  visited_order <- character(0L)

  # Traverse dependency tree using targeted worklist queue
  while (length(queue) > 0L) {
    curr <- queue[1L]
    queue <- queue[-1L]
    in_queue[[curr]] <- NULL

    if (!curr %in% visited_order) {
      visited_order <- c(visited_order, curr)
    }

    # Select candidate satisfying all currently known constraints for curr
    c_list <- constraints_map[[curr]]
    pkg_info <- .find_and_read_desc(
      curr,
      lib.loc = lib.loc,
      constraints = c_list
    )
    selected_info[[curr]] <- pkg_info

    desc <- pkg_info$desc
    deps_graph[[curr]] <- character(0L)
    reach_graph[[curr]] <- character(0L)

    process_field <- function(raw_deps, type_label, add_to_graph = TRUE) {
      .process_dependency_field(
        raw_deps = raw_deps,
        type_label = type_label,
        parent_pkg = curr,
        types_map = types_map,
        required_by_map = required_by_map,
        deps_graph = deps_graph,
        reach_graph = reach_graph,
        constraints_map = constraints_map,
        selected_info = selected_info,
        in_queue = in_queue,
        add_to_graph = add_to_graph
      )
    }

    ## Add new Depends, Imports, and (optionally) Suggests to queue
    new_dep_pkgs <- process_field(desc$Depends, "Depends")
    new_imp_pkgs <- process_field(desc$Imports, "Imports")
    queue <- c(queue, new_dep_pkgs, new_imp_pkgs)

    if (include_suggests) {
      new_sug_pkgs <- process_field(
        desc$Suggests,
        "Suggests",
        add_to_graph = FALSE
      )
      queue <- c(queue, new_sug_pkgs)
    }
  }

  # Reachability filter: collect all packages reachable from target packages
  reachable <- character(0L)
  reach_q <- unique(packages)
  while (length(reach_q) > 0L) {
    node <- reach_q[1L]
    reach_q <- reach_q[-1L]
    if (node %in% reachable) {
      next
    }
    reachable <- c(reachable, node)
    children <- reach_graph[[node]]
    reach_q <- c(reach_q, setdiff(children, reachable))
  }

  # Retain natural discovery ordering among reachable packages
  active_nodes <- intersect(visited_order, reachable)

  # Topological Sort
  sorted_order <- .topological_sort(active_nodes, deps_graph)

  # Assemble resolved dependencies output
  resolved_list <- list()

  for (p in sorted_order) {
    pkg_info <- selected_info[[p]]
    desc <- pkg_info$desc
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
