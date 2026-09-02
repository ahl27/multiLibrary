#' Load Multiple Packages with Dependency and Version Resolution
#'
#' Resolves version requirements across multiple requested packages
#' simultaneously, verifies compatibility, loads namespace dependencies, and
#' attaches targets and their depends sequentially in safe order.
#'
#' @param ... Package names supplied as character strings or unquoted symbols.
#' @param packages Character vector of package names (alternative to
#'   \code{...}).
#' @param lib.loc Character vector of library paths. Defaults to
#'   \code{.libPaths()}.
#' @param quietly Logical; whether to suppress informational messages.
#'   Defaults to \code{FALSE}.
#'
#' @return Invisibly returns the \code{resolved_dependencies} object.
#' @export
#'
#' @examples
#' \dontrun{
#' multiLibrary("stats", "tools")
#' }
multiLibrary <- function(...,
                         packages = NULL,
                         lib.loc = .libPaths(),
                         quietly = FALSE) {
  dots <- match.call(expand.dots = FALSE)$...
  dot_names <- character(0L)

  if (length(dots) > 0L) {
    dot_names <- vapply(dots, function(x) {
      if (is.character(x)) {
        x
      } else if (is.symbol(x)) {
        as.character(x)
      } else {
        stop("Arguments must be package names as symbols or strings")
      }
    }, character(1L))
  }

  all_pkgs <- unique(c(dot_names, packages))
  if (length(all_pkgs) == 0L) {
    stop("No packages specified to multiLibrary")
  }

  resolved <- resolve_dependencies(all_pkgs, lib.loc = lib.loc)

  # 1. Pre-load all namespace-only dependencies (Imports) in topological order
  #    These don't modify the search path, so we can just load them
  for (item in resolved) {
    pkg_name <- item$package
    if (item$type == "Imports" && !isNamespaceLoaded(pkg_name)) {
      if (!quietly) {
        message(sprintf(
          "Loading namespace %s (%s) from %s",
          pkg_name,
          item$version_installed,
          item$path
        ))
      }
      loadNamespace(pkg_name, lib.loc = dirname(item$path))
    }
  }

  # 2. Attach Targets and their associated Depends in user-specified order
  #    We can't just load these in topological order because Targets/Depends
  #    both modify the search path, and we want this to be identical to if
  #    `library` was just called sequentially.
  for (target_pkg in all_pkgs) {
    target_item <- resolved[[target_pkg]]

    # Attach any unattached Depends required by this target
    for (dep_name in target_item$depends) {
      if (!(paste0("package:", dep_name) %in% search())) {
        dep_item <- resolved[[dep_name]]
        if (!quietly) {
          message(sprintf(
            "Attaching dependency %s (%s) from %s",
            dep_name,
            dep_item$version_installed,
            dep_item$path
          ))
        }
        library(
          dep_name,
          character.only = TRUE,
          lib.loc = dirname(dep_item$path),
          quietly = quietly
        )
      }
    }

    # Attach the target package itself
    if (!(paste0("package:", target_pkg) %in% search())) {
      if (!quietly) {
        message(sprintf(
          "Attaching %s (%s) from %s",
          target_pkg,
          target_item$version_installed,
          target_item$path
        ))
      }
      library(
        target_pkg,
        character.only = TRUE,
        lib.loc = dirname(target_item$path),
        quietly = quietly
      )
    }
  }

  invisible(resolved)
}
