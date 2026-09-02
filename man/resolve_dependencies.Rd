\name{resolve_dependencies}
\alias{resolve_dependencies}
\title{Resolve Package Dependencies and Version Constraints}
\usage{
resolve_dependencies(
  packages,
  lib.loc = .libPaths(),
  include_suggests = FALSE
)
}
\arguments{
\item{packages}{Character vector of package names to resolve.}

\item{lib.loc}{Character vector of library paths. Defaults to
  \code{.libPaths()}.}

\item{include_suggests}{Logical; whether to include \code{Suggests}
  dependencies. Defaults to \code{FALSE}.}
}
\value{
A named list of class \code{"resolved_dependencies"} containing for each
package:
\item{package}{Package name}
\item{version_required}{Maximum version required by any dependent}
\item{version_installed}{Actual version found in the library path}
\item{path}{File path to the installed package directory}
\item{type}{Type of dependency (\code{"Target"}, \code{"Depends"}, or
  \code{"Imports"})}
\item{required_by}{Character vector of packages that depend on this package}
\item{load_order}{Topological load order}
}
\description{
Statelessly inspects package DESCRIPTION metadata using
\code{packageDescription} without loading namespaces or modifying the search
path. Computes the maximum required version for each package across all
dependency trees, checks installed versions, and determines the path and
load order.
}
\examples{
res <- resolve_dependencies(c("stats", "tools"))
print(res)
}
