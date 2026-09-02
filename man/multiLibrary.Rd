\name{multiLibrary}
\alias{multiLibrary}
\title{Load Multiple Packages with Dependency and Version Resolution}
\usage{
multiLibrary(
  ...,
  packages = NULL,
  lib.loc = .libPaths(),
  quietly = FALSE
)
}
\arguments{
\item{...}{Package names supplied as character strings or unquoted symbols.}

\item{packages}{Character vector of package names (alternative to
  \code{...}).}

\item{lib.loc}{Character vector of library paths. Defaults to
  \code{.libPaths()}.}

\item{quietly}{Logical; whether to suppress informational messages.
  Defaults to \code{FALSE}.}
}
\value{
Invisibly returns the \code{resolved_dependencies} object.
}
\description{
Resolves version requirements across multiple requested packages
simultaneously, verifies compatibility, and attaches them sequentially in
safe topological order.
}
\examples{
\dontrun{
multiLibrary("stats", "tools")
}
}
