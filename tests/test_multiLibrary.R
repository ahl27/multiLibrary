# ==============================================================================
# Tests for multiLibrary
# ==============================================================================
#
# Tests dependency graph traversal, version reconciliation, topological
# sorting, statelessness, edge cases, and exact search path parity.
#
# ==============================================================================

library(multiLibrary)
source("tests/test_helpers.R")

# ==============================================================================
# 1. Statelessness & S3 Return Structure Verification
# ==============================================================================

local({
  init_search <- search()
  init_loaded <- loadedNamespaces()

  pkgX <- mock_package("pkgX")
  pkgA <- mock_package("pkgA", depends = list(pkgX))
  lib <- create_mock_library(pkgA)
  on.exit(lib$cleanup(), add = TRUE)

  res <- resolve_dependencies("pkgA", lib.loc = lib$lib_dir)

  # Verify S3 return structure
  stopifnot(
    "res must inherit from resolved_dependencies" =
      inherits(res, "resolved_dependencies"),
    "res must inherit from list" =
      inherits(res, "list"),
    "res must contain pkgA and pkgX" =
      all(c("pkgA", "pkgX") %in% names(res)),
    "pkgA must have type Target" =
      res$pkgA$type == "Target",
    "pkgX must have type Depends" =
      res$pkgX$type == "Depends",
    "pkgA must have load_order 2" =
      res$pkgA$load_order == 2L,
    "pkgX must have load_order 1" =
      res$pkgX$load_order == 1L
  )

  # Verify print method output
  out <- capture.output(print(res))
  stopifnot(
    "print() output must contain header" =
      any(grepl("Resolved Dependencies", out)),
    "print() output must contain pkgA" =
      any(grepl("pkgA", out)),
    "print() output must contain pkgX" =
      any(grepl("pkgX", out))
  )

  # Verify statelessness: resolution must NOT modify search() or namespaces
  stopifnot(
    "resolve_dependencies must not modify search path" =
      identical(search(), init_search),
    "resolve_dependencies must not load namespaces" =
      identical(loadedNamespaces(), init_loaded)
  )
})

# ==============================================================================
# 2. Multi-Tier Dependency Tree (Search Path Parity)
# ==============================================================================
#
#        pkgA
#       /    \
#   pkgB      pkgC
#    │         │
#   pkgD      pkgE
#
local({
  pkgD <- mock_package("pkgD")
  pkgE <- mock_package("pkgE")
  pkgB <- mock_package("pkgB", depends = list(pkgD))
  pkgC <- mock_package("pkgC", depends = list(pkgE))
  pkgA <- mock_package("pkgA", depends = list(pkgB, pkgC))

  lib <- create_mock_library(pkgA)
  on.exit(lib$cleanup(), add = TRUE)

  stopifnot(
    "Scenario 1: Multi-tier search path must match standard R" =
      compare_to_base_r("pkgA", lib)
  )
})

# ==============================================================================
# 3. Diamond Dependency with Multiple User Targets
# ==============================================================================
#
#   [pkgX]       [pkgW]   <- Targets
#    │   \       /   │
# (imp)  (dep) (dep) (imp)
#    │      \ /      │
#  pkgZ    pkgY    pkgV
#
local({
  pkgZ <- mock_package("pkgZ")
  pkgV <- mock_package("pkgV")
  pkgY <- mock_package("pkgY")
  pkgX <- mock_package("pkgX", depends = list(pkgY), imports = list(pkgZ))
  pkgW <- mock_package("pkgW", depends = list(pkgY), imports = list(pkgV))

  lib <- create_mock_library(pkgX, pkgW)
  on.exit(lib$cleanup(), add = TRUE)

  # Forward user target order (pkgX then pkgW)
  stopifnot(
    "Scenario 2: Forward target order search path must match standard R" =
      compare_to_base_r(c("pkgX", "pkgW"), lib)
  )

  # Reverse user target order (pkgW then pkgX)
  stopifnot(
    "Scenario 2: Reverse target order search path must match standard R" =
      compare_to_base_r(c("pkgW", "pkgX"), lib)
  )
})

# ==============================================================================
# 4. Asymmetric Dependency (One Target Depends, One Target Imports)
# ==============================================================================
#
#   [pkgImp] (Target 1)     [pkgDep] (Target 2)
#        │                       │
#     (Imports)              (Depends)
#        │                       │
#        ▼                       ▼
#      pkgX                    pkgX
#
# Search path when loaded in order (pkgImp, pkgDep):
#   [.GlobalEnv, pkgDep, pkgX, pkgImp, ...]
#
# Search path when loaded in order (pkgDep, pkgImp):
#   [.GlobalEnv, pkgImp, pkgDep, pkgX, ...]
#
local({
  pkgX <- mock_package("pkgX")
  pkgImp <- mock_package("pkgImp", imports = list(pkgX))
  pkgDep <- mock_package("pkgDep", depends = list(pkgX))

  lib <- create_mock_library(pkgImp, pkgDep, pkgX)
  on.exit(lib$cleanup(), add = TRUE)

  # Case A: multiLibrary(pkgImp, pkgDep)
  stopifnot(
    "Scenario 4A: (pkgImp, pkgDep) search path must match standard R" =
      compare_to_base_r(c("pkgImp", "pkgDep"), lib)
  )

  # Case B: multiLibrary(pkgDep, pkgImp)
  stopifnot(
    "Scenario 4B: (pkgDep, pkgImp) search path must match standard R" =
      compare_to_base_r(c("pkgDep", "pkgImp"), lib)
  )
})

# ==============================================================================
# 5. Version Constraint Operators (>=, >, ==, <=, <)
# ==============================================================================

# 5.1 Operator `>=` (Greater Than or Equal)
#
#   pkgM (>= 1.2.0)    pkgO (>= 2.0.0)
#          \               /
#           ▼             ▼
#            pkgN (2.3.0)  ==> Pass (Max requirement: 2.0.0)
#
#   pkgP (>= 3.0.0)
#          │
#          ▼
#    pkgN (2.3.0)  ==> Fail (Version conflict)
#
local({
  pkgN <- mock_package("pkgN", version = "2.3.0")
  pkgM <- mock_package("pkgM", depends = c("pkgN (>= 1.2.0)"))
  pkgO <- mock_package("pkgO", depends = c("pkgN (>= 2.0.0)"))

  lib <- create_mock_library(pkgM, pkgO, pkgN)
  on.exit(lib$cleanup(), add = TRUE)

  res <- resolve_dependencies(c("pkgM", "pkgO"), lib.loc = lib$lib_dir)
  stopifnot(
    "Operator >=: pkgN required version must be 2.0.0" =
      res$pkgN$version_required == "2.0.0",
    "Operator >=: pkgN installed version must be 2.3.0" =
      res$pkgN$version_installed == "2.3.0"
  )

  pkgP <- mock_package("pkgP", depends = c("pkgN (>= 3.0.0)"))
  lib_fail <- create_mock_library(pkgP, pkgN)
  on.exit(lib_fail$cleanup(), add = TRUE)

  err <- tryCatch(
    resolve_dependencies("pkgP", lib.loc = lib_fail$lib_dir),
    error = function(e) e
  )
  stopifnot(
    "Operator >=: unfulfilled constraint must throw error" =
      inherits(err, "error"),
    "Operator >=: error message must report version conflict" =
      grepl("Version conflict", err$message)
  )
})

# 5.2 Operator `>` (Strictly Greater Than)
#
#   pkgR (> 2.0.0)       pkgQ (> 2.3.0)
#          │                   │
#          ▼                   ▼
#    pkgN (2.3.0)        pkgN (2.3.0)
#       [Pass]              [Fail]
#
local({
  pkgN <- mock_package("pkgN", version = "2.3.0")

  pkgR <- mock_package("pkgR", depends = c("pkgN (> 2.0.0)"))
  lib_pass <- create_mock_library(pkgR, pkgN)
  on.exit(lib_pass$cleanup(), add = TRUE)

  res <- resolve_dependencies("pkgR", lib.loc = lib_pass$lib_dir)
  stopifnot(
    "Operator >: pkgN (2.3.0) satisfying (> 2.0.0)" =
      res$pkgN$version_installed == "2.3.0"
  )

  pkgQ <- mock_package("pkgQ", depends = c("pkgN (> 2.3.0)"))
  lib_fail <- create_mock_library(pkgQ, pkgN)
  on.exit(lib_fail$cleanup(), add = TRUE)

  err <- tryCatch(
    resolve_dependencies("pkgQ", lib.loc = lib_fail$lib_dir),
    error = function(e) e
  )
  stopifnot(
    "Operator >: pkgN (2.3.0) not strictly greater than 2.3.0 must error" =
      inherits(err, "error"),
    "Operator >: error message must report version conflict" =
      grepl("Version conflict", err$message)
  )
})

# 5.3 Operator `==` (Exact Equality)
#
#   pkgS (== 2.3.0)      pkgT (== 2.0.0)
#          │                   │
#          ▼                   ▼
#    pkgN (2.3.0)        pkgN (2.3.0)
#       [Pass]              [Fail]
#
local({
  pkgN <- mock_package("pkgN", version = "2.3.0")

  pkgS <- mock_package("pkgS", depends = c("pkgN (== 2.3.0)"))
  lib_pass <- create_mock_library(pkgS, pkgN)
  on.exit(lib_pass$cleanup(), add = TRUE)

  res <- resolve_dependencies("pkgS", lib.loc = lib_pass$lib_dir)
  stopifnot(
    "Operator ==: exact version match must succeed" =
      res$pkgN$version_installed == "2.3.0"
  )

  pkgT <- mock_package("pkgT", depends = c("pkgN (== 2.0.0)"))
  lib_fail <- create_mock_library(pkgT, pkgN)
  on.exit(lib_fail$cleanup(), add = TRUE)

  err <- tryCatch(
    resolve_dependencies("pkgT", lib.loc = lib_fail$lib_dir),
    error = function(e) e
  )
  stopifnot(
    "Operator ==: mismatched version must error" =
      inherits(err, "error"),
    "Operator ==: error message must report version conflict" =
      grepl("Version conflict", err$message)
  )
})

# 5.4 Operator `<=` (Less Than or Equal)
#
#   pkgU1 (<= 2.3.0)     pkgU2 (<= 2.0.0)
#          │                   │
#          ▼                   ▼
#    pkgN (2.3.0)        pkgN (2.3.0)
#       [Pass]              [Fail]
#
local({
  pkgN <- mock_package("pkgN", version = "2.3.0")

  pkgU1 <- mock_package("pkgU1", depends = c("pkgN (<= 2.3.0)"))
  lib_pass <- create_mock_library(pkgU1, pkgN)
  on.exit(lib_pass$cleanup(), add = TRUE)

  res <- resolve_dependencies("pkgU1", lib.loc = lib_pass$lib_dir)
  stopifnot(
    "Operator <=: 2.3.0 <= 2.3.0 must succeed" =
      res$pkgN$version_installed == "2.3.0"
  )

  pkgU2 <- mock_package("pkgU2", depends = c("pkgN (<= 2.0.0)"))
  lib_fail <- create_mock_library(pkgU2, pkgN)
  on.exit(lib_fail$cleanup(), add = TRUE)

  err <- tryCatch(
    resolve_dependencies("pkgU2", lib.loc = lib_fail$lib_dir),
    error = function(e) e
  )
  stopifnot(
    "Operator <=: 2.3.0 <= 2.0.0 must error" =
      inherits(err, "error"),
    "Operator <=: error message must report version conflict" =
      grepl("Version conflict", err$message)
  )
})

# 5.5 Operator `<` (Strictly Less Than)
#
#   pkgV1 (< 3.0.0)      pkgV2 (< 2.3.0)
#          │                   │
#          ▼                   ▼
#    pkgN (2.3.0)        pkgN (2.3.0)
#       [Pass]              [Fail]
#
local({
  pkgN <- mock_package("pkgN", version = "2.3.0")

  pkgV1 <- mock_package("pkgV1", depends = c("pkgN (< 3.0.0)"))
  lib_pass <- create_mock_library(pkgV1, pkgN)
  on.exit(lib_pass$cleanup(), add = TRUE)

  res <- resolve_dependencies("pkgV1", lib.loc = lib_pass$lib_dir)
  stopifnot(
    "Operator <: 2.3.0 < 3.0.0 must succeed" =
      res$pkgN$version_installed == "2.3.0"
  )

  pkgV2 <- mock_package("pkgV2", depends = c("pkgN (< 2.3.0)"))
  lib_fail <- create_mock_library(pkgV2, pkgN)
  on.exit(lib_fail$cleanup(), add = TRUE)

  err <- tryCatch(
    resolve_dependencies("pkgV2", lib.loc = lib_fail$lib_dir),
    error = function(e) e
  )
  stopifnot(
    "Operator <: 2.3.0 < 2.3.0 must error" =
      inherits(err, "error"),
    "Operator <: error message must report version conflict" =
      grepl("Version conflict", err$message)
  )
})

# 5.6 Multi-Version Candidate Selection with Distinct Dependencies
#
#   Installed candidate versions in library search paths:
#     lib1: pkgDep v3.0.0  ─── Imports ───> [depV3]
#     lib2: pkgDep v2.0.0  ─── Imports ───> [depV2]
#     lib3: pkgDep v1.0.0  ─── Imports ───> [depV1]
#
#   Constraints:
#     pkgA depends on pkgDep (>= 1.5.0)
#     pkgB depends on pkgDep (<= 2.5.0)
#
#   Resolver must select pkgDep v2.0.0 from lib2 and include ONLY depV2,
#   cleanly omitting depV1 and depV3.
#
local({
  lib1 <- tempfile("lib1_")
  lib2 <- tempfile("lib2_")
  lib3 <- tempfile("lib3_")

  depV1 <- mock_package("depV1")
  depV2 <- mock_package("depV2")
  depV3 <- mock_package("depV3")

  pkgDep_v3 <- mock_package("pkgDep", version = "3.0.0", imports = list(depV3))
  pkgDep_v2 <- mock_package("pkgDep", version = "2.0.0", imports = list(depV2))
  pkgDep_v1 <- mock_package("pkgDep", version = "1.0.0", imports = list(depV1))

  m1 <- create_mock_library(pkgDep_v3, depV3, lib_dir = lib1)
  m2 <- create_mock_library(pkgDep_v2, depV2, lib_dir = lib2)
  m3 <- create_mock_library(pkgDep_v1, depV1, lib_dir = lib3)

  pkgA <- mock_package("pkgA", depends = list("pkgDep (>= 1.5.0)"))
  pkgB <- mock_package("pkgB", depends = list("pkgDep (<= 2.5.0)"))
  libApp <- tempfile("libApp_")
  mApp <- create_mock_library(pkgA, pkgB, lib_dir = libApp)

  on.exit({
    m1$cleanup()
    m2$cleanup()
    m3$cleanup()
    mApp$cleanup()
  }, add = TRUE)

  all_libs <- c(mApp$lib_dir, m1$lib_dir, m2$lib_dir, m3$lib_dir)

  # Case 1: Forward order (>= 1.5.0 discovered before <= 2.5.0)
  res_ab <- resolve_dependencies(c("pkgA", "pkgB"), lib.loc = all_libs)
  stopifnot(
    "Multi-version AB: pkgDep must resolve to v2.0.0" =
      res_ab$pkgDep$version_installed == "2.0.0",
    "Multi-version AB: pkgDep path must point to lib2" =
      res_ab$pkgDep$path == file.path(m2$lib_dir, "pkgDep"),
    "Multi-version AB: depV2 (from v2.0.0) must be included" =
      "depV2" %in% names(res_ab),
    "Multi-version AB: depV1 (from v1.0.0) must NOT be included" =
      !"depV1" %in% names(res_ab),
    "Multi-version AB: depV3 (from v3.0.0) must NOT be included" =
      !"depV3" %in% names(res_ab),
    "Multi-version AB: exact resolved package order" =
      identical(names(res_ab), c("depV2", "pkgDep", "pkgA", "pkgB"))
  )

  # Case 2: Reverse order (<= 2.5.0 discovered before >= 1.5.0)
  res_ba <- resolve_dependencies(c("pkgB", "pkgA"), lib.loc = all_libs)
  stopifnot(
    "Multi-version BA: pkgDep must resolve to v2.0.0" =
      res_ba$pkgDep$version_installed == "2.0.0",
    "Multi-version BA: pkgDep path must point to lib2" =
      res_ba$pkgDep$path == file.path(m2$lib_dir, "pkgDep"),
    "Multi-version BA: depV2 (from v2.0.0) must be included" =
      "depV2" %in% names(res_ba),
    "Multi-version BA: depV1 (from v1.0.0) must NOT be included" =
      !"depV1" %in% names(res_ba),
    "Multi-version BA: depV3 (from v3.0.0) must NOT be included" =
      !"depV3" %in% names(res_ba),
    "Multi-version BA: exact resolved package order" =
      identical(names(res_ba), c("depV2", "pkgDep", "pkgB", "pkgA"))
  )

  # Case 3: Nested Diamond Branching
  # (Branch 1 -> pkgDep >= 1.5.0, Branch 2 -> pkgDep <= 2.5.0)
  pkgBr1 <- mock_package("pkgBr1", depends = list("pkgDep (>= 1.5.0)"))
  pkgBr2 <- mock_package("pkgBr2", depends = list("pkgDep (<= 2.5.0)"))
  pkgRoot <- mock_package("pkgRoot", depends = list("pkgBr1", "pkgBr2"))
  libNested <- tempfile("libNested_")
  mNested <- create_mock_library(
    pkgRoot,
    pkgBr1,
    pkgBr2,
    lib_dir = libNested
  )
  on.exit(mNested$cleanup(), add = TRUE)

  all_libs_nested <- c(mNested$lib_dir, m1$lib_dir, m2$lib_dir, m3$lib_dir)
  res_nested <- resolve_dependencies("pkgRoot", lib.loc = all_libs_nested)
  stopifnot(
    "Multi-version Nested: pkgDep must resolve to v2.0.0" =
      res_nested$pkgDep$version_installed == "2.0.0",
    "Multi-version Nested: depV2 must be included" =
      "depV2" %in% names(res_nested),
    "Multi-version Nested: depV1 must NOT be included" =
      !"depV1" %in% names(res_nested),
    "Multi-version Nested: depV3 must NOT be included" =
      !"depV3" %in% names(res_nested),
    "Multi-version Nested: exact resolved package order" =
      identical(
        names(res_nested),
        c("depV2", "pkgDep", "pkgBr1", "pkgBr2", "pkgRoot")
      )
  )
})

# 5.7 Unsatisfiable Multi-Version Constraints Across Library Paths
#
#   Installed candidate versions:
#     lib1: pkgDep v3.0.0 (Imports depV3)
#     lib3: pkgDep v1.0.0 (Imports depV1)
#     (v2.0.0 is NOT installed anywhere)
#
#   Constraints:
#     pkgA depends on pkgDep (>= 1.5.0)
#     pkgB depends on pkgDep (<= 2.5.0)
#
#   Because neither v3.0.0 nor v1.0.0 satisfies [>= 1.5.0, <= 2.5.0],
#   resolution must fail with a version conflict error.
#
local({
  lib1 <- tempfile("lib1_")
  lib3 <- tempfile("lib3_")

  depV1 <- mock_package("depV1")
  depV3 <- mock_package("depV3")

  pkgDep_v3 <- mock_package("pkgDep", version = "3.0.0", imports = list(depV3))
  pkgDep_v1 <- mock_package("pkgDep", version = "1.0.0", imports = list(depV1))

  m1 <- create_mock_library(pkgDep_v3, depV3, lib_dir = lib1)
  m3 <- create_mock_library(pkgDep_v1, depV1, lib_dir = lib3)

  pkgA <- mock_package("pkgA", depends = list("pkgDep (>= 1.5.0)"))
  pkgB <- mock_package("pkgB", depends = list("pkgDep (<= 2.5.0)"))
  libApp <- tempfile("libApp_")
  mApp <- create_mock_library(pkgA, pkgB, lib_dir = libApp)

  on.exit({
    m1$cleanup()
    m3$cleanup()
    mApp$cleanup()
  }, add = TRUE)

  all_libs <- c(mApp$lib_dir, m1$lib_dir, m3$lib_dir)

  err_ab <- tryCatch(
    resolve_dependencies(c("pkgA", "pkgB"), lib.loc = all_libs),
    error = function(e) e
  )
  stopifnot(
    "Unsatisfiable multi-version forward order must fail" =
      inherits(err_ab, "error"),
    "Unsatisfiable multi-version error must report version conflict" =
      grepl("Version conflict", err_ab$message)
  )

  err_ba <- tryCatch(
    resolve_dependencies(c("pkgB", "pkgA"), lib.loc = all_libs),
    error = function(e) e
  )
  stopifnot(
    "Unsatisfiable multi-version reverse order must fail" =
      inherits(err_ba, "error"),
    "Unsatisfiable multi-version reverse error must report version conflict" =
      grepl("Version conflict", err_ba$message)
  )
})

# ==============================================================================
# 6. Cyclic Dependency Handling
# ==============================================================================
#
#   pkgCycA <=======> pkgCycB  (Direct Cycle)
#
local({
  pkgCycA <- mock_package("pkgCycA", depends = c("pkgCycB"))
  pkgCycB <- mock_package("pkgCycB", depends = c("pkgCycA"))

  lib <- create_mock_library(pkgCycA, pkgCycB)
  on.exit(lib$cleanup(), add = TRUE)

  res <- resolve_dependencies("pkgCycA", lib.loc = lib$lib_dir)
  stopifnot(
    "Cycle resolution must contain both cyclic packages" =
      all(c("pkgCycA", "pkgCycB") %in% names(res)),
    "Cycle resolution must assign integer load orders" =
      is.integer(res$pkgCycA$load_order) && is.integer(res$pkgCycB$load_order)
  )
})

# ==============================================================================
# 7. R Version Specification in Depends (R >= 4.0.0)
# ==============================================================================
#
#   pkgWithR ──── Depends: R (>= 4.0.0), pkgLeaf
#
local({
  pkgLeaf <- mock_package("pkgLeaf")
  pkgWithR <- mock_package(
    "pkgWithR",
    depends = c("R (>= 4.0.0)", "pkgLeaf")
  )

  lib <- create_mock_library(pkgWithR, pkgLeaf)
  on.exit(lib$cleanup(), add = TRUE)

  res <- resolve_dependencies("pkgWithR", lib.loc = lib$lib_dir)
  stopifnot(
    "Package 'R' must not be included in resolved package list" =
      !("R" %in% names(res)),
    "Dependencies alongside R must be resolved" =
      "pkgLeaf" %in% names(res),
    "Target package must be resolved" =
      "pkgWithR" %in% names(res)
  )
})

# ==============================================================================
# 8. Suggests Inclusion & Version Constraint Enforcement
# ==============================================================================
#
#   pkgMain ─── Suggests: pkgSug (>= 2.0.0) [Installed: pkgSug 1.0.0]
#
local({
  pkgSug <- mock_package("pkgSug", version = "1.0.0")
  pkgMain <- mock_package("pkgMain", suggests = c("pkgSug (>= 2.0.0)"))

  lib <- create_mock_library(pkgMain, pkgSug)
  on.exit(lib$cleanup(), add = TRUE)

  # When include_suggests = FALSE, Suggests are ignored and resolution passes
  res_no_sug <- resolve_dependencies(
    "pkgMain",
    lib.loc = lib$lib_dir,
    include_suggests = FALSE
  )
  stopifnot(
    "include_suggests = FALSE must ignore suggests" =
      !("pkgSug" %in% names(res_no_sug))
  )

  # When include_suggests = TRUE, Suggests are checked and fail constraint
  err_sug <- tryCatch(
    resolve_dependencies(
      "pkgMain",
      lib.loc = lib$lib_dir,
      include_suggests = TRUE
    ),
    error = function(e) e
  )
  stopifnot(
    "include_suggests = TRUE must enforce suggests version constraint" =
      inherits(err_sug, "error"),
    "include_suggests = TRUE error must report version conflict" =
      grepl("Version conflict", err_sug$message)
  )
})

# ==============================================================================
# 9. Cross-Library Path Resolution (Multiple lib.loc Directories)
# ==============================================================================
#
#   [lib1] pkgFromLib1 ─── Depends: pkgFromLib2 [in lib2]
#
local({
  lib1_dir <- tempfile("lib1_")
  lib2_dir <- tempfile("lib2_")
  dir.create(lib1_dir)
  dir.create(lib2_dir)
  lib1_dir <- normalizePath(lib1_dir)
  lib2_dir <- normalizePath(lib2_dir)

  pkgFromLib2 <- mock_package("pkgFromLib2", version = "1.0.0")
  lib_b <- create_mock_library(pkgFromLib2, lib_dir = lib2_dir)

  pkgFromLib1 <- mock_package("pkgFromLib1", depends = c("pkgFromLib2"))
  lib_a <- create_mock_library(pkgFromLib1, lib_dir = lib1_dir)
  on.exit({
    lib_a$cleanup()
    lib_b$cleanup()
  }, add = TRUE)

  res <- resolve_dependencies(
    "pkgFromLib1",
    lib.loc = c(lib1_dir, lib2_dir)
  )

  stopifnot(
    "pkgFromLib1 must be resolved from lib1" =
      grepl(lib1_dir, res$pkgFromLib1$path),
    "pkgFromLib2 must be resolved from lib2" =
      grepl(lib2_dir, res$pkgFromLib2$path)
  )
})

# ==============================================================================
# 10. Bounded Interval Constraints (>= 1.5.0 AND <= 2.5.0)
# ==============================================================================
#
#   pkgTarget1 (>= 1.5.0)     pkgTarget2 (<= 2.5.0)
#            \                      /
#             ▼                    ▼
#              pkgBounded (2.0.0)  ===> Pass
#
local({
  pkgBounded <- mock_package("pkgBounded", version = "2.0.0")
  pkgTarget1 <- mock_package("pkgTarget1", depends = c("pkgBounded (>= 1.5.0)"))
  pkgTarget2 <- mock_package("pkgTarget2", depends = c("pkgBounded (<= 2.5.0)"))

  lib <- create_mock_library(pkgTarget1, pkgTarget2, pkgBounded)
  on.exit(lib$cleanup(), add = TRUE)

  res <- resolve_dependencies(
    c("pkgTarget1", "pkgTarget2"),
    lib.loc = lib$lib_dir
  )
  stopifnot(
    "Bounded version interval (1.5.0 <= 2.0.0 <= 2.5.0) must pass" =
      res$pkgBounded$version_installed == "2.0.0"
  )
})

# ==============================================================================
# 11. Target Package Is Also a Dependency of Another Target
# ==============================================================================
#
#   [pkgParent] (Target 1) ─── Depends ───> [pkgChild] (Target 2)
#
local({
  pkgChild <- mock_package("pkgChild")
  pkgParent <- mock_package("pkgParent", depends = list(pkgChild))

  lib <- create_mock_library(pkgParent, pkgChild)
  on.exit(lib$cleanup(), add = TRUE)

  res <- resolve_dependencies(
    c("pkgParent", "pkgChild"),
    lib.loc = lib$lib_dir
  )

  stopifnot(
    "pkgParent must have type Target" =
      res$pkgParent$type == "Target",
    "pkgChild must retain primary type Target despite being a dependency" =
      res$pkgChild$type == "Target",
    "Search path must match standard R" =
      compare_to_base_r(c("pkgParent", "pkgChild"), lib)
  )
})

# ==============================================================================
# 12. User Interface & Edge Case Handling
# ==============================================================================

local({
  # Non-existent package error
  err_missing <- tryCatch(
    resolve_dependencies("non_existent_package_xyz123"),
    error = function(e) e
  )
  stopifnot(
    "Missing package must throw error" =
      inherits(err_missing, "error")
  )

  # Empty package input error
  err_empty <- tryCatch(
    resolve_dependencies(character(0L)),
    error = function(e) e
  )
  stopifnot(
    "Empty package input must throw error" =
      inherits(err_empty, "error")
  )

  # Unquoted symbols vs character strings in multiLibrary()
  pkgX <- mock_package("pkgX")
  pkgY <- mock_package("pkgY")
  lib <- create_mock_library(pkgX, pkgY)
  on.exit(lib$cleanup(), add = TRUE)

  res_sym <- resolve_dependencies(c("pkgX", "pkgY"), lib.loc = lib$lib_dir)
  stopifnot(
    "Symbol/string resolution must contain pkgX and pkgY" =
      all(c("pkgX", "pkgY") %in% names(res_sym))
  )
})

# ==============================================================================
# 13. Target Package Is Also an Imported Dependency of Another Target
# ==============================================================================
#
#   [pkgA] ─── Imports ───> [pkgB]
#
# Case A: multiLibrary(pkgB, pkgA) ==> search(): [.GlobalEnv, pkgA, pkgB, ...]
# Case B: multiLibrary(pkgA, pkgB) ==> search(): [.GlobalEnv, pkgB, pkgA, ...]
#
local({
  pkgB <- mock_package("pkgB")
  pkgA <- mock_package("pkgA", imports = list(pkgB))

  lib <- create_mock_library(pkgA, pkgB)
  on.exit(lib$cleanup(), add = TRUE)

  res_ba <- resolve_dependencies(c("pkgB", "pkgA"), lib.loc = lib$lib_dir)
  stopifnot(
    "pkgB must retain primary type Target when requested explicitly" =
      res_ba$pkgB$type == "Target",
    "pkgA must have primary type Target" =
      res_ba$pkgA$type == "Target",
    "Order (pkgB, pkgA) search path must match standard R" =
      compare_to_base_r(c("pkgB", "pkgA"), lib)
  )

  # Reverse user target order (pkgA then pkgB)
  stopifnot(
    "Order (pkgA, pkgB) search path must match standard R" =
      compare_to_base_r(c("pkgA", "pkgB"), lib)
  )
})

# ==============================================================================
# 14. S3 Generics, Method Dispatch, & Imports Execution Behavior
# ==============================================================================
#
# 14.1 Custom S3 Generic in Imported Package, Method in Second Imported Package
#
#   [pkgApp] (Target)
#     │         │
#  (Imports)  (Imports)
#     │         │
#     ▼         ▼
#  pkgMethod   pkgGeneric (Defines generic greet())
#    (Registers S3method(greet, custom_type))
#
local({
  pkgGeneric <- mock_package(
    "pkgGeneric",
    code = "greet <- function(x, ...) UseMethod('greet')",
    exports = "greet"
  )

  pkgMethod <- mock_package(
    "pkgMethod",
    imports = list(pkgGeneric),
    code = c(
      "make_custom <- function() structure(list(), class = 'custom_type')",
      "greet.custom_type <- function(x, ...) 'Dispatched greet.custom_type!'"
    ),
    exports = "make_custom",
    namespace_extra = "S3method(greet, custom_type)"
  )

  pkgApp <- mock_package(
    "pkgApp",
    imports = list(pkgMethod, pkgGeneric),
    code = "run_app <- function() greet(make_custom())",
    exports = "run_app"
  )

  lib <- create_mock_library(pkgApp, pkgMethod, pkgGeneric)
  on.exit(lib$cleanup(), add = TRUE)

  stopifnot(
    "Custom S3 method dispatch across imports must match Base R" =
      compare_execution("pkgApp", "run_app()", lib),
    "pkgApp session state must match Base R" =
      compare_to_base_r("pkgApp", lib)
  )
})

# 14.2 Base R Generic Overload & .onLoad Hook in Imported Package
#
#   [pkgConsumer] (Target)
#         │
#     (Imports)
#         │
#         ▼
#   pkgBaseOverload (Registers S3method(as.character, boxed_val) & .onLoad)
#
local({
  pkgBaseOverload <- mock_package(
    "pkgBaseOverload",
    code = c(
      ".onLoad <- function(libname, pkgname) {",
      "  options(pkgBaseOverload_loaded = TRUE)",
      "}",
      "box_val <- function(v) structure(list(val = v), class = 'boxed_val')",
      "as.character.boxed_val <- function(x, ...) paste0('[', x$val, ']')"
    ),
    exports = "box_val",
    namespace_extra = "S3method(as.character, boxed_val)"
  )

  pkgConsumer <- mock_package(
    "pkgConsumer",
    imports = list(pkgBaseOverload),
    code = c(
      "get_boxed <- function() as.character(box_val(99))",
      "get_hook_state <- function() {",
      "  isTRUE(getOption('pkgBaseOverload_loaded'))",
      "}"
    ),
    exports = c("get_boxed", "get_hook_state")
  )

  lib <- create_mock_library(pkgConsumer, pkgBaseOverload)
  on.exit(lib$cleanup(), add = TRUE)

  stopifnot(
    "Base R generic dispatch in imported package must match Base R" =
      compare_execution("pkgConsumer", "get_boxed()", lib),
    ".onLoad hook execution in imported package must match Base R" =
      compare_execution("pkgConsumer", "get_hook_state()", lib),
    "pkgConsumer session state must match Base R" =
      compare_to_base_r("pkgConsumer", lib)
  )
})

# 14.3 Selective Function Import via importFrom
#
#   [pkgClient] (Target)
#         │
#   (importFrom(pkgLib, calc_add))
#         │
#         ▼
#       pkgLib (Exports calc_add, calc_sub)
#
local({
  pkgLib <- mock_package(
    "pkgLib",
    code = c(
      "calc_add <- function(a, b) a + b",
      "calc_sub <- function(a, b) a - b"
    ),
    exports = c("calc_add", "calc_sub")
  )

  pkgClient <- mock_package(
    "pkgClient",
    imports = list(pkgLib),
    code = "run_calc <- function() calc_add(10, 25)",
    exports = "run_calc",
    namespace_extra = "importFrom(pkgLib, calc_add)"
  )

  lib <- create_mock_library(pkgClient, pkgLib)
  on.exit(lib$cleanup(), add = TRUE)

  stopifnot(
    "Selective importFrom call execution must match Base R" =
      compare_execution("pkgClient", "run_calc()", lib),
    "pkgClient session state must match Base R" =
      compare_to_base_r("pkgClient", lib)
  )
})

# 14.4 S4 Generic & Class Definition in Imports, Method in Second Imports
#
#   [pkgS4App] (Target)
#      │         │
#  (Imports)  (Imports)
#      │         │
#      ▼         ▼
#  pkgS4Method  pkgS4Generic (Defines S4Entity & generic entity_summary)
#    (Implements setMethod("entity_summary", "S4Entity"))
#
local({
  pkgS4Generic <- mock_package(
    "pkgS4Generic",
    imports = "methods",
    code = c(
      "setClass('S4Entity', slots = c(id = 'numeric', label = 'character'))",
      "setGeneric('entity_summary', function(object) {",
      "  standardGeneric('entity_summary')",
      "})"
    ),
    exports = "entity_summary",
    namespace_extra = c(
      "import(methods)",
      "exportClasses(S4Entity)",
      "exportMethods(entity_summary)"
    )
  )

  pkgS4Method <- mock_package(
    "pkgS4Method",
    imports = list("methods", pkgS4Generic),
    code = c(
      "setMethod('entity_summary', signature(object = 'S4Entity'),",
      "  function(object) {",
      "    paste0('S4Entity[', object@id, ':', object@label, ']')",
      "  }",
      ")",
      "create_entity <- function(id, label) {",
      "  new('S4Entity', id = id, label = label)",
      "}"
    ),
    exports = "create_entity",
    namespace_extra = c(
      "import(methods)",
      "import(pkgS4Generic)",
      "exportMethods(entity_summary)"
    )
  )

  pkgS4App <- mock_package(
    "pkgS4App",
    imports = list("methods", pkgS4Generic, pkgS4Method),
    code = c(
      "run_s4_test <- function() {",
      "  e <- create_entity(101, 'MockItem')",
      "  entity_summary(e)",
      "}"
    ),
    exports = "run_s4_test",
    namespace_extra = c(
      "import(methods)",
      "import(pkgS4Generic)",
      "import(pkgS4Method)"
    )
  )

  lib <- create_mock_library(pkgS4App, pkgS4Method, pkgS4Generic)
  on.exit(lib$cleanup(), add = TRUE)

  stopifnot(
    "S4 generic dispatch across imports must match Base R" =
      compare_execution("pkgS4App", "run_s4_test()", lib),
    "pkgS4App session state must match Base R" =
      compare_to_base_r("pkgS4App", lib)
  )
})

# 14.5 Reverse Target Order: Method Package Specified Before Generic Package
#
#   User requests: multiLibrary(pkgMethod, pkgGeneric)
#
#   [pkgMethod] (Target 1, listed first)
#        │
#    (Imports)
#        │
#        ▼
#   [pkgGeneric] (Target 2, defines generic greet())
#
local({
  pkgGeneric <- mock_package(
    "pkgGeneric",
    code = "greet <- function(x, ...) UseMethod('greet')",
    exports = "greet"
  )

  pkgMethod <- mock_package(
    "pkgMethod",
    imports = list(pkgGeneric),
    code = c(
      "make_custom <- function() structure(list(), class = 'custom_type')",
      "greet.custom_type <- function(x, ...) 'Dispatched greet.custom_type!'"
    ),
    exports = "make_custom",
    namespace_extra = "S3method(greet, custom_type)"
  )

  lib <- create_mock_library(pkgMethod, pkgGeneric)
  on.exit(lib$cleanup(), add = TRUE)

  # Verify topological resolution load_order
  res <- resolve_dependencies(
    c("pkgMethod", "pkgGeneric"),
    lib.loc = lib$lib_dir
  )
  stopifnot(
    "pkgGeneric must be assigned lower load_order than pkgMethod" =
      res$pkgGeneric$load_order < res$pkgMethod$load_order
  )

  # Verify execution and session state parity
  stopifnot(
    "Reverse order (pkgMethod, pkgGeneric) execution matches Base R" =
      compare_execution(
        c("pkgMethod", "pkgGeneric"),
        "greet(make_custom())",
        lib
      ),
    "Reverse order (pkgMethod, pkgGeneric) session state matches Base R" =
      compare_to_base_r(c("pkgMethod", "pkgGeneric"), lib),
    "Forward order (pkgGeneric, pkgMethod) session state matches Base R" =
      compare_to_base_r(c("pkgGeneric", "pkgMethod"), lib)
  )
})

# ==============================================================================
# 15. Method Dispatch in .onLoad Hooks (onLoadS3, onLoadS4)
# ==============================================================================
#
# 15.1 S3 Method Dispatch Executed Directly Inside Dependent's .onLoad()
#
#   [pkgS3Dependent] (Executes S3 method dispatch in .onLoad)
#          │
#      (Imports)
#          │
#          ▼
#   [pkgS3Provider] (Defines generic audit_entity & S3method(audit_entity))
#
local({
  pkgS3Provider <- mock_package(
    "pkgS3Provider",
    code = c(
      "audit_entity <- function(x, ...) UseMethod('audit_entity')",
      "audit_entity.AuditToken <- function(x, ...) {",
      "  paste0('AUDIT_OK:', x$name)",
      "}",
      "make_token <- function(n) {",
      "  structure(list(name = n), class = 'AuditToken')",
      "}"
    ),
    exports = c("audit_entity", "make_token"),
    namespace_extra = "S3method(audit_entity, AuditToken)"
  )

  pkgS3Dependent <- mock_package(
    "pkgS3Dependent",
    imports = list(pkgS3Provider),
    code = c(
      ".onLoad <- function(libname, pkgname) {",
      "  tok <- make_token('onLoadS3')",
      "  res <- audit_entity(tok)",
      "  if (res != 'AUDIT_OK:onLoadS3') {",
      "    stop('Failed S3 dispatch in .onLoad!')",
      "  }",
      "  options(on_load_s3_result = res)",
      "}",
      "get_on_load_s3_status <- function() {",
      "  getOption('on_load_s3_result')",
      "}"
    ),
    exports = "get_on_load_s3_status"
  )

  lib <- create_mock_library(pkgS3Dependent, pkgS3Provider)
  on.exit(lib$cleanup(), add = TRUE)

  stopifnot(
    "S3 method dispatch in .onLoad must execute and match Base R" =
      compare_execution(
        "pkgS3Dependent",
        "get_on_load_s3_status()",
        lib
      ),
    "pkgS3Dependent session state must match Base R" =
      compare_to_base_r("pkgS3Dependent", lib)
  )
})

# 15.2 S4 Method Dispatch Executed Directly Inside Dependent's .onLoad()
#
#   [pkgS4Dependent] (Executes S4 method dispatch in .onLoad)
#          │
#      (Imports)
#          │
#          ▼
#   [pkgS4Provider] (Defines S4 class, generic, and S4 method)
#
local({
  pkgS4Provider <- mock_package(
    "pkgS4Provider",
    imports = "methods",
    code = c(
      "setClass('S4ValToken', slots = c(code = 'numeric', name = 'character'))",
      "setGeneric('eval_token', function(x) standardGeneric('eval_token'))",
      "setMethod('eval_token', signature(x = 'S4ValToken'), function(x) {",
      "  paste0('S4_OK:', x@name)",
      "})",
      "make_s4_token <- function(n) new('S4ValToken', code = 1, name = n)"
    ),
    exports = c("eval_token", "make_s4_token"),
    namespace_extra = c(
      "import(methods)",
      "exportClasses(S4ValToken)",
      "exportMethods(eval_token)"
    )
  )

  pkgS4Dependent <- mock_package(
    "pkgS4Dependent",
    imports = list("methods", pkgS4Provider),
    code = c(
      ".onLoad <- function(libname, pkgname) {",
      "  tok <- make_s4_token('onLoadS4')",
      "  res <- eval_token(tok)",
      "  if (res != 'S4_OK:onLoadS4') {",
      "    stop('Failed S4 dispatch in .onLoad!')",
      "  }",
      "  options(on_load_s4_result = res)",
      "}",
      "get_on_load_s4_status <- function() {",
      "  getOption('on_load_s4_result')",
      "}"
    ),
    exports = "get_on_load_s4_status",
    namespace_extra = c(
      "import(methods)",
      "import(pkgS4Provider)"
    )
  )

  lib <- create_mock_library(pkgS4Dependent, pkgS4Provider)
  on.exit(lib$cleanup(), add = TRUE)

  stopifnot(
    "S4 method dispatch in .onLoad must execute and match Base R" =
      compare_execution(
        "pkgS4Dependent",
        "get_on_load_s4_status()",
        lib
      ),
    "pkgS4Dependent session state must match Base R" =
      compare_to_base_r("pkgS4Dependent", lib)
  )
})

# ==============================================================================
# 16. Diamond Imports with Distinct S3 Methods
# ==============================================================================
#
#        [pkgDiamondApp] (Target)
#          /        \
#     (Imports)   (Imports)
#        /            \
#   pkgBranch1     pkgBranch2
#   (Method for    (Method for
#    Class 1)       Class 2)
#        \            /
#     (Imports)   (Imports)
#          \        /
#        [pkgBaseGeneric] (Defines generic calc_metric())
#
local({
  pkgBaseGeneric <- mock_package(
    "pkgBaseGeneric",
    code = "calc_metric <- function(x, ...) UseMethod('calc_metric')",
    exports = "calc_metric"
  )

  pkgBranch1 <- mock_package(
    "pkgBranch1",
    imports = list(pkgBaseGeneric),
    code = c(
      "make_b1 <- function(v) structure(list(v = v), class = 'b1_class')",
      "calc_metric.b1_class <- function(x, ...) x$v * 2"
    ),
    exports = "make_b1",
    namespace_extra = "S3method(calc_metric, b1_class)"
  )

  pkgBranch2 <- mock_package(
    "pkgBranch2",
    imports = list(pkgBaseGeneric),
    code = c(
      "make_b2 <- function(v) structure(list(v = v), class = 'b2_class')",
      "calc_metric.b2_class <- function(x, ...) x$v * 10"
    ),
    exports = "make_b2",
    namespace_extra = "S3method(calc_metric, b2_class)"
  )

  pkgDiamondApp <- mock_package(
    "pkgDiamondApp",
    imports = list(pkgBaseGeneric, pkgBranch1, pkgBranch2),
    code = c(
      "run_diamond <- function() {",
      "  paste(calc_metric(make_b1(5)), calc_metric(make_b2(5)))",
      "}"
    ),
    exports = "run_diamond"
  )

  lib <- create_mock_library(
    pkgDiamondApp,
    pkgBranch1,
    pkgBranch2,
    pkgBaseGeneric
  )
  on.exit(lib$cleanup(), add = TRUE)

  stopifnot(
    "Diamond import S3 method dispatch must execute and match Base R" =
      compare_execution("pkgDiamondApp", "run_diamond()", lib),
    "pkgDiamondApp session state must match Base R" =
      compare_to_base_r("pkgDiamondApp", lib)
  )
})

# ==============================================================================
# 17. Re-Exported Functions / Namespace Aliasing
# ==============================================================================
#
#   [pkgConsumer] -> [pkgFacade] (re-exports) -> [pkgEngine]
#
local({
  pkgEngine <- mock_package(
    "pkgEngine",
    code = "engine_core <- function(x) x^2",
    exports = "engine_core"
  )

  pkgFacade <- mock_package(
    "pkgFacade",
    imports = list(pkgEngine),
    code = "engine_core <- pkgEngine::engine_core",
    exports = "engine_core"
  )

  pkgConsumer <- mock_package(
    "pkgConsumer",
    imports = list(pkgFacade),
    code = "run_facade <- function() pkgFacade::engine_core(7)",
    exports = "run_facade"
  )

  lib <- create_mock_library(pkgConsumer, pkgFacade, pkgEngine)
  on.exit(lib$cleanup(), add = TRUE)

  stopifnot(
    "Re-exported symbol invocation must execute and match Base R" =
      compare_execution("pkgConsumer", "run_facade()", lib),
    "pkgConsumer session state must match Base R" =
      compare_to_base_r("pkgConsumer", lib)
  )
})

# ==============================================================================
# 18. S3 Method Overwriting Precedence Across Target Orders
# ==============================================================================
#
#   pkgM1 and pkgM2 both implement S3 method for the SAME generic & class.
#   Whichever target is loaded second must overwrite the previous method.
#
local({
  pkgGen <- mock_package(
    "pkgGen",
    code = "compute <- function(x, ...) UseMethod('compute')",
    exports = "compute"
  )

  pkgM1 <- mock_package(
    "pkgM1",
    imports = list(pkgGen),
    code = c(
      "make_item <- function() structure(list(), class = 'item_class')",
      "compute.item_class <- function(x, ...) 'M1_METHOD'"
    ),
    exports = "make_item",
    namespace_extra = "S3method(compute, item_class)"
  )

  pkgM2 <- mock_package(
    "pkgM2",
    imports = list(pkgGen),
    code = "compute.item_class <- function(x, ...) 'M2_METHOD'",
    exports = character(0L),
    namespace_extra = "S3method(compute, item_class)"
  )

  lib <- create_mock_library(pkgM1, pkgM2, pkgGen)
  on.exit(lib$cleanup(), add = TRUE)

  stopifnot(
    "Order (pkgM1, pkgM2) method overwrite must match Base R" =
      compare_execution(
        c("pkgM1", "pkgM2"),
        "pkgGen::compute(make_item())",
        lib
      ),
    "Order (pkgM2, pkgM1) method overwrite must match Base R" =
      compare_execution(
        c("pkgM2", "pkgM1"),
        "pkgGen::compute(make_item())",
        lib
      )
  )
})

# ==============================================================================
# 19. Imports Declaration Order Fidelity (Sibling Tie-Breaking)
# ==============================================================================
#
#   [pkgApp] lists Imports: pkgZ, pkgA (in that exact declaration order)
#
#   Base R evaluates Imports left-to-right (pkgZ then pkgA).
#   If both pkgZ and pkgA implement the same S3 method for a generic in pkgGen,
#   pkgA must overwrite pkgZ in Base R.
#   multiLibrary must preserve this exact declaration sequence.
#
local({
  pkgGen <- mock_package(
    "pkgGen",
    code = "greet <- function(x, ...) UseMethod('greet')",
    exports = "greet"
  )

  pkgZ <- mock_package(
    "pkgZ",
    imports = list(pkgGen),
    code = c(
      "make_item <- function() structure(list(), class = 'item_class')",
      "greet.item_class <- function(x, ...) 'Z_METHOD'"
    ),
    exports = "make_item",
    namespace_extra = "S3method(greet, item_class)"
  )

  pkgA <- mock_package(
    "pkgA",
    imports = list(pkgGen),
    code = "greet.item_class <- function(x, ...) 'A_METHOD'",
    exports = character(0L),
    namespace_extra = "S3method(greet, item_class)"
  )

  pkgApp <- mock_package(
    "pkgApp",
    imports = list(pkgZ, pkgA, pkgGen),
    code = "run_app <- function() greet(make_item())",
    exports = "run_app"
  )

  lib <- create_mock_library(pkgApp, pkgZ, pkgA, pkgGen)
  on.exit(lib$cleanup(), add = TRUE)

  stopifnot(
    "Imports declaration order (pkgZ, pkgA) execution must match Base R" =
      compare_execution("pkgApp", "run_app()", lib)
  )
})

cat("All consolidated multiLibrary tests passed successfully!\n")
