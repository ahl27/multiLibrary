# multiLibrary package

This package implements `multiLibrary`, a function that imports multiple packages with automatic version resolution.

Syntax:

```r
multiLibrary::multiLibrary(packageA, packageB, packageC, ...)
```

This example is equivalent to running:

```r
library(packageA)
library(packageB)
library(packageC)
```

The order of the packages passed to `multiLibrary` is the order in which they'll be imported.

## Verification

Some helper functions are implemented at `tests/test_helpers.R`.

Lots of tests are implemented at `tests/test_multiLibrary.R`. This verifies that the core functionality works as expected and leaves the system in the same state
as would be expected with the traditional chained `library` calls.
