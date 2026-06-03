# Test setup: configure property-based testing parameters
#
# {quickcheck} is not currently installed. The project uses manual
# randomization with set.seed() and replicated iterations instead.
#
# This file sets the minimum iteration count for property-based tests
# and provides a helper to check if quickcheck is available.

# Minimum iterations for property-based tests (per the design doc)
PBT_MIN_ITERATIONS <- 100
PBT_DEFAULT_ITERATIONS <- 120  # Slightly above minimum for margin

# Check if {quickcheck} is available
HAS_QUICKCHECK <- requireNamespace("quickcheck", quietly = TRUE)

if (HAS_QUICKCHECK) {
  library(quickcheck)
  # Configure quickcheck with minimum 100 iterations
  # quickcheck uses `tests` parameter in for_all() calls
  message("Using {quickcheck} for property-based tests (min iterations: ", PBT_MIN_ITERATIONS, ")")
} else {
  message("Note: {quickcheck} not installed. Using manual randomization for property tests.")
  message("  Install with: install.packages('quickcheck')")
  message("  Minimum iterations per property: ", PBT_MIN_ITERATIONS)
}
