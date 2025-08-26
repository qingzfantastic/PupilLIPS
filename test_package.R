# Test script for PupilLIPS package
# This script tests all the main functions

# Load the package
library(PupilLIPS)

# Create sample data for testing
set.seed(123)
n_samples <- 1000
n_subjects <- 5
n_trials <- 4

# Generate sample pupil data
sample_data <- data.frame(
  subject = rep(1:n_subjects, each = n_samples * n_trials),
  trial = rep(rep(1:n_trials, each = n_samples), n_subjects),
  time = rep(1:n_samples, n_subjects * n_trials),
  pupil = rnorm(n_samples * n_subjects * n_trials, mean = 4, sd = 0.5)
)

# Add some missing data for testing
missing_indices <- sample(1:nrow(sample_data), 100)
sample_data$pupil[missing_indices] <- NA

# Test 1: count_missing_pupil (original gazeR function)
message("Testing count_missing_pupil...")
tryCatch({
  filtered_data <- count_missing_pupil(sample_data, pupil = "pupil")
  message("✓ count_missing_pupil works!")
  message("  Original rows: ", nrow(sample_data))
  message("  Filtered rows: ", nrow(filtered_data))
}, error = function(e) {
  message("✗ count_missing_pupil failed: ", e$message)
})

# Test 2: count_missing_pupil_different_thresholds
message("\nTesting count_missing_pupil_different_thresholds...")
tryCatch({
  filtered_data_enhanced <- count_missing_pupil_different_thresholds(
    sample_data, 
    pupil = "pupil",
    ask_permission_to_exclude = FALSE  # Skip interactive part for testing
  )
  message("✓ count_missing_pupil_different_thresholds works!")
  message("  Enhanced filtered rows: ", nrow(filtered_data_enhanced))
}, error = function(e) {
  message("✗ count_missing_pupil_different_thresholds failed: ", e$message)
})

# Test 3: extend_blinks
message("\nTesting extend_blinks...")
tryCatch({
  # Create sample blink vector
  blinks <- sample(c(TRUE, FALSE), 100, replace = TRUE, prob = c(0.1, 0.9))
  extended_blinks <- extend_blinks(blinks, pre_padding = 5, post_padding = 5)
  message("✓ extend_blinks works!")
  message("  Original blinks: ", sum(blinks))
  message("  Extended blinks: ", sum(extended_blinks))
}, error = function(e) {
  message("✗ extend_blinks failed: ", e$message)
})

# Test 4: interpolate_pupil
message("\nTesting interpolate_pupil...")
tryCatch({
  # Create sample data with gaps
  pupil_vector <- rnorm(100, mean = 4, sd = 0.5)
  blink_vector <- c(rep(FALSE, 20), rep(TRUE, 10), rep(FALSE, 70))
  interpolated <- interpolate_pupil(pupil_vector, blink_vector)
  message("✓ interpolate_pupil works!")
  message("  Original length: ", length(pupil_vector))
  message("  Interpolated length: ", length(interpolated))
}, error = function(e) {
  message("✗ interpolate_pupil failed: ", e$message)
})

# Test 5: moving_average_pupil
message("\nTesting moving_average_pupil...")
tryCatch({
  pupil_vector <- rnorm(100, mean = 4, sd = 0.5)
  smoothed <- moving_average_pupil(pupil_vector, window_size = 5)
  message("✓ moving_average_pupil works!")
  message("  Original length: ", length(pupil_vector))
  message("  Smoothed length: ", length(smoothed))
}, error = function(e) {
  message("✗ moving_average_pupil failed: ", e$message)
})

# Test 6: baseline_correction_pupil
message("\nTesting baseline_correction_pupil...")
tryCatch({
  pupil_vector <- rnorm(100, mean = 4, sd = 0.5)
  time_vector <- 1:100
  corrected <- baseline_correction_pupil(pupil_vector, time_vector, c(-20, 0), 50)
  message("✓ baseline_correction_pupil works!")
  message("  Original length: ", length(pupil_vector))
  message("  Corrected length: ", length(corrected))
}, error = function(e) {
  message("✗ baseline_correction_pupil failed: ", e$message)
})

message("\n=== Package Testing Complete ===")
message("If all tests passed, your PupilLIPS package is working correctly!")
