# PupilLIPS Package

An internal R package for preprocessing pupillometry data in the lab. This package provides essential functions for analyzing and preprocessing pupil diameter data without external dependencies.

## Installation

### From GitHub (Recommended)

```r
# Install devtools if you don't have it
if (!require("devtools")) install.packages("devtools")

# Install the package from GitHub
devtools::install_github("your-username/PupilLIPS")
```

### From Local Source

```r
# Navigate to the package directory
setwd("path/to/lab_pupillometry_package")

# Install the package
devtools::install()
```

## Functions

### 1. `count_missing_pupil`
Original gazeR function for basic missing data analysis.

```r
filtered_data <- count_missing_pupil(
  datafile = pupil_data,
  pupil = "pupil_diameter",
  participant_thresh = 0.2,
  trial_thresh = 0.2
)
```

### 2. `count_missing_pupil_different_thresholds`
Enhanced version with additional exclusion criteria and researcher confirmation.

```r
filtered_data <- count_missing_pupil_different_thresholds(
  datafile = pupil_data,
  pupil = "pupil_diameter",
  participant_thresh = 0.15,
  trial_thresh = 0.25,
  participant_trial_threshold = 0.2,
  ask_permission_to_exclude = TRUE
)
```

### 3. `extend_blinks`
Extend blink periods with padding before and after detected blinks.

```r
extended_blinks <- extend_blinks(
  blinks = blink_vector,
  pre_padding = 50,
  post_padding = 50
)
```

### 4. `interpolate_pupil`
Interpolate missing or blink-affected pupil data.

```r
interpolated_data <- interpolate_pupil(
  pupil_data = pupil_diameter,
  blinks = blink_vector,
  method = "linear",
  max_gap = 100
)
```

### 5. `moving_average_pupil`
Apply moving average smoothing to reduce noise.

```r
smoothed_data <- moving_average_pupil(
  pupil_data = pupil_diameter,
  window_size = 5,
  method = "mean",
  align = "center"
)
```

### 6. `baseline_correction_pupil`
Perform baseline correction on pupil data.

```r
corrected_data <- baseline_correction_pupil(
  pupil_data = pupil_diameter,
  time_vector = time_ms,
  baseline_window = c(-200, 0),
  stimulus_onset = 1000,
  method = "subtract"
)
```

## Dependencies

The package requires the following R packages:
- dplyr
- ggplot2
- tidyr
- readr
- signal
- zoo
- data.table

## Usage Example

```r
# Load the package
library(PupilLIPS)

# Load your pupil data
pupil_data <- read.csv("your_pupil_data.csv")

# Apply the complete preprocessing pipeline
# 1. Count missing data and filter
filtered_data <- count_missing_pupil_different_thresholds(
  datafile = pupil_data,
  pupil = "pupil_diameter"
)

# 2. Detect and extend blinks
blinks <- detect_blinks(filtered_data$pupil_diameter)
extended_blinks <- extend_blinks(blinks)

# 3. Interpolate missing data
interpolated_data <- interpolate_pupil(
  filtered_data$pupil_diameter,
  extended_blinks
)

# 4. Apply smoothing
smoothed_data <- moving_average_pupil(interpolated_data)

# 5. Baseline correction
corrected_data <- baseline_correction_pupil(
  smoothed_data,
  time_vector = filtered_data$time_ms,
  baseline_window = c(-200, 0),
  stimulus_onset = 1000
)
```

## Contributing

This is an internal lab package. Please contact the lab supervisor for contributions.

## License

MIT License - see LICENSE file for details.
