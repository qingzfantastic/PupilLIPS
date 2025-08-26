# PupilLIPS

Internal use R package ONLY!!

## Installation

```r
# Install devtools if you don't have it
if (!require("devtools")) install.packages("devtools")

# Install PupilLIPS from GitHub
devtools::install_github("qingzfantastic/PupilLIPS")

# Load the package
library(PupilLIPS)
```

## Functions

### Core Functions

**`count_missing_pupil`** - Original gazeR function for basic missing data analysis
```r
filtered_data <- count_missing_pupil(
  datafile = pupil_data,
  missingthresh = 0.2
)
```

**`count_missing_pupil_different_thresholds`** - Enhanced version with researcher confirmation
```r
filtered_data <- count_missing_pupil_different_thresholds(
  datafile = pupil_data,
  participant_thresh = 0.15,
  trial_thresh = 0.25,
  participant_trial_threshold = 0.2,
  ask_permission_to_exclude = TRUE
)
```

**`extend_blinks`** - Extend blink periods with padding
```r
extended_data <- extend_blinks(
  values = pupil_data,
  fillback = 200,
  fillforward = 100,
  hz = 1000
)
```

**`interpolate_pupil`** - Interpolate missing or blink-affected pupil data
```r
interpolated_data <- interpolate_pupil(
  datafile = pupil_data,
  pupil = "pupil",
  subject = "subject",
  trial = "trial",
  extendblinks = TRUE,
  type = "linear",
  hz = 1000
)
```

**`moving_average_pupil`** - Apply moving average smoothing
```r
smoothed_data <- moving_average_pupil(
  x = pupil_data,
  n = 5,
  centered = TRUE
)
```

**`baseline_correction_pupil`** - Perform baseline correction
```r
corrected_data <- baseline_correction_pupil(
  datafile = pupil_data,
  pupil_colname = "pupil",
  baseline_window = c(-200, 0),
  baseline_method = "sub"
)
```

## Complete Preprocessing Pipeline

```r
library(PupilLIPS)

# 1. Count missing data and filter
filtered_data <- count_missing_pupil_different_thresholds(
  datafile = pupil_data,
  participant_thresh = 0.2,
  trial_thresh = 0.2
)

# 2. Extend blinks
alldata <- filtered_data %>% 
  group_by(subject, trial) %>% 
  mutate(extendpupil = extend_blinks(pupil, fillback = 200, fillforward = 100, hz = 1000))

# 3. Interpolate missing data
alldata <- interpolate_pupil(alldata, extendblinks = TRUE, type = "linear")

# 4. Apply smoothing
rolling_mean_data <- alldata %>%
  mutate(movingavgpup = moving_average_pupil(pupil, n = 5))

# 5. Baseline correction
baseline_data <- baseline_correction_pupil(
  rolling_mean_data, 
  pupil_colname = "movingavgpup", 
  baseline_window = c(-500, 0), 
  baseline_method = "sub"
)
```

## Dependencies

The package requires:
- dplyr
- ggplot2
- tidyr
- readr
- signal
- zoo
- data.table