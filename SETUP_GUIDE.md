# PupilLIPS Package Setup Guide

## Quick Start

### 1. Install the Package Locally

```r
# Navigate to the package directory
setwd("path/to/lab_pupillometry_package")

# Install devtools if you don't have it
if (!require("devtools")) install.packages("devtools")

# Install the package
devtools::install()
```

### 2. Test the Package

```r
# Load the package
library(PupilLIPS)

# Run the test script
source("test_package.R")
```

## GitHub Setup (Optional)

If you want to put this on GitHub for your lab:

### 1. Create a New Repository
- Go to GitHub and create a new repository named `PupilLIPS`
- Make it private (since it's internal lab use)

### 2. Push Your Package

```bash
# In your terminal, navigate to the package directory
cd path/to/lab_pupillometry_package

# Initialize git repository
git init

# Add all files
git add .

# Commit
git commit -m "Initial commit: PupilLIPS package"

# Add remote origin (replace with your GitHub username)
git remote add origin https://github.com/your-username/PupilLIPS.git

# Push to GitHub
git push -u origin main
```

### 3. Install from GitHub

```r
# Install from GitHub
devtools::install_github("your-username/PupilLIPS")
```

## What You Get

Your **PupilLIPS** package now includes:

1. **`count_missing_pupil`** - Original gazeR function
2. **`count_missing_pupil_different_thresholds`** - Enhanced version with researcher confirmation
3. **`extend_blinks`** - Extend blink periods with padding
4. **`interpolate_pupil`** - Interpolate missing data
5. **`moving_average_pupil`** - Apply smoothing
6. **`baseline_correction_pupil`** - Baseline correction

## Usage Example

```r
library(PupilLIPS)

# Your main function
filtered_data <- count_missing_pupil_different_thresholds(
  datafile = your_pupil_data,
  pupil = "pupil_diameter",
  participant_thresh = 0.15,
  trial_thresh = 0.25
)
```

## Troubleshooting

- **Package won't install**: Make sure you're in the package directory and have devtools installed
- **Functions not found**: Check that the package loaded correctly with `library(PupilLIPS)`
- **Dependencies missing**: Install required packages: `install.packages(c("dplyr", "ggplot2", "tidyr", "readr", "signal", "zoo", "data.table"))`

## Your Package is Ready! 🎉

You now have a complete internal R package that replaces the need for gazeR. Your supervisor can install it directly without external dependencies!
