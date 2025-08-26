# Installation script for PupilLIPS package
# Run this script to install the package locally

# Check if devtools is installed
if (!require("devtools", quietly = TRUE)) {
  message("Installing devtools...")
  install.packages("devtools")
}

# Load devtools
library(devtools)

# Set working directory to package location (adjust path as needed)
# setwd("path/to/lab_pupillometry_package")

# Install the package
message("Installing PupilLIPS package...")
install()

# Test the installation
message("Testing package installation...")
library(PupilLIPS)

# List available functions
message("Available functions in PupilLIPS:")
ls("package:PupilLIPS")

message("PupilLIPS package installed successfully!")
message("You can now use: library(PupilLIPS)")
