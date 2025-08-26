##################################################################
# STRESS PUPIL PREPROCESSING PIPELINE
# Written for [Your Lab Name] - [Date]
# Based on data structure from 02StressPupilEnglish/01originals
##################################################################

#-----------------------------------------------------------------
# 1. LOAD REQUIRED PACKAGES
#-----------------------------------------------------------------

# Install required packages if not already installed
required_packages <- c("dplyr", "ggplot2", "tidyr", "readr", "signal", "zoo", "data.table")

# Function to install packages if needed
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      install.packages(pkg)
      library(pkg, character.only = TRUE)
    }
  }
}

# Try to load gazer, but provide alternative if not available
if (!require("gazer", character.only = TRUE, quietly = TRUE)) {
  message("gazer package not found. Attempting to install...")
  if (!require("remotes")) install.packages("remotes")
  tryCatch({
    remotes::install_github("dmirman/gazer")
    library(gazer)
    message("gazer package installed and loaded successfully!")
  }, error = function(e) {
    message("Could not install gazer package. Will use alternative approach.")
    message("You may need to install it manually: remotes::install_github('dmirman/gazer')")
  })
}

# Install and load packages
install_if_missing(required_packages)

#-----------------------------------------------------------------
# 2. SETUP AND CONFIGURATION
#-----------------------------------------------------------------

# Clear environment
rm(list = ls())
cat("\014") # Clear console

# Configuration for your stress pupil experiment
config <- list(
  # Experiment settings
  experiment_name = "StressPupilEnglish",
  sample_rate = 1000, # Hz - typical for eye-trackers
  
  # Data paths
  base_path = "/Users/qinghare/Desktop/Pupillometry_Data/02StressPupilEnglish",
  originals_path = "01originals",
  output_path = "02analyses",
  
  # Preprocessing parameters
  blink_threshold = 0.5, # seconds
  interpolation_method = "linear", # "linear", "cubic", "spline"
  smoothing_window = 5, # samples for moving average
  
  # Filtering parameters
  lowpass_cutoff = 4, # Hz
  highpass_cutoff = 0.01, # Hz
  
  # Baseline correction
  baseline_window = c(-200, 0), # milliseconds relative to stimulus onset
  
  # Output settings
  save_intermediate = TRUE,
  output_format = "csv"
)

#-----------------------------------------------------------------
# 3. DATA LOADING FUNCTIONS
#-----------------------------------------------------------------

#' Load ASC file data using gazerR
#' @param file_path Path to the ASC file
#' @return Parsed ASC data object
load_asc_data <- function(file_path) {
  message("Loading ASC file: ", basename(file_path))
  
  tryCatch({
    # Parse ASC file using gazerR
    data <- gazer::parse_asc(file_path)
    message("Data loaded successfully. Sample rate: ", data$info$sample.rate, " Hz")
    return(data)
  }, error = function(e) {
    message("Error loading ASC file: ", e$message)
    return(NULL)
  })
}

#' Extract trial information from ASC messages
#' @param asc_data Parsed ASC data object
#' @return Data frame with trial information
extract_trial_info <- function(asc_data) {
  message("Extracting trial information...")
  
  # Get all messages
  messages <- asc_data$msg
  
  # Find key trial messages based on the reference scripts
  audio_onset <- messages[grep("PlaySound", messages$text, ignore.case = TRUE), ]
  audio_offset <- messages[grep("EndSound", messages$text, ignore.case = TRUE), ]
  trial_offset <- messages[grep("BlinkCue", messages$text, ignore.case = TRUE), ]
  
  # Get syllable onset times
  OnsetSyll1 <- messages[grep("0 OnsetSyll1", messages$text, ignore.case = TRUE), ]
  OnsetSyll2 <- messages[grep("0 OnsetSyll2", messages$text, ignore.case = TRUE), ]
  OnsetSyll3 <- messages[grep("0 OnsetSyll3", messages$text, ignore.case = TRUE), ]
  OnsetSyll4 <- messages[grep("0 OnsetSyll4", messages$text, ignore.case = TRUE), ]
  EndofCompound <- messages[grep("EndofCompound", messages$text, ignore.case = TRUE), ]
  
  # Get trial condition information
  filename <- messages[grep("itemcode", messages$text, ignore.case = TRUE), ]
  rhythmcond <- messages[grep("APLAYSTART", messages$text, ignore.case = TRUE), ]
  response <- messages[grep("keypress", messages$text, ignore.case = FALSE), ]
  
  # Create trial times dataframe
  ntrials <- length(audio_onset$time)
  trial_onset_time <- audio_onset$time - 1000  # 1000ms before stimulus onset for baseline
  trial_end_time <- trial_offset$time - 1000   # 1000ms before blink cue
  
  dfTrialTimes <- data.frame(
    trialID = 1:ntrials,
    trial_onset_time = trial_onset_time,
    trial_end_time = trial_end_time,
    audio_onset_time = audio_onset$time,
    audio_offset_time = audio_offset$time
  )
  
  # Add syllable timing information
  if (nrow(OnsetSyll1) > 0) dfTrialTimes$OnsetSyll1 <- OnsetSyll1$time
  if (nrow(OnsetSyll2) > 0) dfTrialTimes$OnsetSyll2 <- OnsetSyll2$time
  if (nrow(OnsetSyll3) > 0) dfTrialTimes$OnsetSyll3 <- OnsetSyll3$time
  if (nrow(OnsetSyll4) > 0) dfTrialTimes$OnsetSyll4 <- OnsetSyll4$time
  if (nrow(EndofCompound) > 0) dfTrialTimes$EndofCompound <- EndofCompound$time
  
  # Add condition information
  if (nrow(filename) > 0) dfTrialTimes$itemcode <- filename$text
  if (nrow(rhythmcond) > 0) dfTrialTimes$rhythmcondition <- rhythmcond$text
  if (nrow(response) > 0) dfTrialTimes$response <- response$text
  
  return(dfTrialTimes)
}

#' Extract pupil data for specific trial segments
#' @param asc_data Parsed ASC data object
#' @param start_message Message marking start of segment
#' @param end_message Message marking end of segment
#' @return Data frame with pupil data for segment
extract_segment_data <- function(asc_data, start_message, end_message) {
  # Find message indices
  start_indices <- grep(start_message, asc_data$msg$text)
  end_indices <- grep(end_message, asc_data$msg$text)
  
  # Extract data between messages
  segment_data <- data.frame()
  
  for (i in seq_along(start_indices)) {
    if (i <= length(end_indices)) {
      start_time <- asc_data$msg$time[start_indices[i]]
      end_time <- asc_data$msg$time[end_indices[i]]
      
      # Get pupil data between these times
      segment_samples <- asc_data$raw[asc_data$raw$time >= start_time & 
                                     asc_data$raw$time <= end_time, ]
      
      if (nrow(segment_samples) > 0) {
        segment_samples$segment <- paste0(start_message, "_", i)
        segment_samples$trial <- i
        segment_data <- rbind(segment_data, segment_samples)
      }
    }
  }
  
  return(segment_data)
}

#-----------------------------------------------------------------
# 4. PREPROCESSING FUNCTIONS
#-----------------------------------------------------------------

#' Detect and mark blinks
#' @param pupil_data Vector of pupil diameter values
#' @param threshold Blink detection threshold
#' @return Logical vector indicating blink periods
detect_blinks <- function(pupil_data, threshold = 0.5) {
  message("Detecting blinks...")
  
  # Simple blink detection based on rapid changes
  pupil_diff <- diff(pupil_data)
  blink_indices <- abs(pupil_diff) > threshold
  
  # Expand to match original data length
  blinks <- c(FALSE, blink_indices)
  
  message("Detected ", sum(blinks), " potential blink periods")
  return(blinks)
}

#' Interpolate missing/blink data
#' @param pupil_data Vector of pupil diameter values
#' @param blinks Logical vector indicating blink periods
#' @param method Interpolation method
#' @return Vector with interpolated values
interpolate_pupil_data <- function(pupil_data, blinks, method = "linear") {
  message("Interpolating pupil data using ", method, " method...")
  
  # Create time index
  time_index <- seq_along(pupil_data)
  
  # Interpolate only where blinks are detected
  interpolated_data <- pupil_data
  
  if (sum(blinks) > 0) {
    # Get non-blink indices
    valid_indices <- time_index[!blinks]
    valid_values <- pupil_data[!blinks]
    
    # Interpolate
    if (method == "linear") {
      interpolated_data[blinks] <- approx(valid_indices, valid_values, 
                                         xout = time_index[blinks], 
                                         method = "linear")$y
    } else if (method == "cubic") {
      interpolated_data[blinks] <- spline(valid_indices, valid_values, 
                                         xout = time_index[blinks])$y
    }
  }
  
  return(interpolated_data)
}

#' Apply signal filtering
#' @param pupil_data Vector of pupil diameter values
#' @param sample_rate Sampling rate in Hz
#' @param lowpass_cutoff Low-pass filter cutoff frequency
#' @param highpass_cutoff High-pass filter cutoff frequency
#' @return Filtered pupil data
filter_pupil_data <- function(pupil_data, sample_rate, lowpass_cutoff = 4, highpass_cutoff = 0.01) {
  message("Applying signal filtering...")
  
  # Design filters
  nyquist <- sample_rate / 2
  
  # Low-pass filter
  if (lowpass_cutoff < nyquist) {
    lowpass_filter <- signal::butter(4, lowpass_cutoff / nyquist, type = "low")
    pupil_data <- signal::filtfilt(lowpass_filter, pupil_data)
  }
  
  # High-pass filter
  if (highpass_cutoff > 0) {
    highpass_filter <- signal::butter(4, highpass_cutoff / nyquist, type = "high")
    pupil_data <- signal::filtfilt(highpass_filter, pupil_data)
  }
  
  return(pupil_data)
}

#' Apply smoothing using moving average
#' @param pupil_data Vector of pupil diameter values
#' @param window_size Window size for moving average
#' @return Smoothed pupil data
smooth_pupil_data <- function(pupil_data, window_size = 5) {
  message("Applying smoothing with window size: ", window_size)
  
  # Use zoo package for rolling mean
  smoothed_data <- zoo::rollmean(pupil_data, window_size, fill = "extend", align = "center")
  
  return(smoothed_data)
}

#' Perform baseline correction
#' @param pupil_data Vector of pupil diameter values
#' @param time_vector Vector of time points
#' @param baseline_window Baseline window in milliseconds
#' @param stimulus_onset Stimulus onset time
#' @return Baseline-corrected pupil data
baseline_correct <- function(pupil_data, time_vector, baseline_window, stimulus_onset) {
  message("Performing baseline correction...")
  
  # Convert baseline window to time units
  baseline_start <- stimulus_onset + baseline_window[1] / 1000
  baseline_end <- stimulus_onset + baseline_window[2] / 1000
  
  # Find baseline indices
  baseline_indices <- time_vector >= baseline_start & time_vector <= baseline_end
  
  if (sum(baseline_indices) > 0) {
    # Calculate baseline mean
    baseline_mean <- mean(pupil_data[baseline_indices], na.rm = TRUE)
    
    # Subtract baseline
    corrected_data <- pupil_data - baseline_mean
  } else {
    warning("Baseline window not found in data")
    corrected_data <- pupil_data
  }
  
  return(corrected_data)
}

#-----------------------------------------------------------------
# 5. MAIN PREPROCESSING PIPELINE
#-----------------------------------------------------------------

#' Main preprocessing pipeline function for a single subject
#' @param subject_id Subject ID (e.g., "S09")
#' @param config Configuration list
#' @return Processed data object
process_subject <- function(subject_id, config) {
  message("Processing subject: ", subject_id)
  
  # Construct file paths
  subject_dir <- file.path(config$base_path, config$originals_path, paste0("SPE_", subject_id))
  asc_file <- file.path(subject_dir, paste0("SPE_", subject_id, ".asc"))
  output_dir <- file.path(config$base_path, config$output_path, paste0("SPE_", subject_id))
  
  # Check if files exist
  if (!file.exists(asc_file)) {
    message("ASC file not found: ", asc_file)
    return(NULL)
  }
  
  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Step 1: Load ASC data
  message("\n=== STEP 1: Loading ASC Data ===")
  asc_data <- load_asc_data(asc_file)
  if (is.null(asc_data)) return(NULL)
  
  # Step 2: Extract trial information
  message("\n=== STEP 2: Extracting Trial Information ===")
  trial_info <- extract_trial_info(asc_data)
  
  # Step 3: Extract pupil data for key segments
  message("\n=== STEP 3: Extracting Segment Data ===")
  
  # Define key segments based on your experiment
  segments <- list(
    baseline = c("StartBaselinePupil", "EndBaselinePupil"),
    stimulus = c("PlaySound", "EndSound"),
    response = c("EndSound", "ResponseCue")
  )
  
  segment_data <- list()
  for (segment_name in names(segments)) {
    segment_data[[segment_name]] <- extract_segment_data(asc_data, 
                                                        segments[[segment_name]][1], 
                                                        segments[[segment_name]][2])
  }
  
  # Step 4: Process pupil data
  message("\n=== STEP 4: Processing Pupil Data ===")
  
  # Combine all segment data
  all_data <- do.call(rbind, segment_data)
  
  if (nrow(all_data) > 0) {
    # Detect blinks
    blinks <- detect_blinks(all_data$pupil, config$blink_threshold)
    
    # Interpolate missing data
    interpolated_data <- interpolate_pupil_data(all_data$pupil, blinks, config$interpolation_method)
    
    # Apply filtering
    filtered_data <- filter_pupil_data(interpolated_data, config$sample_rate, 
                                      config$lowpass_cutoff, config$highpass_cutoff)
    
    # Apply smoothing
    smoothed_data <- smooth_pupil_data(filtered_data, config$smoothing_window)
    
    # Add processed data to dataframe
    all_data$pupil_processed <- smoothed_data
    all_data$blink <- blinks
  }
  
  message("\n=== PREPROCESSING COMPLETE FOR SUBJECT ", subject_id, " ===")
  
  # Save processed data
  output_file <- file.path(output_dir, paste0("processed_", subject_id, ".csv"))
  write.csv(all_data, output_file, row.names = FALSE)
  message("Processed data saved to: ", output_file)
  
  # Return results
  return(list(
    subject_id = subject_id,
    original_data = asc_data,
    processed_data = all_data,
    trial_info = trial_info,
    config = config,
    timestamp = Sys.time()
  ))
}

#' Process all subjects
#' @param subject_list List of subject IDs
#' @param config Configuration list
#' @return List of results for all subjects
process_all_subjects <- function(subject_list, config) {
  message("Processing ", length(subject_list), " subjects...")
  
  results <- list()
  for (subject in subject_list) {
    message("\n", "="*50)
    result <- process_subject(subject, config)
    if (!is.null(result)) {
      results[[subject]] <- result
    }
    message("="*50, "\n")
  }
  
  return(results)
}

#-----------------------------------------------------------------
# 6. RUN THE PIPELINE
#-----------------------------------------------------------------

# Define subjects to process
subjects <- c("S09", "S10", "S11", "S13")

# Run preprocessing for all subjects
message("Starting pupillometry preprocessing pipeline...")
results <- process_all_subjects(subjects, config)

# Save summary results
summary_file <- file.path(config$base_path, config$output_path, "preprocessing_summary.rds")
saveRDS(results, summary_file)
message("Summary results saved to: ", summary_file)

message("Pipeline complete! Check the output directory for processed data.")

#-----------------------------------------------------------------
# 7. TEST SECTION - UNCOMMENT TO TEST WITH SINGLE SUBJECT
#-----------------------------------------------------------------

# Uncomment the lines below to test with just one subject first
# message("Testing with single subject...")
# test_result <- process_subject("S09", config)
# if (!is.null(test_result)) {
#   message("Test successful! Processed data for subject S09")
#   print(str(test_result))
# } else {
#   message("Test failed. Check error messages above.")
# }
