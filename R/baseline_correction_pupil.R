#' Perform Baseline Correction on Pupil Data
#'
#' This function performs baseline correction on pupil diameter data by
#' subtracting the mean baseline value from the entire time series.
#'
#' @param pupil_data Vector of pupil diameter values
#' @param time_vector Vector of time points (in milliseconds)
#' @param baseline_window Baseline window in milliseconds relative to stimulus onset (default: c(-200, 0))
#' @param stimulus_onset Stimulus onset time in milliseconds (default: 0)
#' @param method Baseline correction method: "subtract" or "divide" (default: "subtract")
#'
#' @return Baseline-corrected pupil data vector
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' corrected_data <- baseline_correction_pupil(
#'   pupil_data = pupil_diameter,
#'   time_vector = time_ms,
#'   baseline_window = c(-200, 0),
#'   stimulus_onset = 1000
#' )
#' }
baseline_correction_pupil <- function(pupil_data, time_vector, 
                                      baseline_window = c(-200, 0),
                                      stimulus_onset = 0, 
                                      method = "subtract") {
  message("Performing baseline correction...")
  
  # Convert baseline window to time units
  baseline_start <- stimulus_onset + baseline_window[1]
  baseline_end <- stimulus_onset + baseline_window[2]
  
  # Find baseline indices
  baseline_indices <- time_vector >= baseline_start & time_vector <= baseline_end
  
  if (sum(baseline_indices) > 0) {
    # Calculate baseline mean
    baseline_mean <- mean(pupil_data[baseline_indices], na.rm = TRUE)
    
    message("Baseline window: ", baseline_start, " to ", baseline_end, " ms")
    message("Baseline mean: ", round(baseline_mean, 4))
    
    # Apply baseline correction
    if (method == "subtract") {
      corrected_data <- pupil_data - baseline_mean
      message("Applied subtractive baseline correction")
    } else if (method == "divide") {
      corrected_data <- pupil_data / baseline_mean
      message("Applied divisive baseline correction")
    } else {
      warning("Unknown method '", method, "'. Using subtractive correction.")
      corrected_data <- pupil_data - baseline_mean
    }
    
    return(corrected_data)
  } else {
    warning("No baseline data found in specified window. Returning original data.")
    return(pupil_data)
  }
}
