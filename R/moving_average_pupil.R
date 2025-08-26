#' Apply Moving Average Smoothing to Pupil Data
#'
#' This function applies moving average smoothing to pupil diameter data
#' to reduce noise and artifacts while preserving the underlying signal.
#'
#' @param pupil_data Vector of pupil diameter values
#' @param window_size Window size for moving average (default: 5)
#' @param method Smoothing method: "mean", "median", or "gaussian" (default: "mean")
#' @param align Alignment of the window: "left", "center", or "right" (default: "center")
#'
#' @return Smoothed pupil data vector
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' smoothed_data <- moving_average_pupil(
#'   pupil_data = pupil_diameter,
#'   window_size = 5,
#'   method = "mean"
#' )
#' }
moving_average_pupil <- function(pupil_data, window_size = 5, 
                                 method = "mean", align = "center") {
  message("Applying moving average smoothing with window size: ", window_size)
  
  # Ensure window size is odd for center alignment
  if (align == "center" && window_size %% 2 == 0) {
    window_size <- window_size + 1
    message("Adjusted window size to ", window_size, " for center alignment")
  }
  
  # Calculate half window size
  half_window <- floor(window_size / 2)
  
  # Initialize smoothed data
  smoothed_data <- pupil_data
  
  # Apply smoothing
  for (i in seq_along(pupil_data)) {
    # Define window boundaries
    if (align == "left") {
      start_idx <- i
      end_idx <- min(i + window_size - 1, length(pupil_data))
    } else if (align == "center") {
      start_idx <- max(1, i - half_window)
      end_idx <- min(length(pupil_data), i + half_window)
    } else { # right alignment
      start_idx <- max(1, i - window_size + 1)
      end_idx <- i
    }
    
    # Get window data
    window_data <- pupil_data[start_idx:end_idx]
    window_data <- window_data[!is.na(window_data)]
    
    if (length(window_data) > 0) {
      if (method == "mean") {
        smoothed_data[i] <- mean(window_data)
      } else if (method == "median") {
        smoothed_data[i] <- median(window_data)
      } else if (method == "gaussian") {
        # Simple gaussian weighting
        weights <- dnorm(seq_along(window_data), mean = mean(seq_along(window_data)), 
                        sd = length(window_data) / 3)
        weights <- weights / sum(weights)
        smoothed_data[i] <- sum(window_data * weights)
      } else {
        warning("Unknown method '", method, "'. Using mean.")
        smoothed_data[i] <- mean(window_data)
      }
    }
  }
  
  message("Smoothing complete using ", method, " method")
  return(smoothed_data)
}
