#' Extend Blink Periods in Pupil Data
#'
#' This function extends blink periods by adding padding before and after detected blinks
#' to ensure complete coverage of blink artifacts.
#'
#' @param pupil Vector of pupil diameter values
#' @param fillback Number of milliseconds to extend before blink onset (default: 200)
#' @param fillforward Number of milliseconds to extend after blink offset (default: 100)
#' @param hz Sampling rate in Hz (default: 1000)
#'
#' @return Vector with extended blink periods
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' extended_blinks <- extend_blinks(
#'   pupil = pupil_data,
#'   fillback = 200,
#'   fillforward = 100,
#'   hz = 1000
#' )
#' }
extend_blinks <- function(pupil, fillback = 200, fillforward = 100, hz = 1000) {
  message("Extending blink periods...")
  
  # Convert milliseconds to samples
  pre_padding <- round(fillback * hz / 1000)
  post_padding <- round(fillforward * hz / 1000)
  
  # Simple blink detection based on rapid changes
  pupil_diff <- diff(pupil)
  blinks <- abs(pupil_diff) > 0.5
  
  # Expand to match original data length
  blinks <- c(FALSE, blinks)
  
  # Find blink onset and offset
  blink_changes <- diff(c(FALSE, blinks, FALSE))
  blink_starts <- which(blink_changes == 1)
  blink_ends <- which(blink_changes == -1) - 1
  
  # Initialize extended blinks
  extended_blinks <- blinks
  
  # Extend each blink period
  for (i in seq_along(blink_starts)) {
    start_idx <- max(1, blink_starts[i] - pre_padding)
    end_idx <- min(length(blinks), blink_ends[i] + post_padding)
    
    extended_blinks[start_idx:end_idx] <- TRUE
  }
  
  message("Extended ", length(blink_starts), " blink periods")
  return(extended_blinks)
}
