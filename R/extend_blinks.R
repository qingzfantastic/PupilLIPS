#' Extend Blink Periods in Pupil Data
#'
#' This function extends blink periods by adding padding before and after detected blinks
#' to ensure complete coverage of blink artifacts.
#'
#' @param blinks Logical vector indicating blink periods (TRUE = blink, FALSE = no blink)
#' @param pre_padding Number of samples to extend before blink onset (default: 50)
#' @param post_padding Number of samples to extend after blink offset (default: 50)
#'
#' @return Logical vector with extended blink periods
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' extended_blinks <- extend_blinks(
#'   blinks = blink_vector,
#'   pre_padding = 50,
#'   post_padding = 50
#' )
#' }
extend_blinks <- function(blinks, pre_padding = 50, post_padding = 50) {
  message("Extending blink periods...")
  
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
