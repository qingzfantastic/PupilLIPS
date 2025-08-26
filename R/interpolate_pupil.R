#' Interpolate Missing Pupil Data
#'
#' This function interpolates missing or blink-affected pupil data using
#' various interpolation methods to fill gaps in the data.
#'
#' @param pupil_data Vector of pupil diameter values
#' @param blinks Logical vector indicating blink periods (TRUE = blink, FALSE = no blink)
#' @param method Interpolation method: "linear", "cubic", or "spline" (default: "linear")
#' @param max_gap Maximum gap size to interpolate in samples (default: 100)
#'
#' @return Vector with interpolated values where blinks were detected
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' interpolated_data <- interpolate_pupil(
#'   pupil_data = pupil_diameter,
#'   blinks = blink_vector,
#'   method = "linear"
#' )
#' }
interpolate_pupil <- function(pupil_data, blinks, method = "linear", max_gap = 100) {
  message("Interpolating pupil data using ", method, " method...")
  
  # Create time index
  time_index <- seq_along(pupil_data)
  
  # Interpolated data starts as original data
  interpolated_data <- pupil_data
  
  if (sum(blinks) > 0) {
    # Find gaps in the data
    gap_starts <- which(diff(c(FALSE, blinks, FALSE)) == 1)
    gap_ends <- which(diff(c(FALSE, blinks, FALSE)) == -1) - 1
    
    # Process each gap
    for (i in seq_along(gap_starts)) {
      gap_start <- gap_starts[i]
      gap_end <- gap_ends[i]
      gap_size <- gap_end - gap_start + 1
      
      # Only interpolate if gap is not too large
      if (gap_size <= max_gap) {
        # Get valid data before and after the gap
        pre_gap_indices <- time_index[1:gap_start-1]
        post_gap_indices <- time_index[(gap_end+1):length(time_index)]
        
        pre_gap_values <- pupil_data[1:gap_start-1]
        post_gap_values <- pupil_data[(gap_end+1):length(pupil_data)]
        
        # Combine pre and post gap data
        valid_indices <- c(pre_gap_indices, post_gap_indices)
        valid_values <- c(pre_gap_values, post_gap_values)
        
        # Remove NA values
        valid_indices <- valid_indices[!is.na(valid_values)]
        valid_values <- valid_values[!is.na(valid_values)]
        
        if (length(valid_values) >= 2) {
          # Interpolate the gap
          gap_indices <- time_index[gap_start:gap_end]
          
          if (method == "linear") {
            interpolated_values <- approx(valid_indices, valid_values, 
                                         xout = gap_indices, 
                                         method = "linear")$y
          } else if (method == "cubic") {
            interpolated_values <- spline(valid_indices, valid_values, 
                                         xout = gap_indices)$y
          } else if (method == "spline") {
            interpolated_values <- spline(valid_indices, valid_values, 
                                         xout = gap_indices, method = "natural")$y
          } else {
            warning("Unknown method '", method, "'. Using linear interpolation.")
            interpolated_values <- approx(valid_indices, valid_values, 
                                         xout = gap_indices, 
                                         method = "linear")$y
          }
          
          # Fill the gap
          interpolated_data[gap_start:gap_end] <- interpolated_values
        }
      } else {
        message("Gap ", i, " too large (", gap_size, " samples). Skipping interpolation.")
      }
    }
  }
  
  message("Interpolation complete. Processed ", sum(blinks), " blink samples.")
  return(interpolated_data)
}
