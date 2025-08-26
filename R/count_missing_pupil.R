#' Count Missing Pupil Data (Original gazeR Function)
#'
#' This is the original count_missing_pupil function from the gazeR package.
#' It provides basic missing data analysis for pupil diameter data.
#'
#' @param datafile A data frame containing the pupil data
#' @param pupil Character string specifying the column name for pupil data (default: "pupil")
#' @param participant_thresh Numeric threshold for participant-level exclusion (default: 0.2)
#' @param trial_thresh Numeric threshold for trial-level exclusion (default: 0.2)
#'
#' @return A filtered data frame with excluded participants and trials removed
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' filtered_data <- count_missing_pupil(
#'   datafile = pupil_data,
#'   pupil = "pupil_diameter",
#'   participant_thresh = 0.2,
#'   trial_thresh = 0.2
#' )
#' }
count_missing_pupil <- function(datafile, pupil = "pupil", 
                                participant_thresh = 0.2,
                                trial_thresh = 0.2) {
  
  # Calculate missing data by subject
  countsbysubject <- datafile %>% 
    dplyr::group_by(subject) %>% 
    dplyr::summarise(missing = sum(is.na(!!sym(pupil))), 
                     samples = sum(!is.na(!!sym(pupil))), 
                     total = length(pupil)) %>%  
    dplyr::mutate(averageMissingSub = missing/total)
  
  # Calculate missing data by trial
  countsbytrial <- datafile %>% 
    dplyr::group_by(subject, trial) %>% 
    dplyr::summarise(missing = sum(is.na(!!sym(pupil))), 
                     samples = sum(!is.na(!!sym(pupil))), 
                     total = length(pupil)) %>%  
    dplyr::mutate(averageMissingTrial = missing/total)
  
  # Apply thresholds
  greaterthan_trial <- dplyr::filter(countsbytrial, averageMissingTrial > trial_thresh)
  greaterthan_subject <- dplyr::filter(countsbysubject, averageMissingSub > participant_thresh)
  
  # Join data
  combineSub <- dplyr::full_join(datafile, 
                                 countsbysubject[, c("subject", "averageMissingSub")], 
                                 by = "subject")
  
  combinetrial <- dplyr::full_join(combineSub, 
                                   countsbytrial[, c("subject", "trial", "averageMissingTrial")], 
                                   by = c("subject", "trial"))
  
  # Filter data based on thresholds
  combinetrial_above_threshold <- dplyr::filter(combinetrial, 
                                                (averageMissingSub < participant_thresh) & 
                                                  (averageMissingTrial < trial_thresh))
  
  return(combinetrial_above_threshold)
}
