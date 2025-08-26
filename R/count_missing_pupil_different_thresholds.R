#' Count Missing Pupil Data with Different Thresholds
#'
#' This function analyzes missing pupil data at different levels (participant, trial) 
#' and applies exclusion criteria based on specified thresholds.
#'
#' @param datafile A data frame containing the pupil data
#' @param pupil Character string specifying the column name for pupil data (default: "pupil")
#' @param participant_thresh Numeric threshold for participant-level exclusion (default: 0.2)
#' @param trial_thresh Numeric threshold for trial-level exclusion (default: 0.2)
#' @param participant_trial_threshold Numeric threshold for participant trial rejection rate (default: 0.2)
#' @param ask_permission_to_exclude Logical, whether to ask for confirmation before excluding participants (default: TRUE)
#'
#' @return A filtered data frame with excluded participants and trials removed
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' filtered_data <- count_missing_pupil_different_thresholds(
#'   datafile = pupil_data,
#'   pupil = "pupil_diameter",
#'   participant_thresh = 0.15,
#'   trial_thresh = 0.25
#' )
#' }
count_missing_pupil_different_thresholds <- function(datafile, pupil = "pupil", 
                                                     participant_thresh = 0.2,
                                                     trial_thresh = 0.2,
                                                     participant_trial_threshold = 0.2,
                                                     ask_permission_to_exclude = TRUE){
  
  # Step 1: Calculate missing data, this result is the same as the gazeR function
  countsbysubject <- datafile %>% 
    dplyr::group_by(subject) %>% 
    dplyr::summarise(missing = sum(is.na(!!sym(pupil))), 
                     samples = sum(!is.na(!!sym(pupil))), 
                     total = length(pupil)) %>%  
    dplyr::mutate(averageMissingSub = missing/total)
  
  countsbytrial <- datafile %>% 
    dplyr::group_by(subject, trial) %>% 
    dplyr::summarise(missing = sum(is.na(!!sym(pupil))), 
                     samples = sum(!is.na(!!sym(pupil))), 
                     total = length(pupil)) %>%  
    dplyr::mutate(averageMissingTrial = missing/total)
  
  # Step 2: Apply filtering logic
  greaterthan_trial <- dplyr::filter(countsbytrial, averageMissingTrial > trial_thresh)
  greaterthan_subject <- dplyr::filter(countsbysubject, averageMissingSub > participant_thresh)
  
  # Calculate proportions 
  prop <- length(greaterthan_trial$trial)/length(countsbytrial$trial)
  message("% trials excluded:", round(prop, digits = 3))
  
  # Show detailed information about excluded trials per trial
  if(nrow(greaterthan_trial) > 0) {
    message("\n=== EXCLUDED TRIALS DETAILS ===")
    message("Trials excluded due to high missing data:")
    for(i in 1:nrow(greaterthan_trial)) {
      subj <- greaterthan_trial$subject[i]
      trial_num <- greaterthan_trial$trial[i]
      missing_pct <- round(greaterthan_trial$averageMissingTrial[i] * 100, 1)
      message(sprintf("  Participant %s, Trial %s: %.1f%% missing data", subj, trial_num, missing_pct))
    }
  } else {
    message("No trials excluded due to missing data")
  }
  
  message("Participants taken out:", list(countsbysubject$subject[countsbysubject$averageMissingSub > participant_thresh]))
  
  # Show detailed information about excluded participants
  if(nrow(greaterthan_subject) > 0) {
    message("\n=== EXCLUDED PARTICIPANTS DETAILS ===")
    message("Participants excluded due to high missing data:")
    for(i in 1:nrow(greaterthan_subject)) {
      subj <- greaterthan_subject$subject[i]
      missing_pct <- round(greaterthan_subject$averageMissingSub[i] * 100, 1)
      message(sprintf("  Participant %s: %.1f%% missing data", subj, missing_pct))
    }
  } else {
    message("No participants excluded due to missing data")
  }
  
  # Step 3: Join data 
  combineSub <- dplyr::full_join(datafile, 
                                 countsbysubject[, c("subject", "averageMissingSub")], 
                                 by = "subject")
  
  combinetrial <- dplyr::full_join(combineSub, 
                                   countsbytrial[, c("subject", "trial", "averageMissingTrial")], 
                                   by = c("subject", "trial"))
  
  # Step 4: Apply filtering
  combinetrial_above_threshold <- dplyr::filter(combinetrial, 
                                                (averageMissingSub < participant_thresh) & 
                                                  (averageMissingTrial < trial_thresh))
  
  # Step 5: Third exclusion check 
  if(ask_permission_to_exclude) {
    # Calculate trial rejection summary for remaining participants
    trial_rejection_summary <- combinetrial_above_threshold %>%
      dplyr::group_by(subject) %>%
      dplyr::summarise(
        total_trials = n_distinct(trial),
        .groups = 'drop'
      ) %>%
      dplyr::left_join(
        countsbytrial %>% 
          dplyr::filter(averageMissingTrial > trial_thresh) %>%
          dplyr::group_by(subject) %>%
          dplyr::summarise(
            rejected_trials = n(),
            .groups = 'drop'
          ),
        by = "subject"
      ) %>%
      dplyr::mutate(
        rejected_trials = ifelse(is.na(rejected_trials), 0, rejected_trials),
        proportion_rejected = rejected_trials / total_trials
      ) %>%
      dplyr::filter(proportion_rejected > participant_trial_threshold)
    
    # Ask for researcher confirmation for each participant
    if(nrow(trial_rejection_summary) > 0) {
      message("\n=== RESEARCHER CONFIRMATION REQUIRED ∩`･◇･) ===")
      message("The following participants have high trial rejection rates:")
      
      for(i in 1:nrow(trial_rejection_summary)) {
        subj <- trial_rejection_summary$subject[i]
        total <- trial_rejection_summary$total_trials[i]
        rejected <- trial_rejection_summary$rejected_trials[i]
        prop <- trial_rejection_summary$proportion_rejected[i]
        
        message(sprintf("Participant %s has %d trials excluded out of %d total (%.1f%%). This exceeds the trial-exclusion threshold of %.1f%%.", 
                        subj, rejected, total, prop*100, participant_trial_threshold*100))
        
        researcher_input <- readline(prompt = "Do you want to exclude this participant? [y/n]: ")
        
        if(tolower(researcher_input) %in% c("y", "yes")) {
          # Add to greaterthan_subject for exclusion
          if(!(subj %in% greaterthan_subject$subject)) {
            greaterthan_subject <- rbind(greaterthan_subject, 
                                         countsbysubject[countsbysubject$subject == subj, ])
          }
        }
      }
      
      # Re-filter data based on updated exclusions
      combinetrial_above_threshold <- dplyr::filter(combinetrial, 
                                                    (averageMissingSub < participant_thresh) & 
                                                      (averageMissingTrial < trial_thresh) &
                                                      !(subject %in% greaterthan_subject$subject))
    }
  }
  
  # Final reporting
  message("\n=== FINAL EXCLUSION SUMMARY ===")
  message("Original participants: ", length(unique(datafile$subject)))
  message("Final participants: ", length(unique(combinetrial_above_threshold$subject)))
  message("Original trials: ", length(unique(paste(datafile$subject, datafile$trial))))
  message("Final trials: ", length(unique(paste(combinetrial_above_threshold$subject, combinetrial_above_threshold$trial))))
  
  return(combinetrial_above_threshold)
}
