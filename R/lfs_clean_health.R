#' Clean health data
#'
#' Cleans the raw data for health and health related variables.
#'
#' @param data data table. Raw LFS data produced using the reading functions.
#'
#' @return Returns a new set of variables
#' @export
lfs_clean_health <- function(
    data
) {

  ################################################
  # Disability (under the Equality Act 2010)
  if ("disea" %in% colnames(data)) {
  data[disea == 1 , disability := "disabled"]
  data[disea == 2 , disability := "not_disabled"]
  } else {

  data[, disability := NA]
  }

  #############################################################################################
  # Main health problem/disability (that limits the kind of paid work the respondent can do)

  if ("health" %in% colnames(data)){
  data[ , main_health_condition := "none"]
  data[health == 1 , main_health_condition := "msk_arms_hands"]
  data[health == 2 , main_health_condition := "msk_legs_feet"]
  data[health == 3 , main_health_condition := "msk_back_neck"]
  data[health == 4 , main_health_condition := "difficulty_seeing"]
  data[health == 5 , main_health_condition := "difficulty_hearing"]
  data[health == 6 , main_health_condition := "speech_impediment"]
  data[health == 7 , main_health_condition := "skin_conditions_disfigurement"]
  data[health == 8 , main_health_condition := "respiratory_problems"]
  data[health == 9 , main_health_condition := "heart_blood_circulatory_problems"]
  data[health == 10 , main_health_condition := "digestive_problems"]
  data[health == 11 , main_health_condition := "diabetes"]
  data[health == 12 , main_health_condition := "depression_anxiety"]
  data[health == 13 , main_health_condition := "epilepsy"]
  data[health == 18 , main_health_condition := "autism"]
  data[health == 14 , main_health_condition := "learning_difficulties"]
  data[health == 15 , main_health_condition := "mental_illness"]
  data[health == 16 , main_health_condition := "progressive_illness_nec"]
  data[health == 17 , main_health_condition := "other_problems"]


  ## only applies to those who are working age so set NA if not otherwise
  ## the "none" category will be incorrect
  data[(sex == 1 & !(age %in% 16:64)) | (sex == 2 & !(age %in% 16:59)), main_health_condition := NA]
  } else {

  data[, main_health_condition := NA]
  }

  if ("health" %in% colnames(data)){
    data[ , main_health_condition_8cat := "none"]
    data[health %in% 1:3 , main_health_condition_8cat := "musculoskeletal"]
    data[health %in% 4:5 , main_health_condition_8cat := "sight_hearing"]
    data[health %in% 8 , main_health_condition_8cat := "respiratory"]
    data[health %in% 9:11 , main_health_condition_8cat := "blood_digestive_diabetes"]
    data[health %in% c(12,15) , main_health_condition_8cat := "mental_health"]
    data[health %in% c(14,18) , main_health_condition_8cat := "lda"]
    data[health %in% c(16,17,13,6,7) , main_health_condition_8cat := "other_health_cond"]

    ## only applies to those who are working age so set NA if not otherwise
    ## the "none" category will be incorrect
    data[(sex == 1 & !(age %in% 16:64)) | (sex == 2 & !(age %in% 16:59)), main_health_condition_8cat := NA]
  } else {

    data[, main_health_condition_8cat := NA]
  }

  ############################################################################
  # Dummies for each health condition (in the 8 category specification above)

  if ("health" %in% colnames(data)){
    data[, musculoskeletal := ifelse(heal01 %in% 1:3 |
                                     heal02 %in% 1:3 |
                                     heal03 %in% 1:3 |
                                     heal04 %in% 1:3 |
                                     heal05 %in% 1:3 |
                                     heal06 %in% 1:3 |
                                     heal07 %in% 1:3 |
                                     heal08 %in% 1:3 |
                                     heal09 %in% 1:3 |
                                     heal10 %in% 1:3, 1, 0)]

    data[, sight_hearing := ifelse(heal01 %in% 4:5 |
                                   heal02 %in% 4:5 |
                                   heal03 %in% 4:5 |
                                   heal04 %in% 4:5 |
                                   heal05 %in% 4:5 |
                                   heal06 %in% 4:5 |
                                   heal07 %in% 4:5 |
                                   heal08 %in% 4:5 |
                                   heal09 %in% 4:5 |
                                   heal10 %in% 4:5, 1, 0)]

    data[, respiratory := ifelse(heal01 %in% 8 |
                                 heal02 %in% 8 |
                                 heal03 %in% 8 |
                                 heal04 %in% 8 |
                                 heal05 %in% 8 |
                                 heal06 %in% 8 |
                                 heal07 %in% 8 |
                                 heal08 %in% 8 |
                                 heal09 %in% 8 |
                                 heal10 %in% 8, 1, 0)]

    data[, blood_digestive_diabetes := ifelse(heal01 %in% 9:11 |
                                              heal02 %in% 9:11 |
                                              heal03 %in% 9:11 |
                                              heal04 %in% 9:11 |
                                              heal05 %in% 9:11 |
                                              heal06 %in% 9:11 |
                                              heal07 %in% 9:11 |
                                              heal08 %in% 9:11 |
                                              heal09 %in% 9:11 |
                                              heal10 %in% 9:11, 1, 0)]

    data[, mental_health := ifelse(heal01 %in% c(12,15) |
                                   heal02 %in% c(12,15) |
                                   heal03 %in% c(12,15) |
                                   heal04 %in% c(12,15) |
                                   heal05 %in% c(12,15) |
                                   heal06 %in% c(12,15) |
                                   heal07 %in% c(12,15) |
                                   heal08 %in% c(12,15) |
                                   heal09 %in% c(12,15) |
                                   heal10 %in% c(12,15), 1, 0)]

    data[, lda := ifelse(heal01 %in% c(14,18) |
                         heal02 %in% c(14,18) |
                         heal03 %in% c(14,18) |
                         heal04 %in% c(14,18) |
                         heal05 %in% c(14,18) |
                         heal06 %in% c(14,18) |
                         heal07 %in% c(14,18) |
                         heal08 %in% c(14,18) |
                         heal09 %in% c(14,18) |
                         heal10 %in% c(14,18), 1, 0)]


    data[, other_health_cond := ifelse(heal01 %in% c(16,17,13,6,7) |
                                       heal02 %in% c(16,17,13,6,7) |
                                       heal03 %in% c(16,17,13,6,7) |
                                       heal04 %in% c(16,17,13,6,7) |
                                       heal05 %in% c(16,17,13,6,7) |
                                       heal06 %in% c(16,17,13,6,7) |
                                       heal07 %in% c(16,17,13,6,7) |
                                       heal08 %in% c(16,17,13,6,7) |
                                       heal09 %in% c(16,17,13,6,7) |
                                       heal10 %in% c(16,17,13,6,7), 1, 0)]

    data[(sex == 1 & !(age %in% 16:64)) | (sex == 2 & !(age %in% 16:59)), musculoskeletal := NA]
    data[(sex == 1 & !(age %in% 16:64)) | (sex == 2 & !(age %in% 16:59)), sight_hearing := NA]
    data[(sex == 1 & !(age %in% 16:64)) | (sex == 2 & !(age %in% 16:59)), respiratory := NA]
    data[(sex == 1 & !(age %in% 16:64)) | (sex == 2 & !(age %in% 16:59)), blood_digestive_diabetes := NA]
    data[(sex == 1 & !(age %in% 16:64)) | (sex == 2 & !(age %in% 16:59)), mental_health := NA]
    data[(sex == 1 & !(age %in% 16:64)) | (sex == 2 & !(age %in% 16:59)), lda := NA]
    data[(sex == 1 & !(age %in% 16:64)) | (sex == 2 & !(age %in% 16:59)), other_health_cond := NA]

  } else {

    data[, musculoskeletal := NA]
    data[, sight_hearing := NA]
    data[, respiratory := NA]
    data[, blood_digestive_diabetes := NA]
    data[, mental_health := NA]
    data[, lda := NA]
    data[, other_health_cond := NA]

  }

  ####################################
  # Main health problem/disability

  ### convert the variables to factors
  data$disability                 <- as.factor(data$disability)
  data$main_health_condition      <- as.factor(data$main_health_condition)
  data$main_health_condition_8cat <- as.factor(data$main_health_condition_8cat)

  heal_vars <- grep("^heal", names(data), value = TRUE)

  ###############################
  ### RETAIN CLEANED VARIABLES

  final_data <- data[, c("obs_id",
                         "disability", "main_health_condition", "main_health_condition_8cat",
                         "musculoskeletal","sight_hearing","respiratory","blood_digestive_diabetes",
                         "mental_health","lda","other_health_cond",
                         heal_vars)]

  var_names <- c("disability", "main_health_condition", "main_health_condition_8cat",
                 "musculoskeletal","sight_hearing","respiratory","blood_digestive_diabetes",
                 "mental_health","lda","other_health_cond")

  setnames(final_data, var_names, paste0("h_", var_names))

  return(final_data)

}
