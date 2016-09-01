#' MRmP - Multilevel Regression and Marginal Poststratification
#'
#' Creates subnational estimates based on the weighted frequence
#' of each subgroup based on their joint probabilities
#' 
#'
#'
#' @param survey_data A survey data frame 
#' @param jointp_list A data frame containg the weighted frequency of each strata
#' @param individualvars variables used in mixed effects formula that will have random intercepts
#' @param groupingvars variables used in mixed effects formula that serve as state-level grouping vars and do not have random intercepts
#' @param response The response/outcome variable on which to regress
#' @param survey_sample Total amount to sample from surve_data
#' @param weights Weights to be used in the bglmer
#' @export

mrmp <- function(survey_data, jointp_list, individualvars, groupingvars, response, survey_sample = NULL, weights = NULL){
  
  response <- as.character(response)
  individualvars <- as.character(individualvars)
  groupingvars <- as.character(groupingvars)
  
  individualvars <- as.character(dplyr::setdiff(.myformulatocharacter2(individualvars), response))
  groupingvars <- as.character(dplyr::setdiff(.myformulatocharacter2(groupingvars), response))
  
  #do the recodes
  survey_data_final <- survey_data %>% 
    dplyr::mutate(
      race      = as.character(as.factor(car::recode(demRace4, "2='Black';3='White';1='Other';4='Other';else=NA"))),
      age       = as.character(as.factor(car::recode(age, "1='18-29'; 2='30-44'; 3='45-64'; 4='65+'"))),
      sex       = as.character(as.factor(ifelse(demGender == 1, "Male", "Female"))),
      education = as.character(as.factor(car::recode(educ4, "1='LTC'; 2='LTC'; 3='Bachelors'; 4='Post-grad'"))),
      stname    = c(state.abb[1:8], "DC", state.abb[9:50])[demState],
      party     = as.character(as.factor(car::recode(demPidNoLn, "1='Republican'; 2='Democrat'; 3='Independent'; 4='Something else'"))),
      religion  = as.character(as.factor(car::recode(xreligion3, "1='Christian'; 2='Non_Christian'")))
    ) %>%
    dplyr::select_(response, 'race', 'age', 'education', 'stname', 'sex', 'party', 'religion') %>% 
    na.omit
  
  survey_data_final <- dplyr::left_join(survey_data_final, mrpExport::grouping_state_final, by='stname')
  
  if (!all(c(individualvars %in% names(survey_data_final)))) {
    stop("Individualvars not included in data - check names of your covariates", call. = FALSE)
  }
  if (!all(c(groupingvars %in% names(survey_data_final)))) {
    stop("Groupingvars not included in data - check names of your covariates", call. = FALSE)
  }
  
  if(!is.null(survey_sample)){
    survey_data_final <- survey_data_final %>% 
      dplyr::sample_n(survey_sample)
  }
  
  #reformlate the parameters of the formula to specified blme
  blme_formula <- as.formula(
    paste0(
      paste0(
        response, ' ~ ',
        paste0('(1|', individualvars, ')', collapse = ' + ')),'+',paste0("", groupingvars, "", collapse = ' + ')))
  
  survey_data_final[[response]] <- as.factor(survey_data_final[[response]])
  
  #run model
  if(!is.null(weights)){
    MRmP <- suppressWarnings({blmer(blme_formula, data = survey_data_final, weights = data$wts, family = binomial(link="logit"))})
  } else{
    MRmP <- suppressWarnings({blmer(blme_formula, data = survey_data_final, family = binomial(link="logit"))})
  }
  
  state_mrmp <- lapply(
    1:length(jointp_list), 
    function(i){
      df <- dplyr::left_join(state_margins, mrpExport::grouping_state_final, by='stname') %>% 
        filter(stname == names(jointp_list[i]))
      
      jointp_list[[i]] <- jointp_list[[i]] %>% 
        dplyr::mutate(
          obama12 = df$obama12,
          stname = names(jointp_list[i]),
          medianhhincome = df$medianhhincome,
          percent_gdp_increase = df$percent_gdp_increase
        )
      
      predicted <- jointp_list[[i]] %>% 
        dplyr::mutate(pred = predict(MRmP, newdata=jointp_list[[i]], type="response"))
      
      predicted <- predicted %>% 
        dplyr::mutate(
          weighted_pred = predicted$pred * predicted$Freq
        )
      
      final_state_df <- data.frame(stname = names(jointp_list[i]), state_pred = sum(predicted$weighted_pred))
      final_state_df
    }
  )
}
