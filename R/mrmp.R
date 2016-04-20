#' MRmP - Multilevel Regression and Marginal Poststratification
#'
#' Creates subnational estimates based on the weighted frequence
#' of each subgroup based on their joint probabilities
#' 
#'
#'
#' @param survey_data A survey data frame 
#' @param jointp_list A data frame containg the weighted frequency of each strata
#' @param mrmp_formula A formulat to fit on survey_data
#' @param survey_sample Total amount to sample from surve_data
#' @export
#' @examples
#' x <- get_margins(states = c("ALL"), vars = c('sex', 'age', 'race', 'education')) 
#' test <- mrmp(
#' survey_data   = df,
#' jointp_list   = get_joint_probs(x),
#' mrmp_formula  = as.formula("y ~ age + stname  + sex + education + race"),
#' survey_sample = NULL') %>% 
#' bind_rows()
mrmp <- function(survey_data, jointp_list, mrmp_formula, survey_sample = NULL){
  
  mrmp_formula <- as.formula(mrmp_formula)
  response <- as.character(mrmp_formula[[2]])
  remaining_variables <- as.character(dplyr::setdiff(.myformulatocharacter(mrmp_formula), response))
  
  #do the recodes
  survey_data_final <- survey_data %>% 
    dplyr::mutate(
      race      = as.character(as.factor(car::recode(demRace4, "2='Black';3='White';1='Other';4='Other';else=NA"))),
      age       = as.character(as.factor(car::recode(age, "1='18-29'; 2='30-44'; 3='45-64'; 4='65+'"))),
      sex       = as.character(as.factor(ifelse(demGender == 1, "Male", "Female"))),
      education = as.character(as.factor(car::recode(educ4, "1='LTC'; 2='LTC'; 3='Bachelors'; 4='Post-grad'"))),
      stname    = c(state.abb[1:8], "DC", state.abb[9:50])[demState],
      party     = as.character(as.factor(car::recode(demPidNoLn, "1='Republican'; 2='Democrat'; 3='Independent'; 4='Something else'")))
    ) %>%
    dplyr::select_(response, 'race', 'age', 'education', 'stname', 'sex', 'party') %>% 
    na.omit

  survey_data_final <- dplyr::left_join(survey_data_final, mrpExport::grouping_state_final, by='stname')
  
  if(!assertthat::assert_that(is.character(mrmp_formula) | is.formula(mrmp_formula))){
    stop("formula provided needs to be in character format", call. = FALSE)
  }
  
  if (!all(is.element(remaining_variables, names(survey_data_final)))) {
    stop("Formula Variables not included in data - check names of your covariates", call. = FALSE)
  }
  
  if(!is.null(survey_sample)){
    survey_data <- survey_data_final %>% 
      dplyr::sample_n(survey_sample)
  }
  
  #reformlate the parameters of the formula to specified blme
  blme_formula <- as.formula(
    paste0(
      response, ' ~ ',
      paste0('(1|', remaining_variables , ')', collapse = ' + ')
    )
  )  
  
  survey_data_final[[response]] <- as.factor(survey_data_final[[response]])
  
  #run model
  MRmP <- suppressWarnings({blmer(blme_formula, data = survey_data_final, family = binomial(link="logit"))})
  
  state_mrmp <- lapply(
    1:length(jointp_list), 
    function(i){
      df <- dplyr::left_join(state_margins, mrpExport::grouping_state_final, by='stname') %>% 
        filter(stname == names(jointp_list[i]))
      
      jointp_list[[i]] <- jointp_list[[i]] %>% 
        dplyr::mutate(
          obama12 = df$obama12,
          stname = names(jointp_list[i])
        )
      
       predicted <- jointp_list[[i]] %>% 
        mutate(pred = predict(MRmP, newdata=jointp_list[[i]], type="response"))
       
       predicted <- predicted %>% 
         mutate(
           weighted_pred = predicted$pred * predicted$Freq
         )
       
       final_state_df <- data.frame(stname = names(jointp_list[i]), state_pred = sum(predicted$weighted_pred))
       final_state_df
    }
  )
}
  
  