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
#' x <- get_joint_margins(states = c('DC', 'FL'), 
#'  vars = c('sex', 'age', 'race', 'education'))
#' y <- get_joint_probs(x)
#' my_formula <- as.formula("y ~ age + sex + education + race + Obama12 + stname")
#' state_estimates <- mrmp(survey_data, y, my_formula)
mrmp <- function(survey_data, jointp_list, mrmp_formula, survey_sample = NULL){
  
  mrmp_formula <- as.formula(mrmp_formula)
  
  if (!all(stname, names(survey_data)))) {
    stop("You need stname - the merging variable with state-data", call. = FALSE)
  }

  survey_data <- dplyr::left_join(survey_data, mrpExport::grouping_state_final, by='stname')
  
  if(!assertthat::assert_that(is.character(mrmp_formula) | is.formula(mrmp_formula))){
    stop("formula provided needs to be in character format", call. = FALSE)
  }
  
  response <- as.character(mrmp_formula[[2]])
  remaining_variables <- dplyr::setdiff(myformulatocharacter(mrmp_formula), response)
  
  if (!all(is.element(remaining_variables, names(survey_data)))) {
    stop("Formula Variables not included in data - check names of your covariates", call. = FALSE)
  }
  
  if(!is.null(survey_sample)){
    survey_data <- survey_data %>% 
      dplyr::sample_n(survey_sample)
  }
  
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
      
      
      MRmP <- suppressWarnings({blmer(as.formula(mrmp_formula), data = survey_data, family = binomial(link="logit"))})
      
     
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
  
  