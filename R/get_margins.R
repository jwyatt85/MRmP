#' MRmP - State Margins
#' 
#' Gets state level marginal proportions for varyings groups such as age, sex,
#' education, race
#' 
#' 
#' 
#' @author James Wyatt
#' @param states Character vector of state abbreviations. Alternatively or you
#'   can pass it \sQuote{ALL} to get margins for all states in DC.
#' @param vars List of variables to get marginal proportions for each state:
#'   sex, age, race, education.
#' @export
#' @examples
#' get_margins(states = c('DC', 'FL'), vars = c('sex', 'age', 'education', 'race', 'obama12'))
get_margins <- function(states, vars){
  if('ALL' %in% states){
    states <- as.character(c(state.abb[1:8], 'DC', state.abb[9:50]))
  }
  df <- state_margins
  output <- lapply(
    states, 
    function(i){
      df <- dplyr::left_join(df, mrpExport::grouping_state_final, by='stname')
      
      ## This is not the best code, but it'll do for now
      
      df <- df %>% 
      dplyr::filter(
        stname == i
      )
      
      if('sex' %in% vars){
        sex_margins <- data.frame(sex = c('Male', 'Female'), Freq = c(df$Male, df$Female))
        final_list <- list(sex = sex_margins)
      }
      if('age' %in% vars){
        age_margins <- data.frame(age = c('18-29', '30-44', '45-64', '65+'), Freq = c(df$`18-29`, df$`30-44`, df$`45-64`, df$`65+`))
        final_list <- list(sex = sex_margins, age = age_margins)
      }
      if('education' %in% vars){
        education_margins <- data.frame(education = c('LTC', 'Bachelors', 'Post-grad'), Freq = c(df$LTC, df$Bachelors, df$`Post-grad`))
        final_list <- list(sex = sex_margins, age = age_margins, education = education_margins)
      }
      if('race' %in% vars){
        race_margins <- data.frame(race = c('Black', 'Other', 'White'), Freq = c(df$Black, df$Other, df$White))
        final_list <- list(sex = sex_margins, age = age_margins, education = education_margins, race = race_margins)
      }
      if('obama12' %in% vars){
        obama12_margins <- data.frame(obama12 = c('obama12'), Freq = c(df$obama12))
        final_list <- list(sex = sex_margins, age = age_margins, education = education_margins, race = race_margins, obama12 = obama12_margins)
      }
      final_list
    }
  )
  names(output) <- states
  output
}




