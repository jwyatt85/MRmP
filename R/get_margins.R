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
#' \donttest{
#' get_margins(states = c('DC', 'FL'), vars = c('sex', 'age', 'education', 'race', 'obama12'))
#' }
get_margins <- function(states, vars){
  if('ALL' %in% states){
    states <- as.character(c(state.abb[1:8], 'DC', state.abb[9:50]))
  }
  output <- lapply(
    states, 
    function(i){
      df <- state_margins
      df <- dplyr::left_join(df, mrpExport::grouping_state_final, by='stname')
      
      ## This is not the best code, but it'll do for now
      
      df <- df %>% 
      dplyr::filter(
        stname == i
      )
      margins <- list()
      
      if('sex' %in% vars){
        sex_margins <- list(data.frame(sex = c('Male', 'Female'), Freq = c(df$Male, df$Female)))
        margins <- c(margins, sex_margins)
        names(margins) <- c(names(margins), "sex")
      }
      if('age' %in% vars){
        age_margins <- list(data.frame(age = c('18-29', '30-44', '45-64', '65+'), Freq = c(df$`18-29`, df$`30-44`, df$`45-64`, df$`65+`)))
        margins <- c(margins, age_margins)
        names(margins) <- c(names(margins)[names(margins) > 1], "age")
      }
      if('education' %in% vars){
        education_margins <- list(data.frame(education = c('LTC', 'Bachelors', 'Post-grad'), Freq = c(df$LTC, df$Bachelors, df$`Post-grad`)))
        margins <- c(margins, education_margins)
        names(margins) <- c(names(margins)[names(margins) > 1], "education")
      }
      if('race' %in% vars){
        race_margins <- list(data.frame(race = c('Black', 'Other', 'White'), Freq = c(df$Black, df$Other, df$White)))
        margins <- c(margins, race_margins)
        names(margins) <- c(names(margins)[names(margins) > 1], "race")
      }
      if('obama12' %in% vars){
        obama12_margins <- list(data.frame(obama12 = c('obama12'), Freq = c(df$obama12)))
        margins <- c(margins, obama12_margins)
        names(margins) <- c(names(margins)[names(margins) > 1], "obama12")
      }
      
      margins
    }
  )
  names(output) <- states
  output
}




