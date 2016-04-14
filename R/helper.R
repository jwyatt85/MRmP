

myformulatocharacter <- function(formula) {
  string <- strsplit(Reduce(paste, deparse(formula)), split = '~')[[1]] %>% 
    paste0(collapse = '+') %>% 
    gsub('^\\s|\\s$', "", .) %>% 
    strsplit(., split = '\\s+\\+\\s+')
  string <- string[[1]] %>% 
    gsub('\\.', "", .)
  string[string != ""]
}


