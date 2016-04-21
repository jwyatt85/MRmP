

.myformulatocharacter <- function(formula) {
  string <- strsplit(Reduce(paste, deparse(formula)), split = '')[[1]] %>% 
    paste0(collapse = '+') %>% 
    gsub('^\\s|\\s$', "", .) %>% 
    strsplit(., split = '\\s+\\+\\s+')
  string <- string[[1]] %>% 
    gsub('\\.', "", .)
  string[nzchar(string)]
}

#post-stravis function
.myformulatocharacter2 <- function(formula) {
  string <- unlist(strsplit(formula, "+", fixed=TRUE)) %>% 
    gsub('\\s', "", .) %>% 
    strsplit(., split = '\\s+\\+\\s+')
  string <- string[nzchar(string)]
}

