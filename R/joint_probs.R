#' MRmP - Joint Probabilities
#'
#' Creats joint probabilities for a combination of varabiles - derived from their marginal distributions 
#' 
#'
#'
#' @param states A list comprised of individual dataframes of 
#'  state abbreviations using get_joint_margins()
#' @export
#' @examples
#' x <- get_margins(states = c('DC', 'FL'), 
#'  vars = c('sex', 'age', 'race', 'education'))
#' get_joint_probs(x)
get_joint_probs <- function(data){
  x <- data
  #### Create marginal grids by state ------------------------------------------------------------------------------------
  final_grids <- lapply(1:length(x), function(i){
    state_strats <- lapply(
      1:length(x[[i]]), function(j){
        levels(x[[i]][[j]][,1])
      }
    )
    names(state_strats) <- names(x[[i]])
    expand.grid(state_strats)
  })
  
  
  names(final_grids) <- names(x)
  
  #### Mutate marginal grids by state ------------------------------------------------------------------------------------
  for(i in 1:length(final_grids)){
    final_grids[[i]] <- final_grids[[i]] %>% 
      dplyr::mutate(
        id = seq(1, nrow(.)),
        wts = rep(1, nrow(.)),
        stname = names(final_grids)[i]
      ) 
  }
  
  #### Make Survey Designs ------------------------------------------------------------------------------------
  survey_designs <- lapply(
    1:length(final_grids), function(i){
      df <- final_grids[[i]]
      census_dsg <- survey::svydesign(id = ~id, weights = ~wts, data = df)
    }
  )
  
  #### Ceate Joint Probabilities ------------------------------------------------------------------------------------
  
final_state_tables <- lapply(
    1:length(x), function(i){
      census_dsg <- survey_designs[[i]]
      print(paste0("state: ", i))
      iter <- 1
      epsilon <- 1
      sample_margins <- vector('list', length(names(x[[i]])))
      for(z in 1:length(sample_margins)) { 
        sample_margins[[z]] <- as.formula(paste0("~",names(x[[i]])[z]))
      }
      nmar <- length(sample_margins)
      population_margins <- x[[i]]
      design <- census_dsg
      
      ff <- formula(
        paste(
          "~", 
          paste(
            unlist(lapply(sample_margins, all.vars)),
            collapse = "+"), 
          sep = ""
        )
      )
      
      strata <- lapply(
        sample_margins,
        function(margin) {
          if (inherits(margin, "formula")) {
            mf <- model.frame(margin, data = design$variables, na.action = na.fail)
          }
        }
      )
      oldtable <- survey::svytable(ff, design)
      
      while (iter < 100) {
        design$postStrata <- NULL
        for (i in 1:nmar) {
          design <- survey::postStratify(
            design,
            strata[[i]],
            population_margins[[i]], 
            compress = FALSE
          )
        }
        newtable <- survey::svytable(ff, design)
        delta <- max(abs(oldtable - newtable))
        if (delta < epsilon) {
          converged <- TRUE
          break
        }
        
        cat('Running iteration: ', iter, '\n')
        oldtable <- newtable
        iter <- iter + 1
      }
      
      newtable <- as.data.frame(newtable)
      newtable <- newtable %>% 
        mutate(
          id = 1:nrow(newtable)
        )

    })
  
names(final_state_tables) <- names(x)
final_state_tables
}
