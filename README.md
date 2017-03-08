# Multilevel Regression and Marginal Poststratification

This `README.md` file gives a quick overview of the code used to generate state level MRmP estimates (For further information on MRmP please refer to Leemann and Wasserfallen's 2014 paper: Extending the Use and Prediction Precision of Subnational Public Opinion Estimation).

## Unique Estimates
This code can be used to generate unique MRmP estimates: 
``` r 
x <- get_margins(states = c("ALL"), vars = c('sex', 'age', 'race', 'education', 'religion', 'party')) 
joints <- get_joint_probs(x)

individualvars <- c("age + stname  + sex + education + race + party + religion")
groupingvars <- c("obama12 + medianhhincome + percent_gdp_increase")

test <- mrmp(
  survey_data     = df,
  jointp_list     = joints,
  individualvars  = individualvars,
  groupingvars    = groupingvars,
  response        = 'y',
  survey_sample   = 10000
) %>% 
  bind_rows()
```

  - `survey_data`, a (nxn) data frame / survey data, where each row is a survey respondent and columns serve as covariates
  - `individualvars`, variables used as individual level covariates - random intercepts
  - `groupingvars`, variables that serve as grouping level variables: state level covariates for example that do not vary in their slope or intercept 
  - `jointp_list`, a single data frame or a list of data frames containing the synthetic joint distributuions by state from their respective state marginal distrbutions of each demograhpic variable, which can be calculated using get_joint_probs(X)
  - `response`, the response variable in the data set (currently needs to be binary, i.e. 0:1)
  - `survey_sample`, Take a random sample of the dataframe (without replacement) 


