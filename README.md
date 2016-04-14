# Multilevel Regression and Marginal Poststratification

This `README.md` file gives a quick overview of the code used to generate state level MRmP estimates (For further information on MRmP please refer to Leemann and Wasserfallen's 2014 paper: Extending the Use and Prediction Precision of Subnational Public Opinion Estimation)

** This code will not function without proper Morning Consult credentials and internal/private packages.  

## Unique Estimates
This code can be used to generate unique MRmP estimates: 
``` r 
x <- get_margins(states = c("ALL"), vars = c('sex', 'age', 'race', 'education')) 

test <- mrmp(
  survey_data   = df,
  jointp_list   = get_joint_probs(x),
  mrmp_formula  = as.formula("y ~ age + stname  + sex + education + race"),
  survey_sample = NULL
) %>% 
  bind_rows()
```
To develop state level estimates the function takes in 4 paramaters

  - `survey_data`, a (nxn) data frame / survey data, where each row is a survey respondent and columns serve as covariates
  - `mrmp_formula`, formula used to specify the MRmP model.
  - `jointp_list`, a single data frame or a list of data frames containing the synthetic joint distributuions by state from their respective state marginal distrbutions of each covariabe, which can be calculated using get_joint_probs(X)
  - `survey_sample`, Take a random sample of the dataframe (without replacement) 


