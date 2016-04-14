library(readr)
library(dplyr)
library(MRmarginalP)
library(mrpExport)
library(car)
library(mrpExport)

setwd("~/Desktop")
survey_data <- read_rds("tsdat_updated.rds")
survey_data$y <- recode(survey_data$nr2Bin, "1=1;2=0;else=NA")

df <- survey_data %>% 
  sample_n(10000)

my_formula <- as.formula("v16g5 ~ age + stname  + sex + education")

table(survey_data$nr2Bin)

data$stname <- as.character(c(state.abb[1:8], 'DC', state.abb[9:50]))[data$demState]


x <- get_margins(states = c('DC', 'FL'), vars = c('sex', 'age', 'race', 'education')) 
y <- get_joint_probs(x)


x <- get_margins(states = c("ALL"), vars = c('sex', 'age', 'race', 'education'))
y <- get_joint_probs(x)

my_formula <- as.formula(y ~ (1|age) + (1|sex) + (1|education) + (1|race) + Obama12)
state_estimates <- mrmp(survey_data, y, my_formula)


?mrpExport()
