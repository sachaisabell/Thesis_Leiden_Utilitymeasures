


library(readxl)
library(tidyverse)

survey_res <- read_excel("results-survey421291 (1).xlsx")


### age range
survey_res$Age = as.numeric(survey_res$Age)
min(survey_res$Age, na.rm = TRUE)
max(survey_res$Age, na.rm = TRUE)
mean(survey_res$Age, na.rm = TRUE)

### experience
table(survey_res$Education)
table(survey_res$Experience)
table(survey_res$work0studies)




getwd()













































































