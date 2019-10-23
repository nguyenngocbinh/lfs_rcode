# summarise
library(dplyr)
library(srvyr)

# Note: 
# Change data, weight in svydesign

lfs_w <- ldvl_2017 %>% 
#  mutate(extra_hours = replace_na(extra_hours, 0)) %>% 
  as_survey_design(ids = 1, data = ldvl_2017, weights = weight)


# twoways tables 
f_tw <- function(x, y){
    survey::svytable(as.formula(paste("~", x, "+", y)), design = lfs_w, round = TRUE) 
}



lfs_w %>%
  group_by(gender, agegroup5) %>% 
  summarize(
    m = survey_mean(extra_hours)
  )
