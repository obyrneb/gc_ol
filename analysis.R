library(tidyverse)
library(broom)
library(janitor)

pop <- pop_raw %>% 
  filter(Date == 201903) %>% 
  spread(FOL, Employees) %>% 
  rename(dept = Departments.and.Agencies)

ol_aug <- ol_raw %>% 
  left_join()
