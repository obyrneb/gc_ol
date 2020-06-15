library(tidyverse)
library(broom)
library(janitor)

pop <- pop_raw %>% 
  clean_names() %>% 
  filter(date == 201903) %>% 
  spread(fol, employees, fill = 0) %>% 
  mutate_at(vars(English,French,Unknown), as.numeric) %>% 
  mutate(total = English + French + Unknown,
         fol_en = English / total,
         fol_fr = French / total) %>% 
  rename(dept = departments_and_agencies)

ol_aug <- ol_raw %>% 
  mutate(test_language = case_when(description_e == "Pass Rates for the English Test" ~ "English",
                              description_e == "Pass Rates for the French Test" ~ "French"),
         test_level = case_when(required_level_e == "Level A only" ~ "A",
                           required_level_e == "Level B only" ~ "B",
                           required_level_e == "Level C only" ~ "C"),
         maj_lang = case_when(region_e == "East" ~ "English",
                              region_e == "West" ~ "English",
                              region_e == "NCR" ~ "Bilingual",
                              region_e == "Quebec except NCR" ~ "French"),
         my_dept = case_when(requesting_department_e == "Treasury Board (Secretariat)" ~ "TBS",
                             TRUE ~ "Other"),
         n_pass = (pass_rate_e / 100 * total_number_tests) %>% round(),
         n_fail = total_number_tests - n_pass) %>%
  select(fiscal_year,
         dept = requesting_department_e,
         my_dept,
         region = region_e,
         test_language,
         test_level,
         maj_lang,
         pass_rate = pass_rate_e,
         n_tests = total_number_tests,
         n_pass,
         n_fail) #%>% 
  #filter(fiscal_year == "2018-2019") %>% 
  #left_join(pop %>% select(dept, fol_en, fol_fr)) 

ggplot(ol_aug) +
  geom_point(aes(x = fiscal_year, y = pass_rate, size = n_tests, fill = region, alpha = my_dept, colour = my_dept),
             position = position_jitter(width = 0.1, height = 0),
             shape = 21) +
  scale_alpha_manual(values = c(TBS = 1, Other = 0.2)) +
  scale_colour_manual(values = c(TBS = "black", Other = "grey80")) +
  #geom_smooth(aes(x = fiscal_year, y = pass_rate)) +
  facet_grid(rows = vars(test_language), cols = vars(test_level))
