library(tidyverse)
library(broom)
library(janitor)
library(cowplot)

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
  mutate(test_language = case_when(description_e == "Pass Rates for the English Test" ~ "English Test",
                              description_e == "Pass Rates for the French Test" ~ "French Test"),
         test_level = case_when(required_level_e == "Level A only" ~ "A Level",
                           required_level_e == "Level B only" ~ "B Level",
                           required_level_e == "Level C only" ~ "C Level"),
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
  scale_alpha_manual(values = c(TBS = 1, Other = 0.5)) +
  scale_colour_manual(values = c(TBS = "black", Other = "grey80")) +
  scale_size_continuous(range = c(1,10)) +
  scale_y_continuous(breaks = c(0,25,50,75,100), limits = c(0,100)) +
  #scale_fill_manual(values = c(East = "#984ea3",
  #                             NCR = "#e41a1c",
  #                             `Quebec except NCR` = "#377eb8",
  #                             West = "#4daf4a")) +
  ##geom_smooth(aes(x = fiscal_year, y = pass_rate)) +
  facet_grid(rows = vars(test_language), cols = vars(test_level)) +
  labs(title = "Oral Second Language Evaluation Results by Department (2010-11 to 2018-19)",
       subtitle = "Each dot represents a number of tests for a department in a specific region.",
       caption = "Data: https://www.canada.ca/content/dam/psc-cfp/documents/data-donnees/ar-ra/2016/ppc-cpp/CFPPSC_SLE03.csv",
       x = "Fiscal year",
       y = "Pass rate (%)",
       fill = "Region",
       colour = "Department",
       alpha = "Department",
       size = "Number of tests") +
  theme(axis.text.x = element_text(angle = 270))

ggsave(file.path(getwd(),"plots","Oral SLE Pass Rates.png"), width = 10, height = 8)
