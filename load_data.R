library(tidyverse)
library(broom)

if (!dir.exists(file.path(getwd(),"data"))) {
  dir.create(file.path(getwd(),"data"))
}

ol_url <- "https://www.canada.ca/content/dam/psc-cfp/documents/data-donnees/ar-ra/2016/ppc-cpp/CFPPSC_SLE03.csv"
ol_file <- file.path(getwd(),"data","CFPPSC_SLE03.csv")

pop_url <- "https://www.canada.ca/content/dam/tbs-sct/documents/innovation/human-resources-statistics/ssa-pop8-eng.csv"
pop_file <- file.path(getwd(),"data","ssa-pop8-eng.csv")

download.file(ol_url, ol_file)
download.file(pop_url, pop_file)

ol_raw <- read.csv(ol_file, na.strings = c("N.A.","S.O."))
pop_raw <- read.csv(pop_file, na.strings = c("N.A.","S.O."))

ol <- ol_raw %>% 
  mutate(language = case_when(description_e == "Pass Rates for the English Test" ~ "English",
                              description_e == "Pass Rates for the French Test" ~ "French"),
         level = case_when(required_level_e == "Level A only" ~ "A",
                           required_level_e == "Level B only" ~ "B",
                           required_level_e == "Level C only" ~ "C"),
         maj_lang = case_when(region_e == "East" ~ "Majority English",
                              region_e == "West" ~ "Majority English",
                              region_e == "NCR" ~ "Bilingual",
                              region_e == "Quebec except NCR" ~ "Majority French"),
         empl_type = case_when(requesting_department_e == "Canadian Forces (military personnel)" ~ "Military",
                          TRUE ~ "Public Service"),
         n_pass = (pass_rate_e / 100 * total_number_tests) %>% round(),
         n_fail = total_number_tests - n_pass)

ol_reg <- ol %>%
  filter(test_name_e == " Test of Oral Proficiency",
         required_level_e == "Level C only") %>% 
  drop_na(pass_rate_e) %>% 
  group_by(fiscal_year, test_name_e, level, language, region_e) %>% 
  summarise(mean_pass = mean(pass_rate_e, na.rm = TRUE),
            n_tests = sum(total_number_tests),
            n_pass = sum(n_pass),
            n_fail = sum(n_fail)) %>% 
  ungroup()

ol_maj <- ol %>%
  filter(test_name_e == " Test of Oral Proficiency",
         required_level_e == "Level C only") %>% 
  drop_na(pass_rate_e) %>% 
  group_by(fiscal_year, test_name_e, level, language, maj_lang) %>% 
  summarise(mean_pass = mean(pass_rate_e, na.rm = TRUE),
            n_tests = sum(total_number_tests),
            n_pass = sum(n_pass),
            n_fail = sum(n_fail)) %>% 
  ungroup()


chisq_fr <- ol_maj %>% filter(fiscal_year == "2018-2019", language == "French") %>% 
  select(maj_lang, n_pass, n_fail) %>% 
  column_to_rownames(var = "maj_lang") %>% 
  data.matrix() %>% 
  chisq.test() %>% 
  augment()

chisq_en <- ol_maj %>% filter(fiscal_year == "2018-2019", language == "English") %>% 
  select(maj_lang, n_pass, n_fail) %>% 
  column_to_rownames(var = "maj_lang") %>% 
  data.matrix() %>% 
  chisq.test() %>% 
  augment()

ggplot(ol_reg) +
  geom_col(aes(x = region_e, y = mean_pass), position = "dodge") +
  facet_grid(rows = vars(language))

ol %>% 
  filter(maj_lang == "Majority French", fiscal_year == "2018-2019") %>% 
  drop_na(pass_rate_e) %>% 
  distinct() %>% 
  select(fiscal_year,requesting_department_e, region_e,language, level, pass_rate_e) %>%
  spread(key = level, value = pass_rate_e)

ol%>% 
  select(fiscal_year,requesting_department_e, region_e,language, level, pass_rate_e)
