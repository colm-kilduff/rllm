# In this analysis, we intend on understanding the
# population of businesses within Canada. 
# We are using the ODBus dataset from 2023 to do this.
library(tidyr)


odbus <- read.csv(file.path(Sys.getenv('PROJ_DIR'), 'code', '202411-canadian-business-analysis/data/ODBus_v1/ODBus_v1.csv'))

naics_codes <-
  tibble(
    code = c("11", "21", "22", "23", list(c("31", "32", "33")), "41", list(c("44", "45")), list(c("48", "49")), "51", "52", "53", 
             "54", "55", "56", "61", "62", "71", "72", "81", "91"),
    sector = c("Agriculture, forestry, fishing and hunting", 
               "Mining, quarrying, and oil and gas extraction", 
               "Utilities", 
               "Construction", 
               "Manufacturing", 
               "Wholesale trade", 
               "Retail trade", 
               "Transportation and warehousing", 
               "Information and cultural industries", 
               "Finance and insurance", 
               "Real estate and rental and leasing", 
               "Professional, scientific and technical services", 
               "Management of companies and enterprises", 
               "Administrative and support, waste management and remediation services", 
               "Educational services", 
               "Health care and social assistance", 
               "Arts, entertainment and recreation", 
               "Accommodation and food services", 
               "Other services (except public administration)", 
               "Public administration")
  ) |>
  # making one row per code.
  unnest(code)

odbus <-
  odbus |>
  left_join(naics_codes, by = c("derived_NAICS" = "code")) |>
  mutate(sector = ifelse(is.na(sector), 'Unknown', sector))
