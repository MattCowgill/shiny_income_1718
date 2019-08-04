# Prepare ABS SIH 2017-18 summary data for a Shiny app

library(tidyverse)
library(readxl)

# Load data ----

range_data_path <- "data/65230do001_201718.xlsx"

gross_ranges <- read_excel(range_data_path,
                           range = "Table 1.3!A5:Q41",
                           col_names = TRUE)

equiv_ranges <- read_excel(range_data_path,
                           range = "Table 1.3!A46:Q74",
                           col_names = FALSE) %>%
  select(-2)

equiv_deciles <- read_excel(range_data_path,
                            range = "Table 1.1!A17:Q25", 
                            col_names = FALSE) %>%
  select(-2)

# Tidy data ------

gross_ranges <- gross_ranges %>%
  rename(income_range = 1,
         units = 2) %>%
  filter(!is.na(units)) %>%
  select(-units) 

names(equiv_ranges) <- names(gross_ranges)
names(equiv_deciles) <- names(gross_ranges) 
equiv_deciles <- equiv_deciles %>%
  rename(percentile = income_range)


tidy_range_data <- function(df) {

  df %>%
    gather(key = year, value = num,
           -income_range) %>%
    mutate(income_range_num = case_when(grepl("Negative", income_range) ~ "-9999999–-1",
                                        income_range == "No income" ~ "0–0",
                                        TRUE ~ income_range),
           income_range_num = gsub(" or more", "–9e9999", income_range_num)) %>% 
    separate(col = income_range_num, 
             into = c("range_bottom", "range_top"),
             sep = "–", 
             remove = TRUE) %>%
    mutate_at(vars(starts_with("range_")),
              parse_number)

}

gross_ranges <- tidy_range_data(gross_ranges)
equiv_ranges <- tidy_range_data(equiv_ranges)

equiv_deciles <- equiv_deciles %>%
  gather(key = year,
         value = income,
         -percentile) %>%
  mutate(percentile = parse_number(percentile))

# Expand data ----
# We want one row per household (gross income) or person (equivalised income)

gross_long <- gross_ranges %>%
  mutate(num = num / 10) %>%
  uncount(weights = num)

equiv_long <- equiv_ranges %>%
  # Round to nearest 100,000
  mutate(num = round(num / 100, 0)) %>%
  # One row per ~100,000 people
  uncount(weights = num)

equiv_long_latest <- equiv_long %>%
  filter(year == max(year)) %>%
  select(-year) %>%
  group_by(income_range) %>%
  mutate(id_within_group = row_number()) %>%
  ungroup() %>%
  mutate(id = row_number())

equiv_dec_latest <- equiv_deciles %>%
  filter(year == max(year))

save(equiv_long_latest, file = "equiv_long_latest.Rda")
save(equiv_dec_latest, file = "equiv_dec_latest.Rda")

