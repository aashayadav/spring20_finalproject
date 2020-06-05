# DATA PREP FILE FOR SHINY APP #
library(tidyverse)
library(rio)
library(here)
library(english)
library(glue)
library(fs)

theme_set(theme_minimal())

# importing the data
NCSH <- import(here("data","2017-2018 NSCH_Topical_DRC_Dec 2019.csv"), setclass = "tbl_df")
state <- import(here("data","fips_state.csv"))

# Left_join to add states in the NCSH dataset
final_data <- left_join(NCSH, state, by = "FIPSST")

# data prep
selected_data <- final_data %>%
  select(HHID, HHLANGUAGE, SC_AGE_YEARS, SC_SEX, MOMAGE, HHCOUNT, K6Q20, K11Q60, K11Q61, K11Q62, S9Q34, K10Q14, ACE1, ACE3, ACE4, ACE5, ACE6,   ACE7, ACE8, ACE9, ACE10, RECOGABC, A1_GRADE, A2_GRADE, K6Q60_R, K6Q61_R, FOODSIT, K8Q30, CONFIDENT, povlev4_1718, AdultEduc_1718, ACEct_1718, ACE2more_1718, State) %>%
  janitor::clean_names() %>% # cleaning names
  filter(sc_age_years == 5) # filtering data

## Function 1 (to recode missing data) and modify_*

# vector with missing values in my dataset
missing_vals <- c(90, 95, 99)

# function that returns true if values in vector are equal to missing_vals. The function takes a vector x, and specified values of missing data
recode_missing <- function(x, missing_vals = c(90, 95, 99)) {
  test <- x %in% missing_vals
  ifelse(test, NA, x)
}

# function that recodes missing values to NA. The function takes a dataframe with variables with missing data, and specified values of missing data
recode_missing_df <- function(df, missing_vals = c(90, 95, 99)) {
  modify(df, ~recode_missing(.x, missing_vals)) # here uses the function created above
}

# data cleaning
d <- recode_missing_df(selected_data) %>%
  drop_na() %>% # dropping cases with missing data
  mutate(confident = factor(confident),
         confident = fct_recode(confident,
                                "Completely confident" = "1",
                                "Mostly confident" = "2",
                                "Somewhat confident" = "3",
                                "Not at all confident" = "4"),
         how_well_demands = factor(k8q30),
         how_well_demands = fct_recode(how_well_demands,
                                       "Very well" = "1",
                                       "Somewhat well" = "2",
                                       "Not very well" = "3",
                                       "Not at all" = "4"),
         primary_cg_ed = factor(a1_grade),
         primary_cg_ed = fct_recode(primary_cg_ed,
                                    "8th grade or less" = "1",
                                    "9th-12th grade; No diploma" = "2",
                                    "High School Graduate or GED" = "3",
                                    "Vocational, trade, or business school program" = "4",
                                    "Some College Credit, no Degree" = "5",
                                    "Associate Degree" = "6",
                                    "Bachelor’s Degree" = "7",
                                    "Master’s Degree" = "8",
                                    "Doctorate" = "9"),
         home_language = factor(hhlanguage),
         home_language = fct_recode(home_language,
                                    "English" = "1",
                                    "Spanish" = "2",
                                    "Other" = "3"),
         child_sex = factor(sc_sex),
         child_sex = fct_recode(child_sex,
                                "Male" = "1",
                                "Female" = "2"),
         child_age = sc_age_years,
         ACE = factor(ace2more_1718),
         ACE = fct_recode(ACE,
                          "Exp 1 ACE" = "2",
                          "Exp 2 ACE" = "3"),
         stories_songs = factor(k6q61_r),
         read = factor(k6q60_r)) %>% 
  mutate_at(c("stories_songs","read"), ~(fct_recode(., 
                                                    "0 days" = "1",
                                                    "1-3 days" = "2",
                                                    "4-6 days" = "3",
                                                    "Every day" = "4"))) %>%
  select(hhid, child_sex, child_age, home_language, stories_songs, read, confident, how_well_demands, primary_cg_ed, ACE, state)


# Export to csv for shiny app use
rio::export(d, "spring20_finalproject_shiny.csv")

