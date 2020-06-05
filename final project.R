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

## function 2 (to create plots) -- add this to the shiny app so peep can interact and change the variables from a drop down menu. Needs further customization.

bar_plot <- function(df, x) {
  plot_graph <- ggplot(df, aes({{x}})) +
    geom_bar(aes(fill = {{x}}), show.legend = FALSE) +
    coord_flip()
  
  if(!as.numeric(pull(df, {{x}}))) {
    stop()
  }
  else{
    plot_graph
  }
  return(plot_graph)
}

bar_plot(d, read) +
  labs(x = 'Number of days reading',
       title = 'Confidence in kindergarten readiness by frequency of reading at home') +
  facet_wrap(~confident)

## Using purrr::nest %>% mutate() and pmap

# Examining 'Confidence in School Readiness' by 'Caregiver's Highest Level of Education'
bar_plot(d, read) +
  labs(x = 'Number of days reading',
       title = 'Confidence in kindergarten readiness by frequency of reading at home') +
  facet_wrap(~confident)

plot1_df <- d %>%
  group_by(state, primary_cg_ed) %>%
  count(confident) %>% 
  mutate(prop_conf = round(n/sum(n), digits = 2)) %>%  
  mutate(label =
           glue("NCES Data from {str_to_title(state)}"))

plot1 <- plot1_df  %>%
  group_by(state, label) %>%
  nest() %>%
  mutate(plots = pmap(list(state, label, data),
                      ~ggplot(..3, aes(primary_cg_ed, prop_conf, fill = confident)) +
                        geom_bar(stat = "identity", position = "dodge") +
                        coord_flip() +
                        labs(title = glue("Confidence in School Preparedness Between \nLevels of Caregiver Education: {..1}"),
                             x = "Caregiver's Highest Level of Education",
                             y = "Proportion of Parents",
                             caption = ..2)))
plot1$plots[[20]]

## Using walk_*

# Saving plots for examining 'Confidence in School Readiness' by 'Caregiver's Highest Level of Education'

# Creating directory to save plots
fs::dir_create(here::here("plots", "plot1"))

# Creating file path
files <- str_replace_all(tolower(plot1$state), " ", "-")
paths <- here::here("plots", "plot1", glue("schl-conf-and-edlevel-by-state_{files}.png"))
paths

# saving plots
walk2(paths, plot1$plots, ggsave,
      width = 9.5, 
      height = 6.5,
      dpi = 500)

## to use map_df and map_dbl 
## Daniel suggested creating a function that looks at one column and calculates the proportion (like Mark did), then another function that loops that function through all the colums in the data frame. 
## We can explore adding a table

##### this one doesn't use a variant of map, so not using it #######

# one case
# prop_read <- d %>%
#   count(read) %>%
#   mutate(prop_read = round(n/sum(n), digits = 2))
# 
# # generalizing to function
# prop_tbl <- function(df, x) {
#   df %>%
#     count({{x}}) %>%
#     mutate(proportion = round(n/sum(n), digits = 2))
# }
# prop_tbl(d, read)

#####################################################################

######### Proportions of each level in a vector #### uses map_* variant

prop_level <- function(x) {
  prop <- purrr::map_dbl(split(x, x), length) / length(x)
  prop
}

prop_level(d$read)

##### selecting factor vars in dataframe ##### 
only_fct <- function(df) {
  select_fct <- dplyr::select_if(df, is.factor)
  select_fct
} # for some reason the factorization gets lost in the data-prep_shiny.R file, so not using this

only_fct(d)

######## FULL FUNCTION ######
prop_var <- function(df, var) {
  select_factor <- only_fct(df)
  prop <- map(select_factor, prop_level)
  tibble(Category = names(prop[[var]]),
         Percentage = round(prop[[var]], 4)*100)
}

prop_var(d, "read") %>% 
  reactable::reactable()

# how to translate to shiny? 
# in ui
selectInput("var", "Select Variable", 
            choices = c("read", "confidence", "etc."))

reactableOutput("tbl")

# in your server
output$tbl <- renderReactable({
  prop_df(d, input$var) %>% 
    reactable::reactable()
})