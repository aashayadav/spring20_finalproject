
library(shiny)
library(reactable)
library(dplyr)
library(tibble)
library(tidyr)
library(rio)
library(english)
library(glue)
library(here)
library(ggplot2)
library(purrr)
library(patchwork)
theme_set(theme_minimal(16))

d <- import(here("spring20_finalproject_shiny", "ncsh.csv")) %>%
  map_at(c('child_sex', 'home_language', 'stories_songs', 'read', 'confident', 'how_well_demands', 'primary_cg_ed', 'ACE'), factor) %>%
  bind_rows() # for some reason we lost the factorization of variables when importing the data set, thus we re-factorized selected variables here.


#################### space to load functions start ####################

# Proportions of each level in a vector #### uses map_* variant
prop_level <- function(x) {
    prop <- purrr::map_dbl(split(x, x), length) / length(x)
    prop
}

# selecting factor vars in dataframe
only_fct <- function(df) {
  select_fct <- dplyr::select_if(df, is.factor)
  select_fct
}

# function that takes percentage of each var
prop_var <- function(df, var) {
    select_factor <- only_fct(df)
    prop <- purrr::map(select_factor, prop_level)
    tibble(Category = names(prop[[var]]),
           Percentage = round(prop[[var]], 4)*100)
}

prop_var(d, "read") %>% 
    reactable::reactable()

#################### space to load functions end ####################



########## Creating choices for tab3 ##########
state_choices <- as.character(d$state)
names(state_choices) <- state_choices
state_choices
######### End space for Creating choices for tab3 ##########

### Create graphing data
# you only want to do this once, so keep it out of the server
graphs_d <- d %>%
  mutate(state == as.character(state)) %>% 
  group_by(state, primary_cg_ed, child_sex) %>%
  count(confident) %>% 
  group_by(state) %>% 
  mutate(percent_conf = round(n/sum(n), digits = 4)*100)
###



####### ~!~ Define UI for application ~!~ #######

ui <- fluidPage(
    titlePanel("Our Shiny App!"),
    h2("National Survey of Children’s Health (NSCH) Data"),
    p("The purpose of this app is to allow the user to examine the National Survey of Children's Health data, specifically looking at the relationships between parent expectations for school readiness and reading behaviors among levels of parental education"),
    p("The data used for this app are publicly available on the", a(href="https://www.census.gov/programs-surveys/nsch/data.html", "US Census Bureau's website.")),
    p("Code for this app can be found on our", a(href="https://github.com/aashayadav/spring20_finalproject.git", "Github repo.")),
    # Application title
    navbarPage(" ",

               ####################### Ale's Panel ######################
               tabPanel("Percentages by Variable",
                        selectInput("var",
                                    "Variable:",
                                    choices = c(
                                      "Child sex" = "child_sex",
                                      "Home language" = "home_language",
                                      "Sing songs" = "stories_songs",
                                      "Read" = "read",
                                      "Confidence in child readiness" = "confident",
                                      "Dealing with caregiving demands" = "how_well_demands",
                                      "Caregiver education" = "primary_cg_ed",
                                      "State" = "state"),
                                    selected = "Child sex"),
                        
                        reactableOutput("tbl")
                   
               ), # closes Ale's tabPanel
               
               
               ####################### Asha's Panel ######################
               tabPanel("Kindergarten readiness",
                    inputPanel(
                        selectInput('x', 'Choose the variable', choices = c("Child sex" = "child_sex",
                                                            "Home language" = "home_language",
                                                            "Sing songs" = "stories_songs",
                                                            "Read" = "read"),
                                      selected = "read")
                          ),
                        mainPanel(plotOutput("distplot", width = "100%"))
               ), # closes Asha's tab panel
               
               
               ####################### Mark's Panel ######################
               tabPanel("Readiness by State",
                        h3("Examining Confidence Between States"),
                        p("Note: Takes a minute to produce all plots; I couldn't get the action button to limit which plots are displayed. And for some reason the percentages are off."),
                       # Show a plot of the generated distribution
                         selectInput(
                           inputId = "submit",
                           label = "Select State(s):",
                           choices = state_choices,
                           selected = c("Oregon", "Washington", "California"),
                           multiple = TRUE),
                         
                         div(style = "overflow-y: scroll;",
                           plotOutput("state_plot", height = "1000px")
                         )

               ) #closes Mark's tab panel
      ) # closes navbarPage
    ) # closes fluidPage


####### ~!~ Define server logic ~!~ #######
server <- function(input, output) {
    
    ####################### Ale's Code ######################
    output$tbl <- renderReactable({
      prop_var(d, input$var) %>% 
        reactable::reactable()
    })
    
    ####################### Asha's Code ######################
    output$distplot <- renderPlot({
      ggplot(d, aes_string(x = input$x)) +
          geom_bar(aes_string(fill= input$x)) +
          coord_flip() +
        facet_wrap(~confident) +
        labs(title = "Parents' confidence in child's kindergarten readiness")
      })

    ####################### Mark's Code ######################
    output$state_plot <- renderPlot({
      out <- graphs_d  %>%
        filter(state %in% input$submit) %>% 
        group_by(state) %>%
        nest() %>%
        mutate(plots = map2(state, data,
                            ~ggplot(.y, aes(primary_cg_ed, percent_conf, fill = confident)) +
                              geom_bar(stat = "identity", position = "stack", alpha = .65) +
                              geom_hline(yintercept = .5, linetype = "dashed", color = "darkgrey", size = .75) +
                              coord_flip() +
                              labs(title = glue("{.x} "),
                                   
                                   x = "",
                                   y = glue("Percentage of Parents in {.x}")
                              )
          )
        ) %>% 
        arrange(state) 
        
      reduce(out$plots, `/`) +
        plot_layout(guides = 'collect') +
        plot_annotation(title = "Parents' Confidence in Child's Kindergarten Readiness",
                        subtitle = "Bars displayed by parents' educational attainment level")
    })
}
    #
# Run the application 
shinyApp(ui = ui, server = server)

