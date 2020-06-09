#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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

theme_set(theme_minimal(15))

d <- import(here("spring20_finalproject_shiny", "ncsh.csv")) %>%
  map_at(c('child_sex', 'home_language', 'stories_songs', 'read', 'confident', 'how_well_demands', 'primary_cg_ed', 'ACE', 'state'), factor) %>%
  bind_rows() # for some reason we lost the factorization of variables when importing the data set, thus we re-factorized selected variables here.

#################### space to load functions start ####################

# Proportions of each level in a vector #### uses map_* variant
prop_level <- function(x) {
    prop <- purrr::map_dbl(split(x, x), length) / length(x)
    prop
}

prop_level(d$home_language)

# selecting factor vars in dataframe
only_fct <- function(df) {
  select_fct <- dplyr::select_if(df, is.factor)
  select_fct
}

only_fct(d)

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


####### ~!~ Define UI for application that draws a histogram ~!~ #######
ui <- fluidPage(
    titlePanel("Our Shiny App!"),
    h2("National Survey of Children’s Health (NSCH) Data"),
    p("The purpose of this app is to alow those who are interested in examining the National Survey of Children's Health data, specifically looking at the relationships between parent expectations for school readiness and reading behaviors among levels of parental education"),
    p("The data used for this app is publicly available on the", a(href="https://www.census.gov/programs-surveys/nsch/data.html", "US Census Bureau's website.")),
    p("[insert rubric and guide for where to find for Daniel]"),
    
    # Application title
    navbarPage("Some name",
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
                   
               ), # this closes my tabPanel
               
               
               ####################### Asha's Panel ######################
               tabPanel("Kindergarten readiness",
                        inputPanel(
                          selectInput('x', 'Choose the variable', choices = c("Child sex" = "child_sex",
                                                            "Home language" = "home_language",
                                                            "Sing songs" = "stories_songs",
                                                            "Read" = "read"),
                                      selected = "read")
                          ),
                        mainPanel(plotOutput("distplot"))
               ), # closes Asha's tab panel
               
               
               ####################### Mark's Panel ######################
               tabPanel("La Conspiración",
                        h3("Examining X by Y"),
                        p("This visualization shows good stuff."),
                        
                        # Show a plot of the generated distribution
                        mainPanel("Cool Plot", plotOutput("distPlot1"))
               )
    )
    )


####### ~!~ Define server logic required to create visualizations ~!~ #######
server <- function(input, output) {

    ####################### Asha's Home ######################
    output$distplot <- renderPlot({
      ggplot(d, aes_string(x = input$x)) +
          geom_bar(aes_string(fill= input$x)) +
          coord_flip() +
        facet_wrap(~confident) +
        labs(title = "Parents' confidence in child's kindergarten readiness")
      })


    
    ####################### Ale's Home ######################
    output$tbl <- renderReactable({
      prop_var(d, input$var) %>% 
        reactable::reactable()
    })

    
    ####################### Mark's Home ######################
    output$distPlot2 <- renderPlot({
        plot1_df <- d %>%
            group_by(state, primary_cg_ed) %>%
            count(confident) %>% 
            mutate(prop_conf = round(n/sum(n), digits = 2)) #%>%  
            #mutate(label = glue("NCES Data from {str_to_title(state)}"))
        
        # NOT SURE HOW TO SHOW JUST ONE VIZ
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
        print(plot1[[1]])
    })
}
# Run the application 
shinyApp(ui = ui, server = server)