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


d <- import(here("spring20_finalproject_shiny", "ncsh.csv")) # data you're working with

#################### space to load functions start ####################

# Proportions of each level in a vector #### uses map_* variant
prop_level <- function(x) {
    prop <- purrr::map_dbl(split(x, x), length) / length(x)
    prop
}

prop_level(d$home_language)

# function that takes percentage of each var
prop_var <- function(df, var) {
    prop <- map(df, prop_level)
    tibble(Category = names(prop[[var]]),
           Percentage = round(prop[[var]], 4)*100)
}

prop_var(d, "state") %>% 
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
    navbarPage("Some Title",
               ####################### Ale's Panel ######################
               tabPanel("Percentage by Variable",
                        h3("Examining percentages"),
                        p("This table shows good stuff."),
                        
                        # Show a plot of the generated distribution
                        mainPanel("Cool Histogram", plotOutput("distPlot2")),
               
                        
                        
                        ), # this closes my tabPanel
               
               
               ####################### Asha's Panel ######################
               tabPanel("El Cuadro",
                        h3("Dope Table"),
                        p("This table shows good stuff."),
                        
                        # Sidebar with a slider input for number of bins 
                        sidebarLayout(
                            sidebarPanel(
                                sliderInput("bins_tbl",
                                            "Number of bins:",
                                            min = 0,
                                            max = 1,
                                            value = .5)
                            ),
                            
                            # Show a plot of the generated distribution
                            mainPanel("Cool Distribution", plotOutput("distPlot")),
                        )
               ),
               
               
               ####################### Mark's Panel ######################
               tabPanel("La Conspiración",
                        h3("Examining X by Y"),
                        p("This visualization shows good stuff."),
                        
                        # Show a plot of the generated distribution
                        mainPanel("Cool Plot", plotOutput("distPlot1", width = "100%")),
               )
    )
)             


####### ~!~ Define server logic required to create visualizations ~!~ #######
server <- function(input, output) {
    
    
    ####################### Ale's Home ######################
    output$distPlot <- renderTable({
        
    })
    
    
    ####################### Asha's Home ######################
    output$distPlot <- renderPlot({
        
        bar_plot <- function(df, x) {
            plot_graph <- ggplot(df, aes({{x}})) +
                geom_bar(aes(fill = {{x}}), show.legend = FALSE) +
                coord_flip()
            
            #if(!is.numeric(pull(df, {{x}}))) {
             #   stop()
            #}
            #else{
             #   plot_graph
            #}
            #return(plot_graph)
        }
        
        bar_plot(d, read) +
            labs(x = 'Number of days reading',
                 title = 'Confidence in kindergarten readiness by frequency of reading at home') +
            facet_wrap(~confident)
    })
    
    ####################### Mark's Home ######################
    output$distPlot1 <- renderPlot({
        
        plot1_df <- d %>%
            group_by(state, primary_cg_ed) %>%
            count(confident) %>% 
            mutate(prop_conf = round(n/sum(n), digits = 2)) %>% 
            mutate(label = glue("NCES Data from state"))
        #mutate(label = glue("NCES Data from {str_to_title(state)}"))
        
        # NOT SURE HOW TO SHOW JUST ONE VIZ
        plot1 <- plot1_df  %>%
            group_by(state) %>%
            nest() %>%
            mutate(plots = pmap(list(state, data),
                                ~ggplot(..2, aes(primary_cg_ed, prop_conf, fill = confident)) +
                                    geom_bar(stat = "identity", position = "stack") +
                                    coord_flip() +
                                    labs(title = glue("Confidence in School Preparedness \nBetween Levels of Caregiver Education: \n{..1}"),
                                         x = "Caregiver's Highest Level of Education",
                                         y = "Proportion of Parents",
                                         caption = ..2)))
        plot1$plots[[2]]
    })
}

# Run the application 
shinyApp(ui = ui, server = server)