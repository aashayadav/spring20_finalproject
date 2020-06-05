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

theme_set(theme_minimal(15))


# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Bomb-Ass Shiny App!"),
    h2("National Survey of Children’s Health (NSCH) Data"),
    p("The purpose of this app is to alow those who are interested in examining the National Survey of Children's Health data, specifically looking at the relationships between parent expectations for school readiness and reading behaviors among levels of parental education"),
    p("The data used for this app can be accessed on the", a(href="https://www.census.gov/programs-surveys/nsch/data.html", "US Census Bureau's website.")),
    
    # Application title
    navbarPage("Interactive Data Visualizations",
               tabPanel("El Gráfico",
                        h3("Examining X by Y"),
                        p("This visualization shows good stuff."),
                        
                        # Show a plot of the generated distribution
                        mainPanel("Cool Histogram", plotOutput("distPlot2")),
               ),
               
               tabPanel("El Cuadro",
                        h3("Examining X by Y"),
                        p("This visualization shows good stuff."),
                        
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
               
               tabPanel("La Conspiración",
                        h3("Examining X by Y"),
                        p("This visualization shows good stuff."),
                        
                        # Show a plot of the generated distribution
                        mainPanel("Cool Plot", plotOutput("distPlot1")),
               )
    )
)             


# Define server logic required to create visualizations
server <- function(input, output) {
    
    d <- import("~/Desktop/Dropbox/edld610_functional_programming_R/spring20_finalproject/spring20_finalproject_shiny/nsch.csv") # data you're working with
    
    # First visual
    output$distPlot <- renderPlot({
        
        bar_plot <- function(df, x) {
            plot_graph <- ggplot({{df}}, aes({{x}})) +
                geom_bar(aes(fill = x), show.legend = FALSE) +
                coord_flip()
            #if(!is.numeric({{x}})) { #I wasn't able to get it tow work with this code
            #  stop()
            #}
            #else{
            # plot_graph
            #}
            #return(plot_graph)
        }
        
         bar_plot(d, d$read) +
            labs(x = 'Number of days reading',
                 title = 'Confidence in kindergarten readiness by \nfrequency of reading at home') +
            facet_wrap(~confident)
    })
    
    # Draw a second one
    output$distPlot1 <- renderPlot({
        # generate bins based on input$bins from ui.R
        bins <- seq(min(x), max(x), length.out = as.numeric(input$bins_hist) + 1)
        
        # draw the histogram with the specified number of bins
        plot1_df <- d %>%
            group_by(state, primary_cg_ed) %>%
            count(confident) %>% 
            mutate(prop_conf = round(n/sum(n), digits = 2))
        
        p2 <- hist(plot1_df$prop_conf, breaks = bins, col = 'darkgray', border = 'white')
        print(p2)
    })

    # Draw a visualization
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