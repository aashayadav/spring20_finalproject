
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

theme_set(theme_minimal(16))

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




########## Creating choices for tab3 ##########
state_choices <- graphs$state
names(state_choices) <- graphs$state
state_choices
######### End space for Creating choices for tab3 ##########



####### ~!~ Define UI for application ~!~ #######

ui <- fluidPage(
    titlePanel("Our Shiny App!"),
    h2("National Survey of Children’s Health (NSCH) Data"),
    p("The purpose of this app is to alow those who are interested in examining the National Survey of Children's Health data, specifically looking at the relationships between parent expectations for school readiness and reading behaviors among levels of parental education"),
    p("The data used for this app is publicly available on the", a(href="https://www.census.gov/programs-surveys/nsch/data.html", "US Census Bureau's website.")),
    p("[insert rubric and guide for where to find for Daniel] - code for this app can be found:", a(href="https://github.com/aashayadav/spring20_finalproject.git", "Github.")),
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
                        mainPanel(plotOutput("distplot", width = "100%"))
               ), # closes Asha's tab panel
               
               
               ####################### Mark's Panel ######################
               tabPanel("La Conspiración",
                h3("Confidence in School Preparedness Between Levels of Caregiver Education"),
                      p("This visualization shows good stuff."),

                        # Show a plot of the generated distribution
                        mainPanel("Cool Plot", 
                                  fluidRow(
                                    column(4, 
                                      selectInput(
                                        inputId = "submit",
                                        label = "Select State(s):",
                                        choices = state_choices,
                                        multiple = TRUE)),
                                    ),
                                  fluidRow(
                                    div(
                                      id = "plot-container",
                                      uiOutput( outputId = "graphs_ui"
                                      )
                                    )
                                  )
                        )
               ) #closes tabPanel
    ) #closes navbarPage
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
    graphs_d <- d %>%
      mutate(state == as.character(state)) %>% 
      group_by(state, primary_cg_ed, child_sex) %>%
      count(confident) %>% 
      mutate(prop_conf = round(n/sum(n), digits = 2))
        
    # purrr::nest %>% mutate() and parallel iteration with pmap() to create list of graphs
    graphs <- eventReactive(input$submit, {
        
        graphs_d  %>%
        group_by(state) %>%
        nest() %>%
        mutate(plots = pmap(list(state, data),
           ~ggplot(..2, aes(primary_cg_ed, prop_conf, fill = confident)) +
            geom_bar(stat = "identity", position = "stack", alpha = .65) +
            geom_hline(yintercept = .5, linetype = "dashed", color = "darkgrey", size = .75) +
                        coord_flip() +
                        labs(x = "Parental's Highest Education",
                             y = glue("Proportion of Parents in {..1}")
                            )
                      )
               ) %>% 
          arrange(state) %>% 
          pull(plots)
        })

      # {purrr} function outside the basic map family 
      # purrr::iwalk to create dynamic number of outputs
      observeEvent(input$submit, {
        
        iwalk(graphs(), ~{
          output_name <- paste0("plot_", .y)
          output[[output_name]] <- renderPlot(.x)
        })
      
      # renderUI to create dynamic number of output ui elements
      output$graphs_ui <- renderUI({
          
          plots_list <- imap(graphs(), ~{
            tagList(
              plotOutput(outputId = paste0("plot_", .y)),
              br()
                    )                   }
                            )

                                  })
                                  })
    }
# Run the application 
shinyApp(ui = ui, server = server)
