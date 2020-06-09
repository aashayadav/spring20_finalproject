
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


####### ~!~ Define UI for application that draws a histogram ~!~ #######
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
                                  inputPanel(
                                    selectInput("state", "Choose a state:", choices = c(
                                      "Alabama" =1, "Alaska" = 2, "Arizona" =3, "Arkansas" =4, "California" =5, "Colorado" =6, "Connecticut" =7, "Delaware" =8, "District of Columbia" =9, "Florida" =10, "Georgia" =11, "Hawaii" =12, "Idaho"  =13, "Illinois" =14, "Indiana"  =15, "Iowa" =16, "Kansas" =17, "Kentucky" =18, "Louisiana" =19, "Maine"=20, "Maryland" =21, "Massachusetts" =22, "Michigan" =23, "Minnesota" =24, "Mississippi" =25, "Missouri" =26, "Montana" =27, "Nebraska" =28, "Nevada" =29, "New Hampshire"  =30, "New Jersey" =31, "New Mexico" =32, "New York" =33, "North Carolina" =34, "North Dakota" =35, "Ohio" =36, "Oklahoma" =37, "Oregon" =38, "Pennsylvania" =39, "Rhode Island" =40, "South Carolina" =41, "South Dakota" =42, "Tennessee" =43, "Texas" =44, "Utah" =45, "Vermont" =46,  "Virginia" =47, "Washington" =48, "West Virginia" =49, "Wisconsin"  =50, "Wyoming" = 51))
                                  ),  imageOutput("distPlot2", width = "100%"))
               ) #closes tabPanel
    )
    )


####### ~!~ Define server logic required to create visualizations ~!~ #######
server <- function(input, output) {
    
    ####################### Ale's Home ######################
    output$tbl <- renderReactable({
      prop_var(d, input$var) %>% 
        reactable::reactable()
    })
    
    ####################### Asha's Home ######################
    output$distplot <- renderPlot({
      ggplot(d, aes_string(x = input$x)) +
          geom_bar(aes_string(fill= input$x)) +
          coord_flip() +
        facet_wrap(~confident) +
        labs(title = "Parents' confidence in child's kindergarten readiness")
      })

    ####################### Mark's Home ######################
    graphs_d <- d %>%
      mutate(state == as.character(state)) %>% 
      group_by(state, primary_cg_ed, child_sex) %>%
      count(confident) %>% 
      mutate(prop_conf = round(n/sum(n), digits = 2))
        
    output$distPlot2 <- renderImage({ 
      # create list of graphs
      graphs <- graphs_d  %>%
        group_by(state) %>%
        nest() %>%
        mutate(plots = pmap(list(state, data),
                            ~ggplot(..2, aes(primary_cg_ed, prop_conf, fill = confident)) +
                              geom_bar(stat = "identity", position = "stack", alpha = .65) +
                              geom_hline(yintercept = .5, linetype = "dashed", color = "darkgrey", size = .75) +
                              coord_flip() +
                              labs(x = "Parent'al's Highest Education",
                                   y = glue("Proportion of Parents in {..1}"),
                                   caption = ..1)))
      # Saving plots
      # Creating directory to save plots
      fs::dir_create(here::here("spring20_finalproject_shiny", "plots"))
      # Creating file path
      files <- stringr::str_replace_all(tolower(graphs$state), " ", "-")
      paths <- here::here("spring20_finalproject_shiny", "plots", glue("conf-edlevel_{files}.png"))
      # saving plots
      walk2(paths, graphs$plots, ggsave,
            width = 9.5, 
            height = 6.5,
            dpi = 500)
    })

}
# Run the application 
shinyApp(ui = ui, server = server)