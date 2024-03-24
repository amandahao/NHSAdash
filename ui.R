library(shiny)
library(leaflet)

# Choices for drop-downs
vars <- c(
  # default choice, add col to df
  "Population" = "population",
  "Median income" = "median_household_income",
  "Number of schools in county" = "number_of_schools",
  "Total student enrollment" = "enrollment",
  "Total number of teachers" = "teachers_total_fte"
)


navbarPage("NHSA Dashboard", id="nav",
           
           tabPanel("Interactive map",
                    div(class="outer",
                        
                        tags$head(
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h2("Head Start explorer"),
                                      
                                      selectInput("color", "Color", vars),
                                      selectInput("size", "Size", vars, selected = "adultpop"),
                                      conditionalPanel("input.color == 'population' || input.size == 'population'"
                                                       # Only prompt for threshold when coloring or sizing by superzip
                                                       # , numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                      ),
                                      
                                      # plotOutput("histCentile", height = 200),
                                      # plotOutput("scatterCollegeIncome", height = 250)
                        ),
                        
                        tags$div(id="cite",
                                 # 'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960–2010'), ' by Charles Murray (Crown Forum, 2012).'
                                 '2021 School District Dataset via ', tags$em('Education Data Portal v. 0.20.0, Urban Institute,'), ' under ODC Attribution License.'
                        )
                    )
           ),
           
           tabPanel("County map",
                    div(class="outer",
                        tags$head(
                          includeCSS("styles.css"),
                        ),
                        leafletOutput("countyMap", width="100%", height="100%")
                    )
           ),
           
           tabPanel("Data explorer",
                    fluidRow(
                      column(3,
                             selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
                      ),
                      column(3,
                             conditionalPanel("input.states",
                                              selectInput("counties", "Counties", c("All counties"=""), multiple=TRUE)
                             )
                      ),
                      column(3,
                             conditionalPanel("input.states",
                                              selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
                             )
                      )
                    ),
                    # fluidRow(
                    #   column(1,
                    #          numericInput("minScore", "Min score", min=0, max=100, value=0)
                    #   ),
                    #   column(1,
                    #          numericInput("maxScore", "Max score", min=0, max=100, value=100)
                    #   )
                    # ),
                    hr(),
                    DT::dataTableOutput("ziptable")
           ),
           
           conditionalPanel("false", icon("crosshairs"))
)