# User Interface for a Place Selection with a map returned for that place.

shinyUI(
    fluidPage(sidebarLayout(
        sidebarPanel(id = "sidebar",
            div(img(src="psrc-logo.png", width = 260, height = 92, style = "padding-top: 25px")),
            br(),
            selectInput("Place","Please select the community you are interested in:",data_places),
            radioButtons("estimate_choice", label = "Estimate Type:",
                         choices = list("Share" = "share", "Total" = "total"), 
                         selected = "share"),
            hr(),
            strong(textOutput("summary_stat_heading")),
            textOutput("Population"),
            textOutput("Households"),
            textOutput("Jobs"),
            textOutput("HHSize"),
            hr(),
            strong("Note on Model Data:"),
            textOutput("ModelBackground"),
            br(),
            textOutput("ActivityModelBackground"),
            br(),
            textOutput("ModelLink"),
            br(),
            width=3),
        mainPanel(shinyjs::useShinyjs(), id ="Main",
                  bsButton("showpanel", "Show/hide sidebar", type = "toggle", value = TRUE),
            navbarPage(title = "", theme = "styles.css", windowTitle = "PSRC Community Profiles",
                             tabPanel(icon("city"),
                                      h1("Mobility 2050 by Community"),
                                      "We invite you to explore various model oututs for Mobility 2050 through our data portal. This data portal provides access to projects and performance results by jurisdiction. If you can't find what you're looking for, or would like further information, please contact us and we will be happy to assist you.",
                                      hr(),
                                      h2(textOutput("general_heading")),
                                      hr(),
                                      leafletOutput("place_map"),
                                      hr()
                            ), # end of Overview tabset panel

                       tabPanel(icon("users"),
                                tabsetPanel(
                                    
                                    tabPanel("Age",
                                             fluidRow(br(),column(width = 6, h2(textOutput("age_heading"))),
                                                      column(width = 3 , selectInput("Age_Year","Year:",data_years,selected = max(data_years))),
                                                      column(width = 3, selectInput("Age_Var","Age Group:",age))),
                                             fluidRow(
                                                 column(width = 6, plotlyOutput("chart_age")),
                                                 column(width = 6, leafletOutput("map_age"))
                                                 
                                             ), # end of fluid row
                                             fluidRow(
                                                 column(width = 12, hr(), DT::dataTableOutput("table_age"))
                                             ) # end of fluid row
                                    ), # end of age tab panel

                                    tabPanel("Gender",
                                             fluidRow(br(),column(width = 6, h2(textOutput("gender_heading"))),
                                                        column(width = 3, selectInput("Gender_Year","Year:",data_years,selected = max(data_years))),
                                                        column(width = 3, selectInput("Gender_Var","Gender:",gender))),
                                             fluidRow(
                                                 column(width = 6, plotlyOutput("chart_gender")),
                                                 column(width = 6, leafletOutput("map_gender"))
                                                 
                                             ), # end of fluid row
                                             fluidRow(
                                                 column(width = 12, hr(), DT::dataTableOutput("table_gender"))
                                             ) # end of fluid row
                                    ), # end of gender tab panel
                                    
                                    tabPanel("Work Status",
                                             fluidRow(br(),column(width = 6, h2(textOutput("worker_heading"))),
                                                      column(width = 3, selectInput("Worker_Year","Year:",data_years,selected = max(data_years))),
                                                      column(width = 3 , selectInput("Worker_Var","Work Status:",worker))),
                                             fluidRow(
                                                 column(width = 6, plotlyOutput("chart_worker")),
                                                 column(width = 6, leafletOutput("map_worker"))
                                                 
                                             ), # end of fluid row
                                             fluidRow(
                                                 column(width = 12, hr(), DT::dataTableOutput("table_worker"))
                                             ) # end of fluid row
                                    ), # end of work status tab panel

                                    tabPanel("Student Status",
                                             fluidRow(br(),column(width = 6, h2(textOutput("student_heading"))),
                                                column(width = 3, selectInput("Student_Year","Year:",data_years,selected = max(data_years))),
                                                column(width = 3 , selectInput("Student_Var","Student Status:",student))),
                                             fluidRow(
                                                column(width = 6, plotlyOutput("chart_student")),
                                                column(width = 6, leafletOutput("map_student"))
                                                 
                                             ), # end of fluid row
                                             fluidRow(
                                                 column(width = 12, hr(), DT::dataTableOutput("table_student"))
                                             ) # end of fluid row
                                    ) # end of student status tab panel
                                    
                                ) # end of people tabset panel
                       ), # end of people Tab Panel                       

                       tabPanel(icon("home"),
                                tabsetPanel(
                                    
                                    tabPanel("Type",
                                             fluidRow(br(),column(width = 6,  h2(textOutput("hhtype_heading"))),
                                                column(width = 3, selectInput("HHType_Year","Year:",data_years,selected = max(data_years))),
                                                column(width = 3 , selectInput("HHType_Var","Household Type:",hhtype))),
                                             fluidRow(
                                                 column(width = 6, plotlyOutput("chart_hhtype")),
                                                 column(width = 6, leafletOutput("map_hhtype"))
                                                 
                                             ), # end of fluid row
                                             fluidRow(
                                                 column(width = 12, hr(), DT::dataTableOutput("table_hhtype"))
                                             ) # end of fluid row
                                    ), # end of type tab panel
                                    
                                    tabPanel("Size",
                                             fluidRow(br(),column(width = 6, h2(textOutput("hhsize_heading"))),
                                                column(width = 3, selectInput("HHSize_Year","Year:",data_years,selected = max(data_years))),
                                                column(width = 3 , selectInput("HHSize_Var","Household Size:",hhsize))),
                                             fluidRow(
                                                 column(width = 6, plotlyOutput("chart_hhsize")),
                                                 column(width = 6, leafletOutput("map_hhsize"))
                                                 
                                             ), # end of fluid row
                                             fluidRow(
                                                 column(width = 12, hr(), DT::dataTableOutput("table_hhsize"))
                                             ) # end of fluid row
                                    ), # end of size tab panel
                                    
                                    tabPanel("Ownership",
                                             fluidRow(br(),column(width = 6, h2(textOutput("hhownership_heading"))), 
                                                column(width = 3, selectInput("HHOwn_Year","Year:",data_years,selected = max(data_years))),
                                                column(width = 3 , selectInput("HHOwn_Var","Ownership:",hhownership))),
                                             fluidRow(
                                                 column(width = 6, plotlyOutput("chart_hhownership")),
                                                 column(width = 6, leafletOutput("map_hhownership"))
                                                 
                                             ), # end of fluid row
                                             fluidRow(
                                                 column(width = 12, hr(), DT::dataTableOutput("table_hhownership"))
                                             ) # end of fluid row
                                    ), # end of ownership tab panel
                                    
                                    tabPanel("Income",
                                             fluidRow(br(),column(width = 6, h2(textOutput("hhincome_heading"))), 
                                                column(width = 3, selectInput("HHIncome_Year","Year:",data_years,selected = max(data_years))),
                                                column(width = 3 , selectInput("HHIncome_Var","Household Income:",hhincome))),
                                             fluidRow(
                                                 column(width = 6, plotlyOutput("chart_hhincome")),
                                                 column(width = 6, leafletOutput("map_hhincome"))
                                                 
                                             ), # end of fluid row
                                             fluidRow(
                                                 column(width = 12, hr(), DT::dataTableOutput("table_hhincome"))
                                             ) # end of fluid row
                                    ) # end of income tab panel
                                    
                                ) # end of household tabset panel
                       ), # end of household Tab Panel     
                                                                                 
                        tabPanel(icon("briefcase"),
                                     tabsetPanel(
                                         
                                         tabPanel("Occupation",
                                                  fluidRow(br(),column(width = 6, h2(textOutput("occupation_heading"))), 
                                                    column(width = 3, selectInput("Job_Year","Year:",data_years,selected = max(data_years))),
                                                    column(width = 3 , selectInput("Job_Var","Occupation:",jobs))),
                                                  fluidRow(
                                                    column(width = 6, plotlyOutput("chart_occupation")),
                                                    column(width = 6, leafletOutput("map_occupation"))
                                                      
                                                  ), # end of fluid row
                                                  fluidRow(
                                                      column(width = 12, hr(), DT::dataTableOutput("table_occupation"))
                                                  ) # end of fluid row
                                         ) # end of occupation tab panel
                                         
                                     ) # end of jobs and income tabset panel
                            ), # end of jobs and income Tab Panel
                       
                       tabPanel(icon("car"),
                                tabsetPanel(
                                    
                                    tabPanel("Mode to Work",
                                             fluidRow(br(),column(width = 6, h2(textOutput("work_mode_heading"))),
                                                      column(width = 3, selectInput("Mode_Year","Year:",data_years,selected = max(data_years))),
                                                      column(width = 3 , selectInput("Mode_Var","Mode:",modes))),
                                             fluidRow(
                                                 column(width = 6, plotlyOutput("chart_mode_to_work")),
                                                 column(width = 6, leafletOutput("map_mode_to_work"))
                                                 
                                             ), # end of fluid row
                                             fluidRow(
                                                 column(width = 12, hr(), DT::dataTableOutput("table_mode_to_work"))
                                             ) # end of fluid row
                                    ), # end of mode to work tab panel

                                tabPanel("Travel Time to Work",
                                             fluidRow(br(),column(width = 4, selectInput("TT_Mode","Mode:",modes)),
                                                      column(width = 4, selectInput("TT_Year","Year:",data_years,selected = max(data_years))),
                                                      column(width = 4 , selectInput("TT_Var","Travel Time:",traveltime))),
                                             fluidRow(
                                                 column(width = 6, plotlyOutput("chart_work_traveltime")),
                                                 column(width = 6, leafletOutput("map_work_traveltime"))
                                                 
                                             ), # end of fluid row
                                             fluidRow(
                                                 column(width = 12, hr(), DT::dataTableOutput("table_work_traveltime"))
                                             ) # end of fluid row
                                    ) # end of work travel-time tab panel                                                                        
                                ) # end of mode to work tabset panel
                       ), # end of mode to work Tab Panel                       
                                                   
                            tabPanel(icon("info-circle"),
                                     h1("Data Sources"),
                                     "The data in this portal comes from a few key sources:",
                                     hr(),
                                     h2("Census Data"),
                                     "The Census Data used in this portal is stored in PSRC's central database but is available from the US Census Bureau. All tables can be downloaded either via the Census API (https://www.census.gov/data/developers/data-sets/acs-5year.html) or the Census Data page (https://data.census.gov/cedsci/).",
                                     br(),
                                     h3("Census Tables:"),
                                     "Travel Time to Work: Table B08303",
                                     br(),
                                     "Age: Data Profile 5 (DP05)",
                                     br(),
                                     "Disability: Data Profile 2 (DP02)",
                                     br(),
                                     "Housing Units: Data Profile 4 (DP04)",
                                     br(),
                                     "Home Value: Data Profile 4 (DP04)",
                                     br(),
                                     "Income: Data Profile 3 (DP03)",
                                     br(),
                                     "Industry: Data Profile 3 (DP03)",
                                     br(),
                                     "Mode Share: Data Profile 3 (DP03)",
                                     br(),
                                     "Monthly Rent: Data Profile 4 (DP04)",
                                     br(),
                                     "Occupation: Data Profile 3 (DP03)",
                                     br(),
                                     "Race: Data Profile 5 (DP05)",
                                     br(),
                                     "Vehicles Available: Data Profile 4 (DP04)",
                                     br()
                            ) # end of Data tabset panel
                            
                                                        
                    ) # end of NavBar Page
                ) # end of main panel
        ) # end of sidebar layout
    ) # end of main fluid page
) #end of shiny ui
