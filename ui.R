library(shiny)
library(shinyWidgets)
library(plotly)
library(DT)
library(shinycssloaders)
library(shinydashboard)

# Define UI for application that draws a histogram
dashboardPage(
    dashboardHeader(
        title = "Title IV Program Dashboard",
        titleWidth = 300
    ),
    
    dashboardSidebar(
        width = 300,
        sidebarMenu(
            hr(),
            menuItem(HTML("Individual University <br>Directed Load Program <br>Dashabord"), 
                     icon = icon("university"), 
                     tabName = "university"),
            hr()
        )
    ),
    
    dashboardBody(
        tags$head( 
            tags$style(HTML(".main-sidebar { font-size: 20px; }")) #change the font size to 20
        ),
        tabItems(
            # tabItem(tabName = "dashboard",
            #         # h2("Dashboard tab content"),
            #         
            # ),
            
            tabItem(
                tabName = "university",
                # h2("Widgets tab content"),
                ##### Individual - First Row Left Side #####
                h2(HTML("Section 1: Individual University Data Visualization")),
                fluidRow(
                    column(
                        width = 6,
                        box( ### 
                            width = 12,
                            title = HTML("University Selection"), 
                            status = "primary",
                            solidHeader = TRUE, 
                            height = "10em",
                            uiOutput("selectedUni_Ind")
                        ),
                        box( ### 
                            width = 6,
                            title = HTML("Direct Loan Category"), 
                            status = "danger",
                            solidHeader = TRUE, 
                            height = "10em",
                            uiOutput("selectedUni_Ind_DLCat")
                        ),
                        box( ### 
                            width = 6,
                            title = HTML("Direct Loan Variable"), 
                            status = "danger",
                            solidHeader = TRUE, 
                            height = "10em",
                            uiOutput("selectedUni_Ind_Cat")
                        ),
                        box(
                            width = 12,
                            title = HTML("Visualization of Quarterly Data Across 5 Years"),
                            status = "danger",
                            solidHeader = TRUE, 
                            height = "42em", 
                            dropdown(
                                # tags$h4("C:"),    
                                circle = FALSE, 
                                # style = "unite", 
                                actionBttn(
                                    inputId = "individualQuarterlyHelp",
                                    label = "Display Descirption Information for This Plot", 
                                    style = "material-flat",
                                    color = "danger", 
                                ),
                                hr(),
                                
                                actionBttn(
                                    inputId = "individualTuitionAssumption",
                                    label = "Display Assumption for This Data", 
                                    style = "material-flat",
                                    color = "primary", 
                                ),
                                icon = icon("question-circle"),
                                status = "danger", width = "300px",
                                animate = animateOptions(
                                    enter = animations$bouncing_entrances$bounceInLeft,
                                    exit = animations$bouncing_exits$bounceOutLeft
                                ),
                                tooltip = tooltipOptions(title = "Click to See Help File")
                            ),
                            shinycssloaders::withSpinner(
                                plotlyOutput("individualQuarterlyPlot", height = "500px"), 
                                type = 6, color = "#DE4B39"
                            )
                        )
                    ),
                    column(
                        width = 6,
                        ##### Individual - First Row Right Side #####
                        # box(
                        #     width = 12,
                        #     title = HTML("External Links for The Select University"),
                        #     height = "12em",
                        #     solidHeader = TRUE,
                        #     status = "primary",
                            infoBoxOutput("scorecard"),
                            infoBoxOutput("googleSearch"), 
                            infoBoxOutput("topUni"), 
                            # infoBox("Link to ", "University Website Search", 
                            #         icon = icon("google"), fill = TRUE, color = "light-blue", width = 6),
                            
                        # ),
                        box(
                            width = 12,
                            status = "success",
                            height = "13em",
                            solidHeader = TRUE, 
                            title = HTML("Tuition Information: Select Variable"), 
                            h6(HTML("Please note some school tuition data are not avaliable")),
                            
                            radioGroupButtons(
                                inputId = "tuitionInfoSelector",
                                label = "Please select a parameter to plot for the given school",
                                choiceNames = c(HTML("<br>Tuition and Fees"), 
                                            HTML("<br>Net Price"),
                                            HTML("<br>Percent Receiving Grant Aid")
                                            ), 
                                choiceValues = c("TuitionAndFees", "NetPrice", "PercentReceivingGrantAid"),
                                justified = TRUE,
                                checkIcon = list(
                                    yes = icon("hand-point-down"))
                            )  
                        ),
                        box( ### 
                            width = 12,
                            title = HTML("Tuition Information 2015 - 2019 (Undergraduate Only)"), 
                            status = "success",
                            solidHeader = TRUE, 
                            height = "42em", 
                            dropdown(
                                # tags$h4("C:"),    
                                circle = FALSE, 
                                # style = "unite", 
                                actionBttn(
                                    inputId = "individualTuitionHelp",
                                    label = "Display Descirption Information for This Plot", 
                                    style = "material-flat",
                                    color = "success", 
                                ),
                                icon = icon("question-circle"),
                                status = "success", 
                                width = "300px",
                                animate = animateOptions(
                                    enter = animations$bouncing_entrances$bounceInRight,
                                    exit = animations$bouncing_exits$bounceOutRight
                                ),
                                tooltip = tooltipOptions(title = "Click to See Help File")
                            ),
                            shinycssloaders::withSpinner(
                                plotlyOutput("tuitionNetPricePlot", height = "500px"), 
                                type = 6, color = "#05A55A"
                            )
                        ),

                    )
                ),
                h2(HTML("Section 2: Yearly Data Linear Model for the Selected University")),
                fluidRow(
                    column(
                        width = 6,
                        box( ### 
                            width = 6,
                            title = HTML("Direct Loan Category"), 
                            status = "warning",
                            solidHeader = TRUE, 
                            height = "10em",
                            uiOutput("selectedUni_Model_DLCat")
                        ),
                        box( ### 
                            width = 6,
                            title = HTML("Direct Loan Variable"), 
                            status = "warning",
                            solidHeader = TRUE, 
                            height = "10em",
                            uiOutput("selectedUni_Model_Cat")
                        ),
                        box( ### 
                            width = 12,
                            title = HTML("Linear Model Output"), 
                            status = "warning",
                            solidHeader = TRUE, 
                            height = "10em",
                            DT::dataTableOutput("modelTable")
                            # uiOutput("selectedUni_Model_DLCat")
                        ),
                        # box( ### 
                        #     width = 6,
                        #     title = HTML("Model Fit"), 
                        #     status = "warning",
                        #     solidHeader = TRUE, 
                        #     height = "10em"
                        #     # uiOutput("selectedUni_Model_Cat")
                        # ),
                    ),
                    column(
                        width = 6,
                        box(
                            width = 12,
                            title = HTML("Linear Model Fit"), 
                            status = "warning",
                            solidHeader = TRUE, 
                            height = "42em", 
                            dropdown(
                                # tags$h4("C:"),    
                                circle = FALSE, 
                                # style = "unite", 
                                actionBttn(
                                    inputId = "linearModelHelp",
                                    label = "Display Descirption Information for This Plot", 
                                    style = "material-flat",
                                    color = "warning", 
                                ),
                                icon = icon("question-circle"),
                                status = "warning", 
                                width = "300px",
                                animate = animateOptions(
                                    enter = animations$bouncing_entrances$bounceInRight,
                                    exit = animations$bouncing_exits$bounceOutRight
                                ),
                                tooltip = tooltipOptions(title = "Click to See Help File")
                            ),
                            shinycssloaders::withSpinner(
                                plotlyOutput("LinaerModelFitPlot", 
                                             height = "500px"), 
                                type = 6, color = "#05A55A"
                            )
                        )
                    )
                )
            )
        )
    )
)
