#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(shinydashboardPlus)
library(colorspace)
library(tidyverse)

## Loading the data
load("./rData/allFSAData.rData")
load("./rData/CATCData.rData")
load("./rData/modelresult.Rdata")
# load("./rData/FAFSA_SchoolApplicationData.rData")
options(rsconnect.error.trace = TRUE)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    values <- reactiveValues(colors5 = lighten(c("#e27D60", "#85CDCB", "#e8A87C", "#c38d9e", "#41B3A3"), 0.5))    
    
    ##### UI Section #####
    output$selectedUni_Ind <- renderUI({
        uniqueList <- allFSAData_Key$UniversityName
        pickerInput(
            inputId = "selectedUni_Ind",
            label = "Select the University You want to Investigate: ",
            choices = uniqueList,
            selected = "NORTH CAROLINA STATE UNIVERSITY",
            options = list(
                `live-search` = TRUE,
                liveSearchPlaceholder = "Search a Parameter:",
                size = 10
            )
            # color = "success",
            # icon = icon("hire-a-helper")
        )
    })
    
    output$selectedUni_Ind_DLCat <- renderUI({
        uniqueList <- unique(DL_ColumnKey$DLCategory)
        pickerInput(
            inputId = "selectedUni_Ind_DLCat",
            label = "Direct Loan Cateogry to Ingestigate: ",
            choices = uniqueList,
            selected = uniqueList[1]
        )
    })
    
    output$selectedUni_Ind_Cat <- renderUI({
        uniqueList <- unique(DL_ColumnKey$category)
        pickerInput(
            inputId = "selectedUni_Ind_Cat",
            label = "Select A Direct Loan Variable to Investigate: ",
            choices = uniqueList,
            selected = uniqueList[1]
        )
    })
    
    ##### Individual Info Box #####
    output$scorecard <- renderInfoBox({
        req(input$selectedUni_Ind)
        curUni <- input$selectedUni_Ind
        
        infoBox("Link to ", "U.S. Department of Education - College Scorecard", 
                icon = icon("school"), fill = TRUE, color = "maroon", width = 6, # north%20Carolina%20State
                href = paste0("https://collegescorecard.ed.gov/search/?search=", 
                              gsub(" ", "%20", curUni), 
                              "&page=0&sort=completion_rate:desc&toggle=institutions"))
    })
    
    output$googleSearch <- renderInfoBox({
        req(input$selectedUni_Ind)
        curUni <- input$selectedUni_Ind
        
        infoBox("Link to ", HTML("Google Search Link for the Selected University "), 
                icon = icon("google"), fill = TRUE, color = "teal", width = 6, # north%20Carolina%20State
                href = paste0("https://www.google.com/search?q=", 
                              gsub(" ", "+", curUni))
        )
    })
    
    output$topUni <- renderInfoBox({
        req(input$selectedUni_Ind)
        curUni <- input$selectedUni_Ind
        
        infoBox("Link to ", "University Search for the Selected University", 
                icon = icon("globe-americas"), fill = TRUE, color = "green", width = 6, # north%20Carolina%20State
                href = paste0("https://www.topuniversities.com/search/all#q=", 
                              gsub(" ", "%20", curUni)))
    })
    
    
    ##### Individual Bar Line Plot #####
    output$individualQuarterlyPlot <- renderPlotly({
        req(input$selectedUni_Ind)
        curUni <- input$selectedUni_Ind
        curDLCat <- input$selectedUni_Ind_DLCat
        curCat <- input$selectedUni_Ind_Cat
        colors5 <- values[["colors5"]]
        
        curDLCat_Ori <- as.character(subset(DL_ColumnKey, DLCategory == curDLCat & category == curCat)$original)
        print(curDLCat_Ori)
        subData <- allFSAData %>%
            filter(UniversityName == curUni) %>%
            select(UniversityName, one_of(curDLCat_Ori), State, Zip.Code, School.Type, Year, OriQuarter) %>%
            pivot_wider(names_from = Year, values_from = one_of(curDLCat_Ori))
        curState <- subData$State[1]
        stateAverage <- stateAvg %>%
            select(State, OriQuarter, one_of(curDLCat_Ori)) %>%
            filter(State == curState) %>%
            rename(curCol = one_of(curDLCat_Ori))
        nationalAverage <- nationalAvg %>%
            select(OriQuarter, one_of(curDLCat_Ori)) %>%
            rename(curCol = one_of(curDLCat_Ori))
        
        subData %>%
            plot_ly() %>%
            add_trace(
                y = ~`2015`,
                type = ~"bar",
                name = "2015",
                x = ~OriQuarter,
                legendgroup = "group1",
                marker = list(color = colors5[1],
                              line = list(color = darken(colors5[1], 0.3),
                                          width = 2))) %>%
            add_trace(y = ~`2016`, name = "2016",
                      x = ~OriQuarter,
                      legendgroup = "group1",
                      marker = list(color = colors5[2],
                                    line = list(color = darken(colors5[2], 0.3),
                                                width = 2)))%>%
            add_trace(y = ~`2017`, name = "2017",
                      x = ~OriQuarter,
                      legendgroup = "group1",
                      marker = list(color = colors5[3],
                                    line = list(color = darken(colors5[3], 0.3),
                                                width = 2)))%>%
            add_trace(y = ~`2018`, name = "2018",
                      x = ~OriQuarter,
                      legendgroup = "group1",
                      marker = list(color = colors5[4],
                                    line = list(color = darken(colors5[4], 0.3),
                                                width = 2)))%>%
            add_trace(y = ~`2019`, name = "2019",
                      x = ~OriQuarter,
                      legendgroup = "group1",
                      marker = list(color = colors5[5],
                                    line = list(color = darken(colors5[5], 0.3),
                                                width = 2))) %>%
            add_trace(data = nationalAverage, 
                      x = ~OriQuarter,
                      y = ~curCol, 
                      legendgroup = "group2",
                      name = "National Average",
                      type = "scatter",
                      mode = "lines+markers",
                      marker = list(size = 15,
                                    line = list(color = "#000000",
                                                size = 5))) %>%
            add_trace(data = stateAverage, 
                      x = ~OriQuarter,
                      y = ~curCol, 
                      legendgroup = "group2",
                      name = "State Average",
                      type = "scatter",
                      mode = "lines+markers",
                      marker = list(size = 15,
                                    line = list(color = "#000000",
                                                size = 5))) %>%
            layout(
                # title = paste0("Direct Loans: ", curDLCat, " - ", curCat),
                   xaxis = list(title = "<b>Quarter</b>"),
                   yaxis = list(title = paste0(" <b>", curDLCat, " <br> ", curCat, "</b>")))
    })  
    
    ##### Individual Tuition Chart #####
    output$tuitionNetPricePlot <- renderPlotly({
        req(input$tuitionInfoSelector)
        colors5 <- values[["colors5"]]
        curData <- tuitionFull
        curOPE <- allFSAData_Key$OPE.ID[match(input$selectedUni_Ind, allFSAData_Key$UniversityName)]
        curData <- tuitionFull %>%
            filter(OPEID == curOPE)
        if(nrow(curData) > 1){
            if(input$tuitionInfoSelector == "TuitionAndFees"){
                curData <- tuitionFull
                curOPE <- allFSAData_Key$OPE.ID[match(input$selectedUni_Ind, allFSAData_Key$UniversityName)]
                curData <- tuitionFull %>%
                    filter(OPEID == curOPE)
                print(curData)
                plot_ly(curData,
                        x = ~Year,
                        y = ~TuitionAndFees ,
                        type = "scatter",
                        mode = "lines+markers",
                        marker = list(color = colors5[5],
                                      line = list(color = darken(colors5[5], 0.3),
                                                  width = 15))
                ) %>% 
                    layout(
                        xaxis = list(title = "<b>Academic Year</b>"),
                        yaxis = list(title = "<b>Tuition and Fees ($)</b>")
                    )
                
                # curData <- filter()
            } else {
                curOPE <- allFSAData_Key$OPE.ID[match(input$selectedUni_Ind, allFSAData_Key$UniversityName)]
                curData <- netPriceFull %>%
                    filter(OPEID == curOPE)
                print(curData)
                
                if(input$tuitionInfoSelector == "NetPrice"){
                    plot_ly(curData,
                            x = ~Year,
                            y = ~NetPrice,
                            type = "scatter",
                            mode = "lines+markers",
                            marker = list(color = colors5[5],
                                          line = list(color = darken(colors5[5], 0.3),
                                                      width = 15))
                    ) %>% 
                        layout(
                            xaxis = list(title = "<b>Academic Year</b>"),
                            yaxis = list(title = "<b>Net Price ($)</b>")
                        )
                } else {
                    plot_ly(curData,
                            x = ~Year,
                            y = ~PercentReceivingGrantAid,
                            type = "scatter",
                            mode = "lines+markers",
                            marker = list(color = colors5[5],
                                          line = list(color = darken(colors5[5], 0.3),
                                                      width = 15))
                    ) %>% 
                        layout(
                            xaxis = list(title = "<b>Academic Year</b>"),
                            yaxis = list(title = "<b>Percent Receiving Grant Aid (%)</b>")
                        )
                }
            }
        }
       
    })
    
    ##### Linear Model #####
    output$selectedUni_Model_DLCat <- renderUI({
        uniqueList <- unique(DL_ColumnKey$DLCategory)
        pickerInput(
            inputId = "selectedUni_Model_DLCat",
            label = "Direct Loan Cateogry to Ingestigate: ",
            choices = uniqueList,
            selected = uniqueList[1]
        )
    })
    
    output$selectedUni_Model_Cat <- renderUI({
        uniqueList <- unique(DL_ColumnKey$category)
        pickerInput(
            inputId = "selectedUni_Model_Cat",
            label = "Select A Direct Loan Variable to Investigate: ",
            choices = uniqueList,
            selected = uniqueList[1]
        )
    })
    
    output$LinaerModelFitPlot <- renderPlotly({
        req(input$selectedUni_Ind)
        curUni <- input$selectedUni_Ind
        curDLCat <- input$selectedUni_Model_DLCat
        curCat <- input$selectedUni_Model_Cat
        curOPE <- allFSAData_Key$OPE.ID[match(input$selectedUni_Ind, allFSAData_Key$UniversityName)]
        curDLCat_Ori <- as.character(subset(DL_ColumnKey, DLCategory == curDLCat & category == curCat)$original)
        
        modelData <- modeldata1 %>%
            select("OPE.ID", "Year", one_of(curDLCat_Ori)) %>%
            filter(OPE.ID == curOPE)  %>%
            rename(curCol = one_of(curDLCat_Ori))
        modelData$curCol <- as.double(as.character(modelData$curCol))
        modelData$Year <- factor(modelData$Year)
        print(modelData)
        
        ### Linear Trend calculation
        modelOutput <- allresult_r[[curOPE]]
        modelOutput <- modelOutput %>% as_tibble() %>%
            select(one_of(curDLCat_Ori)) 
        curBeta0 <- modelOutput[1,1]
        curBeta1 <- modelOutput[2,1]
        curR2 <- modelOutput[3,1]
        curP <- modelOutput[4,1]
        curSE <- modelOutput[5,1]
            
        curModelTable <- data.frame(
            "Estimate of Rate of Change" = round(as.numeric(curBeta1), 3),
            "Standard Error" = round(as.numeric(curSE), 3),
            "P-Value" = round(as.numeric(curP), 3),
            "R-Squared" = round(as.numeric(curR2), 3)
        )
        colnames(curModelTable) <- c("Estimate of Rate of Change", "Standard Error",
                                     "P Value", "R-Squared")
        print(curModelTable)
        print(modelOutput)
        output$modelTable <- DT::renderDataTable({
            # data.select %>% dplyr::select(-UniqueID)
            curModelTable
        },             
        rownames= FALSE,
        options = list(
            searching = FALSE, info = FALSE, 
            lengthChange = FALSE, bPaginate = FALSE
        )
        )
        yearlyTrend <- data.frame("Year" = as.character(2015:2019),
                                  "curCol" = c(
                                      as.numeric(curBeta0 + curBeta1*2015),
                                      as.numeric(curBeta0 + curBeta1*2016),
                                      as.numeric(curBeta0 + curBeta1*2017),
                                      as.numeric(curBeta0 + curBeta1*2018),
                                      as.numeric(curBeta0 + curBeta1*2019)
                                    )
                                  )

        
        # print(yearlyTrend)
        modelData %>% 
            plot_ly() %>%
            add_trace(
                x = ~Year,
                y = ~curCol,
                mode = "markers",
                type = "scatter",
                marker = list(size = 10,
                              color = 'rgba(255, 182, 193, .9)',
                              line = list(color = 'rgba(152, 0, 0, .8)',
                                          width = 2)),
                name = "Yearly Sum"
            ) %>%
            add_trace(
                data = yearlyTrend,
                x = ~Year,
                y = ~curCol, type = 'scatter', mode = 'lines',
                line = list(color = 'rgb(22, 96, 167)', width = 4),
                name = "Linear Model Fit"
            ) %>%
            layout(
                xaxis = list(title = "<b>Year</b>"),
                yaxis = list(title = paste0(" <b>", curDLCat, " <br> ", curCat, "</b>"))
            )
        
        
    })
    

    ##### Modal #####
    observeEvent(input$individualQuarterlyHelp, {
        # Show a modal when the button is pressed
        showModal(modalDialog(
            title = "Data Description", 
            tags$img(src = "FSA.png", align = "center", width = "400"), 
            HTML("<br><br>
                 <b>Data Source</b>:<br> 
Common Origination and Disbursement (COD) System<br>
<a href='https://studentaid.gov/data-center/student/title-iv'>https://studentaid.gov/data-center/student/title-iv</a><br>
<b>Recipients</b>:<br>
The number of loan recipients for the loan type during the award year for the time period reported on the spreadsheet.  For Subsidized, Unsubsidized, and Graduate PLUS loans, this is a count of student borrowers.  For Parent PLUS loans, this is a count of the students on whose behalf the loan was taken.  Since students can have multiple loan types in the same award year, you cannot sum the recipient counts from the four categories to obtain an accurate count of total recipients for the loan program during that award year. <br>
<b># of Loans Originated</b>:<br>
The number of loans initiated for the loan type during the award year for the time period reported on the spreadsheet.  <br>
<b>$ of Loans Originated</b>:<br>
The dollar amount of the loans initiated for the loan type during the award year for the time period reported on the spreadsheet.  This is the expected total loan amount if the loan is fully disbursed. <br>
<b># of Loans Disbursed</b>:<br>
The number of disbursements made for the loan type during the award year and quarter reported on the spreadsheet.  <br>
<b>$ of Loans Disbursed</b>:<br>
The dollar amount of disbursements made for the loan type during the award year for the time period reported on the spreadsheet.  

                "),
            
            # tags$img(src = "workingOnIt.gif"),
            easyClose = TRUE
        ))
    })
    
    observeEvent(input$individualTuitionHelp, {
        # Show a modal when the button is pressed
        showModal(modalDialog(
            title = "Data Description", 
            tags$img(src = "CAT.png", align = "center", width = "400"), 
            HTML("<br><br><b>Data Source</b>:<br>
            
College Affordability and Transparency Data Files, 2014 - 2019 <br>
 <a href='https://collegecost.ed.gov/affordability'>https://collegecost.ed.gov/affordability</a>

 <br>
<b>Tuition and Fees</b>: <br>
Tuition and required fees for full-time, first-time degree/certificate-seeking students at Title IV institutions are sorted by sector of institution and degree sought after for that academic year. 
For institutions that charge different tuition for in-district, in-state, or out-of-state students, the minimum tuition (in-district) was used. <br>
<b>Net price</b>:<br>
This figure plots average net price of attendance for full-time, first-time degree/certificate-seeking students at Title IV institutions sorted by sector of institution  and degree sought after for that academic 
year 
The Higher Education Act, as amended, defines institutional net price as \"the average yearly price actually charged to first-time, full-time undergraduate students receiving student aid at an institution of 
higher education after deducting such aid.\" 
In IPEDS, average institutional net price is generated by subtracting the average amount of federal, state/local government, or institutional grant and scholarship aid from the total cost of attendance. 
Total cost of attendance is the sum of published tuition and required fees (lower of in-district or in-state for public institutions), books and supplies, and the weighted average for room and board and other 
expenses. <br>
<b>Percent receiving Grant Aid:</b>: <br>
This figure plots the percentage of students receiving grant for full-time, first-time degree/certificate-seeking students at Title IV institutions sorted by sector of institution  and degree sought after for that academic year 

                "),
            
            # tags$img(src = "workingOnIt.gif"),
            easyClose = TRUE
        ))
    })
    
    observeEvent(input$individualTuitionAssumption, {
        # Show a modal when the button is pressed
        showModal(modalDialog(
            title =  "Assumption Description", 
            tags$img(src = "FSA.png", align = "center", width = "400"), 
            HTML("<br><br><b>Data Source</b>:<br> 
Common Origination and Disbursement (COD) System <br>
<a href='https://studentaid.gov/data-center/student/title-iv'>https://studentaid.gov/data-center/student/title-iv</a> <br>
<b>Assumptions</b>:<br>
<b>1. </b>Loans are included in the columns RECIPIENTS, # OF LOANS ORIGINATED, $ OF LOANS ORIGINATED, # OF DISBURSEMENTS, and $ OF DISBURSEMENTS if the period begin date of the loan falls within the award year reported on the spreadsheet and the first disbursement of that loan falls within that quarter.  Disbursements occurring prior to the award year are counted in quarter 1 and disbursements occurring after the end of the award year are counted in quarter 4.  For the Year-To-Date data, all numbers are cumulative.  <br>
<b>2. </b>Disbursements are included in the columns # OF DISBURSEMENTS and $ OF DISBURSEMENTS if the loan was included in the loan origination counts and the dates of the disbursements were less than or equal to the last day of the quarter reported on this spreadsheet.  For fourth quarter reports it is the cumulative number of disbursements as of the date the report is created in order to include any late disbursements.  Disbursements for the second, third, and fourth quarters are cumulative from the beginning of the award year. <br>
<b>3. </b>Consolidation loans are not included in either query. <br>
<b>4. </b>A school appears on the report only if at least one loan originated at that school qualifies f <br>or inclusion on the report. <br>
<b>5. </b>Loans with a current loan status of cancelled are excluded from both queries. <br>
<b>6. </b>Disbursements in history with an amount of zero are not counted in the number of disbursements. <br>

                "),
            
            # tags$img(src = "workingOnIt.gif"),
            easyClose = TRUE
        ))
    })
    
    observeEvent(input$linearModelHelp, {
        # Show a modal when the button is pressed
        showModal(modalDialog(
            title =  "Data Description", 
            tags$img(src = "FSA.png", align = "center", width = "400"), 
            HTML("<br><br><b>Data Source</b>:<br> 
Common Origination and Disbursement (COD) System <br>
<a href='https://studentaid.gov/data-center/student/title-iv'>https://studentaid.gov/data-center/student/title-iv</a> <br>
<b>Linear model :</b><br>
By observing that the trend of the overall pattern from 2015-2016 to 2019-2020 school year is approximately linear,
we use a linear model to capture the change of each loan type overall the year. We reported the OLS estimate of the change rate coefficient, 
the standard error of the estimate, p-value of the estimate, and the R-square. 


                "),

# tags$img(src = "workingOnIt.gif"),
easyClose = TRUE
        ))
    })
})
