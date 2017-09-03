# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

options(shiny.maxRequestSize = 12 * 1024 ^ 2)

library(dplyr)
library(DT)
library(openxlsx)
library(plotrix)
library(shiny)
library(shinyBS)
library(RMySQL)
library(tidyr)

dataColumnNames <- readRDS('data/dataColumnNames.rds')
# SERVER Part ====
server <- function(input, output, session) {
   rangesView <- reactiveValues(x = NULL, y = NULL)
   rangesProcessing <- reactiveValues(x = NULL, y = NULL)
   observeEvent(input$regSend, {
      source('dbRegister.R', local = TRUE, echo = FALSE)
      updateActionButton(
         session, 'regSend',
         label = "Successfully Sent!"
      )
   },
   ignoreNULL = TRUE
   )
   observeEvent(input$dataViewPlot_dblClick, {
      brush <- input$dataViewPlot_brush
      if (!is.null(brush)) {
         rangesView$x <- c(brush$xmin, brush$xmax)
         rangesView$y <- c(brush$ymin, brush$ymax)
         output$downloadSize <- renderText({
            paste0("Data time range reduced to: ", floor(rangesView$x[1]), " to ", ceiling(rangesView$x[2]), " h")
         })
      } else {
         rangesView$x <- NULL
         rangesView$y <- NULL
         output$downloadSize <- renderText({
            paste0("")
         })
      }
   })
   observeEvent(input$dataProcessingPlot_dblClick, {
      brush <- input$dataProcessingPlot_brush
      if (!is.null(brush)) {
         rangesProcessing$x <- c(brush$xmin, brush$xmax)
         rangesProcessing$y <- c(brush$ymin, brush$ymax)
      } else {
         rangesProcessing$x <- NULL
         rangesProcessing$y <- NULL
      }
   })
   # File processing ====
   # Upload
   dataInput <- reactive({
      if (is.null(input$dataFile) || (input$dataFile$type !=
                                      'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet')) {
         return(NULL)
      }
      withProgress(
         message = "Processing uploaded data...",
         value = 0.4,
         {
            untidyData <- readWorkbook(input$dataFile$datapath, sheet = 'Data')
            incProgress(0.4)
         }
      )
      return(untidyData)
   })
   # Download - TidyData
   output$downloadData <- downloadHandler(
      # This function returns a string which tells the client
      # browser what name to use when saving the file.
      filename = function() {
         if (input$dataFile$type == 
             'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet') {
            paste(input$dataFile$name)
         }
      }
      ,
      # This function should write data to a file given to it by
      # the argument 'file'.
      content = function(file) {
         # Write to a file specified by the 'file' argument
         if (input$dataFile$type ==
             'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet') {
            withProgress(
               message = "Writing processed data...",
               value = 0.4,
               {
                  pbrDataFile  <- loadWorkbook(file = input$dataFile$datapath)
                  if (is.na(match('TidyData', getSheetNames(file = input$dataFile$datapath)))) {
                     addWorksheet(pbrDataFile, 'TidyData')   
                  }
                  if (!is.null(rangesView$x)) {
                     tidyData <- dataProcessed() %>% filter(.$time > floor(rangesView$x[1])) %>% filter(.$time < ceiling(rangesView$x[2]))
                  } else {
                     tidyData <- dataProcessed()
                  }
                  writeData(pbrDataFile, 'TidyData', tidyData)
                  saveWorkbook(pbrDataFile, paste0(input$dataFile$datapath, '-tempDW'), overwrite = TRUE)
                  incProgress(0.4)
                  file.copy(paste0(input$dataFile$datapath, '-tempDW'), file)
               }
            )
         }
      }
   )
   # Download - Analysis
   output$downloadAnalysis <- downloadHandler(
      # This function returns a string which tells the client
      # browser what name to use when saving the file.
      filename = function() {
         paste(input$dataFile$name)
      }
      ,
      # This function should write data to a file given to it by
      # the argument 'file'.
      content = function(file) {
         # Write to a file specified by the 'file' argument
         if (input$dataFile$type ==
             'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet') {
            withProgress(
               message = "Writing processed data...",
               value = 0.4,
               {
                  pbrDataFile  <- loadWorkbook(file = input$dataFile$datapath)
                  if (is.na(match('TidyData', getSheetNames(file = input$dataFile$datapath)))) {
                     addWorksheet(pbrDataFile, 'TidyData')   
                  }
                  writeData(pbrDataFile, 'TidyData', dataProcessed())
                  if (is.na(match('Analysis', getSheetNames(file = input$dataFile$datapath)))) {
                     addWorksheet(pbrDataFile, 'Analysis')
                  }
                  writeData(pbrDataFile, 'Analysis', growthRates())
                  saveWorkbook(pbrDataFile, paste0(input$dataFile$datapath, '-tempDA'), overwrite = TRUE)
                  incProgress(0.4)
                  file.copy(paste0(input$dataFile$datapath, '-tempDA'), file)
               }
            )
         }
      }
   )
   # Data processing ====
   dataProcessed <- reactive({
      # Read data and remove empty columns
      untidyData <- Filter(function(x)!all(is.na(x)), dataInput())
      if (is.null(untidyData)) {
         return(NULL)
      }
      selectChoices <- merge(data.frame(X = colnames(untidyData)), dataColumnNames) 
      if(input$selectDataView == "") {
         selected <- "OD680, AU"
      } else {
         selected = input$selectDataView
      }
      if(input$selectDataView2 == "" || input$selectDataView2 == "-") {
         selected2 <- "-"
      } else {
         selected2 = input$selectDataView2
      }
      updateSelectInput(session, 'selectDataView', choices = selectChoices$Y, selected = selected)
      updateSelectInput(session, 'selectDataView2', choices = c("-", selectChoices$Y), selected = selected2)
      if (input$interval > 0) {
         factor <- 3600 / (input$interval * 60)
         untidyData$time <- round(untidyData$time * factor) / factor
         untidyData %>%
            fill(eval(2 : ncol(.))) %>%
            group_by(time) %>%
            summarize_all(mean) %>%
            arrange(time)
      } else {
         untidyData %>% fill(eval(2 : ncol(.)))
      }
   })
   # Growth rates calculation
   growthRates <- reactive({
      data <- dataProcessed()
      pumps <- grep('pumps.pump-.$', colnames(data), value = TRUE)
      if (length(pumps) < 1) {
         return(data.frame(time = c(0), mu = c(0), R2 = c(0), Dt = c(0)))
      }
      columnName <- dataColumnNames$X[match(input$selectDilutionPump, dataColumnNames$Y)]
      if(is.null(data[[columnName]]) || (sum(data[[columnName]], na.rm = TRUE) == 0)) {
         return(data.frame(time = c(0), mu = c(0), R2 = c(0), Dt = c(0)))
      }
      withProgress(
         message = "Calculating growth rates...",
         value = 0.0,
         {
            pumpOn <- which(data[[columnName]] > 0)
            expFitStart <- c()
            expFitStop <- c()
            time <- c()
            mu <- c()
            R2 <- c()
            Dt <- c()
            for (i in 2:(length(pumpOn) - 1)) {
               if (data[[columnName]][pumpOn[i] - 1] == 0) expFitStop <- c(expFitStop, pumpOn[i])
               else if (data[[columnName]][pumpOn[i] + 1] == 0) expFitStart <- c(expFitStart, pumpOn[i] + 1)
            }
            incProgress(0.15)
            for (j in 1:length(expFitStart)) {
               interval <- c((expFitStart[j] + floor(input$lagTime/input$interval)):expFitStop[j])
               incProgress(0.15 + (j / length(expFitStart)) * 0.85)
               if (length(interval) < 4) {
                  next
               }
               timeFit <- data$time[interval]
               dataFit <- data[[dataColumnNames$X[match(input$selectGrowthRatesData, dataColumnNames$Y)]]][interval]
               fit <- nls(dataFit ~ exp(a + b * timeFit),
                          start = list(a = 0, b = 0.1),
                          control = list(maxiter = 99, minFactor = 1/2048, warnOnly = TRUE))
               time <- c(time, timeFit[length(timeFit)])
               mu <- c(mu, coef(fit)[2] * 24)
               R2 <- c(R2, cor(dataFit, predict(fit)))
               Dt <- c(Dt, 1 / coef(fit)[2] * log(2))
            }
         })
      return(data.frame(time, mu, R2, Dt))
   })
   # Outputs hadling ====
   output$fileName <- renderText({
      if (!is.null(input$dataFile$name))
         paste("Uploaded file name is ", input$dataFile$name)
   })
   output$fileSize <- renderText({
      if (!is.null(input$dataFile$size))
         paste("Uploaded file size is ",
               round(input$dataFile$size / 1024),
               " kB")
   })
   output$dataDim <- renderText({
      if (!is.null(input$dataFile$size))
         paste(
            "Uploaded/processed table dimensions are ",
            dim(dataInput())[1],
            "/",
            dim(dataProcessed())[1],
            " x ",
            dim(dataInput())[2],
            "/",
            dim(dataProcessed())[2],
            " (rows x cols)"
         )
   })
   output$dataViewPlot <- renderPlot({
      data <- dataProcessed()
      if (!is.null(data)) {
         if(input$selectDataView2 == "" || input$selectDataView2 == "-") {
            plot(x = data$time,
                 y = data[[dataColumnNames$X[match(input$selectDataView, dataColumnNames$Y)]]],
                 xlim = rangesView$x,
                 ylim = rangesView$y,
                 xlab = 'Experiment duration, h',
                 ylab = input$selectDataView)
         } else {
            if (!is.null(rangesView$y)) {
               ymin = min(data[[dataColumnNames$X[match(input$selectDataView, dataColumnNames$Y)]]], na.rm = TRUE)
               ymax = max(data[[dataColumnNames$X[match(input$selectDataView, dataColumnNames$Y)]]], na.rm = TRUE)
               y2min = min(data[[dataColumnNames$X[match(input$selectDataView2, dataColumnNames$Y)]]], na.rm = TRUE)
               y2max = max(data[[dataColumnNames$X[match(input$selectDataView2, dataColumnNames$Y)]]], na.rm = TRUE)
               y2lim <- c((rangesView$y[1] - ymin) / (ymax - ymin) * (y2max - y2min) + y2min, y2max - (ymax - rangesView$y[2]) / (ymax - ymin) * (y2max - y2min))
            } else {
               y2lim <- NULL
            }
            twoord.plot(lx = data$time,
                        ly = data[[dataColumnNames$X[match(input$selectDataView, dataColumnNames$Y)]]],
                        rx = data$time,
                        ry = data[[dataColumnNames$X[match(input$selectDataView2, dataColumnNames$Y)]]],
                        xlim = rangesView$x,
                        lylim = rangesView$y,
                        rylim = y2lim,
                        xlab = 'Experiment duration, h',
                        ylab = input$selectDataView,
                        rylab = input$selectDataView2,
                        rcol = 3,
                        rpch = 1)
         }
      }
   })
   # https://rstudio.github.io/DT/options.html
   # https://datatables.net/reference/option/dom
   output$dataProcessingTable <- DT::renderDataTable({
      if (!is.null(dataProcessed()))
         datatable(growthRates(),
                   options = list(dom = 'tlp', pageLength = 8, lengthChange = FALSE, searching = FALSE)) %>%
         formatRound(c('time', 'mu', 'R2', 'Dt'), digits = 2)
   },
      server = FALSE
   )
   output$dataProcessingPlot <- renderPlot({
      if (!is.null(dataProcessed())) {
         s1 = NULL
         s2 = input$dataProcessingTable_rows_selected
         gRates <- growthRates()
         # plot(x = gRates$time, y = gRates$Dt, xlim = rangesProcessing$x, ylim = rangesProcessing$y, xlab = "Experiment duration, h", ylab = "Doubling time, h")
         twoord.plot(lx = gRates$time,
                     ly = gRates$Dt,
                     rx = gRates$time,
                     ry = gRates$mu,
                     xlim = rangesProcessing$x,
                     lylim = rangesProcessing$y,
                     # rylim = y2lim,
                     xlab = 'Experiment duration, h',
                     ylab = 'Doubling time, h',
                     rylab = 'Specific growth rate, 1/day',
                     rcol = 3,
                     rpch = 1)
         if (length(s1)) {
            points(gRates[s1, , drop = FALSE], pch = 19, cex = 1, col = 'green')
         }
         if (length(s2)) {
            points(gRates$time[s2], gRates$Dt[s2], pch = 19, cex = 1.25)
         }
      }
   })
}

# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
# UI Layout ====
ui <- fluidPage(
   tags$head(includeScript('google-analytics.js')),
   titlePanel("", windowTitle = "PBR Data Analysis"),
   sidebarLayout(
      # UI - Sidebar panel ====
      sidebarPanel(
         conditionalPanel(
            condition = 'input.conditionedSidePanels == 1',
            fluidRow(
               fileInput(
                  'dataFile',
                  "Choose data file to upload",
                  accept = c(
                     'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                     '.xlsx'
                  )
               )
            ),
            fluidRow(
               sliderInput(
                  'interval',
                  "Averaging interval, min", 
                  value = 1, min = 0.0, max = 60, step = 0.1
               ),
               bsTooltip(
                  'interval',
                  "Interval that is used for lumping and averaging untidy data. Provided in minutes.",
                  'right', options = list(container = 'body')
               )
            ),
            tags$hr(),
            fluidRow(
               selectInput('selectDataView', "Data to view", "OD680, AU")
            ),
            fluidRow(
               selectInput('selectDataView2', "Additional data to view", "-")
            ),
            tags$hr(),
            fluidRow(
               downloadButton('downloadData', "Download"),
               textOutput('downloadSize')
            )
         ),
         conditionalPanel(
            condition = 'input.conditionedSidePanels == 2',
            fluidRow(
               sliderInput(
                  'lagTime',
                  "Lag time, min", 
                  value = 5, min = 0, max = 30, step = 1
               ),
               bsTooltip(
                  'lagTime',
                  "Length of lag time that defines part of data that are influenced by the dilution. Provided in minutes.",
                  'right',
                  options = list(container = 'body')
               )
            ),
            tags$hr(),
            fluidRow(
               selectInput('selectGrowthRatesData', "Data for growth calculation", c("OD680, AU", "OD720, AU"), "OD680, AU")
            ),
            fluidRow(
               selectInput('selectDilutionPump', "Pump used for dilutions", c("Pump 0", "Pump 3", "Pump 4", "Pump 5", "Pump 6", "Pump 7"), "Pump 0")
            ),
            tags$hr(),
            fluidRow(
               downloadButton('downloadAnalysis', "Download")
            )
         ),
         width = 3
      ),
      # UI - Main panel ====
      mainPanel(
         tabsetPanel(
            type = 'tabs',
            tabPanel(
               "Data Processing",
               value = 1,
               strong(textOutput('count')),
               br(),
               strong(textOutput('fileName')),
               em(textOutput('fileSize')),
               br(),
               textOutput('dataDim'),
               plotOutput('dataViewPlot',
                          width = '90%',
                          dblclick = 'dataViewPlot_dblClick',
                          brush = brushOpts(
                             id = 'dataViewPlot_brush',
                             resetOnNew = TRUE
                          )
               )
            ), 
            tabPanel(
               "Data Analysis",
               value = 2,
               fluidRow(
                  column(
                     4,
                     br(),
                     DT::dataTableOutput('dataProcessingTable')
                  ),
                  column(
                     8,
                     plotOutput('dataProcessingPlot',   
                                dblclick = 'dataProcessingPlot_dblClick',
                                brush = brushOpts(
                                   id = 'dataProcessingPlot_brush',
                                   resetOnNew = TRUE
                                )
                     )
                  )
               )
            ),
            id = 'conditionedSidePanels'
         ),
         width = 9
      )
   ),
      # UI - Bottom panel ====
   fluidRow(
      column(
         width = 3,
         actionLink('register', label = "Register to support further development"),
         tags$p(),
         conditionalPanel(
            'input.register',
            textInput('regName', label = "First name", value = "First name"),
            textInput('regSurname', label = "Last name", value = "Last name"),
            textInput('regEmail', label = "Email", value = "@"),
            textInput('regOrganization', label = "Organization", value = "Organization"),
            textInput('regDepartment', label = "Department", value = "Department"),
            actionButton('regSend', label = "Send"),
            tags$hr(),
            p(
               "@author CzechGlobe - Department of Adaptive Biotechnologies (JaCe)"
            ),
            p(
               "@email cerveny.j@czechglobe.cz"
            )
         )
      ),
      column(
         width = 7,
         actionLink('help', label = "Help"),
         conditionalPanel(
            'input.help',
            p(
               "This tool was developed to simplify manipulation with untidy data generated by Photon Systems Instruments (PSI) photobioreactor sofware. The data uploaded to this tool are expected in Excel (.xlsx) format."
            ),
            p(
               "For PSI photobioreactor software exported data, the most simple untidy data preparation procedure is to export data from the software in .ods format and then save (as) the file in .xlsx Excel format."
            )
         ),
         offset = 1
      )
   ),
   fluidRow(
      column(
         2,
         tags$img(src = 'img/Logo-CzechGlobe.jpg', alt = "CzechGlobe", height = 60, align = 'top')
      ),
      column(
         1,
         tags$img(src = 'img/Logo-C4Sys.jpg', alt = "C4Sys", height = 45, align = 'right')
      )
   )
)
shinyApp(ui = ui, server = server)
