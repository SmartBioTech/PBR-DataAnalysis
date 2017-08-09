# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

options(shiny.maxRequestSize = 12 * 1024 ^ 2)

library(dplyr)
library(dtplyr)
library(DT)
library(openxlsx)
library(shiny)
library(shinyBS)
library(RMySQL)
library(tidyr)

source('DB-connect.R', echo = FALSE)

dataColumnNames <- readRDS('data/dataColumnNames.rds')

server <- function(input, output, session) {
   rangesView <- reactiveValues(x = NULL, y = NULL)
   rangesProcessing <- reactiveValues(x = NULL, y = NULL)
   observeEvent(input$send, {
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
      } else {
         rangesView$x <- NULL
         rangesView$y <- NULL
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
# File input ====
   dataInput <- reactive({
      if (is.null(input$dataFile) || (input$dataFile$type != 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet')) {
         return(NULL)
      }
      withProgress(message = "Processing uploaded data...", value = 0.4, {
         untidyData <- readWorkbook(input$dataFile$datapath, sheet = 'Data')
         incProgress(0.4)
      })
      return(untidyData)
   })
# Data processing ====
   dataProcessed <- reactive({
      untidyData <- dataInput()
      if (is.null(untidyData)) {
         return(NULL)
      }
      selectChoices <- merge(data.frame(X = colnames(untidyData)), dataColumnNames)
      updateSelectInput(session, 'dataColumn', choices = selectChoices$Y, selected = 'OD680, AU')
      factor <- 3600 / (input$interval * 60)
      untidyData$time <- round(untidyData$time * factor) / factor
      untidyData %>%
         fill(2:length(.)) %>%
         group_by(time) %>%
         summarize_all(mean) %>%
         arrange(time)
   })
# Processed file download ====
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
                  addWorksheet(pbrDataFile, 'TidyData')
                  writeData(pbrDataFile, 'TidyData', dataProcessed())
                  saveWorkbook(pbrDataFile, input$dataFile$datapath, overwrite = TRUE)
                  incProgress(0.4)
                  file.copy(input$dataFile$datapath, file)
               }
            )
         }
      }
   )
# Analysis file download ====
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
                  addWorksheet(pbrDataFile, 'TidyData')
                  writeData(pbrDataFile, 'TidyData', dataProcessed())
                  addWorksheet(pbrDataFile, 'Analysis')
                  writeData(pbrDataFile, 'Analysis', growthRates())
                  saveWorkbook(pbrDataFile, input$dataFile$datapath, overwrite = TRUE)
                  incProgress(0.4)
                  file.copy(input$dataFile$datapath, file)
               }
            )
         }
      }
   )
# Growth Rates calculataion ====
   growthRates <- reactive({
      withProgress(
         message = "Calculating growth rates...",
         value = 0.0,
         {
            data <- dataProcessed()
            pumpOn <- which(data$`pumps.pump-5` > 0)
            expFitStart <- c()
            expFitStop <- c()
            time <- c()
            Dt <- c()
            R2 <- c()
            for (i in 2:(length(pumpOn) - 1)) {
               if (data$`pumps.pump-5`[pumpOn[i] - 1] == 0) expFitStop <- c(expFitStop, pumpOn[i])
               else if (data$`pumps.pump-5`[pumpOn[i] + 1] == 0) expFitStart <- c(expFitStart, pumpOn[i] + 1)
            }
            incProgress(0.15)
            for (j in 1:length(expFitStart)) {
               # interval <- c((expFitStop[j] - ceiling(expFitStop[j] - expFitStart[j]) * (1 - input$lagTime)):expFitStop[j])
               interval <- c((expFitStart[j] + ceiling(input$lagTime/input$interval)):expFitStop[j])
               timeFit <- data$time[interval]
               dataFit <- data$`od-sensors.od-680`[interval]
               fit <- nls(dataFit ~ exp(a + b * timeFit),
                          start = list(a = 0, b = 0.5),
                          control = list(maxiter = 99, warnOnly = TRUE))
               time <- c(time, timeFit[length(timeFit)])
               R2 <- c(R2, cor(dataFit, predict(fit)))
               Dt <- c(Dt, 1 / coef(fit)[2] * log(2))
               incProgress(0.15 + (j / length(expFitStart)) * 0.85)
            }
         })
      return(data.frame(time, Dt, R2))
   })
# UI outputs hadling ====
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
            " (rows x cols)"
         )
   })
   output$dataViewPlot <- renderPlot({
      data <- dataProcessed()
      if (!is.null(data)) {
         plot(x = data$time, y = data[[dataColumnNames$X[match(input$dataColumn, dataColumnNames$Y)]]], xlim = rangesView$x, ylim = rangesView$y, xlab = 'Experiment duration, h', ylab = 'Optical density, AU')
      }
   })
   
   # https://rstudio.github.io/DT/options.html
   # https://datatables.net/reference/option/dom
   output$dataProcessingTable <- DT::renderDataTable({
      if (!is.null(dataProcessed()))
         datatable(growthRates(),
                  options = list(dom = 'tlp', pageLength = 8, lengthChange = FALSE, searching = FALSE)) %>%
                  formatRound(c('time','Dt','R2'), digits = 2)
      },
      server = FALSE
   )
   output$dataProcessingPlot <- renderPlot({
      if (!is.null(dataProcessed())) {
         s1 = NULL
         s2 = input$dataProcessingTable_rows_selected
         gRates <- growthRates()
         plot(x = gRates$time, y = gRates$Dt, xlim = rangesProcessing$x, ylim = rangesProcessing$y, xlab = "Experiment duration, h", ylab = "Doubling time, h")
         if (length(s1)) {
            points(gRates[s1, , drop = FALSE], pch = 19, cex = 1, col = 'green')
         }
         if (length(s2)) {
            points(gRates[s2, , drop = FALSE], pch = 19, cex = 1.25)
         }
      }
   })
}

# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

ui <- fluidPage(
   tags$head(includeScript('google-analytics.js')),
   titlePanel("", windowTitle = "PBR Data Analysis"),
   sidebarLayout(
      # Sidebar panel ====
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
                  value = 1, min = 0.5, max = 10, step = 0.1
               ),
               # numericInput('interval', 'Interval, min', 1, min = 0.5, max = 10, step = 0.1, width = '100px'),
               bsTooltip(
                  'interval',
                  "Interval that is used for lumping and averaging untidy data. Provided in minutes.",
                  'right', options = list(container = 'body')
               )
            ),
            fluidRow(
               downloadButton('downloadData', "Download")
            ),
            tags$hr(),
            fluidRow(
               selectInput('dataColumn', "Data to View", "OD680, AU")
            ),
            tags$hr()
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
            fluidRow(
               downloadButton('downloadAnalysis', "Download")
            )
         ),
         width = 3
      ),
      # Main panel ====
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
   # Bottom panel ====
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
