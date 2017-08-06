# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

options( shiny.maxRequestSize = 12 * 1024 ^ 2 )
options( java.parameters = "-Xmx4g" )

library(data.table)
library(DT)
library(dplyr)
library(dtplyr)
library(mongolite)
library(shiny)
library(shinyBS)
library(tidyr)
library(xlsx)

source("mlabDB-connect.R", echo = FALSE)

shinyServer(function(input, output, session) {
   observeEvent(input$send, {
      mLab$insert(
         data.frame(
            Sys.Date(),
            'User',
            input$name,
            input$surname,
            input$email,
            input$organization,
            input$department
         )
      )
      updateActionButton(
         session, 'send',
         label = 'Successfully Sent!'
      )
   },
   ignoreNULL = TRUE
   )
# File input ====
   dataInput <- reactive({
      if (is.null(input$datafile))
         return(NULL)
      withProgress(message = 'Processing uploaded data...', value = 0.4, {
         if (input$datafile$type == 'application/x-zip-compressed') {
            fileunz = unzip(input$datafile$datapath)
            untidydata <- read.table(
               fileunz,
               header = input$header,
               sep = input$sep,
               dec = input$dec
            )
         }
         else if (
            input$datafile$type ==
               'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet') {
            fileunz <- input$datafile$datapath
            untidydata <- data.table(
               read.xlsx2(
                  fileunz,
                  sheetName = 'Data'
               )
            )
            incProgress(0.4)
            as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
            datacolnames <- colnames(untidydata)
            untidydata[, (datacolnames) := lapply(.SD, as.numeric.factor), 
                       .SDcols = datacolnames]
         }
         else {
            fileunz <- input$datafile$datapath
            untidydata <- read.table(
               fileunz,
               header = input$header,
               sep = input$sep,
               dec = input$dec
            )
         }
      })
      mLab$insert(
         data.frame(
            Sys.Date(),
            'File',
            input$datafile$name,
            input$datafile$type,
            input$datafile$size,
            input$datafile$datapath
         )
      )
      if (input$datafile$type == 'application/x-zip-compressed')
         file.remove(fileunz)
      return(untidydata)
   })
# Data processing ====
   dataProcessed <- reactive({
      factor <- 3600 / (input$interval * 60)
      untidydata <- dataInput()
      if (is.null(untidydata)) return(NULL)
      untidydata$time <- round(untidydata$time * factor) / factor
      as.data.frame.list(untidydata) %>% fill(2:length(.)) %>% group_by(time) %>% 
         summarize_all(funs(mean)) %>% arrange(time)
   })
   
# Processed file download ====
   output$downloadData <- downloadHandler(
      # This function returns a string which tells the client
      # browser what name to use when saving the file.
      filename = function() {
         if (input$datafile$type == 
               'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet')
            paste(input$datafile$name)
         else {
            filetype <-
               switch(input$sep,
                      ";" = "csv",
                      "," = "csv",
                      "\t" = "tsv")
            paste('TidyData', filetype, sep = '.')
         }
      }
      ,
      # This function should write data to a file given to it by
      # the argument 'file'.
      content = function(file) {
         # Write to a file specified by the 'file' argument
         if (input$datafile$type ==
               'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet') {
            withProgress(
               message = 'Writing processed data...',
               value = 0.4,
               {
                  write.xlsx2(
                     data.frame(dataProcessed()),
                     input$datafile$datapath,
                     sheetName = 'TidyData',
                     row.names = FALSE,
                     append = TRUE,
                     showNA = TRUE
                  )
                  incProgress(0.4)
                  file.copy(input$datafile$datapath, file)
               })
         }
         else {
            write.table(
               dataProcessed(),
               file = file,
               sep = input$sep,
               dec = input$dec,
               row.names = FALSE
            )
         }
      }
   )
# Growth Rates calculataion ====
   growthRates <- reactive({
      withProgress(
         message = 'Calculating growth rates...',
         value = 0.0,
         {
            data <- data.frame(dataProcessed())
            pumpon <- which(data[, 'pumps.pump.5'] > 0)
            expfitstart <- c()
            expfitstop <- c()
            time <- c()
            TD <- c()
            R2 <- c()
            for (i in 2:(length(pumpon) - 1)) {
               if (data[pumpon[i] - 1, 'pumps.pump.5'] == 0) expfitstop <- c(expfitstop, pumpon[i])
               else if (data[pumpon[i] + 1, 'pumps.pump.5'] == 0) expfitstart <- c(expfitstart, pumpon[i] + 1)
            }
            incProgress(0.15)
            for (j in 1:length(expfitstart)) {
               # interval <- c((expfitstop[j] - ceiling(expfitstop[j] - expfitstart[j]) * (1 - input$lag)):expfitstop[j])
               interval <- c((expfitstart[j] + ceiling(input$lag/input$interval)):expfitstop[j])
               timefit <- data[interval, 'time']
               datafit <- data[interval, 'od.sensors.od.680']
               fit <- nls(datafit ~ exp(a + b * timefit),
                          start = list(a = 0, b = 0.5),
                          control = list(maxiter = 99, warnOnly = TRUE))
               time <- c(time, timefit[length(timefit)])
               R2 <- c(R2, cor(datafit,predict(fit)))
               TD <- c(TD, 1/coef(fit)[2]*log(2))
               incProgress(0.15 + (j/length(expfitstart))*0.85)
            }
         })
      return(data.frame(time, TD, R2))
   })
# UI outputs hadling ====
   output$filename <- renderText({
      if (!is.null(input$datafile$name))
         paste('Uploaded file name is ', input$datafile$name)
   })
   output$filesize <- renderText({
      if (!is.null(input$datafile$size))
         paste('Uploaded file size is ',
               round(input$datafile$size / 1024),
               ' kB')
   })
   output$inputdim <- renderText({
      if (!is.null(input$datafile$size))
         paste(
            'Uploaded/processed table dimensions are ',
            dim(dataInput())[1],
            '/',
            dim(dataProcessed())[1],
            ' x ',
            dim(dataInput())[2],
            '/',
            dim(dataProcessed())[2],
            ' (rows x cols)'
         )
   })
   output$dataViewPlot <- renderPlot({
      if (!is.null(dataProcessed()))
         plot(x = dataProcessed()$time, y = dataProcessed()$od.sensors.od.680, xlab = 'Experiment duration, h', ylab = 'Optical density, AU')
   })
   # https://rstudio.github.io/DT/options.html
   # https://datatables.net/reference/option/dom
   output$dataProcessingTable <- DT::renderDataTable({
      if (!is.null(dataProcessed()))
         datatable(growthRates(), options = list(dom = 'tlp', pageLength = 8, lengthChange = FALSE, seraching = FALSE)) %>% formatRound(
            c('time','TD','R2'),
            digits = 2
            )},
      server = FALSE
   )
   output$dataProcessingPlot <- renderPlot({
      if (!is.null(dataProcessed())) {
         s1 = NULL
         s2 = input$dataProcessingTable_rows_selected
         plot(x = growthRates()$time, y = growthRates()$TD, xlab = 'Experiment duration, h', ylab = 'Doubling time, h')
         if (length(s1)) {
            points(growthRates()[s1, , drop = FALSE], pch = 19, cex = 1, col = 'green')
         }
         if (length(s2)) {
            points(growthRates()[s2, , drop = FALSE], pch = 19, cex = 1.25)
         }
      }
   })
})
