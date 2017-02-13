# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

options( shiny.maxRequestSize = 12 * 1024 ^ 2 )
options( java.parameters = "-Xmx4g" )

library(shiny)
library(dplyr)
library(dtplyr)
library(tidyr)
library(mongolite)
library(xlsx)

source("mlabDB-connect.R", echo = FALSE)

shinyServer(function(input, output, session) {
   observeEvent(input$send, {
      mLab$insert(
         data.frame(
            Sys.Date(),
            "User",
            input$name,
            input$surname,
            input$email,
            input$organization,
            input$department
         )
      )
      updateActionButton(session, "send",
                         label = "Successfully Sent!")
   }, ignoreNULL = TRUE)
   
   dataInput <- reactive({
      if (is.null(input$datafile))
         return(NULL)
      withProgress(message = "Processing uploaded data...", value = 0.4, {
         if (input$datafile$type == 'application/x-zip-compressed') {
            fileunz = unzip(input$datafile$datapath)
            untidydata <- read.table(
               fileunz,
               header = input$header,
               sep = input$sep,
               dec = input$dec
            )
         }
         else if (input$datafile$type == 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet') {
            fileunz <- input$datafile$datapath
            untidydata <- data.table(
               read.xlsx2(
                  fileunz,
                  sheetName = "Data"
               )
            )
            incProgress(0.4)
            as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
            datacolnames <- colnames(untidydata)
            untidydata[, (datacolnames) := lapply(.SD, as.numeric.factor), .SDcols = datacolnames]
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
            "File",
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
   dataProcessed <- reactive({
      factor <- 3600 / (input$interval * 60)
      untidydata <- dataInput()
      untidydata$time <- round(untidydata$time * factor) / factor
      untidydata %>% fill(2:length(.)) %>% group_by(time) %>% summarize_each(funs(mean)) %>% arrange(time)
   })
   
   output$filename <- renderText({
      if (!is.null(input$datafile$name))
         paste("Uploaded file name is ", input$datafile$name)
   })
   output$filesize <- renderText({
      if (!is.null(input$datafile$size))
         paste("Uploaded file size is ",
               round(input$datafile$size / 1024),
               " kB")
   })
   output$inputdim <- renderText({
      if (!is.null(input$datafile$size))
         paste(
            "Uploaded table dimensions are ",
            dim(dataInput())[1],
            " x ",
            dim(dataInput())[2],
            " (rows x cols)"
         )
   })
   output$processeddim <- renderText({
      if (!is.null(input$datafile$size))
         paste(
            "Processed table dimensions are ",
            dim(dataProcessed())[1],
            " x ",
            dim(dataProcessed())[2],
            " (rows x cols)"
         )
   })
   output$downloadData <- downloadHandler(
      # This function returns a string which tells the client
      # browser what name to use when saving the file.
      filename = function() {
         if (input$datafile$type == 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet')
            paste(input$datafile$name)
         else {
            filetype <-
               switch(input$sep,
                      ";" = "csv",
                      "," = "csv",
                      "\t" = "tsv")
            paste("TidyData", filetype, sep = ".")
         }
      }
      ,
      # This function should write data to a file given to it by
      # the argument 'file'.
      content = function(file) {
         # Write to a file specified by the 'file' argument
         if (input$datafile$type == 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet') {
            withProgress(message = "Writing processed data...", value = 0.4, {
               write.xlsx2(
                  data.frame(dataProcessed()),
                  input$datafile$datapath,
                  sheetName = "TidyData",
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
})