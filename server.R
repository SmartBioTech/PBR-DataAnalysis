


# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(dplyr)
library(tidyr)
library(mongolite)

options(shiny.maxRequestSize = 10 * 1024 ^ 2)

#url <- "mongodb://SAServer:DoAB@ds048279.mlab.com:48279/shinyapps"
#collection <- "TidyUpPBRData"
#mLab <- mongo(collection, url = url)

shinyServer(function(input, output, session) {
   observeEvent(input$send, {
      # mLab$insert(
      #    data.frame(
      #       Sys.Date(),
      #       "User",
      #       input$name,
      #       input$surname,
      #       input$email,
      #       input$organization,
      #       input$department
      #    )
      # )
      updateActionButton(session, "send",
                         label = "Successfully Sent!")
   }, ignoreNULL = TRUE)
   
   dataInput <- reactive({
      inFile <- input$file1
      if (is.null(inFile))
         return(NULL)
      if (inFile$type == 'application/x-zip-compressed')
         fileunz = unzip(inFile$datapath)
      else
         fileunz <- inFile$datapath
         untidydata <- read.table(
         fileunz,
         header = input$header,
         sep = input$sep,
         dec = input$dec
      )
      # mLab$insert(
      #    data.frame(
      #       Sys.Date(),
      #       "File",
      #       inFile$name,
      #       inFile$type,
      #       inFile$size,
      #       inFile$datapath
      #    )
      # )
      if (inFile$type == 'application/x-zip-compressed')
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
      if (!is.null(input$file1$name))
         paste("Uploaded file name is ", input$file1$name)
   })
   output$filesize <- renderText({
      if (!is.null(input$file1$size))
         paste("Uploaded file size is ",
               round(input$file1$size / 1024),
               " kB")
   })
   output$inputdim <- renderText({
      if (!is.null(input$file1$size))
         paste(
            "Uploaded table dimensions are ",
            dim(dataInput())[1],
            " x ",
            dim(dataInput())[2],
            " (rows x cols)"
         )
   })
   output$processeddim <- renderText({
      if (!is.null(input$file1$size))
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
         filetype <-
            switch(input$sep,
                   ";" = "csv",
                   "," = "csv",
                   "\t" = "tsv")
         paste("TidyData", filetype, sep = ".")
      },
      
      # This function should write data to a file given to it by
      # the argument 'file'.
      content = function(file) {
         # Write to a file specified by the 'file' argument
         write.table(
            dataProcessed(),
            file = file,
            sep = input$sep,
            dec = input$dec,
            row.names = FALSE
         )
      }
   )
})