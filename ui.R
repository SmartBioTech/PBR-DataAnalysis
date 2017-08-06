# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(data.table)
library(DT)
library(dplyr)
library(dtplyr)
library(mongolite)
library(shiny)
library(shinyBS)
library(tidyr)
library(xlsx)

shinyUI(fluidPage(
   tags$head(includeScript('google-analytics.js')),
   titlePanel('', windowTitle = 'Tidy Up Data'),
   sidebarLayout(
      # Sidebar panel ====
      sidebarPanel(
         conditionalPanel(
            condition = 'input.conditionedSidePanels==1',
            fluidRow(
               fileInput(
                  'datafile',
                  'Choose data file to upload',
                  accept = c(
                     'application/x-zip-compressed',
                     'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                     'text/csv',
                     'text/comma-separated-values',
                     'text/tab-separated-values',
                     'text/plain',
                     '.xlsx',
                     '.csv',
                     '.tsv'
                  )
               )
            ),
            fluidRow(
               sliderInput(
                  'interval',
                  'Averaging interval, min', 
                  value = 1, min = 0.5, max = 10, step = 0.1
               ),
               # numericInput('interval', 'Interval, min', 1, min = 0.5, max = 10, step = 0.1, width = '100px'),
               bsTooltip(
                  'interval',
                  'Interval that is used for lumping and averaging untidy data. Provided in minutes.',
                  'right', options = list(container = 'body')
               )
            ),
            fluidRow(
               downloadButton('downloadData', 'Download')
            ),
            tags$hr(),
            fluidRow(
               column(
                  width = 6,
                  'Plain text data:'
               ),
               column(
                  width = 6,
                  checkboxInput('header', 'Header', TRUE)
               )
            ),
            fluidRow(
               column(
                  width = 6,
                  radioButtons(
                     'sep',
                     'Separator',
                     c(
                        Comma = ',',
                        Semicolon = ';',
                        Tab = '\t'
                     ),
                     ','
                  )
               ),
               column(
                  width = 6,
                  radioButtons(
                     'dec',
                     'Decimal',
                     c('Point' = '.',
                     'Comma' = ','),
                     '.'
                  )
               )
            )
         ),
         conditionalPanel(
            condition = 'input.conditionedSidePanels==2',
            fluidRow(
               sliderInput(
                  'lag',
                  'Lag time, min', 
                  value = 5, min = 0, max = 30, step = 1
               ),
               bsTooltip(
                  'lag',
                  'Length of lag time that defines part of data that are influenced by the dilution. Provided in minutes.',
                  'right',
                  options = list(container = 'body')
               )
            )
         ),
         width = 3
      ),
      # Main panel ====
      mainPanel(
         tabsetPanel(
            type = 'tabs',
            tabPanel(
               'Data Processing',
               value = 1,
               strong(textOutput("count")),
               br(),
               strong(textOutput("filename")),
               em(textOutput("filesize")),
               br(),
               textOutput('inputdim'),
               plotOutput('dataViewPlot', width = '90%')
            ), 
            tabPanel(
               'Data Analysis',
               value = 2,
               fluidRow(
                  column(
                     4,
                     br(),
                     DT::dataTableOutput('dataProcessingTable')
                  ),
                  column(
                     8,
                     plotOutput('dataProcessingPlot')   
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
         actionLink('register', label = 'Register to support further development'),
         tags$p(),
         conditionalPanel(
            'input.register',
            textInput("name", label = "First name", value = "First name"),
            textInput("surname", label = "Last name", value = "Last name"),
            textInput("email", label = "Email", value = "@"),
            textInput("organization", label = "Organization", value = "Organization"),
            textInput("department", label = "Department", value = "Department"),
            actionButton("send", label = "Send"),
            tags$hr(),
            p(
               '@author CzechGlobe - Department of Adaptive Biotechnologies (JaCe)'
            ),
            p(
               "@email cerveny.j@czechglobe.cz"
            )
         )
      ),
      column(
         width = 7,
         actionLink("help", label = "Help"),
         conditionalPanel(
            "input.help",
            p(
               "This tool was developed to simplify manipulation with untidy data generated by Photon Systems Instruments (PSI) photobioreactor sofware. The data uploaded to this tool are expected in Excel (.xlsx) or plain text tabular data CSV (or TSV) format."
            ),
            p(
               "For PSI photobioreactor software expoerted data, the most simple untidy data preparation procedure is to export data from the software in ODS format and then either save the file as .xlsx Excel format or export the last sheet (Data) as CSV file."
            ),
            p(
               "In general this tool can be used on any numeric data table, e.g. for large datasets tidying up, averaging, etc."
            )
         ),
         offset = 1
      )
   ),
   fluidRow(
      column(
         2,
         tags$img(src = "img/Logo-CzechGlobe.jpg", alt = "CzechGlobe", height = 60, align = "top")
      ),
      column(
         1,
         tags$img(src = "img/Logo-C4Sys.jpg", alt = "C4Sys", height = 45, align = "right")
      )
   )
))
