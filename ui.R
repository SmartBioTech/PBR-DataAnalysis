shinyUI(fluidPage(
  tags$head(includeScript('google-analytics.js')),
  titlePanel("", windowTitle = "PBR Data Analysis"),
  sidebarLayout(
    # UI - Sidebar panel ====
    sidebarPanel(
      conditionalPanel(condition = 'input.conditionedSidePanels == 1',
        fluidRow(
          fileInput(
            'file_data',
            "Choose data file to upload",
            accept = c( 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet', '.xlsx')
          )
        ),
        fluidRow(
          sliderInput('slider_dataView_interval',
            "Averaging interval, min", 
            value = 1, min = 0, max = 60, step = 1
          ),
          bsTooltip('slider_dataView_interval',
            "Interval that is used for lumping and averaging untidy data. Provided in minutes.",
            'right', options = list(container = 'body')
          )
        ),
        tags$hr(),
        fluidRow(
          selectInput('select_dataView_plotSeries', "Data to view", "OD680, AU")
        ),
        fluidRow(
          selectInput('select_dataView_plotSeriesAdd', "Additional data to view", "dO2")
        ),
        tags$hr(),
        fluidRow(
          textOutput('downloadSize'),
          downloadButton('download_dataView', "Download")
        )
      ),
      conditionalPanel(condition = 'input.conditionedSidePanels == 2',
        fluidRow(
          sliderInput( 'slider_dataAnalysisGC_interval',
            "Interval, min", 
            value = 60, min = 15, max = 600, step = 15
          ),
          bsTooltip( 'slider_dataAnalysisGC_interval',
            "Time interval for growth rate determination.",
            'right',
            options = list(container = 'body')
          )
        ),
        tags$hr(),
        fluidRow(
          selectInput('select_dataAnalysisGC_growthRates',
            "Data for growth rates determination",
            c ("OD680, AU", "OD720, AU"),
            "OD680, AU"
          )
        ),
        tags$hr(),
        fluidRow(
          downloadButton('download_dataAnalysisGC',
            "Download"
          )
        )
      ),
      conditionalPanel(condition = 'input.conditionedSidePanels == 3',
        fluidRow(
          sliderInput( 'slider_dataAnalysisTurbi_ignored',
            "Ignored Data, min", 
            value = 5, min = 0, max = 30, step = 1
          ),
          bsTooltip( 'slider_dataAnalysisTurbi_ignored',
            "Time interval that defines part of the data that are influenced just after a dilution. Provided in minutes.",
            'right',
            options = list(container = 'body')
          ),
          sliderInput( 'slider_dataAnalysisTurbi_acceptableR2',
            "Acceptable Coefficient of Determination", 
            value = 55, min = 25, max = 100, step = 5
          ),
          bsTooltip( 'slider_dataAnalysisTurbi_acceptableR2',
            "Filter the growth rates based on minimum CoD (R2) of the regression",
            'right',
            options = list(container = 'body')
          )
        ),
        tags$hr(),
        fluidRow(
          selectInput('select_dataAnalysisTurbi_growthRates',
            "Data for growth rates determination",
              c("OD680, AU", "OD720, AU"),
              "OD680, AU"
          )
        ),
        fluidRow(
          selectInput('select_dataAnalysisTurbi_dilutionPump',
            "Pump used for dilutions",
            c("Pump 0", "Pump 3", "Pump 4", "Pump 5", "Pump 6", "Pump 7"),
            "Pump 0"
          )
        ),
        tags$hr(),
        fluidRow(
          downloadButton('download_dataAnalysisTurbi',
            "Download"
          )
        )
      ),
      conditionalPanel(condition = 'input.conditionedSidePanels == 4',
        fluidRow(
          sliderInput( 'slider_dataAnalysisPeriodic_period',
            "Period duration, h", 
            value = 24, min = 1, max = 48, step = 1
          ),
          bsTooltip( 'slider_dataAnalysisPeriodic_period',
            "Period duration in hours.",
            'right',
            options = list(container = 'body')
          ),
          checkboxInput('checkbox_dataAnalysisPeriodic_searchPeriod', 
            "Identify the period [EXPERIMENTAL]",
            FALSE
           ),
          sliderInput( 'slider_dataAnalysisPeriodic_noPeriods',
            "No. of periods", 
            value = 5, min = 1, max = 10, step = 1
          ),
          bsTooltip( 'slider_dataAnalysisPeriodic_noPeriods',
            "Number of periods to analyze.",
            'right',
            options = list(container = 'body')
          ),
          sliderInput( 'slider_dataAnalysisPeriodic_startTime',
            "First period start time, h", 
            value = 0, min = 0, max = 240, step = 1
          ),
          bsTooltip( 'slider_dataAnalysisPeriodic_startTime',
            "First period start time in hours.",
            'right',
            options = list(container = 'body')
          ),
          sliderInput( 'slider_dataAnalysisPeriodic_shift',
            "Analysis window shift, h", 
            value = 0, min = -12, max = 12, step = 0.1
          ),
          bsTooltip( 'slider_dataAnalysisPeriodic_shift',
            "Set the time window shift for periodic data analysis.",
            'right',
            options = list(container = 'body')
          )
        ),
        tags$hr(),
        fluidRow(
          selectInput('select_dataAnalysisPeriodic_dataSeries',
            "Data for periodic analysis",
            c("dO2", "OD720, AU", "OD720, AU"),
            "dO2"
          )
        ),
        tags$hr(),
        fluidRow(
          textOutput('text_dataPeriodicAnalysis_interval'),
          downloadButton('download_dataAnalysisPeriodic',
            "Download"
          )
        )
      ),
      conditionalPanel(condition = 'input.conditionedSidePanels == 5',
        fluidRow(
          fileInput( 'file_calibration',
            "Choose a calibration file to upload",
            accept = c( 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet', '.xlsx' )
          )
        ),
        fluidRow(
          sliderInput( 'slider_dataCalibration_interval',
            "Averaging interval, min", 
            value = 30, min = 15, max = 240, step = 15
          ),
          bsTooltip( 'slider_dataCalibration_interval',
            "Interval that is used for lumping and averaging untidy data. Provided in minutes.",
            'right', options = list(container = 'body')
          ),
          sliderInput( 'slider_dataCalibration_acceptableSlope',
            "Acceptable stabilization slope", 
            value = 5, min = 0, max = 10, step = 1
          )
        ),
        
        tags$hr(),
        fluidRow(
          selectInput('select_dataCalibration_series',
            "Data to view",
            "dO2"
          )
        )
      ),
      width = 3
    ),
    # UI - Main panel ====
    mainPanel(
      tabsetPanel(
        tabPanel("Data View",
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
        tabPanel("Data Analysis - Growth Curve",
          value = 2,
          fluidRow(
            column(
              4,
              br(),
              DT::dataTableOutput('dataAnalysisGCTable')
            ),
            column(
              8,
              plotOutput('dataAnalysisGCPlot' ,  
                dblclick = 'dataAnalysisGCPlot_dblClick',
                brush = brushOpts(
                  id = 'dataAnalysisGCPlot_brush',
                  resetOnNew = TRUE
                )
              )
            )
          )
        ),
        tabPanel("Data Analysis - Turbidostat",
          value = 3,
          fluidRow(
            column(
              4,
              br(),
              DT::dataTableOutput('dataAnalysisTurbiTable')
            ),
            column(
                8,
                plotOutput('dataAnalysisTurbiPlot',   
                  dblclick = 'dataAnalysisTurbiPlot_dblClick',
                  brush = brushOpts(
                    id = 'dataAnalysisTurbiPlot_brush',
                    resetOnNew = TRUE
                  )
                )
            )
          )
        ),
        tabPanel("Data Analysis - Periodic",
          value = 4,
          fluidRow(
            plotlyOutput('dataAnalysisPeriodicPlot',
              width = '90%'
            )
          )
        ),
        tabPanel("Calibrations",
          value = 5,
          fluidRow(
            column(
              4,
              br(),
              DT::dataTableOutput('dataCalibrationsTable')
            ),
            column(
              8,
              plotOutput('dataCalibrationsPlot' , 
                dblclick = 'dataCalibrationsPlot_dblClick',
                brush = brushOpts(
                    id = 'dataCalibrationsPlot_brush',
                    resetOnNew = TRUE
                )
              )
            )
          )
        ),
        id = 'conditionedSidePanels',
        type = 'tabs'
      ),
      width = 9
    )
  ),
  # UI - Bottom panel ====
  fluidRow(
    column(
      width = 3,
      # actionLink('register', label = "Register to support further development"),
      tags$p(),
      conditionalPanel('input.register',
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
      # actionLink('help', label = "Help"),
      conditionalPanel('input.help',
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
))
