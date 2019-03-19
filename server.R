shinyServer(function(input, output, session) {
  rangesView <- reactiveValues(x = NULL, y = NULL)
  rangesAnalysisGC <- reactiveValues(x = NULL, y = NULL)
  rangesAnalysisTurbi <- reactiveValues(x = NULL, y = NULL)
  rangesCalibrations <-  reactiveValues(x = NULL, y = NULL)
  observeEvent(input$regSend, {
    source('dbAddRegistration.R', local = TRUE, echo = FALSE)
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
      output$downloadSize <- renderText({ paste0("") })
    }
  })
  observeEvent(input$dataAnalysisGCPlot_dblClick, {
    brush <- input$dataAnalysisGCPlot_brush
    if (!is.null(brush)) {
      rangesAnalysisGC$x <- c(brush$xmin, brush$xmax)
      rangesAnalysisGC$y <- c(brush$ymin, brush$ymax)
    } else {
      rangesAnalysisGC$x <- NULL
      rangesAnalysisGC$y <- NULL
    }
  })
  observeEvent(input$dataAnalysisTurbiPlot_dblClick, {
    brush <- input$dataAnalysisTurbiPlot_brush
    if (!is.null(brush)) {
      rangesAnalysisTurbi$x <- c(brush$xmin, brush$xmax)
      rangesAnalysisTurbi$y <- c(brush$ymin, brush$ymax)
    } else {
      rangesAnalysisTurbi$x <- NULL
      rangesAnalysisTurbi$y <- NULL
    }
  })
  observeEvent(input$dataCalibrationsPlot_dblClick, {
    brush <- input$dataCalibrationsPlot_brush
    if (!is.null(brush)) {
      rangesCalibrations$x <- c(brush$xmin, brush$xmax)
      rangesCalibrations$y <- c(brush$ymin, brush$ymax)
    } else {
      rangesCalibrations$x <- NULL
      rangesCalibrations$y <- NULL
    }
  })
  observeEvent(input$slider_dataAnalysisPeriodic_period, {
    isolate(updateCheckboxInput(session,  'checkbox_dataAnalysisPeriodic_searchPeriod', value = FALSE))
  })
  # File processing ====
  # Upload
  dataInput <- reactive({
    if (is.null(input$file_data) || (input$file_data$type !='application/vnd.openxmlformats-officedocument.spreadsheetml.sheet')) {
      withProgress(
        message = "Processing example data...",
        value = 0.4,
        {
          untidyData <- readWorkbook('data/Cyanothece_LD1212_LL.xlsx', sheet = 'Data')
          incProgress(0.4)
        }
      )
    } else {
      withProgress(
        message = "Processing uploaded data...",
        value = 0.4,
        {
          untidyData <- readWorkbook(input$file_data$datapath, sheet = 'Data')
          incProgress(0.4)
        }
      )
    }
    pumpsAvailable <- c("Pump 0", "Pump 3", "Pump 4", "Pump 5", "Pump 6", "Pump 7")
    selectChoices <- merge(data.frame(X = colnames(untidyData)), dataColumnNames)
    pA <- pumpsAvailable[max(which(!is.na(match(pumpsAvailable, selectChoices$Y))))]
    if (is.na(pA)) {
      hideTab(inputId = 'conditionedSidePanels', target = "Data Analysis - Turbidostat")
    } else {
      updateSelectInput(session, 'select_dataAnalysisTurbi_dilutionPump',
        choices = pA,
        selected = max(pA)
      )
    }
    updateSliderInput(session, 'slider_dataAnalysisPeriodic_startTime',
      max = floor(max(untidyData$time, na.rm = TRUE)), step = 1
    )
    updateSliderInput(session, 'slider_dataAnalysisPeriodic_noPeriods',
      max = floor(max(untidyData$time, na.rm = TRUE) / 24), step = 1
    )
    updateSliderInput(session, 'slider_dataAnalysisPeriodic_period',
      max = floor((max(untidyData$time, na.rm = TRUE) - ceiling(max(untidyData$time, na.rm = TRUE) %% 24)) / 3)
    )
    return(untidyData)
  })
  # Download - TidyData
  output$download_dataView <- downloadHandler(
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
        if (input$file_data$type == 
            'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet') {
          paste(input$file_data$name)
        }
    }
    ,
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
        # Write to a file specified by the 'file' argument
        if (input$file_data$type ==
            'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet') {
          withProgress(
              message = "Writing processed data...",
              value = 0.4,
              {
                pbrfile_data  <- loadWorkbook(file = input$file_data$datapath)
                if (is.na(match('TidyData', getSheetNames(file = input$file_data$datapath)))) {
                    addWorksheet(pbrfile_data, 'TidyData')   
                }
                if (!is.null(rangesView$x)) {
                    tidyData <- dataProcessed() %>% filter(.$time > floor(rangesView$x[1])) %>% filter(.$time < ceiling(rangesView$x[2]))
                } else {
                    tidyData <- dataProcessed()
                }
                writeData(pbrfile_data, 'TidyData', tidyData)
                saveWorkbook(pbrfile_data, paste0(input$file_data$datapath, '-tempDV'), overwrite = TRUE)
                incProgress(0.4)
                file.copy(paste0(input$file_data$datapath, '-tempDV'), file)
              }
          )
        }
    }
  )
  # Download - Analysis GC
  output$download_dataAnalysisGC <- downloadHandler(
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste(input$file_data$name)
    }
    ,
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      # Write to a file specified by the 'file' argument
      if (input$file_data$type == 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet') {
        withProgress(
          message = "Writing processed data...",
          value = 0.4,
          {
            pbrfile_data  <- loadWorkbook(file = input$file_data$datapath)
            if (is.na(match('TidyData', getSheetNames(file = input$file_data$datapath)))) {
              addWorksheet(pbrfile_data, 'TidyData') 
            } else {
              removeWorksheet(pbrfile_data, 'TidyData')
              addWorksheet(pbrfile_data, 'TidyData')
            }
            writeData(pbrfile_data, 'TidyData', dataProcessed())
            if (is.na(match('Analysis-GC', getSheetNames(file = input$file_data$datapath)))) {
              addWorksheet(pbrfile_data, 'Analysis-GC')
            } else {
              removeWorksheet(pbrfile_data, 'Analysis-GC')
              addWorksheet(pbrfile_data, 'Analysis-GC')
            }
            writeData(pbrfile_data, 'Analysis-GC', growthRatesGC())
            saveWorkbook(pbrfile_data, paste0(input$file_data$datapath, '-tempDA-GC'), overwrite = TRUE)
            incProgress(0.4)
            file.copy(paste0(input$file_data$datapath, '-tempDA-GC'), file)
          }
        )
      }
    }
  )
  # Download - Analysis Turbi
  output$download_dataAnalysisTurbi <- downloadHandler(
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste(input$file_data$name)
    }
    ,
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
        # Write to a file specified by the 'file' argument
        if (input$file_data$type == 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet') {
          withProgress(
            message = "Writing processed data...",
            value = 0.4,
            {
              pbrfile_data  <- loadWorkbook(file = input$file_data$datapath)
              if (is.na(match('TidyData', getSheetNames(file = input$file_data$datapath)))) {
                addWorksheet(pbrfile_data, 'TidyData') 
              } else {
                removeWorksheet(pbrfile_data, 'TidyData')
                addWorksheet(pbrfile_data, 'TidyData')
              }
              writeData(pbrfile_data, 'TidyData', dataProcessed())
              if (is.na(match('Analysis-Turbi', getSheetNames(file = input$file_data$datapath)))) {
                addWorksheet(pbrfile_data, 'Analysis-Turbi')
              } else {
                removeWorksheet(pbrfile_data, 'Analysis-Turbi')
                addWorksheet(pbrfile_data, 'Analysis-Turbi')
              }
              writeData(pbrfile_data, 'Analysis-Turbi', growthRatesTurbi())
              saveWorkbook(pbrfile_data, paste0(input$file_data$datapath, '-tempDA-Turbi'), overwrite = TRUE)
              incProgress(0.4)
              file.copy(paste0(input$file_data$datapath, '-tempDA-Turbi'), file)
            }
          )
        }
    }
  )
  # Download - Analysis Periodic
  output$download_dataAnalysisPeriodic <- downloadHandler(
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste(input$file_data$name)
    }
    ,
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      # Write to a file specified by the 'file' argument
      if (input$file_data$type == 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet') {
        withProgress(
          message = "Writing processed data...",
          value = 0.4,
          {
            pbrfile_data  <- loadWorkbook(file = input$file_data$datapath)
            if (is.na(match('TidyData', getSheetNames(file = input$file_data$datapath)))) {
              addWorksheet(pbrfile_data, 'TidyData') 
            } else {
              removeWorksheet(pbrfile_data, 'TidyData')
              addWorksheet(pbrfile_data, 'TidyData')
            }
            writeData(pbrfile_data, 'TidyData', dataProcessed())
            if (is.na(match('Analysis-Periodic', getSheetNames(file = input$file_data$datapath)))) {
              addWorksheet(pbrfile_data, 'Analysis-Periodic')
            } else {
              removeWorksheet(pbrfile_data, 'Analysis-Periodic')
              addWorksheet(pbrfile_data, 'Analysis-Periodic')
            }
            writeData(pbrfile_data, 'Analysis-Periodic', dataPeriodic())
            saveWorkbook(pbrfile_data, paste0(input$file_data$datapath, '-tempDA-Periodic'), overwrite = TRUE)
            incProgress(0.4)
            file.copy(paste0(input$file_data$datapath, '-tempDA-Periodic'), file)
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
    if(input$select_dataView_plotSeries == "") {
        selected <- selectChoices$Y[1]
    } else {
        selected = input$select_dataView_plotSeries
    }
    if(input$select_dataView_plotSeriesAdd == "" || input$select_dataView_plotSeriesAdd == "-") {
        selected2 <- "-"
    } else {
        selected2 = input$select_dataView_plotSeriesAdd
    }
    if(input$select_dataAnalysisPeriodic_dataSeries == "") {
        selectedPeriodic <- selectChoices$Y[1]
    } else {
        selectedPeriodic = input$select_dataAnalysisPeriodic_dataSeries
    }
    updateSelectInput(session, 'select_dataView_plotSeries', choices = selectChoices$Y, selected = selected)
    updateSelectInput(session, 'select_dataView_plotSeriesAdd', choices = c("-", selectChoices$Y), selected = selected2)
    updateSelectInput(session, 'select_dataAnalysisPeriodic_dataSeries', choices = selectChoices$Y, selected = selectedPeriodic)
    if (input$slider_dataView_interval > 0) {
        factor <- 3600 / (input$slider_dataView_interval * 60)
        untidyData$time <- round(untidyData$time * factor) / factor
        untidyData %>% fill(eval(2 : ncol(.))) %>% group_by(time) %>% summarize_all(mean) %>% arrange(time)
    } else {
        untidyData %>% fill(eval(2 : ncol(.)))
    }
  })
  dataProcessed_d <- dataProcessed %>% debounce(2000)
  # Growth rates calculation
  growthRatesGC <- reactive({
    data <- dataProcessed_d()
    columnName <- dataColumnNames$X[match(input$select_dataAnalysisGC_growthRates, dataColumnNames$Y)]
    withProgress(
        message = "Calculating growth rates...",
        value = 0.0,
        {
          expFitStart <- c()
          expFitStop <- c()
          time <- c()
          mu <- c()
          Dt <- c()
          R2 <- c()
          data$group <- as.numeric(cut2(data$time, g = floor(max(data$time) / (input$slider_dataAnalysisGC_interval / 60))))
          for (j in 1 :  max(data$group)) {
              incProgress(j / max(data$group)) 
              timeFit <- data$time[data$group %in% j]
               if (length(timeFit) < 4) {
                next
              }
              dataFit <- data[[columnName]][data$group %in% j]
              if (length(dataFit[!is.na(dataFit)]) < length(timeFit)) {
                next
              }
              fit <- nls(dataFit ~ exp(a + b * timeFit),
                        start = list(a = 0, b = 0.1),
                        control = list(maxiter = 99, minFactor = 1/2048, warnOnly = TRUE))
              time <- c(time, timeFit[length(timeFit)])
              mu <- c(mu, coef(fit)[2] * 24)
              Dt <- c(Dt, 1 / coef(fit)[2] * log(2))
              R2 <- c(R2, cor(dataFit, predict(fit)))
          }
        }
      )
    return(data.frame(time, mu, Dt, R2))
  })
  growthRatesTurbi <- reactive({
    data <- dataProcessed_d()
    pumps <- grep('pumps.pump-.$', colnames(data), value = TRUE)
    if ((length(pumps) < 1) || is.na(pumps)) {
      return(data.frame(time = c(0), mu = c(0), Dt = c(0), R2 = c(0)))
    }
    columnName <- dataColumnNames$X[match(input$select_dataAnalysisTurbi_dilutionPump, dataColumnNames$Y)]
    if(is.null(data[[columnName]]) || (sum(data[[columnName]], na.rm = TRUE) == 0)) {
      return(data.frame(time = c(0), mu = c(0), Dt = c(0), R2 = c(0)))
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
          Dt <- c()
          R2 <- c()
          for (i in 2:(length(pumpOn) - 1)) {
              if (data[[columnName]][pumpOn[i] - 1] == 0) expFitStop <- c(expFitStop, pumpOn[i])
              else if (data[[columnName]][pumpOn[i] + 1] == 0) expFitStart <- c(expFitStart, pumpOn[i] + 1)
          }
          for (j in 1 : length(expFitStart)) {
              interval <- c((expFitStart[j] + floor(input$slider_dataAnalysisTurbi_ignored / input$slider_dataView_interval)) : expFitStop[j])
              incProgress(j / length(expFitStart))
              if (length(interval) < 4) {
                next
              }
              timeFit <- data$time[interval]
              dataFit <- data[[dataColumnNames$X[match(input$select_dataAnalysisTurbi_growthRates, dataColumnNames$Y)]]][interval]
              fit <- nls(dataFit ~ exp(a + b * timeFit),
                        start = list(a = 0, b = 0.1),
                        control = list(maxiter = 99, minFactor = 1/2048, warnOnly = TRUE))
              time <- c(time, timeFit[length(timeFit)])
              mu <- c(mu, coef(fit)[2] * 24)
              Dt <- c(Dt, 1 / coef(fit)[2] * log(2))
              R2 <- c(R2, cor(dataFit, predict(fit)))
          }
        })
    return(data.frame(time, mu, Dt, R2))
  })
  dataPeriodic <- reactive({
    withProgress(
      message = "Processing periodic data...",
      value = 0.3,
      {
        factor <- input$slider_dataView_interval / 60
        periodicData <- subset(dataProcessed_d(), ((time > input$slider_dataAnalysisPeriodic_startTime + input$slider_dataAnalysisPeriodic_shift) & (time < (input$slider_dataAnalysisPeriodic_startTime + input$slider_dataAnalysisPeriodic_shift + input$slider_dataAnalysisPeriodic_period * input$slider_dataAnalysisPeriodic_noPeriods))))
        incProgress(0.3)
        # period determination
        if (input$checkbox_dataAnalysisPeriodic_searchPeriod) {
          withProgress(
            message = "Identifying the period ...",
            value = 0.3,
            {
              yvalues <- periodicData[dataColumnNames$X[match(input$select_dataAnalysisPeriodic_dataSeries, dataColumnNames$Y)]]
              xvalues <- periodicData$time
              bp <- boxplot(yvalues)
              xvalues <- xvalues[!yvalues[[1]] %in% bp$out]
              yvalues <- yvalues[[1]][!yvalues[[1]] %in% bp$out]
              bp <- boxplot(yvalues)
              incProgress(0.4)
              out <- tryCatch({
                # TODO need to be replaced with an empty object initialization and evaluated as a part of FOR cycle
                model_final <- nls(yvalues ~ b1 + a1 * sin(2 * pi * (xvalues - d1) / tau), start = list(a1 = ((bp$stats[5] - bp$stats[1]) / 2), b1 = bp$stats[3], tau = input$slider_dataAnalysisPeriodic_period, d1 = 0), control = list(warnOnly = TRUE))
                incProgress(0.1)
                periodTestSet <- seq(from = floor(input$slider_dataAnalysisPeriodic_period * 2 / 3), to = ceiling(input$slider_dataAnalysisPeriodic_period * 3 / 2), by = round(input$slider_dataAnalysisPeriodic_period / 6, digits = 0))
                for (i in periodTestSet) {
                  model <- nls(yvalues ~ b1 + a1 * sin(2 * pi * (xvalues - d1) / tau), start = list(a1 = ((bp$stats[5] - bp$stats[1]) / 2), b1 = bp$stats[3], tau = i, d1 = 0), control = list(warnOnly = TRUE))
                  incProgress(0.9 / length(periodTestSet))
                  if (summary(model_final)$sigma > summary(model)$sigma) {
                    model_final <- model
                  }
                }
              },
              error = function(cond) {
                message(cond)
                # Choose a return value in case of error
                return(NULL)
              },
              warning = function(cond) {
                message(cond)
                # Choose a return value in case of warning
                return(NULL)
              })
            isolate(updateSliderInput(session, 'slider_dataAnalysisPeriodic_period', value = as.numeric(coef(model_final)['tau']), step = 0.1))
            }
          )
        }
        periodicData$time <- round(((periodicData$time - input$slider_dataAnalysisPeriodic_startTime + input$slider_dataAnalysisPeriodic_shift) %% input$slider_dataAnalysisPeriodic_period) / factor) * factor
        averagedData <- periodicData %>% group_by(time) %>% summarize_all(funs(mean, sd), na.rm = TRUE)
        incProgress(0.4)
      }
    )
    return(averagedData)
  })
  calibrationStability <- reactive({
    data <- dataProcessed_d()
    columnName <- dataColumnNames$X[match(input$select_dataCalibration_series, dataColumnNames$Y)]
    withProgress(
        message = "Checking stabilization ...",
        value = 0.0,
        {
          time <- c()
          parameter <- c()
          value <- c()
          slope <- c()
          R2 <- c()
          data$group <- as.numeric(cut2(data$time, g = floor(max(data$time) / (input$slider_dataCalibration_interval / 60))))
          for (j in 1 :  max(data$group)) {
              incProgress(j / max(data$group))
              dataFit <- data[data$group %in% j, ]
               if (dim(data)[1] < 4) {
                next
              }
              fit <- lm(probes.o2 ~  time, data = dataFit)
              time <- c(time, dataFit$time[dim(dataFit)[1]])
              parameter <- c(parameter, as.numeric(names(sort(table(dataFit$`thermo.thermo-reg`),decreasing=TRUE)[1])))
              value <- c(value, mean(dataFit$`probes.o2`))
              slope <- c(slope, coef(fit)[2])
              R2 <- c(R2, summary(fit)$r.squared)
          }
        }
      )
    return(data.frame(time, parameter, value, slope, R2))
  })
  calibrationFit <- reactive({
    stablizedCalibration <-  calibrationStability() %>%  filter(abs(slope) <= 5) %>% group_by(parameter) %>% summarize_all(mean) %>% arrange(parameter)
    return(stablizedCalibration)
  })
  # Outputs hadling ====
  output$fileName <- renderText({
    if (!is.null(input$file_data$name))
        paste("Uploaded file name is ", input$file_data$name)
  })
  output$fileSize <- renderText({
    if (!is.null(input$file_data$size))
        paste("Uploaded file size is ",
              round(input$file_data$size / 1024),
              " kB")
  })
  output$dataDim <- renderText({
    if (!is.null(input$file_data$size))
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
    withProgress(
      message = "Plotting graph...",
      value = 0.4,
      {
        if (!is.null(data)) {
          if(input$select_dataView_plotSeriesAdd == "" || input$select_dataView_plotSeriesAdd == "-") {
            plot(x = data$time,
              y = data[[dataColumnNames$X[match(input$select_dataView_plotSeries, dataColumnNames$Y)]]],
              xlim = rangesView$x,
              ylim = rangesView$y,
              xlab = 'Experiment duration, h',
              ylab = input$select_dataView_plotSeries)
          } else {
            if (!is.null(rangesView$y)) {
              ymin = min(data[[dataColumnNames$X[match(input$select_dataView_plotSeries, dataColumnNames$Y)]]], na.rm = TRUE)
              ymax = max(data[[dataColumnNames$X[match(input$select_dataView_plotSeries, dataColumnNames$Y)]]], na.rm = TRUE)
              y2min = min(data[[dataColumnNames$X[match(input$select_dataView_plotSeriesAdd, dataColumnNames$Y)]]], na.rm = TRUE)
              y2max = max(data[[dataColumnNames$X[match(input$select_dataView_plotSeriesAdd, dataColumnNames$Y)]]], na.rm = TRUE)
              y2lim <- c((rangesView$y[1] - ymin) / (ymax - ymin) * (y2max - y2min) + y2min, y2max - (ymax - rangesView$y[2]) / (ymax - ymin) * (y2max - y2min))
            } else {
              y2lim <- NULL
            }
            incProgress(0.4)
            twoord.plot(
              lx = data$time,
              ly = data[[dataColumnNames$X[match(input$select_dataView_plotSeries, dataColumnNames$Y)]]],
              rx = data$time,
              ry = data[[dataColumnNames$X[match(input$select_dataView_plotSeriesAdd, dataColumnNames$Y)]]],
              xlim = rangesView$x,
              lylim = rangesView$y,
              rylim = y2lim,
              xlab = 'Experiment duration, h',
              ylab = input$select_dataView_plotSeries,
              rylab = input$select_dataView_plotSeriesAdd,
              rcol = 3,
              rpch = 1)
          }
        }
      }
    )
  })
  # https://rstudio.github.io/DT/options.html
  # https://datatables.net/reference/option/dom
  output$dataAnalysisGCTable <- DT::renderDataTable({
    if (!is.null(growthRatesGC())) {
      datatable(growthRatesGC() %>% filter(R2 >= (input$slider_dataAnalysisTurbi_acceptableR2 / 100)), options = list(dom = 'tlp', pageLength = 8, lengthChange = FALSE, searching = FALSE)) %>% formatRound(c('time', 'mu', 'Dt', 'R2'), digits = 2)
    }
  }, server = FALSE)
  output$dataAnalysisGCPlot <- renderPlot({
    if (!is.null(growthRatesGC())) {
       withProgress(
        message = "Plotting graph...",
        value = 0.4,
        {
          gRates <- growthRatesGC() %>% filter(R2 >= (input$slider_dataAnalysisTurbi_acceptableR2 / 100))
          if (dim(gRates)[1] > 0) {
            s1 = NULL
            s2 = input$dataAnalysisGCTable_rows_selected
            twoord.plot(lx = gRates$time,
              ly = gRates$Dt,
              rx = gRates$time,
              ry = gRates$mu,
              xlim = rangesAnalysisGC$x,
              lylim = rangesAnalysisGC$y,
              # rylim = y2lim,
              xlab = "Experiment duration, h",
              ylab = "Doubling time, h",
              rylab = "Specific growth rate, 1/day",
              rcol = 3,
              rpch = 1)
              incProgress(0,3)
            if (length(s1)) {
              points(gRates[s1, , drop = FALSE], pch = 19, cex = 1, col = 'green')
            }
            if (length(s2)) {
              points(gRates$time[s2], gRates$Dt[s2], pch = 19, cex = 1.25)
            }
          }
        } 
      )
    }
  })
  output$dataAnalysisTurbiTable <- DT::renderDataTable({
    if (!is.null(growthRatesTurbi())) {
      datatable(growthRatesTurbi() %>% filter(R2 >= (input$slider_dataAnalysisTurbi_acceptableR2 / 100)), options = list(dom = 'tlp', pageLength = 8, lengthChange = FALSE, searching = FALSE)) %>% formatRound(c('time', 'mu', 'Dt', 'R2'), digits = 2)
    }
  }, server = FALSE)
  output$dataAnalysisTurbiPlot <- renderPlot({
    if (!is.null(growthRatesTurbi())) {
       withProgress(
        message = "Plotting graph...",
        value = 0.4,
        {
          s1 = NULL
          s2 = input$dataAnalysisTurbiTable_rows_selected
          gRates <- growthRatesTurbi() %>% filter(R2 >= (input$slider_dataAnalysisTurbi_acceptableR2 / 100))
          # plot(x = gRates$time, y = gRates$Dt, xlim = rangesAnalysisTurbi$x, ylim = rangesAnalysisTurbi$y, xlab = "Experiment duration, h", ylab = "Doubling time, h")
          if (dim(gRates)[1] > 0) {
            twoord.plot(lx = gRates$time,
              ly = gRates$Dt,
              rx = gRates$time,
              ry = gRates$mu,
              xlim = rangesAnalysisTurbi$x,
              lylim = rangesAnalysisTurbi$y,
              # rylim = y2lim,
              xlab = "Experiment duration, h",
              ylab = "Doubling time, h",
              rylab = "Specific growth rate, 1/day",
              rcol = 3,
              rpch = 1)
              incProgress(0.3)
            if (length(s1)) {
              points(gRates[s1, , drop = FALSE], pch = 19, cex = 1, col = 'green')
            }
            if (length(s2)) {
              points(gRates$time[s2], gRates$Dt[s2], pch = 19, cex = 1.25)
            }
          }
        }
      )
    }
  })
  output$text_dataPeriodicAnalysis_interval <- renderText({
    paste0("Data time range used: ",  input$slider_dataAnalysisPeriodic_startTime + input$slider_dataAnalysisPeriodic_shift, " to ", input$slider_dataAnalysisPeriodic_startTime + input$slider_dataAnalysisPeriodic_shift + (input$slider_dataAnalysisPeriodic_period * input$slider_dataAnalysisPeriodic_noPeriods),  " h")
  })
  output$dataAnalysisPeriodicPlot <- renderPlotly({
    if (!is.null(dataPeriodic())) {
      withProgress(
        message = "Plotting graph...",
        value = 0.4,
        {
          selectedData <- dataPeriodic()[c("time", paste0(dataColumnNames$X[match(input$select_dataAnalysisPeriodic_dataSeries, dataColumnNames$Y)], '_mean'), paste0(dataColumnNames$X[match(input$select_dataAnalysisPeriodic_dataSeries, dataColumnNames$Y)], '_sd'))]
          colnames(selectedData) <- c("time", "mean", "sd")
          incProgress(0.45)
          p <- ggplot(data = selectedData)
          if(!sum(is.na(selectedData$sd))) {
            p <- p + geom_ribbon(mapping = aes(x = time, ymin = mean - sd, max = mean + sd), alpha = 0.2, colour = "orange")
          }
          p <- p +
          geom_point(mapping = aes(x = time, y = mean), colour = "black", size = 2, shape = 21, fill = "white") +
          coord_cartesian(xlim = c(0, input$slider_dataAnalysisPeriodic_period), expand = FALSE) +
          scale_x_continuous(breaks = seq(from = 0, to = input$slider_dataAnalysisPeriodic_period, by = input$slider_dataAnalysisPeriodic_period / 4)) +
          labs(
            subtitle = paste0("Periodic data intervals averaged: ", input$slider_dataAnalysisPeriodic_noPeriods, " x ", input$slider_dataAnalysisPeriodic_period, " h"),
            x = "Time in period, h",
            y = input$select_dataAnalysisPeriodic_dataSeries
          ) +
          theme_bw()
          incProgress(0.35)
          ggplotly(p)
        }
      )
    }
  })
  output$dataCalibrationsTable <- DT::renderDataTable({
    if (!is.null(calibrationStability())) {
      datatable(calibrationStability() %>% filter(abs(slope) <= (input$slider_dataCalibration_acceptableSlope)), options = list(dom = 'tlp', pageLength = 8, lengthChange = FALSE, searching = FALSE)) %>% formatRound(c('time', 'parameter', 'value', 'slope', 'R2'), digits = 2)
    }
  }, server = FALSE)
  output$dataCalibrationsPlot <- renderPlot({
    if (!is.null(calibrationStability())) {
      withProgress(
        message = "Plotting graph...",
        value = 0.4,
        {
          gRates <- calibrationStability() %>% filter(abs(slope) <= (input$slider_dataCalibration_acceptableSlope))
          if (dim(gRates)[1] > 0) {
            s1 = NULL
            s2 = input$dataCalibrationsTable_rows_selected
            twoord.plot(lx = gRates$time,
              ly = gRates$slope,
              rx = gRates$time,
              ry = gRates$parameter,
              xlim = rangesCalibrations$x,
              lylim = rangesCalibrations$y,
              # rylim = y2lim,
              xlab = "Experiment duration, h",
              ylab = "Slope, signal/h",
              rylab = "Parameter",
              rcol = 3,
              rpch = 1)
            if (length(s1)) {
              points(gRates[s1, , drop = FALSE], pch = 19, cex = 1, col = 'green')
            }
            if (length(s2)) {
              points(gRates$time[s2], gRates$slope[s2], pch = 19, cex = 1.25)
            }
          }
        }
      )
    }
  })
  output$dataCalibrationsFitPlot <- renderPlot({
    withProgress(
      message = "Plotting graph...",
      value = 0.4,
      {
        plot(calibrationFit()$parameter, calibrationFit()$value, type = "p")
        fit <- lm(value ~  parameter, data = calibrationFit())
        # TODO adjust newdata definition
        lines(x = calibrationFit()$parameter, y = predict(fit, parameter = calibrationFit()$parameter))
      }
    )
  })
})
