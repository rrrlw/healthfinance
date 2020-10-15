#####ADAPTER/CONTROLLER#####
function(input, output, session) {
  #####SECTION HEADINGS/TEXT#####
  output$outheader <- renderText({"MODEL OUTPUTS (compared to no activity change)"})
  output$growthheader <- renderText({"ANNUAL GROWTH PARAMETERS"})
  output$instxt <- renderUI({
    HTML("<h4><b>INSURANCE BREAKDOWN</b></h4>")
  })
  output$outpttxt <- renderText({"OUTPATIENT BREAKDOWN"})
  output$inpttxt <- renderText({"INPATIENT BREAKDOWN"})
  output$annualtxt <- renderUI({
    HTML("<h4><b>ANNUAL USE BREAKDOWN</b></h4>")
  })
  output$restoretxt <- renderUI({
    HTML("<h4><b>ACTIVITY RESTORATION & BOOSTING</b></h4>")
  })
  output$rownumdeltext <- renderUI({
    HTML("<br>Enter row number:")
  })
  output$rowaddtext <- renderUI({
    HTML("<br>Item info to add:")
  })
  
  # reactive expressions for repeated use
  noinsprop <- reactive(1 - (input$mcareprop + input$mcaidprop + input$commprop))
  outptl5new <- reactive(1 - (input$outptl3new + input$outptl4new))
  outptl5fu <- reactive(1 - (input$outptl3fu + input$outptl4fu))
  inptl3admit <- reactive(1 - (input$inptl1admit + input$inptl2admit))
  annualm12 <- reactive(1 - (input$annualm1 + input$annualm2 + input$annualm3 +
                               input$annualm4 + input$annualm5 + input$annualm6 +
                               input$annualm7 + input$annualm8 + input$annualm9 +
                               input$annualm10 + input$annualm11))
  
  # model params determined by other params (unchangeable by user)
  output$noinsprop <- renderText({
    noinsprop()
  })
  output$outptl5new <- renderUI({
    HTML(paste("<b>Level 5</b>",
               outptl5new(),
               sep = "<br>"))
  })
  output$outptl5fu <- renderUI({
    HTML(paste("<b>Level 5</b>",
               outptl5fu(),
               sep = "<br>"))
  })
  output$inptl3admit <- renderUI({
    HTML(paste("<b>Level 3</b>",
               inptl3admit(),
               sep = "<br>"))
  })
  output$annualm12 <- renderUI({
    HTML(paste("<b>December</b>",
               round(annualm12(), 5),
               sep = "<br>"))
  })
  output$dc30more <- renderUI({
    HTML(paste("<b>>30 min</b>",
               inpt_dc()[[2]],
               sep = "<br>"))
  })
  
  # read in data
  finances <- tibble::tibble(Name = c("Sample Procedure 1", "Sample Procedure 2", "Sample Procedure 3"),
                             Volume = c(100, 200, 300),
                             Revenue = c(64, 512, 256))
  finances$Volume <- as.integer(finances$Volume)
  
  # display table of procedures
  output$currinput <- renderTable(finances, rownames = TRUE)
  
  #FROM HERE ON ?SHOULD BE SWITCHED BACK TO NON-REACTIVE (UNSURE)
  # set parameters
  
  prerev2020 <- 0
  prerev2021 <- 0
  prerev2022 <- 0
  postrev2020 <- 0
  postrev2021 <- 0
  postrev2022 <- 0
  
  coeffs_2020 <- rep(1/12, 12)
  coeffs_2021 <- rep(1/12, 12)
  coeffs_2022 <- rep(1/12, 12)
  
  boost_amt <- rep(0, 8)
  boost_start <- rep(1, 8)
  boost_end <- rep(36, 8)
  boost_proc <- list()
  
  #####SERVER: REVENUE CALCULATIONS#####
  observeEvent(input$calc, {
    # make sure all vars for calculation are correct
    assert_params()
    
    # setup boost vars
    boost_amt <- c(input$boost1,
                    input$boost2,
                    input$boost3,
                    input$boost4,
                    input$boost5,
                    input$boost6,
                    input$boost7,
                    input$boost8)
    
    boost_proc[[1]] <- input$boostproc1
    boost_proc[[2]] <- input$boostproc2
    boost_proc[[3]] <- input$boostproc3
    boost_proc[[4]] <- input$boostproc4
    boost_proc[[5]] <- input$boostproc5
    boost_proc[[6]] <- input$boostproc6
    boost_proc[[7]] <- input$boostproc7
    boost_proc[[8]] <- input$boostproc8
    
    boost_start <- c(lubridate::month(input$boostdur1[1]) + 12 * (lubridate::year(input$boostdur1[1]) - 2020),
                      lubridate::month(input$boostdur2[1]) + 12 * (lubridate::year(input$boostdur2[1]) - 2020),
                      lubridate::month(input$boostdur3[1]) + 12 * (lubridate::year(input$boostdur3[1]) - 2020),
                      lubridate::month(input$boostdur4[1]) + 12 * (lubridate::year(input$boostdur4[1]) - 2020),
                      lubridate::month(input$boostdur5[1]) + 12 * (lubridate::year(input$boostdur5[1]) - 2020),
                      lubridate::month(input$boostdur6[1]) + 12 * (lubridate::year(input$boostdur6[1]) - 2020),
                      lubridate::month(input$boostdur7[1]) + 12 * (lubridate::year(input$boostdur7[1]) - 2020),
                      lubridate::month(input$boostdur8[1]) + 12 * (lubridate::year(input$boostdur8[1]) - 2020))
    
    boost_end <- c(lubridate::month(input$boostdur1[2]) + 12 * (lubridate::year(input$boostdur1[2]) - 2020),
                    lubridate::month(input$boostdur2[2]) + 12 * (lubridate::year(input$boostdur2[2]) - 2020),
                    lubridate::month(input$boostdur3[2]) + 12 * (lubridate::year(input$boostdur3[2]) - 2020),
                    lubridate::month(input$boostdur4[2]) + 12 * (lubridate::year(input$boostdur4[2]) - 2020),
                    lubridate::month(input$boostdur5[2]) + 12 * (lubridate::year(input$boostdur5[2]) - 2020),
                    lubridate::month(input$boostdur6[2]) + 12 * (lubridate::year(input$boostdur6[2]) - 2020),
                    lubridate::month(input$boostdur7[2]) + 12 * (lubridate::year(input$boostdur7[2]) - 2020),
                    lubridate::month(input$boostdur8[2]) + 12 * (lubridate::year(input$boostdur8[2]) - 2020))
    
    # calculate vars
    coeffs_2020 <- str_to_vec_year(input$restore12)
    coeffs_2021 <- str_to_vec_year(input$restore24)
    coeffs_2022 <- str_to_vec_year(input$restore36)
    
    # calculate revenues
    revenues <- calc_rev(procedures = finances,
                         growth = c(input$growth20, input$growth21, input$growth22),
                         comp_ratio = c(1, input$mcaidcomp, input$commcomp, input$noinscomp),
                         ins_prop = c(input$mcareprop, input$mcaidprop, input$commprop, noinsprop()),
                         tech_fee_mult = input$techratio,
                         month_prop = c(input$annualm1,
                                        input$annualm2,
                                        input$annualm3,
                                        input$annualm4,
                                        input$annualm5,
                                        input$annualm6,
                                        input$annualm7,
                                        input$annualm8,
                                        input$annualm9,
                                        input$annualm10,
                                        input$annualm11,
                                        annualm12()),
                         restoration = c(coeffs_2020,
                                         coeffs_2021,
                                         coeffs_2022),
                         boost_amt = boost_amt,
                         boost_proc = boost_proc,
                         boost_start = boost_start,
                         boost_end = boost_end)
    
    # store breakdowns
    prerev <- revenues$Target
    postrev <- revenues$Projected
    
    prerev2020 <- sum(prerev[1:12])
    prerev2021 <- sum(prerev[13:24])
    prerev2022 <- sum(prerev[25:36])
    
    postrev2020 <- sum(postrev[1:12])
    postrev2021 <- sum(postrev[13:24])
    postrev2022 <- sum(postrev[25:36])
    
    output$prerev2020 <- renderText({
      scales::dollar(prerev2020)
    })
    
    output$prerev2021 <- renderText({
      scales::dollar(prerev2021)
    })
    
    output$prerev2022 <- renderText({
      scales::dollar(prerev2022)
    })
    
    percent2020 <- postrev2020 / prerev2020 * 100
    output$postrev2020 <- renderText({
      paste(scales::dollar(postrev2020),
            " (", round(percent2020, 2), "%)",
            sep = "")
    })
    
    percent2021 <- postrev2021 / prerev2021 * 100
    output$postrev2021 <- renderText({
      paste(scales::dollar(postrev2021),
            " (", round(percent2021, 2), "%)",
            sep = "")
    })
    
    percent2022 <- postrev2022 / prerev2022 * 100
    output$postrev2022 <- renderText({
      paste(scales::dollar(postrev2022),
            " (", round(percent2022, 2), "%)",
            sep = "")
    })
    
    presum2022 <- prerev2020 + prerev2021 + prerev2022
    output$presum2022 <- renderText({
      scales::dollar(presum2022)
    })
    
    postsum2022 <- postrev2020 + postrev2021 + postrev2022
    output$postsum2022 <- renderText({
      scales::dollar(postsum2022)
    })
    
    # displaying pre and post revenues
    output$targetrev <- renderUI({
      HTML(paste("<br><b>Target Revenue</b><br>",
                 "<br>2020 &#8594; ", scales::dollar(prerev2020, scale = 1e-6,
                                                     suffix = "M", largest_with_cents = 10),
                 "<br><br>",
                 "2021 &#8594; ", scales::dollar(prerev2021, scale = 1e-6,
                                                 suffix = "M", largest_with_cents = 10),
                 "<br><br>",
                 "2022 &#8594; ", scales::dollar(prerev2022, scale = 1e-6,
                                                 suffix = "M", largest_with_cents = 10),
                 sep = ""))
    })
    
    output$projrev <- renderUI({
      HTML(paste("<br><b>Projected Revenue</b><br>",
                 "<br>2020 &#8594; ", scales::dollar(postrev2020, scale = 1e-6,
                                                     suffix = "M", largest_with_cents = 10),
                 " (", round(percent2020, 0), "%)<br><br>",
                 "2021 &#8594; ", scales::dollar(postrev2021, scale = 1e-6,
                                                 suffix = "M", largest_with_cents = 10),
                 " (", round(percent2021, 0), "%)<br><br>",
                 "2022 &#8594; ", scales::dollar(postrev2022, scale = 1e-6,
                                                 suffix = "M", largest_with_cents = 10),
                 " (", round(percent2022, 0), "%)<br><br>",
                 sep = ""))
    })
    
    #####REVENUE PLOTS#####
    # generate revenue plots
    output$project12 <- renderPlot({
      project_rev <- tibble::tibble(
        Year = rep(2020, 24),
        Month = rep(c("Jan", "Feb", "Mar", "Apr",
                      "May", "Jun", "Jul", "Aug",
                      "Sep", "Oct", "Nov", "Dec"),
                    2),
        Type = c(rep("Target", 12), rep("Projected", 12)),
        Amount = c(prerev[1:12], postrev[1:12]))
      
      project_rev$Month <- factor(project_rev$Month,
                                  levels = c("Jan", "Feb", "Mar", "Apr",
                                             "May", "Jun", "Jul", "Aug",
                                             "Sep", "Oct", "Nov", "Dec"))
      project_rev$Type <- factor(project_rev$Type,
                                 levels = c("Target", "Projected"))
      
      ggplot2::ggplot(project_rev, ggplot2::aes(x = Month, y = Amount, fill = Type)) +
        ggplot2::geom_bar(stat = "identity", position = "dodge") +
        ggplot2::xlab(ggplot2::element_blank()) +
        ggplot2::ylab("Monthly Revenue") +
        ggplot2::scale_y_continuous(labels = custom_dollar) +
        ggplot2::ggtitle("2020 Revenue Breakdown") +
        #theme_cowplot() +
        ggplot2::theme(legend.position = "top",
              plot.margin = ggplot2::unit(c(0, 0, 0, 0), "null"))
    })
    output$project24 <- renderPlot({
      project_rev <- tibble::tibble(
        Year = rep(2021, 24),
        Month = rep(c("Jan", "Feb", "Mar", "Apr",
                      "May", "Jun", "Jul", "Aug",
                      "Sep", "Oct", "Nov", "Dec"),
                    2),
        Type = c(rep("Target", 12), rep("Projected", 12)),
        Amount = c(prerev[13:24], postrev[13:24]))
      
      project_rev$Month <- factor(project_rev$Month,
                                  levels = c("Jan", "Feb", "Mar", "Apr",
                                             "May", "Jun", "Jul", "Aug",
                                             "Sep", "Oct", "Nov", "Dec"))
      project_rev$Type <- factor(project_rev$Type,
                                 levels = c("Target", "Projected"))
      
      ggplot2::ggplot(project_rev, ggplot2::aes(x = Month, y = Amount, fill = Type)) +
        ggplot2::geom_bar(stat = "identity", position = "dodge") +
        ggplot2::xlab(ggplot2::element_blank()) +
        ggplot2::ylab("Monthly Revenue") +
        ggplot2::scale_y_continuous(labels = custom_dollar) +
        ggplot2::ggtitle("2021 Revenue Breakdown") +
        #theme_cowplot() +
        ggplot2::theme(legend.position = "none",
              plot.margin = ggplot2::unit(c(0, 0, 0, 0), "null"))
    })
    output$project36 <- renderPlot({
      project_rev <- tibble::tibble(
        Year = rep(2022, 24),
        Month = rep(c("Jan", "Feb", "Mar", "Apr",
                      "May", "Jun", "Jul", "Aug",
                      "Sep", "Oct", "Nov", "Dec"),
                    2),
        Type = c(rep("Target", 12), rep("Projected", 12)),
        Amount = c(prerev[25:36], postrev[25:36]))
      
      project_rev$Month <- factor(project_rev$Month,
                                  levels = c("Jan", "Feb", "Mar", "Apr",
                                             "May", "Jun", "Jul", "Aug",
                                             "Sep", "Oct", "Nov", "Dec"))
      project_rev$Type <- factor(project_rev$Type,
                                 levels = c("Target", "Projected"))
      
      ggplot2::ggplot(project_rev, ggplot2::aes(x = Month, y = Amount, fill = Type)) +
        ggplot2::geom_bar(stat = "identity", position = "dodge") +
        ggplot2::xlab(ggplot2::element_blank()) +
        ggplot2::ylab("Monthly Revenue") +
        ggplot2::scale_y_continuous(labels = custom_dollar) +
        ggplot2::ggtitle("2022 Revenue Breakdown") +
        #theme_cowplot() +
        ggplot2::theme(legend.position = "none",
              plot.margin = ggplot2::unit(c(0, 0, 0, 0), "null"))
    })
  })
  
  #####IMPORT#####
  observeEvent(input$readfile, {
    # get input file path from user
    filepath <- input$input$datapath
    
    # only read if file has actually been selected
    if (!is.null(filepath)) {
      finances <- readr::read_csv(filepath,
                                  col_names = TRUE,
                                  na = "NA",
                                  col_types = readr::cols(
                                    Name = readr::col_character(),
                                    Volume = readr::col_integer(),
                                    Revenue = readr::col_double()
                                  ))
      
      # display table of procedures
      output$currinput <- renderTable(finances, rownames = TRUE)
      
      # update boost procedures
      updateNumericInput(session, "boost1", "Boost #1 Amount", value = 0)
      updateNumericInput(session, "boost2", "Boost #2 Amount", value = 0)
      updateNumericInput(session, "boost3", "Boost #3 Amount", value = 0)
      updateNumericInput(session, "boost4", "Boost #4 Amount", value = 0)
      updateNumericInput(session, "boost5", "Boost #5 Amount", value = 0)
      updateNumericInput(session, "boost6", "Boost #6 Amount", value = 0)
      updateNumericInput(session, "boost7", "Boost #7 Amount", value = 0)
      updateNumericInput(session, "boost8", "Boost #8 Amount", value = 0)
      updateSelectInput(session, "boostproc1", label = "Boost #1 Procedures",
                        choices = finances$Name)
      updateSelectInput(session, "boostproc2", label = "Boost #2 Procedures",
                        choices = finances$Name)
      updateSelectInput(session, "boostproc3", label = "Boost #3 Procedures",
                        choices = finances$Name)
      updateSelectInput(session, "boostproc4", label = "Boost #4 Procedures",
                        choices = finances$Name)
      updateSelectInput(session, "boostproc5", label = "Boost #5 Procedures",
                        choices = finances$Name)
      updateSelectInput(session, "boostproc6", label = "Boost #6 Procedures",
                        choices = finances$Name)
      updateSelectInput(session, "boostproc7", label = "Boost #7 Procedures",
                        choices = finances$Name)
      updateSelectInput(session, "boostproc8", label = "Boost #8 Procedures",
                        choices = finances$Name)
    }
  })
  
  #####MODIFY#####
  # delete a row (single billed item)
  observeEvent(input$delrow, {
    # switch numeric (double) to integer
    rownum <- as.integer(input$rownumdel)
    
    # make sure number is valid
    assert_rownum(rownum)
    
    # delete row (N.B. CAN'T UNDO)
    finances <- finances[-rownum, ]
    
    # update table of procedures
    output$currinput <- renderTable(finances, rownames = TRUE)
  })
  
  # add a row (single billed item)
  observeEvent(input$addrow, {
    # switch volume from numeric (double) to integer
    vol <- as.integer(input$addvol)
    rev <- input$addrev
    name <- input$addname
    
    # make sure item is valid before adding
    assert_rowadd(name, vol, rev, finances$Name)
    
    # add row (bottom of df)
    temp_df <- tibble::tibble(Name = name, Volume = vol, Revenue = rev)
    finances <- rbind(finances, temp_df)
    
    # update table of procedures
    output$currinput <- renderTable(finances, rownames = TRUE)
  })
  
  #####EXPORT#####
  observeEvent(input$export, {
    output_boost_proc <- character(8) # change if number of boosts changes
    for (i in 1:8) {
      if (length(boost_proc) < i) {
        output_boost_proc[i] <- ""
      } else {
        output_boost_proc[i] <- paste(boost_proc[[i]], collapse = "|")
      }
    }
    
    df <- data.frame(Name = input$modelname,
                     Target_2020 = prerev2020,
                     Target_2021 = prerev2021,
                     Target_2022 = prerev2022,
                     Projected_2020 = postrev2020,
                     Projected_2021 = postrev2021,
                     Projected_2022 = postrev2022,
                     Growth_2020 = input$growth20,
                     Growth_2021 = input$growth21,
                     Growth_2022 = input$growth22,
                     Tech_Fee = input$techratio,
                     Jan = input$annualm1,
                     Feb = input$annualm2,
                     Mar = input$annualm3,
                     Apr = input$annualm4,
                     May = input$annualm5,
                     Jun = input$annualm6,
                     Jul = input$annualm7,
                     Aug = input$annualm8,
                     Sep = input$annualm9,
                     Oct = input$annualm10,
                     Nov = input$annualm11,
                     Dec = annualm12(),
                     Restore_Jan_2020 = coeffs_2020[1],
                     Restore_Feb_2020 = coeffs_2020[2],
                     Restore_Mar_2020 = coeffs_2020[3],
                     Restore_Apr_2020 = coeffs_2020[4],
                     Restore_May_2020 = coeffs_2020[5],
                     Restore_Jun_2020 = coeffs_2020[6],
                     Restore_Jul_2020 = coeffs_2020[7],
                     Restore_Aug_2020 = coeffs_2020[8],
                     Restore_Sep_2020 = coeffs_2020[9],
                     Restore_Oct_2020 = coeffs_2020[10],
                     Restore_Nov_2020 = coeffs_2020[11],
                     Restore_Dec_2020 = coeffs_2020[12],
                     Restore_Jan_2021 = coeffs_2021[1],
                     Restore_Feb_2021 = coeffs_2021[2],
                     Restore_Mar_2021 = coeffs_2021[3],
                     Restore_Apr_2021 = coeffs_2021[4],
                     Restore_May_2021 = coeffs_2021[5],
                     Restore_Jun_2021 = coeffs_2021[6],
                     Restore_Jul_2021 = coeffs_2021[7],
                     Restore_Aug_2021 = coeffs_2021[8],
                     Restore_Sep_2021 = coeffs_2021[9],
                     Restore_Oct_2021 = coeffs_2021[10],
                     Restore_Nov_2021 = coeffs_2021[11],
                     Restore_Dec_2021 = coeffs_2021[12],
                     Restore_Jan_2022 = coeffs_2022[1],
                     Restore_Feb_2022 = coeffs_2022[2],
                     Restore_Mar_2022 = coeffs_2022[3],
                     Restore_Apr_2022 = coeffs_2022[4],
                     Restore_May_2022 = coeffs_2022[5],
                     Restore_Jun_2022 = coeffs_2022[6],
                     Restore_Jul_2022 = coeffs_2022[7],
                     Restore_Aug_2022 = coeffs_2022[8],
                     Restore_Sep_2022 = coeffs_2022[9],
                     Restore_Oct_2022 = coeffs_2022[10],
                     Restore_Nov_2022 = coeffs_2022[11],
                     Restore_Dec_2022 = coeffs_2022[12],
                     Boost_Amt_1 = boost_amt[1],
                     Boost_Amt_2 = boost_amt[2],
                     Boost_Amt_3 = boost_amt[3],
                     Boost_Amt_4 = boost_amt[4],
                     Boost_Amt_5 = boost_amt[5],
                     Boost_Amt_6 = boost_amt[6],
                     Boost_Amt_7 = boost_amt[7],
                     Boost_Amt_8 = boost_amt[8],
                     Boost_Proc_1 = output_boost_proc[1],
                     Boost_Proc_2 = output_boost_proc[2],
                     Boost_Proc_3 = output_boost_proc[3],
                     Boost_Proc_4 = output_boost_proc[4],
                     Boost_Proc_5 = output_boost_proc[5],
                     Boost_Proc_6 = output_boost_proc[6],
                     Boost_Proc_7 = output_boost_proc[7],
                     Boost_Proc_8 = output_boost_proc[8],
                     Boost_Start_1 = boost_start[1],
                     Boost_Start_2 = boost_start[2],
                     Boost_Start_3 = boost_start[3],
                     Boost_Start_4 = boost_start[4],
                     Boost_Start_5 = boost_start[5],
                     Boost_Start_6 = boost_start[6],
                     Boost_Start_7 = boost_start[7],
                     Boost_Start_8 = boost_start[8],
                     Boost_End_1 = boost_end[1],
                     Boost_End_2 = boost_end[2],
                     Boost_End_3 = boost_end[3],
                     Boost_End_4 = boost_end[4],
                     Boost_End_5 = boost_end[5],
                     Boost_End_6 = boost_end[6],
                     Boost_End_7 = boost_end[7],
                     Boost_End_8 = boost_end[8]
    )
    
    filename <- paste0("../data-raw/",input$filename)
    if (!endsWith(x = filename, suffix = ".csv")) {
      filename <- paste0(filename, ".csv")
    }
    export_model(df, filename)
  })
}
