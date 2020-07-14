#####VIEW#####
fluidPage(
  tagList(
    tags$style("# params { display:none; }"),
    tabsetPanel(id = "params",
      # input for the model (file or manual)
      tabPanel("Import",
        fluidRow(
          column(4, fileInput("input", label = "")),
          column(2, actionButton("readfile", "Import Data")),
          column(2, htmlOutput("rownumdeltext")),
          column(2, numericInput("rownumdel", "", value = 1,
                                 min = 1)),
          column(2, actionButton("delrow", "Delete Item"))
        ),
        fluidRow(
          column(2, htmlOutput("rowaddtext")),
          column(2, textInput("addname", "Item Name")),
          column(2, numericInput("addvol", "Item Volume",
                                 value = 0, min = 0)),
          column(2, numericInput("addrev", "Item Revenue",
                                 value = 0, min = 0)),
          column(2, actionButton("addrow", "Add Item"))
        ),
        tableOutput("currinput")
      ),
      # financial model (not model from model-view-adapter)
      tabPanel("Model",
        fluidRow(
          column(3, htmlOutput("targetrev")),
          column(3, htmlOutput("projrev")),
          column(6, plotOutput("project12", height = "225px"))
        ),
        fluidRow(
          column(6, plotOutput("project24", height = "225px")),
          column(6, plotOutput("project36", height = "225px"))
        ),
        
        #####UI: GROWTH#####
        fluidRow(
          # column(12, verbatimTextOutput("growthheader")),
          column(3, actionButton("calc", "Calculate")),
          column(3, sliderInput("growth20", "Growth 2019-20", 
                                min = -10, max = 10, value = 0)),
          column(3, sliderInput("growth21", "Growth 2020-21",
                                min = -10, max = 10, value = 0)),
          column(3, sliderInput("growth22", "Growth 2021-22",
                                min = -10, max = 10, value = 0))
        ),
        
        #####UI: INSURANCE#####
        fluidRow(column(12, htmlOutput("instxt"))),
        fluidRow(
          column(3, "Types:"),
          column(2, "Medicare"),
          column(2, "Medicaid"),
          column(2, "Commercial"),
          column(2, "Other")
        ),
        fluidRow(
          column(3, "Compensation Ratio:"),
          column(2, "1"),  # ratio of medicaid compensation to itself is always 1
          column(2, numericInput("mcaidcomp", NULL, value = 0.63, min = 0)),
          column(2, numericInput("commcomp", NULL, value = 2.25, min = 0)),
          column(2, numericInput("noinscomp", NULL, value = 0, min = 0))
        ),
        fluidRow(
          column(3, "Proportion:"),
          column(2, numericInput("mcareprop", NULL, value = 0.47, min = 0, max = 1)),
          column(2, numericInput("mcaidprop", NULL, value = 0.14, min = 0, max = 1)),
          column(2, numericInput("commprop", NULL, value = 0.37, min = 0, max = 1)),
          column(2, textOutput("noinsprop"))
        ),
        # technical fee slider
        fluidRow(
          column(12, sliderInput("techratio", "Facility Fee Multiplier",
                        min = 0, max = 25, value = 10, width = "100%", step = 0.5))
        ),
        
        #####UI: ANNUAL BREAKDOWN#####
        fluidRow(
          column(12, htmlOutput("annualtxt")),
          column(2, numericInput("annualm1", "January",
                                 value = round(1/12, 5), min = 0, max = 1)),
          column(2, numericInput("annualm2", "February",
                                 value = round(1/12, 5), min = 0, max = 1)),
          column(2, numericInput("annualm3", "March",
                                 value = round(1/12, 5), min = 0, max = 1)),
          column(2, numericInput("annualm4", "April",
                                 value = round(1/12, 5), min = 0, max = 1)),
          column(2, numericInput("annualm5", "May",
                                 value = round(1/12, 5), min = 0, max = 1)),
          column(2, numericInput("annualm6", "June",
                                 value = round(1/12, 5), min = 0, max = 1)),
          column(2, numericInput("annualm7", "July",
                                 value = round(1/12, 5), min = 0, max = 1)),
          column(2, numericInput("annualm8", "August",
                                 value = round(1/12, 5), min = 0, max = 1)),
          column(2, numericInput("annualm9", "September",
                                 value = round(1/12, 5), min = 0, max = 1)),
          column(2, numericInput("annualm10", "October",
                                 value = round(1/12, 5), min = 0, max = 1)),
          column(2, numericInput("annualm11", "November",
                                 value = round(1/12, 5), min = 0, max = 1)),
          column(2, htmlOutput("annualm12"))
        ),
        
        #####UI: ACTIVITY RESTORATION#####
        fluidRow(
          column(12, htmlOutput("restoretxt")),
          column(4, textInput("restore12", "Restoration: 1-12 months",
                              value = paste(rep("1", 12), collapse = ","),
                              width = "100%")),
          column(4, textInput("restore24", "Restoration: 13-24 months",
                              value = paste(rep("1", 12), collapse = ","),
                              width = "100%")),
          column(4, textInput("restore36", "Restoration: 25-36 months",
                              value = paste(rep("1", 12), collapse = ","),
                              width = "100%"))
        ),
        
        #####UI: BOOST PROCEDURES#####
        fluidRow(
          # boost 1-4 amounts
          column(3, numericInput("boost1", "Boost #1 Amount", value = 0)),
          column(3, numericInput("boost2", "Boost #2 Amount", value = 0)),
          column(3, numericInput("boost3", "Boost #3 Amount", value = 0)),
          column(3, numericInput("boost4", "Boost #4 Amount", value = 0)),
          
          # boost 1-4 procedures
          column(3, selectInput("boostproc1", "Boost #1 Procedures",
                                choices = c("Sample Procedure 1",
                                            "Sample Procedure 2",
                                            "Sample Procedure 3"), multiple = TRUE)),
          column(3, selectInput("boostproc2", "Boost #2 Procedures",
                                choices = c("Sample Procedure 1",
                                            "Sample Procedure 2",
                                            "Sample Procedure 3"), multiple = TRUE)),
          column(3, selectInput("boostproc3", "Boost #3 Procedures",
                                choices = c("Sample Procedure 1",
                                            "Sample Procedure 2",
                                            "Sample Procedure 3"), multiple = TRUE)),
          column(3, selectInput("boostproc4", "Boost #4 Procedures",
                                choices = c("Sample Procedure 1",
                                            "Sample Procedure 2",
                                            "Sample Procedure 3"), multiple = TRUE)),
          
          # boost 1-4 durations
          column(3, dateRangeInput("boostdur1", "Boost #1 Duration",
                                   start = "2020-01-01", end = "2022-12-31",
                                   min = "2020-01-01", max = "2022-12-31",
                                   format = "M-yyyy")),
          column(3, dateRangeInput("boostdur2", "Boost #2 Duration",
                                   start = "2020-01-01", end = "2022-12-31",
                                   min = "2020-01-01", max = "2022-12-31",
                                   format = "M-yyyy")),
          column(3, dateRangeInput("boostdur3", "Boost #3 Duration",
                                   start = "2020-01-01", end = "2022-12-31",
                                   min = "2020-01-01", max = "2022-12-31",
                                   format = "M-yyyy")),
          column(3, dateRangeInput("boostdur4", "Boost #4 Duration",
                                   start = "2020-01-01", end = "2022-12-31",
                                   min = "2020-01-01", max = "2022-12-31",
                                   format = "M-yyyy")),
          
          # boost 5-8 amounts
          column(3, numericInput("boost5", "Boost #5 Amount", value = 0)),
          column(3, numericInput("boost6", "Boost #6 Amount", value = 0)),
          column(3, numericInput("boost7", "Boost #7 Amount", value = 0)),
          column(3, numericInput("boost8", "Boost #8 Amount", value = 0)),
          
          # boost 5-8 procedures
          column(3, selectInput("boostproc5", "Boost #5 Procedures",
                                choices = c("Sample Procedure 1",
                                            "Sample Procedure 2",
                                            "Sample Procedure 3"), multiple = TRUE)),
          column(3, selectInput("boostproc6", "Boost #6 Procedures",
                                choices = c("Sample Procedure 1",
                                            "Sample Procedure 2",
                                            "Sample Procedure 3"), multiple = TRUE)),
          column(3, selectInput("boostproc7", "Boost #7 Procedures",
                                choices = c("Sample Procedure 1",
                                            "Sample Procedure 2",
                                            "Sample Procedure 3"), multiple = TRUE)),
          column(3, selectInput("boostproc8", "Boost #8 Procedures",
                                choices = c("Sample Procedure 1",
                                            "Sample Procedure 2",
                                            "Sample Procedure 3"), multiple = TRUE)),
          
          # boost 5-8 durations
          column(3, dateRangeInput("boostdur5", "Boost #5 Duration",
                                   start = "2020-01-01", end = "2022-12-31",
                                   min = "2020-01-01", max = "2022-12-31",
                                   format = "M-yyyy")),
          column(3, dateRangeInput("boostdur6", "Boost #6 Duration",
                                   start = "2020-01-01", end = "2022-12-31",
                                   min = "2020-01-01", max = "2022-12-31",
                                   format = "M-yyyy")),
          column(3, dateRangeInput("boostdur7", "Boost #7 Duration",
                                   start = "2020-01-01", end = "2022-12-31",
                                   min = "2020-01-01", max = "2022-12-31",
                                   format = "M-yyyy")),
          column(3, dateRangeInput("boostdur8", "Boost #8 Duration",
                                   start = "2020-01-01", end = "2022-12-31",
                                   min = "2020-01-01", max = "2022-12-31",
                                   format = "M-yyyy"))
        )
      ),
      tabPanel("Export",
        fluidRow(
          column(4, textInput("modelname", "Model Name")),
          column(4, textInput("filename", "File Name")),
          column(1, actionButton("export", "Export Parameters"))
        )
      )
    )
  )
)
