library(shiny)
# library(shinyjs)
library(shinydashboard)
# library(shinydashboardPlus)
# library(shinycssloaders)
library(DT)

library(tidyverse)
library(lubridate)
library(readxl)
library(zoo)
library(here)
# library(patchwork)
library(ggpubr)
library(ggthemes)



options(shiny.maxRequestSize=20 * 1024^2) # 10 MB Upload Limit


# TODO Date selection for TAT


source("src/analytics.R")


number_format <- function(x, n = 1) format(round(x, n), nsmall = n)


# Constants for checking excel files 
# spec_file_sheets <- c("Device Def", "Shift Def", "Test Group Def", "Test Group Test Def") 
spec_file_sheets <- c("Device Def", "Test Group Def") 

def_file_devices_df_names <- c("device", "sub_device", "include", "test_per_hour")

def_file_lab_test_groups_df_names <- c("test_group", "include", "target_TAT_inlab", "target_TAT_inlab_stat")

lab_data_file_df_names <- c("index", "patient_id", "sample_id", "device", "sub_device", 
                            "sampling_time", "receiving_time", "device_order_time", "sub_device_order_time", 
                            "result_time", "first_validation_time", "validation_time", "first_validation_staff", 
                            "validation_staff", "test_group", "test_id", "test_name")

plot_colors <- c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69')

# helper functions --------------------------------------------------------


read_lab_spec_file <- function(file_name) {
    
    t <- read_excel(file_name) # file_name<-"ClinLabTool/demo_data/0_demo_definitions.xlsx"
    
    # t <- read_excel("ClinLabTool/demo_data/0_demo_definitions.xlsx")
    

    #  Check spec file sheet names if avalaible
    sheet_names <- excel_sheets(file_name)
     if (! all(spec_file_sheets %in% sheet_names )) {
       return(list(error_text = "Wrong Excel definition file. Sheet names do not match!"))
     }
    
    
    
    

    #  Check spec file column names if avalaible
    
    
    devices <- openxlsx::read.xlsx(file_name,"Device Def" ) |> 
        tibble() |> 
        replace_na(list(sub_device  =""))
   
    
    #  Check spec file column names if avalaible
    if (! all(def_file_devices_df_names  %in% names(devices))) {
      return(list(error_text = "Wrong Excel definition file. Devices sheet does not have correct columns!"))
    }
    
    
    lab_test_group <- openxlsx::read.xlsx(file_name,"Test Group Def" ) |> tibble()
    
    #  Check spec file column names if avalaible
    if (! all(def_file_lab_test_groups_df_names  %in% names(lab_test_group))) {
      return(list(error_text = "Wrong Excel definition file. Test Group sheet does not have correct columns!"))
    }
    
    # TODO  Check spec file column names if avalaible
    # shift_def <- openxlsx::read.xlsx(file_name,"Shift Def" ) |> 
    #     tibble()  
    shift_def <- data.frame()

    # TODO  Check spec file column names if avalaible
    # lab_test_group_name <- openxlsx::read.xlsx(file_name,"Test Group Test Def" ) |> 
    #   tibble()
    lab_test_group_name <- data.frame()
    
    
    list(error_text = "",
         devices= devices, shift_def= shift_def, lab_test_group = lab_test_group, lab_test_group_name = lab_test_group_name)

    
}


read_lab_data_file <- function(file_name) {
    
    t <- readxl::read_excel(file_name)  # file_name<-"ClinLabTool/demo_data/0_lab data an.xlsx"
    
    
    
    # Check data file column names if avalaible
    if (! all(lab_data_file_df_names  %in% names(t))) {
      return(list(error_text = "Wrong Excel Laboratory Data file. Lab Data sheet does not have correct columns!"))
    }
    
    
    list(data = t, error_text = "")
    
    
}


# UI ----------------------------------------------------------------------


ui <- dashboardPage(
    
    dashboardHeader(title = "Lab Analytics"),
    
# dashboard side -----------------------------------------------------------
    dashboardSidebar(sidebarMenu(id = "tabs",
                      fluidRow(column(12, hr())),
                                 menuItem("Instructions", tabName = "mi_instructions"),
                                 menuItem("Data Viewer", tabName = "mi_table_viewer"),              
                                 menuItem("Data Summary", tabName = "mi_summary"),
                                 menuItem("TAT Analytics", tabName = "mi_TAT", 
                                          menuSubItem("Test Group", tabName = "mi_sub_TAT_test_group"),
                                          menuSubItem("Test Name", tabName = "mi_sub_TAT_test_name"),
                                          menuSubItem("Daily", tabName = "mi_sub_TAT_hourly")
                                          ),
                                 menuItem("Device Analytics", tabName = "mi_device")
                     ,
                     fluidRow(column(12, hr())),
                     fluidRow(column(12, htmlOutput(outputId = "txt_is_specs_loaded"))),
                     fluidRow(column(12, htmlOutput(outputId = "txt_is_data_loaded"))),
                     fluidRow(column(12, hr())),
                     fluidRow(column(12, fileInput("f_lab_spec", "Upload Lab Specs", accept = c(".xlsx"), width = "70%"))),
                     fluidRow(column(12, fileInput("f_lab_data", "Upload Lab Data", accept = c(".xlsx"), width = "70%"))),
                     fluidRow(column(12, htmlOutput(outputId = "txt_file_err"))),
                     fluidRow(column(12, hr())),
                     disable = FALSE,collapsed = FALSE, minified = FALSE)

                     
                     
                     # actionButton("btn_demo", span("Load Demo Lab Data", id = "UpdateAnimate", class = "loading dots")), 

       
    ),
    
# dashboard body -----------------------------------------------------------
   
    dashboardBody(
        tabItems(
          tabItem("mi_instructions", 
                  fluidRow(h1("Instructions")),
                  fluidRow(column(12,helpText(paste("Instructons are avaiable as PDF File or "),
                                              a("Click Here",     href="https://github.com/ditopcu/ClinLabTool/blob/main/Introduction%20Instructions.pdf"))
                                              )),
                  fluidRow(column(10,downloadButton(outputId = "btn_dl_instructions",label =  "Download Instructions PDF") )),
                  fluidRow(column(12,helpText("Lab data and specs can be uploaded by an Excel (xlsx) file."))),
                  fluidRow(column(12,helpText("Shiny is limited to 20 MB file size. But this can be increased"))),
                  fluidRow(column(12,helpText("Template files are avalaible. Downlad template files from:"))),
                  fluidRow(column(12,helpText(""))),
                  fluidRow(column(1,helpText(" ")), column(10,downloadButton(outputId = "btn_dl_template_specs",label =  "  Definition Template File") )),
                  fluidRow(column(12,helpText(" "))),
                  fluidRow(column(1,helpText(" ")), column(10,downloadButton(outputId = "btn_dl_template_data",label =  "  Data Template File") )),
                  fluidRow(column(12, hr())),
                  fluidRow(column(12,helpText("Demo lab data is also avalaible."))),
                  actionButton(inputId = "btn_demo",label =  "Load Demo Lab Data & Spec"),
                  fluidRow(column(12, hr())),
                  helpText(   a("Click Here for the Github page",     href="https://github.com/ditopcu/ClinLabTool")),
                  ),  
          tabItem("mi_table_viewer", 
                fluidRow(h1("Loaded Data")),
                fluidRow(h3("Lab Specs")),
                fluidRow(column(12,htmlOutput(outputId = "txt_ovtab_no_lab_specs"))),
                fluidRow(column(width = 6, DTOutput("DT_lab_specs"))),
                fluidRow(h3("Lab Data")),
                fluidRow(column(12,(htmlOutput(outputId = "txt_ovtab_no_lab_data")))),
                fluidRow(column(width = 12, DTOutput("DT_data")))
                ),
            tabItem("mi_summary", 
                    h2("Data Summary"),
                    fluidRow(column(6, htmlOutput(outputId = "txt_sumTab_sumText")),
                             column(6, htmlOutput(outputId = "txt_sumTab_date_range")))),
            # tabItem("mi_TAT", h2("TAT tab content")),
            tabItem("mi_sub_TAT_test_group", 
                    fluidRow(h2("Turn-around Time (TAT) by Test Group")),
                    radioButtons("radio_test_group_outlier", label = h3("Outlier Exclusion"),choices = list("No" = 1, "Yes (Tukey)" = 2), 
                                 selected = 1, inline = TRUE),
                    fluidRow( htmlOutput(outputId = "txt_TAT_test_group")),
                    fluidRow(column(7, DTOutput("DT_TAT_test_group"))) ,
                    fluidRow(column(12, plotOutput("plot_TAT_test_group"))) 
                    ),
            tabItem("mi_sub_TAT_test_name", 
                    fluidRow(h2("Turn-around Time (TAT) by Test")),
                    fluidRow(selectInput("slt_TAT_test_group", "Test Group", choices = "", width = "100%", multiple = FALSE, selectize = TRUE)),
                    fluidRow(selectInput("slt_TAT_test", "Test", choices = "", width = "100%", multiple = TRUE, selectize = TRUE)),
                    radioButtons("radio_test_outlier", label = h3("Outlier Exclusion"),choices = list("No" = 1, "Yes (Tukey)" = 2), 
                                 selected = 1, inline = TRUE),
                    fluidRow( htmlOutput(outputId = "txt_TAT_test")),
                    fluidRow(column(7, DTOutput("DT_TAT_test"))) ,
                    fluidRow(column(12, plotOutput("plot_TAT_test")))                    
                    ),
            tabItem("mi_sub_TAT_hourly", 
                    fluidRow(h2("Turn-around Time (TAT) by Hour")),
                    fluidRow(
                      column(3, dateRangeInput("daterange_TAT_hourly", label = h3("Select Date Range"))),
                      column(3, radioButtons("radio_TAT_hourly_type", label = h3("Summary Type"),choices = list("By day" = 1, "By week day"=2,  "Overall" = 3), inline = TRUE)) 
                      #column(4, selectInput("slt_TATDaily_test_group", h3("Test Group"), choices = "", width = "100%", multiple = FALSE, selectize = TRUE)),
                    ),
                    fluidRow(column(12, DTOutput("DT_TAT_hourly"))),
                    fluidRow(column(12,htmlOutput(outputId = "txt_TAT_hourly_text"))),
                    fluidRow(column(12, plotOutput("plot_TAT_hourly") ))
                      
                    ),
            tabItem("mi_device", 
                    fluidRow(column(3, dateRangeInput("daterange_device_load", label = h3("Select Date Range")))),
                    fluidRow(
                      column(3, selectInput("slt_device_load_devices",label =  h3("Device"), choices = "", width = "100%", multiple = FALSE, selectize = TRUE)),
                      column(3, selectInput("slt_device_load_subdevices",label =  h3("Sub Device"), choices = "", width = "100%", multiple = FALSE, selectize = TRUE)),
                      fluidRow(
                        column(3, radioButtons("radio_device_sum", label = h3("Summary Type"),choices = list("By week day"= 1, "Overall" = 2), inline = TRUE)),
                        column(3, radioButtons("radio_info_type", label = h3("Information Type"),choices = list("Test Count"= 1, "Device Utilization" = 2), inline = TRUE))
                               )
                      ),
                    fluidRow(column(12, plotOutput("plot_device") )),
                    fluidRow(column(12, DTOutput("DT_device")))
                    
                    )
        
        ) # end   tabItems      
    ) # end dashboardBody

) # end dashboardPage

# server ------------------------------------------------------------------
server <- function(input, output, session) {
    

# reactive values and expressions -----------------------------------------

    
    rv <- reactiveValues(specs_loaded  = FALSE, 
                         lab_specs_devices_df= NULL, lab_specs_shifts = NULL,  lab_specs_test_groups = NULL,
                         
                         data_loaded = FALSE, lab_data_df = NULL, 
                         
                         lab_analyse_df = NULL,  
                         lab_data_count = NULL, lab_data_receive_min_date = NULL,lab_data_receive_max_date = NULL,
                         lab_data_test_groups = NULL, 
                         
                         TAT_data = NULL, TAT_sum_test_group = NULL, TAT_sum_test = NULL,
                         TAT_hourly_multiple_sum = NULL, TAT_hourly_week_day_sum= NULL,  TAT_hourly_overall_sum = NULL,
                         
                         devices_names  = NULL, subdevices_names = NULL,
                         lab_data_result_min_date = NULL,lab_data_result_max_date = NULL,
                         device_analyse_df =NULL, device_analyse_week_day_sum = NULL,device_analyse_overall_sum = NULL,
  
                         
                         test_choices = NULL)
    
    
    device_analyse_data <- reactive({
      

      req(input$slt_device_load_devices)
      req(input$slt_device_load_subdevices)
      
      
      date_range_start <- input$daterange_device_load[1]
      date_range_stop <- input$daterange_device_load[2]
      
      
      t <- rv$lab_analyse_df |> #TODO Sub device NA
        filter(receiving_time >= date_range_start, receiving_time <= date_range_stop) |> 
        filter(device == input$slt_device_load_devices, sub_device == input$slt_device_load_subdevices) |> 
        get_device_analyse_data(rv$lab_specs_devices_df)
      
   
       rv$device_analyse_week_day_sum <- summarise_device_load_weekday(t)
       rv$device_analyse_overall_sum <- summarise_device_load_overall(t)
      
      
      
    }) 
    
    
    react_lab_specs_devices <- reactive({
        req(rv$lab_specs_devices_df)
        rv$specs_loaded <- TRUE
        rv$lab_specs_devices_df
    })

    react_lab_data<- reactive({
        req(rv$lab_data_df)
        rv$data_loaded <- TRUE
        rv$lab_data_df
        
    })
    
    react_analyse_data <- reactive({
        req(rv$lab_specs_devices_df)
        req(rv$lab_data_df)
        
        my_devices <- rv$lab_specs_devices_df
        data_temp <- react_lab_data()
        

        # print(names(data_temp))
        
        analyse_data <- data_temp    |> 
            filter(device  %in% my_devices$device) |>
            mutate(devices = paste0(device," ", sub_device)) |>
            filter(devices   %in% paste0(my_devices$device," ", my_devices$sub_device) ) |> 
            select(-devices)

        test_groups <- distinct(analyse_data, test_group) |> pull(test_group)
        devices <- distinct(analyse_data, device ) |> pull(device )
        
        rv$lab_data_test_groups <-  rv$lab_data_test_groups 
        rv$lab_data_count <- nrow(analyse_data)
        rv$TAT_data <- calc_TAT(analyse_data)
        rv$devices_names <- devices
        rv$lab_data_receive_min_date <- as_date(min(analyse_data$receiving_time))
        rv$lab_data_receive_max_date <- as_date(max(analyse_data$receiving_time))
        
        rv$lab_data_result_min_date <- as_date(min(analyse_data$result_time))
        rv$lab_data_result_max_date <- as_date(max(analyse_data$result_time))
        
        
          
          
        rv$TAT_sum_test_group <- analyse_data |> 
          calc_TAT() |> 
          summarise_TAT(type = "by_test_group", exclude_outlier = FALSE)
        
        rv$TAT_sum_test <- analyse_data |> 
          calc_TAT() |> 
          summarise_TAT(type = "by_test", exclude_outlier = FALSE)


        
        rv$lab_analyse_df <- analyse_data

        updateSelectInput(session, inputId = "slt_device_load_devices", choices = devices, label = "Device" )
        updateSelectInput(session, inputId = "slt_TAT_test_group", choices = test_groups, label = "Test Group" )
        updateDateRangeInput(session,inputId = "daterange_TAT_hourly", start =rv$lab_data_receive_min_date, min = rv$lab_data_receive_min_date,
                             end = rv$lab_data_receive_max_date, max = rv$lab_data_receive_max_date)
        updateDateRangeInput(session,inputId = "daterange_device_load", start =rv$lab_data_result_min_date, min = rv$lab_data_result_min_date,
                             end = rv$lab_data_result_max_date, max = rv$lab_data_result_max_date)
        
        # shinyjs::enable("btnUpdate")
        # shinyjs::removeClass(id = "UpdateAnimate", class = "loading dots")
        
        analyse_data
    })
# Buttons  --------------------------------------------------------------
    # Demo button
    observeEvent(input$btn_demo, {
   
      # shinyjs::addClass(id = "UpdateAnimate", class = "loading dots")
      # shinyjs::disable("btn_demo")
      
      print("Demo data loading")
      updateTabItems(session, "tabs", selected = "mi_table_viewer")
      
      temp <- suppressMessages(read_lab_spec_file("demo_data/0_demo_definitions.xlsx"))
      
      rv$lab_specs_devices_df <- temp$devices
      rv$lab_specs_shifts <- temp$shift_def
      rv$lab_specs_test_groups <- temp$lab_test_group_name
      rv$specs_loaded <- TRUE
      
      
      temp <- suppressMessages(read_lab_data_file("demo_data/0_lab data an.xlsx"))
      
      rv$lab_data_df <- temp$data
      rv$data_loaded <- TRUE
      
      if (rv$data_loaded & rv$specs_loaded ) {
        # print("data")
        rv$lab_analyse_df <- react_analyse_data()
      }
      

      
    })
    
    
    # TODO Template Files Download
    
# Selection lists observe  -------------------------------------------------------
    

    observeEvent(input$slt_device_load_devices, {
      
      req(rv$lab_analyse_df)
      req(input$slt_device_load_devices )
      
      sub_device_names <- rv$lab_analyse_df    |> 
        filter(device %in% input$slt_device_load_devices) |> 
        distinct(sub_device) |> 
        replace_na(list(sub_device = "N/A")) |> 
        pull(sub_device)  
      
      rv$subdevices_names <- sub_device_names
      

      updateSelectInput(session, inputId = "slt_device_load_subdevices", choices =rv$subdevices_names, selected =rv$subdevices_names[1],  label = "Sub Device" )
 
      
      
    })
    
    observeEvent(input$slt_TAT_test_group, {
      
      req(rv$TAT_sum_test )
      req(input$slt_TAT_test_group )
     
      test_names <- rv$TAT_sum_test    |> 
        filter(test_group %in% input$slt_TAT_test_group) |> 
        distinct(test_name) |> 
        pull(test_name)  
      
      rv$test_choices <- c("Select All", "Select None", test_names)
      
    updateSelectInput(session, inputId = "slt_TAT_test", choices =rv$test_choices, selected =rv$test_choices[3],  label = "Test" )

      
    })
    
    
    observeEvent(input$slt_TAT_test, {
      
      if("Select All" %in% input$slt_TAT_test)
        selected_choices=rv$test_choices[c(-1, -2)] # choose all the choices _except_ "Select All"
      else if ("Select None" %in% input$slt_TAT_test)
        selected_choices= rv$test_choices[3] # update the select input with choice selected by user
      else 
        selected_choices=input$slt_TAT_test # update the select input with choice selected by user
      
      updateSelectInput(session, "slt_TAT_test", selected = selected_choices)
      
    })
    
    
  
# Date time observe  -------------------------------------------------------
 
    observeEvent(input$daterange_device_load, {
      
      req(input$daterange_device_load)
      req(rv$lab_analyse_df)
      
      date_range_start <- input$daterange_device_load[1]
      date_range_stop <- input$daterange_device_load[2]
      
      devices <- rv$lab_analyse_df |> 
        filter(receiving_time >= date_range_start, receiving_time <= date_range_stop) |> 
        distinct(device ) |> 
        pull(device )
      
      rv$devices_names <- devices
      
      
      updateSelectInput(session, inputId = "slt_device_load_devices", choices = devices, label = "Device" )


      
    })
    
    observeEvent(input$daterange_TAT_hourly, {
      
      req(input$daterange_TAT_hourly)
      req(rv$TAT_data)
      
      date_range_start <- input$daterange_TAT_hourly[1]
      date_range_stop <- input$daterange_TAT_hourly[2]
      
      rv$TAT_hourly_multiple_sum <- rv$TAT_data |> 
        filter(receiving_time >= date_range_start, receiving_time <= date_range_stop) |> 
        summarise_TAT_hourly_multiple() 
      
      rv$TAT_hourly_week_day_sum <- rv$TAT_data |> 
        filter(receiving_time >= date_range_start, receiving_time <= date_range_stop) |> 
        summarise_TAT_hourly_weekday() 
      
      
      rv$TAT_hourly_overall_sum <- rv$TAT_data |> 
        filter(receiving_time >= date_range_start, receiving_time <= date_range_stop) |> 
        summarise_TAT_hourly_overall() 
      
    })
    
    
# text outputs ------------------------------------------------------------
  
    output$txt_ovtab_no_lab_specs <- renderText({
        
        if (is.null(rv$lab_specs_devices_df)) {
            "No Spec Loaded"
        } else {
            ""
        }
        
        })
    
    output$txt_is_specs_loaded <- renderText({
        
        if (is.null(rv$lab_specs_devices_df)) {
            "Lab Specs: Not Loaded"
        } else {
            "Lab Specs: Ok"
        }
        
    })
    
    output$txt_ovtab_no_lab_data <- renderText({
        
        if (is.null(rv$lab_data_df)) {
            "No Lab  Loaded"
        } else {
            ""
        }
        
    })
    
    output$txt_is_data_loaded <- renderText({
        
        
        
        if (is.null(rv$lab_data_df)) {
            "Lab Data: Not Loaded"
        } else {
            "Lab Data: Ok"
        }
        
    })

    output$txt_sumTab_sumText <- renderText({
        
        req(react_analyse_data())
        

        
        test_count <- paste0("<b>Number of tests: </b>", as.character(rv$lab_data_count))
        start_date <- paste0("<b>Start date: </b>", as.character(rv$lab_data_receive_min_date ))
        end_date <- paste0("<b>End date: </b>", as.character(rv$lab_data_receive_max_date ))
        
        device_names <- paste0("<b>Devices: </b>", "<br/>", paste("&nbsp&nbsp", rv$devices_names, collapse = "<br/>"))
        test_groups <- paste0("<b>Test groups: </b>", "<br/>", paste("&nbsp&nbsp", rv$lab_data_test_groups, collapse = "<br/>"))
      
        HTML(paste(test_count, 
                   start_date,
                   end_date,
                   device_names,
                   sep="<br/>"))
        

        
    })
    
    

# file operations--------------------------------------------------------------
    
    
    
    
    output$btn_dl_instructions <- downloadHandler(    
      filename = "ClinLabtool Instructions.pdf",
      content =  function(file){
        file.copy("Introduction Instructions.pdf", file)
      }
    )
    

    output$btn_dl_template_specs <- downloadHandler(    
      filename = "ClinLabtool Lab Specs Excel Template File.xlsx",
      content =  function(file){
        file.copy("demo_data/1_TEMPLATE_definitions.xlsx", file)
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )
    
    output$btn_dl_template_data <- downloadHandler(    
      filename = "ClinLabtool Lab Data Excel Template File.xlsx",
      content =  function(file){
        file.copy("demo_data/1_TEMPLATE Data.xlsx", file)
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )
      
    
    
    # Read lab specs
    observeEvent(input$f_lab_spec, {

        updateTabItems(session, "tabs", selected = "mi_table_viewer")
      
        file <- input$f_lab_spec
        ext <- tools::file_ext(file$datapath)
        
        if (ext != "xlsx") {
            
            output$txt_ovtab_no_lab_specs <- renderText("   Wrong file type")
            return(0)
            
        }
        
        validate(need(ext == "xlsx", "Please upload a xlsx file"))

        output$txt_ovtab_no_lab_specs <- renderText("")
        
        temp <- suppressMessages(read_lab_spec_file(file$datapath))
        
   

        if (temp[["error_text"]] != "") {
            # output$txt_ovtab_no_lab_specs <- renderText(temp[["error_text"]])
          output$txt_ovtab_no_lab_specs <- renderText({ paste("<font color=\"#FF0000\"><b>", temp[["error_text"]], "</b></font>") })
            return()
        }
        
        
        # Read Specifications
        rv$lab_specs_devices_df <- temp$devices
        rv$lab_specs_shifts <- temp$shift_def
        rv$lab_specs_test_groups <- temp$lab_test_group_name
        
        rv$specs_loaded <- TRUE
        
        # test_groups <- distinct(analyse_data, test_group) |> pull(test_group)
        # updateSelectInput(session, inputId = "slt_TAT_test_group", choices = test_groups, label = "Test Group" )
        
        if (rv$data_loaded & rv$specs_loaded ) {
          # print("data")
          rv$lab_analyse_df <- react_analyse_data()
          }

        
    }) # end observeEvent file upload lab spec
    
    # Read lab data
    observeEvent(input$f_lab_data, {
      
      updateTabItems(session, "tabs", selected = "mi_table_viewer")
        
        file <- input$f_lab_data
        ext <- tools::file_ext(file$datapath)
        
        if (ext != "xlsx") {
            
            output$txt_file_err <- renderText("   Wrong file type")
            return(0)
            
        }
        
        validate(need(ext == "xlsx", "Please upload a xlsx file"))
        
        output$txt_file_err <- renderText("")
        
        temp <- suppressMessages(read_lab_data_file(file$datapath))
        
        
        
        if (temp[["error_text"]] != "") {
            # output$txt_file_err <- renderText()
            
          output$txt_ovtab_no_lab_data <- renderText({ paste("<font color=\"#FF0000\"><b>", temp[["error_text"]], "</b></font>") })
            return(0)
        }
        
        output$txt_ovtab_no_lab_data <- renderText("")
        
        rv$lab_data_df <- temp$data
        rv$data_loaded <- TRUE
        
        if (rv$data_loaded & rv$specs_loaded ) {
          # print("data")
          rv$lab_analyse_df <- react_analyse_data()
        }
        
        
    }) # end observeEvent file upload lab spec
    
    # TODO Download Spec Template
    # TODO Download Data Template
    
    



# Plots -------------------------------------------------------------------
    
    
    
    output$plot_device <- renderPlot({
      
      req(device_analyse_data())
      
      if (input$radio_info_type == 1) {
        
        stat_type = as.symbol("test_count")
        y_lab <- "Test Count"
        
      } else {
        stat_type = as.symbol("used_device_capacity")
        y_lab <- "Utilization (%)"
      }
      
      
      
      
      if (input$radio_device_sum == 1) { # weekday
        
        rv$device_analyse_week_day_sum |> 
          mutate(x_lab= hms::hms(hours = hour)) |> 
          ggplot(aes(x = x_lab, y = {{stat_type}} , color= w_day  ))+
          # geom_bar(stat = "identity", position = "fill") +
          geom_line(size = 1.1) +
          scale_x_time(labels = format_hm, breaks =  hms::hms(hour = 0:23 )) +
          scale_color_discrete(type = plot_colors ) + 
          theme_pubclean() +
          xlab("") + ylab(y_lab) +
          theme_pubclean() +
          theme(legend.position = "bottom") +
          theme(axis.text.x = element_text(size = "14"),
                axis.text.y = element_text(size = "14"),
                #axis.text.x = element_blank(),
                axis.title.y = element_text(size = "14"),
                strip.text.x = element_text(size = "14"))
        
          
        
      } else if (input$radio_device_sum == 2) {# overall
        
        rv$device_analyse_overall_sum |> 
          mutate(x_lab= hms::hms(hours = hour)) |> 
          ggplot(aes(x = x_lab, y = {{stat_type}}   ))+
          geom_bar(stat = "identity", fill= "cornflowerblue") +
          scale_x_time(labels = format_hm, breaks =  hms::hms(hour = 0:23 )) +
          theme_pubclean() + xlab("") + ylab(y_lab) +
          theme(legend.position = "bottom") +
          theme(axis.text.x = element_text(size = "14"),
                axis.text.y = element_text(size = "14"),
                axis.title.y = element_text(size = "14"),
                strip.text.x = element_text(size = "14"))
        
      } 
      
      
      
      
      
    })
    
    
    output$plot_TAT_hourly <- renderPlot({
      
      req(rv$TAT_data)
      
      
      if (input$radio_TAT_hourly_type == 1) { # multiple
        
        # TODO No plot available
        
        output$txt_TAT_hourly_text <- renderText("No plot available for this summary type")
        
        
        return(NULL)
        
      } else if (input$radio_TAT_hourly_type == 2) {# weekday
        
        output$txt_TAT_hourly_text <- renderText("")
        
        rv$TAT_hourly_week_day_sum  |> 
          rename(Weekday = w_day) |> 
          mutate(x_lab= hms::hms(hours = hour)) |> 
          ggplot(aes(x = x_lab, y = median_TAT , color= Weekday  ))+
          geom_line(size = 1.1) +
          scale_x_time(labels = format_hm, breaks =  hms::hms(hour = 0:23 )) +
          scale_color_discrete(type = plot_colors ) + 
          theme_pubclean() +
          theme(legend.position = "bottom") +
          xlab("") + ylab("Median TAT") +
          theme(axis.text.x = element_text(size = "14"),
                axis.text.y = element_text(size = "14"),
                axis.title.y = element_text(size = "14"),
                strip.text.x = element_text(size = "14"))
        
        
        
        
        
        
      } else if (input$radio_TAT_hourly_type == 3) { # overall
        
        output$txt_TAT_hourly_text <- renderText("")
        
          rv$TAT_hourly_overall_sum |> 
            mutate(x_lab= hms::hms(hours = hour)) |> 
            ggplot(aes(x = x_lab, y = median_TAT   ))+
            geom_bar(stat = "identity", fill= "cornflowerblue") +
            scale_x_time(labels = format_hm, breaks =  hms::hms(hour = 0:23 )) +
            # scale_fill_discrete(type = plot_colors ) + 
            theme_minimal() + 
            theme_pubclean() +
            xlab("") + ylab("Median TAT")  +
            theme(axis.text.x = element_text(size = "14"),
                  axis.text.y = element_text(size = "14"),
                  axis.title.y = element_text(size = "14"),
                  strip.text.x = element_text(size = "14"))
          
          

        
        
      }
      
      

      
      
    })
    

    # TAT Test plot  
    output$plot_TAT_test <- renderPlot({
      
      req(rv$TAT_data)

      
      if (input$radio_test_outlier == 1) {
        
        plot_data <- rv$TAT_data    |> 
          filter(test_group %in% input$slt_TAT_test_group) |> 
          filter(test_name %in% input$slt_TAT_test) 
        
        
      } else if (input$radio_test_outlier == 2) {
        
        plot_data <- rv$TAT_data    |> 
          filter(test_group %in% input$slt_TAT_test_group) |> 
          filter(test_name %in% input$slt_TAT_test) |> 
          TAT_outlier_calc(type = "by_test") |> 
          filter(inlab_TAT >=  low_limit, inlab_TAT <=high_limit)  |> 
          select(test_id, test_name, test_group, inlab_TAT) 
        
      } 
      

      
      plot_data |> 
        ggplot(aes(x = test_name, y = inlab_TAT  )) +
        geom_boxplot() +
        # facet_wrap(.~test_name, scales = "free") +
        ylab("Hours") + xlab("")+
        coord_flip() +
        theme_pubclean() +
        theme(axis.ticks.x = element_blank(),
              axis.text.y = element_text(size = "14"),
              axis.title.y = element_text(size = "14"),
              strip.text.x = element_text(size = "14"))
      
    }) 
    
    # TAT Test group  Plot
    output$plot_TAT_test_group <- renderPlot({

      req(rv$TAT_data)
      
      
      if (input$radio_test_group_outlier == 1) { # no outlier exclusion
        plot_data <- rv$TAT_data    
      } else if (input$radio_test_group_outlier == 2) {# outlier exclusion within test
        plot_data <- rv$TAT_data    |> 
          TAT_outlier_calc(type = "by_test_group") |> 
          filter(inlab_TAT >=  low_limit, inlab_TAT <=high_limit)  |> 
          select(test_id, test_name, test_group, inlab_TAT) 
      }
      
      plot_data |> 
        ggplot(aes(x = test_group, y = inlab_TAT  )) +
        geom_boxplot() +
        # facet_wrap(.~test_name, scales = "free") +
        ylab("Hours") + xlab("")+
        coord_flip() +
        theme_pubclean() +
        theme(axis.ticks.x = element_blank(),
              axis.text.y = element_text(size = "14"),
              axis.title.y = element_text(size = "14"),
              strip.text.x = element_text(size = "14"))

      # plot_data|> 
      #   ggplot(aes(y = (inlab_TAT)  )) +
      #   geom_boxplot() +
      #   facet_wrap(.~test_group, scales = "free") +
      #   ylab("Hours") + 
      #   theme_pubclean()  +
      #   theme(axis.ticks.x = element_blank(),
      #         axis.text.x = element_blank(),
      #         axis.title.y = element_text(size = "14"),
      #         strip.text.x = element_text(size = "14"))

    }) 
    
    
    
# Tables ------------------------------------------------------------------

    output$DT_device <- renderDT({
      

      t <- device_analyse_data()

      if (input$radio_device_sum == 1) { # weekday
        
        sum_data <- rv$device_analyse_week_day_sum
        
      } else if (input$radio_device_sum == 2) {# overall
        
        sum_data <-  rv$device_analyse_overall_sum
        
      } 


      sum_data |> 
        mutate(used_device_capacity = paste0(number_format(used_device_capacity,1),"%" )) |> 
        select(Weekday = 	w_day, Hour =	hour, 
               `Test Count` = test_count, `Device Capacity` = 	device_capacity,
               `Used Device Capacity` = used_device_capacity) # 	cum_test_count	cum_device_capacity)
      
        
      
      
    },options = list(dom = "tp", pageLength =  7  ))

    # TAT Hourly 
    

    
    output$DT_TAT_hourly <- renderDT({
      
      # req(rv$TAT_sum_test)
      req(rv$TAT_data)
      
      # rv$TAT_sum_test <- analyse_data |> 
      #   calc_TAT() |> 
      #   summarise_TAT(type = "by_test", exclude_outlier = FALSE)
      # 
      if (input$radio_TAT_hourly_type == 1) { # multiple
        
        sum_data <- rv$TAT_hourly_multiple_sum |> 
          hourly_TAT_formatter() |> 
          rename(Weekday = w_day, Date = receiving_date) 
        
      } else if (input$radio_TAT_hourly_type == 2) {# weekday
        
        sum_data <- rv$TAT_hourly_week_day_sum |> 
          hourly_TAT_formatter() |> 
          rename(Weekday = w_day) 

      } else if (input$radio_TAT_hourly_type == 3) { # overall
        
        sum_data <- rv$TAT_hourly_overall_sum |> 
          hourly_TAT_formatter() 
        
      }
      
      
      sum_data  
      
    },server = FALSE,rownames=F, options = list(dom = "tp", pageLength =  7  ))

    
    # TAT Test   DT 
    output$DT_TAT_test <- renderDT({
      
    req(rv$TAT_data)
      
    if (input$radio_test_outlier == 1) { # no outlier exclusion

      sum_data <- rv$TAT_data    |> 
        filter(test_group %in% input$slt_TAT_test_group) |> 
        filter(test_name %in% input$slt_TAT_test) |> 
        summarise_TAT()
      
      
    } else if (input$radio_test_outlier == 2) {# outlier exclusion within test
      

      sum_data <- rv$TAT_data    |> 
        filter(test_group %in% input$slt_TAT_test_group) |> 
        filter(test_name %in% input$slt_TAT_test) |> 
        summarise_TAT(type = "by_test", exclude_outlier = TRUE)  
    }
    

    sum_data    |> 
        set_names("Test Group", "Test Name", "Mean (h)", "SD (h)", "Median (h)", "IQR (h)") |> 
        mutate(across(where(is.numeric), number_format,2 ))
      
      
    },options = list(dom = "tp", pageLength =  5  ))

    # TAT Test group  DT 
    output$DT_TAT_test_group <- renderDT({
      
      
      # saveRDS(rv$TAT_sum_test_group, "t.RDS")
      # 	test_group	mean_TAT	sd	median_TAT	IQR
      
      # t <- readRDS("ClinLabTool/t.RDS")
      
      req(rv$TAT_data)

      if (input$radio_test_group_outlier == 1) { # no outlier exclusion
        sum_data <- rv$TAT_data    |> 
          summarise_TAT(type = "by_test_group")
      } else if (input$radio_test_group_outlier == 2) {# outlier exclusion within test
        sum_data <- rv$TAT_data    |> 
          summarise_TAT(type = "by_test_group", exclude_outlier = TRUE)  
      }
      

        
      sum_data   |> 
          set_names("Test Group", "Mean (h)", "SD (h)", "Median (h)", "IQR (h)") |> 
          mutate(across(where(is.numeric), number_format,2 ))     

    },options = list(dom = "tp", pageLength =  5  ))
    
    
    
    
    
    
    # Overview tab: Lab specs
    output$DT_lab_specs <- renderDT({

        req(react_lab_specs_devices())
  

        react_lab_specs_devices()


    },options = list(dom = "tp", pageLength =  5  ))

    
    # Overview tab: Lab Data
    output$DT_data <- renderDT({
        
        req(react_lab_data())
        
        # TODO Ham datayı gösteriyor. Final datayı göstermeli
        react_lab_data()
        
        
    },options = list(dom = "tp", pageLength =  5  )) 
    
   
} # end server






# Run the application 
shinyApp(ui = ui, server = server)
