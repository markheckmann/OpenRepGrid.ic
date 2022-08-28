#///////////////////////////////////////////////////////////////////////////////////////////
#
#                                   SERVER
#
#//////////////////////////////////////////////////////////////////////////////////////////

library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)


# custom code to rotate element labels in DT
headerCallback <- c(
  "function(thead, data, start, end, display){",
  "  var $ths = $(thead).find('th');",
  "  $ths.css({'vertical-align': 'bottom', 'white-space': 'nowrap'});",
  "  var betterCells = [];",
  "  $ths.each(function(){",
  "    var cell = $(this);",
  "    var newDiv = $('<div>', {height: 'auto', width: cell.height()});",
  "    var newInnerDiv = $('<div>', {text: cell.text()});",
  "    newDiv.css({margin: 'auto'});",
  "    newInnerDiv.css({",
  "      transform: 'rotate(180deg)',",
  "      'writing-mode': 'tb-rl',",
  "      'white-space': 'nowrap'",
  "    });",
  "    newDiv.append(newInnerDiv);",
  "    betterCells.push(newDiv);",
  "  });",
  "  $ths.each(function(i){",
  "    $(this).html(betterCells[i]);",
  "  });",
  "}"
)


server <- function(input, output, session) 
{
  rv <- reactiveValues()
  rv$number_of_uploads <- 0  # used as trigger at new upload

  hide("down_btn")  
  

  #### .                       ####
  #### _______________________ ####
  #### UPLOAD ####
  
  # __ UPLOAD FILE ----
  
  observeEvent(input$excel_input, 
  {
     req(input$excel_input)
     in_file <- input$excel_input
     if (is.null(in_file))
       return(NULL)
     rv$file_path <- in_file$datapath   # needed as load_sample_data does not populate input$excel_input
     rv$file_name <- in_file$name
     rv$data <- openxlsx::read.xlsx(in_file$datapath)
     hide("down_btn")
     disable("btn_download_excel")
     # rv$data_status <- NULL  # set back data status
     rv$number_of_uploads <- rv$number_of_uploads + 1
  })

  
  # __ LOAD SAMPLE ----
  
  observeEvent(input$load_sample_data, 
  {
    path <- system.file("extdata", "sylvia.xlsx", package = "OpenRepGrid.ic")
    rv$file_path <- path 
    rv$file_name <- basename(path)
    rv$data <- openxlsx::read.xlsx(path)
    hide("down_btn")
    disable("btn_download_excel")
    rv$number_of_uploads <- rv$number_of_uploads + 1
  })
  
  
  #### .                       ####
  #### _______________________ ####
  #### TABLES ####
  

  output$box_no_elements <- renderInfoBox({
    req(rv$data)
    infoBox("Elements", ncol(rv$data) - 3, icon = icon("gem"), width = 3)
  })
  
  
  output$box_no_constructs <- renderInfoBox({
    req(rv$data)  
    infoBox("Constructs", nrow(rv$data), icon = icon("comment"), width = 3)
  })
  
  
  output$box_no_missing <- renderInfoBox({
    req(rv$data)
    x <- rv$data
    nc <- ncol(x)
    i_ratings <- 2L:(nc - 2)
    n_missings <- x[, i_ratings] %>% unlist %>% is.na %>% sum
    infoBox("Missing scores", n_missings, icon = icon("question"), width = 3)
  })
  
  
  # __ DATA STATUS ----
  
  # check if the data is valid and set a flag if this is the case
  # the flag 'data_status' is needed so other processes can use the info
  observeEvent(rv$data, {
    x <- rv$data
    if (is.null(x)) {
      rv$data_status <- "empty"
    } else {
      # sanity check
      tests <- check_excel_input(x)
      all_passed <- all(tests$passed)
      if (all_passed) {
        rv$data_status <- "passed"
      } else {
        rv$tests <- tests
        rv$data_status <- "failed"
      }
    }
  })
  
  
  observe({
    req(rv$data)
    # rv$data_status
    rv$number_of_uploads # trigger when data is changed
    
    # initialize settings after grid has been successfully read in
    if (rv$data_status == "passed") {
      n_elements <- rv$no_of_elements
      updateNumericInput(session, "par_min_match", value = n_elements - 1, min = 2, max = n_elements)
    }
  })
  
  
  # __ NO DATA ----
  
  dt_null <- reactive({
    rv$number_of_uploads # trigger when data is changed
    dt_default("Use the upload button on the right hand side to get started ...") %>%
      formatStyle(columns = 0, fontSize = "13pt", color = "black")
  })
  
  
  # __ FAILED----
  
  dt_failed_tests <- reactive({
    
    rv$number_of_uploads # trigger when data is changed
    
    green <- "#00CC00"
    red <- "#BF0000"
    neutral <- "#CCCCCC"
    
    cat("\nsome tests failed")
    show("error_box")
    hide("success_box")
    hide("down_btn")
    hide("settings_box_1")
    hide("settings_box_2")
    hide("tour_box")
    show("excel_info_box")
    
    tests <- isolate(rv$tests)
    failed <- 
      tests %>% 
      filter(!passed) %>% 
      rename(Expecting = "assert", Result = "passed", Hint = "error")
    dt <- DT::datatable(failed, rownames = FALSE,
                        options = list(
                          paging = FALSE,
                          ordering = FALSE,
                          dom = 't',
                          columnDefs = list(
                            list(className = 'dt-center', targets = 1),
                            list(className = 'dt-left', targets = c(1,2))
                          )
                        )) %>%
      formatStyle(c("Result"), valueColumns = "Result", color = "red") 
    return(dt)
  })
  
  
  # __ PASSED ----
  
  dt_tests_passed <- reactive({
    
    x <- isolate(rv$data)
    rv$number_of_uploads # trigger when data is changed
    
    grid_font_size <- input$grid_font_size
    grid_line_hight <- input$grid_line_height
    hide_preferred <- input$grid_hide_col_preferred
    
    green <- "#00CC00"
    red <- "#BF0000"
    neutral <- "#CCCCCC"
    
    # all test were passed
    cat("\nall tests passed")
    hide("error_box")
    show("down_btn")
    show("settings_box_1")
    show("settings_box_2")
    show("tour_box")
    show("success_box")
    hide("excel_info_box")
    
    nms <- names(x) %>% str_replace_all("\\.", " ")
    
    if (input$grid_rotate_elements) {
      header_callback <- JS(headerCallback)
    } else {
      header_callback <- NULL
    }
    
    i_preferred <- which(names(x) == "preferred")
    i_left <- 1
    i_right <- i_preferred - 1
    i_ratings <- (i_left + 1):(i_right - 1)
    rv$no_of_elements <- length(i_ratings)
    
    column_defs <- list(
      list(className = 'dt-center', targets = i_ratings - 1),
      list(className = 'dt-right', targets = 0),
      list(className = 'dt-left', targets = i_right - 1)
    )
    if (hide_preferred) {
      column_defs <- c(column_defs, list(list(visible = FALSE, targets = i_preferred - 1)))
    }
    
    dt <- DT::datatable(x, rownames = FALSE, colnames = nms, 
                        options = list(
                          headerCallback = header_callback, # JS(headerCallback),
                          paging = FALSE,
                          ordering = FALSE,
                          dom = 't',
                          columnDefs = column_defs,
                          initComplete = DT::JS(            # change column header size along with cells
                            paste0("function(settings, json) {",
                            "$(this.api().table().header()).css({'font-size': '", grid_font_size, "pt'});",
                            "}"))
                        )
    )  %>%
      formatStyle(c("0"), valueColumns = "preferred",
                  color = styleEqual(c(0, 1, NA), c(green, red, neutral))) %>%
      formatStyle(c("1"), valueColumns = "preferred",
                  color = styleEqual(c(1, 0, NA), c(green, red, neutral))) %>%
      formatStyle(columns = colnames(.$x$data),
                  fontSize = paste0(grid_font_size, "pt")) %>%
      formatStyle(columns = colnames(.$x$data), target = 'row',
                  lineHeight = paste0(grid_line_hight, "%"))    
    
    return(dt)
  })
  

  # RENDER ----
  
  output$dt_grid <- renderDataTable(
  {
    data_status <- rv$data_status
    
    if (is.null(data_status) || isTRUE(data_status == "empty")) {
      cat("\ndata is null")
      return(dt_null())
    } else if (data_status == "failed") {
      return(dt_failed_tests())
    } else if (data_status == "passed") {
      return(dt_tests_passed())
    }
  })
  

  #### .                       ####
  #### _______________________ ####
  #### DOWNLOAD  ####
  
  
  # disable settings for direction of constructs relation
  # if edges are not enabled
  observeEvent(input$par_show_edges, 
  {
    if (input$par_show_edges) {
      enable("par_indicate_direction")
      enable("par_colorize_direction")
    } else {
      disable("par_indicate_direction")
      disable("par_colorize_direction")
    }
  })
  
  
  observeEvent(input$btn_process, 
  {
    # req(input$excel_input$datapath)
    req(rv$data)
    disable("btn_download_excel")
    # file <- input$excel_input$datapath
    file <- rv$file_path
    min_matches <- input$par_min_match
    min_clique_size <- input$par_min_clique_size
    align_poles <- input$par_align_poles
    valence_prefix <- input$par_valence_prefix
    show_edges <- input$par_show_edges
    indicate_direction <- input$par_indicate_direction
    colorize_direction <- input$par_colorize_direction
    colorize_cliques <- input$par_colorize_cliques
    label_max_length <- input$par_label_max_length
    
    withProgress(message = 'Creating Excel file: ', value = 0, min = 0, max = 2,
    {
      l <- network_graph_images(rv$data, 
                                min_clique_size = min_clique_size, 
                                min_matches = min_matches,
                                show_edges = show_edges, 
                                align_poles = align_poles,
                                valence_prefix = valence_prefix,
                                label_max_length = label_max_length,
                                indicate_direction = indicate_direction,
                                colorize_direction = colorize_direction,
                                colorize_cliques = colorize_cliques)
      incProgress(1, detail = "Process data")
      rv$excel_out_path <- create_excel_output(file, l) 
      incProgress(1, detail = "Finishing")
      Sys.sleep(.2)
     })

    # allow download if excel has been created and saved successfully
    if (file.exists(rv$excel_out_path)) {
      enable("btn_download_excel")
      sendSweetAlert(session, title = "Calculation successful", 
                     text = "You can now download the Excel file containing the results by clicking on the 'Download results' button", 
                     type = "success",
                     btn_labels = "Ok", btn_colors = "#3085d6")
    }
  })


  output$btn_download_excel <- downloadHandler(
    
    filename = function() {
      # filename <- input$excel_input$name
      filename <- rv$file_name
      str_replace(filename, ".xlsx", "_CLIQUES.xlsx")
    },
    content = function(file) {
      file_out <- rv$excel_out_path
      file.copy(from = file_out, to = file)
    },
    contentType = "application/vnd.ms-excel"
  )
  
  
  output$download_sample_excel <- downloadHandler(
    filename = function() {
      "sylvia.xlsx"
    },
    content = function(file) {
      file_out <- system.file("extdata", "sylvia.xlsx", package = "OpenRepGrid.ic")
      file.copy(from = file_out, to = file)
    },
    contentType = "application/vnd.ms-excel"
  )
  
  # start introjs when button is pressed 
  observeEvent(input$start_tour,{
      introjs(session, 
              options = list("skipLabel" = "Cancel"),
              events = list("oncomplete" = 'alert("It is over")'))
  })
  
  
  observeEvent(input$par_min_match, {
    n_elements <- rv$no_of_elements
    criterion <- ifelse(is.null(n_elements), 0, n_elements)
    criterion <- ceiling(criterion / 2)
    feedbackWarning(
      text = "Low criterion for relatedness",
      inputId = "par_min_match",
      show = input$par_min_match <= criterion
    )
  })
  
}


