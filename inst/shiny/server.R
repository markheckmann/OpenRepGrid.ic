#////////////////////////////////////////////////////
#
#                     server
#
#////////////////////////////////////////////////////

library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)
library(shinyauthr)


# shared variables across users
users = reactiveValues(count = 0)

# dataframe that holds usernames, passwords and other user data
user_base <- data.frame(
  user = c("admin"),
  password = c("admin"), 
  permissions = c("standard"),
  name = c("Admin"),
  stringsAsFactors = FALSE,
  row.names = NULL
)

SHOW_LOGIN <- F   # set FALSE for dev puposes 


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
  
  hide("down_btn")
  # hide("success_box")
  # hide("grid_box")
  
  
  #### .                       ####
  #### _______________________ ####
  #### LOGIN  ####
  
  # keep track of number of users
  onSessionStart = isolate({
    users$count = users$count + 1
  })
  
  onSessionEnded(function() {
    isolate({
      users$count = users$count - 1
    })
  })
  
  
  # call the logout module with reactive trigger to hide/show
  if (SHOW_LOGIN) {
    logout_init <- callModule(shinyauthr::logout, id = "logout", 
                              active = reactive(credentials()$user_auth))
    
    # call login module supplying data frame, user and password cols and reactive trigger
    credentials <- callModule(shinyauthr::login, 
                              id = "login", 
                              data = user_base,
                              user_col = user,
                              pwd_col = password,
                              log_out = reactive(logout_init()))    
  } else {
    # suppress for dev purposes
    credentials <- reactive({
      list(user_auth = T)
    })
  }
  
  
  # on login / logout
  observe({
    if (isTRUE(credentials()$user_auth)) {
      # on Login
      cat("\nUI update on login")
      show(selector = ".sidebar-menu")
      show("sidebarCollapsed")
      hide(selector = '[data-value="tab_login"]')
      removeClass(id = "logout-button", class = "shinyjs-hide")
      updateTabItems(session, "sidebar", "tab_start")
    } else {
      # on Logout
      cat("\nUI update on logout")
      hide("sidebarCollapsed")
      updateTabItems(session, "sidebar", "tab_login")
    }
  })
  
  
  #### .                       ####
  #### _______________________ ####
  #### NOTIFICATIONS ####

  # no users and logout button  
  output$notification_menu <- renderMenu(
  {
    msg <- messageItem( from = "", 
                        message = paste("Number of current users:", users$count),
                        icon = icon("users"))
    logout <- tags$li(shinyauthr::logoutUI(id = "logout", label = "Logout"))
    dropdownMenu(msg, logout, icon = icon("users"), type = "messages", headerText = "")
  })
  

  #### .                       ####
  #### _______________________ ####
  #### TABLES ####
  
  # upload grid
  observeEvent(input$excel_input, 
  {
    req(input$excel_input)
    in_file <- input$excel_input
    if (is.null(in_file))
     return(NULL)
    rv$data <- openxlsx::read.xlsx(in_file$datapath)
    hide("down_btn")
    disable("btn_download_excel")
    
  })

  
  output$box_no_elements <- renderInfoBox({
    req(input$excel_input)
    infoBox("Elements", ncol(rv$data) - 3, icon = icon("gem"), width = 3)
  })
  
  
  output$box_no_constructs <- renderInfoBox({
    req(input$excel_input)
    infoBox("Constructs", nrow(rv$data), icon = icon("comment"), width = 3)
  })
  
  
  output$box_no_missing <- renderInfoBox({
    req(input$excel_input)
    x <- rv$data
    nc <- ncol(x)
    i_ratings <- 2L:(nc - 2)
    n_missings <- x[, i_ratings] %>% unlist %>% is.na %>% sum
    infoBox("Missing scores", n_missings, icon = icon("question"), width = 3)
  })
  
  
  output$dt_grid <- DT::renderDataTable(
  {
    req(rv$data)
    req(input$excel_input)
    
    grid_font_size <- input$grid_font_size
    grid_line_hight <- input$grid_line_height
    hide_preferred <- input$grid_hide_col_preferred
    min_matches <- input$par_min_match
    min_clique_size <- input$Par_min_clique_size
    
    green <- "#00CC00"
    red <- "#BF0000"
    neutral <- "#CCCCCC"
    
    # pass if NULL
    x <- rv$data
    if (is.null(x)) {
      cat("\ndata is null")
      return(dt_default())
    }
    
    # sanity check
    tests <- check_excel_input(x)
    all_passed <- all(tests$passed)
    
    # failed test
    if (!all_passed) {
      cat("\nsome tests failed")
      show("error_box")
      hide("success_box")
      hide("down_btn")
      hide("settings_box_1")
      hide("settings_box_2")
      # show("main_table")
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
          formatStyle(c("Result"), valueColumns = "Result", color = "white",
                      backgroundColor = styleEqual(c(TRUE, FALSE, NA), c(green, red, neutral))) 
      return(dt)
    } 
      
    # all test were passed
    cat("\nall tests passed")
    hide("error_box")
    show("success_box")
    show("down_btn")
    show("settings_box_1")
    show("settings_box_2")
    # show("main_table")
    # hide("grid_box")
    
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
    

    column_defs <- list(
        list(className = 'dt-center', targets = i_ratings - 1),
        list(className = 'dt-right', targets = 0)
    )
    if (hide_preferred) 
      column_defs <- c(column_defs, list(list(visible = FALSE, targets = i_preferred - 1)))
      
    dt <- DT::datatable(x, rownames = FALSE, colnames = nms, 
                  options = list(
                    headerCallback = header_callback, #JS(headerCallback),
                    paging = FALSE,
                    ordering = FALSE,
                    dom = 't',
                    columnDefs = column_defs
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
  
  
  #### .                       ####
  #### _______________________ ####
  #### DOWNLOAD  ####
  
  
  observeEvent(input$btn_process, 
  {
    req(input$excel_input$datapath)
    disable("btn_download_excel")
    file <- input$excel_input$datapath

    min_matches <- input$par_min_match
    min_clique_size <- input$Par_min_clique_size
    
    withProgress(message = 'Creating Excel file: ', value = 0, min = 0, max = 2,
    {
      l <- network_graph_images(rv$data, min_clique_size = min_clique_size, min_matches = min_matches)
      incProgress(1, detail = "Process data")
      rv$excel_out_path <- create_excel_output(file, l) 
      incProgress(1, detail = "Finishing")
      Sys.sleep(.2)
     })

    # allow download if excel has been created and saved succesfully
    if (file.exists(rv$excel_out_path)) {
      enable("btn_download_excel")
    }
  })

  
  output$btn_download_excel <- downloadHandler(
    filename = function() {
      filename <- input$excel_input$name
      str_replace(filename, ".xlsx", "_CLIQUES.xlsx")
    },
    content = function(file) {
      file_out <- rv$excel_out_path
      file.copy(from = file_out, to = file)
      # file_path <- input$excel_input$datapath
      # wb <- openxlsx::loadWorkbook(file_path)
      # saveWorkbook(wb, con)
    },
    contentType = "application/vnd.ms-excel"
  )
  
  
  output$download_sample_excel <- downloadHandler(
    filename = function() {
      "sylvia.xlsx"
    },
    content = function(file) {
      file_out <- system.file("extdata/sylvia.xlsx", package = "OpenRepGrid.ic")
      file.copy(from = file_out, to = file)
    },
    contentType = "application/vnd.ms-excel"
  )
  
}


