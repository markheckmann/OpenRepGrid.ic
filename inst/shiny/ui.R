#////////////////////////////////////////////////////
#
#                       UI
#
#////////////////////////////////////////////////////


suppressWarnings({
  suppressMessages({
    
    library(shiny)
    library(shinyjs)
    library(shinythemes)
    library(shinyWidgets)
    library(shinydashboard)
    library(shinydashboardPlus)
    library(shinycssloaders)
    library(shinyauthr)
    
    library(data.table)
    library(formattable)
    library(openxlsx)
    library(DT)
    library(stringr)
    library(magrittr)
    library(tidyr)
    library(reshape2)
    library(testthat)
    library(scales)
    library(tidyverse)
    library(glue)
    library(splines)
    library(plotly)
    library(officer)
    
    library(igraph)
    # library(visNetwork)
    
    library(OpenRepGrid.ic)
    
  })
})

library(reactlog)
options(shiny.reactlog = TRUE) 


sidebar_right = rightSidebar(
  background = "dark",
  rightSidebarTabContent(
    id = 1,
    icon = "desktop",
    title = "Tab 1",
    active = TRUE,
    sliderInput(
      "obs", 
      "Number of observations:",
      min = 0, max = 1000, value = 500
    )
  ),
  rightSidebarTabContent(
    id = 2,
    title = "Tab 2",
    textInput("caption", "Caption", "Data Summary")
  ),
  rightSidebarTabContent(
    id = 3,
    title = "Tab 3",
    icon = "paint-brush",
    numericInput("obs", "Observations:", 10, min = 1, max = 100)
  ),
  title = "Right Sidebar"
  
)


#### _______________________####
#### HEADER                 ####


header <- dashboardHeaderPlus(
  dropdownMenuOutput("notification_menu"),
  title = tagList(
    span(class = "logo-lg",  paste0("OpenRepGrid.ic v", packageVersion("OpenRepGrid.ic"))), 
    span(class = "logo-sm", "")
  ),
  fixed = FALSE 
  # enable_rightsidebar = TRUE
)


#### _______________________####
#### SIDEBAR                ####

menuSubItem(text, tabName = NULL, href = NULL, newtab = TRUE,
            icon = shiny::icon("angle-double-right"), selected = NULL)

sidebar <- dashboardSidebar(
  
  hidden(
    sidebarMenu(id = "sidebar",
        menuItem("Login", tabName = "tab_login", icon = icon("login")),
        menuItem("Info", tabName = "tab_start", icon = icon("info")),
        menuItem("Grid", tabName = "tab_grid", icon = icon("th"))
    )
  )
)


#### _______________________####
#### BODY                   ####

body <- dashboardBody(
  
  useShinyjs(),             
  tags$head(tags$link(rel = "favicon", href = "favicon.png")), # show favicon
  # custom CSS
  tags$head(tags$style("
  
    .shiny-notification {
      top: 50% !important;
      left: 50% !important;
      margin-top: -100px !important;
      margin-left: -250px !important; 
      font-size: 20px;
    }
    
    .selectize-input {
      min-width: 200px;
      max-height: 200px;
      overflow: auto;
    }
    
    #sidebarItemExpanded .disabled {
      color: #4c5254  !important;
    }

  ")),
  
  tabItems(
    
    #### __ LOGIN  ####
    
    tabItem(tabName = "tab_login",
            shinyauthr::loginUI(id = "login", 
                                title = "Please log in",
                                login_title = "Login",
                                user_title = "User",
                                pass_title = "Password",
                                error_message = "Incorrect user or password!")
    ),
    
    #### __ START  ####
    
    tabItem(tabName = "tab_start",
              div( id = "tab_start_text", 
              fluidRow(
                column(width = 6, 
                   box(width = NULL,
                     h2("Interpretive Clustering"),
                     p("This website accompanies the paper", 
                       tags$a(href = "#", "Interpretive clustering"),  "by",
                       tags$a(href = "#", "Burr, King, Heckmann and Jankowicz (2020)"),
                       "The authors describe a variant of construct clustering which uses a procedure from graph theory called", 
                       tags$a(href = "https://en.wikipedia.org/wiki/Clique_problem#Listing_all_maximal_cliques", "maximal clique enumeration."),
                       "Given a similarity measure, in our case the number of matching scores between two constructs, a network graph of relatedness between constructs is construed.",
                       "A clique is a group of constructs in this network which are mutually related to each other, given some cut-off criterion for relatedness (e.g. 6 matching scores).",
                       "While an offline approach is also described to find the construct cliques, this software automates the process.",
                       "On the left you see the resulting cliques for Sylvia's sample grid as discussed in our paper."),
                     p("In case you have any questions, please",
                       tags$a("email us.", href = paste0("mailto:heckmann.mark@gmail.com",
                                                             "?subject=Question regarding OpenRepGrid.ic v", packageVersion("OpenRepGrid.ic"),
                                                             "&body=Your question goes here."))
                      )              
                    ),
                   box(width = NULL,
                       img(src = "sylvia_raw.png", align = "left", style = "width: 100%")
                   )
                ), 
                # column(width = 6,
                #   # box(width = NULL,
                    img(src = "ic_logo.png", align = "left")#, style = "width: 80%")
                #   )
                #)
              )
            )
    ),
    
    #### __ GRID  ####
    
    tabItem(tabName = "tab_grid",
        fluidRow(
              column(width = 9,
                     hidden(div(id = "error_box",
                          box(width = NULL, status = "danger", solidHeader = T, #background = "red",
                              title = "Uuups. Something went wrong", 
                              p("I could not read your Excel file. Please see the comments below.",
                                "Make sure your Excel file has the required format.")
                          )
                     )),
                    hidden(
                      div(id = "success_box",
                          box(width = NULL,
                            infoBoxOutput("box_no_elements"),
                            infoBoxOutput("box_no_constructs"),
                            infoBoxOutput("box_no_missing")
                        )
                     )
                    ),
                    # div(id = "grid_box",
                     box(width = NULL, 
                         div(#style = "font-size:120%", 
                             id = "main_table",
                             dataTableOutput("dt_grid") %>% withSpinner(color = "#D33724") 
                         )
                     )
                    
              ),
              column(width = 3,
                     box(width = NULL, status = "warning", title = "Upload",
                         p("Please upload an Excel file containing a grid."),
                         p("You can download a sample file", downloadLink(outputId = "download_sample_excel", label = "here.")),
                         fileInput("excel_input", "Choose Excel File (.xlsx)", accept = ".xlsx")
                     ),
                     hidden(div(id = "settings_box_1",
                       box(width = NULL, status = "warning", title = "Grid settings", collapsible = TRUE, 
                          numericInput("grid_font_size", "Font size", 12, 6, 30, step = 1),
                          numericInput("grid_line_height", "Line height", value = 100, 50, 200, step = 10),
                          awesomeCheckbox("grid_rotate_elements", "Rotate header", value = FALSE),
                          awesomeCheckbox("grid_hide_col_preferred", "Hide preferred column", value = TRUE)
                      )
                     )),
                     hidden(div(id = "settings_box_2",
                        box(width = NULL, status = "warning", title = "Output settings", collapsible = TRUE,
                            numericInput("par_min_match", "Number of matches for relatedness", value = 6, 2, 20),
                            numericInput("Par_min_clique_size", "Minimal cliques size", 3, 2, 10),
                             actionButton("btn_process", label = "Process data"),
                             disabled(
                               downloadButton(outputId = "btn_download_excel", style = "minimal", 
                                              color = "primary", label = "Dowload results")
                            )
                        )
                     ))
              )
         )
    )
    
  ) # end tabitems
) # ende body



#### _______________________####
#### DASHBOARD ####

ui <- dashboardPagePlus(
  title = "OpenRepGrid.ic",
  skin = "red",
  header = header,
  sidebar = sidebar,
  # rightsidebar = sidebar_right,
  body = body
)







