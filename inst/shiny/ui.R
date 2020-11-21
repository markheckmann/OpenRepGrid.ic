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
    library(rintrojs)
    library(shinyFeedback)
    library(data.table)
    library(formattable)
    library(openxlsx)
    library(DT)
    library(stringr)
    library(magrittr)
    library(reshape2)
    library(testthat)
    library(scales)
    library(tidyverse)
    library(glue)
    library(splines)
    library(igraph)
    library(OpenRepGrid.ic)
  })
})


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

pkg_version <- paste0("OpenRepGrid.ic v", packageVersion("OpenRepGrid.ic"))

header <- dashboardHeaderPlus(
  # dropdownMenuOutput("notification_menu"),
  title = tagList(
    span(class = "logo-lg", "OpenRepGrid.ic"), 
    span(class = "logo-sm", "")
  ),
  fixed = FALSE 
)


#### _______________________####
#### SIDEBAR                ####


sidebar <- dashboardSidebar(  
    sidebarMenu(id = "sidebar",
        menuItem("Home", tabName = "tab_start", icon = icon("home")),
        menuItem("Method", tabName = "tab_method", icon = icon("info")),
        menuItem("Analysis", tabName = "tab_grid", icon = icon("th"))
    )
)


#### _______________________####
#### BODY                   ####

body <- dashboardBody(
  
  useShinyjs(),    
  introjsUI(),
  useShinyFeedback(),

  tags$head(tags$link(rel = "favicon", href = "favicon.png")), # show favicon
  # custom CSS
  tags$head(tags$style("
  
  table a:link {
	color: #666;
	font-weight: bold;
	text-decoration:none;
  }
  table a:visited {
  	color: #999999;
  	font-weight:bold;
  	text-decoration:none;
  }
  table a:active,
  table a:hover {
  	color: #bd5a35;
  	text-decoration:underline;
  }
  table {
  	font-family:Arial, Helvetica, sans-serif;
  	color:#666;
  	font-size:12px;
  	text-shadow: 1px 1px 0px #fff;
  	background:#eaebec;
  	margin:20px;
  	border:#ccc 1px solid;
  
  	-moz-border-radius:3px;
  	-webkit-border-radius:3px;
  	border-radius:3px;
  
  	-moz-box-shadow: 0 1px 2px #d1d1d1;
  	-webkit-box-shadow: 0 1px 2px #d1d1d1;
  	box-shadow: 0 1px 2px #d1d1d1;
  }
  table th {
  	padding: 10px 12px 10px 12px;
  	border-top:1px solid #fafafa;
  	border-bottom:1px solid #e0e0e0;
  
  	background: #ededed;
  	background: -webkit-gradient(linear, left top, left bottom, from(#ededed), to(#ebebeb));
  	background: -moz-linear-gradient(top,  #ededed,  #ebebeb);
  }
  table th:first-child {
  	text-align: left;
  	padding-left:12px;
  }
  table tr:first-child th:first-child {
  	-moz-border-radius-topleft:3px;
  	-webkit-border-top-left-radius:3px;
  	border-top-left-radius:3px;
  }
  table tr:first-child th:last-child {
  	-moz-border-radius-topright:3px;
  	-webkit-border-top-right-radius:3px;
  	border-top-right-radius:3px;
  }
  table tr {
  	text-align: center;
  	padding-left:20px;
  }
  table td:first-child {
  	text-align: left;
  	padding-left:12px;
  	border-left: 0;
  }
  table td {
  	padding:18px;
  	border-top: 1px solid #ffffff;
  	border-bottom:1px solid #e0e0e0;
  	border-left: 1px solid #e0e0e0;
  
  	background: #fafafa;
  	background: -webkit-gradient(linear, left top, left bottom, from(#fbfbfb), to(#fafafa));
  	background: -moz-linear-gradient(top,  #fbfbfb,  #fafafa);
  }
  table tr.even td {
  	background: #f6f6f6;
  	background: -webkit-gradient(linear, left top, left bottom, from(#f8f8f8), to(#f6f6f6));
  	background: -moz-linear-gradient(top,  #f8f8f8,  #f6f6f6);
  }
  table tr:last-child td {
  	border-bottom:0;
  }
  table tr:last-child td:first-child {
  	-moz-border-radius-bottomleft:3px;
  	-webkit-border-bottom-left-radius:3px;
  	border-bottom-left-radius:3px;
  }
  table tr:last-child td:last-child {
  	-moz-border-radius-bottomright:3px;
  	-webkit-border-bottom-right-radius:3px;
  	border-bottom-right-radius:3px;
  }
  table tr:hover td {
  	background: #f2f2f2;
  	background: -webkit-gradient(linear, left top, left bottom, from(#f2f2f2), to(#f0f0f0));
  	background: -moz-linear-gradient(top,  #f2f2f2,  #f0f0f0);	
  }


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


  /*
  .downloadBtn {
    background: #00c851;
    color: green;
  }
  */
  
  ")),
  
  tabItems(
    
    #### __ START  ####
    
    tabItem(tabName = "tab_start",
              div( id = "tab_start_text", 
              fluidRow(
                column(width = 6, 
                   box(width = NULL, 
                       tags$span(style = "color:grey; font-style: italic", 
                                 pkg_version, 
                                 "- Please use a recent version of Chrome, Firefox or the Edge browser for optimal performance."),
                       status = "danger"
                   ),
                   box(width = NULL,
                     h2("Interpretive Clustering"),
                     p("This software implements the", 
                       tags$a("Interpretive Clustering (IC)", href = "https://doi.org/10.1080/14780887.2020.1794088", target = "_blank"),
                       "method described in",
                       tags$a("Burr, King, and Heckmann (2020).", href = "https://doi.org/10.1080/14780887.2020.1794088", target = "_blank"),
                       "An introduction to the software is available on", tags$a("YouTube.", href = "https://youtu.be/f9oINYA22Rc", target = "_blank")
                     ),
                     p("Interpretive Clustering is a variant of construct clustering for ", 
                       tags$a("repertory grid", href = "https://en.wikipedia.org/wiki/Repertory_grid", target = "_blank"),
                       "data.",
                       "While derived from theoretical considerations based on",
                       tags$a("Personal Construct Theory,", href = "https://en.wikipedia.org/wiki/Personal_construct_theory", target = "_blank"),
                       "the procedure itself is mathematically equivalent to a problem from graph theory called", 
                       tags$a(href = "https://en.wikipedia.org/wiki/Clique_problem#Listing_all_maximal_cliques", "maximal clique enumeration."),
                       "Given a similarity measure, in our case the number of matching scores between two constructs, a network graph of relatedness between constructs is construed.",
                       "A clique is a group of constructs which are all mutually related, given some cut-off criterion for relatedness (e.g. ", HTML("&ge;"), "6 matching scores for a 7 element grid).",
                       "While an offline approach is also described to find the construct cliques, this software automates the procedure.",
                       "In the image you can see the resulting construct cliques for Sylvia's sample grid."
                     ),
                     p("Under the entry", tags$em("Method"), "in the left sidebar you will find a step-by-step description of the manual process
                       to generate the construct clusters. To upload and analyse a grid programatically, click on the ", 
                       tags$em("Analysis"), "entry in the sidebar."
                     )
                    ),
                   box(width = NULL, 
                       status = "danger",
                       p("Please",
                         tags$a("email us", 
                                target="_blank", 
                                href = paste0("mailto:heckmann.mark@gmail.com",
                                              "?subject=Problem with OpenRepGrid.ic v", packageVersion("OpenRepGrid.ic"),
                                              "&body=Please describe the problem you encounter in detail, preferably with some screenshots.", 
                                                     "%0d%0aPlease also attach the data file you tried to process when the problem occurred. Data will be treated confidentially and deleted after fixing the problem.")
                          ),
                        "if you experience problems with the software."
                       )
                    )
                ), 
                column(width = 6,
                    img(src = "sylvia_cliques.png", align = "left")#, style = "width: 80%")
               )
              )
            )
    ),
    
    
    #### __ METHOD  ####
    
    tabItem(tabName = "tab_method",
          div( id = "tab_method_text",
               # includeMarkdown("www/method.md")
               includeHTML("www/method.html")
          )
    ),
    
    #### __ GRID  ####
    
    tabItem(tabName = "tab_grid",
        fluidRow(
              column(width = 9,
                      hidden(div(id = "error_box",
                          box(width = NULL, status = "danger", solidHeader = TRUE, #background = "red",
                              title = "Uuups. Something went wrong", 
                              p("I could not read your Excel file. Please see the comments below.",
                                "Make sure your Excel file has the required format.")                          )
                      )),
                      hidden(
                          div(id = "success_box",
                              introBox(data.step = 2, 
                                       data.intro = "The boxes contain basic information about the grid you uploaded.",
                              # box(width = NULL,
                              fluidRow(
                                infoBoxOutput("box_no_elements"),
                                infoBoxOutput("box_no_constructs"),
                                infoBoxOutput("box_no_missing")
                            )
                         )
                        )
                      ),
                     
                    #______ table -----------------
                    #
                    # div(id = "grid_box",
                    introBox(data.step = 3, 
                             data.intro = "The table shows the grid data from the Excel file you uploaded.
                                          Always double check that the data is correct before proceeding.",
                       box(width = NULL, 
                           div(#style = "font-size:120%", 
                               id = "main_table",
                               DT::dataTableOutput("dt_grid") %>% withSpinner(color = "#D33724") 
                           )
                       )
                    ),
                    div(id = "excel_info_box",
                    box(width = NULL, title = h4("Required Excel format"), collapsible = TRUE,
                      p("The grid must be specified on the first sheet of an Excel file (.xlsx) using the following conventions:"),
                      tags$ul(
                        tags$li("The Excel sheet has a header row containing the element labels."), 
                        tags$li("The first and second last columns contain the left and right construct poles, respectively.",
                                "Right poles may be left blank."),
                        tags$li("The ratings indicate which pole applies. 0 means that the left pole, 1 means that the right pole applies.",
                                "Use NA or a blank cell for missing values."),
                        tags$li("The rightmost column 'preferred' (all lower case letters) indicates which of the construct poles is the preferred pole.",
                                "0  means that the left pole is preferred, 1 means that the right pole is preferred.",
                                "Use NA or a blank cell if none of the poles is preferred.")
                        ),
                        p("Below you see a screenshot of a valid Excel format. The colors are for convenience only and are discarded by the software.",
                          "You can download a valid sample Excel file in the right hand panel and use it as a template."),
                        br(),
                        HTML('<center><img src="excel_grid_format.png" width="85%"></center>')
                    ))
                    
              ),
              column(width = 3,
                     hidden(div(id = "tour_box",
                        box(width = NULL, status = "warning", title = "Software Tour", collapsible = TRUE,
                          actionBttn("start_tour", "Start tour", icon = icon("info"), color = "warning")
                        )
                     )),
                     introBox(
                       box(width = NULL, status = "warning", title = "Upload",
                           p("Please upload an Excel file containing a grid.",
                             "To get started, you can download a sample file", downloadLink(outputId = "download_sample_excel", label = "here."),
                              "More datasets can be found", 
                              tags$a(href = "https://doi.org/10.5281/zenodo.3629868", target = "_blank", "here.")),
                           fileInput("excel_input", "Choose Excel File (.xlsx)", accept = ".xlsx")                     
                       ),
                       data.step = 1, 
                       data.intro = "As you already know, you can upload an Excel file here. 
                                     You may also download a sample file and use it as a template."
                     ),
                     
                     #______ settings ----------------
                     
                     hidden(div(id = "settings_box_1",
                        introBox(
                            box(width = NULL, status = "warning", title = "Grid settings", collapsible = TRUE, collapsed = TRUE, 
                              numericInput("grid_font_size", "Font size", 12, 6, 30, step = 1),
                              numericInput("grid_line_height", "Line height", value = 100, 50, 200, step = 10),
                              awesomeCheckbox("grid_rotate_elements", "Rotate header", value = FALSE),
                              awesomeCheckbox("grid_hide_col_preferred", "Hide preferred column", value = TRUE)
                            ),
                            data.step = 4, 
                            data.intro = "Settings to adjust the way the grid on the left is displayed. 
                                          You can change the font size or line height and rotate the header 
                                          if the element labels are very long."
                        )
                     )),
                     hidden(div(id = "settings_box_2",
                      introBox(data.step = 5, 
                               data.intro = "Specify the settings for the clique detection and start the calculation here.",
                          box(width = NULL, status = "warning", title = "Output settings", collapsible = TRUE,
                              introBox(data.step = 6, 
                                       data.intro = "Set the minimal number of matches between two construct to consider them 'related'.
                                                     By default it is set to the number of elements minus one.",
                                  numericInput("par_min_match", "Number of matches for relatedness", value = 6, min = 2, max = 20)
                              ),
                              introBox(data.step = 7, 
                                       data.intro = "Set the minimal number of mutually related construct that form a 'clique'.
                                                     By default the value is set to three constructs.",
                                       numericInput("par_min_clique_size", "Minimal cliques size", value = 3, min = 2, max = 10)
                              ),
                              introBox(data.step = 8, 
                                       data.intro = "Check to align positive / negative poles before building the graphs. Generally, this you be done.",
                                       awesomeCheckbox("par_align_poles", "Align pos./neg. poles", value = TRUE)
                              ),
                              introBox(data.step = 9, 
                                       data.intro = "Check to draw lines between related constructs.",
                                       awesomeCheckbox("par_show_edges", "Lines between related constructs", value = TRUE)
                              ),
                              introBox(data.step = 10, 
                                       data.intro = "Check to indicate the direction of a construct relation (positive / negative) by a +/- sign.",
                                       awesomeCheckbox("par_indicate_direction", "Indicate relation by +/-", value = TRUE)
                              ),
                              introBox(data.step = 11, 
                                       data.intro = "Check to colorize the direction of a construct relation (positive / negative) by red and green color.",
                                       awesomeCheckbox("par_colorize_direction", "Indicate relation by color", value = TRUE)
                              ),
                              
                              introBox(data.step = 12, 
                                       data.intro = "Check to get a different color for each clique/cluster.",
                                       awesomeCheckbox("par_colorize_cliques", "Colorize clusters", value = TRUE)
                              ),
                              introBox(data.step = 13, 
                                       data.intro = "Set maximal no of characters of construct labels to avoid cluttering the plot.",
                                       numericInput("par_label_max_length", "Max. length of construct labels", value = 100, min = -1, max = 100)
                              ),
                              # introBox(data.step = 8, 
                              #          data.intro = "Whether to show full or abbreviated construct labels (C1, C2 etc.) in the output graphs.
                              #                        By default full labels are shown.",
                              #          awesomeCheckbox("par_abbr_labels", "Abbreviate construct labels", value = FALSE)
                              # ),
                              div(style = "display:inline-block; float:left; margin-right: 10px",
                                introBox(data.step = 13, 
                                         data.intro = "Process the grid data and generate an Excel file containing 
                                                       the results for download.",
                                    actionButton("btn_process", label = "Process data")
                                 )
                              ),
                              div(style="display:inline-block; float:left",
                                introBox(data.step = 14, 
                                         data.intro = "After the result file has been created, you can download it here.",
                                  disabled(
                                   downloadButton(outputId = "btn_download_excel", style = "minimal", class = "downloadBtn", label = "Download results")
                                  )
                                )
                              )
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
  body = body
)







