
library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinythemes)
library(data.table)
library(DT)
library(shinyWidgets)
library(rclipboard)
data <- fread('Data/data_proscons.csv', sep = ";", header = TRUE)
meth <- fread('Data/data_methods.csv', sep = ";", header = TRUE)
plot <- fread('Data/chartData.csv', sep = ";", header = TRUE)

# Define UI for application ####
ui <- dashboardPage(
  dashboardHeader(title = 'CRF Workflow App',
                  titleWidth = 300,
                  dropdownMenu(type = "messages",
                               messageItem(
                                 from = "Contact email",
                                 message = "me@gmail.com"
                               ),
                               messageItem(
                                 from = "Help",
                                 message = "More info here!",
                                 icon = icon("life-ring")
                               ))
  ),
  # sidebar####
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Home", tabName = "dashboard", icon = icon("home",lib = "glyphicon")),
      menuItem("Basis of Record", tabName = "BasisofRecord", icon = icon("eye-open",lib = "glyphicon")),
      menuItem("Taxonomic", tabName = "Taxonomic", icon = icon("tree-conifer",lib = "glyphicon"),
               menuSubItem("1. Download records options", tabName = "Tax1"),
               menuSubItem(HTML("2. Choose type of taxonomical source for<br/>  standardize / harmonize"), tabName = "Tax2"),
               menuSubItem(HTML("3. Previous filters based on taxonomical<br/>information included"), tabName = "Tax3"),
               menuSubItem(HTML("4. Query species names with taxonomical<br/>database"), tabName = "Tax4")
      ), 
      menuItem("Geographic", tabName = "Geographic", icon = icon("map-marker",lib = "glyphicon"),
               menuSubItem("1. Previous filters in download process", tabName = "Geo1"),
               menuSubItem("2. Location check", tabName = "Geo2"),
               menuSubItem(HTML('3. Correct / assign coordinates<br>to records without 
                                them or<br>errors from previous validations'), tabName = "Geo3"),
               menuSubItem("4. Outliers check", tabName = "Geo4")
      ),
      menuItem("Temporal", tabName = "Temporal", icon = icon("time",lib = "glyphicon")),
      menuItem("Duplicates", tabName = "Duplicates", icon = icon("duplicate",lib = "glyphicon")),
      menuItem("Final Report", tabName = "report", icon = icon("file", lib = "glyphicon"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
      # HOME ####
        tabName = 'dashboard',
        fluidRow(
          valueBoxOutput("infoBox", width = 12),
          box(
             h4(strong(HTML('INSTRUCTIONS<br>')),
             HTML('1. Choose any dimension of the 5 available in the left panel (see flowchart below).<br> 
              2. Select filters / steps in left-upper box (there are no previous 
              selections marked).<br>
              3. Check "Pros / Cons" table that will display with each selection 
              in the right-upper box (left panel).<br>
              4. Check "Methods" table that will display with each selection in 
              the right-upper box (right panel).<br> 
              5. Check how certainty/data coverage varies with each selection 
              in the left-bottom panel for each dimension. Values goes from 0 
              (minimum certainty or data coverage available) to 1 (maximum certainty 
              or data coverage available).<br> 
              6. The users may download a "Final report" of the steps selected in 
              each dimension providing the final guide to process their data 
              and write their methods section.')), width = 12)
                 ),
      fluidRow(
        img(src ='mermaid-diagram.svg')
      )
      ),
      # INPUT BOR ####
      tabItem(tabName = "BasisofRecord",
              fluidRow(
                column(width = 6,
                       box(h4(strong(HTML("Filters<br><br>"))),
                    h4(radioButtons(inputId = 'bor', 
                                 label = HTML("Choose between:<br><br>"), 
                                 choices = c('a. No filter'= 'Filter not applied',
                                             'b. Preserved specimens' = 'Preserved specimens',
                                             'c. Observations' = 'Observations',
                                             'd. Preserved specimens + Observations' = 'Preserved specimens + Observations'
                                 ),
                                 selected = '')),
                    height = 300, width = NULL),
                  box(plotOutput("PlotBOR", height = 325), height = "auto", width = NULL)
                ),
                column(width = 6,
                   box(tabsetPanel(
                          tabPanel(strong('Pros / Cons'),
                                   DT::dataTableOutput(outputId = "bortable")),
                          tabPanel(strong('Methods'),
                                   h4('Filter dataset using fields like dwc::basisOfRecord'))
                          ),
                       height = 550, width = NULL),
                valueBoxOutput("progressBox1", width = NULL)
                )
             
              )
      ),
      # INPUT TAX1 ####
      tabItem(tabName = "Tax1",
              fluidRow(
                column(width = 6,
                       box(h4(strong(HTML("Filters<br><br>"))),
                           h4(radioButtons(inputId = 'download',
                                        label = HTML("Choose between:<br><br>"), 
                                        choices = c('Download all records from higher taxonomic level'= 1,  
                                                    'Create a list of species 
                                            (accepted names and synonyms) from 
                                            previous taxonomical knowledge 
                                            and query databases.' = 2), 
                                        selected = "")), width = NULL, height = 300),
                       box(plotOutput("PlotTax1", height = 325), height = "auto", width = NULL)
                       ),
                column(width = 6,
                       box(tabsetPanel(
                         tabPanel(strong('Pros / Cons'),
                                  DT::dataTableOutput(outputId = "taxDownTable")),
                         tabPanel(strong('Methods'))
                       ), width = NULL, height = 550),
                       valueBoxOutput("progressBox2", width = NULL))
              )
      ),
      # INPUT TAX2 ####
      tabItem(tabName = "Tax2",
              fluidRow(
                column(width = 6,
                       box(
                         tabsetPanel(
                           tabPanel(strong(HTML('Checklist Type')),
                                    h4(radioButtons(inputId = 'op_checkType',
                                                 label = HTML('Choose between:<br><br>'),
                                                 choices = c('Manual' = 1,  
                                                             'Automatic' = 2), 
                                                 selected = ""))),
                           tabPanel(strong(HTML('Spatial coverage')),
                                    h4(radioButtons(inputId = 'op_spatialCov',
                                                 label = HTML('Choose between:<br><br>'),
                                                 choices = c('Global' = 1,  
                                                             'Regional' = 2), 
                                                 selected = ""))),
                           tabPanel(strong(HTML('Taxonomical coverage')),
                                    h4(radioButtons(inputId = 'op_taxCov',
                                                 label = HTML('Choose between:<br><br>'),
                                                 choices = c('General' = 1,  
                                                             'Specific' = 2), 
                                                 selected = ""))),
                           tabPanel(strong(HTML('Matching Type')),
                                    h4(radioButtons(inputId = 'op_matchType',
                                                 label = HTML('Choose between:<br><br>'),
                                                 choices = c('Fuzzy' = 1,  
                                                             'Exact' = 2), 
                                                 selected = "")))
                         ), width = NULL, height = 300),
                       box(plotOutput("PlotTax2", height = 325), height = "auto", width = NULL)
                       ),
                column(width = 6,
                box(tabsetPanel(
                  tabPanel(strong('Pros / Cons'),
                           DT::dataTableOutput(outputId = "taxCheckTablePC"),
                           DT::dataTableOutput(outputId = "taxSpatTablePC"),
                           DT::dataTableOutput(outputId = "taxCovTablePC"),
                           DT::dataTableOutput(outputId = "taxMatchTablePC")),
                  tabPanel(strong('Methods'),
                           DT::dataTableOutput(outputId = "taxCheckTableM"),
                           DT::dataTableOutput(outputId = "taxSpatTableM"),
                           DT::dataTableOutput(outputId = "taxCovTableM"),
                           DT::dataTableOutput(outputId = "taxMatchTableM"))
                ), width = NULL, height = 550),
                valueBoxOutput("progressBox2b", width = NULL))

              )
      ),
      # INPUT TAX3 ####
      tabItem(tabName = "Tax3",
              fluidRow(
                column(width = 6,
                       box(h4(strong(HTML("Filters<br><br>"))),
                           h4('Mark if options below will be checked:'),
                           h4(checkboxInput(inputId = 'taxRank',
                                         label = strong("Is the record identified at a proper taxonomic rank?"),
                                         value = FALSE)),
                           h4(checkboxInput(inputId = 'taxAuthor',
                                         label = strong("Does the scientific name have authorship information?"),
                                         value = FALSE))
                           , width = NULL, height = 300),
                       box(plotOutput("PlotTax3", height = 325), height = "auto", width = NULL)),
                column(width = 6,
                       box(tabsetPanel(
                         tabPanel(strong('Pros / Cons'),
                                  DT::dataTableOutput(outputId = "taxRankTable"),
                                  DT::dataTableOutput(outputId = "taxAuthorTablePC")
                         ),
                         tabPanel(strong('Methods'),
                                  h4(textOutput(outputId = "taxAuthorTableM")))
                       ), width = NULL, height = 550),
                       valueBoxOutput("progressBox2c", width = NULL)
                )
              )
      ),
      # INPUT TAX4 ####
      tabItem(tabName = "Tax4",
              fluidRow(
                column(width = 6,
                box(h4(strong("Filters")),
                    h4(checkboxGroupInput(inputId = 'taxStatus', 
                                       label = h4("Check taxonomical status and include scientific names classified as:"), 
                                       choices = c('Accepted'= 1,
                                                   'Synonym' = 2,
                                                   'Unresolved / No match' = 3
                                       ),
                                       selected = '')), height = 300, width = NULL),
                box(plotOutput("PlotTax4", height = 325), height = "auto", width = NULL)
                ),
                column(width = 6,
                       box(tabsetPanel(
                         tabPanel(strong('Pros / Cons'),
                                  DT::dataTableOutput(outputId = "taxStatusTable")),
                         tabPanel(strong('Methods'))
                       ), height = 550, width = NULL),
                       valueBoxOutput("progressBox2d", width = NULL)
                       )

              )
      ),
      # INPUT GEO1 ####
      tabItem(tabName = 'Geo1',
              fluidRow(
                column(width = 6,
                       box(h4(strong(HTML("Filters<br><br>"))),
                           h4(radioButtons(inputId = 'prev_geofilter',
                                           label = HTML("Choose between:<br><br>"), 
                                            choices = c(
                                            'Do not apply previous filter' = 3,
                                            'Only records with coordinates' = 2,
                                            'Only records with coordinates 
                                            filtered by spatial extent 
                                            (area or administrative units)' = 1,
                                            'Only records without known coordinate 
                                            issues' = 0
                                            ),
                                            selected = '')), width = NULL, height = 300
                           ),
                       box(plotOutput("PlotGeo1", height = 325),height ="auto", width = NULL)
                       ),
                column(width = 6,
                       box(tabsetPanel(
                            tabPanel(strong('Pros / Cons'),
                                     DT::dataTableOutput(outputId = 'prev_geoTable')),
                            tabPanel(strong('Methods'))
                        ), width = NULL, height = 525),
                valueBoxOutput("progressBox3", width = NULL)
                )

              )
      ),
      # INPUT GEO2 ####
      tabItem(tabName = 'Geo2',
              fluidRow(
                column(width = 6,
                box(tabsetPanel(id = 'filterGeo2',
                  tabPanel(strong(HTML('1. Check coordinates<br>precision')),
                           value = 'panGeo21',
                           h4(radioButtons(inputId = 'Geo21',
                                           label = HTML("Choose between:<br><br>"), 
                                        choices = c(
                                          'a.	Filter and discard records which precision value is 
                                       below an established threshold' = 1,
                                       'b. Use number of decimal digits of coordinates 
                                       as a measure of their precision' = 2,
                                       'c. Skip filtering by coordinates precision' = 3),
                                       selected = ''))
                  ),
                  tabPanel(strong(HTML('2. Check coordinates<br>values')),
                           value = 'panGeo22',
                           h4(radioButtons(inputId = 'Geo22',
                                        label = HTML('Validate records based on whether these conditions are true:<br><br>'),
                                        choices = c(
                                          'a. Latitude and longitude present the exact same value' = 1,
                                          'b. Coordinates are out of a reliable range' = 2,
                                          'c. Latitude or longitude values equals to zero' = 3,
                                          'd. Skip this step' = 4
                                        ), selected = ''))),
                  tabPanel(strong(HTML('3. Check position<br>of coordinates')),
                           value = 'panGeo23',
                           h4(radioButtons(inputId = 'Geo23',
                                        label = HTML('Validate each option from low to high strictness:<br><br>'),
                                        choices = c(
                                          'a. Are coordinates placed in correct habitat (sea / land)?' = 1,
                                          'b. Are coordinates placed in the country assigned?' = 2,
                                          'c. Check position of records that are not placed in the country assigned.' = 3,
                                          'd. Check records placed in prime meridian or equator countries' = 4,
                                          'e. Delete or label as potential errors those records whose coordinates are centroids' = 5,
                                          'f. Skip this step' = 6
                                        ), selected = '')))
                  
                ), width = NULL, height = 300),
                box(plotOutput("PlotGeo2", height = 325), height = "auto", width = NULL)
                ),
                column(width = 6,
                       box(tabsetPanel(
                         tabPanel(strong('Pros / Cons'),
                                  tabsetPanel(id = 'tabPCGeo2',
                                    tabPanel(strong(HTML('1. Check coordinates<br>precision')),
                                             value = 'tabPCGeo21',
                                             DT::dataTableOutput(outputId = 'geoPrecisionTablePC')),
                                    tabPanel(strong(HTML('2. Check coordinates<br>values')),
                                             value = 'tabPCGeo22',
                                             DT::dataTableOutput(outputId = 'geoValueTablePC')),
                                    tabPanel(strong(HTML('3. Check position<br>of coordinates')),
                                             value = 'tabPCGeo23',
                                             DT::dataTableOutput(outputId = 'geoPositionTablePC'))
                                  )
                         ),
                         tabPanel(strong('Methods'),
                                  tabsetPanel(
                                    tabPanel(strong(HTML('1. Check coordinates<br>precision')),
                                             DT::dataTableOutput(outputId = 'geoPrecisionTableM')),
                                    tabPanel(strong(HTML('2. Check coordinates<br>values')),
                                             DT::dataTableOutput(outputId = 'geoValueTableM')),
                                    tabPanel(strong(HTML('3. Check position<br>of coordinates')),
                                             DT::dataTableOutput(outputId = 'geoPositionTableM'))))
                           ),width = NULL, height = 525),
                       valueBoxOutput("progressBox3b", width = NULL))

              )
      ),
      # INPUT GEO3 ####
      tabItem(tabName = 'Geo3',
              fluidRow(
                column(width = 6,
                       box(
                         h4(radioButtons(inputId = 'Geo3',
                                      label = HTML("Choose between:<br><br>"), 
                                      choices = c(
                                      'a.	Retrieve coordinates indicated in 
                                      other formats from other fields as locality information.' = 1,
                                      'b. Use locality information or position description to 
                                      generate coordinates.' = 2,
                                      'c. Do not correct coordinate values' = 3),
                                      selected = '')), 
                          width = NULL, height = 300),
                       box(plotOutput("PlotGeo3", height = 325), height = "auto", width = NULL)
                ),
                column(width = 6,
                       box(tabsetPanel(
                         tabPanel(strong('Pros / Cons'),
                                  DT::dataTableOutput(outputId = "geo3TablePC")),
                         tabPanel(strong('Methods'),
                                  DT::dataTableOutput(outputId = "geo3TableM")),
                        ), width = NULL, height = 525
                       ),
                   valueBoxOutput("progressBox3c", width = NULL)
                   )

              )
      ),
      # INPUT GEO4 ####
      tabItem(tabName = 'Geo4',
              fluidRow(
                column(width = 6,
                       box(
                         tabsetPanel(id = 'filterGeo4',
                           tabPanel(strong('Use distributional information'),
                                    value = 'filterGeo4Dist',
                                    h4(radioButtons(inputId = 'dist_info',
                                                    label = HTML("Choose between:<br><br>"), 
                                                    choices = c('a.	Are coordinates placed in the species native range / extent of presence?' = 1,
                                                                'b.	Does location information match with their native region? ' = 2,
                                                                'c.	Do not apply a filter for distributional outliers' = 3),
                                                    selected = ''))),
                           tabPanel(strong('Use environmental information'),
                                    value = 'filterGeo4Env',
                                    h4(radioButtons(inputId = 'env_info',
                                                    label = HTML("Choose between:<br><br>"), 
                                                    choices = c('a.	Calculate environmental centroids for the species and validate outliers.' = 1,
                                                                'b.	Calculate environmental space for species and check overlaps and delete outliers.' = 2,
                                                                'c.	Overlap environmental information by geographical position and filter occurrences by threshold.' = 3,
                                                                'd.	Do not apply a filter for environmental outliers' = 4),
                                                    selected = '')))
                         ),width = NULL, height = 300
                         
                       ),
                       box(plotOutput("PlotGeo4", height = 325), height = "auto", width = NULL)
                       ),
                column(width = 6,
                       box(tabsetPanel(
                         tabPanel(strong('Pros / Cons'),
                                  tabsetPanel(id = 'tabPCGeo4',
                                    tabPanel(strong('Use distributional information'),
                                             value = 'tabPCDist',
                                             DT::dataTableOutput(outputId = "distTablePC")),
                                    tabPanel(strong('Use environmental information'),
                                             value = 'tabPCEnv',
                                             DT::dataTableOutput(outputId = "envTablePC")))),
                         tabPanel(strong('Methods'),
                                  tabsetPanel(
                                    tabPanel(strong('Use distributional information'),
                                             DT::dataTableOutput(outputId = "distTableM")),
                                    tabPanel(strong('Use environmental information'),
                                             DT::dataTableOutput(outputId = "envTableM"))))
                       ), width = NULL,height = 525),
                       valueBoxOutput("progressBox3d", width = NULL)  
                       )
              )
      ),
      # INPUT TEMP ####
      tabItem(tabName = 'Temporal',
              fluidRow(
                column(width = 6,
                       box(h4(strong("Filters")),
                           radioButtons(inputId = 'temp', 
                                        label = "Choose between:",
                                        choices = c('a. Do not apply temporal filter' = 1,
                                                    'b. Apply temporal filter' = 2),
                                        selected = ""
                           ),
                           conditionalPanel(condition = 'input.temp == 2',
                                            radioButtons(
                                              inputId = 'op_range',
                                              label = 'Set temporal range of time',
                                              choices = c('TRUE' = 'within temporal range',
                                                          'FALSE' = 'with no temporal range'),
                                              selected = ''
                                            )
                           ),
                           conditionalPanel(condition = 'input.temp == 2',
                                            radioButtons(
                                              inputId = 'op_level',
                                              label = 'Choose level of temporal information',
                                              choices = c('a. Complete date of collection' = 'Date of collection',
                                                          'b. At least year of collection' = 'Year of collection'),
                                              selected = ''
                                            )
                           ), width = NULL, height = 300
                       ),
                  box(plotOutput("PlotTEMP", height = 325), height = "auto", width = NULL)
                ),
                column(width = 6,
                  box(tabsetPanel(
                        tabPanel(strong('Pros / Cons'),
                                    DT::dataTableOutput(outputId = "temptable"),
                                 tabsetPanel(
                                    tabPanel('Range', DT::dataTableOutput(outputId = "tempRangetable")),
                                    tabPanel('Level', DT::dataTableOutput(outputId = "tempLeveltable")))),
                        tabPanel(strong('Methods'),
                                 h4('Filter dataset using fields like day; month; year or eventDate'))
                    ), width = NULL, height = 550),
                  valueBoxOutput("progressBox4", width = NULL)
                )
              
                
              )
              ),
      # INPUT DUPLICATES ####
      tabItem(tabName = 'Duplicates',
              fluidRow(
                column(width = 6,
                  box(h5(strong('Select the combination of fields to detect records 
                        duplicates with species name')),
                    radioButtons(inputId = 'op_position',
                                 label = 'a. Include position information as:',
                                 choices = c('Cell' = 'Cell',  
                                             'Coordinates + Buffer or rounded coordinates' = 'Coordinates + Buffer or rounded coordinates',
                                             'Coordinates (latitude and logitude)' = 'Coordinates'), 
                                 selected = ""),
                    radioButtons(inputId = 'op_time',
                                 label = 'b. Include time of collection as:',
                                 choices = c('Date - dd/mm/yyyy' = 'Date',  
                                             'Year' = 'Year',
                                             'Discard temporal information for detecting duplicates' = 'No temporal info'), 
                                 selected = ""),
                    checkboxInput(inputId = 'Recorder',
                                  label = strong("c. Include recorder's name"),
                                  value = FALSE),
                    height = 300, width = NULL),
                  box(plotOutput("PlotDUP", height = 325), height = "auto", width = NULL)),
               column(width = 6,
                 box(tabsetPanel(
                   tabPanel(strong('Pros / Cons'),
                            DT::dataTableOutput(outputId = "duplicatesPostablePC"),
                            DT::dataTableOutput(outputId = "duplicatesTimetablePC"),
                            DT::dataTableOutput(outputId = "duplicatesRectablePC")
                            ),
                   
                   tabPanel(strong('Methods'),
                            DT::dataTableOutput(outputId = "duplicatesPostableM"),
                            DT::dataTableOutput(outputId = "duplicatesTimetableM"),
                            DT::dataTableOutput(outputId = "duplicatesRectableM"))
                 ), width = NULL, height = 550),

                 valueBoxOutput("progressBox5", width = NULL)
               ) 
              )
      ),
      
      # REPORT ####
    tabItem(
      tabName = "report",
      fluidRow(
        box(rclipboardSetup(), verbatimTextOutput(outputId = 'prueba'), width = 12)),
      fluidRow(
        column(6, offset = 10, downloadBttn(outputId = "downloadData", style = "jelly", color = "success")))
     )
    )
  ), skin = "purple"
)
# Define server logic required ####
server <- function(input, output, session) {
  observe({
    if(input$filterGeo4 == 'filterGeo4Dist'){
      updateTabsetPanel(session, "tabPCGeo4", selected = 'tabPCDist')}
    else if(input$filterGeo4 == 'filterGeo4Env'){
      updateTabsetPanel(session, "tabPCGeo4", selected = 'tabPCEnv')}
  })
  observe({
    if(input$filterGeo2 == 'panGeo21'){
      updateTabsetPanel(session, 'tabPCGeo2', selected = 'tabPCGeo21')}
    else if(input$filterGeo2 == 'panGeo22'){
      updateTabsetPanel(session, 'tabPCGeo2', selected = 'tabPCGeo22')}
    else if(input$filterGeo2 == 'panGeo23'){
      updateTabsetPanel(session, 'tabPCGeo2', selected = 'tabPCGeo23')}
  })
  # PLOT OUTPUTS ####
  ploty <- function(data, color){
          ggplot(data) +
          geom_col(aes(x = measure, y = value, fill = measure, alpha = measure)) +
          scale_fill_manual(
            values = c(Certainty = color,
                       `Data coverage` = color)) +  
          scale_alpha_manual(name = "measure", values = c(1, 0.7)) +
          ylim(0, 1) +
          theme_minimal() +
          theme(
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = 12, face = "bold"),
            axis.title.y = element_blank(),
            legend.position = "none")
    }
  output$PlotBOR <- renderPlot({
    plot1 <- plot %>%  filter(variable == 'Basis of Record')
    plot1a <- plot %>%  filter(variable == 'Basis of Record')
    if(is.null(input$bor)){
      plot1[[1,3]] <- plot1a[[1,3]]
      plot1[[2,3]] <- plot1a[[2,3]]
    }
    else if(input$bor == 'Filter not applied'){
      plot1[[1,3]] <- 1
      plot1[[2,3]] <- 0.25
    }
    else if(input$bor == 'Preserved specimens'){
      plot1[[1,3]] <- 0.25
      plot1[[2,3]] <- 0.75  
    }
    else if(input$bor == 'Observations'){
      plot1[[1,3]] <- 0.5
      plot1[[2,3]] <- 0.5 
    }
    else if(input$bor == 'Preserved specimens + Observations'){
      plot1[[1,3]] <- 0.75
      plot1[[2,3]] <- 1 
    }
    ploty(plot1, "dodgerblue3")
  })
  output$PlotTax1 <- renderPlot({
    plot2 <- plot %>%  filter(variable == 'Taxonomy')
    plot2a <- plot %>%  filter(variable == 'Taxonomy')
    
    if(is.null(input$download)){
      if(is.null(input$op_matchType)){
        if(input$taxRank == 1){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.1 + 0.2
            }
          }
        }
        else if(input$taxRank == 0){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.1 + 0.2
            }
          }
        }
        
      }
      else if(input$op_matchType == 1){
        if(input$taxRank == 1){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.2
            }
          }
        }
        else if(input$taxRank == 0){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.2
            }
          }
        }
      }
      else if(input$op_matchType == 2){
        if(input$taxRank == 1){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.2
            }
          }
        }
        else if(input$taxRank == 0){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.2
            }
          }
        }
      }
    }
    else if(input$download == 1){
      if(is.null(input$op_matchType)){
        if(input$taxRank == 1){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.2
            }
          }
        }
        else if(input$taxRank == 0){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.2
            }
          }
        }
      }
      else if(input$op_matchType == 1){
        if(input$taxRank == 1){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.2
            }
          }
        }
        else if(input$taxRank == 0){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.2
            }
          }
        }
      }
      else if(input$op_matchType == 2){
        if(input$taxRank == 1){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.2
            }
          }
        }
        else if(input$taxRank == 0){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.2
            }
          }
        }
      }
    }
    else if(input$download == 2){
      if(is.null(input$op_matchType)){
        if(input$taxRank == 1){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.2
            }
          }
        }
        else if(input$taxRank == 0){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.2
            }
          }
        }
      }
      else if(input$op_matchType == 1){
        if(input$taxRank == 1){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.2
            }
          }
        }
        else if(input$taxRank == 0){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2+ 0.1 + 0.1 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0.2
            }
          }
        }
      }
      else if(input$op_matchType == 2){
        if(input$taxRank == 1){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.2
            }
          }
        }
        else if(input$taxRank == 0){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.2
            }
          }
        }
      } 
    }
    ploty(plot2a, "orange")
  })
  output$PlotTax2 <- renderPlot({
    plot2 <- plot %>%  filter(variable == 'Taxonomy')
    plot2a <- plot %>%  filter(variable == 'Taxonomy')
    if(is.null(input$download)){
      if(is.null(input$op_matchType)){
        if(input$taxRank == 1){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.1 + 0.2
            }
          }
        }
        else if(input$taxRank == 0){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.1 + 0.2
            }
          }
        }
        
      }
      else if(input$op_matchType == 1){
        if(input$taxRank == 1){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.2
            }
          }
        }
        else if(input$taxRank == 0){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.2
            }
          }
        }
      }
      else if(input$op_matchType == 2){
        if(input$taxRank == 1){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.2
            }
          }
        }
        else if(input$taxRank == 0){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.2
            }
          }
        }
      }
    }
    else if(input$download == 1){
      if(is.null(input$op_matchType)){
        if(input$taxRank == 1){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.2
            }
          }
        }
        else if(input$taxRank == 0){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.2
            }
          }
        }
      }
      else if(input$op_matchType == 1){
        if(input$taxRank == 1){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.2
            }
          }
        }
        else if(input$taxRank == 0){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.2
            }
          }
        }
      }
      else if(input$op_matchType == 2){
        if(input$taxRank == 1){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.2
            }
          }
        }
        else if(input$taxRank == 0){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.2
            }
          }
        }
      }
    }
    else if(input$download == 2){
      if(is.null(input$op_matchType)){
        if(input$taxRank == 1){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.2
            }
          }
        }
        else if(input$taxRank == 0){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.2
            }
          }
        }
      }
      else if(input$op_matchType == 1){
        if(input$taxRank == 1){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.2
            }
          }
        }
        else if(input$taxRank == 0){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2+ 0.1 + 0.1 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0.2
            }
          }
        }
      }
      else if(input$op_matchType == 2){
        if(input$taxRank == 1){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.2
            }
          }
        }
        else if(input$taxRank == 0){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.2
            }
          }
        }
      } 
    }
    ploty(plot2a, "orange")
  })
  output$PlotTax3 <- renderPlot({
    plot2 <- plot %>%  filter(variable == 'Taxonomy')
    plot2a <- plot %>%  filter(variable == 'Taxonomy')
    if(is.null(input$download)){
      if(is.null(input$op_matchType)){
        if(input$taxRank == 1){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.1 + 0.2
            }
          }
        }
        else if(input$taxRank == 0){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.1 + 0.2
            }
          }
        }
        
      }
      else if(input$op_matchType == 1){
        if(input$taxRank == 1){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.2
            }
          }
        }
        else if(input$taxRank == 0){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.2
            }
          }
        }
      }
      else if(input$op_matchType == 2){
        if(input$taxRank == 1){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.2
            }
          }
        }
        else if(input$taxRank == 0){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.2
            }
          }
        }
      }
    }
    else if(input$download == 1){
      if(is.null(input$op_matchType)){
        if(input$taxRank == 1){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.2
            }
          }
        }
        else if(input$taxRank == 0){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.2
            }
          }
        }
      }
      else if(input$op_matchType == 1){
        if(input$taxRank == 1){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.2
            }
          }
        }
        else if(input$taxRank == 0){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.2
            }
          }
        }
      }
      else if(input$op_matchType == 2){
        if(input$taxRank == 1){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.2
            }
          }
        }
        else if(input$taxRank == 0){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.2
            }
          }
        }
      }
    }
    else if(input$download == 2){
      if(is.null(input$op_matchType)){
        if(input$taxRank == 1){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.2
            }
          }
        }
        else if(input$taxRank == 0){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.2
            }
          }
        }
      }
      else if(input$op_matchType == 1){
        if(input$taxRank == 1){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.2
            }
          }
        }
        else if(input$taxRank == 0){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2+ 0.1 + 0.1 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0.2
            }
          }
        }
      }
      else if(input$op_matchType == 2){
        if(input$taxRank == 1){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.2
            }
          }
        }
        else if(input$taxRank == 0){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.2
            }
          }
        }
      } 
    }
    ploty(plot2a, "orange")
  })
  output$PlotTax4 <- renderPlot({
    plot2 <- plot %>%  filter(variable == 'Taxonomy')
    plot2a <- plot %>%  filter(variable == 'Taxonomy')
    if(is.null(input$download)){
      if(is.null(input$op_matchType)){
        if(input$taxRank == 1){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.2 + 0.1 + 0.2
            }
          }
        }
        else if(input$taxRank == 0){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0 + 0.1 + 0.1 + 0.2
            }
          }
        }
        
      }
      else if(input$op_matchType == 1){
        if(input$taxRank == 1){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.2
            }
          }
        }
        else if(input$taxRank == 0){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.2
            }
          }
        }
      }
      else if(input$op_matchType == 2){
        if(input$taxRank == 1){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.2 + 0.1 + 0.2
            }
          }
        }
        else if(input$taxRank == 0){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0 + 0.2 + 0.1 + 0.1 + 0.2
            }
          }
        }
      }
    }
    else if(input$download == 1){
      if(is.null(input$op_matchType)){
        if(input$taxRank == 1){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.2
            }
          }
        }
        else if(input$taxRank == 0){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.2
            }
          }
        }
      }
      else if(input$op_matchType == 1){
        if(input$taxRank == 1){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.2
            }
          }
        }
        else if(input$taxRank == 0){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.2
            }
          }
        }
      }
      else if(input$op_matchType == 2){
        if(input$taxRank == 1){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.2
            }
          }
        }
        else if(input$taxRank == 0){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.2
            }
          }
        }
      }
    }
    else if(input$download == 2){
      if(is.null(input$op_matchType)){
        if(input$taxRank == 1){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.2 + 0.1 + 0.2
            }
          }
        }
        else if(input$taxRank == 0){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0 + 0.1 + 0.1 + 0.2
            }
          }
        }
      }
      else if(input$op_matchType == 1){
        if(input$taxRank == 1){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.2 + 0.1 + 0.2
            }
          }
        }
        else if(input$taxRank == 0){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2+ 0.1 + 0.1 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.2 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.1 + 0.1 + 0.1 + 0.2
            }
          }
        }
      }
      else if(input$op_matchType == 2){
        if(input$taxRank == 1){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.1 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.2 + 0.1 + 0.2
            }
          }
        }
        else if(input$taxRank == 0){
          if(input$taxAuthor == 1){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.1 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.2 + 0.2
            }
          }
          else if(input$taxAuthor == 0){
            if(is.null(input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.2
            }
            else if(all(c(1,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(1,2,3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.2
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.1
            }
            else if(all(c(3) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(2) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0
            }
            else if(all(c(1) == input$taxStatus)){
              plot2a[[1,3]] <- 0.1 + 0.1 + 0.2 + 0.2 + 0.1
              plot2a[[2,3]] <- 0.2 + 0.2 + 0.1 + 0.1 + 0.2
            }
          }
        }
      } 
    }
    ploty(plot2a, "orange")
  })
  output$PlotGeo1 <- renderPlot({
    plot4 <- plot %>%  filter(variable == 'Geography')
    plot4a <- plot %>%  filter(variable == 'Geography')
    
    if(is.null(input$prev_geofilter)){
      plot4a[[1,3]] <- 0
      plot4a[[2,3]] <- 0
    }
    else if(input$prev_geofilter == 0){
      plot4a[[1,3]] <- 0.25
      plot4a[[2,3]] <- 1
    }
    else if(input$prev_geofilter == 1){
      plot4a[[1,3]] <- 0.5
      plot4a[[2,3]] <- 0.75
    }
    else if(input$prev_geofilter == 2){
      plot4a[[1,3]] <- 0.75
      plot4a[[2,3]] <- 0.5
    }
    else if(input$prev_geofilter == 3){
         plot4a[[1,3]] <- 1
         plot4a[[2,3]] <- 0.25
    }
    ploty(plot4a, "maroon")
  })
  output$PlotGeo2 <- renderPlot({
    plot4 <- plot %>%  filter(variable == 'Geography')
    plot4b <- plot %>%  filter(variable == 'Geography')
    
    if(is.null(input$Geo23)){
      if(is.null(input$Geo21)){
            if(is.null(input$Geo22)){
              plot4b[[1,3]] <- 0
              plot4b[[2,3]] <- 0
            }
            else if(input$Geo22 %in% c(1,2,3)){
              plot4b[[1,3]] <- 0.15
              plot4b[[2,3]] <- 0.33
            }
            else if(input$Geo22 == 4){
              plot4b[[1,3]] <- 0.33
              plot4b[[2,3]] <- 0
            }}
      else if(input$Geo21 %in% c(1,2)){
            if(is.null(input$Geo22)){
              plot4b[[1,3]] <- 0.15
              plot4b[[2,3]] <- 0.33
            }
            else if(input$Geo22 %in% c(1,2,3)){
              plot4b[[1,3]] <- 0.3
              plot4b[[2,3]] <- 0.66
            }
            else if(input$Geo22 == 4){
              plot4b[[1,3]] <- 0.5
              plot4b[[2,3]] <- 0.33
            }
        }
      else if(input$Geo21 == 3){
            if(is.null(input$Geo22)){
              plot4b[[1,3]] <- 0.3
              plot4b[[2,3]] <- 0
            }
            else if(input$Geo22 %in% c(1,2,3)){
              plot4b[[1,3]] <- 0.5
              plot4b[[2,3]] <- 0.33
            }
            else if(input$Geo22 == 4){
              plot4b[[1,3]] <- 0.66
              plot4b[[2,3]] <- 0
            }
        }
      
    }
    else if(input$Geo23 %in% c(1,2,3,4,5)){
      if(is.null(input$Geo21)){
        if(is.null(input$Geo22)){
          plot4b[[1,3]] <- 0.15
          plot4b[[2,3]] <- 0.33
        }
        else if(input$Geo22 %in% c(1,2,3)){
          plot4b[[1,3]] <- 0.3
          plot4b[[2,3]] <- 0.66
        }
        else if(input$Geo22 == 4){
          plot4b[[1,3]] <- 0.5
          plot4b[[2,3]] <- 0.33
        }
        }
      else if(input$Geo21 %in% c(1,2)){
          if(is.null(input$Geo22)){
            plot4b[[1,3]] <- 0.33
            plot4b[[2,3]] <- 0.66
          }
          else if(input$Geo22 %in% c(1,2,3)){
            plot4b[[1,3]] <- 0.5
            plot4b[[2,3]] <- 1
          }
          else if(input$Geo22 == 4){
            plot4b[[1,3]] <- 0.66
            plot4b[[2,3]] <- 0.66
          }
        }
      else if(input$Geo21 == 3){
          if(is.null(input$Geo22)){
            plot4b[[1,3]] <- 0.5
            plot4b[[2,3]] <- 0.33
          }
          else if(input$Geo22 %in% c(1,2,3)){
            plot4b[[1,3]] <- 0.66
            plot4b[[2,3]] <- 0.66
          }
          else if(input$Geo22 == 4){
            plot4b[[1,3]] <- 0.8
            plot4b[[2,3]] <- 0.33
          }
        }
      
    }
    else if(input$Geo23 == 6){
      if(is.null(input$Geo21)){
        if(is.null(input$Geo22)){
          plot4b[[1,3]] <- 0.33
          plot4b[[2,3]] <- 0
        }
        else if(input$Geo22 %in% c(1,2,3)){
          plot4b[[1,3]] <- 0.5
          plot4b[[2,3]] <- 0.33
        }
        else if(input$Geo22 == 4){
          plot4b[[1,3]] <- 0.66
          plot4b[[2,3]] <- 0
        }
      }
      else if(input$Geo21 %in% c(1,2)){
        if(is.null(input$Geo22)){
          plot4b[[1,3]] <- 0.5
          plot4b[[2,3]] <- 0.33
        }
        else if(input$Geo22 %in% c(1,2,3)){
          plot4b[[1,3]] <- 0.66
          plot4b[[2,3]] <- 0.66
        }
        else if(input$Geo22 == 4){
          plot4b[[1,3]] <- 0.8
          plot4b[[2,3]] <- 0.33
        }
      }
      else if(input$Geo21 == 3){
        if(is.null(input$Geo22)){
          plot4b[[1,3]] <- 0.66
          plot4b[[2,3]] <- 0
        }
        else if(input$Geo22 %in% c(1,2,3)){
          plot4b[[1,3]] <- 0.8
          plot4b[[2,3]] <- 0.33
        }
        else if(input$Geo22 == 4){
          plot4b[[1,3]] <- 1
          plot4b[[2,3]] <- 0
        }
      }

    }
    ploty(plot4b, "maroon")
  })
  output$PlotGeo3 <- renderPlot({
    plot4 <- plot %>%  filter(variable == 'Geography')
    plot4c <- plot %>%  filter(variable == 'Geography')
    if(is.null(input$Geo3)){
      plot4c[[1,3]] <- 0
      plot4c[[2,3]] <- 0
    }
    else if(input$Geo3 == 1){
      plot4c[[1,3]] <- 0.5
      plot4c[[2,3]] <- 0.75
    }
    else if(input$Geo3 == 2){
      plot4c[[1,3]] <- 0.75
      plot4c[[2,3]] <- 0.5
    }
    else if(input$Geo3 == 3){
      plot4c[[1,3]] <- 0
      plot4c[[2,3]] <- 0
    }
    ploty(plot4c, "maroon")
  })
  output$PlotGeo4 <- renderPlot({
    plot4 <- plot %>%  filter(variable == 'Geography')
    plot4d <- plot %>%  filter(variable == 'Geography')
    if(is.null(input$dist_info)){
      if(is.null(input$env_info)){
                    plot4d[[1,3]] <- 0
                    plot4d[[2,3]] <- 0}
      else if(input$env_info %in% c(1,2,3)){
                plot4d[[1,3]] <- 0.25
                plot4d[[2,3]] <- 0.5
      }
      else if(input$env_info == 4){
                plot4d[[1,3]] <- 0.5
                plot4d[[2,3]] <- 0
      }
    }
    else if(input$dist_info == 1){
      if(is.null(input$env_info)){
        plot4d[[1,3]] <- 0.25
        plot4d[[2,3]] <- 0.5}
      else if(input$env_info %in% c(1,2,3)){
        plot4d[[1,3]] <- 0.5
        plot4d[[2,3]] <- 1
      }
      else if(input$env_info == 4){
        plot4d[[1,3]] <- 0.75
        plot4d[[2,3]] <- 0
      }
    }
    else if(input$dist_info == 2){
      if(is.null(input$env_info)){
        plot4d[[1,3]] <- 0.25
        plot4d[[2,3]] <- 0.5}
      else if(input$env_info %in% c(1,2,3)){
        plot4d[[1,3]] <- 0.5
        plot4d[[2,3]] <- 1
      }
      else if(input$env_info == 4){
        plot4d[[1,3]] <- 0.75
        plot4d[[2,3]] <- 0
      }
    }
    else if(input$dist_info == 3){
      if(is.null(input$env_info)){
        plot4d[[1,3]] <- 0.5
        plot4d[[2,3]] <- 0}
      else if(input$env_info %in% c(1,2,3)){
        plot4d[[1,3]] <- 0.75
        plot4d[[2,3]] <- 0.5
      }
      else if(input$env_info == 4){
        plot4d[[1,3]] <- 1
        plot4d[[2,3]] <- 0
      }
    }
    ploty(plot4d, "maroon")
  })
  output$PlotTEMP <- renderPlot({
    plot3b <- plot %>%  filter(variable == 'Temporal')
    plot3 <- plot %>%  filter(variable == 'Temporal')
    if(is.null(input$temp)){
      plot3b[[1,3]] <- plot3[[1,3]]
      plot3b[[2,3]] <- plot3[[2,3]]  
    }
    else if(input$temp == 1){
      plot3b[[1,3]] <- 1
      plot3b[[2,3]] <- 0.25  
    }
    else if(input$temp == 2){
      if(is.null(input$op_range)){
       if(is.null(input$op_level)){
         plot3b[[1,3]] <- plot3[[1,3]]
         plot3b[[2,3]] <- plot3[[2,3]]
       }
       else if(input$op_level == 'Date of collection'){
         plot3b[[1,3]] <- plot3[[1,3]]
         plot3b[[2,3]] <- plot3[[1,3]]
       } 
       else if(input$op_level == 'Year of collection'){
         plot3b[[1,3]] <- plot3[[1,3]]
         plot3b[[2,3]] <- plot3[[1,3]]
       } 
      }
      else if(input$op_range == 'with no temporal range'){
          if(is.null(input$op_level)){
            plot3b[[1,3]] <- plot3[[1,3]]
            plot3b[[2,3]] <- plot3[[2,3]]
          }
          else if(input$op_level == 'Date of collection'){
            plot3b[[1,3]] <- plot3[[1,3]] + 0.5
            plot3b[[2,3]] <- plot3[[1,3]] + 0.75 
          } 
          else if(input$op_level == 'Year of collection'){
            plot3b[[1,3]] <- plot3[[1,3]] + 0.75 
            plot3b[[2,3]] <- plot3[[1,3]] + 0.5 
          } }
      else if(input$op_range == 'within temporal range'){
          if(is.null(input$op_level)){
            plot3b[[1,3]] <- plot3[[1,3]]
            plot3b[[2,3]] <- plot3[[2,3]]
          }    
          else if(input$op_level == 'Date of collection'){
                plot3b[[1,3]] <- plot3[[1,3]] + 0.25
                plot3b[[2,3]] <- plot3[[1,3]] + 1 
              } 
          else if(input$op_level == 'Year of collection'){
            plot3b[[1,3]] <- plot3[[1,3]] + 0.5 
            plot3b[[2,3]] <- plot3[[1,3]] + 0.75 
          } }
    }
    ploty(plot3b, "green4")
  })
  output$PlotDUP <- renderPlot({
    plot5 <- plot %>%  filter(variable == 'Duplicates')
    plot5a <- plot %>%  filter(variable == 'Duplicates')
    
    if(input$Recorder == 0){
      if(is.null(input$op_position)){
        if(is.null(input$op_time)){
          plot5a[[1,3]] <- plot5[[1,3]]
          plot5a[[2,3]] <- plot5[[2,3]]
        }
        else if(input$op_time == 'Date'){
          plot5a[[1,3]] <- 0
          plot5a[[2,3]] <- 0 
        }
        else if(input$op_time == 'Year'){
          plot5a[[1,3]] <- 0
          plot5a[[2,3]] <- 0
        }
        else if(input$op_time == 'No temporal info'){
          plot5a[[1,3]] <- 0
          plot5a[[2,3]] <- 0
        }
      }
      else if(input$op_position == 'Cell'){
        if(is.null(input$op_time)){
          plot5a[[1,3]] <- plot5[[1,3]]
          plot5a[[2,3]] <- plot5[[2,3]]
        }
        else if(input$op_time == 'Date'){
          plot5a[[1,3]] <- 0.5
          plot5a[[2,3]] <- 0.6 
        }
        else if(input$op_time == 'Year'){
          plot5a[[1,3]] <- 0.6
          plot5a[[2,3]] <- 0.4
        }
        else if(input$op_time == 'No temporal info'){
          plot5a[[1,3]] <- 0.8
          plot5a[[2,3]] <- 0.2
        }
      }
      else if(input$op_position == 'Coordinates + Buffer or rounded coordinates'){
        if(is.null(input$op_time)){
          plot5a[[1,3]] <- plot5[[1,3]]
          plot5a[[2,3]] <- plot5[[2,3]]
        }
       else if(input$op_time == 'Date'){
         plot5a[[1,3]] <- 0.5
         plot5a[[2,3]] <- 0.8
       }
       else if(input$op_time == 'Year'){
         plot5a[[1,3]] <- 0.6
         plot5a[[2,3]] <- 0.6
       }
       else if(input$op_time == 'No temporal info'){
         plot5a[[1,3]] <- 0.8
         plot5a[[2,3]] <- 0.4
       }
    }
      else if(input$op_position == 'Coordinates'){
        if(is.null(input$op_time)){
          plot5a[[1,3]] <- plot5[[1,3]]
          plot5a[[2,3]] <- plot5[[2,3]]
        }
        else if(input$op_time == 'Date'){
          plot5a[[1,3]] <- 0.5
          plot5a[[2,3]] <-  0.8
        }
        else if(input$op_time == 'Year'){
          plot5a[[1,3]] <- 0.6
          plot5a[[2,3]] <-  0.6
        }
        else if(input$op_time == 'No temporal info'){
          plot5a[[1,3]] <- 0.8
          plot5a[[2,3]] <- 0.4
        }
      }
    }
    else if(input$Recorder == 1){
      if(is.null(input$op_position)){
        if(is.null(input$op_time)){
          plot5a[[1,3]] <- plot5[[1,3]]
          plot5a[[2,3]] <- plot5[[2,3]]
        }
        else if(input$op_time == 'Date'){
          plot5a[[1,3]] <- 0
          plot5a[[2,3]] <- 0 
        }
        else if(input$op_time == 'Year'){
          plot5a[[1,3]] <- 0
          plot5a[[2,3]] <- 0
        }
        else if(input$op_time == 'No temporal info'){
          plot5a[[1,3]] <- 0
          plot5a[[2,3]] <- 0
        }
      }
      else if(input$op_position == 'Cell'){
        if(is.null(input$op_time)){
          plot5a[[1,3]] <- plot5[[1,3]]
          plot5a[[2,3]] <- plot5[[2,3]]
        }
        else if(input$op_time == 'Date'){
          plot5a[[1,3]] <- 0.3
          plot5a[[2,3]] <- 0.8 
        }
        else if(input$op_time == 'Year'){
          plot5a[[1,3]] <- 0.8
          plot5a[[2,3]] <- 0.6
        }
        else if(input$op_time == 'No temporal info'){
          plot5a[[1,3]] <- 0.6
          plot5a[[2,3]] <- 0.4
        }
      }
      else if(input$op_position == 'Coordinates + Buffer or rounded coordinates'){
        if(is.null(input$op_time)){
          plot5a[[1,3]] <- plot5[[1,3]]
          plot5a[[2,3]] <- plot5[[2,3]]
        }
        else if(input$op_time == 'Date'){
          plot5a[[1,3]] <- 0.3
          plot5a[[2,3]] <- 1
        }
        else if(input$op_time == 'Year'){
          plot5a[[1,3]] <- 0.4
          plot5a[[2,3]] <- 0.8
        }
        else if(input$op_time == 'No temporal info'){
          plot5a[[1,3]] <- 0.6
          plot5a[[2,3]] <- 0.6
        }
      }
      else if(input$op_position == 'Coordinates'){
        if(is.null(input$op_time)){
          plot5a[[1,3]] <- plot5[[1,3]]
          plot5a[[2,3]] <- plot5[[2,3]]
        }
        else if(input$op_time == 'Date'){
          plot5a[[1,3]] <- 0.3
          plot5a[[2,3]] <- 1
        }
        else if(input$op_time == 'Year'){
          plot5a[[1,3]] <- 0.4
          plot5a[[2,3]] <-  0.8
        }
        else if(input$op_time == 'No temporal info'){
          plot5a[[1,3]] <- 0.6
          plot5a[[2,3]] <- 0.6
        }
      }
    }
    ploty(plot5a, "#605ca8")
  })
  # BOXES OUTPUTS ####
  output$infoBox <- renderValueBox({
    infoBox(
      title = '',
      subtitle = h4(HTML(
        'CRF ("Cleaning Records and Filter") app was created to facilitate the process of filtering,
             cleaning and validating occurrences species records from available repositories.<br>
             Here you will find a "step by step" workflow that goes over 5 different dimensions
             concerning to data records.<br>
             The main objective is to help the user assessing all the 5 aspects of data records
             considering their pros and cons.<br> Users will also find a series of suggestions 
             to overcome each step with R functions, web services, fields to consider, 
             bibliographic references of methods, etc.
             By selecting each of the scenarios the user will see how them affect to
             information certainty and data coverage.<br> Following the ideas of Meyer et al. 2016,
             some steps that reduce uncertainty imply decreases of data availability and
             viceversa. The user has to consider this trade off to choose the final filters<br>')),
      color = "purple",icon = shiny::icon("info") 
    )})
  output$progressBox1 <- renderValueBox({
    if(is.null(input$bor)){valueBox(
      h4("Your final selection:"), HTML(' <br> '),
      icon = icon("eye-open",lib = "glyphicon", style = "font-size: 40px"),
      color = "blue"
    )}
    else if(!is.null(input$bor)){valueBox(
      h4("Your final selection:"), paste0(input$bor), 
      icon = icon("eye-open",lib = "glyphicon", style = "font-size: 40px"),
      color = "blue"
    )}
  })
  output$progressBox4 <- renderValueBox({
    if(is.null(input$temp)){
      valueBox(
      h4("Your final selection:"), HTML(' <br> '),
      icon = icon("time",lib = "glyphicon", style = "font-size: 40px"),
      color = "green")
      }
    else if(input$temp == 2){valueBox(
      h4("Your final selection:"), 
      HTML(paste('Data', input$op_range,'<br>Using', input$op_level, sep = ' ')), 
      icon = icon("time",lib = "glyphicon", style = "font-size: 40px"),
      color = "green")}
    
    else if(input$temp == 1){valueBox(
      h4("Your final selection:"), 'Filter not applied', 
      icon = icon("time",lib = "glyphicon", style = "font-size: 40px"),
      color = "green")}
  })
  output$progressBox5 <- renderValueBox({
    
    if(input$Recorder == 'TRUE'){
      valueBox( h4("Your final selection:"), 
                HTML(paste('Identify duplicates as the combination of:<br>Species',
                           input$op_position, input$op_time, 'Recorder', sep = ' ')),
                icon = icon("duplicate",lib = "glyphicon", style = "font-size: 40px"),
                color = "purple")}
    else if(input$Recorder == 'FALSE'){
      valueBox(h4("Your final selection:"), 
               HTML(paste('Identify duplicates as the combination of:<br>Species',
                          input$op_position, input$op_time, sep = ' ')),
               icon = icon("duplicate",lib = "glyphicon", style = "font-size: 40px"),
               color = "purple")
    }
  })

  ## tax boxes ####
  counter <- reactiveValues(countervalue = 0) # Defining & initializing the reactiveValues object
  observeEvent(input$download, {
    if(!is.null(input$download)){
      counter$countervalue <- 1     # if the add button is clicked, increment the value by 1 and update it
    }
  })
  counteri <- reactiveValues(countervaluei = 0) 
  observeEvent(input$taxStatus, {
    if(!is.null(input$taxStatus)){
      counteri$countervaluei <- 1     # if the add button is clicked, increment the value by 1 and update it
    }
  })
  counterii <- reactiveValues(countervalueii = 0) 
  observeEvent(input$taxRank, {
    if(input$taxRank == 1){
      counterii$countervalueii <- 1     # if the add button is clicked, increment the value by 1 and update it
    }
    else if(input$taxRank == 0){
      counterii$countervalueii <- 0     # if the add button is clicked, increment the value by 1 and update it
    }
  })
  counteriii <- reactiveValues(countervalueiii = 0) 
  observeEvent(input$taxAuthor, {
    if(input$taxAuthor == 1){
      counteriii$countervalueiii <- 1     # if the add button is clicked, increment the value by 1 and update it
    }
    else if(input$taxAuthor == 0){
      counteriii$countervalueiii <- 0     # if the add button is clicked, increment the value by 1 and update it
    }
  })
  counter2 <- reactiveValues(countervalue2 = 0) # Defining & initializing the reactiveValues object
  observeEvent(input$op_checkType, {
    if(!is.null(input$op_checkType)){counter2$countervalue2 <- 1}
    # if the add button is clicked, increment the value by 1 and update it
  })
  counter2i <- reactiveValues(countervalue2i = 0) 
  observeEvent(input$op_spatialCov, {
    if(!is.null(input$op_spatialCov)){counter2i$countervalue2i <- 1}
  })
  counter2ii <- reactiveValues(countervalue2ii = 0) 
  observeEvent(input$op_taxCov, {
    if(!is.null(input$op_taxCov)){counter2ii$countervalue2ii <- 1}
  })
  counter2iii <- reactiveValues(countervalue2iii = 0) 
  observeEvent(input$op_matchType, {
    if(!is.null(input$op_matchType)){counter2iii$countervalue2iii <- 1}
    
  })
  output$progressBox2 <- renderValueBox({
    valueBox(
      h4("Your selection:"), 
      paste0('You set ', 
             counter$countervalue + counteri$countervaluei + counterii$countervalueii + counteriii$countervalueiii, 
              '/4 steps to validate taxonomically and ', 
             counter2$countervalue2+ counter2i$countervalue2i+counter2ii$countervalue2ii+counter2iii$countervalue2iii,
             '/4 to choose tax. db'), 
      icon = icon("tree-conifer",lib = "glyphicon", style = "font-size: 40px"),
      color = "yellow"
    )
  })
  output$progressBox2b <- renderValueBox({
    valueBox(
      h4("Your selection:"), 
      paste0('You set ', 
             counter$countervalue+counteri$countervaluei+counterii$countervalueii+counteriii$countervalueiii, 
            '/4 steps to validate taxonomically and ', 
            counter2$countervalue2+ counter2i$countervalue2i+counter2ii$countervalue2ii+counter2iii$countervalue2iii,
            '/4 to choose tax. db'), 
      icon = icon("tree-conifer",lib = "glyphicon", style = "font-size: 40px"),
      color = "yellow"
    )
  })
  output$progressBox2c <- renderValueBox({
    valueBox(
      h4("Your selection:"), 
      paste0('You set ', 
             counter$countervalue+counteri$countervaluei+counterii$countervalueii+counteriii$countervalueiii, 
               '/4 steps to validate taxonomically and ', 
              counter2$countervalue2+ counter2i$countervalue2i+counter2ii$countervalue2ii+counter2iii$countervalue2iii,
               '/4 to choose tax. db'), 
      icon = icon("tree-conifer",lib = "glyphicon", style = "font-size: 40px"),
      color = "yellow"
    )
  })
  output$progressBox2d <- renderValueBox({
    valueBox(
      h4("Your selection:"), 
      paste0('You set ', 
             counter$countervalue+counteri$countervaluei+counterii$countervalueii+counteriii$countervalueiii, 
              '/4 steps to validate taxonomically and ', 
            counter2$countervalue2+ counter2i$countervalue2i+counter2ii$countervalue2ii+counter2iii$countervalue2iii,
              '/4 to choose tax. db'), 
      icon = icon("tree-conifer",lib = "glyphicon", style = "font-size: 40px"),
      color = "yellow"
    )
  })
  
  ## geo boxes ####  
  x <- reactiveValues(xvalue = '') 
  observeEvent(input$prev_geofilter, {
    if(input$prev_geofilter %in% c(0,1,2)){
      x$xvalue <- 'TRUE'
    }
    else if(input$prev_geofilter == 3){
      x$xvalue <- 'FALSE'
    }
  })
  
  geo21 <- reactiveValues(geo21value = '') 
  observeEvent(input$Geo21, {
    if(input$Geo21 %in% c(1,2)){
      geo21$geo21value <- 'TRUE'
    }
    else if(input$Geo21 == 3){
      geo21$geo21value <- 'FALSE'
    }
  })
  geo22 <- reactiveValues(geo22value = '') 
  observeEvent(input$Geo22, {
    if(input$Geo22 %in% c(1,2,3)){
      geo22$geo22value <- 'TRUE'
    }
    else if(input$Geo22 == 4){
      geo22$geo22value <- 'FALSE'
    }
  })
  geo23 <- reactiveValues(geo23value = '') 
  observeEvent(input$Geo23, {
    if(input$Geo23 %in% c(1,2,3,4,5)){
      geo23$geo23value <- 'TRUE'
    }
    
  })
  y <- reactiveValues(yvalue = '') 
  observeEvent(input$Geo3, {
    if(input$Geo3 %in% c(1,2)){
      y$yvalue <- 'TRUE'
    }
    else if(input$Geo3 == 3){
      y$yvalue <- 'FALSE'
    }
  })
  z <- reactiveValues(zvalue = '') 
  observeEvent(input$dist_info, {
    if(input$dist_info %in% c(1,2)){
      z$zvalue <- 'TRUE'
    }
    else if(input$dist_info == 3){
      z$zvalue <- 'FALSE'
    }
  })
  zz <- reactiveValues(zzvalue = '') 
  observeEvent(input$env_info, {
    if(input$env_info %in% c(1,2, 3)){
      zz$zzvalue <- 'TRUE'
    }
    else if(input$env_info == 4){
      zz$zzvalue <- 'FALSE'
    }
  })
  
  output$progressBox3 <- renderValueBox({
    valueBox(
      h4("Your selection:"), h5(HTML(paste('*Applying previous filter: ', x$xvalue, 
                                      '&emsp;&emsp;*Checking coordinates precision: ', geo21$geo21value, 
                                      '<br>*Checking coordinates value: ',geo22$geo22value,
                                      '&emsp;&emsp;&emsp;*Checking coordinates position: ', geo23$geo23value, 
                                      '<br>*Recovering coordinates: ', y$yvalue, 
                                      '&emsp;&emsp;&emsp;*Detecting distributional outliers: ', z$zvalue, 
                                      '<br>*Detecting environmental outliers: ', zz$zzvalue))), 
      icon = icon("map-marker",lib = "glyphicon", style = "font-size: 60px"),
      color = "maroon"
    )
  })
  output$progressBox3b <- renderValueBox({
    valueBox(
      h4("Your selection:"), h5(HTML(paste('*Applying previous filter: ', x$xvalue, 
                                           '&emsp;&emsp;*Checking coordinates precision: ', geo21$geo21value, 
                                           '<br>*Checking coordinates value: ',geo22$geo22value,
                                           '&emsp;&emsp;&emsp;*Checking coordinates position: ', geo23$geo23value, 
                                           '<br>*Recovering coordinates: ', y$yvalue, 
                                           '&emsp;&emsp;&emsp;*Detecting distributional outliers: ', z$zvalue, 
                                           '<br>*Detecting environmental outliers: ', zz$zzvalue))), 
      icon = icon("map-marker",lib = "glyphicon", style = "font-size: 60px"),
      color = "maroon"
    )
  })
  output$progressBox3c <- renderValueBox({
    valueBox(
      h4("Your selection:"), h5(HTML(paste('*Applying previous filter: ', x$xvalue, 
                                           '&emsp;&emsp;*Checking coordinates precision: ', geo21$geo21value, 
                                           '<br>*Checking coordinates value: ',geo22$geo22value,
                                           '&emsp;&emsp;&emsp;*Checking coordinates position: ', geo23$geo23value, 
                                           '<br>*Recovering coordinates: ', y$yvalue, 
                                           '&emsp;&emsp;&emsp;*Detecting distributional outliers: ', z$zvalue, 
                                           '<br>*Detecting environmental outliers: ', zz$zzvalue))), 
      icon = icon("map-marker",lib = "glyphicon", style = "font-size: 60px"),
      color = "maroon"
    )
  })
  output$progressBox3d <- renderValueBox({
    valueBox(
      h4("Your selection:"), h5(HTML(paste('*Applying previous filter: ', x$xvalue, 
                                           '&emsp;&emsp;*Checking coordinates precision: ', geo21$geo21value, 
                                           '<br>*Checking coordinates value: ',geo22$geo22value,
                                           '&emsp;&emsp;&emsp;*Checking coordinates position: ', geo23$geo23value, 
                                           '<br>*Recovering coordinates: ', y$yvalue, 
                                           '&emsp;&emsp;&emsp;*Detecting distributional outliers: ', z$zvalue, 
                                           '<br>*Detecting environmental outliers: ', zz$zzvalue))), 
      icon = icon("map-marker",lib = "glyphicon", style = "font-size: 60px"),
      color = "maroon"
    )
  })
  # BOR OUTPUT ####
  output$bortable <- renderDataTable({
    req(input$bor)
    if(input$bor == 'Filter not applied'){data %>% filter(bor == 1) %>% select(Pros, Cons)}
    else if(input$bor == 'Preserved specimens'){data %>% filter(bor == 2) %>% select(Pros, Cons)}
    else if(input$bor == 'Observations'){data %>% filter(bor == 3) %>% select(Pros, Cons)}
    else if(input$bor == 'Preserved specimens + Observations'){data %>% filter(bor == 4) %>% select(Pros, Cons)}
  }, 
  options = list(lengthChange = FALSE, paging = FALSE, searching = FALSE, ordering = FALSE,info = FALSE,
                 columnDefs = list(list(className = 'dt-body-left', width = '160px', targets = "_all"))),rownames = FALSE,
  colnames = '')
  # TAX OUTPUTS ####
  output$taxDownTable <- renderDataTable({
    req(input$download)
    if(input$download == 1){data %>% filter(download_tax == 1) %>% select(Pros, Cons)}
    else if(input$download == 2){data %>% filter(download_tax == 2) %>% select(Pros, Cons)}
  }, 
  options = list(lengthChange = FALSE, paging = FALSE, searching = FALSE, ordering = FALSE,info = FALSE,
                 columnDefs = list(list(className = 'dt-body-left', width = '160px',targets = "_all"))), 
  rownames = FALSE, colnames = '')
  
  output$taxCheckTablePC <- renderDataTable({
    req(input$op_checkType)
    
    if(input$op_checkType == 1){data %>% filter(taxonomic_db == 1) %>% select(Pros, Cons)}
    else if(input$op_checkType == 2){data %>% filter(taxonomic_db == 2) %>% select(Pros, Cons)}
  }, 
  options = list(lengthChange = FALSE, paging = FALSE, searching = FALSE, ordering = FALSE,info = FALSE,
                 columnDefs = list(list(className = 'dt-body-left', width = '160px',targets = "_all"))), 
  rownames = FALSE, colnames = c('Checklist Type',''))
  
  output$taxCheckTableM <- renderDataTable({
    req(input$op_checkType)
    if(input$op_checkType == 1){meth %>% filter(taxonomic_db == 2) %>% select(2)}
  }, 
  options = list(lengthChange = FALSE, paging = FALSE, searching = FALSE, ordering = FALSE,info = FALSE,
                 columnDefs = list(list(className = 'dt-body-left', targets = "_all"))), rownames = FALSE, 
  colnames = 'Checklist Type')
  output$taxSpatTablePC <- renderDataTable({
    req(input$op_spatialCov)
    if(input$op_spatialCov == 1){data %>% filter(taxonomic_db == 3) %>% select(Pros, Cons)}
    else if(input$op_spatialCov == 2){data %>% filter(taxonomic_db == 4) %>% select(Pros, Cons)}
  }, 
  options = list(lengthChange = FALSE, paging = FALSE, searching = FALSE, ordering = FALSE,info = FALSE,
                 columnDefs = list(list(className = 'dt-body-left', width = '160px',targets = "_all"))), 
  rownames = FALSE, colnames = c('Spatial coverage',''))
  
  output$taxSpatTableM <- renderDataTable({
    req(input$op_spatialCov)
    if(input$op_spatialCov == 1){meth %>% filter(taxonomic_db == 3) %>% select(2)}
  }, 
  options = list(lengthChange = FALSE, paging = FALSE, searching = FALSE, ordering = FALSE,info = FALSE,
                 columnDefs = list(list(className = 'dt-body-left', targets = "_all"))), rownames = FALSE, 
  colnames = 'Spatial coverage')
  
  output$taxCovTablePC <- renderDataTable({
    req(input$op_taxCov)
    if(input$op_taxCov == 1){data %>% filter(taxonomic_db == 5) %>% select(Pros, Cons)}
    else if(input$op_taxCov == 2){data %>% filter(taxonomic_db == 6) %>% select(Pros, Cons)}
  }, 
  options = list(lengthChange = FALSE, paging = FALSE, searching = FALSE, ordering = FALSE,info = FALSE,
                 columnDefs = list(list(className = 'dt-body-left', width = '160px',targets = "_all"))), 
  rownames = FALSE, colnames = c('Taxonomical coverage',''))
  
  output$taxCovTableM <- renderDataTable({
    req(input$op_taxCov)
    if(input$op_taxCov == 1){meth %>% filter(taxonomic_db == 5) %>% select(2)}
    else if(input$op_taxCov == 2){meth %>% filter(taxonomic_db == 6) %>% select(2)}
  }, 
  options = list(lengthChange = FALSE, paging = FALSE, searching = FALSE, ordering = FALSE,info = FALSE,
                 columnDefs = list(list(className = 'dt-body-left', targets = "_all"))), rownames = FALSE, 
  colnames = 'Taxonomical coverage')
  
  output$taxMatchTablePC <- renderDataTable({
    req(input$op_matchType)
    if(input$op_matchType == 1){data %>% filter(taxonomic_db == 7) %>% select(Pros, Cons)}
    else if(input$op_matchType == 2){data %>% filter(taxonomic_db == 8) %>% select(Pros, Cons)}
  }, 
  options = list(lengthChange = FALSE, paging = FALSE, searching = FALSE, ordering = FALSE,info = FALSE,
                 columnDefs = list(list(className = 'dt-body-left', width = '160px',targets = "_all"))), 
  rownames = FALSE, colnames = c('Matching Type',''))
  
  output$taxMatchTableM <- renderDataTable({
    req(input$op_matchType)
    if(input$op_matchType == 1){meth %>% filter(taxonomic_db == 7) %>% select(2)}
    else if(input$op_matchType == 2){meth %>% filter(taxonomic_db == 8) %>% select(2)}
  }, 
  options = list(lengthChange = FALSE, paging = FALSE, searching = FALSE, ordering = FALSE,info = FALSE,
                 columnDefs = list(list(className = 'dt-body-left', targets = "_all"))), rownames = FALSE, 
  colnames = 'Matching Type')
  
  output$taxRankTable <- renderDataTable({
    req(input$taxRank)
    if(input$taxRank == 1){data %>% filter(prev_tax == 1) %>% select(Pros, Cons)}
  }, 
  options = list(lengthChange = FALSE, paging = FALSE, searching = FALSE, ordering = FALSE,info = FALSE,
                 columnDefs = list(list(className = 'dt-body-left', width = '160px', targets = "_all"))),
  rownames = FALSE, colnames = c('Taxon Rank',''))
  
  output$taxAuthorTablePC <- renderDataTable({
    req(input$taxAuthor)
    if(input$taxAuthor == 1){data %>% filter(prev_tax == 2) %>% select(Pros, Cons)}
  }, 
  options = list(lengthChange = FALSE, paging = FALSE, searching = FALSE, ordering = FALSE,info = FALSE,
                 columnDefs = list(list(className = 'dt-body-left', width = '160px',targets = "_all"))), 
  rownames = FALSE, colnames = c('Authorship',''))
  
  output$taxAuthorTableM <- renderText({
    req(input$taxAuthor)
    if(input$taxAuthor == 1){return("See Ribeiro et al. 2022: bdc::bdc_clean_names")}
  })
  
  taxStatusTable_results <- reactive({
    tax1 <- data %>% filter(query_tax == 1) %>% select(Pros, Cons)
    tax2 <- data %>% filter(query_tax == 2) %>% select(Pros, Cons)
    tax3 <- data %>% filter(query_tax == 3) %>% select(Pros, Cons)
    tax4 <-(bind_rows(
      if ("1" %in% input$taxStatus){tax1} else {tax1[F,]},
      if ("2" %in% input$taxStatus){tax2} else {tax2[F,]},
      if ("3" %in% input$taxStatus){tax3} else {tax3[F,]}
    ))
    return(tax4)
  })
  output$taxStatusTable <- renderDataTable({
    req(input$taxStatus)
    taxStatusTable_results()
  }, 
  options = list(lengthChange = FALSE, paging = FALSE, searching = FALSE, ordering = FALSE,info = FALSE,
                 columnDefs = list(list(className = 'dt-body-left', width = '160px',targets = "_all"))), 
  rownames = FALSE, colnames = '')
  
  # GEO OUTPUTS ####
  output$prev_geoTable <- renderDataTable({
    req(input$prev_geofilter)
    if(input$prev_geofilter == 0){data %>% filter(geo_prev_filter == 1) %>% select(Pros, Cons)}
    else if(input$prev_geofilter == 1){data %>% filter(geo_prev_filter == 2) %>% select(Pros, Cons)}
    else if(input$prev_geofilter == 2){data %>% filter(geo_prev_filter == 3) %>% select(Pros, Cons)}
    else if(input$prev_geofilter == 3){data %>% filter(geo_prev_filter == 4) %>% select(Pros, Cons)}
  }, 
  options = list(lengthChange = FALSE, paging = FALSE, searching = FALSE, ordering = FALSE,info = FALSE,
                 columnDefs = list(list(className = 'dt-body-left',width = '160px', targets = "_all"))), 
  rownames = FALSE, colnames = '')
  
  output$geoPrecisionTablePC <- renderDataTable({
    req(input$Geo21)
    if(input$Geo21 == 1){data %>% filter(coords == 1) %>% select(Pros, Cons)}
    else if(input$Geo21 == 2){data %>% filter(coords == 2) %>% select(Pros, Cons)}
    else if(input$Geo21 == 3){data %>% filter(coords == 3) %>% select(Pros, Cons)}
  }, 
  options = list(lengthChange = FALSE, paging = FALSE, searching = FALSE, ordering = FALSE,info = FALSE,
                 columnDefs = list(list(className = 'dt-body-left',width = '160px', targets = "_all"))), 
  rownames = FALSE, colnames = c('',''))
  
  output$geoPrecisionTableM <- renderDataTable({
    req(input$Geo21)
    if(input$Geo21 == 1){meth %>% filter(coords == 1) %>% select(2)}
    else if(input$Geo21 == 2){meth %>% filter(coords == 2) %>% select(2)}
  }, 
  options = list(lengthChange = FALSE, paging = FALSE, searching = FALSE, ordering = FALSE,info = FALSE,
                 columnDefs = list(list(className = 'dt-body-left', targets = "_all"))), rownames = FALSE, 
  colnames = NULL)  
  
  output$geoValueTablePC <- renderDataTable({
    req(input$Geo22)
    if(input$Geo22 %in% c(1,2,3)){data %>% filter(coords == 4) %>% select(Pros, Cons)}
    else if(input$Geo22 == 4){data %>% filter(coords == 5) %>% select(Pros, Cons)}
  }, 
  options = list(lengthChange = FALSE, paging = FALSE, searching = FALSE, ordering = FALSE,info = FALSE,
                 columnDefs = list(list(className = 'dt-body-left',width = '160px', targets = "_all"))), 
  rownames = FALSE, colnames = c('',''))
  
  output$geoValueTableM <- renderDataTable({
    req(input$Geo22)
    if(input$Geo22 == 1){meth %>% filter(coords == 3) %>% select(2)}
    else if(input$Geo22 == 2){meth %>% filter(coords == 4) %>% select(2)}
    else if(input$Geo22 == 3){meth %>% filter(coords == 5) %>% select(2)}
  }, 
  options = list(lengthChange = FALSE, paging = FALSE, searching = FALSE, ordering = FALSE,info = FALSE,
                 columnDefs = list(list(className = 'dt-body-left', targets = "_all"))), rownames = FALSE, 
  colnames = NULL)
  
  output$geoPositionTablePC <- renderDataTable({
    req(input$Geo23)
    if(input$Geo23 == 1){data %>% filter(coords == 6) %>% select(Pros, Cons)}
    else if(input$Geo23 == 2){data %>% filter(coords == 7) %>% select(Pros, Cons)}
    else if(input$Geo23 == 3){data %>% filter(coords == 8) %>% select(Pros, Cons)}
    else if(input$Geo23 == 4){data %>% filter(coords == 9) %>% select(Pros, Cons)}
    else if(input$Geo23 == 5){data %>% filter(coords == 10) %>% select(Pros, Cons)}
  }, 
  options = list(lengthChange = FALSE, paging = FALSE, searching = FALSE, ordering = FALSE,info = FALSE,
                 columnDefs = list(list(className = 'dt-body-left',width = '160px', targets = "_all"))), 
  rownames = FALSE, colnames = c('',''))
  
  output$geoPositionTableM <- renderDataTable({
    req(input$Geo23)
    if(input$Geo23 == 1){meth %>% filter(coords == 6) %>% select(2)}
    else if(input$Geo23 == 2){meth %>% filter(coords == 7) %>% select(2)}
    else if(input$Geo23 == 3){meth %>% filter(coords == 8) %>% select(2)}
    else if(input$Geo23 == 4){meth %>% filter(coords == 9) %>% select(2)}
    else if(input$Geo23 == 5){meth %>% filter(coords == 10) %>% select(2)}
  }, 
  options = list(lengthChange = FALSE, paging = FALSE, searching = FALSE, ordering = FALSE,info = FALSE,
                 columnDefs = list(list(className = 'dt-body-left', targets = "_all"))), rownames = FALSE, 
  colnames = NULL)
  
  output$geo3TablePC <- renderDataTable({
    req(input$Geo3)
    if(input$Geo3 == 1){data %>% filter(no_coords == 1) %>% select(Pros, Cons)}
    else if(input$Geo3 == 2){data %>% filter(no_coords == 2) %>% select(Pros, Cons)}
    else if(input$Geo3 == 3){data %>% filter(no_coords == 3) %>% select(Pros, Cons)}
  }, 
  options = list(lengthChange = FALSE, paging = FALSE, searching = FALSE, ordering = FALSE,info = FALSE,
                 columnDefs = list(list(className = 'dt-body-left',width = '160px', targets = "_all"))), 
  rownames = FALSE, colnames = c('',''))
  
  output$geo3TableM <- renderDataTable({
    req(input$Geo3)
    if(input$Geo3 == 1){meth %>% filter(no_coords == 1) %>% select(2)}
    else if(input$Geo3 == 2){meth %>% filter(no_coords == 2) %>% select(2)}
  }, 
  options = list(lengthChange = FALSE, paging = FALSE, searching = FALSE, ordering = FALSE,info = FALSE,
                 columnDefs = list(list(className = 'dt-body-left', targets = "_all"))), rownames = FALSE, 
  colnames = NULL)
  
  output$distTablePC <- renderDataTable({
    req(input$dist_info)
    if(input$dist_info == 1){data %>% filter(distr_env == 1) %>% select(Pros, Cons)}
    else if(input$dist_info == 2){data %>% filter(distr_env == 2) %>% select(Pros, Cons)}
    else if(input$dist_info == 3){data %>% filter(distr_env == 3) %>% select(Pros, Cons)}
  }, 
  options = list(lengthChange = FALSE, paging = FALSE, searching = FALSE, ordering = FALSE,info = FALSE,
                 columnDefs = list(list(className = 'dt-body-left',width = '160px', targets = "_all"))), 
  rownames = FALSE, colnames = c('',''))
  
  output$distTableM <- renderDataTable({
    req(input$dist_info)
    if(input$dist_info == 1){meth %>% filter(distr_env == 1) %>% select(2)}
    else if(input$dist_info == 2){meth %>% filter(distr_env == 2) %>% select(2)}
  }, 
  options = list(lengthChange = FALSE, paging = FALSE, searching = FALSE, ordering = FALSE,info = FALSE,
                 columnDefs = list(list(className = 'dt-body-left', targets = "_all"))), rownames = FALSE, 
  colnames = NULL)
  
  output$envTablePC <- renderDataTable({
    req(input$env_info)
    if(input$env_info == 1){data %>% filter(distr_env == 4) %>% select(Pros, Cons)}
    else if(input$env_info == 2){data %>% filter(distr_env == 5) %>% select(Pros, Cons)}
    else if(input$env_info == 3){data %>% filter(distr_env == 6) %>% select(Pros, Cons)}
    else if(input$env_info == 4){data %>% filter(distr_env == 7) %>% select(Pros, Cons)}
  }, 
  options = list(lengthChange = FALSE, paging = FALSE, searching = FALSE, ordering = FALSE,info = FALSE,
                 columnDefs = list(list(className = 'dt-body-left',width = '160px', targets = "_all"))), 
  rownames = FALSE, colnames = c('',''))
  
  output$envTableM <- renderDataTable({
    req(input$env_info)
    if(input$env_info == 1){meth %>% filter(distr_env == 4) %>% select(2)}
    else if(input$env_info == 2){meth %>% filter(distr_env == 5) %>% select(2)}
    else if(input$env_info == 3){meth %>% filter(distr_env == 6) %>% select(2)}
  }, 
  options = list(lengthChange = FALSE, paging = FALSE, searching = FALSE, ordering = FALSE,info = FALSE,
                 columnDefs = list(list(className = 'dt-body-left', targets = "_all"))), rownames = FALSE, 
  colnames = NULL)
  
  # TEMP OUTPUTS ####
  output$temptable <- renderDataTable({
    req(input$temp)
    if(input$temp == 1){data %>% filter(temp == 1) %>% select(Pros, Cons)}
  }, 
  options = list(lengthChange = FALSE, paging = FALSE, searching = FALSE, ordering = FALSE,info = FALSE,
                 columnDefs = list(list(className = 'dt-body-left', width = '160px', targets = "_all"))), 
  rownames = FALSE, colnames = '')
  
  output$tempRangetable <- renderDataTable({
    req(input$op_range)
    if(input$temp == 2){
      if(input$op_range == 'within temporal range'){data %>% filter(temp == 2 & temp_range == 1) %>% select(Pros, Cons)}
      else if(input$op_range == 'with no temporal range'){data %>% filter(temp == 2 & temp_range == 2) %>% select(Pros, Cons)}}
  }, 
  options = list(lengthChange = FALSE, paging = FALSE, searching = FALSE, ordering = FALSE,info = FALSE,
                 columnDefs = list(list(className = 'dt-body-left', width = '160px', targets = "_all"))), 
  rownames = FALSE, colnames = '')
  
  output$tempLeveltable <- renderDataTable({
    req(input$op_level)
    if(input$temp == 2){
      if(input$op_level == 'Date of collection'){data %>% filter(temp == 2 & temp_level == 1) %>% select(Pros, Cons)}
      else if(input$op_level == 'Year of collection'){data %>% filter(temp == 2 & temp_level == 2) %>% select(Pros, Cons)}}
  }, 
  options = list(lengthChange = FALSE, paging = FALSE, searching = FALSE, ordering = FALSE,info = FALSE,
                 columnDefs = list(list(className = 'dt-body-left', width = '160px', targets = "_all"))), 
  rownames = FALSE, colnames = '')
  
  # DUPLICATES OUTPUTS ####
  output$duplicatesPostablePC <- renderDataTable({
    req(input$op_position)
    if(input$op_position == 'Cell'){data %>% filter(dup_position == 1) %>% select(Pros, Cons)}
    else if(input$op_position == 'Coordinates + Buffer or rounded coordinates'){data %>% filter(dup_position == 2) %>% select(Pros, Cons)}
    else if(input$op_position == 'Coordinates'){data %>% filter(dup_position == 3) %>% select(Pros, Cons)}
  }, 
  options = list(lengthChange = FALSE, paging = FALSE, searching = FALSE, ordering = FALSE,info = FALSE,
                 columnDefs = list(list(className = 'dt-body-left', width = '160px', targets = "_all"))), 
  rownames = FALSE, colnames = c('Position',''))
  
  output$duplicatesTimetablePC <- renderDataTable({
    req(input$op_time)
    if(input$op_time == 'Date'){data %>% filter(dup_time == 1) %>% select(Pros, Cons)}
    else if(input$op_time == 'Year'){data %>% filter(dup_time == 2) %>% select(Pros, Cons)}
    else if(input$op_time == 'No temporal info'){data %>% filter(dup_time == 3) %>% select(Pros, Cons)}
  }, 
  options = list(lengthChange = FALSE, paging = FALSE, searching = FALSE, ordering = FALSE,info = FALSE,
                 columnDefs = list(list(className = 'dt-body-left', width = '160px', targets = "_all"))), 
  rownames = FALSE, colnames = c('Time',''))
  
  output$duplicatesRectablePC <- renderDataTable({
    req(input$Recorder)
    if(input$Recorder == 1){data %>% filter(dup_recorder == 1) %>% select(Pros, Cons)}
  }, 
  options = list(lengthChange = FALSE, paging = FALSE, searching = FALSE, ordering = FALSE,info = FALSE,
                 columnDefs = list(list(className = 'dt-body-left', width = '160px',targets = "_all"))), 
  rownames = FALSE, colnames = c('Recorder',''))
  
  output$duplicatesPostableM <- renderDataTable({
    req(input$op_position)
    if(input$op_position == 'Cell'){meth %>% filter(dup_position == 1) %>% select(2)}
    else if(input$op_position == 'Coordinates + Buffer or rounded coordinates'){meth %>% filter(dup_position == 2) %>% select(2)}
    else if(input$op_position == 'Coordinates'){meth %>% filter(dup_position == 3) %>% select(2)}
  }, 
  options = list(lengthChange = FALSE, paging = FALSE, searching = FALSE, ordering = FALSE,info = FALSE,
                 columnDefs = list(list(className = 'dt-body-left', targets = "_all"))), rownames = FALSE, colnames = NULL)
  
  output$duplicatesTimetableM <- renderDataTable({
    req(input$op_time)
    if(input$op_time == 'Date'){meth %>% filter(dup_time == 1) %>% select(2)}
    else if(input$op_time == 'Year'){meth %>% filter(dup_time == 2) %>% select(2)}
  }, 
  options = list(lengthChange = FALSE, paging = FALSE, searching = FALSE, ordering = FALSE,info = FALSE,
                 columnDefs = list(list(className = 'dt-body-left', targets = "_all"))), rownames = FALSE, colnames = NULL)
  
  output$duplicatesRectableM <- renderDataTable({
    req(input$Recorder)
    if(input$Recorder == 1){meth %>% filter(dup_recorder == 1) %>% select(2)}
  }, 
  options = list(lengthChange = FALSE, paging = FALSE, searching = FALSE, ordering = FALSE,info = FALSE,
                 columnDefs = list(list(className = 'dt-body-left', targets = "_all"))), rownames = FALSE, colnames = NULL)
  
  # REPORT OUTPUTS ####
  introRep <- "|-------------------------------- FINAL REPORT --------------------------------|
              \nBased on the steps selected in CRF App, the summary of methods chose by the user to filter and clean biodiversity records is:"
  finalRep <- '\nThe user agrees to use this final report as a guide to process their data and rewrite this text to avoid conflicts due to plagiarism.'
  output$prueba <- renderText({report <<- paste(introRep, rep(), rep21(), rep22a(),rep22b(),
                                    rep22c(),rep22d(), rep23a(), rep23b(), rep24(),
                                    rep31(), rep32a(), rep32b(), rep32c(), rep33(), 
                                    rep34a(), rep34b(),rep4(), 
                                    rep5(), finalRep, sep ="\n")
                                report})
  rep <- renderText({
    if(is.null(input$bor)){'\n*Basis of Record* filter NOT PROVIDED'}
    else if(!is.null(input$bor)){paste('\n*Basis of Record*: The user will filter records to select', input$bor)}
    })
  rep21 <- renderText({
   if(is.null(input$download)){
     paste('\n*Taxonomical check sums up following the steps: \n1. Download option NOT PROVIDED')}
   else if(input$download == 1){
    paste('\n*Taxonomical check sums up following the steps: \n1. Download all records from higher taxonomic level')}
   else if(input$download == 2){
    paste('\n*Taxonomical check sums up following the steps: \n1. Create a list of species (accepted names and synonyms) from previous taxonomical knowledge and query databases.')}
   })
  rep22a <- renderText({
    if(is.null(input$op_checkType)){'2. The taxonomical source for standardization / harmonization will be: \n\tType NOT PROVIDED;'}
    else if(input$op_checkType == 1){'2. The taxonomical source for standardization / harmonization will be: \n\tType MANUAL;'}
    else if(input$op_checkType == 2){'2. The taxonomical source for standardization / harmonization will be: \n\tType AUTOMATIC;'}
  })
  rep22b <- renderText({
    if(is.null(input$op_spatialCov)){'\tSpatial coverage NOT PROVIDED;'}
    else if(input$op_spatialCov == 1){'\tSpatial coverage GLOBAL;'}
    else if(input$op_spatialCov == 2){'\tSpatial coverage REGIONAL;'}
  })
  rep22c <- renderText({
    if(is.null(input$op_taxCov)){'\tTaxonomical coverage NOT PROVIDED;'}
    else if(input$op_taxCov == 1){'\tTaxonomical coverage GENERAL;'}
    else if(input$op_taxCov == 2){'\tTaxonomical coverage SPECIFIC;'}
  })
  rep22d <- renderText({
    if(is.null(input$op_matchType)){'\tusing Matching Type NOT PROVIDED'}
    else if(input$op_matchType == 1){'\tusing Matching Type FUZZY'}
    else if(input$op_matchType == 2){'\tusing Matching Type EXACT'}
  })
  rep23a <- renderText({
    if(input$taxRank == 1){'3. Selecting only records identified at a proper taxonomic rank'}
    else if(input$taxRank == 0){'3. Selecting records identified at ANY taxonomic rank'}
  })
  rep23b <- renderText({
    if(input$taxAuthor == 1){'4. Selecting only records with authorship information in their scientific name'}
    else if(input$taxAuthor == 0){'4. Selecting records with or without authorship information in their scientific name'}
  })
  rep24 <- renderText({
    if(is.null(input$taxStatus)){'5. Including scientific names classified with taxonomical status: NOT PROVIDED'}
    else if(all(c(1, 2, 3) == input$taxStatus)){'5. Including scientific names classified with taxonomical status: Accepted, Synonym and Unresolved'}
    else if(all(c(1, 2) == input$taxStatus)){'5. Including scientific names classified with taxonomical status: Accepted and Synonym'}
    else if(all(c(1, 3)== input$taxStatus)){'5. Including scientific names classified with taxonomical status: Accepted and Unresolved'}
    else if(all(c(2, 3) == input$taxStatus)){'5. Including scientific names classified with taxonomical status: Synonym and Unresolved'}
    else if(all(c(1) == input$taxStatus)){'5. Including scientific names classified with taxonomical status: Accepted'}
    else if(all(c(2) == input$taxStatus)){'5. Including scientific names classified with taxonomical status: Synonym'}
    else if(all(c(3) == input$taxStatus)){'5. Including scientific names classified with taxonomical status: Unresolved'}
    })
  rep31 <- renderText({
    if(is.null(input$prev_geofilter)){'\n*Geographical check sums up the following the steps: \n1. Previous filters in download process: NOT CONSIDERED'}
    else if(input$prev_geofilter == 0){'\n*Geographical check sums up the following the steps: \n1. Previous filters in download process: Only records without known coordinate issues'}
    else if(input$prev_geofilter == 1){'\n*Geographical check sums up the following the steps: \n1. Previous filters in download process: Only records with coordinates filtered by spatial extent (area or administrative units)'}
    else if(input$prev_geofilter == 2){'\n*Geographical check sums up the following the steps: \n1. Previous filters in download process: Only records with coordinates'}
    else if(input$prev_geofilter == 3){'\n*Geographical check sums up the following the steps: \n1. Previous filters in download process: Do not apply'}
  })
  rep32a <- renderText({
    if(is.null(input$Geo21)){'2. Location check: \n\t- Check coordinates precision: NOT CONSIDERED'}
    else if(input$Geo21 == 1){'2. Location check: \n\t- Check coordinates precision: Filter and discard records which precision value is below an established threshold'}
    else if(input$Geo21 == 2){'2. Location check: \n\t- Check coordinates precision: Use number of decimal digits of coordinates as a measure of their precision'}
    else if(input$Geo21 == 3){'2. Location check: \n\t- Check coordinates precision: Do not apply'}
  })  
  rep32b <- renderText({
    if(is.null(input$Geo22)){'\t- Validate records based on coordinates values: NOT CONSIDERED'}
    else if(input$Geo22 == 1){'\t- Validate records based on whether latitude and longitude present the exact same value'}
    else if(input$Geo22 == 2){'\t- Validate records based on whether coordinates values are out of a reliable range'}
    else if(input$Geo22 == 3){'\t- Validate records based on whether latitude or longitude values equals to zero'}
    else if(input$Geo22 == 4){'\t- Validate records based on coordinates values: Do not apply'}
  }) 
  rep32c <- renderText({
    if(is.null(input$Geo23)){'\t- Validate coordinates position: NOT CONSIDERED'}
    else if(input$Geo23 %in% 1:5){'\t- Validate coordinates position'}
    else if(input$Geo23 == 6){'\t- Validate coordinates position: Do not apply'}
  }) 
  rep33 <- renderText({
    if(is.null(input$Geo3)){'3. Correct / assign coordinates to records without them or errors from previous validations: NOT CONSIDERED'}
    else if(input$Geo3 == 1){'3. Correct / assign coordinates to records without them or errors from previous validations: Retrieve coordinates indicated in other formats from other fields as locality information.'}
    else if(input$Geo3 == 2){'3. Correct / assign coordinates to records without them or errors from previous validations: Use locality information or position description to generate coordinates.'}
    else if(input$Geo3 == 3){'3. Correct / assign coordinates to records without them or errors from previous validations: Do not correct coordinate values'}
  })
  rep34a <- renderText({
    if(is.null(input$dist_info)){'4. Distributional Outliers check: NOT CONSIDERED'}
    else if(input$dist_info == 1){'4. Distributional Outliers: check if coordinates placed in the species native range / extent of presence'}
    else if(input$dist_info == 2){'4. Distributional Outliers: check if location information matches with their native region'}
    else if(input$dist_info == 3){'4. Distributional Outliers check: Do not apply'}
  })
  rep34b <- renderText({
    if(is.null(input$env_info)){'5. Environmental Outliers check: NOT CONSIDERED'}
    else if(input$env_info == 1){'5. Environmental Outliers check: Calculate environmental centroids for the species and validate outliers'}
    else if(input$env_info == 2){'5. Environmental Outliers check: Calculate environmental space for species and check overlaps and delete outliers'}
    else if(input$env_info == 3){'5. Environmental Outliers check: Overlap environmental information by geographical position and filter occurrences by threshold.'}
    else if(input$env_info == 4){'5. Environmental Outliers check: Do not apply'}
  })
  rep4 <- renderText({
   if(is.null(input$temp)){'\n*Temporal information filter NOT PROVIDED'}
   else if(input$temp == 1){'\n*Temporal information filter will not applied'}
   else if (input$temp == 2){paste('\n*Temporal information filter as: Data ', input$op_range,' using ', input$op_level)}
 }) 
  rep5 <- renderText({
   if(input$Recorder == 'TRUE'){
     if(is.null(input$op_position)){
       if(is.null(input$op_time)){
         '\nFinally the identification and deletion of *duplicate records* NOT PROVIDED'}
       else if(!is.null(input$op_time)){
         paste('\nFinally the identification and deletion of *duplicate records* will be done as the combination of same species', input$op_time, 'Recorder')}}
     else if(!is.null(input$op_position)){
       if(is.null(input$op_time)){
         paste('\nFinally the identification and deletion of *duplicate records* will be done as the combination of same species', input$op_position, 'Recorder')}
       else if(!is.null(input$op_time)){
         paste('\nFinally the identification and deletion of *duplicate records* will be done as the combination of same species', input$op_position, input$op_time, 'Recorder')}}}
   
   else if(input$Recorder == 'FALSE'){
     if(is.null(input$op_position)){
       if(is.null(input$op_time)){
         '\nFinally the identification and deletion of *duplicate records* NOT PROVIDED'}
       else if(!is.null(input$op_time)){
         paste('\nFinally the identification and deletion of *duplicate records* will be done as the combination of same species', input$op_time)}}
     else if(!is.null(input$op_position)){
       if(is.null(input$op_time)){
         paste('\nFinally the identification and deletion of *duplicate records* will be done as the combination of same species', input$op_position)}
       else if(!is.null(input$op_time)){
         paste('\nFinally the identification and deletion of *duplicate records* will be done as the combination of same species', input$op_position, input$op_time)}}}
   
 })
  # DOWNLOAD OUTPUT####
  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste('CRF_App_FinalReport_', Sys.Date(), '.txt', sep = '\t')
    },
    content = function(con) {
      write.table(report, con, row.names = FALSE, col.names = FALSE, sep = ';', quote = FALSE)
    }
  )

}

# Run the application 
shinyApp(ui = ui, server = server)
