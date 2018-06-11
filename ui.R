library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(d3heatmap)
library(heatmaply)

# Choices for drop-downs
vars <- c(
  "File 1" = "file1",
  "File 2" = "file2"
)

vars2 <- c(
  "1" = "pos",
  "0" = "neg",
  "All" = "all"
)

vars_file <- c(
  "File 1" = "f1",
  "File 2" = "f2"
)

navbarPage("MApp", id="nav",

  tabPanel("Interactive Map", value = "map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css")
        #includeScript("gomap.js")
      ),

      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class="modal" instead.
      absolutePanel(id = "controls", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 400, height = "auto",

        tabsetPanel(
          tabPanel("Distributions",
            plotOutput("histfile1", height = 200),
            plotOutput("histfile2", height = 200),
            checkboxInput("min_max_data1", "Show min/max year on map (Dataset 1)", FALSE),
            checkboxInput("min_max_data2", "Show min/max year on map (Dataset 2)", FALSE),
            numericInput("nb_bins_min_max", "Number of bins", min = 1, max = 500, value = 10)
          ),
          tabPanel("Correlation",
            plotOutput("plot1"),
            verbatimTextOutput("info"),
            checkboxInput("temporal_corr", "Show temporal correlation on map", FALSE),
            numericInput("nb_bins_corr", "Number of bins", min = 1, max = 500, value = 10)
          ),
          tabPanel("Prevalence",
            plotOutput("plot2"),
            verbatimTextOutput("info2"),
            checkboxInput("temporal_prev", "Show prevalence on map", FALSE),
            numericInput("nb_bins_prev", "Number of bins", min = 1, max = 500, value = 10)
          ),
          tabPanel("X")
        )
        
      ),

      absolutePanel(id = "controls", fixed = TRUE,
        draggable = TRUE, top = 60, left = 20, right = "auto", bottom = "auto",
        width = 200, height = "auto",
        
        tabsetPanel(
          tabPanel("Map",
            checkboxInput("hide_points1", "Dataset 1", TRUE),
            radioButtons('File1', 'File', c('file1', 'file2'), inline = TRUE, selected = "file1"),
            radioButtons('Data1', 'Status', c('0', '1', "All"), inline = TRUE, selected = "1"),

            hr(),

            checkboxInput("hide_points2", "Dataset 2", TRUE),
            radioButtons('File2', 'File', c('file1', 'file2'), inline = TRUE, selected = "file2"),
            #radioButtons('Data2', 'Status', c('0', '1', "All"), inline = TRUE, selected = "1"),
            uiOutput("radio_file2"),

            hr(),

            uiOutput("min_year_choice1"),
            uiOutput("max_year_choice1"),

            hr(),

            
            

            textOutput("info_file1"),
            textOutput("info_file2")
          ),
          tabPanel("X")
        )

    )
    )
  ),

  tabPanel("Temporal Heatmaps", value = "heatmaps",

    absolutePanel(fixed = TRUE,
        draggable = FALSE, top = "auto", left = 5, right = "auto", bottom = "1%",
        width = "14%", height = "9%",
        numericInput("nb_bins_temp", "Number of bins", min = 1, max = 500, value = 25)),

    absolutePanel(fixed = TRUE,
        draggable = FALSE, top = "auto", left = "auto", right = 5, bottom = "1%",
        width = "10%", height = "9%",
        checkboxInput("dendrogram", "Dendogram", FALSE)),

    absolutePanel(fixed = TRUE,
        draggable = FALSE, top = 60, left = "1%", right = "auto", bottom = "12%",
        width = "49%", height = "auto",
        plotlyOutput("hm_lat", width = "100%", height = "100%")
        ),

    absolutePanel(fixed = TRUE,
        draggable = FALSE, top = 60, left = "auto", right = "1%", bottom = "12%",
        width = "49%", height = "auto",
        plotlyOutput("hm_lon", width = "100%", height = "100%")
        )
  ),

  tabPanel("Multi Sampling", value = "multi",

    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = FALSE, top = 60, left = "1%", right = "49%", bottom = "1%",
        width = "auto", height = "auto",
        plotOutput("plot_multi2",height="100%",
          dblclick = "plot_multi2_dblclick",
          brush = brushOpts(id = "plot_multi2_brush",resetOnNew = TRUE)
          )
        ),
    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = FALSE, top = 60, left = "51%", right = "1%", bottom = "1%",
        width = "auto", height = "auto",
        plotOutput("plot_multi",height="100%")

        )
    ),

  navbarMenu("Explore Files",

    tabPanel("File 1", value = "file1_exp",
      fluidRow(
        column(2,
          uiOutput("min_year_1")
        ),
        column(2,
          uiOutput("max_year_1")
        ),
        column(2,
          radioButtons('Status1', 'Status', c('0', '1', "All"), inline = TRUE, selected = "All")
        )
      ),
      hr(),
      DT::dataTableOutput("file1_table")
    ),

    tabPanel("File 2", value = "file2_exp",
      fluidRow(
        column(2,
          uiOutput("min_year_2")
        ),
        column(2,
          uiOutput("max_year_2")
        ),
        column(2,
          radioButtons('Status2', 'Status', c('0', '1', "All"), inline = TRUE, selected = "All")
        )
      ),
      hr(),
      DT::dataTableOutput("file2_table")
    )
  ),

  tabPanel("Upload Files", value = "upload",
    sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose file 1 to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
      ),
      fileInput('file2', 'Choose file 2 to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
      ),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"'),
      selectInput("file_sel", "Show selected file", vars_file, selected = "f1")
    ),
    mainPanel(
      tableOutput('contents')
    )
  )
  
  ),

  tabPanel("Help/Example files", value = "help",

      h5("If the application freezes, please reload the page and upload your files again."),
      br(),

      h4("Example datasets"),
        h5("Two example datasets are available in MApp, both randomly generated under certain rules that would allow you to observe and test the different features of MApp:"),
        h5("\"Two correlated examples\" corresponds to two datafiles correlated in space and time. You can load them to test the spatial correlation directly on the map, as well as the temporal heatmaps."),
        h5("\"Multi-samplig example\" corresponds to one datafile, with individuals sampled multiple times. You can load it to test the multi-sampling tool provided in MApp."),
      selectInput("dataset", "Choose an example dataset to upload:", choices = c("Select dataset","Two correlated examples", "Multi-sampling example")),


      br(),
      h4("Upload Files"),
        h5("You can upload up to two .csv files using the two corresponding buttons. Depending on your file(s), you must select the right options (header, separator, quote)."),
        h5("If you upload only one file, then it will also be considered as file 2."),
        h5("Note: Your files must be in the csv format, the first 5 columns being (in order): Id, Latitude, Longitude, Year, and Status (0 or 1, corresponding to negative and positive)."),
        imageOutput("csv_file",height = 300),
        h5("You can check the files on the right panel once they are uploaded."),
        imageOutput("input", height = 300),
      br(),
      h4("Explore Files"),
        h5("Use this tab to explore and search in your files. File 2 is the same as file 1 if only one file has been uploaded."),
        imageOutput("explore", height = 300),
      br(),
        strong("General comment - number of bins: in some of MApp's features, you can choose a “number of bins”. This number is used to divide the map into a certain number of squares (depending on the number of bins) for different statistics you might wish to apply locally on the map."),
      br(),
      h4("Interactive Map"),
        h5("Datasets - Two datasets, defined in the left panel, can be displayed on the map. Each dataset consists of a file and a status. The default datasets are [file1 - 1 / file2 - 1] if two files have been uploaded, and [file1 - 1 / file1 - 0] if only one file has been uploaded. You can also choose the minimum/maximum year for the displayed data."),
        h5("Map - The map is automatically generated, mapping both datasets. Each point correspond to an individual sample, its color and size indicating the year. The red points correspond to the Dataset 1 and the blue points to the Dataset 2."), 
        imageOutput("map_example", height = 300),
        h5("Clicking on a particular point on the map shows the lat/long corresponding to this point, as well as the distributions relative to the individuals present at those coordinates."),
        imageOutput("distrib_select", height = 300),
        h5("Distributions - The first tab on the right panel corresponds to the temporal distributions of the datasets 1 and 2. The distribution depends only on the points that are displayed on the map following the changes of zoom or centering of the map. The two boxes show/hide the minimum and maximum year in each square defined by the number of bins."),
        imageOutput("dataset", height = 300),
        h5("By ticking the corresponding box, you can show the minimum/maximum year represented in each square of the map defined by the number of bins."),
        imageOutput("min_max", height = 300),
        h5("Correlation - The second tab corresponds to the temporal correlation between the two datasets. The graph represents the temporal distribution of each Dataset. A Pearson’s correlation coefficient is calculated between those two distributions and returned. You can also calculate the temporal correlation for each square defined by the number of bins by ticking the box."),
        imageOutput("correl_select", height = 300),
        h5("Prevalence - The third tab corresponds to the prevalence. The graph represents the evolution of the prevalence - Data1/(Data1+Data2) - over years. Here, the prevalence is defined as the proportion of points from Data1 among all the points represented on the map. The overall prevalence is also returned. You can calculate the prevalence for each square defined by the number of bins by ticking the corresponding box."),
        imageOutput("prev_select", height = 300),
      br(),
      h4("Temporal Heatmaps"),
        h5("The temporal heatmaps sum up the spatial and temporal correlation between the two datasets. They give an insight about the variation of spatial repartition of both datasets over years."),
        h5("There are two of them: the first one represents latitude correlation and the second one, the longitude correlation. To build them, the map is once again separated into a certain number of squares. For each year, two projected vectors are then defined: one for the latitudes and one for the longitudes."),
        imageOutput("grid", height = 300),
        h5("The heatmaps correspond to the Pearson’s correlation coefficients for all the pair of years, respectively for the latitudes and the longitudes vectors. You can tick the corresponding box to show/hide the dendograms"),
        imageOutput("hm_dendo",height = 300),
      br(),
      h4("Multi Sampling"),
        h5("In some cases, the datasets contain repeated identifiers (e.g. individuals sampled multiple times over years). In that case, the multi-sampling tab sums up the information about those individual in two graphs."),
        h5("The first graph is a graphical representation of the history of each of those multi-sampled individual: each line corresponds to one individual, each red point to a positive sample, each blue point to a negative sample."),
        imageOutput("multi1", height = 300),
        h5("The second graph sums up the information contained in the first one. Each point is a transition from negative to positive (top left part of the graph) or positive to negative (bottom right part) for one individual. The X and Y-axes correspond to the years. An arrow links two point from the same individual."),
        imageOutput("multi2", height = 300)
    ),

    tabPanel("Contact", value = "contact",
      h5("Abdelkader Behdenna"),
      h5("Institute of Biodiversity, Animal Health and Comparative Medicine"),
      h5("College of Medical, Veterinary & Life Sciences"),
      h5("Graham Kerr Building"),
      h5("University of Glasgow"),
      h5("Glasgow G12 8QQ"),
      br(),
      h5("You can report any bug or suggestion to:"),
      h5("abdelkader.behdenna@glasgow.ac.uk.")
    ),

  conditionalPanel("false", icon("crosshair"))
)