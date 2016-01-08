library(shiny)
library(leaflet)
library(ggvis)
library(rCharts)


shinyUI(navbarPage("Reading Is Fundamental", id="nav",
   tabPanel("Current Plot",
      sidebarLayout(
        sidebarPanel(
          wellPanel(
            selectInput("options","Schools In The RIF Program",c("All","Yes","No"))    
              )
            ),  
          mainPanel(
            ggvisOutput("plot1")     
          )    
        )
      ),
                   
                   
    tabPanel("Distribution of Variables", 
    # Sidebar with a slider input for the number of bins
      sidebarLayout(
        sidebarPanel(
          sliderInput("Mquant",
            "Quantile for Meals",
            min = 75,
            max = 100,
            value = 85),
         sliderInput("Equant",
            "Quantile for Reading Scores",
            min = 0,
            max = 25,
            value = 20) 
        ),
     # Show a plot of the generated distribution
        mainPanel(
          plotOutput("mealsPlot"),#,width = "50%"),
          plotOutput("PCSTPlot")#,width = "50%")
        )
      )
    ),
  tabPanel("Table of Reference", 
        sidebarLayout(
          sidebarPanel(
            checkboxGroupInput("checkGroup", label = h3("Selection of Categories"), 
                                choices = LETTERS[1:16])#,
      #                          selected = LETTERS[1])
                              ),
          mainPanel(
            img(src="color.PNG")                
            )
        )
    ),
  tabPanel("Lists",
    sidebarLayout(
       sidebarPanel(
          selectInput("sort", "Sort by:", 
                       choices = c("School Name","Meals","Reading Scores","Letter")),
          downloadButton('downloadData', 'Download')
            ),

       mainPanel(
          h4("Lists"),
          tableOutput(outputId="groupInput") )
        )
    ),
  tabPanel("Map",showOutput('myChart2', 'leaflet'))
#   tabPanel("Map",
#            div(class="outer",
#                tags$head(
#                  # Include our custom CSS
#                  includeCSS("styles.css"),
#                  includeScript("gomap.js")
#                ),
#                
#                leafletMap("map", width="100%", height="100%",
#                           initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
#                           initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
#                           options=list(
#                             center = c(34.052363, -118.399373),
#                             zoom = 10,
#                             maxBounds = list(list(33.316168, -118.924221), list(34.860564, -117.592129))
#                             
#                           )
#                ),
#                
#                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, draggable = TRUE,
#                              top = 70, left = "auto", right = 20, bottom = "auto",
#                              width = 330, height = "auto"
#                ),
#                
#                tags$div(id="cite",
#                         'Data compiled for ', tags$em('Reading is Fundamental')
#                )
#            )
#   )
           
))