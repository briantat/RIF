library(shiny)
library(leaflet)
library(ggvis)
library(rCharts)


shinyUI(navbarPage("Reading Is Fundamental", id="nav",
   tabPanel("Background", 
      sidebarLayout(
        sidebarPanel(p("
                     Reading is Fundamental (RIF) is the largest children's literacy nonprofit organization in the United States, with programs in every state. The goal of RIF is to develop a love and passion for reading among elementary school-aged children in low-income families by providing them with free books and other reading resources. RIF of Southern California is one of the largest branches of RIF, and aims to bring over 155,000 books to disadvantaged students in the 2014-2015 school year. The goal of this report is to find good criteria for selecting which schools in Los Angeles and Orange County should be included in this program.
                     "),p("For more information, please visit"), a("http://rifsocal.org/")), mainPanel(
                       h4("Data Description"),
                       
                       p("The main dataset of school data and their testing scores used for this project was obtained from the California Department of Education. The API (Academic Performance Index) data file for 2013 was used as the most recent available data possible for analysis. The dataset contained information regarding standardized testing, socio-economic status, and so on from all of the schools in California. 10946 schools and their respective reading scores were used to conduct a statewide benchmark of reading score. The data was then filtered for elementary schools within Los Angeles county and Orange county for RIF's focus in Southern California. 
                         "),
                       p("A list of schools that participated in the program provided by RIF was combined with the filtered data along with the latitude and longitude coordinates of the elementary schools (obtained from the Elementary and Secondary Information System).
                          "),
                       p("Using the initial dataset of all the schools, a statewide benchmark for elementary schools was computed for reading scores. Schools that had performed better than the statewide benchmark would not be taken into consideration for being academically needy.
                          "),
                       p("Variables of interest used to classify the neediness of a school included the reading score of a school (PCST_E28) and the percent of enrolled students eligible for free or reduced lunch (MEALS). The MEALS variable was used as a proxy for socio-economic status (SES), since there was no publicly available data on the average income for parents for each school. The translation of the MEALS variable to socio-economic status was based on the assumption that schools with higher eligibility rates for free or reduced lunch would have lower socio-economic status.
                          "),
                       h4("Navigation"),
                       p("For the Current Plot tab, there is a plot of  all elementary schools within CA based on Meals and Reading Score. The blue dots in the program represent all of the elementary schools that are not in the RIF program and the orange dots represent the elementary schools that are in the RIF program. On the left, there is a click down menu that allows the user to view schools that are in or out of the RIF program."),
                       p("For the Distribution of Variables tab, there is a histogram for both Meals and Reading Score that allows the user to customize the quantile and see the area of interest. The area of interest is labeled as orange as the most neediest."),
                       p("For the Table of Reference tab, the user is allowed to choose a selection of letter categories to create their own list of neediest schools. This allows the user to choose based on value or quantile of any of the two variables."),
                       p("Once the user selects the schools, the Lists tab will be updated and allow the user to sort the list itself in any fashion to further examine the schools. Once the user is satisfied with the schools within the list, the list can be downloaded via the download button. The Map tab will also update itself and allow the user to view within the area the selected schools. Also by clicking a specific marker, the user may view specific information regarding reading score, meals percentage, and student enrollment.")
                          
      )      
            
            
            )),
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
            img(src="color.PNG"),
            h6("For classification of schools, the schools were placed into different categories (in letter format) of their reading score and meals variables. For our illustration, schools that were given the letter A were determined to be most qualified for the RIF program as their reading scores are in the lowest 25% of reading scores available and their meals are in the highest 25% of meals available.")
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
  tabPanel("Map",showOutput('myChart2', 'leaflet')),
  tabPanel("About", p("My name is Brian Tat and this was an interactive website created for the Reading is Fundamental group based in Los Angeles as part of a consulting project. For the code, you may visit"), a("https://github.com/briantat"))
   )
)

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