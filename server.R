library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(ggvis)
library(ggplot2)
library(foreign)
library(datasets)
library(rMaps)

# set working directory here.
data1 <- read.csv("data/final_data.csv")
all_data1 <- data1

#library(rsconnect)
#deployApp()

shinyServer(function(input, output, session) {
  dat <- reactive({
    d <- all_data1
    
    if(input$options == "Yes"){
      d<-d[d$alreadyInRIF == 1,]
    }
    if(input$options == "No"){
      d<-d[d$alreadyInRIF == 0,]
    }
    #d <- as.data.frame(d)
    d
  })
  
  vis <- reactive({
    dat %>% ggvis(~MEALS, ~PCST_E28,fill = ~aiRIFtxt) %>%
      add_axis("x", title = "Meals") %>%
      add_axis("y", title = "Reading Scores") %>%
      set_options(width = 500, height = 500) %>%
      layer_points( size :=50, size.hover:=200,
                    fillOpacity := 0.8, fillOpacity.hover := 1, key := ~SNAME) %>%
      add_tooltip(function(df) paste0("<b>",df$SNAME,"</b><br>(",df$MEALS,", ",df$PCST_E28,")")) %>%
      add_legend("fill", title = "Already In RIF", values = c("Yes", "No")) %>%
      scale_numeric("x",domain = c(0,100)) %>%   scale_numeric("y",domain = c(0,600))
    
  })
  vis %>% bind_shiny("plot1")
  
  output$mealsPlot <- renderPlot({
    junk <- data1
    junk2 <- quantile(junk$MEALS, probs = c(input$Mquant/100))
    junk$mealsQuant <- findInterval(junk$MEALS, junk2)
    
    a <- ggplot(junk, aes(x = MEALS, fill = factor(mealsQuant)))
    a + geom_histogram(binwidth = 3)+labs(x = "Meals")+
      scale_fill_manual(values = c("darkblue", "orange"))
    
  })
  
  output$PCSTPlot <- renderPlot({
    junk <- data1
    junk3 <- quantile(junk$PCST_E28, probs = c(input$Equant/100))
    junk$pcst_e28Quant <- findInterval(junk$PCST_E28, junk3)
    
    a <- ggplot(junk, aes(x = PCST_E28, fill = factor(pcst_e28Quant)))
    a + geom_histogram(binwidth = 10)+labs(x = "Reading Scores")+
      scale_fill_manual(values = c("orange","darkblue"))
  })
  
  # Return the requested data1set
  groupInput <- reactive({
    group2 <- rep(FALSE,dim(data1)[1])
    if("A" %in% input$checkGroup){group2 <- (data1$mealsQuant == 3 & data1$pcst_e28Quant == 0)|group2}
    if("B" %in% input$checkGroup){group2 <- (data1$mealsQuant == 3 & data1$pcst_e28Quant == 1)|group2}
    if("C" %in% input$checkGroup){group2 <- (data1$mealsQuant == 3 & data1$pcst_e28Quant == 2)|group2}
    if("D" %in% input$checkGroup){group2 <- (data1$mealsQuant == 3 & data1$pcst_e28Quant == 3)|group2}
    if("E" %in% input$checkGroup){group2 <- (data1$mealsQuant == 2 & data1$pcst_e28Quant == 0)|group2}
    if("F" %in% input$checkGroup){group2 <- (data1$mealsQuant == 2 & data1$pcst_e28Quant == 1)|group2}
    if("G" %in% input$checkGroup){group2 <- (data1$mealsQuant == 2 & data1$pcst_e28Quant == 2)|group2}
    if("H" %in% input$checkGroup){group2 <- (data1$mealsQuant == 2 & data1$pcst_e28Quant == 3)|group2}
    if("I" %in% input$checkGroup){group2 <- (data1$mealsQuant == 1 & data1$pcst_e28Quant == 0)|group2}
    if("J" %in% input$checkGroup){group2 <- (data1$mealsQuant == 1 & data1$pcst_e28Quant == 1)|group2}
    if("K" %in% input$checkGroup){group2 <- (data1$mealsQuant == 1 & data1$pcst_e28Quant == 2)|group2}
    if("L" %in% input$checkGroup){group2 <- (data1$mealsQuant == 1 & data1$pcst_e28Quant == 3)|group2}
    if("M" %in% input$checkGroup){group2 <- (data1$mealsQuant == 0 & data1$pcst_e28Quant == 0)|group2}
    if("N" %in% input$checkGroup){group2 <- (data1$mealsQuant == 0 & data1$pcst_e28Quant == 1)|group2}
    if("O" %in% input$checkGroup){group2 <- (data1$mealsQuant == 0 & data1$pcst_e28Quant == 2)|group2}
    if("P" %in% input$checkGroup){group2 <- (data1$mealsQuant == 0 & data1$pcst_e28Quant == 3)|group2}
    
    #if("A" %in% input$checkGroup){group2 <- (data1$mealsQuant == 3 & data1$pcst_e28Quant == 0)|group2}
    
    #if("B" %in% input$checkGroup){group2 <- (data1$mealsQuant == 2 & data1$pcst_e28Quant == 0) |
    #                         (data1$mealsQuant == 3 & data1$pcst_e28Quant == 1)|group2}
    
    #if("C" %in% input$checkGroup){group2 <- (data1$mealsQuant == 1 & data1$pcst_e28Quant == 0) |
    #                         (data1$mealsQuant == 2 & data1$pcst_e28Quant == 1) |
    #                         (data1$mealsQuant == 3 & data1$pcst_e28Quant == 2)|group2}
    
    #if("D" %in% input$checkGroup){group2 <- (data1$mealsQuant == 0 & data1$pcst_e28Quant == 0) |
    #                         (data1$mealsQuant == 1 & data1$pcst_e28Quant == 1) |
    #                         (data1$mealsQuant == 2 & data1$pcst_e28Quant == 2) |
    #                         (data1$mealsQuant == 3 & data1$pcst_e28Quant == 3)|group2}
    
    #if("E" %in% input$checkGroup){group2 <- (data1$mealsQuant == 0 & data1$pcst_e28Quant == 1) |
    #                         (data1$mealsQuant == 1 & data1$pcst_e28Quant == 2) |
    #                         (data1$mealsQuant == 2 & data1$pcst_e28Quant == 3)|group2}
    
    
    #if("F" %in% input$checkGroup){group2 <- (data1$mealsQuant == 0 & data1$pcst_e28Quant == 2) |
    #                         (data1$mealsQuant == 1 & data1$pcst_e28Quant == 3)|group2}
    
    #if("G" %in% input$checkGroup){group2 <- (data1$mealsQuant == 0 & data1$pcst_e28Quant == 3)|group2}
    
    newdata1 <- data1[group2,c(1,8,13,18)]
    
    if(input$sort == "School Name"){newdata1 <- newdata1[order(newdata1$SNAME),]}
    
    if(input$sort == "Meals"){newdata1 <- newdata1[order(-newdata1$MEALS,newdata1$PCST_E28),]}
    
    if(input$sort == "Reading Scores"){newdata1 <- newdata1[order(newdata1$PCST_E28),]}
    
    if(input$sort == "Letter"){newdata1 <- newdata1[order(newdata1$letter,-newdata1$MEALS,newdata1$PCST_E28),]}
    
    newdata1
    
  })
  
  output$value <- renderPrint({ input$checkGroup })
  
  groupInput2 <- reactive({
    y<- groupInput()
    names(y) <- c("Name", "Meals", "Reading Score", "Letter")
    y
  })
  
  output$groupInput <- renderTable({
    y<- groupInput()
    names(y) <- c("Name", "Meals", "Reading Score", "Letter")
    y
  },include.rownames=FALSE)
  
  
  output$downloadData <- downloadHandler(
    filename = 'list.csv',
    content = function(file) {
      write.csv(groupInput2(), file,row.names=FALSE)
    }
  )
  
#   ## Interactive Map ###########################################
#   
#   # Create the map
#   map <- createLeafletMap(session, "map")
#   outputOptions(output, "map")#, suspendWhenHidden=FALSE)
#   
# 
#   zipsInBounds <- reactive({
#     if (is.null(input$map_bounds))
#       return(data1[FALSE,])
#     bounds <- input$map_bounds
#     latRng <- range(bounds$north, bounds$south)
#     lngRng <- range(bounds$east, bounds$west)
#     
#     group3 <- c()
#     if("A" %in% input$checkGroup){group3 <- c(group3,"A")}
#     if("B" %in% input$checkGroup){group3 <- c(group3,"B")}
#     if("C" %in% input$checkGroup){group3 <- c(group3,"C")}
#     if("D" %in% input$checkGroup){group3 <- c(group3,"D")}
#     if("E" %in% input$checkGroup){group3 <- c(group3,"E")}
#     if("F" %in% input$checkGroup){group3 <- c(group3,"F")}
#     if("G" %in% input$checkGroup){group3 <- c(group3,"G")}
#     if("H" %in% input$checkGroup){group3 <- c(group3,"H")}
#     if("I" %in% input$checkGroup){group3 <- c(group3,"I")}
#     if("J" %in% input$checkGroup){group3 <- c(group3,"J")}
#     if("K" %in% input$checkGroup){group3 <- c(group3,"K")}
#     if("L" %in% input$checkGroup){group3 <- c(group3,"L")}
#     if("M" %in% input$checkGroup){group3 <- c(group3,"M")}
#     if("N" %in% input$checkGroup){group3 <- c(group3,"N")}
#     if("O" %in% input$checkGroup){group3 <- c(group3,"O")}
#     if("P" %in% input$checkGroup){group3 <- c(group3,"P")}
#     
#     subset(data1,
#            LAT >= latRng[1] & LAT <= latRng[2] &
#              LONG >= lngRng[1] & LONG <= lngRng[2])
#     
#   })
  
#   session$onFlushed(once=TRUE, function() {
#     paintObs <- observe({
#       #colorData <- all_data1[["letter"]]
#       #colors <- brewer.pal(16, "Spectral")[colorData]
#       #colors <- colors[match(locdata()$SNAME, all_data1$SNAME)]
#       
#       map$clearShapes()
#       map$addCircle(locdata()$LAT,locdata()$LONG,100)#,
#        #             options = list(color = colors))
#     })
#     
#     session$onSessionEnded(paintObs$suspend)
#   })
  
#   # Show a popup at the given location
#   showZipcodePopup <- function(id, lat, lng) {
#     selectedZip <- locdata()[locdata()$LAT == lat & locdata()$LONG == lng,]
#     content <- as.character(tagList(
#       tags$h5("Letter:", as.character(selectedZip$letter)),
#       tags$strong(HTML(sprintf("%s",
#                                selectedZip$SNAME
#       ))), tags$br(),
#       sprintf("Reading Score: %s", as.numeric(selectedZip$PCST_E28)), tags$br(),
#       sprintf("Percent of students eligible for reduced lunch: %s%%", as.integer(selectedZip$MEALS)), tags$br(),
#       sprintf("Number of enrolled students: %s", selectedZip$ENROLL)
#     ))
#     map$showPopup(lat, lng,content)
#   }
  
#   # When map is clicked, show a popup with city info
#   clickObs <- observe({
#     map$clearPopups()
#     event <- input$map_shape_click
#     if (is.null(event))
#       return()
#     
#     isolate({
#       showZipcodePopup(event$id, event$lat, event$lng)
#     })
#   })
#   
#   session$onSessionEnded(clickObs$suspend)
  
locdata <- reactive({
  group3 <- c()
  if("A" %in% input$checkGroup){group3 <- c(group3,"A")}
  if("B" %in% input$checkGroup){group3 <- c(group3,"B")}
  if("C" %in% input$checkGroup){group3 <- c(group3,"C")}
  if("D" %in% input$checkGroup){group3 <- c(group3,"D")}
  if("E" %in% input$checkGroup){group3 <- c(group3,"E")}
  if("F" %in% input$checkGroup){group3 <- c(group3,"F")}
  if("G" %in% input$checkGroup){group3 <- c(group3,"G")}
  if("H" %in% input$checkGroup){group3 <- c(group3,"H")}
  if("I" %in% input$checkGroup){group3 <- c(group3,"I")}
  if("J" %in% input$checkGroup){group3 <- c(group3,"J")}
  if("K" %in% input$checkGroup){group3 <- c(group3,"K")}
  if("L" %in% input$checkGroup){group3 <- c(group3,"L")}
  if("M" %in% input$checkGroup){group3 <- c(group3,"M")}
  if("N" %in% input$checkGroup){group3 <- c(group3,"N")}
  if("O" %in% input$checkGroup){group3 <- c(group3,"O")}
  if("P" %in% input$checkGroup){group3 <- c(group3,"P")}
  subset(data1, letter %in% group3)
  m <- all_data1 %>% filter(letter %in% group3)
  m
})



  
  output$myChart2 <- renderMap({
    
    df <- data.frame(locdata())
    map <- Leaflet$new()
    map$params$width="100%"
    map$params$urlTemplate="//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png"
    map$params$layerOpts$attribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>')
    map$html_assets$js = "gomap.js"
    map$html_assets$css = "styles.css"
    map$setView(c(34.052363, -118.399373), 10)
    #map$tileLayer(provider = 'Stamen.TonerLite')
    for (i in 1:nrow(df)) {
      map$marker(c(df[i, "LAT"], df[i, "LONG"]), 
                 bindPopup = (paste0("<b> Letter: ", df[i,"letter"],
                                     "<br>",df[i,"SNAME"],
                                     "</b><br>Reading Score: ",df[i,"PCST_E28"],
                                     "<br>Percent of students eligible for reduced lunch: ",df[i,"MEALS"],"%",
                                     "<br>Number of enrolled students: ",df[i,"ENROLL"])
                 )                 
              )
    }
    
    map
  })  
    
})
