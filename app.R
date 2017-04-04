## app.R ##
## Application controller ##
library(shinydashboard)
library(rjson)
library(dplyr)
library(ggplot2)

# Import our modules
source("read_past_data.R")
source("predict.R")

pastData <- readPastDataset("2016_hist.csv")
date_from <-"2016/03/04"
today <- as.Date("2016/03/19")
date_till <-"2016/03/26"
min_freq_type <- 15

# Visualization should include an option to aggragate al types
crime_type_options <- c("All", as.character(unique(pastData$Primary.Type)))

# To Do: Decouple view from business logic -> move js/html code to assets/ and source it instead of embedding
header <- dashboardHeader(title = "CrimeStorm")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Source code", icon = icon("file-code-o"), 
             href = "https://github.com/XomniaJADS/crimeStorm"),
    menuItem("About", icon = icon("user-o"), tabName = "about")
  ),
  sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                    label = "Search...")
)


aboutHTML <-"Crime storm was created 18/3/2017 within a 24h datathon organised by <a href=\"http://www.xomnia.com\">Xomnia</a>.
            The showcase presented in the dashboard shows is set to show a period from year of 2016, because the data for 2017 were unavailable. 
            The application was created by a team of six PDEng trainees of <a href=\"http://www.jads.nl/\"> Jheronimus Academy of Data Science</a>: 
            <ul>
             <li>Andriy Drebot</li>
             <li>Mahmoud Khodier (<a href=\"mailto:m.khodier@tue.nl\">contact</a>)</li>
             <li>You Yue Huang</li>
             <li>Diego Perez</li>
             <li>Adam Zika</li>
             <li>Manos Stergiadis</li>
            </ul>"
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            fluidRow(
               box(includeHTML('assets/templates/test.html'),
                   selectInput("crime_type", "Type", crime_type_options, selected="")
                 
                    ),
            
               box(#title = "Crime type",  status = "primary",
                 tabsetPanel(
                   tabPanel("Crime Type",  plotOutput("histPrimaryType", height = 400)), 
                   tabPanel("Location Description",  plotOutput("histLocDesc", height = 400))
                 ),
                 
                   radioButtons("arrest", "Arrest", c("Show", "Hide"), selected="Hide", inline = TRUE)
                 )
                 
                 #title = "Crime type",  status = "primary", 
                
                ),
                
            fluidRow(
              box(width = 12,
                div(style="height: 80px;",
                sliderInput("date",
                            "Dates:",
                            min = as.Date(date_from,"%Y/%m/%d"),
                            max = as.Date(date_till,"%Y/%m/%d"),
                            value = as.Date(date_from),
                            timeFormat="%Y-%m-%d",
                            animate = animationOptions(1000)
                          )
                    )
                  )
            )
            
    ),
    
    tabItem(tabName = "about",
            
            box(title = "About", width=10,
                
                tags$p(HTML(aboutHTML)),
                tags$img(src = "images/pic.jpg", style="width:100%" ))
    )
  ),
  
  tags$head(tags$script(src="assets/js/main.js")), #general .js
  includeCSS("./assets/styles/css.css"), #general labelling
  tags$script('
     google.maps.event.addListener(map, "bounds_changed", function() {
        Shiny.onInputChange("bounds_coords", map.getBounds());
      });
  '),
  
  tags$script('            
    Shiny.addCustomMessageHandler("myCallbackHandler",     
          function(json) {
            display_heatmap(json);
          }
    );
 ')
)

ui <- dashboardPage(header, sidebar, body, skin = "purple")

server <- function(input, output, session) {
  set.seed(122)
  
  observeEvent(input$crime_type, {
    generateHeatMapData(input)
  })

  generateHeatMapData <- function(input) {
    cat(str(input$bounds_coords))
    bounds_coords <- input$bounds_coords
    
    cat(input$crime_type_options)
    cat(input$date)

    cat('hist')
    if (input$crime_type == "All") {
      newdata <- subset(pastData, Date == input$date  , 
                        select = c(Latitude, Longitude))
    } else {
      newdata <- subset(pastData,Primary.Type == input$crime_type & Date == input$date  , 
                        select = c(Latitude, Longitude))
    }
      
    
    newdata[['int']] <- rep(1,nrow(newdata))
    names(newdata)[names(newdata)=="Latitude"] <- "lat"
    names(newdata)[names(newdata)=="Longitude"] <- "lng"
    newdata_json <- toJSON(unname(split(newdata, 1:nrow(newdata))))
    cat(str(newdata_json))
    session$sendCustomMessage(type='myCallbackHandler', newdata_json)
    
  }
  
  observeEvent(input$date, {
    generateHeatMapData(input)
  })
  
  observe( {
    bounds_coords <- input$bounds_coords
    newdata <- subset(pastData, Latitude >= bounds_coords[['south']] & Latitude <= bounds_coords[['north']] &
                        Longitude <= bounds_coords[['east']] & Longitude >= bounds_coords[['west']] & Date == input$date)
    output$histPrimaryType<-renderPlot ({
      
        temp <- newdata %>% dplyr::select(Primary.Type, Arrest) %>% dplyr::group_by(Primary.Type) %>% count
        if(nrow(temp) > 0 ) {
          importantTypes = temp[order(temp$n, decreasing = T),][1:6,]$Primary.Type
          
          mm <- newdata %>% dplyr::select(Primary.Type, Arrest) %>% dplyr::group_by(Primary.Type, Arrest) %>% count %>% dplyr::filter(Primary.Type %in% importantTypes)
          mm <- mm[order(mm$n, decreasing = T),][1:12,]
          mm <- mm[complete.cases(mm),]
        
          
          if(input$arrest == "Show") {
            g <- ggplot(mm, aes(x = reorder(Primary.Type, -n), y = n, fill =  Arrest)) +
              geom_bar(stat="identity") +
              ylab("Count") + 
              xlab("") + 
              theme(axis.text = element_text(size = 8),
                    axis.title = element_text(size = 13),
                    axis.text.x = element_text(angle = 45, hjust = 1))
          } else {
            g <- ggplot(mm, aes(x = reorder(Primary.Type, -n), y = n)) +
              geom_bar(stat="identity", fill = "#F8766D") +
              ylab("Count") + 
              xlab("") + 
              theme(axis.text = element_text(size = 8),
                    axis.title = element_text(size = 13),
                    axis.text.x = element_text(angle = 45, hjust = 1))
          }
          return (g)
        }
        })
    
    output$histLocDesc<-renderPlot ({
      
      
      temp <- newdata %>% dplyr::select(Location.Description, Arrest) %>% dplyr::group_by(Location.Description) %>% count
      if(nrow(temp) > 0 ) {
        importantTypes = temp[order(temp$n, decreasing = T),][1:6,]$Location.Description
        
        mm <- newdata %>% dplyr::select(Location.Description, Arrest) %>% dplyr::group_by(Location.Description, Arrest) %>% count %>% dplyr::filter(Location.Description %in% importantTypes)
        mm <- mm[order(mm$n, decreasing = T),][1:12,]
        mm <- mm[complete.cases(mm),]
       
        
        if(input$arrest == "Show") {
          g <- ggplot(mm, aes(x = reorder(Location.Description, -n), y = n, fill =  Arrest)) +
            geom_bar(stat="identity") +
            ylab("Count") + 
            xlab("") + 
            theme(axis.text = element_text(size = 8),
                  axis.title = element_text(size = 13),
                  axis.text.x = element_text(angle = 45, hjust = 1))
        } else {
          g <- ggplot(mm, aes(x = reorder(Location.Description, -n), y = n)) +
            geom_bar(stat="identity", fill = "#F8766D") +
            ylab("Count") + 
            xlab("Crime type") + 
            theme(axis.text = element_text(size = 8),
                  axis.title = element_text(size = 13),
                  axis.text.x = element_text(angle = 45, hjust = 1))
        }
        g
      }
    })
   
  })
}

shinyApp(ui, server)