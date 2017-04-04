## app.R ##
library(shinydashboard)
library(rjson)
library(dplyr)
library(ggplot2)

data <-  read.csv("2016_pred.csv")
predictions <- data  # read csv file 


predictions$Date <- gsub( " .*$", "", predictions$Date )
predictions$Date <- as.Date(predictions$Date, format = "%m/%d/%Y")


'%<%' <- function(date1, date2) {
  as.numeric(as.POSIXct(date1))  <= as.numeric(as.POSIXct(as.Date(date2)))
}

date_from <-"2016/03/04"
today <- as.Date("2016/03/19")
date_till <-"2016/03/26"

predictions <- subset(predictions,( today  %<% Date)  & (Date  %<% date_till), select = c(Latitude, Longitude, Date))
predictions[['pred']] <- rep(T, nrow(predictions))

#write.table(predictions, file = "2016_pred.csv", sep = ",", quote=FALSE)

mydata <- read.csv("2016_hist.csv", sep = "\t")
#browser()
#mydata$Date <- gsub( " .*$", "", mydata$Date )
mydata$Date <- as.Date(mydata$Date)

mydata <- subset(mydata, Date %<%  date_till &  date_from %<%  Date)
mydata <- mydata[complete.cases(mydata),]
#write.table(mydata, file = "2016_hist.csv", sep = "\t", quote=FALSE, row.names = F)

min_freq_type <- 15
crime_type_options <- c("All", as.character(unique(mydata$Primary.Type)))


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



body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            fluidRow(
               box(includeHTML('www/test.html'),
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
                
                tags$p(HTML("
                        Crime storm was created 18/3/2017 within a 24h datathon organised by <a href=\"http://www.xomnia.com\">Xomnia</a>.
                      The showcase presented in the dashboard shows is set to show a period from year of 2016, because the data for 2017 were unavailable. 
                        The application was created by a team of six PDEng trainees of <a href=\"http://www.jads.nl/\"> Jheronimus Academy of Data Science</a>: 
                      <ul>
                       <li>Andriy Drebot</li>
                       <li>Mahmoud Khodier (<a href=\"mailto:m.khodier@tue.nl\">contact</a>)</li>
                       <li>You Yue Huang</li>
                       <li>Diego Perez</li>
                       <li>Adam Zika</li>
                       
                       
                       <li>Manos Stergiadis</li>
                      </ul>")),
                tags$img(src = "pic.jpg", style="width:100%" ))
    )
  ),
  tags$head(tags$script(src="js.js")), #general .js
  includeCSS("./www/css.css"), #general labelling
 # useShinyjs(),
 # extendShinyjs(text = jsCode),
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
   # browser()
    if (input$crime_type == "All") {
      newdata <- subset(mydata, Date == input$date  , 
                        select = c(Latitude, Longitude))
    } else {
      newdata <- subset(mydata,Primary.Type == input$crime_type & Date == input$date  , 
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
    #invalidateLater(500, session)
    #str(input$bounds_coords[['south']]*2)
    bounds_coords <- input$bounds_coords
    newdata <- subset(mydata, Latitude >= bounds_coords[['south']] & Latitude <= bounds_coords[['north']] &
                        Longitude <= bounds_coords[['east']] & Longitude >= bounds_coords[['west']] & Date == input$date)
    #browser()
    output$histPrimaryType<-renderPlot ({
      
        temp <- newdata %>% dplyr::select(Primary.Type, Arrest) %>% dplyr::group_by(Primary.Type) %>% count
       # browser()
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
      #browser()
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
        
        return (g)
      }
    })
    
    
    #output$text1 <- renderText({str(input$bounds_coords, recursive=FALSE)})
  })
  #
}

shinyApp(ui, server)