## app.R ##
library(shinydashboard)
library(rjson)
library()

predictions <- read.csv("Crimes_-_2001_to_present.csv", nrows = 50000)  # read csv file 

predictions$Date <- gsub( " .*$", "", predictions$Date )
predictions$Date <- as.Date(predictions$Date, format = "%m/%d/%Y")


'%<%' <- function(date1, date2) {
  as.numeric(as.POSIXct(date1))  < as.numeric(as.POSIXct(as.Date(date2)))
}

today <- as.Date("2016/02/01")
date_till <-"2016/02/15"
date_from <-"2015/12/15"
predictions <- subset(mydata,( today  %<% Date)  & (Date  %<% date_till), select = c(Latitude, Longitude, Date))
predictions[['pred']] <- rep(T, nrow(predictions))


mydata <- read.csv("Crimes_-_2001_to_present.csv", nrows=50000)  # read csv file
mydata$Date <- gsub( " .*$", "", mydata$Date )
mydata$Date <- as.Date(mydata$Date, format = "%m/%d/%Y")

mydata <- subset(mydata, Date %<%  today &  date_from %<%  Date)
mydata <- mydata[complete.cases(mydata),]

min_freq_type <- 15
crime_type_options <- c("All", as.character(unique(mydata$Primary.Type)))


header <- dashboardHeader(title = "Crime Storm")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("About", icon = icon("th"), tabName = "about"),
    menuItem("Source code", icon = icon("file-code-o"), 
             href = "https://github.com/rstudio/shinydashboard/")
  ),
  sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                    label = "Search...")
)


jsCode <- '
shinyjs.shinyjs_display_heatmap = function(params) {
  display_heatmap(params[0]);
}'


body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            fluidRow(
               box(includeHTML('www/test.html'),
                   selectInput("crime_type", "Type", crime_type_options, selected="")
                 
                    ),
            
               box(#title = "Crime type",  status = "primary",
                 
                 tabsetPanel(
                   tabPanel("Crime Type",  plotOutput("histPrimaryType", height = 440)), 
                   tabPanel("Location Description",  plotOutput("histLocDesc", height = 440))
                 ),
                 
                   radioButtons("arrest", "Arrest", c("Show", "Hide"), selected="Hide")
                 )
                 
                 #title = "Crime type",  status = "primary", 
                
                ),
                
            fluidRow(
              box(width = 12,
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
            
    ),
    
    tabItem(tabName = "about",
            h2("Widgets tab content")
    )
  ),
  tags$head(tags$script(src="js.js")), #general .js
  includeCSS("./www/css.css"), #general labelling
  useShinyjs(),
  extendShinyjs(text = jsCode),
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
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  observeEvent(input$crime_type, {
    generateHeatMapData(input)
  })

  generateHeatMapData <- function(input) {
    cat(str(input$bounds_coords))
    bounds_coords <- input$bounds_coords
    
    cat(input$crime_type_options)
    cat(input$date)
    if (today %<% input$date) {
      cat('pred')
      newdata <- subset(predictions, Date == input$date, select = c(Latitude, Longitude)) #TODO: Add intensity
    } else {
      cat('hist')
      if (input$crime_type == "All") {
        newdata <- subset(mydata, Date == input$date  , 
                          select = c(Latitude, Longitude))
      } else {
        newdata <- subset(mydata,Primary.Type == input$crime_type & Date == input$date  , 
                          select = c(Latitude, Longitude))
      }
      
    }
    
    newdata[['int']] <- rep(1,nrow(newdata))
    names(newdata)[names(newdata)=="Latitude"] <- "lat"
    names(newdata)[names(newdata)=="Longitude"] <- "lng"
    newdata_json <- toJSON(unname(split(newdata, 1:nrow(newdata))))
    
    session$sendCustomMessage(type='myCallbackHandler', newdata_json)
    
  }
  
  observeEvent(input$date, {
    generateHeatMapData(input)
  })
  
  observe( {
    #str(input$bounds_coords[['south']]*2)
    bounds_coords <- input$bounds_coords
    newdata <- subset(mydata, Latitude >= bounds_coords[['south']] & Latitude <= bounds_coords[['north']] &
                        Longitude <= bounds_coords[['east']] & Longitude >= bounds_coords[['west']] & Date == input$date)
    
    output$histPrimaryType<-renderPlot ({
      
        temp <- newdata %>% dplyr::select(Primary.Type, Arrest) %>% dplyr::group_by(Primary.Type) %>% count
        importantTypes = temp[order(temp$freq, decreasing = T),][0:6,]$Primary.Type
        
        mm <- newdata %>% dplyr::select(Primary.Type, Arrest) %>% dplyr::group_by(Primary.Type, Arrest) %>% count %>% dplyr::filter(Primary.Type %in% importantTypes)
        mm <- mm[order(mm$freq, decreasing = T),][0:12,]
        mm <- mm[complete.cases(mm),]
      
        
        if(input$arrest == "Show") {
          g <- ggplot(mm, aes(x = reorder(Primary.Type, -freq), y = freq, fill =  Arrest)) +
            geom_bar(stat="identity") +
            ylab("Count") + 
            xlab("Crime type") + 
            theme(axis.text = element_text(size = 8),
                  axis.title = element_text(size = 13),
                  axis.text.x = element_text(angle = 45, hjust = 1))
        } else {
          g <- ggplot(mm, aes(x = reorder(Primary.Type, -freq), y = freq)) +
            geom_bar(stat="identity", fill = "#F8766D") +
            ylab("Count") + 
            xlab("Crime type") + 
            theme(axis.text = element_text(size = 8),
                  axis.title = element_text(size = 13),
                  axis.text.x = element_text(angle = 45, hjust = 1))
        }
        return (g)
        })
    
    output$histLocDesc<-renderPlot ({
      
      
      temp <- newdata %>% dplyr::select(Location.Description, Arrest) %>% dplyr::group_by(Location.Description) %>% count
      importantTypes = temp[order(temp$freq, decreasing = T),][0:6,]$Location.Description
      
      mm <- newdata %>% dplyr::select(Location.Description, Arrest) %>% dplyr::group_by(Location.Description, Arrest) %>% count %>% dplyr::filter(Location.Description %in% importantTypes)
      mm <- mm[order(mm$freq, decreasing = T),][0:12,]
      mm <- mm[complete.cases(mm),]
     
      
      if(input$arrest == "Show") {
        g <- ggplot(mm, aes(x = reorder(Location.Description, -freq), y = freq, fill =  Arrest)) +
          geom_bar(stat="identity") +
          ylab("Count") + 
          xlab("Crime type") + 
          theme(axis.text = element_text(size = 8),
                axis.title = element_text(size = 13),
                axis.text.x = element_text(angle = 45, hjust = 1))
      } else {
        g <- ggplot(mm, aes(x = reorder(Location.Description, -freq), y = freq)) +
          geom_bar(stat="identity", fill = "#F8766D") +
          ylab("Count") + 
          xlab("Crime type") + 
          theme(axis.text = element_text(size = 8),
                axis.title = element_text(size = 13),
                axis.text.x = element_text(angle = 45, hjust = 1))
      }
      
      
      
     
      return (g)
    })
    
    
    #output$text1 <- renderText({str(input$bounds_coords, recursive=FALSE)})
  })
  #
}

shinyApp(ui, server)