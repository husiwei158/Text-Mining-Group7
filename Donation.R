library(tidyverse)
library(readxl)
library(leaflet)
library(magrittr)
library(maps)
#######################################################################################
setwd("C:\\Users\\Siwei Hu\\OneDrive\\document\\MA615\\maping\\class28")
MASS <- read.csv("11-5 MASSCONTRIBUTIONS-csv.csv")

MASS.amount <- MASS %>% group_by(city, state, party) %>% 
  summarize(total = sum(amount), num = n()) %>% mutate(average = total/num) 

st <- read.csv("states.csv")
colnames(st)[1] <- "state"
MASS.city <- left_join(st, MASS.amount)
MASS.st <- MASS.city %>% group_by(state,st_name,party) %>% summarise(total = sum(total),num = sum(num), average = total/num) 

map.state <- map_data("state") %>% rename(st_name = region) %>% group_by(st_name) %>% summarize(long = mean(c(max(long),min(long))), lat = mean(c(min(lat),max(lat)))) 

### include party information and longtitude&latitude. Use for further Party map
MASS.st %<>%   left_join(map.state,MASS.st, by="st_name")

Mass.Democrat <- filter(MASS.st, party =="D")
Mass.Republic <- filter(MASS.st, party == "R")

#########################################################################################

states <- geojsonio::geojson_read("gz_2010_us_040_00_500k.json", what = "sp")
colnames(Mass.Democrat)[2]  <- "NAME"
colnames(Mass.Republic)[2]  <- "NAME"
states@data <- mutate_each(states@data, funs(tolower))
### .x means democrat and .y means Republic
states@data <- left_join(states@data,Mass.Democrat, by = "NAME")
states@data <- left_join(states@data,Mass.Republic, by = "NAME")


bins.1 <- c(0, 5000, 20000, 30000 ,40000,50000,100000, 250000,500000, Inf)
pal.r.1 <- colorBin("Reds", domain = states@data$total.y, bins = bins.1)
pal.b.1 <- colorBin("Blues", domain = states@data$total.x, bins = bins.1)
bins.2 <- c(0, 500, 1000  ,2000 ,8000, 20000,100000, Inf)
pal.r.2 <- colorBin("Reds", domain = states@data$num.y, bins = bins.2)
pal.b.2 <- colorBin("Blues", domain = states@data$num.x, bins = bins.2)
bins.3 <- c(0,20,40,60,80,100,200, Inf)
pal.r.3 <- colorBin("Reds", domain = states@data$average.y, bins = bins.3)
pal.b.3 <- colorBin("Blues", domain = states@data$average.x, bins = bins.3)


labels.b.1 <- sprintf(
  "<strong>%s</strong><br/> donation amount:  %g",
  states$NAME, states$total.x
) %>% lapply(htmltools::HTML)

total.x <- as.numeric(states$total.x)

dt <- leaflet(states) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
  addPolygons(
    fillColor = ~pal.b.1(total.x),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels.b.1,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal.b.1, values = ~total.x, opacity = 0.7, title = NULL,
            position = "bottomright")
#######################################################################################


labels.r.1 <- sprintf(
  "<strong>%s</strong><br/> donation amount:  %g<br/> donors: %g <br/> Average: %g",
  states$NAME, states$total.y,states$num.y,states$average.y
) %>% lapply(htmltools::HTML)

total.y <- as.numeric(states$total.y)

rt <- leaflet(states) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
  addPolygons(
    fillColor = ~pal.r.1(total.y),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#44444",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels.r.1,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal.r.1, values = ~total.y, opacity = 0.7, title = NULL,
            position = "bottomright")
#######################################################################################





labels.r.2 <- sprintf(
  "<strong>%s</strong><br/> donors: %g ",
  states$NAME, states$num.y
) %>% lapply(htmltools::HTML)

num.y <- as.numeric(states$num.y)

rtd <- leaflet(states) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
  addPolygons(
    fillColor = ~pal.r.2(num.y),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#44444",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels.r.2,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal.r.2, values = ~num.y, opacity = 0.7, title = NULL,
            position = "bottomright")
#########################################################################################

labels.b.2 <- sprintf(
  "<strong>%s</strong><br/> donors: %g ",
  states$NAME, states$num.x
) %>% lapply(htmltools::HTML)

num.x <- as.numeric(states$num.x)

dtd <- leaflet(states) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
  addPolygons(
    fillColor = ~pal.b.2(num.x),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels.b.2,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal.b.2, values = ~num.x, opacity = 0.7, title = NULL,
            position = "bottomright")
#########################################################################################

labels.b.3 <- sprintf(
  "<strong>%s</strong><br/> Average: %g",
  states$NAME, states$average.x
) %>% lapply(htmltools::HTML)

average.x <- as.numeric(states$average.x)

da <- leaflet(states) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
  addPolygons(
    fillColor = ~pal.b.3(average.x),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels.b.3,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal.b.3, values = ~average.x, opacity = 0.7, title = NULL,
            position = "bottomright")
########################################################################################


labels.r.3 <- sprintf(
  "<strong>%s</strong><br/> Average: %g",
  states$NAME, states$average.y
) %>% lapply(htmltools::HTML)

average.y <- as.numeric(states$average.y)

ra <- leaflet(states) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
  addPolygons(
    fillColor = ~pal.r.3(average.y),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#44444",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels.r.3,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal.r.3, values = ~average.y, opacity = 0.7, title = NULL,
            position = "bottomright")

#######################################################################################


ui <- fluidPage(
   
   titlePanel("Donation contribution to MA"),
   
     sidebarPanel(
        radioButtons("Party", label = h3("Please choose the Party"),
                     choices = c("Republic","Democrat"), 
                     selected = "Republic") ,
     conditionalPanel(
       condition = "input.Party == 'Republic'",
        selectInput("Republic", "Data about Republic",
                                       choices = c("Republic Donation","Republic Donsors","Republic Average"),selected = "Republic Donation")),
     conditionalPanel(
       condition = "input.Party == 'Democrat'",
       selectInput("Democrat","Data about Democrat", choices = c("Democrat Donation","Democrat Donsors","Democrat Average"),selected = "Democrat Donation")
     )),
      mainPanel(
         leafletOutput("Partyplot")
      )
   
)


server <- function(input, output) {
 
  RepublicInput <- reactive(if(input$Republic == "Rebulic Donation") {rt}
                            else if (input$Republic == "Repulic Donsors"){print(rtd)}
                            else {print(ra)})
  DemocratInput <- reactive(if(input$Democrat == "Democrat Donation") {print(dt)} 
                            else if (input$Democrat == "Democrat Donsors"){print(dtd)}  
                            else {print(da)}
    
  )
  
  output$Partyplot <- renderLeaflet({ 
    ifelse(input$Party == "Republic",RepublicInput(),DemocratInput())
  })
  }
    
# Run the application 
shinyApp(ui = ui, server = server)

