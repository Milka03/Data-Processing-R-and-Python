
library("shiny")
library("dplyr")
library("unglue")
library("ggplot2")

cancellations <- read.csv("cancellations.csv")
airports <- read.csv("airports.csv")
carriers <- read.csv("carriers.csv")

createAirportsList <- function(isOrigin) {
  cancellations %>%
    group_by(Origin) %>%
    summarize(Flights = n()) %>%
    arrange(desc(Flights)) %>%
    head(20) -> top20Origins
  
  cancellations %>%
    group_by(Dest) %>%
    summarize(Flights = n()) %>%
    arrange(desc(Flights)) %>%
    head(20) -> top20Dests
  
  filterList <- top20Origins$Origin
  if(isOrigin == 'dest') {
    filterList <- top20Dests$Dest
  }
  
  airports %>%
    filter(iata %in% filterList) %>%
    mutate(DisplayName = paste("(", iata, ") ", airport, ", ", city, sep="")) %>%
    pull(8)
}

createCarriersList <- function() {
  cancellations %>%
    group_by(UniqueCarrier) %>%
    summarize(CarrierNum = n()) %>%
    arrange(desc(CarrierNum)) %>%
    head(20) -> top20Carriers
    
  carriers %>%
    filter(Code %in% top20Carriers$UniqueCarrier) %>%
    mutate(DisplayName = paste("(", Code, ") ", Description, sep="")) %>%
    pull(3)
}

getAirportCode <- function(airportName) {
  if(airportName == "All") { return("All") }
  else {
    airports %>%
      filter(iata == unglue::unglue_vec(airportName, "{}({x}){}")) %>%
      pull(1)
  }
}

getCarrierCode <- function(carrierName) {
  if(carrierName == "All") { return("All") }
  else {
    carriers %>%
      filter(Code == unglue::unglue_vec(carrierName, "{}({x}){}")) %>%
      pull(1)
  }
}


avgCancellationsByYear <- function(origin, destination, cancelCode, carrierCode) {
  cancellations %>%
    filter(if(origin != "All") Origin == origin else TRUE & 
          if(destination != "All") Dest == destination else TRUE & 
          if(cancelCode != "All") CancellationCode == cancelCode else TRUE & 
          if(carrierCode != "All") UniqueCarrier == carrierCode else TRUE) %>%
    group_by(Year) %>%
    summarise(Count = n()) %>%
    ungroup() -> perYear
  as.integer(mean(perYear$Count))
}

totalCancellationsByYear <- function(origin, destination, cancelCode, carrierCode) {
  cancellations %>%
    filter(if(origin != "All") Origin == origin else TRUE) %>% 
    filter(if(destination != "All") Dest == destination else TRUE) %>%
    filter(if(cancelCode != "All") CancellationCode == cancelCode else TRUE) %>% 
    filter(if(carrierCode != "All") UniqueCarrier == carrierCode else TRUE) %>%
    group_by(Year) %>%
    summarise(Total = n()) %>%
    ungroup()
}

cancellationsByYearReason <- function(origin, destination, cancelCode, carrierCode) {
  cancellations %>%
    filter(if(origin != "All") Origin == origin else TRUE) %>% 
    filter(if(destination != "All") Dest == destination else TRUE) %>%
    filter(if(cancelCode != "All") CancellationCode == cancelCode else TRUE) %>% 
    filter(if(carrierCode != "All") UniqueCarrier == carrierCode else TRUE) %>%
    group_by(Year, CancellationCode) %>%
    summarise(Total = n()) %>%
    mutate(CancellationCode = if_else(CancellationCode == "A", "Carrier",
                              if_else(CancellationCode == "B", "Weather",
                              if_else(CancellationCode == "C", "NAS",
                              if_else(CancellationCode == "D", "Security", NULL))))) %>%
    ungroup()
}




ui <- fluidPage(
  titlePanel('Flight cancellations in years 2004-2008'),
  
  sidebarLayout(
    sidebarPanel(
      selectInput('originAirport', 'Select the origin airport',
                  choices = c("All", createAirportsList('origin')),
                  selected = "All"),
      selectInput('destAirport', 'Select the destination airport',
                  choices = c("All", createAirportsList('dest')),
                  selected = "All"),
      selectInput('carrierName', 'Select the carrier name',
                  choices = c("All", createCarriersList()),
                  selected = "All"),
      radioButtons('cancelName', 'Select cancellation reason',
                  selected = "All",
                  choiceNames = c("All", "Carrier", "Weather", "NAS", "Security"),
                  choiceValues = c("All", "A", "B", "C", "D"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Total flight cancellations", plotOutput("TotalCancBarPlot"), textOutput("avgCancellations")),
        tabPanel("Flight cancellations by reason", plotOutput("LinePlot1"))
      )
    )
  )
)

server <- function(input, output) {
  originAirport <- reactive(input$originAirport)
  destinationAirport <- reactive(input$destAirport)
  carrName <- reactive(input$carrierName)
  cancelCode <- reactive(input$cancelName)
  
  originCode <- reactive(getAirportCode(originAirport()))
  destinationCode <- reactive(getAirportCode(destinationAirport()))
  carrierCode <- reactive(getCarrierCode(carrName()))
  
  avgCanc <- reactive({ avgCancellationsByYear(originCode(), destinationCode(), cancelCode(), carrierCode()) })
  
  appData <- reactive({
    totalCancellationsByYear(originCode(), destinationCode(), cancelCode(), carrierCode())
  })
  linePlotData <- reactive({
    cancellationsByYearReason(originCode(), destinationCode(), cancelCode(), carrierCode())
  })
  
  
  output$TotalCancBarPlot <- renderPlot({
    validate(
      need(appData(), 'There are no cancellations for selected requirements')
    )
    g <-ggplot(appData(),aes(x=Year, y=Total)) +
      stat_summary(geom="bar")
    g
  })
  
  output$avgCancellations <- renderText({
    paste("Average number of cancellations per year: ", avgCanc())
  })
  
  output$LinePlot1 <- renderPlot({
    validate(
      need(linePlotData(), 'There are no cancellations for selected requirements')
    )
    g1 <-ggplot(linePlotData(),aes(x=Year, y=Total, color=CancellationCode)) +
      stat_summary(geom="line",size=1)
    g1
  })
}

shinyApp(ui = ui, server = server)
