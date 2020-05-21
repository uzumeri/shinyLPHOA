library(shiny)
library(dplyr)
library(ggplot2)

# Define UI for random distribution app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Tabsets"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider demand rate ----
      sliderInput("dr",
                  "Demand Rate (pctg units per year):",
                  value = 10,
                  min = 1,
                  max = 30),
      
      # Input: Slider for fixed hiatus ----
      sliderInput("fh",
                  "Fixed Hiatus (months):",
                  value = 24,
                  min = 6,
                  max = 48)

    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
    
    # Output: Tabset w/ plot, summary, and table ----
    tabsetPanel(type = "tabs",
                tabPanel("Active Permits", plotOutput("activeperms")),
                tabPanel("Blocked Demand", plotOutput("closed")),
                tabPanel("Open Demand", plotOutput("open")),
                tabPanel("New Permits", plotOutput("new"))
                # tabPanel("Monte Carlo (100)", plotOutput("montecarlo"))
    )
  )
)
)


# Define server logic for random distribution app ----
server <- function(input, output) {
  
  validIDs <- data.frame(ID=as.numeric(1:224))
  Permits <- data.frame(mon = 1) %>% mutate(ID = 0, start=0, end = 0, length=0)
  Actives <- data.frame(mon = 1) %>% mutate(active = 0, new=0, valid=0, closed=0)
  MC <- data.frame(run = 1) %>% mutate(run=0, active = 0, new=0, valid=0, closed=0)
  # HiatusMultiplier <- 3
  # HiatusVariable <- FALSE
  UnitsSoldPerYear <- 40
  

  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  active <- reactive({
    AvgNoDemanded <- input$dr*224/1200
    
    for (month in 1:120) {
      
      # sample a number of IDs that want permits this month
      NumIDs <- rpois(1,AvgNoDemanded)
      
      # Establish the cutoff month to ignore earlier permits after hiatus
      if (month > input$fh) {
        cutoff <- month - input$fh
      }
      else {
        cutoff = 0
      }
      
      # If there are any permit requests this month, process them
      if (NumIDs > 0) {
        closed <- Permits %>% group_by(ID) %>% summarize(last = max(end)) %>% filter(last >= cutoff) %>% select(ID)
        valids <- setdiff(Units,closed) 
        SampIDs <- sample(valids$ID,NumIDs)
        n <- length(SampIDs)
        for (i in 1:n) {
          len <- sample(6:12,1)
          rID <- SampIDs[i]
          Permits <- Permits %>% add_row(mon = month, start = month, ID = rID, length = len, end = month + len)
        }
      }
      AvgNoDemanded <- input$dr*nrow(valids)/1200
      count <- Permits %>% filter(month >= start & month <= end) %>% tally(name="active")
      Actives <- Actives %>% add_row(mon=month, new=NumIDs, active=as.integer(count), valid=nrow(valids),closed=nrow(closed))
    }
    Actives
    
  })
  
  
  montecarlo <- reactive({

    AvgNoDemanded <- input$dr*224/1200
    
    for (r in 1:100){
    
    for (month in 1:120) {
      
      # sample a number of IDs that want permits this month
      NumIDs <- rpois(1,AvgNoDemanded)
      
      # Establish the cutoff month to ignore earlier permits after hiatus
      if (month > input$fh) {
        cutoff <- month - input$fh
      }
      else {
        cutoff = 0
      }
      
      # If there are any permit requests this month, process them
      if (NumIDs > 0) {
        closed <- Permits %>% group_by(ID) %>% summarize(last = max(end)) %>% filter(last >= cutoff) %>% select(ID)
        valids <- setdiff(Units,closed) 
        SampIDs <- sample(valids$ID,NumIDs)
        n <- length(SampIDs)
        for (i in 1:n) {
          len <- sample(6:12,1)
          rID <- SampIDs[i]
          Permits <- Permits %>% add_row(mon = month, start = month, ID = rID, length = len, end = month + len)
        }
      }
      AvgNoDemanded <- input$dr*nrow(valids)/1200
      count <- Permits %>% filter(month >= start & month <= end) %>% tally(name="active")
      Actives <- Actives %>% add_row(mon=month, new=NumIDs, active=as.integer(count), valid=nrow(valids),closed=nrow(closed))
    }
    MC <- bind_rows(MC,Actives)
    MC
  }
  
  })  
  # Generate a plot of the data ----
 output$activeperms <- renderPlot({
    output <- active()
    # str(output)
    ggplot(output, aes(x=mon, y=active)) + geom_point(size=3) +
      labs(x="Month", y="Outstanding Permits" ) +
      ylim(0,50)
  })

 # Generate a plot of the data ----
 output$new <- renderPlot({
   output <- active()
   ggplot(output, aes(x=mon, y=new)) + geom_point(size=3) +
     labs(x="Month", y="New Permits" ) +
     ylim(0,10)
 })

  # Generate a plot of the data ----
 output$open <- renderPlot({
   output <- active()
   ggplot(output, aes(x=mon, y=valid)) + geom_point(size=3) +
     labs(x="Month", y="Open Units" )
 })
 
 # Generate a plot of the data ----
 output$closed <- renderPlot({
   output <- active()
   ggplot(output, aes(x=mon, y=closed)) + geom_point(size=3) +
     labs(x="Month", y="Blocked Units" )
 })
  # # Generate a plot of the data ----
  # output$montecarlo <- renderPlot({
  #   output <- montecarlo()
  #   ggplot(output, aes(x=mon, y=active)) + geom_point(size=3) +
  #     labs(x="Month", y="Active Permits" )
  # })
  
  
}

# Create Shiny app ----
shinyApp(ui, server)
