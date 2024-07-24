library(shiny)
library(bs4Dash)
library(dplyr)
library(readr)
library(plotly)
library(leaflet)
library(DT)
library(fresh)
library(RPostgres)
library(DBI)

plot_colour <- "#8965CD"

theme <- create_theme(
  bs4dash_color(
    lime = "#52A1A5",
    olive = "#4A9094",
    purple = "#8965CD"
  ),
  bs4dash_status(
    primary = "#E1EDED",
    info = "#E4E4E4"
  )
)

# Database connection settings
dbDetails <- list(
  dbname = "advertising",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "k3nbr1ght"
)

# Function to get a database connection
getConnection <- function() {
  con <- dbConnect(RPostgres::Postgres(),
                   dbname = dbDetails$dbname,
                   host = dbDetails$host,
                   port = dbDetails$port,
                   user = dbDetails$user,
                   password = dbDetails$password)
  return(con)
}

# Fetch data from database
fetchData <- function(query) {
  con <- getConnection()
  data <- dbGetQuery(con, query)
  dbDisconnect(con)
  return(data)
}

# User Interface ----------------------------------------------------------
ui <- dashboardPage(
  title = "Advertising Report",
  
  freshTheme = theme,
  dark = NULL,
  help = NULL,
  fullscreen = TRUE,
  scrollToTop = TRUE,
  
  # Header ----
  header = dashboardHeader(
    status = "lime",
    title = dashboardBrand(
      title = "Advertising Report",
      color = "olive",
      image = "https://images.unsplash.com/photo-1539664030485-a936c7d29c6e?q=80&w=1160&auto=format&fit=crop&ixlib=rb-4.0.3&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D"
    ),
    controlbarIcon = icon("circle-info"),
    fixed = TRUE
  ),
  
  # Sidebar ----
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "sidebarMenuid",
      menuItem(
        "Home",
        tabName = "home",
        icon = icon("home")
      ),
      menuItem(
        "LinkedIn",
        tabName = "linkedin",
        icon = icon("linkedin")
      ),
      menuItem(
        "Facebook",
        tabName = "facebook",
        icon = icon("facebook")
      ),
      menuItem(
        "Instagram",
        tabName = "instagram",
        icon = icon("instagram")
      ),
      menuItem(
        "X (Twitter)",
        tabName = "x",
        icon = icon("twitter")
      )
    )
  ),
  
  # Control bar ----
  controlbar = dashboardControlbar(),
  
  # Footer ----
  footer = dashboardFooter(
    left = "Robin Ochieng",
    right = "2024"
  ),
  
  # Body ----
  body = dashboardBody(
    tabItems(
      
      # Home tab ----
      tabItem(
        tabName = "home",
        
        jumbotron(
          title = "Welcome!",
          status = "info",
          lead = "Advertising Report",
          href = "",
          btnName = "Download",
          "Emerging Markets"
        )
      ),
      
      # LinkedIn tab ----
      tabItem(
        tabName = "linkedin",
        h2("LinkedIn Report"),
        dataTableOutput("linkedinTable")
      ),
      
      # Facebook tab ----
      tabItem(
        tabName = "facebook",
        h2("Facebook Report"),
        dataTableOutput("facebookTable")
      ),
      
      # Instagram tab ----
      tabItem(
        tabName = "instagram",
        h2("Instagram Report"),
        dataTableOutput("instagramTable")
      ),
      
      # X (Twitter) tab ----
      tabItem(
        tabName = "x",
        h2("X (Twitter) Report"),
        dataTableOutput("xTable")
      )
      
    )
  )
  
)

# Server ------------------------------------------------------------------

server <- function(input, output) {
  # Fetch data for LinkedIn
  linkedinData <- reactive({
    fetchData("SELECT * FROM linkedin;")
  })
  
  # Fetch data for Facebook
  facebookData <- reactive({
    fetchData("SELECT * FROM facebook;")
  })
  
  # Fetch data for Instagram
  instagramData <- reactive({
    fetchData("SELECT * FROM instagram;")
  })
  
  # Fetch data for X (Twitter)
  xData <- reactive({
    fetchData("SELECT * FROM x;")
  })
  
  # Populate DataTables
  output$linkedinTable <- renderDataTable({
    linkedinData()
  })
  
  output$facebookTable <- renderDataTable({
    facebookData()
  })
  
  output$instagramTable <- renderDataTable({
    instagramData()
  })
  
  output$xTable <- renderDataTable({
    xData()
  })
  
}

shinyApp(ui, server)
