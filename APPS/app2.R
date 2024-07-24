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
          title = tagList(
            icon("chart-line", style = "color: #555; margin-right: 10px;"), 
            HTML("<span style='color: #555; font-size: 28px;'>Welcome to </span>"), 
            HTML("<span style='color: #007BFF; font-size: 32px; font-weight: bold;'>Kenbright's</span>"), 
            HTML("<span style='color: #555; font-size: 28px;'> Advertising Insights</span>")
          ),
          status = "info",
          lead = HTML("<span style='color: #555; font-size: 22px;'>Dive Deep into Emerging Markets</span>"),
          p(style = "color: #555; font-size: 18px;", "Unlock potential with data-driven strategies."),
          style = "background: linear-gradient(135deg, #EFEFEF 0%, #D6D6D6 100%); color: #555; padding: 20px;",
          div(
            style = "margin-top: 30px; display: flex; justify-content: center; gap: 20px;",
            a(href = "https://www.facebook.com", style = "text-decoration: none; color: #555;", icon("facebook", style = "color: #3b5998;")),
            a(href = "https://www.linkedin.com", style = "text-decoration: none; color: #555;", icon("linkedin", style = "color: #0077b5;")),
            a(href = "https://www.instagram.com", style = "text-decoration: none; color: #555;", icon("instagram", style = "color: #e4405f;")),
            a(href = "https://twitter.com", style = "text-decoration: none; color: #555;", icon("twitter", style = "color: #1da1f2;"))
          ),
          "Leverage actionable insights for strategic advantage."
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
  
  # DataTables options
  dataTableOptions <- list(
    dom = 't', # This option is to show only the table without the default DataTables controls
    paging = FALSE, # Disable pagination
    ordering = TRUE, # Enable column ordering
    autoWidth = TRUE, # Enable automatic column width calculation
    info = FALSE, # Disable showing table information
    searching = FALSE, # Disable search box
    columnDefs = list(
      list(className = 'dt-center', targets = '_all') # Center text in all columns
    ),
    initComplete = JS(
      "function(settings, json) {", 
      "$(this.api().table().header()).css({'background-color': '#4A9094', 'color': 'white'});", 
      "}"
    )
  )
  
  # Render DataTables within bs4Dash boxes
  output$linkedinTable <- renderDT({
    datatable(linkedinData(), options = dataTableOptions, class = "cell-border stripe")
  }, server = FALSE)
  
  output$facebookTable <- renderDT({
    datatable(facebookData(), options = dataTableOptions, class = "cell-border stripe")
  }, server = FALSE)
  
  output$instagramTable <- renderDT({
    datatable(instagramData(), options = dataTableOptions, class = "cell-border stripe")
  }, server = FALSE)
  
  output$xTable <- renderDT({
    datatable(xData(), options = dataTableOptions, class = "cell-border stripe")
  }, server = FALSE)
  

  
}

shinyApp(ui, server)