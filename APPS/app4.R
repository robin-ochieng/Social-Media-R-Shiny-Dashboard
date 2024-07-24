library(shiny)
library(bs4Dash)
library(dplyr)
library(readxl)
library(plotly) 
library(leaflet)
library(scales)
library(DT)
library(fresh)
library(shinycssloaders)
library(tidyverse)

plot_colour <- "#8965CD"

theme <- create_theme(
  bs4dash_color(
    lime = "#52A1A5",
    olive = "#4A9094",
    purple = "#8965CD",
    light = "#f8f9fa"
  ),
  bs4dash_status(
    primary = "#4A9094",
    secondary = "#6c757d",
    success = "#198754",
    info = "#0dcaf0",
    warning = "#ffc107",
    danger = "#dc3545",
    light = "#f8f9fa",
    dark = "#343a40"
  ),
  bs4dash_vars(
    navbar_light_color = "#ffffff",
    navbar_light_active_color = "#52A1A5",
    navbar_light_hover_color = "#6c757d"
  )
)


# User Interface ----------------------------------------------------------
ui <- dashboardPage(
  title = "Advertising Report",
  
  freshTheme = theme,
  dark = NULL,
  help = NULL,
  fullscreen = FALSE,
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
  controlbar = bs4DashControlbar(
    skin = "light",
    title = "Filter Settings",  # Aesthetic and well-labeled title for the control bar
    id = "dashboardControlbar",
    width = 300,
    bs4Card(
      title = "LinkedIn Filters",
      background = "info",
      width = 12,
      style = "max-width: 100%;",
      selectInput(
        inputId = "linkedinmonth",
        label = "Select Month",
        choices = NULL
      ),
      selectInput(
        inputId = "linkedinyear",
        label = "Select Year",
        choices = NULL
      )
    ),
    bs4Card(
      title = "Facebook Filters",
      background = "primary",
      width = 12,
      style = "max-width: 100%;",
      selectInput(
        inputId = "",
        label = "Select Month",
        choices = NULL
      ),
      selectInput(
        inputId = "",
        label = "Select Year",
        choices = NULL
      )
    ),
    bs4Card(
      title = "Instagram Filters",
      width = 12,
      background = "secondary",
      style = "max-width: 100%;",
      selectInput(
        inputId = "",
        label = "Select Month",
        choices = NULL
      ),
      selectInput(
        inputId = "",
        label = "Select Year",
        choices = NULL
      )
    ),
    bs4Card(
      title = "X Filters",
      width = 12,
      background = "success",
      style = "max-width: 100%;",
      selectInput(
        inputId = "",
        label = "Select Month",
        choices = NULL
      ),
      selectInput(
        inputId = "",
        label = "Select Year",
        choices = NULL
      )
    )
  ),
  
  # Footer ----
  footer = bs4DashFooter(
    div(style = "background-color: #4A9094; color: white; text-align: center; padding: 8px;", 
        "© 2024 Advertising Report | Powered by Tech and Research Department | Kenbright")
  ),
  
  # Body ----
  body = dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Mulish:wght@400;700&display=swap"),
      tags$style(HTML("
        body, .content-wrapper, .main-header .navbar, .main-sidebar {
          font-family: 'Mulish', sans-serif;
        }
      "))
    ),
    tabItems(
      # LinkedIn tab ----
      tabItem(
        tabName = "linkedin",
        fluidRow(
          valueBoxOutput("linkedinTotalReactions"),
          valueBoxOutput("linkedinTotalComments"),
          valueBoxOutput("linkedinNewFollowers")
        ),
        fluidRow(
          box(
            solidHeader = TRUE,
            status = "olive",
            title = "LinkedIn KPI Table",
            DTOutput("kpiTablelinkedin") %>% withSpinner(),
            width = 12
          )
        ),
        fluidRow(
          box(title = "LinkedIn Report",
              width = 12,
              status = "olive",
              solidHeader = TRUE, 
              dataTableOutput("linkedinTable") %>% withSpinner())
        )
      ),
      
      # Facebook tab ----
      tabItem(
        tabName = "facebook",
        fluidRow(
          valueBoxOutput("facebookTotalPostReach"),
          valueBoxOutput("facebookTotalInteractions"),
          valueBoxOutput("facebookTotalImpressions")
        ),
        fluidRow(
          box(
            solidHeader = TRUE,
            status = "olive",
            title = "Facebook KPI Table",
            dataTableOutput("kpiTablefacebook") %>% withSpinner(),
            width = 12
          )
        ),
        fluidRow(
          box(title = "Facebook Report",
              width = 12,
              status = "olive",
              solidHeader = TRUE, 
              DTOutput("facebookTable") %>% withSpinner())
        )
      ),
      
      # Instagram tab ----
      tabItem(
        tabName = "instagram",
        fluidRow(
          valueBoxOutput("instagramTotalAccountsReached"),
          valueBoxOutput("instagramTotalImpressions"),
          valueBoxOutput("instagramTotalPostInteractions")
        ),
        fluidRow(
          box(
            solidHeader = TRUE,
            status = "olive",
            title = "Instagram KPI Table",
            DTOutput("kpiTable") %>% withSpinner(),
            width = 12
          )
        ),
        fluidRow(
          box(title = "Instagram Report",
              width = 12,
              status = "olive",
              solidHeader = TRUE, 
              dataTableOutput("instagramTable") %>% withSpinner())
        )
      ),
      
      # X (Twitter) tab ----
      tabItem(
        tabName = "x",
        fluidRow(
          valueBoxOutput("xAverageEngagementRate"),
          valueBoxOutput("xTotalLinkClicks"),
          valueBoxOutput("xTotalImpressions")
        ),
        fluidRow(
          box(
            solidHeader = TRUE,
            status = "olive",
            title = "X (Twiter) KPI Table",
            DTOutput("kpiTableX") %>% withSpinner(),
            width = 12
          )
        ),
        fluidRow(
          box(title = "Twitter Report",
              width = 12,
              status = "olive",
              solidHeader = TRUE,  
              dataTableOutput("xTable") %>% withSpinner())
        )
      )
      
    )
  )
)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Load the datasets
  linkedinData <- read_excel("./data/data.xlsx", sheet = "Linkedin")
  
  facebookData <- reactive({
    read_excel("./data/data.xlsx", sheet = "Facebook")
  })
  
  instagramData <- reactive({
    read_excel("./data/data.xlsx", sheet = "Instagram")
  })
  
  xData <- reactive({
    read_excel("./data/data.xlsx", sheet = "x")
  })
  
  # LINKEDIN FILTERS
  observe({
    linkedinData <- linkedinData %>%
      mutate(month = as.character(month),  
             month = trimws(month),
             year = as.numeric(as.character(year)))
    
    # Filter out NA values if present
    month_choices <- linkedinData$month[!is.na(linkedinData$month)] %>% unique()
    # Add "All" option for months
    month_choices <- c("All" = "All", month_choices)
    year_choices <- linkedinData$year[!is.na(linkedinData$year)] %>% unique()
    
    # Update the selectInput for months and years
    updateSelectInput(session, "linkedinmonth",
                      choices = month_choices,
                      selected = "All")
    updateSelectInput(session, "linkedinyear",
                      choices = year_choices,
                      selected = format(Sys.Date(), "%Y"))
  })
  
  # Reactive expression to filter the data based on selected month and year
  filtered_data_linkedin <- reactive({
    if (input$linkedinmonth == "All") {
      linkedinData %>%
        filter(year == as.numeric(input$linkedinyear))
    } else {
      linkedinData %>%
        filter(month == input$linkedinmonth, year == as.numeric(input$linkedinyear))
    }
  })
  
  
  
  # DataTables options
  dataTableOptions <- list(
    dom = 't', # This option is to show only the table without the default DataTables controls
    paging = FALSE, # Disable pagination
    ordering = TRUE, # Enable column ordering
    info = FALSE, # Disable showing table information
    searching = FALSE, # Disable search box
    scrollX = TRUE,
    columnDefs = list(
      list(className = 'dt-left', targets = '_all') # Center text in all columns
    ),
    initComplete = JS(
      "function(settings, json) {", 
      "$(this.api().table().header()).css({'background-color': '#4A9094', 'color': 'white', 'text-align': 'center'});", 
      "}"
    )
  )
  
  # Render DataTables within bs4Dash boxes
  output$linkedinTable <- renderDT({
    datatable(filtered_data_linkedin(), options = dataTableOptions, class = "cell-border stripe")
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
  
  
  # LinkedIn_______---------+++++++++++_----------++++__________+++++++++++________++++++++++++++_____________+++++++++++++++++++++++++++++++++++++++++++
  
  # LinkedIn value boxes
  output$linkedinTotalReactions <- renderValueBox({
    data <- filtered_data_linkedin()
    totalReactions <- sum(data$reactions, na.rm = TRUE)
    valueBox(
      formatC(totalReactions, format = "d", big.mark = ","),
      "Total Reactions",
      icon = icon("thumbs-up"),
      color = "olive"
    )
  })
  
  output$linkedinTotalComments <- renderValueBox({
    data <- filtered_data_linkedin()
    totalComments <- sum(data$comments, na.rm = TRUE)
    valueBox(
      formatC(totalComments, format = "d", big.mark = ","),
      "Total Comments",
      icon = icon("comments"),
      color = "warning"
    )
  })
  
  output$linkedinNewFollowers <- renderValueBox({
    data <- filtered_data_linkedin()
    newFollowers <- sum(data$new_followers, na.rm = TRUE)
    valueBox(
      formatC(newFollowers, format = "d", big.mark = ","),
      "New Followers",
      icon = icon("user-plus"),
      color = "info"
    )
  })
  
  output$kpiTablelinkedin <- DT::renderDataTable({
    # Assuming fetchDataLinkedIn() is a function that fetches your LinkedIn data
    data <- filtered_data_linkedin()
    
    # Calculate percentage changes for each KPI
    # Assuming the data is ordered from oldest to newest
    data <- data %>%
      mutate(month_num = match(month, month.name)) %>%
      arrange(year, month_num) %>%
      mutate(
        Impressions_change = (Impressions - lag(Impressions)) / lag(Impressions) * 100,
        reactions_change = (reactions - lag(reactions)) / lag(reactions) * 100,
        comments_change = (comments - lag(comments)) / lag(comments) * 100,
        reposts_change = (reposts - lag(reposts)) / lag(reposts) * 100,
        page_views_change = (page_views - lag(page_views)) / lag(page_views) * 100,
        unique_visitors_change = (unique_visitors - lag(unique_visitors)) / lag(unique_visitors) * 100,
        new_followers_change = (new_followers - lag(new_followers)) / lag(new_followers) * 100
      ) %>%
      select(-month_num) %>% 
      tail(1) # Assuming you want the latest month's data
    
    # Prepare the KPI table
    kpi_table <- tibble(
      KPI = c("Unique Visitors", "Followers new", "Content reactions", "Reposts", "Comments", "Impressions", "Page Views"),
      Total = c(data$unique_visitors, data$new_followers, data$reactions, data$reposts, data$comments, data$Impressions, data$page_views),
      `% Change from Prev Month` = c(data$unique_visitors_change, data$new_followers_change, data$reactions_change, data$reposts_change, data$comments_change, data$Impressions_change, data$page_views_change)
    ) %>%
      mutate(Total = round(Total, 0), # Round to 0 decimal points
             Total = format(Total, big.mark = ",", scientific = FALSE, nsmall = 0), 
             `% Change from Prev Month` = ifelse(is.na(`% Change from Prev Month`), "", sprintf("%+.2f%%", `% Change from Prev Month`))) %>%
      mutate(`% Change from Prev Month` = case_when(
        str_detect(`% Change from Prev Month`, "^\\+") ~ sprintf("<span style='color:green;'>%s &#x25B2;</span>", `% Change from Prev Month`),
        str_detect(`% Change from Prev Month`, "^\\-") ~ sprintf("<span style='color:red;'>%s &#x25BC;</span>", `% Change from Prev Month`),
        TRUE ~ `% Change from Prev Month`
      ))
    
    # Render the table with DT, using HTML for percentage formatting
    datatable(kpi_table, escape = FALSE, options = dataTableOptions) %>%
      formatStyle('`% Change from Prev Month`', `target` = 'html')
  })
  
  
  #X value boxes ----------------=++==============+++++++++++++++++++++++++++++_--------------------------------------==========================++++++++++++++++++++++++++++++++++++++++++++++++++++++value boxes ----------------=++==============+++++++++++++++++++++++++++++_--------------------------------------==========================++++++++++++++++++++++++++++++++++++++++++++++++++++++
  output$xAverageEngagementRate <- renderValueBox({
    data <- xData()
    averageEngagementRate <- mean(as.numeric(gsub("%", "", (data$engagement_rate)*100)), na.rm = TRUE)
    valueBox(
      sprintf("%.2f%%", averageEngagementRate),
      "Average Engagement Rate",
      icon = icon("chart-pie"),
      color = "olive"
    )
  })
  
  output$xTotalLinkClicks <- renderValueBox({
    data <- xData()
    totalLinkClicks <- sum(as.numeric(gsub("-", "0", data$link_clicks)), na.rm = TRUE)
    valueBox(
      formatC(totalLinkClicks, format = "d", big.mark = ","),
      "Total Link Clicks",
      icon = icon("mouse-pointer"),
      color = "warning"
    )
  })
  
  output$xTotalImpressions <- renderValueBox({
    data <- xData()
    totalImpressions <- sum(as.numeric(gsub(",", "", data$impressions)), na.rm = TRUE)
    valueBox(
      formatC(totalImpressions, format = "d", big.mark = ","),
      "Total Impressions",
      icon = icon("eye"),
      color = "info"
    )
  })
  
  
  output$kpiTableX <- DT::renderDataTable({
    # Fetch Facebook data
    data <- read_excel("./data/data.xlsx", sheet = "x")
    
    # Calculate percentage changes for each KPI
    data <- data %>%
      mutate(month_num = match(month, month.name)) %>%
      arrange(year, month_num) %>%
      mutate(year = as.numeric(year),
             impressions = as.numeric(gsub(",", "", impressions)),
             engagement_rate = as.numeric(engagement_rate),
             retweets = as.numeric(retweets),
             likes = as.numeric(likes),
             replies = as.numeric(replies)) %>%
      mutate(
        Impressions_change = (impressions - lag(impressions)) / lag(impressions) * 100,
        Engagement_Rate_change = (engagement_rate - lag(engagement_rate)) / lag(engagement_rate) * 100,
        Retweets_change = (retweets - lag(retweets)) / lag(retweets) * 100,
        Likes_change = (likes - lag(likes)) / lag(likes) * 100,
        Replies_change = (replies - lag(replies)) / lag(replies) * 100
      )
    
    # Select the latest month's data for the KPI table
    latest_data <- tail(data, 1)
    
    # Prepare the KPI table
    kpi_table <- tibble(
      KPI = c("Impressions", "Engagement Rate", "Retweets", "Likes", "Replies"),
      Total = c(latest_data$impressions, latest_data$engagement_rate, latest_data$retweets, latest_data$likes, latest_data$replies),
      `% Change from Prev Month` = c(latest_data$Impressions_change, latest_data$Engagement_Rate_change, latest_data$Retweets_change, latest_data$Likes_change, latest_data$Replies_change)
    ) %>%
      mutate(`% Change from Prev Month` = ifelse(is.na(`% Change from Prev Month`), "", sprintf("%+.1f%%", `% Change from Prev Month`))) %>%
      mutate(`% Change from Prev Month`  = case_when(
        str_detect(`% Change from Prev Month`, "^\\+") ~ sprintf("<span style='color:green;'>%s &#x25B2;</span>", `% Change from Prev Month`),
        str_detect(`% Change from Prev Month`, "^\\-") ~ sprintf("<span style='color:red;'>%s &#x25BC;</span>", `% Change from Prev Month`),
        TRUE ~ `% Change from Prev Month`
      ))
    
    # Render the KPI table
    datatable(kpi_table, escape = FALSE, options = dataTableOptions) %>%
      formatStyle('`% Change from Prev Month`', `target` = 'html')
  })
  
  
  
  
  # FACEBOOK value boxes ----------------=++==============+++++++++++++++++++++++++++++_--------------------------------------==========================++++++++++++++++++++++++++++++++++++++++++++++++++++++
  output$facebookTotalPostReach <- renderValueBox({
    data <- facebookData()
    totalPostReach <- sum(as.numeric(gsub(",", "", data$post_reach)), na.rm = TRUE)
    valueBox(
      formatC(totalPostReach, format = "d", big.mark = ","),
      "Total Post Reach",
      icon = icon("chart-line"),
      color = "olive"
    )
  })
  
  output$facebookTotalInteractions <- renderValueBox({
    data <- facebookData()
    totalInteractions <- sum(data$interactions, na.rm = TRUE)
    valueBox(
      formatC(totalInteractions, format = "d", big.mark = ","),
      "Total Interactions",
      icon = icon("exchange-alt"),
      color = "warning"
    )
  })
  
  output$facebookTotalImpressions <- renderValueBox({
    data <- facebookData()
    totalImpressions <- sum(as.numeric(gsub(",", "", data$impressions)), na.rm = TRUE)
    valueBox(
      formatC(totalImpressions, format = "d", big.mark = ","),
      "Total Impressions",
      icon = icon("eye"),
      color = "info"
    )
  })
  
  output$kpiTablefacebook <- DT::renderDataTable({
    # Fetch Facebook data
    data <- read_excel("./data/data.xlsx", sheet = "Facebook")
    
    # Calculate percentage changes for each KPI
    data <- data %>%
      mutate(month_num = match(month, month.name)) %>%
      arrange(year, month_num) %>%
      mutate(
        post_reach_change = (post_reach - lag(post_reach)) / lag(post_reach) * 100,
        interactions_change = (interactions - lag(interactions)) / lag(interactions) * 100,
        followers_change = (followers - lag(followers)) / lag(followers) * 100,
        visits_change = (visits - lag(visits)) / lag(visits) * 100,
        new_follows_change = (new_follows - lag(new_follows)) / lag(new_follows) * 100,
        impressions_change = (impressions - lag(impressions)) / lag(impressions) * 100
      ) %>%
      select(-month_num) %>%
      tail(1) # Latest month's data
    # Prepare the KPI table with formatted percentage changes
    kpi_table <- tibble(
      KPI = c("Reach", "Interactions", "New followers", "Page Visits", "New Follows", "Impressions"),
      Total = c(data$post_reach, data$interactions, data$followers, data$visits, data$new_follows, data$impressions),
      `% Change from Prev Month` = c(data$post_reach_change, data$interactions_change, data$followers_change, data$visits_change, data$new_follows_change, data$impressions_change)
    ) %>%
      mutate(Total = round(Total, 0), # Round to 0 decimal points
             Total = format(Total, big.mark = ",", scientific = FALSE, nsmall = 0), 
             `% Change from Prev Month` = ifelse(is.na(`% Change from Prev Month`), "", sprintf("%+.2f%%", `% Change from Prev Month`))) %>%
      mutate(`% Change from Prev Month` = case_when(
        str_detect(`% Change from Prev Month`, "^\\+") ~ sprintf("<span style='color:green;'>%s &#x25B2;</span>", `% Change from Prev Month`),
        str_detect(`% Change from Prev Month`, "^\\-") ~ sprintf("<span style='color:red;'>%s &#x25BC;</span>", `% Change from Prev Month`),
        TRUE ~ `% Change from Prev Month`
      ))
    
    # Render the table with DT, allowing HTML content
    datatable(kpi_table, escape = FALSE, options = dataTableOptions) %>%
      formatStyle('`% Change from Prev Month`', `target` = 'html')
  })
  
  
  
  
  
  
  # INSTAGRAM Value Boxes __+++++------______===____-----+++++++-------------------------------------------_____________________________________+__++++++++++
  output$instagramTotalAccountsReached <- renderValueBox({
    data <- instagramData()
    totalAccountsReached <- sum(data$accounts_reached, na.rm = TRUE)
    valueBox(
      formatC(totalAccountsReached, format = "d", big.mark = ","),
      "Total Accounts Reached",
      icon = icon("users"),
      color = "olive"
    )
  })
  
  output$instagramTotalImpressions <- renderValueBox({
    data <- instagramData()
    totalImpressions <- sum(data$impressions, na.rm = TRUE)
    valueBox(
      formatC(totalImpressions, format = "d", big.mark = ","),
      "Total Impressions",
      icon = icon("eye"),
      color = "warning"
    )
  })
  
  output$instagramTotalPostInteractions <- renderValueBox({
    data <- instagramData()
    totalPostInteractions <- sum(data$post_interactions, na.rm = TRUE)
    valueBox(
      formatC(totalPostInteractions, format = "d", big.mark = ","),
      "Total Post Interactions",
      icon = icon("comments"),
      color = "info"
    )
  })
  
  
  #Reactive expression to calculate the KPIs
  output$kpiTable <- DT::renderDataTable({
    # Fetch Instagram data
    data <- read_excel("./data/data.xlsx", sheet = "Instagram")
    
    # Calculate percentage changes for each KPI
    data <- data %>%
      mutate(month_num = match(tolower(month), tolower(month.abb))) %>%
      arrange(year, month_num) %>%
      mutate(
        accounts_reached_change = (accounts_reached - lag(accounts_reached)) / lag(accounts_reached) * 100,
        impressions_change = (impressions - lag(impressions)) / lag(impressions) * 100,
        profile_activity_change = (profile_activity - lag(profile_activity)) / lag(profile_activity) * 100,
        profile_visits_change = (profile_visits - lag(profile_visits)) / lag(profile_visits) * 100,
        post_interactions_change = (post_interactions - lag(post_interactions)) / lag(post_interactions) * 100,
        follows_change = (follows - lag(follows)) / lag(follows) * 100,
        accounts_engaged_change = (accounts_engaged - lag(accounts_engaged)) / lag(accounts_engaged) * 100
      ) %>%
      select(-month_num) %>%
      tail(1) # Latest month's data
    
    # Prepare the KPI table with formatted percentage changes
    kpi_table <- tibble(
      KPI = c("Accounts Reached", "Impressions", "Profile Activity", "Profile Visits", "Post Interactions", "Follows", "Accounts Engaged"),
      Total = c(data$accounts_reached, data$impressions, data$profile_activity, data$profile_visits, data$post_interactions, data$follows, data$accounts_engaged),
      `% Change from Prev Month` = c(data$accounts_reached_change, data$impressions_change, data$profile_activity_change, data$profile_visits_change, data$post_interactions_change, data$follows_change, data$accounts_engaged_change)
    ) %>%
      mutate(Total = round(Total, 0), # Round to 0 decimal points
             Total = format(Total, big.mark = ",", scientific = FALSE, nsmall = 0), 
             `% Change from Prev Month` = ifelse(is.na(`% Change from Prev Month`), "", sprintf("%+.2f%%", `% Change from Prev Month`))) %>%
      mutate(`% Change from Prev Month` = case_when(
        str_detect(`% Change from Prev Month`, "^\\+") ~ sprintf("<span style='color:green;'>%s &#x25B2;</span>", `% Change from Prev Month`),
        str_detect(`% Change from Prev Month`, "^\\-") ~ sprintf("<span style='color:red;'>%s &#x25BC;</span>", `% Change from Prev Month`),
        TRUE ~ `% Change from Prev Month`
      ))
    
    # Render the table with DT, allowing HTML content
    datatable(kpi_table, escape = FALSE, options = dataTableOptions) %>%
      formatStyle('`% Change from Prev Month`', `target` = 'html')
  })
}

shinyApp(ui, server)