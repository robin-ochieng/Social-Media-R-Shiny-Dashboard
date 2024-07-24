# Load necessary libraries
library(RPostgres)
library(DBI)
library(readxl)

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
  dbConnect(RPostgres::Postgres(),
            dbname = dbDetails$dbname,
            host = dbDetails$host,
            port = dbDetails$port,
            user = dbDetails$user,
            password = dbDetails$password)
}

# Initialize database with tables if not exists
initializeDatabase <- function(con) {
  queries <- c(
    "CREATE TABLE IF NOT EXISTS linkedin (
      id SERIAL PRIMARY KEY,
      month VARCHAR(20),
      year INTEGER,
      reactions INTEGER,
      comments INTEGER,
      reposts INTEGER,
      page_views INTEGER,
      unique_visitors INTEGER,
      custom_button_clicks INTEGER,
      new_followers INTEGER
    );",
    "CREATE TABLE IF NOT EXISTS facebook (
      id SERIAL PRIMARY KEY,
      month VARCHAR(20),
      year INTEGER,
      post_reach INTEGER,
      interactions INTEGER,
      followers FLOAT,
      link_clicks INTEGER,
      visits INTEGER,
      new_follows INTEGER,
      impressions FLOAT
    );",
    "CREATE TABLE IF NOT EXISTS instagram (
      id SERIAL PRIMARY KEY,
      month VARCHAR(20),
      year INTEGER,
      accounts_reached INTEGER,
      impressions INTEGER,
      profile_activity INTEGER,
      external_link_taps INTEGER,
      follows INTEGER,
      accounts_engaged INTEGER,
      post_interactions INTEGER,
      profile_visits INTEGER
    );",
    "CREATE TABLE IF NOT EXISTS x (
      id SERIAL PRIMARY KEY,
      month VARCHAR(20),
      year INTEGER,
      engagement_rate FLOAT,
      impressions INTEGER,
      link_clicks INTEGER,
      retweets INTEGER,
      likes INTEGER,
      replies INTEGER
    );"
  )
  
  for (query in queries) {
    dbExecute(con, query)
  }
}

# Function to load data from Excel and insert into database
loadDataFromExcel <- function(con, path_to_excel) {
  sheets <- c("Linkedin", "Facebook", "Instagram", "x")
  
  for (sheet in sheets) {
    # Read data from Excel
    data <- read_excel(path_to_excel, sheet = sheet)
    # Overwrite existing table with new data
    dbWriteTable(con, tolower(sheet), data, overwrite = TRUE, row.names = FALSE)
  }
}
# Main script execution
con <- getConnection() # Establish connection
initializeDatabase(con) # Initialize database tables

# Define path to Excel file
path_to_excel <- "C:/Users/Robin Ochieng/Desktop/data/data.xlsx"
loadDataFromExcel(con, path_to_excel) # Load data from Excel and insert into database

dbDisconnect(con) # Close the database connection