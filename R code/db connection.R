library(RPostgres)
library(DBI)

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




initializeDatabase <- function(con) {
  queries <- c(
    "CREATE TABLE IF NOT EXISTS linkedin (
      id SERIAL PRIMARY KEY,
      month VARCHAR(20),
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




# Run database initialization
con <- getConnection()
initializeDatabase(con)
dbDisconnect(con)





library(readxl)
library(RPostgres)
library(DBI)

# Assuming getConnection() is already defined as per your previous code

# Step 1: Read Excel Sheets into R
path_to_excel <- "C:/Users/Robin Ochieng/Desktop/data/data.xlsx"

linkedin_data <- read_excel(path_to_excel, sheet = "LinkedIn")
facebook_data <- read_excel(path_to_excel, sheet = "Facebook")
instagram_data <- read_excel(path_to_excel, sheet = "Instagram")
x_data <- read_excel(path_to_excel, sheet = "x")

# Step 2: Connect to the Database
con <- getConnection()

# Step 3: Write the Data to the Database
dbWriteTable(con, "linkedin", linkedin_data, append = TRUE)
dbWriteTable(con, "facebook", facebook_data, append = TRUE)
dbWriteTable(con, "instagram", instagram_data, append = TRUE)
dbWriteTable(con, "x", x_data, append = TRUE)

# Close the database connection
dbDisconnect(con)
