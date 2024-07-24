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