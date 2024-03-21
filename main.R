# Import Data
source('Scripts/Import Data.R')
source("Scripts/Graphics.R")

## Convert your data for HEC4

# Import Data and Assign Names
data <- getDataFolder("Data")   # Importing data and storing it in a variable
data_class <- sort_by_year(data) # Sort by year
data_final <- change_year(data_class) # Change name of the year

# Call the function and save the results in a variable
formatted_data <- format_dataframes(data_final)

# Add header and footer
header <- get_header(data_class[[1]])
footer <- "A"

# Concatenate header, formatted data, and footer
final_output <- paste(c(header, formatted_data, footer), collapse = "")

# Write final_output to a file named DATOS.DAT
writeLines(final_output, "DATOS.DAT")
