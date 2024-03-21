source('Scripts/tabla.R')

# Function to import data from a folder
getDataFolder <- function(path) {
  folder_path <- path # Path of the folder {url}
  
  # Use list.files() to retrieve the list of files in the folder
  files_in_folder <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Import all data files and structure them appropriately
  data <- setTables(files_in_folder)
  
  return(data)
}

# Function to import names from files in a folder
setNames <- function(name) {
  # Define the path of the folder
  folder_path <- name
  
  # Use list.files() to retrieve the list of files in the folder
  files_in_folder <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Create an empty array to store the names of the DataFrames
  dataframe_names <- c()
  
  # Iterate over each file in the list
  for (file in files_in_folder) {
    # Extract the name of the file
    dataframe_name <- sub(".csv$", "", basename(file))
    
    # Append the name to the array
    dataframe_names <- c(dataframe_names, dataframe_name)
  }
  
  return(dataframe_names)
}

# Function to format the dataframes
format_dataframes <- function(dataframes) {
  formatted_data <- c()
  
  for (df_name in names(dataframes)) {
    df <- dataframes[[df_name]]
    
    # Formatear los datos
    formatted_df <- ""
    for (i in 1:nrow(df)) {
      formatted_row <- sprintf("%-8s %5s %5s %5s %5s %5s %5s %5s %5s %5s %5s %5s %5s",
                               df[i, "year"],
                               df[i, "January"],
                               df[i, "February"],
                               df[i, "March"],
                               df[i, "April"],
                               df[i, "May"],
                               df[i, "June"],
                               df[i, "July"],
                               df[i, "August"],
                               df[i, "September"],
                               df[i, "October"],
                               df[i, "November"],
                               df[i, "December"])
      formatted_df <- paste(formatted_df, formatted_row, "\n", sep = "")
    }
    
    # Agregar los datos formateados al vector
    formatted_data <- c(formatted_data, formatted_df)
  }
  
  # Retornar los datos formateados
  return(formatted_data)
}

# Sort by the minimum and maximum years
sort_by_year <- function(lista_df) {
  # Obtain the minimum and maximum years for each dataframe
  years_min <- sapply(lista_df, function(df) min(df$year))
  years_max <- sapply(lista_df, function(df) max(df$year))
  
  # Find the largest minimum year and smallest maximum year
  year_min_grande <- max(years_min)
  year_max_pequeno <- min(years_max)
  
  # Create a new list of data frames with years within the range
  lista_df_filtered <- lapply(lista_df, function(df) {
    df[df$year >= year_min_grande & df$year <= year_max_pequeno, ]
  })
  
  return(lista_df_filtered)
}

# Get header
get_header <- function(df) {
  # Obtain the minimum year
  min_year <- min(df$year)
  
  # Obtain the number of rows
  num_rows <- nrow(df)
  
  # Obtain the maximum year
  max_year <- max(df$year)
  
  # Future years
  next_min_year <- max_year + 1
  next_max_year <- max_year + 4
  
  # Form the header
  header <- paste("A    PRECIPITATION (MM)
A    COMPLETION AND EXTENSION OF PRECIPITATION SERIES
A
B   ", min_year, "       1       1      ", num_rows, "       1       4       1       0       0       0
C      0       0       0       0       0    ", next_min_year, "       1    ", next_max_year, "       0       1
", sep = "")
  
  return(header)
}

# Change the name of the year
change_year <- function(list_df) {
  for (i in 1:length(list_df)) {
    list_df[[i]]$year <- paste("H10", i, list_df[[i]]$year, sep = "")
  }
  
  return(list_df)
}


