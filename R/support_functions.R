# Function to get the directory with the newest date (as directory name)
get_newest_directory <- function(base_path) {
  # List all directories in the base path
  dir_list <- list.dirs(base_path, full.names = TRUE, recursive = FALSE)
  
  # Remove the base path from the list (since list.dirs adds the base_path itself)
  dir_list <- dir_list[dir_list != base_path]
  
  # Extract the part of the directory name that corresponds to the date
  # Assuming the date is in the format YYYYMMDD-HHMMSS and is part of the folder name
  date_extracted <- gsub(".*/([0-9]{8}-[0-9]{6})$", "\\1", dir_list)
  
  # Convert the extracted date strings to Date objects (this assumes the format is correct)
  dates <- as.POSIXct(date_extracted, format = "%Y%m%d-%H%M%S", tz = "UTC")
  
  # Find the index of the newest directory
  newest_index <- which.max(dates)
  
  # Return the full path to the newest directory
  return(dir_list[newest_index])
}
# # Example usage
# base_path <- "output/complexity"
# newest_dir <- get_newest_directory(base_path)
# 
# # Print the path with the newest directory appended
# new_path <- file.path(base_path, newest_dir)
# cat("Newest directory added to the path:", new_path, "\n")


# ---- Metric calculation functions ----
f_MAE <- function(error) {
  return(mean(abs(error)))
}
f_MSE <- function(error) {
  return(mean((error)^2))
}
f_RMSE <- function(error) {
  return(sqrt(mean(error^2)))
}
f_R2 <- function(prediction, groundtruth) {
  TSS <- sum((groundtruth - mean(groundtruth))^2) # total sum of squares - sum of squared differences between observed values and their mean
  RSS <- sum((groundtruth - prediction)^2) # residual sum of squares - sum of squared differences between observed and predicted values
  R2 <- 1 - (RSS / TSS)
  return(R2)
}