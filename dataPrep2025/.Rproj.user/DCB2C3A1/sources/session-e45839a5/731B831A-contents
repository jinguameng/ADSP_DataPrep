
## Alaina's functions
#get copy-able information about a data frames observations, columns, and individuals
info <- function(df, ID_name) {
  # Get the number of rows
  num_rows <- nrow(df)
  
  # Get the number of columns
  num_cols <- ncol(df)
  
  # Get the number of unique IDs in the specified column
  num_unique_ids <- length(unique(df[[ID_name]]))
  
  # Construct the formatted string
  output <- paste0("#obs:", num_rows, ", cols:", num_cols, ", inds:", num_unique_ids)
  
  # Print the output using cat
  cat(output, "\n")
  
  #prints: #obs:X, cols:Y, inds:Z
}
