
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


## common columns detection
commonColsDetect <- function(df_names) {
  
  ## Step 1: Get a named list of column names for each data frame
  df_cols <- lapply(df_names, function(name) colnames(get(name)))
  names(df_cols) <- df_names
  
  ## Step 2: Initialize storage
  n <- length(df_names)
  results <- list()
  
  ## Step 3: Loop through lower triangle (i > j)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i > j) {
        df1 <- df_names[i]
        df2 <- df_names[j]
        common <- intersect(df_cols[[i]], df_cols[[j]])
        results[[length(results) + 1]] <- list(
          df1 = df1,
          df2 = df2,
          n_common = length(common),
          common_cols = paste(common, collapse = ", "),
          common_list = I(list(common))  # store as list for filtering later
        )
      }
    }
  }
  
  ## Step 4: Convert to data frame
  common_df <- do.call(rbind, lapply(results, as.data.frame))
  rownames(common_df) <- NULL
  
  ## Step 5: Filter out rows where common_cols is only from a known set
  key_cols <- c("WRAPNo", "wrapnum", "VisNo")
  common_df <- common_df[!sapply(common_df$common_list, function(cols) {
    all(cols %in% key_cols)
  }), ]
  
  ## Step 6: Clean up and sort
  common_df <- common_df[order(-common_df$n_common), ]
  common_df$common_list <- NULL  # drop helper column
  
  return(DT::datatable(common_df))
}


## check unique values for character columns
uniqueValforChrColumns <- function(df){
  chrcols <- colnames(df)[sapply(df, is.character)]
  
  for (x in chrcols) {
    cat(x, ":\n")
    print(unique(df[[x]]))
    cat("\n")
  }
  
}


## check unique values for selected columns
## this function will return mismatched variables names
uniqueValCheck <- function(df, uniqueVals, cols2Bchecked) {
  `%!in%` <- Negate(`%in%`)
  
  for (x in cols2Bchecked) {
    vals <- unique(df[[x]])
    vals <- vals[!is.na(vals)]  # Remove NA values
    
    if (any(vals %!in% uniqueVals)) {
      cat(x, ":\n")
      print(vals[vals %!in% uniqueVals])
      cat("\n")
    }
  }
}


## merge function
merge_with_info <- function(df1,df2_name, id_col) {
  
  df2 <- get(df2_name)

  cat("Info for", df2_name, ":\n")
  info(df2, id_col)
  
  merged <- merge(df1, df2, all = TRUE)
  
  cat("\nInfo for merged dataset:\n")
  info(merged, id_col)
  
  cat("=======================================================================")
  cat("\n")
  
  return(merged)
}


## this function check mismatched chr columns between dataset and DD
## df_valid: contains VarNames and Valid Responses
## df_data: the dataset to check
check_valid_responses <- function(df_valid, df_data) {
  # Clean up Valid Responses: remove \r\n and split into list of valid values
  df_valid <- df_valid %>%
    mutate(
      `Valid Responses` = gsub("\r\n", "", `Valid Responses`),
      valid_list = strsplit(`Valid Responses`, ";\\s*")
    )
  
  # Initialize a list to store results
  result_list <- list()
  
  # Loop over each variable in df_valid
  for (i in seq_len(nrow(df_valid))) {
    varname <- df_valid$VarNames[i]
    
    # Skip if variable not in df_data
    if (!varname %in% names(df_data)) next
    
    # Get unique non-NA values in data
    observed <- unique(na.omit(df_data[[varname]]))
    expected <- trimws(df_valid$valid_list[[i]])
    
    # Find unexpected values
    invalid <- setdiff(as.character(observed), expected)
    
    if (length(invalid) > 0) {
      result_list[[varname]] <- data.frame(
        VarName = varname,
        Invalid_Values = paste(invalid, collapse = ", ")
      )
    }
  }
  
  # Combine results
  if (length(result_list) > 0) {
    do.call(rbind, result_list)
  } else {
    message("All values are within valid ranges.")
    return(invisible(NULL))
  }
}



## similar as above, but for numeric columns
check_valid_numeric_responses <- function(df_valid, df_data) {
  # Step 1: clean and normalize the Valid Responses column
  df_valid <- df_valid %>%
    mutate(
      cleaned_response = gsub("\r\n", "", `Valid Responses`),
      cleaned_response = str_replace_all(cleaned_response, "thru", "-"),
      cleaned_response = str_replace_all(cleaned_response, ";\\s*", ";"),
      cleaned_response = str_replace_all(cleaned_response, ";+$", ""),
      response_list = str_split(cleaned_response, ";")
    )

  # Initialize results
  result_list <- list()
  
  # Step 2: loop and check
  for (i in seq_len(nrow(df_valid))) {
    varname <- df_valid$VarNames[i]
    
    if (!varname %in% names(df_data)) next  # skip if var not in data
    
    observed <- unique(na.omit(df_data[[varname]]))
    valid_vals <- df_valid$response_list[[i]]
    
    # Prepare accepted values
    accepted <- c()
    
    for (val in valid_vals) {
      
      val <- trimws(val)
      
      if (grepl("^-?\\d+\\s*-\\s*-?\\d+$", val)) {
        bounds <- as.numeric(str_split(val, "\\s*-\\s*")[[1]])
        accepted <- c(accepted, seq(bounds[1], bounds[2]))
        
      } else if (grepl("^-?\\d+$", val)) {
        # Single numeric value
        accepted <- c(accepted, as.numeric(val))
      }
    }
    
    accepted <- unique(accepted)
    
    # Find unexpected values
    invalid <- setdiff(observed, accepted)
    
    if (length(invalid) > 0) {
      result_list[[varname]] <- data.frame(
        VarName = varname,
        Invalid_Values = paste(invalid, collapse = ", ")
      )
    }
  }
  
  # Step 3: return result
  if (length(result_list) > 0) {
    return(do.call(rbind, result_list))
  } else {
    message("All numeric values are within valid ranges.")
    return(invisible(NULL))
  }
}

