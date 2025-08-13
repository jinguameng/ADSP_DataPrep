
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
# parse_range("1 - 99999")                # seq(1, 99999)
# parse_range("1;2;8;9")                  # c(1, 2, 8, 9)
# parse_range("0 - 25;96;97;98;99")       # c(0:25, 96:99)
# parse_range("0.0 - 3.4")                # 0.0, 0.1, 0.2, ..., 3.4
# parse_range("-1.5 - 0.5;2.25;3")        # supports negatives and decimals
# parse_range("0.0 - 3.4", step = 0.2)    # override step: 0.0, 0.2, ..., 3.4
parse_range <- function(x, step = NULL, unique_sort = FALSE) {
  parts <- unlist(strsplit(x, ";", fixed = TRUE))
  out <- numeric(0)
  
  rx_range <- "^([+-]?\\d+(?:\\.\\d+)?)\\s*-\\s*([+-]?\\d+(?:\\.\\d+)?)$"
  rx_num   <- "^[+-]?\\d+(?:\\.\\d+)?$"
  
  dec_places <- function(s) {
    if (!grepl("\\.", s)) 0 else nchar(sub(".*\\.", "", s))
  }
  
  for (p in parts) {
    p <- trimws(p)
    
    # range a - b
    m <- regexec(rx_range, p)
    hit <- regmatches(p, m)[[1]]
    if (length(hit)) {
      a_chr <- hit[2]; b_chr <- hit[3]
      a <- as.numeric(a_chr); b <- as.numeric(b_chr)
      
      by <- if (!is.null(step)) {
        step
      } else {
        10^(-max(dec_places(a_chr), dec_places(b_chr)))
      }
      if (by <= 0) stop("Step must be positive.")
      
      if (a <= b) {
        out <- c(out, seq(a, b, by = by))
      } else {
        out <- c(out, seq(a, b, by = -by))
      }
      next
    }
    
    # single number
    if (grepl(rx_num, p)) {
      out <- c(out, as.numeric(p))
      next
    }
    
    if (nzchar(p)) stop(sprintf("Unrecognized token: '%s'", p))
  }
  
  if (unique_sort) unique(sort(out)) else out
}


check_valid_numeric_responses <- function(df_valid, df_data) {
  # Step 1: clean and normalize the Valid Responses column
  df_valid <- df_valid %>%
    mutate(
      cleaned_response = gsub("\r\n", "", `Valid Responses`),
      cleaned_response = str_replace_all(cleaned_response, "thru", "-"),
      cleaned_response = str_replace_all(cleaned_response, ";\\s*", ";"),
      cleaned_response = str_replace_all(cleaned_response, ";+$", "")
    )
  
  # Initialize results
  result_list <- list()
  
  # Step 2: loop and check
  for (i in seq_len(nrow(df_valid))) {
    
    varname <- df_valid$VarNames[i]
    
    if (!varname %in% names(df_data)) next  # skip if var not in data
    
    observed <- unique(na.omit(df_data[[varname]]))
    valid_vals <- df_valid$cleaned_response[i]
    
    # Prepare accepted values
    accepted <- c()
    accepted <- parse_range(valid_vals)
    
    # Find unexpected values
    invalid <- setdiff(observed, accepted)
    
    if (length(invalid) > 0) {
      result_list[[varname]] <- data.frame(
        VarName = varname,
        Invalid_Values = paste(invalid, collapse = ", "),
        Accepted_values = df_valid$cleaned_response[df_valid$VarNames == varname]
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

