# Return API's built in error message if invalid call
apiCheck <- function(req) {
  if (req$status_code==400) {
    error_message <- (gsub("<[^>]*>", "", httr::content(req, as="text")))
    if (error_message == "error: missing 'for' argument") {
      stop("This dataset requires you to specify a geography with the 'region' argument.")
    }
    stop(paste("The Census Bureau returned the following error message:\n", error_message,
               "\n Your API call was: ", print(req$url)))
  }
  # Some time series don't give error messages, just don't resolve (e.g. SAIPE)
  if (req$status_code==204) stop("204, no content was returned.\nSee ?listCensusMetadata to learn more about valid API options.", call. = FALSE)
  if (identical(httr::content(req, as = "text"), "")) stop(paste("No output to parse. \n Your API call was: ", print(req$url)), call. = FALSE)
}

apiParse <- function (req) {
  if (jsonlite::validate(httr::content(req, as="text"))[1] == FALSE) {
    error_message <- (gsub("<[^>]*>", "", httr::content(req, as="text")))
    stop(paste("The Census Bureau returned the following error message:\n", error_message, "\nYour api call was: ", req$url))
  } else {
    raw <- jsonlite::fromJSON(httr::content(req, as = "text"))
  }
}

# Function to clean up column names - particularly ones with periods in them
cleanColnames <- function(dt) {
  # No trailing punct
  colnames(dt) <- gsub("\\.[[:punct:]]*$", "", colnames(dt))
  # All punctuation becomes underscore
  colnames(dt) <- gsub("[[:punct:]]", "_", colnames(dt))
  # Get rid of repeat underscores
  colnames(dt) <- gsub("(_)\\1+", "\\1", colnames(dt))
  return(dt)
}

responseFormat <- function(raw) {
  # Make first row the header
  colnames(raw) <- raw[1, ]
  df <- data.frame(raw)
  df <- df[-1,]
  df <- cleanColnames(df)
  # Make all columns character
  df[] <- lapply(df, as.character)
  # Make columns numeric if they have numbers in the column name - note some APIs use string var names
  # For ACS data, do not make columns numeric if they are ACS annotation variables - ending in MA or EA or SS
  # Do not make label variables (ending in _TTL) numeric
  value_cols <- grep("[0-9]", names(df), value=TRUE)
  error_cols <- grep("MA|EA|SS|_TTL|_NAME|NAICS2012|NAICS2012_TTL|fage4|FAGE4", value_cols, value=TRUE, ignore.case = T)
  for(col in setdiff(value_cols, error_cols)) df[,col] <- as.numeric(df[,col])
  
  row.names(df) <- NULL
  return(df)
}



getCensus2 <- function(name, vars, region, vintage, key = Sys.getenv("CENSUS_KEY")){
  
  vars1 <- paste(c(vars), collapse = ",")
  
  API_URL <- paste("https://api.census.gov/data/", vintage, "/", name, "?get=", vars1, "&for=", region, "&key=", key, sep = "")
  x <- httr::GET(API_URL)
  # Check the API call for a valid response
  apiCheck(x)
  
  # If check didn't fail, parse the content
  raw <- apiParse(x)
  
  # Format the response into a nice data frame
  df <- responseFormat(raw)
  
  return(df)
  
}
