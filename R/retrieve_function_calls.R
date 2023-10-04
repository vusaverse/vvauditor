# Retrieves all created functions within a Rscript
#' retrieve_function_calls
#'
#' @param script_name The script to search functions in
#'
#' @return dataframe
#' @export
#'
retrieve_function_calls <- function(script_name) {
  # Read the script file
  file <- readLines(script_name)

  # Initialize empty vectors to hold function names, their arguments, and bodies
  function_name <- character()
  arguments <- character()
  function_body <- character()

  # Iterate over each line in the script file
  for (i in 1:length(file)) {
    # Extract the function names and arguments using regex
    pattern <- stringr::str_match(file[i],
      pattern = "(\\w+)\\s*<-\\s*function\\((.*)\\)\\s*\\{"
    )
    # If a function definition is found
    if (!is.na(pattern[1, 1])) {
      function_name <- c(function_name, pattern[1, 2])
      arguments <- c(arguments, pattern[1, 3])

      # Initialize counter for open brackets
      open_brackets <- 1
      # Initialize function body with the current line
      body <- file[i]
      # Start from the next line
      j <- i + 1

      while (open_brackets > 0 && j <= length(file)) {
        # Update the count of open and close brackets
        open_brackets <- open_brackets + length(gregexpr("\\{", file[j])[[1]])
        open_brackets <- open_brackets - length(gregexpr("\\}", file[j])[[1]])
        # Add the line to the function body
        body <- paste(body, file[j], sep = "\n")
        # Move to the next line
        j <- j + 1
      }

      # Add the function body to the vector
      function_body <- c(function_body, body)
    }
  }

  # If no function definitions were found, create a data frame with NA values
  if (length(function_name) == 0) {
    data_frame <- data.frame(
      main_script = script_name,
      function_name = NA,
      arguments = NA,
      function_body = NA
    )
  } else {
    # Create a dataframe
    data_frame <- data.frame(
      main_script = script_name,
      function_name = function_name,
      arguments = arguments,
      function_body = function_body
    )
  }

  # Return the dataframe
  return(data_frame)
}
