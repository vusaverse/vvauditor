# Retrieves all assigned string objects within a Rscript
#' retrieve_string_assignments
#'
#' @param script_name The script to search objects in
#'
#' @return dataframe
#' @export
#'
retrieve_string_assignments <- function(script_name) {
  # Read the script file
  file <- readLines(script_name)

  # Initialize empty vectors to hold object names and their assigned strings
  object_name <- character()
  string <- character()

  # Iterate over each line in the script file
  for (line in file) {
    # Extract the object names and strings using regex
    pattern <- stringr::str_match(line,
                                  pattern = "(\\w+)\\s*<-\\s*\"(.*)\"")
    # If a string assignment is found
    if (!is.na(pattern[1, 1])) {
      object_name <- c(object_name, pattern[1, 2])
      string <- c(string, pattern[1, 3])
    }
  }

  # If no string assignments were found, create a data frame with NA values
  if (length(object_name) == 0) {
    data_frame <- data.frame(main_script = script_name,
                             object_name = NA,
                             string = NA)
  } else {
    # Create a dataframe
    data_frame <- data.frame(main_script = script_name,
                             object_name = object_name,
                             string = string)
  }

  # Return the dataframe
  return(data_frame)
}
