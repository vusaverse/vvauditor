# Retrieves all sourced scripts within a main Rscript
#' retrieve_sourced_scripts
#'
#' @param script_name The main script to search
#'
#' @return dataframe
#' @export
#'
retrieve_sourced_scripts <- function(script_name) {
  # Read the script file
  file <- readLines(script_name)

  # Initialize empty vectors to hold sourced scripts and comment status
  sourced_script <- character()
  line_number <- integer()
  conditional_sourcing <- logical()
  loop_sourcing <- logical()
  repeated_sourcing <- logical()
  script_exists <- logical()
  commented_out <- logical()

  # Iterate over each line in the script file
  for (i in seq_along(file)) {
    line <- file[i]
    # Extract the sourced file name and parameters using regex
    pattern <- stringr::str_extract(line,
                                    pattern = "(?<=source\\(\")[^\"]+\\.R(?=\"\\))")
    # If a source call is found
    if (!is.na(pattern)) {
      sourced_script <- c(sourced_script, pattern)
      # Check if the source call is commented out
      line_number <- c(line_number, i)
      # Check for conditional sourcing and loop sourcing
      conditional_sourcing <- c(conditional_sourcing, any(grepl("\\b(if|else|switch)\\b", file[max(1, i-5):i])))
      loop_sourcing <- c(loop_sourcing, any(grepl("\\b(for|while|repeat)\\b", file[max(1, i-5):i])))
      # Check if the sourced script exists
      script_exists <- c(script_exists, file.exists(pattern))
      # Check if the source call is commented out
      commented_out <- c(commented_out, startsWith(trimws(line), "#"))
    }
  }

  # Check for repeated sourcing
  repeated_sourcing <- duplicated(sourced_script)

  # If no sourced scripts were found, create a data frame with NA values
  if (length(sourced_script) == 0) {
    data_frame <- data.frame(main_script = script_name,
                             sourced_script = NA,
                             line_number = NA,
                             commented_out = NA,
                             conditional_sourcing = NA,
                             loop_sourcing = NA,
                             repeated_sourcing = NA,
                             script_exists = NA)
  } else {
    # Create a dataframe
    data_frame <- data.frame(main_script = script_name,
                             sourced_script = sourced_script,
                             line_number = line_number,
                             commented_out = commented_out,
                             conditional_sourcing = conditional_sourcing,
                             loop_sourcing = loop_sourcing,
                             repeated_sourcing = repeated_sourcing,
                             script_exists = script_exists)
  }

  # Return the dataframe
  return(data_frame)
}
