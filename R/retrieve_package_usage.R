#' Retrieve packages that are loaded in a script
#'
#' @param script_name The path to the R script
#'
#' @return dataframe
#' @export
retrieve_package_usage <- function(script_name) {
  # Read the script file
  file <- readLines(script_name)

  # Initialize empty vectors to hold package names and call types
  package_name <- character()
  call_type <- character()
  line_number <- integer()
  conditional_usage <- logical()
  commented_out <- logical()

  # Patterns for package calls
  patterns <- list(
    library = "\\blibrary\\((\"[^\"]+\"|'[^']+')\\)",
    require = "\\brequire\\((\"[^\"]+\"|'[^']+')\\)",
    install_packages = "install\\.packages\\((\"[^\"]+\"|'[^']+')\\)"
  )

  # Iterate over each line in the script file
  for (i in seq_along(file)) {
    line <- file[i]
    # Check each type of package call
    for (type in names(patterns)) {
      pattern <- patterns[[type]]
      matches <- stringr::str_extract(line, pattern = pattern)
      # If a match is found
      if (!is.na(matches)) {
        # Extract the package name and remove surrounding quotes
        pkg_name <- gsub('^"|"$', "", stringr::str_extract(matches, '(?<=")[^"]+(?=")|(?<=\')[^\']+(?=\')'))
        # Store the package name and call type
        package_name <- c(package_name, pkg_name)
        call_type <- c(call_type, type)
        line_number <- c(line_number, i)
        # Check for conditional usage
        conditional_usage <- c(conditional_usage, any(grepl("\\b(if|else|switch)\\b", file[max(1, i - 5):i])))
        # Check if the package usage is commented out
        commented_out <- c(commented_out, grepl("^#", line))
      }
    }
  }

  # If no package calls were found, create a data frame with NA values
  if (length(package_name) == 0) {
    data_frame <- data.frame(
      main_script = script_name,
      package_name = NA,
      call_type = NA,
      line_number = NA,
      conditional_usage = NA,
      commented_out = NA
    )
  } else {
    # Create a dataframe
    data_frame <- data.frame(
      main_script = script_name,
      package_name = package_name,
      call_type = call_type,
      line_number = line_number,
      conditional_usage = conditional_usage,
      commented_out = commented_out
    )
  }

  # Return the dataframe
  return(data_frame)
}
