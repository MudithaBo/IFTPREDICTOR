#' @param ... Parameters needed for equation generation
#' @return The generated equation as a string
#' @examples
#' # Example data for splits and leaf node coefficients
#' split_rows <- data.frame(
#'   number = c(1, 2, 3),
#'   left = c(2, NA, NA),
#'   right = c(3, NA, NA),
#'   variable = c("age", "income", "education"),
#'   threshold = c(30, 50, 12)
#' )
#' leaf_node_numbers <- c(2, 3)
#' leaf_node_coefficients <- c("0.5", "-0.3")
#' names(leaf_node_coefficients) <- c("2", "3")  # Coefficients for leaf nodes

#' # Generate equation for the root node (node 1)
#' equation <- branch_equation_generator(1, split_rows, leaf_node_numbers, leaf_node_coefficients)
#' print(equation)
#'
#' # Expected output: (0.5 * (dataset[['age']] <= 30)) + (-0.3 * (dataset[['income']] > 50))
#' @keywords internal
branch_equation_generator <- function(node_number, split_rows, leaf_node_numbers, leaf_node_coefficients) {
  generate_equation_part <- function(node_number, split_conditions) {
    if (node_number %in% leaf_node_numbers) {
      leaf_coefficient <- as.numeric(leaf_node_coefficients[as.character(node_number)])
      condition_string <- paste(split_conditions, collapse = " * ")
      leaf_equation <- paste0("(", leaf_coefficient, " * ", condition_string, ")")
      return(leaf_equation)
    } else {
      current_split <- split_rows[split_rows$number == node_number, ]
      if (nrow(current_split) == 0) {
        return("No split found")
      }
      left_node_number <- current_split$left
      right_node_number <- current_split$right
      split_variable_name <- current_split$variable
      split_threshold <- current_split$threshold

      # Add current split condition for the left and right nodes
      left_conditions <- c(split_conditions, paste0("(dataset[['", split_variable_name, "']] <= ", split_threshold, ")"))
      right_conditions <- c(split_conditions, paste0("(dataset[['", split_variable_name, "']] > ", split_threshold, ")"))

      # Recursively generate equations for the left and right branches
      left_equation <- generate_equation_part(left_node_number, left_conditions)
      right_equation <- generate_equation_part(right_node_number, right_conditions)

      return(paste0(left_equation, " + ", right_equation))
    }
  }

  final_equation <- generate_equation_part(node_number, split_conditions = list())
  return(final_equation)
}
