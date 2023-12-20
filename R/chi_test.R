#' @format A data frame with 471252 rows and 13 columns:
#' @keywords studies
"studies"

#' @importFrom utils data
data("studies")

#' @title Clean the data
#' @description The function is to clean and summarize a given dataset.
#' @param data The dataset in which the function operates.
#' @param x The name of the column in data containing terms or items to be analyzed.
#' @param y The name of the column containing categorical data against which the terms in x are to be counted.
#' @importFrom utils head
#' @importFrom dplyr filter group_by summarise arrange n
#' @importFrom tidyr separate_rows
#' @export

clean_data <- function (data, x, y) {
  # Check if 'data' is a data frame or tibble
  if (!is.data.frame(data)) {
    stop("The 'data' argument must be a data frame or tibble.")
  }

  # Proceed with the function
  data <- data %>% dplyr::filter(!is.na(.data[[x]]))

  # Remove columns by unsetting them
  data$url <- NULL
  data$keyword <- NULL
  data$conditions <- NULL
  data$brief_title <- NULL

  # Separate the terms and count them
  data <- data %>% separate_rows(.data[[x]], sep = ",\\s*")
  data_summary <- data %>%
    group_by(.data[[x]]) %>%
    summarise(total_count = n(), .groups = 'drop') %>%
    arrange(desc(total_count))

  # Extract the top 100 terms based on total_count
  x_top_100 <- utils::head(data_summary, 100)[[x]]

  # Get unique values of the class_col_name
  unique_y <- unique(as.character(data[[y]]))

  # Return a list containing all three
  return (list(data = data, x_top_100 = x_top_100, unique_y = unique_y))
}

#' @title Normalization
#' @description The function is to create and normalize a count matrix based on two specific columns from a given dataset
#' @param data The dataset in which the function operates.
#' @param x The name of the column in data containing terms or items to be analyzed.
#' @param y The name of the column containing categorical data against which the terms in x are to be counted.
#' @param x_top_100  A vector of the top 100 terms or items from the x column that you want to include in the analysis.
#' @param unique_y A vector of unique categories from the y column.
#' @export
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr arrange
#' @importFrom tidyr separate_rows
#' @export

normalization <- function(data, x, y, x_top_100, unique_y) {

  # Create an empty matrix with the number of rows equal to the length of x_top_100
  count_matrix <- matrix(0, nrow = length(x_top_100), ncol = length(unique_y))
  rownames(count_matrix) <- x_top_100
  colnames(count_matrix) <- unique_y

  # Populate the matrix
  for(i in 1:length(x_top_100)) {
    for(j in 1:length(unique_y)) {
      class <- colnames(count_matrix)[j]
      term <- rownames(count_matrix)[i]

      # Count rows where both the term and class are present
      count_matrix[i, j] <- sum(grepl(term, data[[x]]) & data[[y]] == class)
    }
  }

  normalization <- sweep(count_matrix, 2, colSums(count_matrix), FUN="/")
  return (list(count_matrix, normalization))
}

#' @title Chi-Square Test Matrix
#' @description The function is to function in R is designed to compute a matrix of Chi-squared test p-values for pairs of columns in a given input matrix.
#' @param normalization_matrix A matrix contains the normalized values from two specific columns in the data set.
#' @importFrom stats chisq.test
#' @export
chi_square_matrix <- function (normalization_matrix) {
  col_names <- colnames(normalization_matrix)
  chitest = matrix(0, nrow = length(col_names), ncol = length(col_names))

  # Loop over each pair of columns
  for (i in 1:(length(col_names) - 2)) {
    for (j in (i + 1):(length(col_names)-1)) {
      x = normalization_matrix[,i]
      y = normalization_matrix[,j]

      chi_square_result <- chisq.test(x , y)
      chitest[i,j] = chi_square_result$p.value
    }
  }
  return (chitest)
}

#to perform pairwise comparisons between the columns of a normalization matrix
#' and generates line charts for those comparisons that are statistically significant.
#'
#' @title Line Charts for Normalization Matrix
#' @description The function is to perform pairwise comparisons between the columns of a normalization matrix
#' and generates line charts for those comparisons that are statistically significant.
#' @param normalization_matrix A matrix containing normalized data for pairwise comparison.
#' @param count_matrix A matrix containing count data for Chi Test
#' @param column The name of the column in the matrix used for sorting before generating comparisons.
#' @param var_name The name of the variable in the long format that will hold the original column names of the matrix.
#' @param value_name The name of the value variable in the long format.
#' @importFrom dplyr arrange slice_head
#' @importFrom ggplot2 ggplot aes_string geom_line theme element_text labs scale_y_continuous ggsave
#' @importFrom reshape2 melt
#' @export
line_chart_comparison <- function(normalization_matrix, count_matrix, column, var_name, value_name){
  # Ensure that the column to sort by is present in the matrix
  if(!(column %in% colnames(normalization_matrix))) {
    stop("The specified column for sorting does not exist in the matrix.")
  }

  chitest <- chi_square_matrix(count_matrix)
  # Convert the matrix to a data frame and sort by the specified column
  normalization_df<- as.data.frame(normalization_matrix)
  normalization_df$disease <- rownames(normalization_df)
  normalization_df <- normalization_df %>%
    arrange(desc(.[[column]])) %>%
    slice_head(n = 50) # Get the top 50

  # Melt the data frame to long format
  normalization_long <- melt(normalization_df, id.vars = 'disease', variable.name = var_name, value.name = value_name)

  # Ensure that the value column is numeric
  normalization_long[[value_name]] <- as.numeric(normalization_long[[value_name]])

  # Plotting loop
  n <- ncol(normalization_matrix)
  col_names <- colnames(normalization_matrix)
  for (i in 2:(n - 1)) {
    for (j in (i + 1):n) {
        subset_data <- normalization_long[normalization_long[[var_name]] %in% col_names[c(i, j)],]
        p <- ggplot(subset_data, aes_string(x = "disease", y = value_name, group = var_name, color = var_name)) +
          geom_line() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
          labs(title = paste("Comparison between", col_names[i], "and", col_names[j]),
               x = "Disease", y = "Percentage of Trials") +
          scale_y_continuous(labels = scales::percent)
         #ggsave(paste0("plots/linechart",i,"_",j,".png",sep = ""))
    }
  }
  return(p)
}




