#' @format A data frame with 471252 rows and 13 columns:
#' @keywords studies
"studies"

#' @importFrom utils data
data("studies")

#' @title Time Series Data Cleanning
#' @description The function is to preprocess a time series dataset, focusing on data cleaning and aggregation tasks.
#' @param data The dataset in which the function operates.
#' @param col1 The data frame to be processed.
#' @param col1_expected The expected value in col1 for filtering rows.
#' @param col2 The name of the second column to be used for additional filtering.
#' @param col2_expected_1 Value in col2 to be excluded from the dataset.
#' @param col2_expected_2 Value in col2 to be excluded from the dataset.
#' @param col2_expected_3 Value in col2 to be excluded from the dataset.
#' @param col3 The name of the column representing the start date of each entry.
#' @param col4 The name of the column representing the end or completion date of each entry.
#' @param col5 The name of the column representing the end or completion date of each entry.
#' @importFrom dplyr filter group_by summarise ungroup arrange sym
#' @importFrom stats plot.ts
#' @importFrom grDevices dev.off png
#' @export

clean_data_time_series<- function (data, col1, col1_expected, col2, col2_expected_1, col2_expected_2,col2_expected_3, col3, col4, col5){
  # Filter both dates before 2023-12-01 & U.S
  data_clean <- data %>% dplyr::filter(!is.na(.data[[col2]])) %>%
    filter(.data[[col1]] == col1_expected) %>%
    filter(.data[[col2]] != col2_expected_1) %>%
    filter(.data[[col2]] != col2_expected_2) %>%
    filter(.data[[col2]] != col2_expected_3) %>%
    filter(!(is.na(.data[[col3]]) & is.na(.data[[col4]])))

  # Group by using sym function for dynamic column name
  top10_mesh_term <- data_clean %>%
    group_by(!!sym(col5)) %>%
    summarise(total_count = n()) %>%
    ungroup() %>%
    arrange(desc(total_count)) %>%
    as.data.frame()

  # Calculate mean
  average_duration <-
    mean(na.omit(as.numeric(data_clean[[col4]] - data_clean[[col3]])))

  # For start_date
  data_clean[[col3]][is.na(data_clean[[col3]])] <-
    data_clean[[col4]][is.na(data_clean[[col3]])] - average_duration

  data_clean[[col4]][is.na(data_clean[[col4]])] <-
    data_clean[[col3]][is.na(data_clean[[col4]])] + average_duration

  # Convert to Date
  data_clean[[col3]] <- as.Date(data_clean[[col3]])
  data_clean[[col4]] <- as.Date(data_clean[[col4]])

  return (list(data_clean, top10_mesh_term))
}

#' @title Create Time Series Vectors
#' @description The function is to convert the aggregated monthly counts into a vector and to create a time series object (ts) from this vector.
#' @param mesh_term A data frame that is expected to contain at least two columns named start_date and completion_date. These columns should contain date information for each entry in the data frame.
#' @importFrom dplyr rowwise do count
#' @importFrom lubridate floor_date
#' @importFrom stats ts
#' @export

create_ts <- function(mesh_term) {
  monthly_counts <- mesh_term %>%
    rowwise() %>%
    do(data.frame(month = seq(.$start_date, .$completion_date, by = "month"))) %>%
    count(month = floor_date(month, unit = "month"))

  # Create a vector of all counts
  counts_vector <- as.vector(t(monthly_counts[,-1]))

  # The frequency is 12 for monthly data, and the start is the first month of the   first year
  ts_counts <- ts(counts_vector, start=c(monthly_counts$year[1], 2013), frequency=12)

  return(ts_counts)
}

