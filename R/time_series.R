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

#' @title Time Series Analysis
#' @description The function is to design for comprehensive time series analysis and forecasting based on specific criteria within a given dataset. Also, this function is to process the dataset, to perform time series analysis, to generate various plots for analysis, to conduct forecasting, and to save these visualizations and results.
#' @param clean_data A data frame that contains the dataset to be analyzed.
#' @param col The name of the column in clean_data used for filtering the data based on col_value.
#' @param col_value The specific value in the column col used to filter the data.
#' @param date1 The start date for filtering the data.
#' @param date2 The end date for filtering the data.
#' @param date3 The future date for filtering the data.
#' @importFrom dplyr filter
#' @importFrom grDevices png
#' @importFrom stats acf qnorm plot.ts window
#' @importFrom astsa sarima acf2 sarima.for
#' @export

time_series_analysis <- function (clean_data, col, col_value, date1, date2, date3) {
  mesh_term <- clean_data %>% dplyr::filter(.data[[col]] == col_value)%>%
    filter(start_date < as.Date(date2) & start_date > as.Date(date1) & completion_date < as.Date(date3))

  ts_data <- create_ts(mesh_term)
  ts_data_filtered <- stats::window(ts_data, start = c(2010, 1), end=c(2022, 12))

  file_name1 <- paste0("plots/Number of ", col_value, " studies.png")
  png(file_name1)
  plot(ts_data_filtered, ylab = paste0("Number of ", col_value, " studies"))
  dev.off()

  file_name2 <- paste0("plots/", col_value, " Combination Plot.png")
  png(file_name2)
  x = ts_data_filtered
  lx = log(ts_data_filtered)
  dlx = diff(lx)
  ddlx = diff(dlx, 12)
  plot.ts(cbind(x, lx, dlx, ddlx), main= '')
  dev.off()

  file_name3 <- paste0("plots/", col_value, " Seasonal Plot dlx.png")
  png(file_name3, width=1000, height=300)
  par(mfrow=c(1,2))
  monthplot(dlx)
  monthplot(ddlx)
  dev.off()

  file_name4 <- paste0("plots/", col_value, " ACF and PCAF.png")
  png(file_name4)
  acf2(ddlx,50)
  dev.off()

  file_name5 <- paste0("plots/", col_value, " Stats.png")
  png(file_name5)
  sarima(lx, 0,0,1, 0,0,0,12)
  dev.off()

  # Forecasting for the next 10 months
  file_name6 <- paste0("plots/", col_value, " Predicted.png")
  png(file_name6)
  pred = sarima.for(lx,12,0,2,1, 0,1,0,12)
  dev.off()

  upper = pred$pred+qnorm(0.95)*pred$se
  lower = pred$pred-qnorm(0.95)*pred$se
  predict <- data.frame(pred$pred,upper,lower)

  return (list(ts_data_filtered, predict))
}


