library(dplyr)
library(tidyr)
library(lubridate)
library(reshape2)
library(ggplot2)
library(astsa)

test_that("Clean data time series",{
  data("studies")
  data <- studies
  col1 <- "country"
  col1_expected <- "United States"
  col2 <- "source_class"
  col2_expected_1 <- "UNKNOWN"
  col2_expected_2 <- "INDIV"
  col2_expected_3 <- "OTHER"
  col3 <- "start_date"
  col4 <- "completion_date"
  col5 <- "downcase_mesh_term"

  result <- clean_data_time_series(data, col1, col1_expected, col2, col2_expected_1, col2_expected_2,col2_expected_3, col3, col4, col5)
  clean_data <- result[[1]]
  top10_mesh_term <- result[[2]]

  expect_true(all(clean_data[[col1]] == "United States"))
  expect_true(all(!(is.na(clean_data[[col3]]) & is.na(clean_data[[col4]]))))
  expect_true(all(clean_data[[col2]] != "UNKNOWN" &clean_data[[col2]] != "UNKNOWN" & clean_data[[col2]] != "OTHER"))

  expect_true(all(!is.na(clean_data[[col3]])))
  expect_true(all(!is.na(clean_data[[col4]])))

  expect_true(all(sapply(clean_data[[col3]], inherits, "Date")))
  expect_true(all(sapply(clean_data[[col4]], inherits, "Date")))

  covid_19_index <- which(top10_mesh_term[[col5]] == "covid-19")

  covid_19_count <- top10_mesh_term$total_count[covid_19_index]
  expect_equal(covid_19_count, 451)
})

test_that("Create Time Series Vector",{
  matrix_lists <- clean_data_time_series(studies, "country", "United States", "source_class", "UNKNOWN", "INDIV", "OTHER", "start_date", "completion_date", "downcase_mesh_term")
  clean_data <- matrix_lists[[1]]
  mesh_term <- clean_data %>% dplyr::filter(clean_data[["downcase_mesh_term"]] == "diabetes mellitus")%>%
    filter(start_date < as.Date("2023-12-01") & start_date > as.Date("2007-12-31") & completion_date < as.Date("2030-12-01"))
  result<- create_ts(mesh_term)

  expected_counts <- c(4, 6, 19, 14, 36, 18)
  expect_equal(as.vector(result)[1:6], expected_counts)
  expect_equal(frequency(result), 12)
  expect_true(inherits(result, "ts"))
})
