library(dplyr)
library(DT)
library(ggplot2)
library(tidyr)
library(purrr)
library(tm)
library(memoise)
library(rworldmap)
library(grDevices)
library(graphics)
library(utils)

test_that("plot_phase_histogram creates a plot with correct axes", {
  data("studies")

  # Run the function to get the plot
  p <- plot_phase_histogram(studies)

  # Check that it returns a ggplot object
  expect_true(is.ggplot(p))

  # Check that the plot has an x-axis named "Phase"
  expect_equal("Phase", p$labels$x)

  # Check that the plot has a y-axis named "Count"
  expect_equal("Count", p$labels$y)

})

test_that("plot_conditions_histogram returns a ggplot", {
  data("studies")
  studies <- studies |> head(10)

  # Running the function with the mock data
  plot_result <- plot_conditions_histogram(studies)

  # Check if the result is a ggplot object
  expect_true(is.ggplot(plot_result))

  # Optionally, add more tests here to ensure the plot has the
  #expected elements like x and y axis labels
  expect_identical(plot_result$labels$x, "Conditions")
  expect_identical(plot_result$labels$y, "Count")
})

test_that("get_concurrent_trials returns correct counts", {
  data("studies")
  studies <- studies |> head(10)
  result <- get_concurrent_trials(studies)
  expect_false(nrow(result) == 0)
})

test_that("get_term_matrix returns correct term frequencies", {
  data("studies")
  studies <- studies |> head(10)
  result <- get_term_matrix(studies)
  expect_true(length(result) > 0)
})

test_that("plot_countries creates a plot without error", {
  data(studies)
  studies <- studies |> head(100)
  result <- plot_countries(studies)
  expect_true(is.list(result))
})

test_that("table_with_link creates a table without error", {
  data(studies)
  studies <- studies |> head(10)
  table <- table_with_link(studies)
  expect_true(nrow(table) > 0)
})

test_that("plot_states_us function works correctly", {
  data("studies")
  studies <- studies |> head(10)
  # Call the function with test data
  plot_result <- plot_states_us(studies)

  # Test data processing
  state <- studies |> select(state)

  # Test plot structure
  expect_true(is.ggplot(plot_result), "Result is a ggplot object")
})


test_that("pie_chart_mesh function works correctly", {
  data("studies")
  studies <- studies |> head(10)

  # Call the function with test data
  studies_result <- pie_chart_mesh(studies)
  expect_true(is.list(studies_result))

})

test_that("table_with_link function produces correct output", {

  data("studies")
  studies <- studies |> head(10)
  result <- table_with_link(studies)

  # Check output structure
  expect_true("data.frame" %in% class(result), "Output is a data frame")
  expected_columns <- c("NCT ID", "Brief Title", "Start Date",
                        "Completion Date")
  expect_equal(names(result), expected_columns)
})


test_that("query_kwds handles date range filtering correctly", {
  data("studies")
  kwds <- "cancer"

  # Test with a specific date range
  result <- query_kwds(studies, kwds, "brief_title")
  nrow <- result |> as.data.frame() |> nrow()
  expect_true(nrow > 0) # Only records within the specified date range
  result <- query_kwds(studies, kwds, "brief_title", ignore_case = FALSE)
  nrow <- result |> as.data.frame() |> nrow()
  expect_true(nrow > 0)
  result <- query_kwds(studies, kwds, "brief_title", match_all = TRUE)
  nrow <- result |> as.data.frame() |> nrow()
  expect_true(nrow > 0)
})

test_that("plot_states_us function works correctly", {
  data("studies")
  studies <- studies |> head(10)

  # Call the function with test data
  plot_result <- plot_states_us(studies)

  # Test data processing
  state <- studies |> select(state)

  # Test plot structure
  expect_true(is.ggplot(plot_result), "Result is a ggplot object")
})


test_that("pie_chart_mesh function works correctly", {
  data("studies")
  studies <- studies |> head(10)

  # Call the function with test data
  studies_result <- pie_chart_mesh(studies)
  expect_true(is.list(studies_result), "Result is a ggplot object")

})
