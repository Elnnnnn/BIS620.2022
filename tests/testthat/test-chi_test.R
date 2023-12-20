library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)

test_that("Clean the data", {
  data("studies")

  result <- clean_data(studies, "downcase_mesh_term", "source_class")

  # Check if unnecessary columns are removed
  expect_false("url" %in% names(result$data))
  expect_false("keyword" %in% names(result$data))
  expect_false("conditions" %in% names(result$data))
  expect_false("brief_title" %in% names(result$data))

  # Check if data is separated and grouped correctly
  expect_true("downcase_mesh_term" %in% names(result$data))
  expect_true(all(c("diabetes mellitus", "carcinoma", "breast neoplasms", "covid-19") %in% result$x_top_100))
})


test_that("normalization constructs matrix correctly", {
  data("studies")
  result_list <- clean_data(studies, "downcase_mesh_term", "source_class")
  # Extract the results
  studies <- result_list$data
  stored_top_100 <- result_list$x_top_100
  unique_source_class <- result_list$unique_y
  result <- normalization(studies, "downcase_mesh_term", "source_class",stored_top_100, unique_source_class)

  count_matrix <- result[[1]]
  expect_true(is.matrix(count_matrix))
  expect_equal(dim(count_matrix), c(100, 8))
  expect_equal(count_matrix["carcinoma", "OTHER"], 8390)
  expect_equal(count_matrix["infections", "NIH"], 1160)

  normalization_matrix <- result[[2]]

  # Check if the sums of columns in the normalized matrix are 1
  expect_true(length(colSums(normalization_matrix)) == 8)
  expect_equal(normalization_matrix["hypertension", "OTHER"], 0.015261091)
})

test_that("chi_square_matrix stops with invalid inputs", {
  # Assuming 'normalization_matrix' needs to be a numeric matrix
  expect_error(chi_square_matrix(matrix("not numeric", nrow = 2, ncol = 2)))
})

test_that("chi_square_matrix constructs matrix correctly", {
  # Create a simple test matrix
  data("studies")
  result_list <- clean_data(studies, "downcase_mesh_term", "source_class")
  studies <- result_list$data
  stored_top_100 <- result_list$x_top_100
  unique_source_class <- result_list$unique_y
  matrix_list <- normalization(studies, "downcase_mesh_term", "source_class",stored_top_100, unique_source_class)
  count_matrix <- matrix_list[[1]]
  result <- chi_square_matrix(count_matrix)

  # The result should be a square matrix with dimensions equal to the number of columns in the input matrix
  expect_true(is.matrix(result))
  expect_equal(dim(result), c(ncol(count_matrix), ncol(count_matrix)))
})

test_that("line_chart_comparison stops with invalid column name", {
  # Create a simple test matrix
  test_matrix <- matrix(c(0.2, 0.5, 0.3, 0.4), nrow = 2)
  colnames(test_matrix) <- c("A", "B")
  rownames(test_matrix) <- c("term1", "term2")

  # Check for error with invalid column name
  expect_error(line_chart_comparison(test_matrix, "C", "term", "VarName", "ValueName"))
})

test_that("line_chart_comparison transforms data correctly", {
  data("studies")
  result_list <- clean_data(studies, "downcase_mesh_term", "source_class")
  studies <- result_list$data
  stored_top_100 <- result_list$x_top_100
  unique_source_class <- result_list$unique_y
  matrix_list <- normalization(studies, "downcase_mesh_term", "source_class",stored_top_100, unique_source_class)
  normalization_matrix <- matrix_list[[2]]
  count_matrix <- matrix_list[[1]]
  p = line_chart_comparison(normalization_matrix, count_matrix, "INDUSTRY", "sponsor", "percentage")
  expect_true(is.ggplot(p))
})
