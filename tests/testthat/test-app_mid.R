test_that("shiny_app works", {
  result = shiny_app()
  expect_true(class(result) == "shiny.appobj")
})

