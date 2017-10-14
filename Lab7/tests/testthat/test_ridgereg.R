context("ridgereg")



data("iris")



Polygon <- setRefClass("Polygon", fields = c("sides"))

square <- Polygon$new(sides = 4)

test_that("ridgereg rejects errounous input", {
  
  expect_error(ridgereg_mod <- ridgereg$new(formula = Petal.Length~Sepdsal.Width+Sepal.Length, data=iris, lambda = 1))
  
  expect_error(ridgereg_mod <- lridgereg$new(formula = Petal.Length~Sepdsal.Width+Sepal.Length, data=irfsfdis, lambda = 0))
  
})





test_that("class is correct", {
  
  ridgereg_mod <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 1)
  
  
  
  expect_true(class(ridgereg_mod)[1] == "ridgereg")
  
})



test_that("coef() method works", {
  
  lmr <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 2)
  
  lm_output <- lm.ridge(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 2)
  
  lm_coef <- round(as.vector(lm_output$coef),2)
  
  lmr_coef <- round(as.vector(lmr$coef()),2)
  
  is_equivalent_to(lm_coef,lmr_coef)
  
})



lmr_test <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = c(3,2))



test_that("print() method works", {
  
  expect_output(print_output <-lmr_test$print())
  
})



test_that("predict() method works", {
  
  expect_output(lmr_test_output <-lmr_test$predict())
  
})

