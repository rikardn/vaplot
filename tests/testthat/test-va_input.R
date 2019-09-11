test_that("va_input can be created", {
  input <- va_input(column_names = c("TIME", "ID"),
                    theta = c(THETA1 = 12, THETA2 = 1),
                    omega = diag(0.1, 2, 2),
                    sigma = matrix(1, 1, 1),
                    derivative_data = list(),
                    input_file = "file.lst")
  expect_is(input, "va_input")
})


test_that("va_input can be printed", {
  input <- va_input(column_names = c("TIME", "ID"),
                    theta = c(THETA1 = 12, THETA2 = 1),
                    omega = diag(0.1, 2, 2),
                    sigma = matrix(1, 1, 1),
                    derivative_data = list(),
                    input_file = "file.lst")
  expect_output(print(input))
})

test_that("va_input can be summarized", {
  input <- va_input(column_names = c("TIME", "ID"),
                    theta = c(THETA1 = 12, THETA2 = 1),
                    omega = diag(0.1, 2, 2),
                    sigma = matrix(1, 1, 1),
                    derivative_data = list(),
                    input_file = "file.lst")
  expect_output(summary(input))
})
