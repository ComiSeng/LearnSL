#' @title Multivariate Linear Regression
#'
#' @description This function calculates and plots the linear regression of a
#' given set of values. Beind all of them independent values but one, that is the dependent value
#'
#' @param data x*y data frame with already classified observations. Each column
#' represents a parameter of the values (independent variable). The last column
#' represents the classification value (dependent variable). Each row is a different observation.
#' @param details boolean value. If it is set to "TRUE" multiple clarifications
#' and explanations are printed along the code
#'
#' @return vector with 2 values (If covariance = 0 the return vector will be empty):
#' * "R^2": Value that represents how well the regression line fits the given
#' values.
#' * "f(x)": Regression line equation.
#'
#' @keywords linear regression, supervised classification, learning, information
#' gain
#'
#' @importFrom graphics abline legend par points
#' @importFrom stats cov var runif
#' @importFrom utils combn
#'
#' @export
multivariate_linear_regression <- function(data, details) {
  par(mfrow = c(1, 1))
  num_columns <- ncol(data)

  cat("Press the 'n' key and then press Enter to continue along the code: \n")
  cat("An empty plot is made with appropiate limits\n")

  plot(1, type = "n", xlim = range(data[, num_columns]),
       ylim = range(data[, 1:(num_columns - 1)]),
       main = "Multivariate Linear Regression",
       xlab = colnames(data)[num_columns],  # Use dependent variable as x-axis label
       ylab = "Variables")  # Use "Variables" as y-axis label
  n_espera(details)

  # Initialize empty vectors for legends
  legend_labels <- character(num_columns - 1)
  legend_colors <- integer(num_columns - 1)
  variable_names <- character(num_columns - 1)  # To store variable names

  dependent_var <- data[, num_columns]
  mean_y <- mean(dependent_var)
  cat("The mean of ",colnames(data)[num_columns]," is", mean_y,"\n")
  # Iterate through each column (except the last one) as the independent variable
  for (i in 1:(num_columns - 1)) {
    independent_var <- data[, i]

    mean_x <- mean(independent_var)
    covar <- cov(independent_var, dependent_var)
    var_x <- var(independent_var)

    cat("The mean of ",colnames(data)[i]," is", mean_x, "covariance is", covar ,"and the variance of x is",var_x,"\n")

    if (covar != 0) {
      # Calculate the slope and intercept
      b <- covar / var(dependent_var)
      a <- mean_x - b * mean_y

      ssr <- sum((a + b * dependent_var - mean_x)^2)
      ssy <- sum((independent_var - mean_y)^2)
      rcua <- ssr / ssy

      # Plot the points and regression line for each column
      points(dependent_var, independent_var, pch = 16, cex = 1, col = i)
      abline(a, b, col = i, lty = i)
      cat("For",colnames(data)[i],"the intercept (a) is", a ,", the slope is", b ,
          ", the sum of squared residuals (ssr) is", ssr ,", the sum of squared deviations of y is",
          ssy ,". They are used to calculate the sum of squared deviations of y (r^2)", rcua,"\n")

      # Store legend labels, colors, and variable names
      legend_labels[i] <- paste(" f(x) =", round(a, 3), "+", round(b, 3), "x")
      legend_colors[i] <- i
      variable_names[i] <- colnames(data)[i]

      legend_text <- paste(variable_names, ": ", legend_labels, sep = "")
      legend("topleft", legend = legend_text, col = legend_colors,
             pch = 1, lty = 1, bty = 'n', xjust = 1, cex = 0.8)
      if (i != num_columns - 1){
        n_espera(details)
      }
    } else {
      cat("Covariance = 0 for column", i, "infinite slope, no line fits the given data.\n")
    }
  }
  par(mfrow = c(1, 1))
}
