#' @title Multivariate Polynomial Regression
#'
#' @description This function calculates and plots the linear regression of a
#' given set of values. Being all of them independent values but one, that is the dependent value
#'
#' @param data x*y data frame with already classified observations. Each column
#' represents a parameter of the values (independent variable). The last column
#' represents the classification value (dependent variable). Each row is a different observation.
#' @param degree Degree of the equations approximation.
#' @param details Boolean value. If it is set to "TRUE" multiple clarifications
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
#' @importFrom stats coef lm predict
#' @importFrom graphics lines
#'
#' @export
polynomial_regression <- function(data, degree, details) {
  par(mfrow = c(1, 1))

  num_columns <- ncol(data)

  # Create an empty plot
  cat("Press the 'n' key and then press Enter to continue along the code: \n")
  cat("An empty plot is made with appropiate limits\n")

  plot(1, type = "n", xlim = range(data[, num_columns]),
       ylim = range(data[, 1:(num_columns - 1)]),
       main = "Polynomial Regression",
       xlab = colnames(data)[num_columns],
       ylab = "Variables")
  n_espera(details)

  # Initialize empty vectors for legends
  legend_labels <- character((num_columns - 1))
  legend_colors <- integer((num_columns - 1))

  cat("The aproximations of the following equations to the provided values are done adjusting the coefficients of the line to make it the best-fit possible.\n")
  # Iterate through each column (except the last one) as the independent variable
  for (i in 1:(num_columns - 1)) {
    independent_var <- data[, i]
    dependent_var <- data[, num_columns]

    # Fit polynomial regression
    poly_fit <- lm(independent_var ~ poly(dependent_var, degree, raw = TRUE))

    # Generate points for the regression line
    y_range <- range(dependent_var)
    y_pred <- seq(y_range[1], y_range[2], length.out = 100)
    x_pred <- predict(poly_fit, newdata = data.frame(dependent_var = y_pred))

    points(dependent_var, independent_var, pch = 16, cex = 1, col = i)

    # Plot the regression line
    lines(y_pred, x_pred, col = i, lty = i)

    # Extract coefficients of the polynomial
    poly_coefs <- coef(poly_fit)

    # Create legend labels with the full equation
    equation <- paste(
      "f(x) =", round(poly_coefs[1], 3),
      ifelse(poly_coefs[2] >= 0, "+", "-"), abs(round(poly_coefs[2], 3)), "x")

    for (d in 3:(degree + 1)) {
      equation <- paste(equation, ifelse(poly_coefs[d] >= 0, "+", "-"), abs(round(poly_coefs[d], 3)), "x^", (d - 1), sep = "")
    }

    cat("The Equation ( degree",degree,") aproximation to the",colnames(data)[i],"values is:",equation,
        "\n")

    legend_labels[i] <- paste(colnames(data)[i], ":", equation)
    legend_colors[i] <- i

    # Create the legend
    legend("topleft", legend = legend_labels, col = legend_colors,
           pch = 1, lty = 1, bty = 'n', xjust = 1, cex = 0.8)
    if (i != num_columns - 1){
      n_espera(details)
    }
  }
  par(mfrow = c(1, 1))
}
