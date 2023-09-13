#' @title Perceptron
#'
#' @description Applies k-nn algorithm to classify data.
#'
#' @param training_data hola
#' @param to_clasify hola
#' @param activation_method hola
#' @param max_iter hola
#' @param learning_rate hola
#'
#' @return mete los ejemplos perrrrrooooooo @@examples
#' @keywords hola
#' @author Víctor Amador Padilla, \email{vamadorpadilla@@edu.uah.es}
#' @export
perceptron <- function(training_data, to_clasify, activation_method, max_iter, learning_rate){
  weigths <- per_training( training_data, activation_method, max_iter, learning_rate )
  clasificacion <- as.numeric(act_method(activation_method,sum(weigths * to_clasify)) > 0.5)
  cat("Classification of the new value:", clasificacion, "\n")
}

#' @importFrom stats runif
per_training <- function(training_data, activation_method, max_iter, learning_rate){
  weigths <- runif(ncol(training_data)-1, min = -1, max = 1)
  is_correct <- FALSE
  sapply(
    1:max_iter,
    function(a){
      if (!is_correct){# If every element is classified, we are done
        is_correct <- TRUE
        # Verify if every value is correctly classified
        apply(
          training_data,
          1,
          function(b){
            if (is_correct){
              inputs <- b[1:length(b)-1]
              expected_output <- b[length(b)]
              output <- act_method(activation_method,sum(weigths * inputs))
              if (as.numeric(output > 0.5) != expected_output) {is_correct <- FALSE}
            }
          }
        )
        if (!is_correct){
          # select a random value from training_data
          row_num <- sample(1:nrow(training_data), 1)
          inputs <- training_data[row_num, 1:ncol(training_data)-1]
          expected_output <- training_data[row_num, ncol(training_data)]

          # calculate output and update weights
          output <- act_method(activation_method,sum(weigths * inputs))
          error <- expected_output - output
          weigths <- weigths + learning_rate * error * inputs
        }
      }
    }
  )
  return(weigths)
}

#'@title act_method
#'
#'@description Sets the activation method the user wants to use.
#'
#'@param method String with the name of the activation method that will
#'be used. It must be one of \code{"step"}, \code{"sine"},
#'\code{"tangent"}, \code{"linear"}, \code{"relu"}, \code{"gelu"} or
#'\code{"swish"}. Anything else will raise an error.
#'@param x numeric on which the activation function will be applied. Typically, the sum of weighted weights.
#'
#'@return Value after applying the activation method.
#'
#'@keywords internal
#'
act_method <- function(method, x){
  switch (tolower(method),
          "step"     = as.numeric(x > 0.5),
          "sine"     = (exp(x) - exp(-x)) / 2,
          "tangent"  = (exp(x) - exp(-x)) / (exp(x) + exp(-x)),
          "linear"   = x,
          "relu"     = pmax(x, 0),
          "gelu"     = 0.5 * x * (1 + tanh(sqrt(2 / pi) * (x + 0.044715 * x^3))),
          "swish"    = x / (1 + exp(-x)),
          stop("Unknown activation method")
  )
}
