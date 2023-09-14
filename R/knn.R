#' @title K-Nearest Neighbors
#'
#' @description This function applies knn algorith to clasify data.
#'
#' @param data Data frame with already classified observations. Each
#' column represents a parameter of the values. The last column contains the
#' output, this means, the expected output when the other column values are
#' inputs. Each row is a different observation.
#' @param p1 Vector containing the parameters of the new value that we want to
#' clasify.
#' @param d_method String with the name of the distance method that will
#' be used. It must be one of \code{"Euclidean"}, \code{"Manhattan"},
#' \code{"Cosine"}, \code{"Chebyshev"}, \code{"Minkowski"}, \code{"Canberra"},
#' \code{"Octile"}, \code{"Hamming"}, \code{"Binary"}or \code{"Jaccard"}. Where
#' both \code{"Hamming"} and \code{"Binary"} use the same method, as it is known
#' by both names.
#' @param k Number of closest values that will be considered in order to clasify
#' the new value ("p1").
#' @param p Exponent used in the \code{Minkowski distance}. 3 by default,
#' otherwise if specified.
#'
#' @return Value of the new classified example.
#'
#' @keywords knn, supervised classification, K-Nearest Neighbors, distance.
#'
#' @importFrom graphics pairs
#' @author Víctor Amador Padilla, \email{victor.amador@@edu.uah.es}
#' @export
knn <- function(data, p1, d_method = "euclidean", k, p = 3) {
  dist <- apply(
    data[, 1:(length(data) - 1)],
    1,
    distance_method,
    p1 = p1,
    d_method = d_method,
    p = p
  )

  neighbors <- sort(dist, index.return = TRUE)$ix[1:k] # k closest values position

  tags <- data[neighbors, length(data)] # class names in k values

  my_string <- "New_Value"
  my_list <- list()
  my_list<- c(my_list, p1)
  my_list <- c(my_list, my_string)
  data <- rbind(data, my_list)

  # Extract the features (columns except the last one, which is the class)
  features <- data[, 1:(length(data) - 1)]
  num_dimensions <- ncol(features)

  # Create a scatterplot matrix with different colors for each class
  colors <- c("red", "blue", "green", "purple", "orange", "cyan", "magenta", "brown", "gray", "pink")
  class_colors <- colors[match(data$ClassLabel, unique(data$ClassLabel))]

  pairs(features, col = class_colors)

  legend("topleft", legend = unique(data$ClassLabel), fill = colors, cex = 0.7, xpd = TRUE, ncol = 1)
  clas <- table(tags)

  prediction <- names(clas)[clas == max(clas)][1] #most repeated class
}

distance_method <- function(p1, p2, d_method = "euclidean", p = 3){
  switch (tolower(d_method),
          "euclidean" = euclidean_d(p1, p2),
          "manhattan" = manhattan_d(p1, p2),
          "chebyshev" = chebyshev_d(p1, p2),
          "minkowski" = minkowski_d(p1, p2, p),
          "canberra"  = canberra_d(p1, p2),
          "octile"    = octile_d(p1, p2),
          "hamming"   = hamming_d(p1, p2),
          "binary"    = hamming_d(p1, p2),
          "jaccard"   = jaccard_d(p1, p2),
          "cosine"    = cosine_d(p1, p2),
          stop("Unknown distance method")
  )
}

euclidean_d <- function(p1, p2 = p1){
  sqrt(sum((p1 - p2)^2))
}

manhattan_d <- function(p1, p2 = p1){
  sum(p1 - p2)
}

chebyshev_d <- function(p1, p2 = p1){
  max(abs(p1 - p2))
}

minkowski_d <- function(p1, p2 = p1, p = 3){
  (sum(abs(p1 - p2)^p)^(1/p))
}

canberra_d <- function(p1, p2 = p1){
  sum(abs(p1 - p2)/(abs(p1) + abs(p2)))
}

octile_d <- function(p1, p2 = p1){
  ((max(abs(p1 - p2))) + ((sqrt(2) - 1) * (min(abs(p1 - p2)))))
}

hamming_d <- function(p1, p2 = p1){
  sum(p1 != p2)
}

jaccard_d <- function(p1, p2 = p1){
  (1 - (length(intersect(p1, p2)) / length(union(p1, p2))))
}

cosine_d <- function(p1, p2 = p1){
  (sum(p1 * p2) / (sqrt(sum(p1^2)) * sqrt(sum(p2^2))))
}
