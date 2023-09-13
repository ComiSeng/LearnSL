#' @title Decision Tree
#'
#' @description This function creates a set of rules needed to classify data,
#' calculating the best classifier possible in each step. Only creates perfect
#' divisions, this means, if the rule doesn't create a classified group, it is
#' not considered.
#'
#' @param data A data frame with already classified observations. Each column
#' represents a parameter of the value. Each row is a different observation.
#' The column names in the parameter "data" must not contain the
#' sequence of characters " or ".
#' As this is supposed to be a binary decision rules generator and not a binary
#' decision tree generator, no tree structures are used, except for the
#' information gain formulas.
#' @param classy Name of the column we want the data to be classified by.
#' the set of rules obtained will be calculated according to this.
#' @param m Maximum numbers of child nodes each node can have.
#' @param method The definition of Gain. It must be one of
#' \code{"Entropy"}, \code{"Gini"}or \code{"Error"}.
#' @return data frame with a list of steps used to classify the data with this
#' columns:
#' Classifier  Value   Classified
#'
#' @details Available gain information methods are:
#'
#' \describe{
#'  * \emph{Entropy}: The formula to calculate the information gain
#'  works as follows: \deqn{p = -\sum{fi p\sub i * \log2 p\sub i}}
#' }
#' @author Víctor Amador Padilla
#' @keywords decision rules, supervised classification, learning, information gain
#'
#' @export
decision_tree <- function (data, classy, m, method = "entropy"){
  tree_strctr <- list(list(0))
  result <- aux_decision_tree(data,classy, m, method, tree_strctr, id = 0, id_f = 0, h = 0)
  result <- result[2:length(result)]
  for (i in 1:length(result)){
    cat("En el nivel", i ,"hay", length(result[[i]]),"hijos, estos se han divido por la variable", result[[i]][[1]][[5]],":\n\n")
    for (j in 1:length(result[[i]])){
      cat("El hijo", result[[i]][[j]][[2]] ,"(con padre", result[[i]][[j]][[3]], ") filtra por \"", result[[i]][[j]][[6]], "\" y contiene los siguientes datos:\n")
      print(result[[i]][[j]][[1]])
      cat("\n")
    }
    cat("\n")
  }
}

aux_decision_tree <- function (data, classy, m, method, tree_strctr, id, id_f, h){
  if (length(unique(data[, classy])) < 2){
    return (tree_strctr)
  }
  #print(data)
  candidates <- mapply(
    function (n, name){
      df <- all_combs(unique(n),m)
      df <- gain_method(df, data, classy, method, name)
      df$classifier <- name
      #print(df)
      m <- which(df$Gain == max(df$Gain))
      indice_fila_max_dashes <- which.min(sapply(df[m, ], function(column) sum(column == "---")))
      max_value <- df[m, ][indice_fila_max_dashes, ]
      columnas_con_dashes <- colnames(max_value)[apply(max_value == "---", 2, all)]
      max_value <- max_value[, !colnames(max_value) %in% columnas_con_dashes]
      candidato <- list(c(max_value[ 1 : (length(max_value) - 2) ]), as.numeric(max_value[length(max_value)-1]), as.character(max_value[length(max_value)]))
    },
    data[, !colnames(data) %in% classy],  #todas las columnas menos classy
    colnames(data)[!colnames(data) %in% classy], #los nombres de las columnas
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )

  candidates <- t(data.frame(candidates))
  colnames(candidates) <- c("Sons", "Gain", "Classifier")
  max_gain <- list(candidates[which.max(candidates[, "Gain"]), ])
  #print(candidates)
  #print(max_gain)

  h <- h + 1
  if (length(tree_strctr)-1 < h){
    tree_strctr <- append(tree_strctr, list(list()))
  }

  name <- max_gain[[1]][[3]]
  for (i in 1:length(max_gain[[1]][[1]])){
    df <- subset(data, get(name) %in% strsplit(as.character(max_gain[[1]][[1]][i]), " ")[[1]])
    rownames(df) <- NULL
    tree_strctr[[1]][[1]]<- tree_strctr[[1]][[1]]+1
    tree_strctr[[h+1]] <- append(tree_strctr[[h+1]], list(list(df, tree_strctr[[1]][[1]], id_f, h, max_gain[[1]][[3]],max_gain[[1]][[1]][[i]],  max_gain[[1]][[2]])))

    tree_strctr <- aux_decision_tree(df, classy, m, method, tree_strctr, tree_strctr[[1]][[1]], tree_strctr[[1]][[1]], h)
  }
  return (tree_strctr)
}

gain_method <- function(da, data, classy, method, name){
  if (is.character(da)){
    df <- as.data.frame(matrix(da, nrow = 1, ncol = 1))
    df$Gain <- 0
    return (df)
  }
  da$Gain <- apply(
    da,
    1,
    function(n, data, classy){
      n <- as.vector(n[n != "---"])
      switch (tolower(method),
              "entropy" = entropy(n, data, classy, name),
              "gini"    = gini(n, data, classy, name),
              "error"   = error(n, data, classy, name),
              stop("Unknown method")
      )
    },
    data = data,
    classy = classy,
    simplify = TRUE
  )
  return(da)
}

entropy <- function(n, data, classy, name){
  ent_hijos <- vector(mode="integer", length = length(n))
  entp <- 0
  valores <- table(data[,classy])
  for (i in 1:length(valores)){
    entp <- entp - (valores[i]/sum(valores)* log2(valores[i]/sum(valores)))
  }
  ganancia = entp[[1]]
  for (i in 1:length(n)){
    subset_data <- table(data[data[[name]] %in% unlist(strsplit(n[i], " ")), ][,classy])
    for (j in subset_data){
      ent_hijos[i] <- ent_hijos[i] - (j/sum(subset_data)* log2(j/sum(subset_data)))
    }
    ganancia = ganancia - ((sum(subset_data)/sum(valores)) * ent_hijos[i])
  }
  return(ganancia)
}

gini <- function(n, data, classy, name){
  gin_hijos <- rep(1, length(n))
  ginp <- 1
  valores <- table(data[,classy])
  for (i in 1:length(valores)){
    ginp <- ginp - (valores[i]/sum(valores) * (valores[i]/sum(valores)))
  }
  ganancia = ginp[[1]]
  for (i in 1:length(n)){
    subset_data <- table(data[data[[name]] %in% unlist(strsplit(n[i], " ")), ][,classy])
    for (j in subset_data){
      gin_hijos[i] <- gin_hijos[i] - ((j/sum(subset_data)) * (j/sum(subset_data)))
    }
    ganancia = ganancia - ((sum(subset_data)/sum(valores)) * gin_hijos[i])
  }
  return(ganancia)
}

error <- function(n, data, classy, name){
  err_hijos <- rep(1, length(n))
  errp <- 1
  valores <- table(data[,classy])
  for (i in 1:length(valores)){
    errp <- errp - (valores[i]/sum(valores) * (valores[i]/sum(valores)))
  }
  ganancia = errp[[1]]
  for (i in 1:length(n)){
    subset_data <- table(data[data[[name]] %in% unlist(strsplit(n[i], " ")), ][,classy])
    err_hijos[i] <- max(subset_data/sum(subset_data))
    ganancia = ganancia - ((sum(subset_data)/sum(valores)) * err_hijos[i])
  }
  return(ganancia)
}


#'@title comb
#'@description Performes all possible combinations.
#'
#'@param v a
#'@param vp b
#'@param k c
#'@param combinations d
#'
#'@return Value of the new classified example.
#'@importFrom utils combn
#'@keywords knn, supervised classification, K-Nearest Neighbors, distance.
comb <- function(v, vp, k, combinations){
  if (k > 1){
    if (length(vp) == 0){
      v <- c(v,"---")
      comb(v,vp,k-1, combinations)
    }
    else if(length(vp) == 1){
      v <- c(v, paste(vp, collapse = " "))
      comb(v,c(),k-1, combinations)
    }
    else {
      vec <- c(v, paste(vp, collapse = " "), rep(c("---"), each = k-1))
      combinations <- rbind (combinations, vec)
      combi <- vector("list", length(vp))
      l <- ifelse(length(vp)%%2 == 0, length(vp)/2, length(vp)/2 - 0.5)

      for (i in 1:l) {
        temp <- combn(vp, i, paste, collapse = " ")
        if (length(vp)%%2 == 0 && nchar(temp[1]) == l*2 - 1){
          half <- split(temp, f = ifelse(seq_along(temp) <= length(temp)/2, "first", "second"))
          temp <- half$first
        }
        combi[[i]] <- temp
      }
      df <- unlist(combi)
      for (i in 1:length(df)){
        n <- strsplit(df[i], " ")[[1]]
        new_data <- setdiff(vp, n)
        combinations <- comb(c(v,paste(n, collapse = " ")),new_data, k-1, combinations)
      }
      return (combinations)
    }
  }
  else if (k == 1){
    ifelse(length(vp) > 0, v <- c(v, paste(vp, collapse = " ")) , v <- c(v,"---"))
    combinations <- rbind(combinations, v)
    return(combinations)
  }
}

all_combs <- function(vp, k){
  v = c()
  if (k > length(vp)){
    k = length(vp)
  }
  combinations<- data.frame()
  combinations <- comb(v, vp, k, combinations)
  combinations <- apply(combinations, 1, sort, simplify = TRUE)
  combinations <- data.frame(t(combinations))
  combinations <- combinations[!duplicated(combinations), ]
  rownames(combinations) <- NULL

  return(combinations)
}
