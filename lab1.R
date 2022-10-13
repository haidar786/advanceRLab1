name <- "Hussnain Haidar"
liuid <- "husha897"

my_num_vector <- function() {
  value1 <- log10(11)
  value2 <- cos(pi/5)
  value3 <- exp(pi/3)
  value4 <- 1173%%7/19
  round(c(value1, value2, value3, value4),5)
}

filter_my_vector <- function(x, leq) {
  replace(x, x >= leq, NA)
}


dot_prod <- function(a,b) {
  sum(a*b)
}


approx_e <- function(N) {
  value <- 0
  for (i in 0:N) {
    value = value + 1 / factorial(i)
  }
  return(round(value, 4))
}


my_magic_matrix <- function(){
  vector <- c(4,9,2,3,5,7,8,1,6)
  mm <- matrix(vector, ncol = 3, nrow = 3, byrow = TRUE)
  return(mm)
}


calculate_elements <- function(A) {
  return(nrow(A) * ncol(A))
}

row_to_zero <- function(A, i) {
  A[i, ] <- 0
  return(A)
}

add_elements_to_matrix <- function(A, x, i, j) {
  newValue <- A[i, j] + x
  A[i, j] <- newValue
  return(A)
}

my_magic_list <- function() {
  element1 <- info <- "my own list"
  element2 <- my_num_vector()
  element3 <- my_magic_matrix()
  magicList <- list(element1,element2, element3)
  return(magicList)
}

change_info <- function(x, text) {
  x$info <- text
  return(x)
}

add_note <- function(x, note) {
  x$note <- note
  return(x)
}

sum_numeric_parts <- function(x) {
  value <- 0
  for (var in x) {
    for (var1 in var) {
      if (is.numeric(var1)) {
        value <- value + var1
      }
    }
  }
  return(round(value, 4))
}

my_data.frame <- function(){
  ids <- c(1,2,3)
  names <- c("John", "Lisa", "Azra")
  incomes <- c(7.30, 0.00, 15.21)
  riches <- c(FALSE, FALSE, TRUE)
  framedData <- data.frame(id = ids ,name = names, income = incomes, rich = riches)
  return(framedData)
}

sort_head <- function(df, var.name, n) {
  df <- df[order(df[var.name], decreasing = TRUE),]
  return(df[1:n,])
}

data(iris)
sort_head(df = iris, var.name = "Petal.Length", n = 5)

add_median_variable <- function(df, j) {
  medianDf <- median(df)
}

