name <- "Hussnain Haidar"
liuid <- "husha897"

my_num_vector <- function() {
  value1 <- log10(11)
  value2 <- cos(pi/5)
  value3 <- exp(pi/3)
  value4 <- 1173%%7/19
  c(formatC(value1, digits = 5, format = "f"),
    formatC(value2, digits = 5, format = "f"),
    formatC(value3, digits = 5, format = "f"),
    formatC(value4, digits = 5, format = "f"))
}

filter_my_vector <- function(x, leq) {
  replace(x, x>=leq, NA)
}

dot_prod <- function(a,b) {
  sum(a*b)
}

approx_e <- function(n) {

}

my_magic_matrix <- function(){
  mm <- matrix(c(4,9,2,3,5,7,8,1,6), nrow = 3, byrow = TRUE)
}

calculate_elements <- function(A) {
  length(A)
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
  x[["info"]] <- text
  return(x)
}

add_note <- function(x, note) {
  x[["note"]] <- note
  return(x)
}

sum_numeric_parts <- function(x) {

}

my_data.frame <- function(){
  ids <- c(1,2,3)
  names <- c("John", "Lisa", "Azra")
  incomes <- c(7.30, 0.00, 15.21)
  riches <- c(FALSE, FALSE, TRUE)
  framedData <- data.frame(id = ids ,name = names, income = incomes, rich = riches)
  return(framedData)
}

add_median_variable <- function(df, j) {
  medianDf <- median(df)
}

