name <- "Hussnain Haidar"
liuid <- "husha897"

my_num_vector <- function() {
  value1 <- log10(11)
  value2 <- cos(pi/5)
  value3 <- exp(pi/3)
  value4 <- 1173%%7/19
  return(c(value1, value2, value3, value4))
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
  return(round(value, digits = N))
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
  element1 <- "my own list"
  element2 <- my_num_vector()
  element3 <- my_magic_matrix()
  magicList <- list(info = element1, element2, element3)
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
  df <- df[order(df[,var.name], decreasing = TRUE),]
  return(df[1:n,])
}

add_median_variable <- function(df, j) {
  med <- median(df[,j])
  compared_to_median <- c()
  for (variable in df[,j]) {
    if (variable == med) {
      compared_to_median <- append(compared_to_median, "Median")
    }else if (variable > med) {
      compared_to_median <- append(compared_to_median, "Greater")
    }else {
      compared_to_median <- append(compared_to_median, "Smaller")
    }
  }
  df$compared_to_median <- compared_to_median
  return(df)
}

data(faithful)
tail(add_median_variable(df = faithful, 2))

analyze_columns <- function(df, j) {
  pData <- function(index) {
    mean <- mean(df[,j[index]])
    med <- median(df[,j[index]])
    sd <- sd(df[,j[index]])
    list <- list(mean = mean, median = med, sd = sd)
    return(unlist(list))
  }

  if(j[1] < j[2]) {
    l <- list(pData(j[1]), pData(j[2]))
    names(l) <- c(colnames(df)[1], colnames(df)[2])
    l$correlation_matrix <- cor(df[c(j[1],j[2])])
    return(l)
  }else {
    l <- list(pData(j[2]), pData(j[1]))
    names(l) <- c(colnames(df)[2], colnames(df)[1])
    l$correlation_matrix <- cor(df[c(j[2],j[1])])
    return(l)
  }
}

# Test cases (arguments)
data("ChickWeight")
df1 <- ChickWeight
j2 <- 1:2

analyze_columns(df = df1, j = j2)





