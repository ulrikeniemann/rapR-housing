# ..............................................................................
#
# Chapter 6
# functional programming

ls()

data(mtcars)

a <- 1

ls()

# ..............................................................................
f <- function(name){
  print(paste0(name, " likes lasagna"))
}
f("bruno")

ls()

g <- function(name){
  food <- sample(c("lasagna", "cassoulet", "feijoada"), 1)
  print(paste0(name, " likes ", food))
}
g("Bruno")
g("Bruno")

h <- function(name){
  food <- sample(c("lasagna", "cassoulet", "feijoada"), 1)
  if(exists("food_list")){
    food_list <<- append(food_list, food) # <<- for global environment
  } else {
    food_list <<- append(list(), food)
  }
  print(paste0(name, " likes ", food))
}
ls()
h("Bruno")
ls()
food_list
h("Bruno")
food_list
h()

# better
h <- function(name, food_list = list()){
  food <- sample(c("lasagna", "cassoulet", "feijoada"), 1)
  food_list <- append(food_list, food) # <- only inside function environment
  print(paste0(name, " likes ", food))
  food_list
}
h("uli")
food_list <- h("Bruno", food_list)
food_list

# one more
h <- function(name, food_list = list(), seed = 123){
  # set.seed making sure get the same result for a given seed
  set.seed(seed)
  food <- sample(c("lasagna", "cassoulet", "feijoada"), 1)
  # we unset the seed, else it will stay set for the whole session!
  set.seed(NULL)
  food_list <- append(food_list, food) 
  print(paste0(name, " likes ", food))
  food_list
}
h("Bruno")
h("Bruno")
h("Bruno")

# ..............................................................................
# referentially transparent functions:
# function that does not use any variable that is not also one of its inputs

# bad expample
bad <- function(x){
  x + y
}
y <- 10
bad(5)
y <- 45
bad(5)
# good version
good <- function(x, y){
  x + y
}

# ..............................................................................
# functions can have functions as input
h <- function(number, f){
  f(number)
}
h(4, sqrt)
h(10, log10)

# ..............................................................................
# use placeholder for further inputs
h <- function(number, f, ...){
  f(number, ...)
}
h(c(1, 2, NA, 3), mean)
h(c(1, 2, NA, 3), mean, na.rm = TRUE)

# use numbered arguments
w <- function(...){
  paste0("first argument: ", ..1,
         ", second argument: ", ..2,
         ", last argument: ", ..3)
}
w(1)
w(1, 2)
w(1, 2, 3)

?dots

# ..............................................................................
# functions that return functions
sqrt(-5)
# redefine to raise an error
strict_sqrt <- function(x){
  if(x < 0) stop("x is negative")
  sqrt(x)
}
strict_sqrt(-5)

# define a function that takes a function as an argument 
# and convert any warning into an error 
# and return a new function
strictly <- function(f){
  function(...){
    tryCatch({
      f(...)
    },
    warning = function(warning) stop("Can't do that chief"))
  }
}
s_sqrt <- strictly(sqrt)
s_sqrt(-4) # raise an error
sqrt(-4) # original warning
s_log <- strictly(log)
s_log(-4)

# functions that return functions are called function factories
# check out maybe package!
s_sqrt <- chronicler::record(sqrt)
result <- s_sqrt(-4)
result
chronicler::read_log(result)
# check also purrr package
library(purrr)
?purrr::possibly # return a value instead of an error
possible_sqrt <- purrr::possibly(sqrt, otherwise = "otherwise")
numbers_with_error <- list(1, 2, 3, "spam", 4)
map(numbers_with_error, possible_sqrt)

?purrr::safely
?purrr::quietly

# ..............................................................................
# optinal arguments
g <- function(x, y = NULL){
  if(is.null(y)){
    print("optional argument y is NULL")
    x
  } else {
    if(y == 5) print("y is present") # only printed for y=5
    x+y # for every y
  }
}
g(10)
g(10, 5)
g(10, 7)

# ..............................................................................
# safe functions
# this function is not safe:
nchar("10000000") # 8
nchar(10000000)   # 5
10000000          # 1e+07

nchar2 <- function(x, result = 0) {
  if(!isTRUE(is.character(x))){
    stop(paste0("x should be of type character, but is type ",
                typeof(x), " instead."))
  } else if(x == ""){
    result
  } else {
    result <- result + 1
    split_x <- strsplit(x, split = "")[[1]]
    #print(split_x)
    nchar2(paste0(split_x[-1], collapse = ""), result)
  }
}
nchar2(10000000)
nchar2("")
nchar2("test")
split_x <- strsplit("test", split = "")[[1]]
split_x

# ..............................................................................
# recursive function: calls itselfs!
# sometimes its easier to use recursive than iterative functions

# example: factorial function
fact_iter <- function(n){
  result = 1
  for(i in 1:n){
    result = result * i
    #print(paste("i =", i, "result =", result))
    i = i + 1
  }
  result
}
fact_iter(3)
fact_recur <- function(n){
  if(n == 0 || n == 1){
    result = 1
  } else {
    #print(paste("n =", n))
    n * fact_recur(n-1)
  }
}
fact_recur(3)
# benchmark:
microbenchmark::microbenchmark(
  fact_recur(50),
  fact_iter(50)
)
# recursive is much slower! use better iterative functions

# ..............................................................................
# anonyme functions
(function(x)(x+1))(10)
# since R 4.1
(\(x)(x+1))(10)

# unix philosophy
# do one thing and do it well
# write simple functions that only perform one task
# split big functions into smaller ones
# pipe the smaller functions: a |> f() |> g() |> h()
# better to handle, easier to maintain, test, document and debug


