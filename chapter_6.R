# ..............................................................................
#
# Chapter 6
# functional programming

ls()

data(mtcars)

a <- 1

ls()

# ..............................................................................
# simple functions
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
      f(...) # return value of function
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

# ..............................................................................
# lists

data(mtcars)
typeof(mtcars)

library(ggplot2)
my_plot <- ggplot(data = mtcars) + 
  geom_line(aes(y = hp, x = mpg))
my_plot
typeof(my_plot)

# dataframe, fitted models, ggplots are lists

# functions that returns many objects, only solution is to place them in a list

# function returns square root of a number using Newtons algorithm
# as well as the number of steps or iterations took to reach the solution
sqrt_newton <- function(a, init = 1, eps = 0.01, steps = 1){
  stopifnot(a >= 0)
  while(abs(init**2 - a) > eps){
    init <- 1/2 * (init + a/init)
    steps <- steps + 1
  }
  list(
    "result" = init,
    "steps" = steps
  )
}
result_list <- sqrt_newton(-1)
result_list <- sqrt_newton(1600)
result_list

# if needed separate objects
result <- result_list$result
result_steps <- result_list$steps

# lists can hold objects of different types
list(
  "a" = head(mtcars),
  "b" = ~lm(y ~ x)   # formula object
)

# lists can hold other lists
list(
  "a" = head(mtcars),
  "b" = list(
    "c" = sqrt,
    "d" = my_plot
  )
)

# ..............................................................................
# lists are the cure to loops

# compute sum of the first 100 integers
result <- 0
for (i in 1:100){
  result <- result + i
}
print(result)

# avoid loops - write a function
looping <- function(a_list, a_func, init = NULL, ...){
  # if user dont provide init value set head of list as initial value
  if(is.null(init)){
    init <- a_list[[1]]
    a_list <- tail(a_list, - 1)
  }
  # separeate head from tail of list and apply function
  # to initial value and head of list
  head_list <- a_list[[1]]
  tail_list <- tail(a_list, -1)
  init <- a_func(init, head_list, ...)
  # check if we're done: if there is still some tail,
  # return whole thing if there is no tail left
  if(length(tail_list) != 0){
    looping(tail_list, a_func, init, ...)
  }
  else {
    init
  }
}
looping(as.list(seq(1, 100)), `+`)

# looping() is actually ships with R and its called Reduce()
# Reduce uses a binary function to successively combine the elements 
# of a given vector and a possibly given initial value. 
Reduce(`+`, seq(1:100))

# loop that applies a function to each element of a list instead of whole list
result <- as.list(seq(1, 5))
for (i in seq_along(result)){
  result[[i]] <- sqrt(result[[i]])
}
print(result)

# lets abstract this process away in a function
applying <- function(a_list, a_func, ...){
  head_list <- a_list[[1]]
  tail_list <- tail(a_list, -1)
  result <- a_func(head_list, ...)
  # check if we are done - rerun until no tail left
  if(length(tail_list) != 0){
    append(result, applying(tail_list, a_func, ...))
  }
  else {
    result
  }
}
applying(as.list(seq(1, 5)), sqrt)

# R's implementation of the function
# lapply returns a list of the same length as X, each element of which 
# is the result of applying FUN to the corresponding element of X.
lapply(list(seq(1, 5)), sqrt)

# in other languages lapply is often called map()
# in purrr package there is also map2()
library(purrr)
# map(): The map functions transform their input by applying a function to 
# each element of a list or atomic vector and returning an object of the same length as the input.
map(list(seq(1, 5)), sqrt)
# The base equivalent to map() is lapply()

# map2(): These functions are variants of map() that iterate over two arguments at a time.
map2(
  .x = seq(1, 5),
  .y = seq(1, 5),
  .f = `+`
)

# for more than two lists: pmap()

# you can use matrix multiplication instead of loops
# sum of the first 100 integers - %*% is matrix multiplication
rep(1, 100) %*% seq(1, 100)

# many functions are vectorized by default - no loop is required
# check if functions are vectorized before using loops!
sqrt(seq(1, 5))
seq(1, 5) + seq(1, 5)

# ..............................................................................
# data frames
# are a special type of lists
# so lapply works on colums of a dataframe
lapply(iris, class)

# i want to generate several plots
# use dplyr, purrr, ggplot2, tidyr

# create temporary file
unemp_path <- tempfile(fileext = ".rda")

# downlad data and save to temp path
download.file("https://is.gd/l57cNX", destfile = unemp_path)
# load data
load(unemp_path)

# letztlich per Hand gedownloadet - irgendwas funktioniert sonst nicht wie es soll
# download.file("https://is.gd/l57cNX", destfile = "./data/unemp.rda")
# download.file("https://is.gd/l57cNX", destfile = "unemp.rda")
# load(file = 'unemp.rda')
load("./data/unemp.rda")

library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)

glimpse(unemp)

# i want a separate plot for the three communes of luxembourg
# lets start with filtering
filtered_umemp <- unemp |> 
  filter(
    level == "Commune",
    place_name %in% c("Luxembourg", "Esch-sur-Alzette", "Wiltz")
  )
glimpse(filtered_umemp)

# dataframes are lists!
# dplyr::group_nest(): groups dataframe by variable and then nests the other cols
nested_unempl <- filtered_umemp |> 
  group_nest(place_name)
# 1st col: communes, 2nd: data: every other variable inside a smaller dataframe
# such a col is called a list-column - a list of lists
nested_unempl

# simple example of map(): no of rows for each dataframe
# nrows is a list of integers
nested_unempl |> mutate(nrows = map(data, nrow))

# converting it to an atomic vector of intgers by using map_int() instead of map()
nested_unempl |> mutate(nrows = map_int(data, nrow))

# complex example: filter rows, use anonymus function (because argument year is fixed)
nested_unempl |> mutate(nrows = map(data, \(x)filter(x, year == 2015)))

# plot data

# select data for Luxembourg
lux_data <- nested_unempl |> 
  filter(place_name == "Luxembourg") |> 
  unnest(data)
# plot
ggplot(data = lux_data)+
  theme_minimal() +
  geom_line(aes(year, unemployment_rate_in_percent, group = 1)) +
    labs(title = "Umemployment in Luxembourg")

# turn it into function with arguments
make_plot <- function(x, y){
  ggplot(data = x)+
    theme_minimal() +
    geom_line(aes(year, unemployment_rate_in_percent, group = 1)) +
    labs(title = paste("Umemployment in", y))
}
# apply this function to our nested dataframe
nested_unempl <- nested_unempl |> 
  mutate(plots = map2(
    .x = data,    # column of data frame
    .y = place_name, # column of commune name
    .f = make_plot
  ))
# plots are now a column of lists of ggplots
nested_unempl
nested_unempl$plots

# we also could have used an anonymus function
# (but its more difficult to get right)
nested_unempl |> 
  mutate(plots2 = map2(
    .x = data,     
    .y = place_name, 
    .f = \(.x, .y) {
      ggplot(data = .x)+
        theme_minimal() +
        geom_line(aes(year, unemployment_rate_in_percent, group = 1)) +
        labs(title = paste("Umemployment in", .y))
    }  
  )) |> 
  pull(plots2)

# this was list-column-based workflow.. extremly powerful
# need to avoid copy and pasting at all costs

# ..............................................................................
# ..............................................................................
# functional programming in R
Map()
# base
# Reduce(), Map(), lapply(), apply(), sapply(), vapply(), mapply(), tapply()
?lapply

# Filter() is similar to dyplr::filter(), but focussing on lists
Filter(is.character, list(seq(1, 5), "Hey"))
# only returns elements of the lists which is.character == TRUE

# Negate() takes boolean as input and return the opposite boolean
Filter(Negate(is.character), list(seq(1, 5), "Hey"))

# locate() runs code in an temporary environment that gets discards at the end
local({a <- 2})
exists("a") # a doesnt exists

# print() ... many methods for different objects
# R´s OOP capabilities.. object-oriented programming (OOP)

# purrr
library(purrr)
# map() family of functions
map(seq(1, 5), sqrt)
# variante: map_dbl, map_.. return an atomic vector of the indicated type
map_dbl(seq(1, 5), sqrt)
map_chr(letters, toupper)

# or walk() for functions without return value (i.e. downloading files)
# purrr::reduce(), and many other functions in purrr...

# withr
library(withr)
# makes it easy to purify functions

# example from start of chapter:
h <- function(name, food_list = list()){
  food <- sample(c("lasagna", "cassoulet", "feijoada"), 1)
  food_list <- append(food_list, food) # <- only inside function environment
  print(paste0(name, " likes ", food))
  food_list
}
# improve by set.seed
h2 <- function(name, food_list = list(), seed = 123){
  # set seed: make sure to same results
  set.seed(seed)
  food <- sample(c("lasagna", "cassoulet", "feijoada"), 1)
  # now need to unset seed because seed will stay whole session
  set.seed(NULL)
  food_list <- append(food_list, food) # <- only inside function environment
  print(paste0(name, " likes ", food))
  food_list
}

# we can use instead with_seed()
withr::with_seed(seed = 123, h("Bruno"))
# we can create a wrapper around
h3 <- function(..., seed) {
  withr::with_seed(seed = seed, h(...))
}
h3("Bruno", seed = 123)
# or
h("Bruno") |> with_seed(seed = 123)

# another example for download without tempfile ... only test link
withr::with_tempfile("unemp", {
  download.file("https://is.gd/l57cNX", destfile = unemp)
  load(unemp)
  nrow(unemp)
})
# file isnt in global environment!
# wie oben... download and then load doesnt work (here??)

# conclusion: writing pure functions, 
# avoid loops, replacing with higherorder functions lapply, Reduce, map, ...

# ..............................................................................
# ..............................................................................
# ..............................................................................
