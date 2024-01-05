# ..............................................................................
#
# Chapter 7
# literate programming

ls()

# ..............................................................................
# Test:
# compile pdf from tex ... works
# writeLines(c(
#   '\\documentclass{article}',
#   '\\begin{document}', 'Hello world!', '\\end{document}'
# ), 'test.tex')
# tinytex::pdflatex('test.tex')

# ..............................................................................
# 1) sveave example
# Test: Rnw - compile R Sweave to pdf
# install/update tinytex
# for R Sveave example
#install.packages("rmarkdown")
install.packages("tinytex")
tinytex::install_tinytex()
tinytex::tlmgr_install("fancyhdr")
#tinytex::uninstall_tinytex() 
# 
# tinytex::tlmgr_search('/fancyvfb.sty')
# tinytex::tlmgr_install("fancyvfb")
# tinytex::tlmgr_install("ae")

# important! because of MikTeX installation we have to force use tinytex
# Tools -> Global Options -> Sweave -> LaTeX Editing and Compilation -> Use tinytex when compiling


# ..............................................................................
# 2) rmd example

# additional:
# bash commands in rmd
# You can also write Shell scripts in R Markdown, if your system can run them 
# (the executable bash or sh should exist). Usually this is not a problem 
# for Linux or macOS users. It is not impossible for Windows users to run 
# Shell scripts, but you will have to install additional software 
# (such as Cygwin or the Linux Subsystem).


# install https://cygwin.com/
Sys.which("bash")
Sys.getenv("PATH")

# https://stackoverflow.com/questions/66386771/sys-whichmake-empty-although-rtools-is-in-path
path <- Sys.getenv("PATH")
path <- c("C:\\cygwin64\\bin", path) 
#Remember to substitute the two paths with your actual paths.
path <- paste(path,collapse=";")

#Then run
Sys.setenv(PATH=path)
Sys.which("bash")

# ..............................................................................
# quatro example

# instead of
# output: pdf
# i have to use
# format: pdf
# in the header

# ..............................................................................
# tables

# flextable: the fact that it works for PDF, Html, Word and Powerpoint outputs 
# is really a massive plus
# see https://ardata-fr.github.io/flextable-book/

library(flextable)

my_table <- head(mtcars)

flextable(my_table) |>
  set_caption(caption = "Head of the mtcars dataset") |>
  theme_booktabs()

# modelsummary: focuses on regression and summary tables
#  just like {flextable}, works for any type of output
# see https://modelsummary.com/

library(modelsummary)

model_1 <- lm(mpg ~ hp + am, data = mtcars)
model_2 <- lm(mpg ~ hp, data = mtcars)

models <- list("Model 1" = model_1,
               "Model 2" = model_2)

modelsummary(models)

# ..............................................................................

# parametrisid reports

# see header of param-report-example.Rmd
# (default parameters)
# params:
# dataset: mtcars
# var: "am"

rmarkdown::render(
  #input = "param_report_example.Rmd",
  input = "./literate/param-report-example/param-report-example.Rmd",
  params = list(
    dataset = "mtcars",
    var = "cyl"
  )
)

# get objects from strings
string <- "mtcars"
get(string)
# object to string
deparse(substitute(mtcars))


# create one report per variable
# use lapply() to loop over a list of column names
# The complete call to rmarkdown::render() is wrapped inside an 
# anonymous function, because I need to use the argument x 
# (which is each column defined in the columns list) in different places.

columns <- colnames(mtcars)

lapply(columns,
       (\(x)rmarkdown::render(
         input = "./literate/param-report-example/param-report-example.Rmd",
         output_file = paste0(
           "param_report_example_", x, ".html"
         ),
         params = list(
           dataset = "mtcars",
           var = x
         )
       )
       )
)

# try with iris instead
rmarkdown::render(
  input = "./literate/param-report-example/param-report-example.Rmd",
  output_file = "param-report-example-iris.html",
  params = list(
    dataset = "iris",
    var = "Species"
  )
)
