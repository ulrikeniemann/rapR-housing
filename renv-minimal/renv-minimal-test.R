
# make sure that it correctly compiles
rmarkdown::render("my-new-project.Rmd")

# init renv
renv::init()

# add library(ggplot2)
rmarkdown::render("my-new-project.Rmd")
# error in library

# install in project library
# (start from project folder!)
install.packages("ggplot2")

# update renv.lock
renv::snapshot()

# if you din´t want to install the latest version
# install older version of AER
renv::install("AER@1.0-0") # this is a version from August 2008

# make sure that it correctly compiles
rmarkdown::render("my-new-project.Rmd")

# update renv.lock
renv::snapshot()
