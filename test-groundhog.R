
# Test groundhog
# https://groundhogr.com/

#install.packages("groundhog")

groundhog::groundhog.library("
    library(purrr)
    library(ggplot2)",
                             "2017-10-04"
)

data(mtcars)

myplot <- ggplot(mtcars) +
  geom_line(aes(y = hp, x = mpg))

ggsave("/plots/myplot.pdf", myplot)

# ---------------------------------------------------------------------------
#   |   IMPORTANT.
# |       You are using R-4.1.0, but the version of R current for the entered 
# |       date, '2017-10-04', is R-3.4.x. It is recommended that you either 
# |       keep this date and switch to that version of R, or you keep the 
# |       version of R you are using but switch the date to between 
# |       '2021-05-18' and '2022-04-21'. 
# |       
#   |       You may bypass this R-version check by adding: 
#   |       `tolerate.R.version='4.1.0'`as an option in your groundhog.library() 
# |       call. 
# **Groundhog stopped**


# You can keep trak of installation progress and expected 
# completion time opening this real-time updated text file:
#   C:\Users\uniem\Documents\R_groundhog\groundhog_library\\install_progress.txt


groundhog::groundhog.library("
    library(purrr)
    library(ggplot2)",
                  "2017-10-04",
                  tolerate.R.version = "4.1.0")
# doensn´t work
# --   Installation Failed   -- 

groundhog::groundhog.library(c("rlang"),
                             "2022-01-01")
groundhog::groundhog.library(c("purrr"),
                             "2022-01-01")
groundhog::groundhog.library(c("ggplot2"),
                             "2022-01-01")

 

data(mtcars)

myplot <- ggplot(mtcars) +
  geom_line(aes(y = hp, x = mpg))

myplot

ggsave("./plots/myplot.pdf", myplot)

# ..............................................................................


pkgs <- c("rio","metafor")
groundhog::groundhog.library(pkgs, "2021-09-01")
