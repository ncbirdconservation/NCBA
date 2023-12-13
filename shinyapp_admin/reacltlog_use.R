### Reactlog for viewing dependencies
### https://rstudio.github.io/reactlog/articles/reactlog.html

### install package
if(!require(reactlog)) install.packages(
  "reactlog", repos = "http://cran.us.r-project.org")

### load package
library(reactlog)

### tells shiny to log reactivity
reactlog_enable()

### run the app

### once app has been closed, display the reactlog from shiny
shiny::reactlogShow()