### Testing Connecting Tabs
### https://davidruvolo51.github.io/shinytutorials/tutorials/internal-links-a/
### 
### I think this is how we would link tabs but I cannot figure it out
### https://davidruvolo51.github.io/shinytutorials/tutorials/shiny-link/
### 
shinyLink <- function(to, label) {
  tags$a(
    class = "shiny__link",
    href = to,
    label
  )
}

### add shorter values for navigation, otherwise the values are the title by default