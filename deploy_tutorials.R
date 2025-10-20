# find all tutorial directories and deploy each

library(rsconnect)
library(stringr)
library(fs)

# dirs <- list.dirs(".", recursive = FALSE)
# dirs <- dirs[!grepl("/\\.", dirs)]

is_tutorial <- function(d) {
  rmds <- dir_ls(d, glob = "*.Rmd")
  if (length(rmds) > 0) {
    any(grepl("runtime:\\s*shiny_prerendered", readLines(rmds, warn = FALSE)))
  } else{
    FALSE
  }
}

# tutorial_dirs <- dirs[vapply(dirs, is_tutorial, logical(1))]

# just deploy the ones that are 'real'
tutorial_dirs <- c("./base R", "./more base R", "./more-tidy-R", "./tidy R")

for (d in tutorial_dirs) {
  message("📤 Deploying tutorial in ", d)
  rsconnect::deployApp(
    appDir = d,
    forceUpdate = TRUE,
    launch.browser = FALSE
  )
}