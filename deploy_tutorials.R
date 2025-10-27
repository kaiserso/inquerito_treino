# find all tutorial directories and deploy each

library(rsconnect)
library(stringr)
library(fs)

# note, you have to do this if you installed package locally, so that it is reinstalled
# from github, so that rsconnect finds it
# remotes::install_github("kaiserso/learnrLogr", force = TRUE)

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
deployed <- c()

for (d in tutorial_dirs) {
  message("📤 Deploying tutorial in ", d)
  tryCatch({
    res <- rsconnect::deployApp(
      appDir = d,
      appName = str_replace_all(str_replace_all(basename(d), " ", "-"), "^./", ""),
      account = "peteryoung",
      server = "shinyapps.io",
      forceUpdate = TRUE,
      launch.browser = FALSE
    )
    message("✅ Upload succeeded: ", res$url)
    deployed <- c(deployed, res$url)
  }, error = function(e) {
    message("❌ Deployment failed: ", conditionMessage(e))
    FALSE
  })
}

# confirm successful deployment
for (u in deployed) {
  message("checking deployment for ", u)
  resp <- httr::GET(u)
  if (httr::status_code(resp) == 200) {
    message("✅ App is live at ", u)
  } else {
    message("⚠️ App deployed but returned status ", httr::status_code(resp))
  }
}
