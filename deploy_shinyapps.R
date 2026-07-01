# Deploy 5 exercise-based Shiny apps to shinyapps.io
# Run from project root

library(rsconnect)

rsconnect::setAccountInfo(
  name = 'cuigeng',
  token = '4D1B1D7505BFD88BCD48DE226932BCAF',
  secret = 'dzHUM9DLa46VkTx6ecNX6Y+IYuVM/6WvYbWAST37'
)

apps <- c(
  "Exercise_Ch2_NPV",
  "Decision_Risk",
  "Decision_Uncertainty",
  "Decision_AHP",
  "Decision_Markov"
)

for (app in apps) {
  message("\n========================================")
  message("Deploying ", app, " ...")
  message("========================================\n")
  tryCatch({
    rsconnect::deployApp(
      appDir = app,
      appName = app,
      account = "cuigeng",
      launch.browser = FALSE,
      forceUpdate = TRUE
    )
    message("SUCCESS: ", app)
  }, error = function(e) {
    message("ERROR deploying ", app, ": ", conditionMessage(e))
  })
}

message("\nAll deployment attempts finished.")
