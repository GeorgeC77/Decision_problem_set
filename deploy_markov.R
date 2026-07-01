library(rsconnect)

rsconnect::setAccountInfo(
  name = 'cuigeng',
  token = '4D1B1D7505BFD88BCD48DE226932BCAF',
  secret = 'dzHUM9DLa46VkTx6ecNX6Y+IYuVM/6WvYbWAST37'
)

rsconnect::deployApp(
  appDir = "Decision_Markov",
  appName = "Decision_Markov",
  account = "cuigeng",
  launch.browser = FALSE,
  forceUpdate = TRUE
)
