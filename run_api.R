library(plumber)
r <- plumb("plumber.r")
r$run(port = 8000)