# script to start the shiny app
library(shiny)
options(shiny.port = 6769) # You need to forward this port
options(shiny.host = '0.0.0.0') # Set to listen to all connections
shiny::runApp()