# ==============================================================================
# app.R — Thin entry point for the RCBC Shiny Election Suite
#
# This is the only file that runs on startup. All logic is split into
# modules in the R/ directory and the voter/ package.
# ==============================================================================

# Load global settings, constants, and helpers
source("global.R")

# Load UI definition
source("ui.R")

# Load server logic
source("server.R")

# Run the application
shinyApp(ui = ui, server = server)
