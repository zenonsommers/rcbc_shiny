# ==============================================================================
# ui.R — Complete UI definition
# ==============================================================================

# Load module UI functions (order matters: components first, then pages)
source("R/mod-ui-components.R")
source("R/mod-ui-hub.R")
source("R/mod-ui-create.R")
source("R/mod-ui-ballot.R")
source("R/mod-ui-process.R")
source("R/mod-edit.R")

# The main UI definition
ui <- fluidPage(
  theme = bs_theme_update(theme, bootswatch = light_bootswatch),
  useShinyjs(),

  # Custom CSS for padding, dark mode toggle, and sortable items
  tags$head(tags$style(HTML(css_rules))),

  # Dark mode toggle switch
  div(style = "position: absolute; top: 10px; right: 20px; z-index: 1000;",
      actionButton("darkModeToggle", "",
                   icon = icon("sun"),
                   style = "border: none; background: transparent;")
  ),

  titlePanel("🗳️ RCBC Shiny Election Suite"),

  # The main UI will be dynamically rendered here based on user choices
  uiOutput("main_ui")
)
