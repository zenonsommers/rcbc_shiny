# ==============================================================================
# mod-end.R — End screen UI
# ==============================================================================

# ==============================================================================
# End Screen UI Function
# ==============================================================================

# @export
mod_end_ui <- function() {
  tagList(
    h3("Action Complete"),
    hr(),
    p(end_screen_message()),
    br(),
    actionButton("return_home_general", "Return to Home",
                 class = "btn-primary", icon = icon("home"))
  )
}
