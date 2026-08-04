# ==============================================================================
# mod-dark-mode.R — Dark Mode Toggle
# ==============================================================================

# ==============================================================================
# Dark Mode Server Function
# ==============================================================================

# @export
mod_dark_mode_server <- function(id, session) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$darkModeToggle, {
      is_dark(!is_dark())

      # Update the icon based on the new mode
      if (is_dark()) {
        updateActionButton(session, "darkModeToggle", icon = icon("moon"))
      } else {
        updateActionButton(session, "darkModeToggle", icon = icon("sun"))
      }

      # Update the theme
      session$setCurrentTheme(
        bs_theme_update(theme, bootswatch = if (is_dark()) dark_bootswatch
                        else light_bootswatch)
      )
    })

  })
}
