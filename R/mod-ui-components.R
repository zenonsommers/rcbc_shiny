# ==============================================================================
# mod-ui-components.R — Shared UI widgets and helpers
# ==============================================================================

# ==============================================================================
# Modal Dialogs (also in global.R, re-exported for module consistency)
# ==============================================================================

show_error_modal <- function(message) {
  modalDialog(
    title = "Validation Error",
    p(message),
    easyClose = TRUE,
    footer = modalButton("Dismiss")
  )
}

show_success_modal <- function(message) {
  modalDialog(
    title = "Success",
    p(message),
    easyClose = TRUE,
    footer = modalButton("OK")
  )
}

# ==============================================================================
# Initial Ballot Order (used by ballot module)
# ==============================================================================

# Generate a randomized initial ordering of candidates
# @param candidates character vector of candidate names
# @return character vector of candidates in randomized order
initial_ballot_order <- function(candidates) {
  sample(candidates)
}

# ==============================================================================
# Rank List Widget
# ==============================================================================

# Create a drag-and-drop ranking list UI element
# @param text description text to display above the list
# @param labels character vector of items to rank
# @param input_id input ID for Shiny to access the result
# @param class CSS class for styling
# @return UI element (sortable rank list)
rank_list <- function(text, labels, input_id, class = "custom-rank-list") {
  tagList(
    tags$p(text, class = "mb-2"),
    sortable::rank_list(
      items = labels,
      input_id = input_id,
      class = class
    )
  )
}

# ==============================================================================
# End Screen Message Handler
# ==============================================================================

# Display end-of-action screen content
# @param message text message to display
# @param session Shiny session
end_screen_message <- function(message, session) {
  # Called from module context to update the end screen
  # Implementation lives in mod-electron.R (state management)
  NULL
}

# ==============================================================================
# Footer Button Row
# ==============================================================================

# Create a standard footer button row with spaced buttons
# @param buttons list of UI elements (typically actionButtons)
# @return div with footer-buttons class
footer_buttons <- function(...) {
  div(class = "footer-buttons", ...)
}
