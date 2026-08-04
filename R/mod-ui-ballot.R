# ==============================================================================
# mod-ui-ballot.R — Ballot Submission UI
# ==============================================================================

# ==============================================================================
# Ballot UI Function
# ==============================================================================

# @export
mod_ballot_ui <- function() {
  req(election_config())
  config <- election_config()
  tagList(
    h3(paste("Submitting Ballot for:", config$title)),
    p(paste("Election ID:", config$unique_identifier)),
    hr(),
    uiOutput("ballot_interface"),
    hr(),
    footer_buttons(
      actionButton("return_home_general", "Return to Home",
                   class = "btn-secondary"),
      actionButton("submit_ballot", "Submit Ballot",
                   class = "btn-success", icon = icon("person-booth"))
    )
  )
}
