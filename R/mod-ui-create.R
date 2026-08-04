# ==============================================================================
# mod-ui-create.R — Create Election (multi-step) UI
# ==============================================================================

# ==============================================================================
# Create UI Functions
# ==============================================================================

# @export
mod_create_ui <- function() {
  if (creation_page() == 1) create_page_1_ui() else create_page_2_ui()
}

create_page_1_ui <- function() {
  tagList(
    h3(paste("Creating New Election:", active_election_id())),
    p(
      "You can pre-fill this election by uploading a CSV of ranked ballots,",
      "typically from a Google Form."
    ),
    hr(),
    fileInput("ballot_file", "Optional: Upload CSV to Pre-fill Ballots",
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain", ".csv")),
    footer_buttons(
      actionButton("return_home_general", "Return to Home",
                   class = "btn-secondary"),
      actionButton("to_create_page_2", "Continue to Details",
                   class = "btn-primary")
    )
  )
}

create_page_2_ui <- function() {
  tagList(
    h3(paste("Creating New Election:", active_election_id())),
    p("Define the parameters for your new election."),
    hr(),
    textInput("election_title", "Election Title",
              placeholder = "e.g., Annual Board Election"),
    textAreaInput("candidate_names", "Candidate Names (one per line)",
                  rows = 5),
    numericInput("seats", "Number of Seats to Elect", value = 3, min = 1,
                 step = 1),
    checkboxInput("allow_incomplete",
                  "Allow incomplete ballots",
                  value = FALSE),
    checkboxInput("allow_ties", "Allow tied ranks", value = FALSE),
    passwordInput("password", "Optional: Set an Admin Password"),
    hr(),
    footer_buttons(
      actionButton("return_home_general", "Return to Home",
                   class = "btn-secondary"),
      actionButton("submit_creation", "Create Election",
                   class = "btn-success", icon = icon("check"))
    )
  )
}
