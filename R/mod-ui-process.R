# ==============================================================================
# mod-ui-process.R — Process Results UI
# ==============================================================================

# ==============================================================================
# Process UI Functions
# ==============================================================================

# @export
mod_process_ui <- function() {
  if (processing_page() == 1) process_page_1_ui() else process_page_2_ui()
}

process_page_1_ui <- function() {
  req(election_config())
  config <- election_config()
  tagList(
    h3(paste("Processing Results for:", config$title)),
    p(paste("Election ID:", config$unique_identifier)),
    hr(),
    strong("Candidates in this Election:"),
    p(HTML(paste(config$candidates, collapse = "<br>"))),
    textOutput("ballot_count_p1"),
    tags$details(
      tags$summary("View Recorded Ballots"),
      div(style = "max-height: 300px; overflow-y: auto;",
          tableOutput("ballot_table_p1"))
    ),
    tags$details(
      tags$summary("View Preferences by Candidate"),
      div(class = "plot-container", # Add class for scrolling
          uiOutput("candidate_pref_plots_p1"))
    ),
    hr(),
    radioButtons("tabulation_method_choice", "Choose Tabulation Method:",
                 choices = c("CPO-STV" = "cpo_stv",
                             "Standard STV" = "stv",
                             "Borda Scores" = "borda",
                             "Borda with Tiebreakers" = "borda_tb"),
                 selected = "cpo_stv"),
    footer_buttons(
      actionButton("return_home_general", "Return to Home",
                   class = "btn-secondary"),
      actionButton("to_process_page_2", "Continue", class = "btn-primary")
    )
  )
}

process_page_2_ui <- function() {
  req(election_config())
  config <- election_config()

  method_name <- switch(tabulation_method(),
                        "cpo_stv" = "CPO-STV",
                        "stv" = "Standard STV",
                        "borda" = "Borda Scores",
                        "borda_tb" = "Borda with Tiebreakers")

  tagList(
    h3(paste("Processing Results for:", config$title)),
    p(paste("Election ID:", config$unique_identifier)),
    p(paste("Tabulation method:", method_name)),
    hr(),
    strong("Candidates in this Election:"),
    p(HTML(paste(config$candidates, collapse = "<br>"))),
    textOutput("ballot_count_p2"),
    tags$details(
      tags$summary("View Recorded Ballots"),
      div(style = "max-height: 300px; overflow-y: auto;",
          tableOutput("ballot_table_p2"))
    ),
    tags$details(
      tags$summary("View Preferences by Candidate"),
      div(class = "plot-container", # Add class for scrolling
          uiOutput("candidate_pref_plots_p2"))
    ),
    br(),
    div(id = "processing_inputs",
        div(id = "seats_option",
            numericInput("process_seats", "Number of seats to elect",
                         value = config$seats, min = 1)),

        div(id = "tiebreak_options_cpo",
            rank_list(
              text = "Drag to order CPO-STV tie-break methods",
              labels = tiebreaker_choices[names(tiebreaker_choices) != "cpo_stv"],
              input_id = "tiebreak_methods_cpo",
              class = "custom-rank-list"
            )),

        div(id = "tiebreak_options_borda_tb",
            rank_list(
              text = "Drag to order Borda tie-break methods",
              labels = tiebreaker_choices[names(tiebreaker_choices) != "borda"],
              input_id = "tiebreak_methods_borda_tb",
              class = "custom-rank-list"
            )),

        div(id = "seed_option",
            numericInput("seed", "Random Seed for Tie-Breaking",
                         value = default_seed)),

        checkboxInput("verbose_output", "Show verbose output",
                      value = FALSE),

        footer_buttons(
          actionButton("return_home_general", "Return to Home",
                       class = "btn-secondary"),
          actionButton("submit_processing", "Process Results",
                       class = "btn-info", icon = icon("calculator"))
        )
    ),
    hr(),
    h4("Results:"),
    verbatimTextOutput("text_results"),
    tableOutput("table_results"),

    shinyjs::hidden(
      div(id = "post_processing_ui", class = "footer-buttons",
          # Buttons are already spaced due to footer-buttons class
          span(), # Pushes button right
          actionButton("return_home_general", "Return to Home",
                       class = "btn-primary", icon = icon("home"))
      )
    )
  )
}
