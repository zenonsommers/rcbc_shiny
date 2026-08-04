# ==============================================================================
# mod-ballot-server.R — Ballot Submission Server Logic
# ==============================================================================

# @export
mod_ballot_server <- function(id, session) {
  moduleServer(id, function(input, output, session) {

    # Render the ballot interface (drag-and-drop or dropdown)
    output$ballot_interface <- renderUI({
      config <- election_config()
      req(config)
      randomized_candidates <- sample(config$candidates)
      if (!config$allow_incomplete && !config$allow_ties) {
        initial_ballot_order(randomized_candidates)
        rank_list(
          text = paste("Rank candidates by dragging them into order from",
                       "most preferred (top) to least preferred (bottom)."),
          labels = randomized_candidates,
          input_id = "ranked_ballot_strict",
          class = "custom-rank-list"
        )
      } else {
        initial_ballot_order(NULL)
        max_rank <- length(config$candidates)
        ranks <- 1:max_rank
        lapply(randomized_candidates, function(cand) {
          sanitized_name <- gsub("\\s+|[^A-Za-z0-9]", "_", cand)
          selectInput(
            inputId = paste0("rank_", sanitized_name), label = cand,
            choices = c("Unranked" = 0, ranks), selected = 0
          )
        })
      }
    })

    # Process and save a ballot
    process_and_save_ballot <- function() {
      config <- election_config()

      # Re-check accepting responses status right before saving
      if (is.null(config$accepting_responses)) config$accepting_responses <- TRUE
      if (!config$accepting_responses) {
        showModal(show_error_modal(
          "This election is not currently accepting responses."
        ))
        return()
      }

      ranks <- NULL
      if (!config$allow_incomplete && !config$allow_ties) {
        ranks <- match(config$candidates, input$ranked_ballot_strict)
      } else {
        ranks <- sapply(config$candidates, function(cand) {
          sanitized_name <- gsub("\\s+|[^A-Za-z0-9]", "_", cand)
          as.numeric(input[[paste0("rank_", sanitized_name)]])
        })
      }

      if (!config$allow_incomplete && any(ranks == 0 | is.na(ranks))) {
        showModal(show_error_modal("Please rank all candidates."))
        return()
      }
      if (!config$allow_ties) {
        ranked_positions <- ranks[ranks > 0 & !is.na(ranks)]
        if (any(duplicated(ranked_positions))) {
          showModal(show_error_modal("Please assign a unique rank per candidate."))
          return()
        }
      }
      ballot_data <- setNames(as.list(ranks), config$candidates)
      ballot_filename <- paste0("ballot_", UUIDgenerate(), ".json")
      election_path <- file.path("Elections", active_election_id())
      write_json(ballot_data, file.path(election_path, ballot_filename),
                 auto_unbox = TRUE, na = "null") # Ensure NAs are handled
      end_screen_message("Your ballot has been successfully submitted.")
      current_ui("end")
    }

    # Submit ballot with confirmation check
    observeEvent(input$submit_ballot, {
      config <- election_config()
      if (!config$allow_incomplete && !config$allow_ties) {
        is_unchanged <- identical(initial_ballot_order(),
                                  input$ranked_ballot_strict)
        if (is_unchanged) {
          showModal(modalDialog(
            title = "Confirm Submission",
            "You have not reordered the candidates. Submit as-is?",
            easyClose = TRUE,
            footer = tagList(
              modalButton("Cancel"),
              actionButton("confirm_submit", "Yes, Submit",
                           class = "btn-primary")
            )
          ))
          return()
        }
      }
      process_and_save_ballot()
    })

    # Confirm ballot submission (from confirmation modal)
    observeEvent(input$confirm_submit, {
      removeModal()
      process_and_save_ballot()
    })

  })
}
