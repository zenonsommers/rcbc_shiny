# ==============================================================================
# mod-create-server.R — Create Election Server Logic
# ==============================================================================

# @export
mod_create_server <- function(id, session) {
  moduleServer(id, function(input, output, session) {

    # Navigate from page 1 (CSV upload) to page 2 (election details)
    observeEvent(input$to_create_page_2, {
      candidate_names_from_csv <- ""
      if (!is.null(input$ballot_file)) {
        tryCatch({
          ballot_df <- read.csv(input$ballot_file$datapath, check.names = FALSE)
          processed_df <- ballot_df %>% select(where(is.numeric)) %>%
            removeQuestion()
          candidates <- colnames(processed_df)
          if (length(candidates) < 1) {
            stop("No numeric ranking columns found after processing.")
          }
          candidate_names_from_csv <- paste(candidates, collapse = "\n")
        }, error = function(e) {
          showModal(show_error_modal(
            paste("Error processing CSV:", e$message,
                  "Please check file format and ensure ranks are numeric.")
          ))
          reset("ballot_file")
          candidate_names_from_csv <<- ""
        })
      }
      creation_page(2)
      shinyjs::delay(1, {
        updateTextInput(session, "election_title", value = active_election_id())
        if (candidate_names_from_csv != "") {
          updateTextAreaInput(session, "candidate_names",
                              value = candidate_names_from_csv)
        }
      })
    })

    # Submit the new election creation
    observeEvent(input$submit_creation, {
      candidates <- trimws(unlist(strsplit(input$candidate_names, "\n")))
      candidates <- candidates[candidates != ""]
      if (length(candidates) < 3) {
        showModal(show_error_modal("Please specify at least three candidates."))
        return()
      }
      if (input$seats >= length(candidates)) {
        showModal(show_error_modal(
          "The number of seats must be less than the number of candidates."
        ))
        return()
      }
      election_path <- file.path("Elections", active_election_id())
      dir.create(election_path)
      config <- list(
        unique_identifier = active_election_id(),
        title = input$election_title, candidates = candidates,
        seats = input$seats, allow_incomplete = input$allow_incomplete,
        allow_ties = input$allow_ties,
        password_hash = if (input$password != "") {
          digest::digest(input$password, "sha256")
        } else { "" },
        accepting_responses = TRUE
      )
      write_json(config, file.path(election_path, "config.json"),
                 auto_unbox = TRUE, pretty = TRUE)
      if (!is.null(input$ballot_file)) {
        tryCatch({
          ballot_df <- read.csv(input$ballot_file$datapath, check.names = FALSE)
          processed_df <- ballot_df %>% select(where(is.numeric)) %>%
            removeQuestion()
          csv_candidates <- colnames(processed_df)
          if(!identical(candidates, csv_candidates)){
            if(setequal(candidates, csv_candidates)){
              processed_df <- processed_df[, candidates]
              warning("CSV columns reordered to match candidate list.")
            } else {
              stop("Candidate list derived from CSV does not match final list.")
            }
          }

          for (i in 1:nrow(processed_df)) {
            ballot_data <- setNames(as.list(processed_df[i, ]), candidates)
            ballot_data <- lapply(ballot_data,
                                  function(x) if(is.numeric(x)) x else NA)
            ballot_filename <- paste0("ballot_", UUIDgenerate(), ".json")
            write_json(ballot_data,
                       file.path(election_path, ballot_filename),
                       auto_unbox = TRUE, na = "null")
          }
        }, error = function(e) {
          unlink(election_path, recursive = TRUE)
          showModal(show_error_modal(
            paste("Error processing CSV for ballot creation:", e$message)
          ))
          return()
        })
      }
      end_screen_message(paste("Successfully created election:",
                               input$election_title))
      current_ui("end")
    })

  })
}
