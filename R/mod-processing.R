# ==============================================================================
# mod-processing.R — Tabulation Engine & Process Results Logic
# ==============================================================================

# ==============================================================================
# Process Results Server Function
# ==============================================================================

# @export
mod_processing_server <- function(id, session) {
  moduleServer(id, function(input, output, session) {

    # Process page navigation
    observeEvent(input$to_process_page_2, {
      tabulation_method(input$tabulation_method_choice)
      processing_page(2)
      shinyjs::delay(1, {
        method <- input$tabulation_method_choice

        show_seats <- method != "borda"
        show_cpo_ties <- method == "cpo_stv"
        show_borda_tb_ties <- method == "borda_tb"
        show_seed <- method != "borda"

        shinyjs::toggle(id = "seats_option", condition = show_seats)
        shinyjs::toggle(id = "tiebreak_options_cpo", condition = show_cpo_ties)
        shinyjs::toggle(id = "tiebreak_options_borda_tb",
                        condition = show_borda_tb_ties)
        shinyjs::toggle(id = "seed_option", condition = show_seed)
      })
    })

    # Submit processing results
    observeEvent(input$submit_processing, {
      output$text_results <- renderPrint({ "" })
      output$table_results <- renderTable({ NULL })

      ballot_df <- ballot_data_reactive()

      if (is.null(ballot_df)) {
        output$text_results <- renderPrint({
          "No ballots found or error reading ballots for this election."
        })
        return()
      }

      returned_value <- NULL
      printed_output <- NULL

      tryCatch({
        # Use sink to capture all output, including direct prints and messages
        output_con <- textConnection("printed_output", "w", local = TRUE)
        sink(output_con, type = "output")
        sink(output_con, type = "message")

        method <- tabulation_method()
        verbose_flag <- input$verbose_output

        current_seed <- if (method == "borda") default_seed else input$seed
        if (is.na(current_seed) || is.null(current_seed)) {
          current_seed <- default_seed
        }

        returned_value <- if (method == "cpo_stv") {
          cpo_stv(ballot_df, seats = input$process_seats,
                  ties = input$tiebreak_methods_cpo, seed = current_seed,
                  verbose = verbose_flag)
        } else if (method == "borda_tb") {
          borda(ballot_df, seats = input$process_seats,
                ties = input$tiebreak_methods_borda_tb, seed = current_seed,
                verbose = verbose_flag)
        } else if (method == "borda") {
          borda(ballot_df, seats = 0, seed = current_seed,
                verbose = verbose_flag)
        } else { # Standard STV
          config <- election_config()
          # Pass verbose flag to base STV function
          stv(ballot_df, nseats = input$process_seats,
              seed = current_seed, verbose = verbose_flag,
              equal.ranking = config$allow_ties)
        }

        # Close connections
        sink(type = "message"); sink(type = "output")
        close(output_con)

      }, error = function(e) {
        # Ensure sinks are closed on error too
        if (exists("output_con") && isOpen(output_con)) close(output_con)
        printed_output <<- paste("An error occurred during calculation:", e$message)
        returned_value <<- NULL # Ensure no table is shown on error
      })

      if (is.data.frame(returned_value) || is_tibble(returned_value)) {
        output$table_results <- renderTable({ returned_value })
      } else if (!is.null(returned_value)) {
        # Capture the print output of non-data frame results
        additional_output <- capture.output(print(returned_value))
        # Prepend additional output to the captured console output
        printed_output <- c(additional_output, printed_output)
      }

      output$text_results <- renderPrint({
        if (length(printed_output) > 0) {
          cat(paste(printed_output, collapse = "\n"))
        }
      })

      shinyjs::hide("processing_inputs")
      shinyjs::show("post_processing_ui")
    })

  })
}
