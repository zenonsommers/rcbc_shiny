# ==============================================================================
# mod-server-main.R — Main UI switcher and return-to-hub logic
#
# This module provides the output$main_ui renderUI that switches between
# hub, create, ballot, process, edit, and end screens.
# ==============================================================================

# @export
mod_server_main <- function(id, session) {
  moduleServer(id, function(input, output, session) {

    # Main UI switcher — renders the appropriate page based on current_ui()
    output$main_ui <- renderUI({
      switch(current_ui(),
             "hub"     = mod_hub_ui(),
             "create"  = mod_create_ui(),
             "ballot"  = mod_ballot_ui(),
             "process" = mod_process_ui(),
             "edit"    = mod_edit_ui(),
             "end"     = mod_end_ui(),
             mod_hub_ui() # fallback
      )
    })

    # Return to hub logic (shared across modules)
    reset_to_hub <- function() {
      # This logic now correctly finds the last ID used,
      # whether from a successful action (active) or a failed one (known)
      last_id <- active_election_id()
      if (is.null(last_id) || last_id == "") {
        last_id <- last_known_id()
      }

      active_election_id(NULL) # Clear the *active* session
      last_known_id(last_id) # Persist the last known ID

      election_config(NULL)
      end_screen_message("")
      shinyjs::show("processing_inputs")
      output$text_results <- renderPrint({ "" })
      output$table_results <- renderTable({ NULL })
      creation_page(1)
      processing_page(1)
      editing_page(1)
      current_ui("hub")

      shinyjs::delay(1, {
        updateTextInput(session, "election_id", value = last_id)
      })
    }

    observeEvent(input$return_home_general, { reset_to_hub() })

  })
}
