# ==============================================================================
# mod-edit-server.R — Edit Election Server Logic
# ==============================================================================

# @export
mod_edit_server <- function(id, session) {
  moduleServer(id, function(input, output, session) {

    # Password check for edit
    observeEvent(input$submit_password_check, {
      req(input$admin_password_check)
      config <- election_config()

      hashed_input <- digest(input$admin_password_check, "sha256")

      if (hashed_input == config$password_hash) {
        editing_page(2)
      } else {
        showModal(show_error_modal("Incorrect password."))
      }
    })

    # Toggle accepting responses
    observeEvent(input$accepting_responses, {
      if(is.null(election_config())) return()

      config <- election_config()
      config$accepting_responses <- input$accepting_responses
      election_config(config)

      config_path <- file.path("Elections", active_election_id(), "config.json")
      tryCatch({
        write_json(config, config_path, auto_unbox = TRUE, pretty = TRUE)
        showNotification(
          paste("Accepting responses set to:", input$accepting_responses),
          type = "message"
        )
      }, error = function(e){
        showModal(show_error_modal(paste("Failed to update config file:", e$message)))
      })
    }, ignoreInit = TRUE)

    # Change password
    observeEvent(input$change_password, {
      config <- election_config()

      if (input$new_password != input$confirm_new_password) {
        showModal(show_error_modal("New passwords do not match."))
        return()
      }

      has_old_pass <- !is.null(config$password_hash) && config$password_hash != ""
      if (has_old_pass) {
        hashed_old_pass <- digest(input$old_password, "sha256")
        if (hashed_old_pass != config$password_hash) {
          showModal(show_error_modal("Incorrect old password."))
          return()
        }
      } else {
        if (input$old_password != "") {
          showModal(show_error_modal(
            "No old password is set; 'Old Password' field should be blank."
          ))
          return()
        }
      }

      config$password_hash <- if (input$new_password != "") {
        digest(input$new_password, "sha256")
      } else { "" }
      election_config(config)

      config_path <- file.path("Elections", active_election_id(), "config.json")
      tryCatch({
        write_json(config, config_path, auto_unbox = TRUE, pretty = TRUE)
        show_success_modal("Password updated successfully.")
        updateTextInput(session, "old_password", value = "")
        updateTextInput(session, "new_password", value = "")
        updateTextInput(session, "confirm_new_password", value = "")
      }, error = function(e){
        showModal(show_error_modal(paste("Failed to update config file:", e$message)))
      })
    })

    # Delete election confirmation prompt
    observeEvent(input$delete_election_confirm_prompt, {
      showModal(modalDialog(
        title = "Confirm Deletion",
        p(paste("This action cannot be undone. Are you sure you want to delete",
                "election", sQuote(active_election_id()), "?")),
        passwordInput("delete_password_confirm", "Re-enter Admin Password to Confirm"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_delete", "Delete Election", class = "btn-danger")
        )
      ))
    })

    # Confirm and execute deletion
    observeEvent(input$confirm_delete, {
      config <- election_config()

      # Check if password is required and correct
      password_required <- !is.null(config$password_hash) && config$password_hash != ""
      if(password_required) {
        req(input$delete_password_confirm)
        hashed_input <- digest(input$delete_password_confirm, "sha256")
        if (hashed_input != config$password_hash) {
          showModal(show_error_modal("Incorrect password. Deletion cancelled."))
          return()
        }
      } else {
        # If no password set, check if user entered anything
        if(input$delete_password_confirm != "delete") {
          showModal(show_error_modal("No password is set for this election. Type the word 'delete' in all lowercase in the password field to continue."))
          return()
        }
      }

      election_path <- file.path("Elections", active_election_id())

      tryCatch({
        unlink(election_path, recursive = TRUE, force = TRUE)
        removeModal()
        last_known_id(NULL) # Clear the last known ID
        end_screen_message(paste("Election", sQuote(active_election_id()),
                                 "successfully deleted."))
        current_ui("end")
      }, error = function(e){
        removeModal() # Close confirmation modal first
        showModal(show_error_modal(paste("Failed to delete election:", e$message)))
      })
    })

  })
}
