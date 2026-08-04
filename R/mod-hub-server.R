# ==============================================================================
# mod-hub-server.R — Hub Page Server Logic
# ==============================================================================

# @export
mod_hub_server <- function(id, session) {
  moduleServer(id, function(input, output, session) {

    # Navigate to selected function from hub
    observeEvent(input$go_to_function, {
      last_function_choice(input$app_function)
      id_val <- tolower(trimws(input$election_id))

      if (id_val == "") {
        showModal(show_error_modal("Election ID cannot be empty."))
        return()
      }

      # Set the 'last_known_id' immediately, so we can return to it.
      last_known_id(id_val)

      election_path <- file.path("Elections", id_val)
      config_path <- file.path(election_path, "config.json")

      if (input$app_function %in% c("ballot", "process", "edit")) {
        if (!dir.exists(election_path) || !file.exists(config_path)) {
          showModal(show_error_modal(
            "No election found with this ID. Please check or create a new one."
          ))
          return()
        }

        config <- tryCatch(fromJSON(config_path), error = function(e) {
          showModal(show_error_modal(paste("Error reading config file:", e$message)))
          return(NULL)
        })
        if (is.null(config)) return()

        if (input$app_function == "ballot") {
          if (is.null(config$accepting_responses)) config$accepting_responses <- TRUE
          if (!config$accepting_responses) {
            showModal(show_error_modal(
              "This election is not currently accepting new ballots."
            ))
            return()
          }
        }

        if (input$app_function == "edit") {
          if (is.null(config$accepting_responses)) {
            config$accepting_responses <- TRUE
            tryCatch(
              write_json(config, config_path, auto_unbox = TRUE, pretty = TRUE),
              error = function(e) {
                showModal(show_error_modal("Could not update config file."))
              }
            )
          }
        }

        election_config(config)
        active_election_id(id_val)

        if (input$app_function == "edit") {
          if (config$password_hash == "") {
            editing_page(2)
          } else {
            editing_page(1)
          }
        }

        current_ui(input$app_function)

      } else if (input$app_function == "create") {
        if (dir.exists(election_path)) {
          showModal(show_error_modal(
            "An election with this ID already exists. Please choose another."
          ))
          return()
        } else {
          active_election_id(id_val)
          current_ui("create")
        }
      }
    })

  })
}
