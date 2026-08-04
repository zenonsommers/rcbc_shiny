# ==============================================================================
# server.R — Complete server logic (module instantiation and wiring)
# ==============================================================================

# Source module server files (order matters: electron first for state)
source("R/mod-electron.R")
source("R/mod-dark-mode.R")
source("R/mod-file-io.R")
source("R/mod-file-uploader.R")
source("R/mod-plots.R")
source("R/mod-processing.R")
source("R/mod-results.R")

# Load UI functions for module definitions
source("R/mod-ui-components.R")
source("R/mod-ui-hub.R")
source("R/mod-ui-create.R")
source("R/mod-ui-ballot.R")
source("R/mod-ui-process.R")
source("R/mod-edit.R")

# Load module server implementations
source("R/mod-electron-server.R")
source("R/mod-dark-mode-server.R")
source("R/mod-file-io-server.R")
source("R/mod-file-uploader-server.R")
source("R/mod-plots-server.R")
source("R/mod-processing-server.R")
source("R/mod-results-server.R")

# Source the election engine (will move to voter package later)
source("cpo_stv.R")

# Main server function
server <- function(input, output, session) {

  # -- Instantiate modules (state first, then UI handlers) --------------------

  # Electron (reactive state manager)
  mod_electron_server("electron", session)

  # Hub (page navigation from home)
  mod_hub_server("hub", session)

  # Create election (multi-step)
  mod_create_server("create", session)

  # Ballot submission
  mod_ballot_server("ballot", session)

  # Process results
  mod_process_server("process", session)

  # Edit election
  mod_edit_server("edit", session)

  # File operations
  mod_file_io_server("file-io", session)

  # File upload handling
  mod_file_uploader_server("file-uploader", session)

  # Plot rendering
  mod_plots_server("plots", session)

  # Tabulation engine
  mod_processing_server("processing", session)

  # Results display
  mod_results_server("results", session)

  # -- End screen helper ------------------------------------------------------

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

}
