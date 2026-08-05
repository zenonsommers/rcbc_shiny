# ==============================================================================
# server.R — Complete server logic (module instantiation and wiring)
# ==============================================================================

# Load module server files (order matters: electron first for state)
source("R/mod-electron.R")
source("R/mod-electron-server.R")
source("R/mod-hub-server.R")
source("R/mod-create-server.R")
source("R/mod-ballot-server.R")
source("R/mod-edit-server.R")
source("R/mod-dark-mode-server.R")
source("R/mod-file-io-server.R")
source("R/mod-file-uploader-server.R")
source("R/mod-plots-server.R")
source("R/mod-processing-server.R")
source("R/mod-results-server.R")
source("R/mod-server-main.R")

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

  # Dark mode toggle
  mod_dark_mode_server("dark-mode", session)

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

  # Main UI switcher and return-to-hub logic
  mod_server_main("main", session)

}
