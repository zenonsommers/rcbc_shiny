# ==============================================================================
# mod-electron.R — Reactive State Manager
#
# Centralizes all reactive state for the app. Follows the "electron" pattern
# from Shiny module best practices — one source of truth for shared state.
# ==============================================================================

# ==============================================================================
# State Definitions (exported for use by modules)
# ==============================================================================

# UI state
current_ui <- reactiveVal("hub")       # Current page: hub, create, ballot, process, edit, end

# Election state
active_election_id <- reactiveVal(NULL) # Currently selected election ID
last_known_id <- reactiveVal(NULL)       # Last entered ID (for return-to-hub)
election_config <- reactiveVal(NULL)     # Parsed config.json content
end_screen_message <- reactiveVal("")    # Message for end screen
initial_ballot_order <- reactiveVal(NULL) # Original ballot order (for confirmation)
is_dark <- reactiveVal(FALSE)            # Dark mode flag

# Page state
creation_page <- reactiveVal(1)          # Multi-step creation: 1 (CSV) or 2 (details)
processing_page <- reactiveVal(1)        # Multi-step processing: 1 (review) or 2 (run)
editing_page <- reactiveVal(1)            # Multi-step editing: 1 (password) or 2 (main)

# Tabulation state
tabulation_method <- reactiveVal("cpo_stv") # Selected method

# Last function choice (for hub radio button)
last_function_choice <- reactiveVal("create")

# ==============================================================================
# Helper Functions
# ==============================================================================

# Load election config from disk
# @param id election ID string
# @return list config object or NULL if not found
load_election_config <- function(id) {
  config_path <- file.path("Elections", id, "config.json")
  if (!file.exists(config_path)) {
    return(NULL)
  }
  tryCatch(fromJSON(config_path, simplifyVector = TRUE), error = function(e) NULL)
}

# Save election config to disk
# @param config list config object
# @param id election ID string
# @return TRUE on success, FALSE on failure
save_election_config <- function(config, id) {
  config_path <- file.path("Elections", id, "config.json")
  tryCatch({
    write_json(config, config_path, auto_unbox = TRUE, pretty = TRUE)
    TRUE
  }, error = function(e) {
    message(paste("Failed to save config:", e$message))
    FALSE
  })
}
