# ==============================================================================
# mod-ui-hub.R — Hub page UI
# ==============================================================================

# ==============================================================================
# Hub UI Function
# ==============================================================================

# @export
mod_hub_ui <- function() {
  tagList(
    h3("Welcome!"),
    p("Please provide an Election ID and select an action to begin."),
    hr(),
    wellPanel(
      fluidRow(
        column(8,
               textInput("election_id", "Election ID",
                         placeholder = paste0("e.g., city-council-",
                                              format(Sys.Date(), "%Y")))
        ),
        column(4, style = "margin-top: 25px;",
               actionButton("generate_id", "Generate New ID",
                            icon = icon("wand-magic-sparkles"))
        )
      ),
      radioButtons("app_function", "What would you like to do?",
                   choices = c("Create a new election" = "create",
                               "Submit a ballot" = "ballot",
                               "Process election results" = "process",
                               "Edit an election" = "edit"),
                   selected = "create"),
      br(),
      actionButton("go_to_function", "Continue →",
                   class = "btn-primary btn-lg", icon = icon("arrow-right"))
    )
  )
}

# ==============================================================================
# Hub Server Function
# ==============================================================================

# @export
mod_hub_server <- function(id, session) {
  moduleServer(id, function(input, output, session) {

    # Generate a unique election ID using the adjective-color-animal pattern
    observeEvent(input$generate_id, {
      election_count <- length(list.dirs(path = "Elections", recursive = FALSE))
      adjectives <- c("bold", "brave", "bright", "calm", "clever", "cool",
                      "eager", "epic", "fast", "fierce", "fine", "good", "grand",
                      "great", "happy", "jolly", "jovial", "keen", "kind",
                      "lively", "lucky", "magic", "merry", "neat", "noble",
                      "plucky", "proud", "quirky", "rapid", "regal", "sharp",
                      "shiny", "silent", "silly", "sleek", "slick", "smart",
                      "smooth", "snappy", "spunky", "stark", "stellar", "sturdy",
                      "super", "swift", "true", "vital", "vivid", "witty", "zany")
      colors <- c("aquamarine", "blue", "brown", "coral", "cyan", "green",
                  "indigo", "lime", "magenta", "maroon", "navy", "olive",
                  "orange", "pink", "purple", "red", "silver", "teal",
                  "violet", "yellow")
      animals <- c("albatross", "alligator", "armadillo", "badger", "bat",
                   "bear", "beaver", "bison", "buffalo", "camel", "cheetah",
                   "chimpanzee", "crab", "crocodile", "coyote", "deer",
                   "dolphin", "eagle", "elephant", "elk", "falcon", "fox",
                   "giraffe", "goat", "gorilla", "hawk", "hedgehog", "hippo",
                   "horse", "hummingbird", "hyena", "jaguar", "jellyfish",
                   "kangaroo", "koala", "lemur", "leopard", "lion", "lizard",
                   "lobster", "meerkat", "monkey", "moose", "octopus", "ostrich",
                   "otter", "owl", "panda", "panther", "parrot", "peacock",
                   "pelican", "penguin", "porcupine", "raccoon", "rhino",
                   "seagull", "seal", "shark", "skunk", "sloth", "snake",
                   "squid", "starfish", "swan", "tiger", "toucan", "vulture",
                   "walrus", "warthog", "whale", "wolf", "woodpecker", "zebra")

      new_id <- NULL
      is_unique <- FALSE
      while (!is_unique) {
        if (election_count <= 10000) {
          new_id <- paste(sample(adjectives, 1), sample(colors, 1),
                          sample(animals, 1), sep = "-")
        } else {
          new_id <- paste(sample(adjectives, 1), sample(colors, 1),
                          sample(animals, 1), floor(runif(1, 1000, 9999)),
                          sep = "-")
        }
        if (!dir.exists(file.path("Elections", tolower(new_id)))) {
          is_unique <- TRUE
        }
      }
      updateTextInput(session, "election_id", value = new_id)
    })

    # Navigate to selected function
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
          # Default to TRUE if missing
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
        active_election_id(id_val) # Set active ID only on successful load

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
          active_election_id(id_val) # Set active ID on successful create nav
          current_ui("create")
        }
      }
    })

  })
}
