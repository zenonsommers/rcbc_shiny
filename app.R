# ==============================================================================
# Vibe-Coded Shiny Election App (Final Version)
#
# To Run:
# 1. Install packages:
#    install.packages(c("shiny", "shinyjs", "uuid", "jsonlite", "sortable",
#    "dplyr", "vote", "tidyverse", "magrittr", "readxl", "stringi", "gtools"))
# 2. Save this entire script as 'app.R'.
# 3. Open R/RStudio and run `shiny::runApp()` in the directory where you
#    saved the file.
# ==============================================================================

# Load necessary libraries
library(shiny)
library(shinyjs)
library(uuid)
library(jsonlite)
library(sortable)
library(dplyr)
library(vote)

# Load the cpo_stv function
source("cpo_stv.R")

# Define default seed
default_seed <- 38725

# -- App Setup -----------------------------------------------------------------

# Ensure the main directory for storing elections exists
if (!dir.exists("Elections")) {
  dir.create("Elections")
}

# Helper function for showing error pop-ups
show_error_modal <- function(message) {
  modalDialog(
    title = "Validation Error",
    p(message),
    easyClose = TRUE,
    footer = modalButton("Dismiss")
  )
}

# -- UI Definition -------------------------------------------------------------

ui <- fluidPage(
  useShinyjs(), # Initialize shinyjs
  titlePanel("🗳️ RCBC Shiny Election Suite"),
  
  # The main UI will be dynamically rendered here based on user choices
  uiOutput("main_ui")
)

# -- Server Logic --------------------------------------------------------------

server <- function(input, output, session) {
  
  # -- State Management --------------------------------------------------------
  
  current_ui <- reactiveVal("hub")
  active_election_id <- reactiveVal(NULL)
  election_config <- reactiveVal(NULL)
  end_screen_message <- reactiveVal("")
  initial_ballot_order <- reactiveVal(NULL)
  
  # Page managers for multi-step processes
  creation_page <- reactiveVal(1)
  processing_page <- reactiveVal(1)
  
  # To store the chosen tabulation method
  tabulation_method <- reactiveVal("cpo_stv")
  
  # -- Dynamic UI Rendering ----------------------------------------------------
  
  output$main_ui <- renderUI({
    switch(current_ui(),
           "hub"     = hub_ui(),
           "create"  = create_ui(),
           "ballot"  = ballot_ui(),
           "process" = process_ui(),
           "end"     = end_ui()
    )
  })
  
  # -- UI Components -----------------------------------------------------------
  
  hub_ui <- function() {
    fluidPage(
      h3("Welcome"),
      p("Select an action and provide an Election ID to begin."),
      hr(),
      wellPanel(
        fluidRow(
          column(8,
                 textInput("election_id", "Election ID",
                           placeholder = "e.g., city-council-2025")
          ),
          column(4, style = "margin-top: 25px;",
                 actionButton("generate_id", "Generate New ID",
                              icon = icon("wand-magic-sparkles"))
          )
        ),
        radioButtons("app_function", "Choose Function:",
                     choices = c("Create a new election" = "create",
                                 "Submit a ballot" = "ballot",
                                 "Process election results" = "process"),
                     selected = "create"),
        br(),
        actionButton("go_to_function", "Continue →",
                     class = "btn-primary btn-lg", icon = icon("arrow-right"))
      )
    )
  }
  
  create_ui <- function() {
    if (creation_page() == 1) create_page_1_ui() else create_page_2_ui()
  }
  
  create_page_1_ui <- function() {
    fluidPage(
      h3(paste("Creating New Election:", active_election_id())),
      p(
        "You can pre-fill this election by uploading a CSV of ranked ballots,",
        "typically from a Google Form."
      ),
      hr(),
      fileInput("ballot_file", "Optional: Upload CSV to Pre-fill Ballots",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain", ".csv")),
      actionButton("to_create_page_2", "Continue to Details",
                   class = "btn-primary")
    )
  }
  
  create_page_2_ui <- function() {
    fluidPage(
      h3(paste("Creating New Election:", active_election_id())),
      p("Define the parameters for your new election."),
      hr(),
      textInput("election_title", "Election Title",
                placeholder = "e.g., Annual Board Election"),
      textAreaInput("candidate_names", "Candidate Names (one per line)",
                    rows = 5),
      numericInput("seats", "Number of Seats to Elect", value = 3, min = 1,
                   step = 1),
      checkboxInput("allow_incomplete",
                    "Allow incomplete ballots",
                    value = FALSE),
      checkboxInput("allow_ties", "Allow tied ranks", value = FALSE),
      passwordInput("password", "Optional: Set an Admin Password"),
      hr(),
      actionButton("submit_creation", "Create Election",
                   class = "btn-success", icon = icon("check"))
    )
  }
  
  ballot_ui <- function() {
    req(election_config())
    config <- election_config()
    fluidPage(
      h3(paste("Submitting Ballot for:", config$title)),
      p(paste("Election ID:", config$unique_identifier)),
      hr(),
      uiOutput("ballot_interface"),
      hr(),
      actionButton("submit_ballot", "Submit Ballot",
                   class = "btn-success", icon = icon("person-booth"))
    )
  }
  
  process_ui <- function() {
    if (processing_page() == 1) process_page_1_ui() else process_page_2_ui()
  }
  
  process_page_1_ui <- function() {
    req(election_config())
    config <- election_config()
    fluidPage(
      h3(paste("Processing Results for:", config$title)),
      p(paste("Election ID:", config$unique_identifier)),
      hr(),
      strong("Candidates in this Election:"),
      p(HTML(paste(config$candidates, collapse = "<br>"))),
      hr(),
      radioButtons("tabulation_method_choice", "Choose Tabulation Method:",
                   choices = c("CPO STV" = "cpo_stv",
                               "Standard STV" = "stv"),
                   selected = "cpo_stv"),
      actionButton("to_process_page_2", "Continue", class = "btn-primary")
    )
  }
  
  process_page_2_ui <- function() {
    req(election_config())
    config <- election_config()
    fluidPage(
      h3(paste("Processing Results for:", config$title)),
      p(paste("Election ID:", config$unique_identifier)),
      p(paste("Tabulation method:", tabulation_method())),
      hr(),
      strong("Candidates in this Election:"),
      p(HTML(paste(config$candidates, collapse = "<br>"))),
      br(),
      div(id = "processing_inputs",
          numericInput("process_seats", "Number of seats to elect",
                       value = config$seats, min = 1),
          
          div(id = "tiebreak_options",
              rank_list(
                text = "Drag to order tie-break methods (Top = First)",
                labels = c("Borda" = "borda", "Random" = "random",
                           "STV" = "stv"),
                input_id = "tiebreak_methods"
              )
          ),
          
          numericInput("seed", "Random Seed for Tie-Breaking",
                       value = default_seed),
          actionButton("submit_processing", "Process Results",
                       class = "btn-info", icon = icon("calculator"))
      ),
      hr(),
      h4("Results:"),
      verbatimTextOutput("stv_results"),
      
      shinyjs::hidden(
        div(id = "post_processing_ui",
            br(),
            actionButton("return_home_from_results", "Return to Home",
                         class = "btn-primary", icon = icon("home"))
        )
      )
    )
  }
  
  end_ui <- function() {
    fluidPage(
      h3("Action Complete"),
      hr(),
      p(end_screen_message()),
      br(),
      actionButton("return_home", "Return to Home", class = "btn-primary",
                   icon = icon("home"))
    )
  }
  
  # -- Hub Logic ---------------------------------------------------------------
  
  observeEvent(input$generate_id, {
    election_count <- length(list.dirs(path = "Elections", recursive = FALSE))
    adjectives <- c("brave", "bright", "calm", "clever", "cool", "eager",
                    "epic", "fast", "fierce", "fine", "bold", "good", "grand",
                    "great", "happy", "jolly", "jovial", "keen", "kind",
                    "lively", "lucky", "magic", "merry", "neat", "noble",
                    "plucky", "proud", "quirky", "rapid", "regal", "sharp",
                    "shiny", "silent", "silly", "sleek", "slick", "smart",
                    "smooth", "snappy", "spunky", "stark", "stellar", "sturdy",
                    "super", "swift", "true", "vital", "vivid", "witty", "zany")
    colors <- c("red", "green", "blue", "yellow", "orange", "purple", "pink",
                "brown", "cyan", "magenta", "teal", "lime", "maroon", "navy",
                "silver", "olive", "aquamarine", "coral", "indigo", "violet")
    animals <- c(
      "lion", "tiger", "bear", "wolf", "fox", "eagle", "shark", "zebra",
      "rhino", "hippo", "monkey", "giraffe", "elephant", "dolphin", "whale",
      "penguin", "koala", "kangaroo", "panda", "leopard", "cheetah",
      "panther", "jaguar", "crocodile", "alligator", "snake", "lizard",
      "gorilla", "chimpanzee", "horse", "goat", "octopus", "squid",
      "starfish", "peacock", "ostrich", "swan", "owl", "camel", "badger",
      "hyena", "warthog", "meerkat", "lemur", "sloth", "armadillo", "beaver",
      "otter", "raccoon", "skunk", "porcupine", "hedgehog", "bat", "bison",
      "buffalo", "moose", "elk", "deer", "coyote", "falcon", "hawk",
      "vulture", "parrot", "toucan", "hummingbird", "woodpecker", "pelican",
      "seagull", "albatross", "crab", "lobster", "walrus", "seal",
      "jellyfish")
    
    new_id <- NULL; is_unique <- FALSE
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
  
  observeEvent(input$go_to_function, {
    id <- tolower(trimws(input$election_id))
    if (id == "") {
      showModal(show_error_modal("Election ID cannot be empty."))
      return()
    }
    election_path <- file.path("Elections", id)
    config_path <- file.path(election_path, "config.json")
    if (input$app_function == "create") {
      if (dir.exists(election_path)) {
        showModal(show_error_modal(
          "An election with this ID already exists. Please choose another."
        ))
      } else {
        active_election_id(id)
        current_ui("create")
      }
    } else {
      if (!dir.exists(election_path) || !file.exists(config_path)) {
        showModal(show_error_modal(
          "No election found with this ID. Please check or create a new one."
        ))
      } else {
        config <- fromJSON(config_path)
        election_config(config)
        active_election_id(id)
        current_ui(input$app_function)
      }
    }
  })
  
  # -- Create Election Logic & Page Flow ---------------------------------------
  
  observeEvent(input$to_create_page_2, {
    candidate_names_from_csv <- ""
    if (!is.null(input$ballot_file)) {
      ballot_df <- read.csv(input$ballot_file$datapath, check.names = FALSE)
      processed_df <- ballot_df %>% select(where(is.numeric)) %>%
        removeQuestion()
      candidates <- colnames(processed_df)
      candidate_names_from_csv <- paste(candidates, collapse = "\n")
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
      } else { NULL }
    )
    write_json(config, file.path(election_path, "config.json"),
               auto_unbox = TRUE)
    if (!is.null(input$ballot_file)) {
      ballot_df <- read.csv(input$ballot_file$datapath, check.names = FALSE)
      processed_df <- ballot_df %>% select(where(is.numeric)) %>%
        removeQuestion()
      for (i in 1:nrow(processed_df)) {
        ballot_data <- setNames(as.list(processed_df[i, ]), candidates)
        ballot_filename <- paste0("ballot_", UUIDgenerate(), ".json")
        write_json(ballot_data,
                   file.path(election_path, ballot_filename),
                   auto_unbox = TRUE)
      }
    }
    end_screen_message(paste("Successfully created election:",
                             input$election_title))
    current_ui("end")
  })
  
  # -- Submit Ballot Logic -----------------------------------------------------
  
  output$ballot_interface <- renderUI({
    config <- election_config()
    randomized_candidates <- sample(config$candidates)
    if (!config$allow_incomplete && !config.allow_ties) {
      initial_ballot_order(randomized_candidates)
      rank_list(
        text = "Rank candidates by dragging them into order (Top = 1st).",
        labels = randomized_candidates,
        input_id = "ranked_ballot_strict"
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
  
  process_and_save_ballot <- function() {
    config <- election_config()
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
               auto_unbox = TRUE)
    end_screen_message("Your ballot has been successfully submitted.")
    current_ui("end")
  }
  
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
  
  observeEvent(input$confirm_submit, {
    removeModal()
    process_and_save_ballot()
  })
  
  # -- Process Results Logic & Page Flow ---------------------------------------
  
  observeEvent(input$to_process_page_2, {
    tabulation_method(input$tabulation_method_choice)
    processing_page(2)
    shinyjs::delay(1, {
      shinyjs::toggle(id = "tiebreak_options",
                      condition = input$tabulation_method_choice == "cpo_stv")
    })
  })
  
  observeEvent(input$submit_processing, {
    election_path <- file.path("Elections", active_election_id())
    ballot_files <- list.files(election_path,
                               pattern = "ballot_.*\\.json", full.names = TRUE)
    if (length(ballot_files) == 0) {
      output$stv_results <- renderPrint({
        "No ballots have been submitted for this election."
      })
      return()
    }
    all_ballots <- lapply(ballot_files, function(f) {
      ballot <- fromJSON(f)
      ballot[ballot == 0] <- NA
      return(as.data.frame(ballot))
    })
    ballot_df <- bind_rows(all_ballots)
    
    results <- tryCatch({
      capture.output(
        if (tabulation_method() == "cpo_stv") {
          cpo_stv(ballot_df, seats = input$process_seats,
                  ties = input$tiebreak_methods, seed = input$seed)
        } else { # Standard stv
          stv(ballot_df, nseats = input$process_seats, seed = input$seed)
        }
      )
    }, error = function(e) {
      paste("An error occurred during calculation:", e$message)
    })
    
    output$stv_results <- renderPrint({
      cat(paste(results, collapse = "\n"))
    })
    shinyjs::hide("processing_inputs")
    shinyjs::show("post_processing_ui")
  })
  
  # -- End Screen/Return Home Logic -------------------------------------------
  
  reset_to_hub <- function() {
    active_election_id(NULL)
    election_config(NULL)
    end_screen_message("")
    shinyjs::reset("election_id")
    shinyjs::show("processing_inputs")
    output$stv_results <- renderText({ "" })
    creation_page(1)
    processing_page(1)
    current_ui("hub")
  }
  
  observeEvent(input$return_home_from_results, { reset_to_hub() })
  observeEvent(input$return_home, { reset_to_hub() })
  
}

# Run the application
shinyApp(ui = ui, server = server)

