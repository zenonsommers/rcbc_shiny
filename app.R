# ==============================================================================
# Vibe-Coded Shiny Election App
#
# To Run:
# 1. Install packages:
#    install.packages(c("bslib", "digest", "dplyr", "forcats", "gtools",
#    "ggplot2", "jsonlite", "magrittr", "RColorBrewer", "readxl", "shiny",
#    "shinyjs", "sortable", "stringi", "tibble", "tidyverse", "uuid", "vote"))
# 2. Download the latest version of the repo from
#    https://github.com/zenonsommers/rcbc_shiny/tree/main and extract it to a
#    local directory
# 3. Open R/RStudio and run `shiny::runApp()` in the directory where you
#    saved the files from the repo.
# ==============================================================================

# Load necessary libraries
library(shiny)
library(shinyjs)
library(uuid)
library(jsonlite)
library(sortable)
library(dplyr)
library(vote)
library(tibble)
library(bslib)
library(digest)
library(ggplot2) # Needed for plots
library(forcats) # Needed for factor levels in plots
library(RColorBrewer) # For color palettes

# Load the cpo_stv function
source("cpo_stv.R")

# --- Global Settings ---
default_seed <- 38725
light_bootswatch <- "flatly"
dark_bootswatch <- "darkly"
# Set to FALSE to disable custom styling for drag-and-drop elements
enable_custom_sortable_style <- TRUE

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

# Helper function for success pop-ups
show_success_modal <- function(message) {
  modalDialog(
    title = "Success",
    p(message),
    easyClose = TRUE,
    footer = modalButton("OK")
  )
}


# -- Theme Definition for Dark Mode --------------------------------------------

# Define a base theme with the desired font
theme <- bs_theme(version = 5, base_font = font_google("Inter"),
                  bootswatch = light_bootswatch)

# -- UI Definition -------------------------------------------------------------

# Base CSS rules
css_rules <- "
  body {
    padding-top: 5px;
    padding-bottom: 10px; /* Add padding to the bottom */
  }
  #darkModeToggle {
    color: var(--bs-body-color);
  }
  /* Style for details/summary collapsible element */
  details > summary {
    cursor: pointer;
    font-weight: bold;
    margin-bottom: 5px;
  }
  details > div {
    padding: 10px;
    border: 1px solid var(--bs-border-color);
    border-radius: var(--bs-border-radius);
    background-color: var(--bs-tertiary-bg);
  }
  .footer-buttons { /* Container for footer buttons */
    display: flex;
    justify-content: space-between; /* Space out buttons */
    margin-top: 20px;
  }
  /* Style for plot containers to allow horizontal scroll if needed */
  .plot-container {
    overflow-x: auto;
    overflow-y: hidden; /* Hide vertical scrollbar on container */
    padding-bottom: 15px; /* Add space for horizontal scrollbar */
  }
  /* Ensure modal content respects theme */
  .modal-body {
    color: var(--bs-body-color);
  }
  #delete_password_confirm {
     background-color: var(--bs-input-bg);
     color: var(--bs-body-color);
     border-color: var(--bs-input-border-color);
  }
"

# Conditionally add the sortable style if the flag is TRUE
if (enable_custom_sortable_style) {
  css_rules <- paste(css_rules, "
    /* Target our custom-classed sortable items */
    .custom-rank-list .rank-list-item {
      background-color: var(--bs-tertiary-bg); /* Use a contrast color */
      border: 1px solid var(--bs-border-color); /* Explicitly set border */
      color: var(--bs-body-color); /* Use main text color */

      /* Add back structural styles */
      padding: 6px 12px;
      margin-bottom: 4px;
      border-radius: var(--bs-border-radius);
    }
  ")
}


ui <- fluidPage(
  theme = bs_theme_update(theme, bootswatch = light_bootswatch),
  useShinyjs(),
  
  # Custom CSS for padding, dark mode toggle, and sortable items
  tags$head(tags$style(HTML(css_rules))),
  
  # Dark mode toggle switch
  div(style = "position: absolute; top: 10px; right: 20px; z-index: 1000;",
      actionButton("darkModeToggle", "",
                   icon = icon("sun"),
                   style = "border: none; background: transparent;")
  ),
  
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
  is_dark <- reactiveVal(FALSE)
  
  # Page managers for multi-step processes
  creation_page <- reactiveVal(1)
  processing_page <- reactiveVal(1)
  editing_page <- reactiveVal(1)
  
  # To store the chosen tabulation method
  tabulation_method <- reactiveVal("cpo_stv")
  
  # To store the last chosen function from the hub
  last_function_choice <- reactiveVal("create")
  
  # -- Dark Mode Logic ---------------------------------------------------------
  
  observeEvent(input$darkModeToggle, {
    is_dark(!is_dark())
    
    # Update the icon based on the new mode
    if (is_dark()) {
      updateActionButton(session, "darkModeToggle", icon = icon("moon"))
    } else {
      updateActionButton(session, "darkModeToggle", icon = icon("sun"))
    }
    
    # Update the theme
    session$setCurrentTheme(
      bs_theme_update(theme, bootswatch = if (is_dark()) dark_bootswatch
                      else light_bootswatch)
    )
  })
  
  # -- Dynamic UI Rendering ----------------------------------------------------
  
  output$main_ui <- renderUI({
    switch(current_ui(),
           "hub"     = hub_ui(),
           "create"  = create_ui(),
           "ballot"  = ballot_ui(),
           "process" = process_ui(),
           "edit"    = edit_ui(),
           "end"     = end_ui()
    )
  })
  
  # -- UI Components -----------------------------------------------------------
  
  hub_ui <- function() {
    tagList(
      h3("Welcome"),
      p("Select an action and provide an Election ID to begin."),
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
                     selected = last_function_choice()),
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
    tagList(
      h3(paste("Creating New Election:", active_election_id())),
      p(
        "You can pre-fill this election by uploading a CSV of ranked ballots,",
        "typically from a Google Form."
      ),
      hr(),
      fileInput("ballot_file", "Optional: Upload CSV to Pre-fill Ballots",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain", ".csv")),
      div(class = "footer-buttons",
          actionButton("return_home_general", "Return to Home",
                       class="btn-secondary"),
          actionButton("to_create_page_2", "Continue to Details",
                       class = "btn-primary")
      )
    )
  }
  
  create_page_2_ui <- function() {
    tagList(
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
      div(class = "footer-buttons",
          actionButton("return_home_general", "Return to Home",
                       class="btn-secondary"),
          actionButton("submit_creation", "Create Election",
                       class = "btn-success", icon = icon("check"))
      )
    )
  }
  
  ballot_ui <- function() {
    req(election_config())
    config <- election_config()
    tagList(
      h3(paste("Submitting Ballot for:", config$title)),
      p(paste("Election ID:", config$unique_identifier)),
      hr(),
      uiOutput("ballot_interface"),
      hr(),
      div(class = "footer-buttons",
          actionButton("return_home_general", "Return to Home",
                       class="btn-secondary"),
          actionButton("submit_ballot", "Submit Ballot",
                       class = "btn-success", icon = icon("person-booth"))
      )
    )
  }
  
  process_ui <- function() {
    if (processing_page() == 1) process_page_1_ui() else process_page_2_ui()
  }
  
  process_page_1_ui <- function() {
    req(election_config())
    config <- election_config()
    tagList(
      h3(paste("Processing Results for:", config$title)),
      p(paste("Election ID:", config$unique_identifier)),
      hr(),
      strong("Candidates in this Election:"),
      p(HTML(paste(config$candidates, collapse = "<br>"))),
      textOutput("ballot_count_p1"),
      tags$details(
        tags$summary("View Recorded Ballots"),
        div(style = "max-height: 300px; overflow-y: auto;",
            tableOutput("ballot_table_p1")
        )
      ),
      tags$details(
        tags$summary("View Preferences by Candidate"),
        div(class = "plot-container", # Add class for scrolling
            uiOutput("candidate_pref_plots_p1")
        )
      ),
      hr(),
      radioButtons("tabulation_method_choice", "Choose Tabulation Method:",
                   choices = c("CPO-STV" = "cpo_stv",
                               "Standard STV" = "stv",
                               "Borda Scores" = "borda",
                               "Borda with Tiebreakers" = "borda_tb"),
                   selected = "cpo_stv"),
      div(class = "footer-buttons",
          actionButton("return_home_general", "Return to Home",
                       class="btn-secondary"),
          actionButton("to_process_page_2", "Continue", class = "btn-primary")
      )
    )
  }
  
  process_page_2_ui <- function() {
    req(election_config())
    config <- election_config()
    
    method_name <- switch(tabulation_method(),
                          "cpo_stv" = "CPO-STV",
                          "stv" = "Standard STV",
                          "borda" = "Borda Scores",
                          "borda_tb" = "Borda with Tiebreakers")
    
    tagList(
      h3(paste("Processing Results for:", config$title)),
      p(paste("Election ID:", config$unique_identifier)),
      p(paste("Tabulation method:", method_name)),
      hr(),
      strong("Candidates in this Election:"),
      p(HTML(paste(config$candidates, collapse = "<br>"))),
      textOutput("ballot_count_p2"),
      tags$details(
        tags$summary("View Recorded Ballots"),
        div(style = "max-height: 300px; overflow-y: auto;",
            tableOutput("ballot_table_p2")
        )
      ),
      tags$details(
        tags$summary("View Preferences by Candidate"),
        div(class = "plot-container", # Add class for scrolling
            uiOutput("candidate_pref_plots_p2")
        )
      ),
      br(),
      div(id = "processing_inputs",
          div(id = "seats_option",
              numericInput("process_seats", "Number of seats to elect",
                           value = config$seats, min = 1)
          ),
          
          div(id = "tiebreak_options_cpo",
              rank_list(
                text = "Drag to order CPO-STV tie-break methods",
                labels = c("Borda" = "borda", "Random" = "random",
                           "STV" = "stv"),
                input_id = "tiebreak_methods_cpo",
                class = "custom-rank-list"
              )
          ),
          
          div(id = "tiebreak_options_borda_tb",
              rank_list(
                text = "Drag to order Borda tie-break methods",
                labels = c("CPO-STV" = "cpo_stv", "Random" = "random",
                           "STV" = "stv"),
                input_id = "tiebreak_methods_borda_tb",
                class = "custom-rank-list"
              )
          ),
          
          div(id = "seed_option",
              numericInput("seed", "Random Seed for Tie-Breaking",
                           value = default_seed)
          ),
          
          checkboxInput("verbose_output", "Show verbose output",
                        value = FALSE),
          
          div(class = "footer-buttons",
              actionButton("return_home_general", "Return to Home",
                           class="btn-secondary"),
              actionButton("submit_processing", "Process Results",
                           class = "btn-info", icon = icon("calculator"))
          )
      ),
      hr(),
      h4("Results:"),
      verbatimTextOutput("text_results"),
      tableOutput("table_results"),
      
      shinyjs::hidden(
        div(id = "post_processing_ui", class = "footer-buttons",
            # Buttons are already spaced due to footer-buttons class
            span(), # Pushes button right
            actionButton("return_home_general", "Return to Home",
                         class = "btn-primary", icon = icon("home"))
        )
      )
    )
  }
  
  edit_ui <- function() {
    if (editing_page() == 1) edit_page_1_password_ui() else edit_page_2_main_ui()
  }
  
  edit_page_1_password_ui <- function() {
    tagList(
      h3(paste("Admin Access for:", active_election_id())),
      p("This election is password-protected. Please enter the admin password."),
      hr(),
      passwordInput("admin_password_check", "Admin Password"),
      div(class = "footer-buttons",
          actionButton("return_home_general", "Return to Home",
                       class="btn-secondary"),
          actionButton("submit_password_check", "Submit", class = "btn-primary")
      ),
      # Ensure keypress listener is removed before adding a new one
      shinyjs::runjs("
        $(document).off('keypress', '#admin_password_check').on('keypress', '#admin_password_check', function(e) {
          if (e.which == 13) {
            $('#submit_password_check').click();
          }
        });
      ")
    )
  }
  
  edit_page_2_main_ui <- function() {
    req(election_config())
    config <- election_config()
    
    tagList(
      h3(paste("Editing Election:", config$title)),
      p(paste("Election ID:", config$unique_identifier)),
      hr(),
      
      wellPanel(
        h4("Election Status"),
        checkboxInput("accepting_responses", "Accepting new responses",
                      value = config$accepting_responses),
        p(
          class = "text-muted",
          "Uncheck this to prevent new ballots from being submitted."
        )
      ),
      
      wellPanel(
        h4("Change Admin Password"),
        passwordInput("old_password", "Old Password (leave blank if none)"),
        passwordInput("new_password", "New Password"),
        passwordInput("confirm_new_password", "Confirm New Password"),
        actionButton("change_password", "Change Password",
                     class = "btn-warning")
      ),
      
      hr(),
      
      div(style = "text-align: center; margin-top: 20px;",
          actionButton("delete_election_confirm_prompt", "Delete Election",
                       class = "btn-danger", icon = icon("trash"))
      ),
      
      br(),
      div(class = "footer-buttons",
          actionButton("return_home_general", "Return to Home",
                       class="btn-secondary"),
          span() # Placeholder for spacing if needed later
      )
    )
  }
  
  end_ui <- function() {
    tagList(
      h3("Action Complete"),
      hr(),
      p(end_screen_message()),
      br(),
      actionButton("return_home_general", "Return to Home",
                   class = "btn-primary", icon = icon("home"))
    )
  }
  
  # -- Helper Reactive for Ballot Data -----------------------------------------
  
  ballot_data_reactive <- reactive({
    req(active_election_id(), election_config())
    election_path <- file.path("Elections", active_election_id())
    ballot_files <- list.files(election_path,
                               pattern = "ballot_.*\\.json", full.names = TRUE)
    
    if (length(ballot_files) == 0) {
      return(NULL)
    }
    
    config_candidates <- election_config()$candidates
    if(is.null(config_candidates)) return(NULL) # Ensure candidates are loaded
    
    all_ballots_list <- lapply(ballot_files, function(f) {
      ballot_list <- tryCatch(fromJSON(f), error = function(e) {
        warning(paste("Could not read or parse ballot file:", f, "-", e$message))
        NULL
      })
      if (is.null(ballot_list)) return(NULL) # Skip corrupted/unreadable files
      
      # Convert ballot values to numeric, coercing errors to NA
      # and ensure names match config_candidates
      processed_ballot <- vector("list", length(config_candidates))
      names(processed_ballot) <- config_candidates
      
      for (cand in config_candidates) {
        val <- ballot_list[[cand]] # Access by name
        if (is.null(val)) {
          processed_ballot[[cand]] <- NA_real_ # Use NA_real_ for numeric NA
        } else {
          num_val <- suppressWarnings(as.numeric(val))
          # Set ranks outside valid range or non-numeric/zero to NA
          if (is.na(num_val) || num_val == 0 || num_val < 1 || num_val > length(config_candidates)) {
            processed_ballot[[cand]] <- NA_real_
          } else {
            processed_ballot[[cand]] <- num_val
          }
        }
      }
      return(processed_ballot) # Return the processed list
    })
    
    # Filter out NULLs from failed reads
    all_ballots_valid_lists <- Filter(Negate(is.null), all_ballots_list)
    if (length(all_ballots_valid_lists) == 0) return(NULL)
    
    # Convert the list of lists to a data frame
    combined_df <- tryCatch(bind_rows(all_ballots_valid_lists), error = function(e){
      warning(paste("Error combining ballots:", e$message))
      NULL # Return NULL if bind_rows fails
    })
    
    return(combined_df)
  })
  
  
  # -- Output Renderers for Ballot Count and Table -----------------------------
  
  # Helper function to format ballot table for display
  format_ballot_table <- function(df) {
    if (is.null(df) || !is.data.frame(df)) return(NULL)
    # Apply formatting to numeric columns only
    df %>% mutate(across(where(is.numeric), ~ sprintf("%.0f", .)))
  }
  
  output$ballot_count_p1 <- renderText({
    df <- ballot_data_reactive()
    count <- if (is.null(df) || !is.data.frame(df)) 0 else nrow(df)
    paste("Number of ballots recorded:", count)
  })
  
  output$ballot_table_p1 <- renderTable({
    format_ballot_table(ballot_data_reactive())
  }, na = "Unranked", rownames = TRUE)
  
  output$ballot_count_p2 <- renderText({
    df <- ballot_data_reactive()
    count <- if (is.null(df) || !is.data.frame(df)) 0 else nrow(df)
    paste("Number of ballots recorded:", count)
  })
  
  output$ballot_table_p2 <- renderTable({
    format_ballot_table(ballot_data_reactive())
  }, na = "Unranked", rownames = TRUE)
  
  # -- Output Renderers for Candidate Preference Plots -------------------------
  
  observe({
    req(election_config(), ballot_data_reactive())
    ballot_df <- ballot_data_reactive()
    candidates <- election_config()$candidates
    num_candidates <- length(candidates)
    
    if (is.null(ballot_df) || nrow(ballot_df) == 0 || ncol(ballot_df) == 0) {
      output$candidate_pref_plots_p1 <- renderUI({ p("No valid ballots recorded yet.") })
      output$candidate_pref_plots_p2 <- renderUI({ p("No valid ballots recorded yet.") })
      return()
    }
    
    # Define a color palette
    max_ranks <- num_candidates
    palette_name <- "Paired"
    num_colors_needed <- max_ranks
    if (num_colors_needed <= 12) {
      rank_colors <- brewer.pal(max(3, num_colors_needed), palette_name)[1:num_colors_needed]
    } else {
      rank_colors <- colorRampPalette(brewer.pal(12, palette_name))(num_colors_needed)
    }
    names(rank_colors) <- as.character(1:max_ranks)
    
    
    lapply(candidates, function(cand) {
      output_name <- paste0("plot_", gsub("\\s+|[^A-Za-z0-9]", "_", cand))
      
      output[[output_name]] <- renderPlot({
        current_ballot_df <- ballot_data_reactive()
        if (!cand %in% names(current_ballot_df)) {
          return(ggplot() + labs(title = cand) + annotate("text", x=1, y=1, label="Data error: Candidate column missing"))
        }
        
        ranks <- current_ballot_df[[cand]][!is.na(current_ballot_df[[cand]])]
        
        if (length(ranks) == 0) {
          # Empty plot for no ranks
          ggplot() +
            labs(title = cand, x = "Rank", y = "Number of Ballots") +
            scale_x_continuous(breaks = 1:num_candidates,
                               limits = c(0.5, num_candidates + 0.5)) +
            theme_minimal(base_size = 10) +
            theme(plot.title = element_text(hjust = 0.5),
                  panel.background = element_rect(fill = "transparent", colour = NA),
                  plot.background = element_rect(fill = "transparent", colour = NA)
            ) +
            annotate("text", x = (num_candidates + 1) / 2, y = 0,
                     label = "No ranks received", hjust = 0.5, vjust = 0,
                     color = ifelse(is_dark(), "white", "black"))
        } else {
          # Plotting logic for candidates with ranks
          rank_factor <- factor(ranks, levels = 1:num_candidates)
          rank_counts <- table(rank_factor)
          plot_data <- data.frame(
            Rank = factor(names(rank_counts), levels = 1:num_candidates),
            Count = as.integer(rank_counts)
          )
          
          ggplot(plot_data, aes(x = Rank, y = Count, fill = Rank)) +
            geom_bar(stat = "identity") +
            scale_fill_manual(values = rank_colors, drop = FALSE,
                              name = "Rank") +
            labs(title = cand, x = "Rank", y = "Number of Ballots") +
            theme_minimal(base_size = 10) +
            theme(axis.text.x = element_text(angle = 0),
                  plot.title = element_text(hjust = 0.5),
                  panel.background = element_rect(fill = "transparent", colour = NA),
                  plot.background = element_rect(fill = "transparent", colour = NA),
                  axis.text = element_text(color = ifelse(is_dark(), "white", "black")),
                  axis.title = element_text(color = ifelse(is_dark(), "white", "black")),
                  title = element_text(color = ifelse(is_dark(), "white", "black")),
                  legend.position = "none" # Hide legend
            ) +
            scale_x_discrete(drop = FALSE) +
            scale_y_continuous(
              limits = c(0, NA),
              breaks = scales::pretty_breaks(
                n = max(3, max(plot_data$Count, na.rm = TRUE), na.rm = TRUE)
              )
            )
        }
      }, bg="transparent")
    })
    
    # Generate the UI placeholders for the plots
    output$candidate_pref_plots_p1 <- renderUI({
      req(election_config())
      candidates <- election_config()$candidates
      num_candidates <- length(candidates)
      plot_width <- max(200, num_candidates * 25 + 60)
      
      plot_output_list <- lapply(candidates, function(cand) {
        plotOutput(paste0("plot_", gsub("\\s+|[^A-Za-z0-9]", "_", cand)),
                   height = "250px", width = paste0(plot_width, "px"))
      })
      div(style="display: flex; flex-wrap: wrap; gap: 10px;",
          plot_output_list)
    })
    
    output$candidate_pref_plots_p2 <- renderUI({
      req(election_config())
      candidates <- election_config()$candidates
      num_candidates <- length(candidates)
      plot_width <- max(200, num_candidates * 25 + 60)
      
      plot_output_list <- lapply(candidates, function(cand) {
        plotOutput(paste0("plot_", gsub("\\s+|[^A-Za-z0-9]", "_", cand)),
                   height = "250px", width = paste0(plot_width, "px"))
      })
      div(style="display: flex; flex-wrap: wrap; gap: 10px;",
          plot_output_list)
    })
    
  })
  
  
  # -- Hub Logic ---------------------------------------------------------------
  
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
    last_function_choice(input$app_function)
    id <- tolower(trimws(input$election_id))
    if (id == "") {
      showModal(show_error_modal("Election ID cannot be empty."))
      return()
    }
    election_path <- file.path("Elections", id)
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
      if(is.null(config)) return()
      
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
      active_election_id(id)
      
      if (input$app_function == "edit") {
        if (!is.null(config$password_hash) && config$password_hash != "") {
          editing_page(1)
        } else {
          editing_page(2)
        }
      }
      
      current_ui(input$app_function)
      
    } else if (input$app_function == "create") {
      if (dir.exists(election_path)) {
        showModal(show_error_modal(
          "An election with this ID already exists. Please choose another."
        ))
      } else {
        active_election_id(id)
        current_ui("create")
      }
    }
  })
  
  # -- Create Election Logic & Page Flow ---------------------------------------
  
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
      } else { NULL },
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
  
  # -- Submit Ballot Logic -----------------------------------------------------
  
  output$ballot_interface <- renderUI({
    config <- election_config()
    randomized_candidates <- sample(config$candidates)
    if (!config$allow_incomplete && !config$allow_ties) {
      initial_ballot_order(randomized_candidates)
      rank_list(
        text = paste("Rank candidates by dragging them into order from",
                     "most preferred (top) to least preferred (bottom)."),
        labels = randomized_candidates,
        input_id = "ranked_ballot_strict",
        class = "custom-rank-list"
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
    
    # Re-check accepting responses status right before saving
    if (is.null(config$accepting_responses)) config$accepting_responses <- TRUE
    if (!config$accepting_responses) {
      showModal(show_error_modal(
        "This election is not currently accepting responses."
      ))
      return()
    }
    
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
               auto_unbox = TRUE, na = "null") # Ensure NAs are handled
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
  
  # -- Edit Election Logic -----------------------------------------------------
  
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
    } else { NULL }
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
  
  observeEvent(input$confirm_delete, {
    req(input$delete_password_confirm)
    config <- election_config()
    
    # Check if password is required and correct
    password_required <- !is.null(config$password_hash) && config$password_hash != ""
    if(password_required) {
      hashed_input <- digest(input$delete_password_confirm, "sha256")
      if (hashed_input != config$password_hash) {
        showModal(show_error_modal("Incorrect password. Deletion cancelled."))
        return()
      }
    } else {
      # If no password set, check if user entered anything (they shouldn't have)
      if(input$delete_password_confirm != "") {
        showModal(show_error_modal("No password is set for this election. Leave field blank."))
        return()
      }
    }
    
    election_path <- file.path("Elections", active_election_id())
    
    tryCatch({
      unlink(election_path, recursive = TRUE, force = TRUE)
      removeModal()
      end_screen_message(paste("Election", sQuote(active_election_id()),
                               "successfully deleted."))
      current_ui("end")
    }, error = function(e){
      removeModal() # Close confirmation modal first
      showModal(show_error_modal(paste("Failed to delete election:", e$message)))
    })
  })
  
  
  # -- Process Results Logic & Page Flow ---------------------------------------
  
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
      
      current_seed <- if(method == "borda") default_seed else input$seed
      if(is.na(current_seed) || is.null(current_seed)){
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
      } else { # Standard stv
        config <- election_config()
        stv(ballot_df, nseats = input$process_seats,
            verbose = verbose_flag, seed = current_seed,
            equal.ranking = config$allow_ties)
      }
      
      # Close connections
      sink(type = "message"); sink(type = "output")
      close(output_con)
      
    }, error = function(e) {
      # Ensure sinks are closed on error too
      if(sink.number(type="message") > 0) sink(type="message")
      if(sink.number(type="output") > 0) sink(type="output")
      if(exists("output_con") && isOpen(output_con)) close(output_con)
      
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
  
  # -- End Screen/Return Home Logic -------------------------------------------
  
  reset_to_hub <- function() {
    last_election_id <- active_election_id()
    
    active_election_id(NULL)
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
      updateTextInput(session, "election_id", value = last_election_id)
    })
  }
  
  observeEvent(input$return_home_general, { reset_to_hub() })
  
}

# Run the application
shinyApp(ui = ui, server = server)

