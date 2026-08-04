# ==============================================================================
# global.R — Package loads, constants, and shared helpers
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
library(ggplot2)
library(forcats)
library(RColorBrewer)

# --- Constants ---------------------------------------------------------------

default_seed <- 38725
light_bootswatch <- "flatly"
dark_bootswatch <- "darkly"
enable_custom_sortable_style <- TRUE

# Master lookup: display label (value) → function ID (name).
# sortable::rank_list returns the vector NAME, so these names are what
# cpo_stv.R's break_tie() checks against.
tiebreaker_choices <- c(
  "borda"   = "Borda",
  "random"  = "Random",
  "stv"     = "STV",
  "cpo_stv" = "CPO-STV"
)

# Theme definition (light default)
theme <- bs_theme(version = 5, base_font = font_google("Inter"),
                  bootswatch = light_bootswatch)

# --- Helpers -----------------------------------------------------------------

# Modal dialog helpers
show_error_modal <- function(message) {
  modalDialog(
    title = "Validation Error",
    p(message),
    easyClose = TRUE,
    footer = modalButton("Dismiss")
  )
}

show_success_modal <- function(message) {
  modalDialog(
    title = "Success",
    p(message),
    easyClose = TRUE,
    footer = modalButton("OK")
  )
}

# --- CSS rules ---------------------------------------------------------------

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

# --- File system setup -------------------------------------------------------

# Ensure the main directory for storing elections exists
if (!dir.exists("Elections")) {
  dir.create("Elections")
}
