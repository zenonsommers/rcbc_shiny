# ==============================================================================
# mod-ballot-data.R — Ballot Data Reactive Helper
#
# Shared reactive function that loads and processes ballot JSON files
# from the active election directory. Used by plots, results, and
# processing modules.
# ==============================================================================

# Ballot Data Reactive
# Returns a data frame of all ballots for the active election, or NULL.
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
