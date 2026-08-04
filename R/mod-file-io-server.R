# ==============================================================================
# mod-file-io-server.R — File I/O Server Logic
# ==============================================================================

# @export
mod_file_io_server <- function(id, session) {
  moduleServer(id, function(input, output, session) {

    # Election directory path helper
    election_path <- function(id) {
      file.path("Elections", id)
    }

    # Save a single ballot to disk
    # @param ballot_data list with candidate names as keys and ranks as values
    # @param id election ID string
    save_ballot <- function(ballot_data, id) {
      ballot_filename <- paste0("ballot_", UUIDgenerate(), ".json")
      write_json(ballot_data,
                 file.path(election_path(id), ballot_filename),
                 auto_unbox = TRUE, na = "null")
    }

    # Load all ballots for an election
    # @param id election ID string
    # @return list of ballot data frames, or empty list
    load_ballots <- function(id) {
      election_dir <- election_path(id)
      ballot_files <- list.files(election_dir,
                                 pattern = "ballot_.*\\.json",
                                 full.names = TRUE)
      if (length(ballot_files) == 0) return(list())

      lapply(ballot_files, function(f) {
        tryCatch(fromJSON(f), error = function(e) {
          warning(paste("Could not read ballot file:", f, "-", e$message))
          NULL
        })
      })
    }

  })
}
