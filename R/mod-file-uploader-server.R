# ==============================================================================
# mod-file-uploader-server.R — CSV Upload Handler Server Logic
# ==============================================================================

# @export
mod_file_uploader_server <- function(id, session) {
  moduleServer(id, function(input, output, session) {

    # Process CSV upload for pre-filling election candidates
    # Called from the create page to extract candidate names from CSV
    process_csv_upload <- function() {
      req(input$ballot_file)
      tryCatch({
        ballot_df <- read.csv(input$ballot_file$datapath, check.names = FALSE)
        processed_df <- ballot_df %>%
          select(where(is.numeric)) %>%
          removeQuestion()
        candidates <- colnames(processed_df)
        if (length(candidates) < 1) {
          stop("No numeric ranking columns found after processing.")
        }
        # Return candidate names
        list(candidates = paste(candidates, collapse = "\n"), df = processed_df)
      }, error = function(e) {
        showModal(show_error_modal(
          paste("Error processing CSV:", e$message,
                "Please check file format and ensure ranks are numeric.")
        ))
        reset("ballot_file")
        list(candidates = "", df = NULL)
      })
    }

  })
}
