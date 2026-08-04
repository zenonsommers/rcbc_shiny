# ==============================================================================
# mod-results.R — Results Display Helpers
# ==============================================================================

# ==============================================================================
# Results Server Function
# ==============================================================================

# @export
mod_results_server <- function(id, session) {
  moduleServer(id, function(input, output, session) {

    # Render ballot count text
    output$ballot_count_p1 <- renderText({
      df <- ballot_data_reactive()
      count <- if (is.null(df) || !is.data.frame(df)) 0 else nrow(df)
      paste("Number of ballots recorded:", count)
    })

    output$ballot_count_p2 <- renderText({
      df <- ballot_data_reactive()
      count <- if (is.null(df) || !is.data.frame(df)) 0 else nrow(df)
      paste("Number of ballots recorded:", count)
    })

    # Format ballot table for display
    format_ballot_table <- function(df) {
      if (is.null(df) || !is.data.frame(df)) return(NULL)
      # Apply formatting to numeric columns only
      df %>% mutate(across(where(is.numeric), ~ sprintf("%.0f", .)))
    }

    # Render ballot table
    output$ballot_table_p1 <- renderTable({
      format_ballot_table(ballot_data_reactive())
    }, na = "Unranked", rownames = TRUE)

    output$ballot_table_p2 <- renderTable({
      format_ballot_table(ballot_data_reactive())
    }, na = "Unranked", rownames = TRUE)

  })
}
