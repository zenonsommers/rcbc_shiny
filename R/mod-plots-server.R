# ==============================================================================
# mod-plots-server.R — Candidate Preference Plot Server Logic
# ==============================================================================

# @export
mod_plots_server <- function(id, session) {
  moduleServer(id, function(input, output, session) {

    # Generate candidate preference plots reactively
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

      # Generate output renderers for each candidate's plot
      lapply(candidates, function(cand) {
        output_name <- paste0("plot_", gsub("\\s+|[^A-Za-z0-9]", "_", cand))

        output[[output_name]] <- renderPlot({
          current_ballot_df <- ballot_data_reactive()
          if (!cand %in% names(current_ballot_df)) {
            return(ggplot() +
                     labs(title = cand) +
                     annotate("text", x = 1, y = 1,
                              label = "Data error: Candidate column missing"))
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
                    plot.background = element_rect(fill = "transparent", colour = NA)) +
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
              scale_fill_manual(values = rank_colors, drop = FALSE, name = "Rank") +
              labs(title = cand, x = "Rank", y = "Number of Ballots") +
              theme_minimal(base_size = 10) +
              theme(axis.text.x = element_text(angle = 0),
                    plot.title = element_text(hjust = 0.5),
                    panel.background = element_rect(fill = "transparent", colour = NA),
                    plot.background = element_rect(fill = "transparent", colour = NA),
                    axis.text = element_text(color = ifelse(is_dark(), "white", "black")),
                    axis.title = element_text(color = ifelse(is_dark(), "white", "black")),
                    title = element_text(color = ifelse(is_dark(), "white", "black")),
                    legend.position = "none") +
              scale_x_discrete(drop = FALSE) +
              scale_y_continuous(
                limits = c(0, NA),
                breaks = scales::pretty_breaks(
                  n = max(3, max(plot_data$Count, na.rm = TRUE), na.rm = TRUE)
                )
              )
          }
        }, bg = "transparent")
      })

      # Generate the UI placeholders for the plots
      render_plot_ui <- function(page) {
        req(election_config())
        candidates <- election_config()$candidates
        num_candidates <- length(candidates)
        plot_width <- max(200, num_candidates * 25 + 60)

        plot_output_list <- lapply(candidates, function(cand) {
          plotOutput(paste0("plot_", gsub("\\s+|[^A-Za-z0-9]", "_", cand)),
                     height = "250px", width = paste0(plot_width, "px"))
        })
        div(style = "display: flex; flex-wrap: wrap; gap: 10px;",
            plot_output_list)
      }

      output$candidate_pref_plots_p1 <- renderUI({ render_plot_ui("p1") })
      output$candidate_pref_plots_p2 <- renderUI({ render_plot_ui("p2") })

    })

  })
}
