# ==============================================================================
# mod-edit.R — Edit Election UI
# ==============================================================================

# ==============================================================================
# Edit UI Functions
# ==============================================================================

# @export
mod_edit_ui <- function() {
  if (editing_page() == 1) edit_page_1_password_ui() else edit_page_2_main_ui()
}

edit_page_1_password_ui <- function() {
  tagList(
    h3(paste("Admin Access for:", active_election_id())),
    p("This election is password-protected. Please enter the admin password."),
    hr(),
    passwordInput("admin_password_check", "Admin Password"),
    footer_buttons(
      actionButton("return_home_general", "Return to Home",
                   class = "btn-secondary"),
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
    footer_buttons(
      actionButton("return_home_general", "Return to Home",
                   class = "btn-secondary"),
      span() # Placeholder for spacing if needed later
    )
  )
}
