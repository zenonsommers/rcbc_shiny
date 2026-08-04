# ==============================================================================
# mod-electron-server.R — Reactive State Manager Server
#
# Wires up the reactive state defined in mod-electron.R at module level.
# ==============================================================================

# ModuleServer call — exports no public functions, only manages state
mod_electron_server <- function(id, session) {
  moduleServer(id, function(input, output, session) {
    # All state is already reactiveVal() defined in mod-electron.R
    # This moduleServer block exists to scope the state to the module
    # and to provide any state-management helpers that need reactive context
  })
}
