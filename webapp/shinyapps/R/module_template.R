# UI: updateProjectUI("module1", "Template"),
# Server: updateProjectServer("module1", config, client_id)

moduleNameUI <- function(id, label = "Template Module") {
  ns <- NS(id)
  tagList(
    textInput(ns("text"), "text")
  )
}

moduleNameServer <- function(id, config, client_id) {
  moduleServer(
    id,
    function(input, output, session) {
      d <- reactive({input$text})
    })
}
