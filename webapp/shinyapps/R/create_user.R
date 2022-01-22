# UI: createClientUI("createClient", "Create client"),
# Server: createClientServer("createClient", config)

createUserUI <- function(id, label = "Create client") {
  ns <- NS(id)
  tagList(
    textInput(ns("name"), "Name"),
    textInput(ns("email"), "Email"),
    selectInput(ns("time_zone"), "Time zone", choices = c(none = "--")),
    radioButtons(ns("admin"), "Org. Admin.", choices = c("Yes" = "TRUE", "No" = "FALSE"), selected = "FALSE"),
    actionButton(ns("add"), "Add Client")
  )
}

createUserServer <- function(id, config) {
  moduleServer(
    id,
    function(input, output, session) {
      
      #### Data wrangling ----
      
      time_zones <- readRDS("data/time_zones.rds")
      d <- reactiveValues()
      
      #### Actions ----
      
      observeEvent(input$add, {
        query <- "{config$api}/clients/create?\\
                  name={URLencode(input$name, reserved = TRUE)}&\\
                  email={URLencode(input$email, reserved = TRUE)}&\\
                  time_zone={URLencode(input$time_zone, reserved = TRUE)}&\\
                  admin={input$admin}"
        result <- fromJSON(readLines(glue(query))) %>% as_tibble()
        d$return <- tibble(id = result$id,
                           name = input$name,
                           email = input$email)
      })
      
      #### Render UI ----
      
      updateSelectInput(session, "time_zone", choices = time_zones$name, selected = Sys.timezone())
      
      return(reactive({ d$return }))
      
    })
}
