# UI: createClientUI("createClient", "Create client"),
# Server: createClientServer("createClient", config)

createClientUI <- function(id, label = "Create client") {
  ns <- NS(id)
  tagList(
    textInput(ns("name"), "Organisation Name"),
    textInput(ns("email"), "Organisation Email"),
    selectInput(ns("time_zone"), "Time zone", choices = c(none = "--")),
    radioButtons(ns("tier"), "Client Tier", choices = c(Individual = 0, `Tier 1` = 1)),
    actionButton(ns("add"), "Add Client")
  )
}

createClientServer <- function(id, config) {
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
                  tier={input$tier}"
        result <- fromJSON(readLines(glue(query))) %>% as_tibble()
        d$return <- tibble(id = result$id,
                           name = input$name,
                           tier = as.numeric(input$tier))
      })
      
      #### Render UI ----
      
      updateSelectInput(session, "time_zone", choices = time_zones$name, selected = Sys.timezone())
      
      return(reactive({ d$return }))
      
    })
}
