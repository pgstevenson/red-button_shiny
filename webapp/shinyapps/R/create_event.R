createEventUI <- function(id, label = "Create (start/stop) Event") {
  ns <- NS(id)
  tagList(
    selectInput(ns("projects"), "Project:", c(`--` = "")),
    selectInput(ns("services"), "Service:", c(`--` = "")),
    selectInput(ns("tasks"), "Task:", c(`--` = "")),
    textInput(ns("description"), "Description", ""),
    actionButton(ns("start"), "Start", class="btn btn-success"),
    hidden(actionButton(ns("back"), "Back", class="btn btn-light")),
    hidden(actionButton(ns("stop"), "Stop", class="btn btn-danger")),
    hidden(actionButton(ns("startNew"), "Start New", class="btn btn-info")),
  )
}

createEventServer <- function(id, config, dat, client_id, user_id) {
  moduleServer(
    id,
    function(input, output, session) {

      d <- reactiveValues()
      d$create <- d$stop <- NA
      d$new_flag <- FALSE
      
      observe({
        req(dat$projects)
        d$projects <- filter(dat$projects, is.na(parent_id))
      })
      
      observe({
        req(dat$events)
        if (is.na(slice_head(dat$events, n = 1)$end_time))
          d$active_id <- slice_head(dat$events, n = 1)$id
      })
      
      #### Actions ----
      
      observeEvent(input$projects, {
        req(dat$projects)
        d$services <- filter(dat$projects, parent_id == input$projects)
      })
      observeEvent(input$services, {
        req(dat$projects)
        d$tasks <- filter(dat$projects, parent_id == input$services)
      })

      observeEvent(input$start, {

        shiny::validate(need(isTruthy(input$projects), "No project"),
                        need(isTruthy(input$services), "No service"),
                        need(isTruthy(input$tasks), "No task"))
        
        # Starting a new event without stopping the previous then end previous before inserting new row
        d$stop <- NA
        if (d$new_flag) {
          fromJSON(readLines(glue("{config$api}/clients/{client_id}/users/{user_id}/events/{d$active_id}/stop")))
          d$stop <- tibble(id = d$active_id, end_time = Sys.time())
        }
        query <- "{config$api}/clients/{client_id}/users/{user_id}/events/create?task_id={input$tasks}"
        if (isTruthy(input$description))
          query <- paste(query, "description={input$description}", sep = "&")
        result <- fromJSON(readLines(glue(query))) %>% as_tibble()
        
        d$active_id <- result$event_id
        d$create <- tibble(id = d$active_id,
                           start_time = Sys.time(),
                           project = dat$projects[dat$projects$id == input$projects,]$name,
                           service = dat$projects[dat$projects$id == input$services,]$name,
                           task = dat$projects[dat$projects$id == input$tasks,]$name,
                           task_id = input$tasks,
                           description = ifelse(isTruthy(input$description),
                                                input$description,
                                                "")) %>%
          mutate(across(contains("id"), as.numeric))
        
      })

      observeEvent(input$stop, {
        req(d$active_id)
        query <- "{config$api}/clients/{client_id}/users/{user_id}/events/{d$active_id}/stop"
        fromJSON(readLines(glue(query)))
        d$stop <- tibble(id = d$active_id, end_time = Sys.time())
        walk(c("projects", "services", "tasks", "description", "start"), shinyjs::show)
        walk(c("stop", "startNew"), shinyjs::hide)
        d$active_id <- NA
        d$create <- NA
      })

      observeEvent(input$startNew, {
        walk(c("projects", "services", "tasks", "description", "start", "back"), shinyjs::show)
        walk(c("stop", "startNew"), shinyjs::hide)
        d$new_flag <- TRUE
      })

      observeEvent(input$back, {
        ui_reset_event(session)
        d$new_flag <- FALSE
      })

      #### Render UI ----

      observeEvent(d$active_id, {
        req(d$active_id)
        ui_reset_event(session)
      })
      
      observeEvent(d$projects, {
        req(d$projects)
        updateSelectInput(session, "projects", choices = c(c(`--` = ""), setNames(d$projects$id, d$projects$name)))
      })

      observeEvent(d$services, {
        req(d$services)
        updateSelectInput(session, "services", choices = c(c(`--` = ""), setNames(d$services$id, d$services$name)))
      })

      observeEvent(d$tasks, {
        req(d$tasks)
        updateSelectInput(session, "tasks", choices = c(c(`--` = ""), setNames(d$tasks$id, d$tasks$name)))
      })

      return(reactive({ list(create = d$create,
                             stop = d$stop) }))
    })
}
