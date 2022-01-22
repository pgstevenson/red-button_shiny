updateEventUI <- function(id, label = "Update event") {
  ns <- NS(id)
  tagList(
    hidden(textInput(ns("id"), "id")),
    textInput(ns("start"), "Start"),
    textInput(ns("end"), "End"),
    selectInput(ns("projects"), "Project:", c(`--` = "")),
    selectInput(ns("services"), "Service:", c(`--` = "")),
    selectInput(ns("tasks"), "Task:", c(`--` = "")),
    textInput(ns("description"), "Description", ""),
    actionButton(ns("update"), "Update")
  )
}

updateEventServer <- function(id, config, dat, client_id, user_id, event, proxy) {
  moduleServer(
    id,
    function(input, output, session) {
      #### Update Event ----
      
      d <- reactiveValues()
      d$projects <- d$services <- d$tasks <- NA
      
      #### Data wranging ----
      
      observe({
        req(dat)
        req(event())
        
        d$projects <- update_project_select(dat$projects, event()$project_id)
        d$services <- update_project_select(dat$projects, event()$service_id, event()$project_id)
        d$tasks <- update_project_select(dat$projects, event()$task_id, event()$service_id)
        
        updateSelectInput(session, "projects", choices = d$projects$choices, selected = d$projects$selected)
        updateSelectInput(session, "services", choices = d$services$choices, selected = d$services$selected)
        updateSelectInput(session, "tasks", choices = d$tasks$choices, selected = d$tasks$selected)
        
        updateTextInput(session, "id", value = event()$id)
        updateTextInput(session, "start", value = as.character(event()$start_time))
        updateTextInput(session, "end", value = as.character(event()$end_time))
        updateTextInput(session, "description", value = event()$description)
      })
      
      #### Actions ----
      
      observeEvent(input$update, {
        
        shiny::validate(need(isTruthy(input$start), "No start time"),
                        need(isTruthy(input$projects), "No project"),
                        need(isTruthy(input$services), "No service"),
                        need(isTruthy(input$tasks), "No task"))
        
        start_time <- format(as.POSIXct(input$start), iso_time)
        end_time <- format(as.POSIXct(input$end), iso_time)
        
        values_psql <- c()
        values_r <- list(id = as.numeric(input$id))
        if (start_time != format(event()$start_time, iso_time)) {
          values_psql["start_time"] <- paste(start_time, dat$info$time_zone)
          values_r$start_time <- c(as.POSIXct(input$start))
        }
        if (end_time != format(event()$end_time, iso_time)) {
          values_psql["end_time"] <- paste(end_time, dat$info$time_zone)
          values_r$end_time <- c(as.POSIXct(input$end))
        }
        if (as.numeric(input$tasks) != event()$task_id) {
          values_psql["task_id"] <- input$tasks
          values_r$task_id <- c(as.numeric(input$tasks))
        }
        if (input$description != event()$description) {
          values_psql["description"] <- glue("{input$description}")
          values_r$description <- c(input$description)
        }
        if (input$projects != event()$project)
          values_r$project <- c(filter(dat$projects, id == as.numeric(input$projects))$name)
        if (input$services != event()$service)
          values_r$service <- c(filter(dat$projects, id == as.numeric(input$services))$name)
        if (input$tasks != event()$task)
          values_r$task <- c(filter(dat$projects, id == as.numeric(input$tasks))$name)
        
        query <- paste("{config$api}/clients/{client_id}/users/{user_id}/events/{input$id}/update",
                       paste0(map2_chr(names(values_psql), values_psql, paste, sep = "="),
                              collapse = "&"),
                       sep = "?")
        
        result <- fromJSON(readLines(glue(query))) %>% as_tibble()
        d$return <- as_tibble(values_r)

        if (result$code == 200) {
          showNotification("Update Succesfull", type = "message")
        } else if (result$code == 422) {
          showNotification(result$value, type = "default")
        }
      })
      
      return(reactive({ d$return }))
    })
}
