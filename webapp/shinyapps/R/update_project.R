updateProjectUI <- function(id, label = "Update Project/Service/Task") {
  ns <- NS(id)
  tagList(
    textInput(ns("name"), "Name", ""),
    actionButton(ns("create"), "Add New")
  )
}

updateProjectServer <- function(id, config, client_id, pid = reactive({ NA })) {
  moduleServer(
    id,
    function(input, output, session) {
      
      #### Data wrangling  ----
      
      o <- reactiveValues()
      
      #### Actions ----
      
      observeEvent(input$create, {
        req(isTruthy(input$name))
        query <- "{config$api}/clients/{client_id}/projects/create?name='{input$name}'"
        if (!is.na(pid()))
          query <- paste(query, "parent_id={pid()}", sep = "&")
        new_id <- fromJSON(readLines(glue(query)))
        if (new_id$code == 201)
          o$return <- list(project = tibble(id = new_id$project_id,
                                            name = input$name,
                                            parent_id = pid()))
        if ("project_users_id" %in% names(new_id))
          o$return = append(o$return,
                            list(project_users = tibble(id = new_id$project_users_id,
                                                        project_id = new_id$project_id,
                                                        user_id = as.numeric(client_id),
                                                        project_admin = TRUE)))
        updateTextInput(session, "name", value = "")
      })
      
      return(reactive({ o$return }))
    })
}
