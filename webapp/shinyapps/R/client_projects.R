clientProjectsUI <- function(id, label = "Client projets") {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(listProjectsUI(ns("project"), "List Projects"),
          updateProjectUI(ns("project"), "Create Project"),
          title = "Project",
          width = 4),
      box(listProjectsUI(ns("service"), "List Services"),
          updateProjectUI(ns("service"), "Create Service"),
          title = "Service",
          width = 4),
      box(listProjectsUI(ns("task"), "List Tasks"),
          updateProjectUI(ns("task"), "Create Task"),
          title = "Task",
          width = 4)
    ),
    fluidRow(
      box(dataTableOutput(ns("projectUsers")),
          hr(),
          p("Add User to Project"),
          selectInput(ns("users"), "User", ""),
          actionButton(ns("addUser"), "Add To Project"),
          title = "Project Users",
          width = "12")
    )
  )
}

clientProjectsServer <- function(id, config, client_id, dat) {
  moduleServer(
    id,
    function(input, output, session) {
      
      #### Data wrangling ----
      
      d <- reactiveValues(project = reactive({ NULL }),
                          service = reactive({ NULL }),
                          task = reactive({ NULL }))
      
      #### Actions ----
      
      new_project <- updateProjectServer("project", config, client_id)
      new_service <- updateProjectServer("service", config, client_id, d$project)
      new_task <- updateProjectServer("task", config, client_id, d$service)
      
      observe({
        d$project <- listProjectsServer("project", dat)
      })
      
      observe({
        req(d$project())
        d$service <- listProjectsServer("service", dat, d$project())
      })
      
      observe({
        req(d$service())
        listProjectsServer("task", dat, d$service())
      })
      
      observeEvent(input$addUser, {
        req(d$project())
        query <- "{config$api}/clients/{client_id}/projects/{d$project()}/users/{input$users}/create"
        res <- fromJSON(readLines(glue(query))) %>% as_tibble()
        if (res$code == 201)
          dat$project_users <- bind_rows(dat$project_users,
                                         tibble(id = res$id,
                                                project_id = d$project(),
                                                user_id = as.numeric(input$users)))
      })
      
      #### Append new data ----
      
      observeEvent(new_project(), {
        req(new_project())
        dat$projects <- bind_rows(dat$projects, new_project()$project)
        if ("project_users" %in% names(new_project()))
          dat$project_users <- bind_rows(dat$project_users, new_project()$project_users)
      })
      
      observeEvent(new_service(), {
        req(new_service())
        dat$projects <- bind_rows(dat$projects, new_service()$project)
      })
      
      observeEvent(new_task(), {
        req(new_task())
        dat$projects <- bind_rows(dat$projects, new_task()$project)
      })
      
      #### Render UI ----
      
      observe({
        updateSelectInput(session, "users", choices = setNames(dat$users$id, dat$users$name))
      })
      
      output$projectUsers <- DT::renderDataTable({
        req(d$project())
        users <- filter(dat$project_users, project_id == d$project())
        o <- filter(dat$users, id %in% users$user_id)
        datatable(select(o, name, email), escape = FALSE, rownames = FALSE, select = "single")
      })
      
    }
  )
}
