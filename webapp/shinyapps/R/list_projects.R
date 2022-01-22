listProjectsUI <- function(id, label = "List Project/Service/Task") {
  ns <- NS(id)
  tagList(
    dataTableOutput(ns("projects"))
  )
}

listProjectsServer <- function(id, dat, pid = -1) {
  moduleServer(
    id,
    function(input, output, session) {
      
      #### Data wrangling  ----
      
      d <- reactiveValues()
      
      observe({
        req(dat$projects)
        req(pid)
        if (pid < 0) {
          d$projects <- filter(dat$projects, is.na(parent_id))
        } else {
          d$projects <- filter(dat$projects, parent_id == pid)
        }
      })
      
      #### Actions ----
      
      observeEvent(input$projects_rows_selected, {
        d$return <- slice(d$projects, input$projects_rows_selected)$id
      })
      
      #### Render UI ----
      
      output$projects <- DT::renderDataTable({
        req(d$projects)
        datatable(select(d$projects, name), escape = FALSE, rownames = FALSE, selection = "single")
      })
      
      return(reactive({ d$return }))
    })
}
