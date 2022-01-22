
listEventsUI <- function(id, label = "Template Module") {
  ns <- NS(id)
  tagList(
    dataTableOutput(ns("events"))
  )
}

listEventsServer <- function(id, events) {
  moduleServer(
    id,
    function(input, output, session) {

      d <- reactiveValues()
      
      observe({
        req(events())
        req("id" %in% names(events()))
        
        d$events <- events() %>%
          transmute(id,
                    date = format(start_time, "%d-%b-%Y"),
                    start = format(start_time, "%H:%M:%S"),
                    end = format(end_time, "%H:%M:%S"),
                    project, service, task, description)
        
        d$out <- tibble(Task = glue("<b>{d$events$date}</b>\\
                                <br/>{d$events$start} - {ifelse(is.na(d$events$end), 'Ongoing', d$events$end)}<br/>\\
                                {d$events$project}/{d$events$service}<br/>\\
                                {d$events$task}<br />\\
                                {d$events$description}"))
      })
      
      output$events = DT::renderDataTable({
        req(d$out)
        datatable(d$out,
                  escape = FALSE,
                  rownames = FALSE,
                  select = "single")
      })
      
      observeEvent(input$events_rows_selected, {
        req(input$events_rows_selected)
        d$return <- slice(d$events, input$events_rows_selected)$id
      })

      return(reactive({ d$return }))
    })
}
