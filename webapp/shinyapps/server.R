
#### Server ----

server <- function(input, output, session) {
    
    #### Environment ----
    
    config <- config::get()
    
    #### Data wrangling ----
    
    clients <- reactiveValues(clients = fromJSON(readLines(glue('{config$api}/clients'))) %>% as_tibble())
    client <- reactiveValues()
    user <- reactiveValues()
    
    observeEvent(input$clients, {
        req(input$clients)
        dat <- fromJSON(readLines(glue("{config$api}/clients/{input$clients}"))) %>%
            map(as_tibble)
        client$projects <- dat$projects
        client$users <- dat$users
        client$project_users <- dat$project_users
    })
    
    observeEvent(input$users, {
        req(input$users)
        dat <- fromJSON(readLines(glue("{config$api}/clients/{input$clients}/users/{input$users}"))) %>%
            map(as_tibble)
        
        user$client_id <- dat$client_id
        user$projects <- dat$projects
        user$info <- dat$user
        
        if (nrow(dat$events) > 0)
            user$events <- dat$events %>%
            rowwise() %>%
            mutate(across(c("start_time", "end_time"), as.POSIXlt, "%Y-%m-%dT%H:%M:%S", tz = ""),
                   across("task", paste, collapse = ";"),
                   across("description", ~ifelse(is.na(.), "", .))) %>%
            separate(task, into = c("project", "service", "task"), sep = ";") %>%
            mutate(project_id = map2_dbl(project, task_id, project_lookup, dat = dat$projects),
                   service_id = map2_dbl(service, task_id, project_lookup, dat = dat$projects))
        
    })
    
    observe({ updateSelectInput(session, "clients", choices = setNames(clients$clients$id, clients$clients$name)) })
    observe({ updateSelectInput(session, "users", choices = setNames(client$users$id, client$users$name)) })
    
    observeEvent(input$refreshProjects, {
        req(input$users)
        dat <- fromJSON(readLines(glue("{config$api}/clients/{input$clients}/users/{input$users}"))) %>%
            map(as_tibble)
        user$projects <- dat$projects
    })
    
    proxy <- dataTableProxy("listEvents")
    
    #### Event Group ----

    event_result <- createEventServer("userEvent", config, user, input$clients, input$users)
    observeEvent(event_result(), {
        req(event_result())
        # Append new event
        if (isTruthy(event_result()$create))
            user$events <- bind_rows(event_result()$create, user$events)
        # Update event end time
        if (isTruthy(event_result()$stop))
            user$events[user$events$id == event_result()$stop$id,]$end_time <- event_result()$stop$end_time
    })
    
    #### List Events ----

    events_list <- reactive({ user$events })
    event_list_id <- listEventsServer("listEvents", events_list)

    ### Update Event ----

    observeEvent(event_list_id(), {
        req(event_list_id())
        user$selected_event <- filter(user$events, id == event_list_id())
    })
    selected_event <- reactive({ user$selected_event })
    update_event_result <- updateEventServer("updateEvent", config, user, input$clients, input$users, selected_event, proxy)

    observeEvent(update_event_result(), {
        req(update_event_result())
        user$events <- rows_update(user$events, update_event_result(), by = "id")
    })

    ### Projects Group ----

    clientProjectsServer("clientProjects", config, input$clients, client)

    #### Client Group ----
    
    new_user <- createUserServer("createUser", config)
    observeEvent(new_user(), {
        req(new_user())
        client$users <- bind_rows(client$users, new_user())
    })

    #### System Group ----

    new_client <- createClientServer("createClient", config)
    observeEvent(new_client(), {
        req(new_client())
        clients$clients <- bind_rows(clients$clients, new_client())
    })
    
}
