
#### libraries ----

library(tidyverse)
library(DT)
library(glue)
library(lubridate)
library(config)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(jsonlite)
library(httr)

#### Environment ----

Sys.setenv(R_CONFIG_ACTIVE = "development")

#### UI ----

ui <- dashboardPage(
    dashboardHeader(title = "Basic dashboard"),
    dashboardSidebar(
        sidebarMenu(
            useShinyjs(),
            selectInput("clients", "Client:", c(`--` = "")),
            selectInput("users", "User:", c(`--` = "")),
            hr(),
            actionButton("refreshProjects", "Refresh Projects"),
            hr(),
            menuItem("User", tabName = "event_tab", icon = icon("user-clock")),
            menuItem("Projects", tabName = "project_tab", icon = icon("project-diagram")),
            menuItem("Manage", tabName = "manage_tab", icon = icon("users")),
            menuItem("System", tabName = "system_tab", icon = icon("superpowers"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "event_tab",
                    fluidRow(
                        box(createEventUI("userEvent"),
                            title = "Start/Stop",
                            width = 4),
                        box(listEventsUI("listEvents", "List events"),
                            title = "Events",
                            width = 4),
                        box(updateEventUI("updateEvent"),
                            title = "Edit Event",
                            width = 4)
                    )
            ),
            tabItem(tabName = "project_tab", clientProjectsUI("clientProjects", "Client projects")),
            tabItem(tabName = "manage_tab",
                    fluidRow(
                        box(createUserUI("createUser", "Create user"),
                            title = "Add User",
                            width = 12)
                    )),
            tabItem(tabName = "system_tab",
                    fluidRow(
                        box(createClientUI("createClient", "Create client"),
                            title = "Create Client",
                            width = 4)
                    )
            )
        )
    )
)
