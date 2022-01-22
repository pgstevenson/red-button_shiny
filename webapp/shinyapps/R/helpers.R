
iso_time <- "%Y-%m-%dT%H:%M:%S"

current_time_iso <- function() format(Sys.time(), iso_time)

# time_zones <- function() as_tibble(fromJSON(readLines(paste(config$api, "time_zones", sep = "/"))))

ui_reset_event <- function(session) {
  walk(c("projects", "services", "tasks", "description", "start", "back"), shinyjs::hide)
  walk(c("stop", "startNew"), shinyjs::show)
  walk(c("projects", "services", "tasks"), ~updateSelectInput(session, ., selected = c(`--` = "none")))
  updateTextInput(session, "description", value = "")
}

ui_reset_edit <- function(session) {
  walk(c("id", "start", "end", "description"), ~updateTextInput(session, ., value = ""))
  walk(c("projects", "services", "tasks"), ~updateSelectInput(session, ., selected = c(`--` = "none")))
}

list_project_tree <- function(d, parent_id) {
  x <- d[d$id == parent_id,]
  if (is.na(x$parent_id))
    return(x)
  bind_rows(list_project_tree(d, x$parent_id), x)
}

project_lookup <- function(x, y, dat) {
  projects <- list_project_tree(dat, y)
  projects[projects$name == x,]$id
}

update_project_select <- function(d, project_id, parent = NA) {
  if (is.na(parent)) {
    x <- filter(d, is.na(parent_id))
  } else {
    x <- filter(d, parent_id == parent)
  }
  y <- filter(d, id == project_id)
  list(choices = c(c(`--` = ""), setNames(x$id, x$name)),
       selected = setNames(y$id, y$name))
}