#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import bs4Dash
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(
      title = "Basic Dashboard",
      fullscreen = FALSE,
      header = dashboardHeader(
        title = dashboardBrand(
          title = "simclindata.shiny",
          color = "primary"
        ),
        skin = "light",
        status = "white",
        border = TRUE,
        sidebarIcon = icon("bars"),
        controlbarIcon = icon("th"),
        fixed = FALSE
      ),
      sidebar = dashboardSidebar(
        skin = "light",
        status = "primary",
        elevation = 3,
        minified = FALSE,
        collapsed = FALSE,
        width = "320px",
        sidebarMenu(
          id = "tabs",
          menuItem(
            "Simulation Summary",
            tabName = "demographics"
          )
        )
      ),
      body = dashboardBody(
        tabItems(
          tabItem(
            tabName = "demographics",
            fluidRow(
              column(
                width = 3,
                mod_dateselect_ui("dateselect_1")
              ),
              column(
                width = 3,
                mod_ageselect_ui("ageselect_1")
              ),
              column(
                width = 3,
                mod_encounterselect_ui("encounterselect_1")
              )
            ),
            mod_demographics_ui("demographics_1")
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "simclindata.shiny"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
