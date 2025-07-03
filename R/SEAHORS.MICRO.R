SEAHORS.BUCKET <- function(){
  shiny::addResourcePath('www', system.file('www', package = 'SEAHORS'))
  
  shinyApp(ui = app_ui.MICRO, server = app_server.MICRO)
}
