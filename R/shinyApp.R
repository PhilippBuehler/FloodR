PKGENVIR <- new.env(parent=emptyenv())

Run_WebFlood <- function(Discharge=NULL,Precipitation=NULL,Catchment_Properties=NULL,language="en")
{
  require(shiny)
  
  PKGENVIR$language <- language
  PKGENVIR$Discharge <- Discharge
  PKGENVIR$Precipitation <- Precipitation
  PKGENVIR$Catchment_Properties <- Catchment_Properties


  shiny::runApp(appDir = system.file("shinyApp", package = "FloodR"))
}
