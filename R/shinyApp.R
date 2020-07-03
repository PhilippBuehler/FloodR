PKGENVIR <- new.env(parent=emptyenv())

Run_WebFlood <- function(Discharge=NULL,Precipitation=NULL,Catchment_Properties=NULL, language="en")
{
  require(shiny)

  PKGENVIR$language <- language
  PKGENVIR$Discharge <- Discharge
  PKGENVIR$Precipitation <- Precipitation
  PKGENVIR$Catchment_Properties <- Catchment_Properties
  PKGENVIR$Dummy <- Dummy

  if(any(c(sapply(PKGENVIR$Discharge, function(x) class(x[[1]]))) %in% c("POSIXct", "POSIXt"))){
    print("hourly")
    shiny::runApp(appDir = system.file("shinyApp_hourly", package = "FloodR"))
  }else{
    print("daily")
    shiny::runApp(appDir = system.file("shinyApp_daily", package = "FloodR"))
  }
}
