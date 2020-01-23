PKGENVIR <- new.env(parent=emptyenv())

Run_WebFlood <- function(Discharge,Precipitation=NULL,Catchment_Properties=NULL,language="en")
{
  PKGENVIR$language <- language
  PKGENVIR$Discharge <- Discharge
  PKGENVIR$Precipitation <- Precipitation
  PKGENVIR$Catchment_Properties <- Catchment_Properties

  a = sapply(c("Discharge","Precipitation","Catchment_Properties","language"),
             function(x) exists(x,envir = PKGENVIR))
  print(a)

  shiny::runApp(appDir = system.file("shinyApp", package = "FloodR"))
}
