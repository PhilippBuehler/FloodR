\name{voronoiinterpolation}
\alias{voronoiinterpolation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interpolation of precipitation or temperature data for a specified region using Voronoi-polygons
}
\description{
Precipitation or temperature data from surrounding DWD-stations are interpolated for the specified region to obtain areal values. Voronoi polygons are created, every polygon's share from the region is calculated and the precipitation or temperature data for the region is interpolated.
}
\usage{
voronoiinterpolation(Area,All_Days,YY,parameter,Station_list,coordinates,
stations,station_values)
}
\arguments{
  \item{Area}{
A dataframe containing all subdomain's names, gridcodes and heights. The column names must be "names", "gridcode" and "height".
}
  \item{All_Days}{
A sequence containing the date for every day within the period for which the data is interpolated. The days' format must be "Y-m-d".
}
  \item{YY}{
An array containing every year considered in the interpolation.
}
  \item{parameter}{
Character (either "prec" or "temp"), determines whether precipitation or temperature data is interpolated.
}
  \item{Station_list}{
A dataframe containing the relevant DWD-stations' IDs, eastings, northings and heights. The Station-IDs must consist of 5 digits. If the ID is shorter than that it must be filled with zeros as leading digits. The column names must be "Stations_id", "Easting", "Northing" and "Height".
}
  \item{coordinates}{
A dataframe containing every subdomain's coordinates. The column names must be "Easting_gridcode" and "Northing_gridcode" ("gridcode" must be replaced with the actual gridcode). Missing values indicated by NA-values are omitted during the procedure.
}
  \item{stations}{
A dataframe containing TRUE or FALSE values for every day and every DWD-station. If a precipitation or temperature value has been registered there is a "TRUE", otherwise there is a "FALSE".
}
  \item{station_values}{
A dataframe containing the precipitation or temperature data for every day and every relevant DWD-station. If a DWD-station did not register data for a period, the values must be set to zero.
}
}
\value{
\item{interpolated_params }{A matrix containing the interpolated precipitation or temperature data for every day for every subdomain.}
}
\author{
Laura Haendel
}
\examples{
Area <- data.frame("name"=c("Aue", "Niederschlema", "Golzern"),
"gridcode"=c(563790,562040,560020),"height" = c(2210, 685, 452))
coordinates <- data.frame("Easting_563790" = c(759830.4946,764830.4946,
769830.4946,759830.4946,764830.4946,769830.4946,759830.946,764830.4946,
769830.4946,759830.4946,764830.4946,769830.4946),
"Northing_563790" = c(5608061.8,5608061.8,5608061.8,5603061.8,5603061.8,
5603061.8,5598061.8,5598061.8,5598061.8,5593061.8,5593061.8,5593061.8),
"Easting_562040" = c(744830.4946,749830.4946,754830.4946,749830.4946,
754830.4946,744830.4946,749830.4946,754830.4946,744830.4946,749830.4946,
754830.4946,744830.4946),
"Northing_562040" = c(5608061.8,5608061.8,5608061.8,5603061.8,5603061.8,
5598061.8,5598061.8,5598061.8,5593061.8,5593061.8,5593061.8,5603061.8),
"Easting_560020" = c(754830.4946,754830.4946,754830.4946, 754830.4946,
759830.4946,764830.4946,759830.4946,764830.4946,759830.4946,764830.4946,
759830.4946,764830.4946),
"Northing_560020" = c(5608061.8,5603061.8,5598061.8,5593061.8,5608061.8,
5608061.8,5603061.8,5603061.8,5598061.8,5598061.8,5593061.8,5593061.8))
All_Days <- seq(from = as.Date("1900-01-01"), to = as.Date("1900-12-31"), by = 1)
YY <- unique(format(All_Days,"\%Y"))
Station_list <- data.frame("Stations_id" = c("00438","00559","00840",
"01153","02038","02372","04506","04767"), "Easting" = c(767617.4963,
761110.605,756463.7461,755137.788,753573.4476,752950.0651,757558.097,
758533.0727),
"Northing" = c(5603716.394,5605149.476,5592820.687,5603864.269,
5607286.876,5605496.48,5610110.483,5599517.766),
"Height" = c(2451,586, 620,500,653,4764,663,658))
stations <- data.frame("00438" = unlist(list(rep(TRUE,365))),
"00559" = unlist(list(rep(TRUE,365))),
"00840" = unlist(list(rep(TRUE,365))),"01153" = unlist(list(rep(TRUE,365))),
"02038" = unlist(list(rep(TRUE,365))),
"02372" = unlist(list(rep(TRUE,365))),"04506" = unlist(list(rep(TRUE,365))),
"04767" = unlist(list(rep(TRUE,365))))
station_values <- data.frame("00438" = runif(365,min=0,max=10),
"00559" = runif(365,min=0,max=10), "00840" = runif(365,min=0,max=10),
"01153" = runif(365,min=0,max=10),"02038" = runif(365,min=0,max=10),
"02372" = runif(365,min=0,max=10),"04506" = runif(365,min=0,max=10),
"04767" = runif(365,min=0,max=10))

names(stations) <- c("00438", "00559", "00840", "01153",
"02038", "02372", "04506", "04767")
names(station_values)<- c("00438", "00559", "00840",
"01153", "02038", "02372", "04506", "04767")

parameter = "prec"

interpolated_values <- voronoiinterpolation(Area,All_Days,YY,parameter,Station_list,
coordinates,stations,station_values)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~ts }
\keyword{ ~classif }% __ONLY ONE__ keyword per line
