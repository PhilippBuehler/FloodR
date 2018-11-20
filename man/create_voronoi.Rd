\name{create_voronoi}
\alias{create_voronoi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creation of Voronoi polygons based on DWD-stations
}
\description{
For the region in question, Voronoi polygons are created based on coordinates of DWD-stations in and surrounding the region. For every day, they are created anew to consider every change in data availability.
}
\usage{
create_voronoi(stations,z,Days,Station_list,limits_voronoi,coordinates,YY,
All_Days,Area)
}
\arguments{
  \item{stations}{
A dataframe containing TRUE or FALSE values for every day and every DWD-station. If the station has registered a value, there is a "TRUE", otherwise there is a "FALSE".
}
  \item{z}{
A number that indicates the timestep of the year considered. If the data is interpolated for the years from 1900 to 2015, z <- 1 indicates the year 1900.
}
  \item{Days}{
An array containig the date of every day of the year considered.
}
  \item{Station_list}{
A dataframe containing the relevant DWD-stations' IDs, eastings, northings and heights. The Station-IDs must consist of 5 digits. If the ID is shorter than that it must be filled with zeros as leading digits. The column names must be "Stations_id", "Easting", "Northing" and "Height".
}
  \item{limits_voronoi}{
A vector containing the minimal and maximal eastings and northings of the DWD-stations and the region in question.
}
  \item{coordinates}{
A dataframe containing every subdomain's coordinates. The column names must be "Easting_gridcode" and "Northing_gridcode" ("gridcode" must be replaced with the actual gridcode). Missing values indicated by NA-values are omitted during the procedure.
}
  \item{YY}{
An array containing all considered years.
}
  \item{All_Days}{
A sequence containing the date for every day within the period for which the Voronoi polygons are created.The days' format must be "Y-m-d".
}
  \item{Area}{
A dataframe containing all subdomain's names, gridcodes and heights. The column names must be "names", "gridcode" and "height".
}
}
\value{
A list with the following entries:
\item{names_over_all_days}{A list containing as many dataframes as there are subdomains. Every dataframe contains the relevant DWD-stations for the considered subdomain and the year in question.}
\item{D}{A three-dimensional array containing every DWD-station's share for every day and every subdomain.}
}
\author{
Laura Haendel
}
\examples{
Area <- data.frame("name"=c("Aue", "Golzern"), "gridcode"=c(563790,560021),
"height" = c(2210, 560))
Station_list <- data.frame("Stations_id" = c("02444", "01960", "02928"),
"Easting" = c(681527, 706010, 740145), "Northing" = c(5644673, 5707725, 5690506),
"Height" = c(2451,586, 620))
limits_voronoi <- c(680000, 750000, 5500000, 5710000)
All_Days <- seq(from = as.Date("1900-01-01"),
to = as.Date("1900-01-10"), by = 1)
YY <- unique(format(All_Days,"\%Y"))
z <- 1
coordinates <- data.frame("Easting_563790" = c(741203.460201, 742203.460201),
"Northing_563790" = c(5591816.694238,5613816.694238),
"Easting_560021" = c(738203.460201, 739203.460201),
"Northing_560021" = c(5587816.694238,5590816.694238))
stations <- data.frame("02444" = c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE),
"01960" = c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE),
"02928" = c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE))
colnames(stations)[1] <- "02444"
colnames(stations)[2] <- "01960"
colnames(stations)[3] <- "02928"
Days <- seq(from = as.Date(paste("01-01-", YY[z], sep = ""),
"\%d-\%m-\%Y"), to = as.Date(paste("10-01-", YY[z], sep = ""),
'\%d-\%m-\%Y'), by = 1)

polygons <- create_voronoi(stations,z,Days,Station_list,limits_voronoi,
coordinates,YY,All_Days,Area)
names_over_all_days <- polygons[[1]]
D <- polygons[[2]]
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~ts }
\keyword{ ~classif }% __ONLY ONE__ keyword per line
