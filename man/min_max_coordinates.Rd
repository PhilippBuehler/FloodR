\name{min_max_coordinates}
\alias{min_max_coordinates}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Determine the minimal and maximal eastings and northings for a region and the surrounding DWD-stations
}
\description{
The coordinates of the subdomains of the specified region and the coordinates of the relevant DWD-stations are read. The minimal and maximal eastings and northings of all these coordinates are determined.
}
\usage{
min_max_coordinates(Station_list, Area,coordinates)
}
\arguments{
  \item{Station_list}{
A dataframe containing the relevant DWD-stations' IDs, eastings, northings and heights. The Station-IDs must consist of 5 digits. If the ID is shorter than that it must be filled with zeros as leading digits. The column names must be "Stations_id", "Easting", "Northing" and "Height".
}
  \item{Area}{
A dataframe containing all subdomain's names, gridcodes and heights. The column names must be "names", "gridcode" and "height".
}
  \item{coordinates}{
A dataframe containing every subdomain's coordinates. The column names must be "Easting_gridcode" and "Northing_gridcode" ("gridcode" must be replaced with the actual gridcode). Missing values indicated by NA-values are omitted during the procedure.
}
}
\value{
\item{results}{A vector containing the minimal easting, the maximal easting, the minimal northing and the maximal northing in this order.}
}
\author{
Laura Haendel
}
\examples{
Area <- data.frame("name"=c("Aue", "Golzern"), "gridcode"=c(563790,560021),
"height" = c(2210, 560))
Station_list <- data.frame("Stations_id" = c("02444", "01960"),
"Easting" = c(681527, 706010), "Northing" = c(5644673, 5707725),
"Height" = c(2451,586))
coordinates <- data.frame("Easting_563790" = c(741203.460201, 742203.460201),
"Northing_563790" = c(5591816.694238,5613816.694238),
"Easting_560021" = c(738203.460201, 739203.460201),
"Northing_560021" = c(5587816.694238,5590816.694238))
limits_voronoi <- min_max_coordinates(Station_list,Area,coordinates)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~ts }
\keyword{ ~classif }% __ONLY ONE__ keyword per line
