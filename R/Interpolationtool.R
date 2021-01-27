#' Determine the minimal and maximal eastings and northings for a region and
#' the surrounding DWD-stations
#'
#' The coordinates of the subdomains of the specified region and the
#' coordinates of the relevant DWD-stations are read. The minimal and maximal
#' eastings and northings of all these coordinates are determined.
#'
#'
#' @param Station_list A dataframe containing the relevant DWD-stations' IDs,
#' eastings, northings and heights. The Station-IDs must consist of 5 digits.
#' If the ID is shorter than that it must be filled with zeros as leading
#' digits. The column names must be "Stations_id", "Easting", "Northing" and
#' "Height".
#' @param Area A dataframe containing all subdomain's names, gridcodes and
#' heights. The column names must be "names", "gridcode" and "height".
#' @param coordinates A dataframe containing every subdomain's coordinates. The
#' column names must be "Easting_gridcode" and "Northing_gridcode" ("gridcode"
#' must be replaced with the actual gridcode). Missing values indicated by
#' NA-values are omitted during the procedure.
#' @return \item{results}{A vector containing the minimal easting, the maximal
#' easting, the minimal northing and the maximal northing in this order.}
#' @author Laura Haendel
#' @keywords ~classif ~ts
#' @keywords internal
min_max_coordinates <- function(Station_list, Area,coordinates){
  #Koordinatenpunkte aller Gebiete einlesen um minimale und maximale Rechts- und Hochwerte f?r die Voronoi-Polygone zu haben
  min_e <- list()
  max_e <- list()
  min_n <- list()
  max_n <- list()
  for (q in 1:nrow(Area)){
    id_c <- Area$gridcode[q]
    P_c_e <- coordinates[paste("Easting",id_c,sep="_")][!is.na(coordinates[paste("Easting",id_c,sep="_")])]
    P_c_n <- coordinates[paste("Northing",id_c,sep="_")][!is.na(coordinates[paste("Northing",id_c,sep="_")])]
    min_e[q] <- min(P_c_e)
    max_e[q] <- max(P_c_e)
    min_n[q] <- min(P_c_n)
    max_n[q] <- max(P_c_n)
  }
  min_e[q+1] <- min(Station_list$Easting)
  max_e[q+1] <- max(Station_list$Easting)
  min_n[q+1] <- min(Station_list$Northing)
  max_n[q+1] <- max(Station_list$Northing)

  min_easting <- min(unlist(min_e))
  max_easting <- max(unlist(max_e))
  min_northing <- min(unlist(min_n))
  max_northing <- max(unlist(max_n))

  results <- c(min_easting, max_easting, min_northing, max_northing)

  return(results)
}








#' Creation of Voronoi polygons based on DWD-stations
#'
#' For the region in question, Voronoi polygons are created based on
#' coordinates of DWD-stations in and surrounding the region. For every day,
#' they are created anew to consider every change in data availability.
#'
#'
#' @param stations A dataframe containing TRUE or FALSE values for every day
#' and every DWD-station. If the station has registered a value, there is a
#' "TRUE", otherwise there is a "FALSE".
#' @param z A number that indicates the timestep of the year considered. If the
#' data is interpolated for the years from 1900 to 2015, z <- 1 indicates the
#' year 1900.
#' @param Days An array containig the date of every day of the year considered.
#' @param Station_list A dataframe containing the relevant DWD-stations' IDs,
#' eastings, northings and heights. The Station-IDs must consist of 5 digits.
#' If the ID is shorter than that it must be filled with zeros as leading
#' digits. The column names must be "Stations_id", "Easting", "Northing" and
#' "Height".
#' @param limits_voronoi A vector containing the minimal and maximal eastings
#' and northings of the DWD-stations and the region in question.
#' @param coordinates A dataframe containing every subdomain's coordinates. The
#' column names must be "Easting_gridcode" and "Northing_gridcode" ("gridcode"
#' must be replaced with the actual gridcode). Missing values indicated by
#' NA-values are omitted during the procedure.
#' @param YY An array containing all considered years.
#' @param All_Days A sequence containing the date for every day within the
#' period for which the Voronoi polygons are created.The days' format must be
#' "Y-m-d".
#' @param Area A dataframe containing all subdomain's names, gridcodes and
#' heights. The column names must be "names", "gridcode" and "height".
#' @return A list with the following entries: \item{names_over_all_days}{A list
#' containing as many dataframes as there are subdomains. Every dataframe
#' contains the relevant DWD-stations for the considered subdomain and the year
#' in question.} \item{D}{A three-dimensional array containing every
#' DWD-station's share for every day and every subdomain.
#' }
#' @import spatstat
#' @author Laura Haendel
#' @keywords ~classif ~ts
#' @keywords internal
create_voronoi <- function(stations,z,Days,Station_list,limits_voronoi,coordinates,YY,All_Days,Area){
  #Erzeugung der Voronoi-Polygone

  D <- array(NaN,dim=c(length(Days),nrow(Station_list),nrow(Area)))
  jot <- 0


  #Schleife ?ber alle Areae
  for (j in 1:nrow(Area)){
    jot <- j/nrow(Area)*100
    stat_vergleich <- array(0,dim=c(2,3))
    id=Area$gridcode[j]

    #x- und y-Werte der Gebiete in Form von Punktdaten einlesen
    P_e <- coordinates[paste("Easting",id,sep="_")][!is.na(coordinates[paste("Easting",id,sep="_")])]
    P_n <- coordinates[paste("Northing",id,sep="_")][!is.na(coordinates[paste("Northing",id,sep="_")])]

    for (i in 1:length(Days)){
      #N-Stationen raussuchen, welche am jeweiligen Tag Aufzeichnungen vorliegen haben
      relevanter_tag <- which(format(All_Days,"%Y")==YY[z])[i]
      rel_stations <- stations[relevanter_tag,]
      selection<- as.logical(rel_stations)
      Station_y <- Station_list[selection,]

      if (nrow(stat_vergleich)==nrow(Station_y)){
        if (FALSE %in% (stat_vergleich==Station_y)){
          x=Station_y$Easting
          y=Station_y$Northing

          #Erstellt Thiessen-Polygone
          voro = dirichlet(ppp(x, y,c(limits_voronoi[1], limits_voronoi[2]),c(limits_voronoi[3], limits_voronoi[4])))

          QC=quadratcount(ppp(P_e, P_n,range(P_e),range(P_n)), tess=voro)
          #ppp -> spannt ein Quadrat ?ber die x- und y-Werte des jeweiligen Gebietes auf das von den minimalen zu den
          #       maximalen Werten von x und y reicht
          #quadratcount ->  teilt das ppp-Quadrat in die vorher festgelegten Thiessen-Polygone und z?hlt, wie viele Punkte
          #                 in den jeweiligen Polygonen liegen
          #Alle Polygone, in welchen keine Punkte liegen, werden rausgenommen
          QC=QC[QC!=0]

          #Anteil der Punkte pro Polygon im Verh?ltnis zu allen Punkten
          QC=QC/sum(QC)
          QC=QC*100
          #Die Thiessen-Polygone welcher N-Stationen haben einen Anteil an dem jeweiligen Gebiet
          b1=which(Station_y$Stations_id %in% Station_y[as.numeric(names(QC)),"Stations_id"])
          b <- list()
          for (bla in 1:length(b1)){
            b[bla] <- which(Station_y[b1[bla],1] == Station_list$Stations_id)
          }
          b=unlist(b)

          #F?r das jeweilige Jahr (i) und das jeweilige Gebiet (j) wird bei den jeweiligen N-Stationen (b) die
          #Anteile von QC eingetragen
          D[i,b,j]=round(as.numeric(QC),2)
        }else{
          D[i,b,j]=round(as.numeric(QC),2)
        }
      }else{
        x=Station_y$Easting
        y=Station_y$Northing

        #Erstellt Thiessen-Polygone
        voro = dirichlet(ppp(x, y,c(limits_voronoi[1], limits_voronoi[2]),c(limits_voronoi[3], limits_voronoi[4])))

        QC=quadratcount(ppp(P_e, P_n,range(P_e),range(P_n)), tess=voro)

        #Alle Polygone, in welchen keine Punkte liegen, werden rausgenommen
        QC=QC[QC!=0]

        #Anteil der Punkte pro Polygon im Verh?ltnis zu allen Punkten
        QC=QC/sum(QC)
        QC=QC*100
        #Die Thiessen-Polygone welcher N-Stationen haben einen Anteil an dem jeweiligen Gebiet
        b1=which(Station_y$Stations_id %in% Station_y[as.numeric(names(QC)),"Stations_id"])
        b <- list()
        for (bla in 1:length(b1)){
          b[bla] <- which(Station_y[b1[bla],1] == Station_list$Stations_id)
        }
        b=unlist(b)

        #F?r das jeweilige Jahr (i) und das jeweilige Gebiet (j) wird bei den jeweiligen N-Stationen (b) die
        #Anteile von QC eingetragen
        D[i,b,j]=round(as.numeric(QC),2)
      }
      stat_vergleich <- Station_y
    }
  }

  #F?r jedes Gebiet wird betrachtet, welche N-Stationen relevant sind
  names_over_all_days=apply(D,3,  function(y) Station_list[apply(y,2,function(x) !all(is.nan(x))),c(1:ncol(Station_list))])

  results <- list(names_over_all_days,D)

  return(results)
}







#' Interpolation of precipitation or temperature data for a specified region
#' using Voronoi-polygons
#'
#' Precipitation or temperature data from surrounding DWD-stations are
#' interpolated for the specified region to obtain areal values. Voronoi
#' polygons are created, every polygon's share from the region is calculated
#' and the precipitation or temperature data for the region is interpolated.
#'
#'
#' @param Area A dataframe containing all subdomain's names, gridcodes and
#' heights. The column names must be "names", "gridcode" and "height".
#' @param All_Days A sequence containing the date for every day within the
#' period for which the data is interpolated. The days' format must be "Y-m-d".
#' @param YY An array containing every year considered in the interpolation.
#' @param parameter Character (either "prec" or "temp"), determines whether
#' precipitation or temperature data is interpolated.
#' @param Station_list A dataframe containing the relevant DWD-stations' IDs,
#' eastings, northings and heights. The Station-IDs must consist of 5 digits.
#' If the ID is shorter than that it must be filled with zeros as leading
#' digits. The column names must be "Stations_id", "Easting", "Northing" and
#' "Height".
#' @param coordinates A dataframe containing every subdomain's coordinates. The
#' column names must be "Easting_gridcode" and "Northing_gridcode" ("gridcode"
#' must be replaced with the actual gridcode). Missing values indicated by
#' NA-values are omitted during the procedure.
#' @param stations A dataframe containing TRUE or FALSE values for every day
#' and every DWD-station. If a precipitation or temperature value has been
#' registered there is a "TRUE", otherwise there is a "FALSE".
#' @param station_values A dataframe containing the precipitation or
#' temperature data for every day and every relevant DWD-station. If a
#' DWD-station did not register data for a period, the values must be set to
#' zero.
#' @return \item{interpolated_params }{A matrix containing the interpolated
#' precipitation or temperature data for every day for every subdomain.}
#' @author Laura Haendel
#' @keywords ~classif ~ts
#' @examples
#'
#' \dontrun{
#' Area <- data.frame("name"=c("Aue", "Niederschlema", "Golzern"),
#' "gridcode"=c(563790,562040,560020),"height" = c(2210, 685, 452))
#' coordinates <- data.frame("Easting_563790" = c(759830.4946,764830.4946,
#' 769830.4946,759830.4946,764830.4946,769830.4946,759830.946,764830.4946,
#' 769830.4946,759830.4946,764830.4946,769830.4946),
#' "Northing_563790" = c(5608061.8,5608061.8,5608061.8,5603061.8,5603061.8,
#' 5603061.8,5598061.8,5598061.8,5598061.8,5593061.8,5593061.8,5593061.8),
#' "Easting_562040" = c(744830.4946,749830.4946,754830.4946,749830.4946,
#' 754830.4946,744830.4946,749830.4946,754830.4946,744830.4946,749830.4946,
#' 754830.4946,744830.4946),
#' "Northing_562040" = c(5608061.8,5608061.8,5608061.8,5603061.8,5603061.8,
#' 5598061.8,5598061.8,5598061.8,5593061.8,5593061.8,5593061.8,5603061.8),
#' "Easting_560020" = c(754830.4946,754830.4946,754830.4946, 754830.4946,
#' 759830.4946,764830.4946,759830.4946,764830.4946,759830.4946,764830.4946,
#' 759830.4946,764830.4946),
#' "Northing_560020" = c(5608061.8,5603061.8,5598061.8,5593061.8,5608061.8,
#' 5608061.8,5603061.8,5603061.8,5598061.8,5598061.8,5593061.8,5593061.8))
#' All_Days <- seq(from = as.Date("1900-01-01"), to = as.Date("1900-12-31"), by = 1)
#' YY <- unique(format(All_Days,"%Y"))
#' Station_list <- data.frame("Stations_id" = c("00438","00559","00840",
#' "01153","02038","02372","04506","04767"), "Easting" = c(767617.4963,
#' 761110.605,756463.7461,755137.788,753573.4476,752950.0651,757558.097,
#' 758533.0727),
#' "Northing" = c(5603716.394,5605149.476,5592820.687,5603864.269,
#' 5607286.876,5605496.48,5610110.483,5599517.766),
#' "Height" = c(2451,586, 620,500,653,4764,663,658))
#' stations <- data.frame("00438" = unlist(list(rep(TRUE,365))),
#' "00559" = unlist(list(rep(TRUE,365))),
#' "00840" = unlist(list(rep(TRUE,365))),"01153" = unlist(list(rep(TRUE,365))),
#' "02038" = unlist(list(rep(TRUE,365))),
#' "02372" = unlist(list(rep(TRUE,365))),"04506" = unlist(list(rep(TRUE,365))),
#' "04767" = unlist(list(rep(TRUE,365))))
#' station_values <- data.frame("00438" = runif(365,min=0,max=10),
#' "00559" = runif(365,min=0,max=10), "00840" = runif(365,min=0,max=10),
#' "01153" = runif(365,min=0,max=10),"02038" = runif(365,min=0,max=10),
#' "02372" = runif(365,min=0,max=10),"04506" = runif(365,min=0,max=10),
#' "04767" = runif(365,min=0,max=10))
#'
#' names(stations) <- c("00438", "00559", "00840", "01153",
#' "02038", "02372", "04506", "04767")
#' names(station_values)<- c("00438", "00559", "00840",
#' "01153", "02038", "02372", "04506", "04767")
#'
#' parameter = "prec"
#'
#' interpolated_values <- voronoiinterpolation(Area,All_Days,YY,parameter,Station_list,
#' coordinates,stations,station_values)
#' }
#' @import spatstat
#' @export voronoiinterpolation
voronoiinterpolation <- function(Area,All_Days,YY,parameter,Station_list,coordinates,stations,station_values){

  interpolated_params <- array(0,dim=c(length(All_Days),nrow(Area)))
  rownames(interpolated_params) <- as.character(as.Date(All_Days))
  colnames(interpolated_params) <- as.character(Area$name)


  limits_voronoi <- min_max_coordinates(Station_list, Area, coordinates)
  for (z in 1:length(YY)){
    bar <- z/length(YY)*100

    #Voronoi-Anteile bestimmen
    Days <- seq(from = as.Date(paste("01-01-", YY[z], sep = ""), "%d-%m-%Y"), to = as.Date(paste("31-12-", YY[z], sep = ""), '%d-%m-%Y'), by = 1)
    polygons <- create_voronoi(stations,z,Days,Station_list, limits_voronoi,coordinates,YY,All_Days,Area)
    names_over_all_days <- polygons[[1]]
    D <- polygons[[2]]

    anzahl_Days <- length(Days)

    #Anteile der relevanten N-Stationen pro Gebiet
    S=apply(D,3, function(y) y[, apply(y,2,function(x) !all(is.nan(x)))])
    S=rapply(S, f=function(x) ifelse(is.nan(x),0,x), how="replace" )

    #Schleife ?ber alle Gebiete
    for (g in 1:nrow(Area)){
      anteile=as.data.frame(S[[g]])
      colnames(anteile)=names_over_all_days[[g]]$Stations_id
      rownames(anteile)=c(as.character(Days))

      id <- Area$gridcode[g]
      hoehe_g <- Area$height[g]

      ##Anteile mit Niederschlagsdaten verrechnen##
      anzahl_Stationen <- ncol(anteile)
      indizes_jahr <- which(format(as.Date(All_Days),"%Y")==YY[z])
      param_bestimmt <- array(0,dim=c(anzahl_Days,anzahl_Stationen))
      for (h in 1:anzahl_Stationen){
        index_stations <- which(colnames(station_values) == colnames(anteile)[h])
        if (parameter == "temp"){
          hoehe_s <- Station_list$height[index_stations]
          korrektur <- (hoehe_s-hoehe_g)/100*0.65
        }else{
          korrektur <- 0
        }
        param <- station_values[indizes_jahr,index_stations]+korrektur
        param_bestimmt[,h] <- param*anteile[,h]/100
      }

      param_ges = array(0,dim=c(anzahl_Days,1))
      for (o in 1:nrow(param_bestimmt)){
        param_ges[o] <- sum(param_bestimmt[o,])
      }
      interpolated_params[indizes_jahr,g] <- round(param_ges,3)
    }
  }
  return(interpolated_params)
}
