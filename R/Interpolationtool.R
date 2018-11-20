
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


create_voronoi <- function(stations,z,Days,Station_list,limits_voronoi,coordinates,YY,All_Days,Area){
  #Erzeugung der Voronoi-Polygone

  D <- array(NaN,dim=c(length(Days),nrow(Station_list),nrow(Area)))
  jot <- 0
  print("\n")
  pbj <- txtProgressBar(min = 0, max = 100, style = 3)
  Sys.sleep(0.1)

  #Schleife ?ber alle Areae
  for (j in 1:nrow(Area)){
    jot <- j/nrow(Area)*100
    # update progress bar
    setTxtProgressBar(pbj, jot)
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

voronoiinterpolation <- function(Area,All_Days,YY,parameter,Station_list,coordinates,stations,station_values){

  interpolated_params <- array(0,dim=c(length(All_Days),nrow(Area)))
  rownames(interpolated_params) <- as.character(as.Date(All_Days))
  colnames(interpolated_params) <- as.character(Area$name)

  # create progress bar
  print("\n")
  bar <- 0
  pb <- txtProgressBar(min = 0, max = 100, style = 3)
  Sys.sleep(0.1)

  limits_voronoi <- min_max_coordinates(Station_list, Area, coordinates)
  for (z in 1:length(YY)){
    bar <- z/length(YY)*100
    # update progress bar
    setTxtProgressBar(pb, bar)

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
