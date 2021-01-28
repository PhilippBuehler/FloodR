#' Apply the TMPS model for a typewise statistical estimate of return periods
#' as well as a combined TMPS return period. See also reference Fischer (2018).
#'
#' @param p The points (return periods) where the quantiles should be calculated
#' Defaults to: 2, 5, 10, 20, 25, 50, 100, 200, 500, 1000
#' @param Flood_events data.frame: Floods events with the columns "Peak_date" (Date format), "HQ" (numeric) and "Type" (factor)
#' @param Daily_discharge data.frame: Daily discharge (continuous) with columns "Date (Date format) and "Discharge" (numeric)
#' @param return_TMPS character vector: What should be returned. Any combination of "TMPS" and the occuring floodtypes
#' @param Threshold_Q numeric: Peak of Threshold, defaults to 3
#' @param p_as_annuality logical: Should all probabilities be treated as an annuality [in years] instead of dimensionless values between 0 and 1
#' @author Svenja Fischer
#' @author Philipp Bühler
#' @references Fischer, S. (2018). A seasonal mixed-POT model to estimate high flood quantiles from different
#' event types and seasons. Journal of Applied Statistics, 45(15), 2831–2847.
#' https://doi.org/10.1080/02664763.2018.1441385
#' @export qTMPS
qTMPS <- function(p = c(2, 5, 10, 20, 25, 50, 100, 200, 500, 1000),
  Flood_events, Daily_discharge,
  return_TMPS = c("TMPS", "R1", "R2", "R3", "S1", "S2"),
  Threshold_Q = 3, p_as_annuality = TRUE){

  if(!p_as_annuality) p <- (-1/(p-1))


  Results <- TMPS_model(Flood_events = Flood_events, Daily_discharge = Daily_discharge,
    p_input = p, q_input = NULL,
    return_TMPS = return_TMPS, Threshold_Q = Threshold_Q)

  return(Results)
}



#' Apply the TMPS model for a typewise statistical estimate of return periods
#' as well as a combined TMPS return period. See also reference Fischer (2018).
#'
#' @param q The quantiles for which the return periods should be calculated
#' Defaults to: 20, 100, 1000
#' @param Flood_events data.frame: Floods events with the columns "Peak_date" (Date format), "HQ" (numeric) and "Type" (factor)
#' @param Daily_discharge data.frame: Daily discharge (continuous) with columns "Date (Date format) and "Discharge" (numeric)
#' @param return_TMPS character vector: What should be returned. Any combination of "TMPS" and the occuring floodtypes
#' @param Threshold_Q numeric: Peak of Threshold, defaults to 3
#' @param p_as_annuality logical: Should all probabilities be treated as an annuality [in years] instead of dimensionless values between 0 and 1
#' @author Svenja Fischer
#' @author Philipp Bühler
#' @references Fischer, S. (2018). A seasonal mixed-POT model to estimate high flood quantiles from different
#' event types and seasons. Journal of Applied Statistics, 45(15), 2831–2847.
#' https://doi.org/10.1080/02664763.2018.1441385
#' @export pTMPS
pTMPS <- function(q = c(10, 100, 1000),
  Flood_events, Daily_discharge,
  return_TMPS = c("TMPS", "R1", "R2", "R3", "S1", "S2"),
  Threshold_Q = 3, p_as_annuality = TRUE){

  Results <- TMPS_model(Flood_events = Flood_events, Daily_discharge = Daily_discharge,
    p_input = NULL, q_input = q,
    return_TMPS = return_TMPS, Threshold_Q = Threshold_Q)

  if(!p_as_annuality) Results <- (1-(1/Results))

  return(Results)
}




#' Apply the TMPS model to a set of data for a typewise statistical estimate of return periods
#' as well as a combined TMPS return period. See reference Fischer (2018)
#' @param Flood_events data.frame: Floods events with the columns "Peak_date" (Date format), "HQ" (numeric) and "Type" (factor)
#' @param Daily_discharge data.frame: Daily discharge (continuous) with columns "Date (Date format) and "Discharge" (numeric)
#' @param p_input The points (as years of return period) where the quantiles should be calculated
#' @param q_input Probabilities to calculate the quantiles for
#' @param Threshold_Q numeric: Peak of Threshold, defaults to 3
#' @param return_TMPS character vector: What should be returned. Any combination of "TMPS" and the occuring floodtypes
#' @author Svenja Fischer
#' @author Philipp Bühler
#' @import fExtremes
#' @importFrom tibble has_name
#' @keywords internal
TMPS_model <- function(Flood_events, Daily_discharge, return_TMPS, Threshold_Q,
  p_input = NULL, q_input = NULL){

  stopifnot(any(class(Flood_events$Type) %in% c("factor", "character")))

  if(any(!has_name(Flood_events, c("Peak_date", "HQ", "Type")))){
    stop("Flood_events are missing a column!")
  }
  if(any(!has_name(Daily_discharge, c("Date", "Discharge")))){
    stop("Daily_discharge are missing a column!")
  }


  if(!is.factor(Flood_events$Type)){
    Flood_events$Type <- factor(Flood_events$Type)
    warning(paste0("Floodtypes aren't factors, so assuming they are:\n",
      paste(levels(Flood_events$Type), collapse = "  -  "), "\n"))
  }


  g_const <- levels(Flood_events$Type)
  GG <- list()
  n <- list()
  TH <- list()
  params <- list()
  params_POT <- list()
  lambda <- list()
  antseas <- list()
  nyear <- list()

  #Prepare Flood_events
  Flood_events$Year <- sapply(Flood_events$Peak_date, hydroyear)
  Flood_events$Month <- as.numeric(format(Flood_events$Peak_date, "%m"))


  for (i in g_const){
    GG[[i]]=data.frame(Month=Flood_events$Month[Flood_events$Type==i],HQ=Flood_events$HQ[Flood_events$Type==i])
  }

  MonatsMQ <- aggregate(Daily_discharge$Discharge, by=list(format(Daily_discharge$Date,"%m")), FUN = mean, na.rm=TRUE)$x


  min_events_reached <- sapply(GG, nrow)<3
  if(any(min_events_reached)){
    warning(paste0("Floodtype ", paste(g_const[min_events_reached], collapse = " - "),
      " has fewer than 3 flood events. Consider removing this flood type beforehand!"))
  }


  for (i in g_const){
    n[[i]]=length(GG[[i]][,1])
  }

  for (i in g_const){
    TH[[i]]<-0
    for(j in 1:12){
      TH[[i]]<-TH[[i]]+length(GG[[i]]$HQ[GG[[i]]$Month==j])/n[[i]]*MonatsMQ[j]
    }
    TH[[i]]<-as.numeric(TH[[i]]*Threshold_Q)
  }


  any_under_TH <- sapply(g_const, function(x) all(GG[[x]]$HQ<TH[[x]]))
  if(any(any_under_TH)){
    warning(paste0("Floodtype ", paste(g_const[any_under_TH], collapse = " - "),
      " has no events above the Threshold. Consider removing this flood type beforehand!"))
  }
  invalid_type <- (min_events_reached | any_under_TH)

  g_valid <- g_const[!invalid_type]
  g_invalid <- g_const[invalid_type]

  antseas <- sapply(n, function(x)  x /Reduce("+", n))
  nyear <- max(Flood_events$Year) - min(Flood_events$Year) + 1

  for(i in g_valid){
    params_POT[[i]]<-gpdFit(GG[[i]]$HQ, type="pwm",u=TH[[i]])@fit$par.ests
    params[[i]]<-gevFit(GG[[i]]$HQ, type="pwm")@fit$par.ests
  }


  ##Einzelwkeit##
  for (i in g_valid){
    lambda[[i]]=1/nyear*length(GG[[i]]$HQ[GG[[i]]$HQ>TH[[i]]])
  }



  #return Quantiles
  if(!is.null(p_input)){

    R <- matrix(NA, nrow = length(return_TMPS), ncol = length(p_input),
      dimnames = list(return_TMPS, p_input))

    for(i in seq_along(return_TMPS)){
      temp_return <- return_TMPS[i]

      if(temp_return %in% g_valid){

        R[i,] <- as.numeric(qgev(1-1/p_input,
          xi=params_POT[[temp_return]][1],
          beta=params_POT[[temp_return]][2]* lambda[[temp_return]]^params_POT[[temp_return]][1],
          mu=TH[[temp_return]]-(params_POT[[temp_return]][2] *(1-lambda[[temp_return]]^params_POT[[temp_return]][1]))/
            params_POT[[temp_return]][1]))

      }else if(temp_return %in% g_invalid){
        R[i,] <- rep(as.numeric(NA), length(p_input))

      }else if(temp_return == "TMPS"){
        R[i,] <- sapply(p_input, function(x) qPOT_mixmixPOT(x,params_POT, TH, params, antseas, g_valid))

      }else{
        stop(paste0("No type ", temp_return, " in floodtypes!"))
      }
    }
    return(R)
  }


  # return probabilities
  if(!is.null(q_input)){

    R <- matrix(NA, nrow = length(return_TMPS), ncol = length(q_input),
      dimnames = list(return_TMPS, q_input))

    for(i in seq_along(return_TMPS)){
      temp_return <- return_TMPS[i]

      if(temp_return %in% g_valid){

        R[i,] <- as.numeric(1/(1-pgev(q_input,
          xi=params_POT[[temp_return]][1],
          beta=params_POT[[temp_return]][2]* lambda[[temp_return]]^params_POT[[temp_return]][1],
          mu=TH[[temp_return]]-(params_POT[[temp_return]][2] *(1-lambda[[temp_return]]^params_POT[[temp_return]][1]))/
            params_POT[[temp_return]][1])))


      }else if(temp_return %in% g_invalid){
        R[i,] <- rep(as.numeric(NA), length(q_input))

      }else if(temp_return == "TMPS"){
        R[i,] <- sapply(q_input, function(y) 1/(1-MixmixPOT(y, params_POT, TH, params, antseas, g_valid)))

      }else{
        stop(paste0("No type ", temp_return, " in floodtypes!"))
      }
    }
    return(R)
  }

}





#' @keywords internal
hydroyear<-function(Data){
  if(is.na(Data[,1])){return(NA)}else{
    if(as.numeric(as.character(format(Data[,1],"%m")))>10){return(as.numeric(as.character(format(Data[,1],"%Y")))+1)}else{
      return(as.numeric(as.character(format(Data[,1],"%Y"))))
    }
  }
}

uniroot_NA<-function(f, interval,...){
  tryCatch({uniroot(f, interval,...)}, error=function(e){return(data.frame(root=NA))})

}


#Mischungsmodel
#' @keywords internal
qPOT_mixmixPOT<-function(Ta,params_POT,TH,params,antseas, AnzT){
  uniroot_NA(optfun_mixmixPOT,Ta=Ta,params_POT=params_POT,TH=TH,
    params=params,antseas=antseas, AnzT=AnzT, interval=c(0,10000000))$root
}


#' @keywords internal
optfun_mixmixPOT<-function(x,Ta,params_POT,TH,params,antseas, AnzT){
  MixmixPOT(x,params_POT,TH,params,antseas, AnzT)-(1-(1/Ta))
}


#' @keywords internal
MixmixPOT<-function(x,params_POT,TH,params,antseas, AnzT){
  1-1/TmixPOT(x,params_POT,TH,params,antseas, AnzT)
}


#' @keywords internal
TmixPOT<-function(x,params_POT,TH,params,antseas, AnzT){
  optim(10,optquan_v2, x=x,params_POT=params_POT,TH=TH,params=params,
    antseas=antseas, AnzT=AnzT,method="Brent",lower=0,upper=1000000000)$par
}


#' @keywords internal
optquan_v2<-function(x,Ta,params_POT,TH,params,antseas, AnzT){
  abs((1-mixPOT2(x,params_POT,TH,params,antseas,AnzT))-1/(Ta))
}



#' @keywords internal
mixPOT2<-function(x,params_POT,TH,params,antseas, AnzT){
  res<-1
  for(i in AnzT){
    res<-res*(pgev(TH[[i]], xi=params[[i]][1], mu=params[[i]][2],
      beta=params[[i]][3])+(pgpd(x,xi=params_POT[[i]][1], mu=TH[[i]], beta=params_POT[[i]][2]))*(1-pgev(TH[[i]],
        xi=params[[i]][1], mu=params[[i]][2], beta=params[[i]][3])))

  }
  return(as.numeric(res))
}



#' @keywords internal
hydroyear<-function(Dates){

  if(as.numeric(format(Dates,"%m"))>10){
    return(as.numeric(format(Dates,"%Y"))+1)
  }else{
    return(as.numeric(format(Dates,"%Y")))
  }
}

