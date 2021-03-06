#' Flood event separation based on variance
#'
#' Based on daily mean discharges, a window-variance is calculated. If this
#' variance exceeds a certain threshold, a flood event has been detected. To
#' determine the begin of the flood event, the lag 1 differences are
#' considered. The point, where the differences are positive for the last time
#' before the variance exceeds the threshold, is defined as the starting point
#' (including natural variability). The end of the flood event is defined as
#' the point where the sum of the rising limb of the event equals approximately
#' the sum of the falling limb.
#'
#' To characterize the flood regime, it is necessary to estimate single events
#' and to separate the amount of direct runoff from the total volume. For this
#' purposes, a procedure is developed, which is based on the following three
#' assumptions:
#'
#' • A flood event is an exceedance of the normal discharge within a time
#' span. For every flood event beginning and end has to be specified.
#'
#' • A flood event is characterized by significantly increased discharge
#' dynamics. Especially around the flood peaks, the sample variance of daily
#' discharges within a moving time window will be higher than for other time
#' periods with the same length.
#'
#' • For all flood events, the volume of the rising limb of the hydrograph
#' has to be equal the volume of the falling limb.
#'
#' An assumed increase of base flow during the event has to be considered. The
#' automatic event separation is based on the Lag one (1 day) differences of
#' the daily mean discharges \eqn{QD_i= Q_i-Q_{i-1}} , with \eqn{QD_1=0}, and
#' the sample variance of daily discharges within a moving window of dvar days:
#' \deqn{Var_{d_{var}}(i)=Var(Q_{i-(d_{var}-1)},\ldots,Q_i)}
#' \deqn{Var_{d_{var}}(j)=0, for j=1,…,dvar.} The appropriated window length
#' depends on the reaction time of the watershed and in this way from the
#' catchment size. In our studies we modified it between 3 and 7 days, but 3
#' days were appropriated for nearly all catchments.  To indicate the time span
#' of the flood event, the sample variance has to exceed a certain threshold.
#' We defined this threshold with the mean plus \eqn{0.25 \sigma} variance of
#' all sample variances which could be derived within the long discharge
#' series: \deqn{T{{H}_{var}}=\overline{{{V}_{{{d}_{var}}}}}+0.25\cdot
#' \sqrt{Var({{V}_{{{d}_{var}}}})}.} The peak is located within the time span,
#' where the sample variance exceeds this threshold.  The starting point of the
#' flood event was defined as the time step in front of the peak for which the
#' lag one-difference QD is negative for the last time:
#' \deqn{t_{start}=\max\lbrace 1\leq i < t_{peak} | QD_i <0 \rbrace.} From this
#' point on, discharges increase until the peak is reached.  However, this
#' increase does not always indicate the true start of the flood event.
#' Instead, the natural variability in discharge can lead to an increase. To
#' consider this, a temporal decrease between two discharges in succession of
#' 10 percent was accepted. In such cases, the starting point was shifted
#' further in the future (\eqn{Q_{t_{start}}= Q_{t_{start}+1}}) until the
#' following condition was fulfilled:
#' \deqn{|({{Q}_{{{t}_{s}}_{tart}}}-{{Q}_{{{t}_{start}}+1}})/{{Q}_{{{t}_{start}}+1}}|<0.1.}
#' The rising limb of a flood event is not steady, often pre-floods occured. We
#' handled this by setting \eqn{t_{start}=t_{start}-2}, if
#' \eqn{(Q_{t_{start}-1}-Q_{t_{start}-2})> 0.4 (Q_{t_{peak}}-Q_{t_{start}})}.
#' This condition ensures that the rising limb is included in total in the
#' flood event. For the case where the rising limb of the two days before the
#' peak has been larger than \eqn{40\%} of the actual limb we included these
#' days in the flood event since they define a pre-flood. To define the end of
#' the flood event we needed to find the time for which the differences between
#' the volumes of the rising limb (SI) and the falling limb is approximately
#' zero. Since also a possible increase of the baseflow during the flood event
#' has to be considered, we define the end by \deqn{{{t}_{end}}= \min \left\{
#' {{t}_{peak}}+1\le t\le n|\left( {{\beta }_{B;k}}\le median({{\beta }_{B}})
#' \right)\vee \left( \left( \sum\limits_{i={{t}_{peak}}+1}^{t}{Q}{{D}_{i}}\le
#' {{Q}_{{{t}_{start}}}} \right)\wedge \\ \left( 0.2\left(
#' SI-\sum\limits_{i={{t}_{peak}}+1}^{t}{Q}{{D}_{i}} \right)\ge
#' (Q{{D}_{t+1}}+Q{{D}_{t+2}}) \right) \right) \right\}, } where
#' \eqn{\beta_{B;k}} is the slope of the baseflow of the event. This includes
#' two criteria: the volume of the falling limb is smaller than the baseflow at
#' the beginning of the flood and the following two days the difference between
#' the volume of the rising limb and the falling limb is not reduced by more
#' than \eqn{20\%}. Alternatively, also the slope of the baseflow between start
#' and end of the hydrograph is smaller than the median of the slope of the
#' baseflow for all other flood events of the considered gauge. The number of
#' peaks of the flood event then can be estimated by the number of time periods
#' of exceedance of the variance threshold.
#'
#' A special case that cannot be addresses directly by the procedure described
#' above are flood events with multiple peaks. Here, we want to divide into two
#' main cases: the superposition of two flood events and superimposed floods.
#' In the first case, a second flood event begins before the recession of the
#' first event has ended. This leads to an enhancement of the second event. The
#' second case corresponds to a time period of an already increased baseflow,
#' e.g. due to snowmelt. If flood events occur during this period, they are
#' superimposed on this increased baseflow. Of course, not every flood event
#' with multiple peaks belong to either of this cases. We define that a flood
#' event consists potentially of superpositioned events of the originally
#' separated event has a duration of less than 40 days. This parameter can be
#' chosen according to the catchment size. Since the Mulde basin only consist
#' of mesoscale catchments, this threshold is sufficient for our purpose. We
#' then apply the independence criterion by Klein (2009) to validate if there
#' are independent peaks within this event, i.e. two peaks - That deviate by
#' less than five times the smaller peak - For which the larger peak is more
#' than 2.5 times as large as the smallest discharge between both peaks - For
#' which \eqn{70\%} of the smaller peak are larger than the smalles discharge
#' between two peaks.
#'
#' This criterion ensures that both peak are large enough to be separate flood
#' event and that the recession of the first event is large enough to state
#' them as independent. For all peaks that fulfill this criterion we choose the
#' two peaks with the largest recession in between. According to this, two
#' sub-events are defined, the first one with starting point equal to the
#' original event begin. The end of this event is estimated by the lowest
#' discharge between both peaks to which the recession of the original event is
#' added (that are all discharge values smaller than the values of the valley)
#' to reconstruct the overlaid recession of the first event. The second events
#' begins with the smallest discharge value between both peaks and end with the
#' original ending. Superimposed events can occur if the originally separated
#' event has a duration larger than 40 days. Again, all peaks are checked for
#' independence and are separated again. Since the separation of multiple peak
#' events, especially of superimposed events is rather difficult, a manual
#' check afterwards is recommended.
#'
#' @param dailyMQ A dataframe where the date is given as R-date or string
#' (d.m.Y)) in the first and the daily mean discharges are given in the second
#' column.
#' @param monthlyHQ A dataframe where the date is given as R-date or string
#' (d.m.Y) in the first and the monthly maximum discharge peaks are given in
#' the second column.
#' @param dvar integer: The window length of the moving variance in days. By
#' default, it is set to three days.
#' @param gamma numeric: Parameter for finding the start.
#' @param theta numeric: The proportion of the sample variance of the
#' dvar-day-window variance on the threshold. By default it is set to
#' theta=0.25.
#' @param ddur integer: The approximate minimum length of an overlaid event in
#' days. By default it is set to ddur=40.
#' @param omega integer: Threshold parameter for defining the end of a flood
#' event. By default it is set to omega=2.
#' @param Kappa numeric: Threshold parameter for defining the begin of a flood
#' event. By default it is set to Kappa=0.4.
#' @param eta numeric: Threshold parameter for defining the begin of a flood
#' event. By default it is set to eta=0.1.
#' @param delta numeric: Threshold parameter for defining the end of a flood
#' event. By default it is set to delta=0.2.
#' @param usemed logical: Should the median of relative difference between the
#' baseflow at the end and the begin be considered to determine the end of the
#' flood event.
#' @param medbf numeric: The median of relative difference between the baseflow
#' at the end and the begin of all separated flood events. Used to define the
#' end of the event, if usemed=TRUE.
#' @param NA_mode integer: If the timeseries does contain any NA values,
#' NA_mode can be used to interpolate all gaps of continious NA-values by
#' linear interpolation. Only gaps smaller or equal to NA_mode (>=0) will be
#' interpolated.  Additionally, if the timeseries contains larger gaps, the
#' timeseries is splited into subsamples with each being seperated intependend.
#' The resulting separation table will still be append together.
#' @param Messages logical: Should messages be thrown out
#' @return A dataframe is returned with the following columns, where each row
#' is a flood event:
#'
#' \item{Begin}{date (d.m.Y) of the begin of the flood event} \item{End}{date
#' (d.m.Y) of the end of the flood event} \item{Peak_date}{date (d.m.Y) of the
#' maximum daily discharge during the flood event} \item{DailyMQ}{maximum daily
#' mean discharge [m³/s] during the flood event} \item{Volume}{volume [Mio.
#' m³] of the flood event calculated by the sum of all daily mean discharges
#' during the flood event} \item{dir_Volume}{direct volume [Mio. m³]
#' calculated by the difference of the volume and the baseflow. The baseflow is
#' estimated by a straight line between the discharge at the beginning and the
#' end of the flood event.} \item{baseflow_peak}{baseflow [m³/s] at the day of
#' the flood peak, calculated by the straight-line method.}
#' \item{baseflow_begin}{baseflow [m³/s] at the beginning of the flood event,
#' equal to the daily mean discharge of the beginning of the flood event.}
#' \item{baseflow_end}{baseflow [m³/s] at the end of the flood event, equal to
#' the daily mean discharge of the end of the flood event.}
#' \item{No_peaks}{number of peaks during the flood event} \item{HQ}{peak
#' discharge [m³/s] of the flood event taken from the monthly maximum
#' discharges. If no monthly maximum discharge is available for the flood
#' event, it is set to NA.} \item{HQ_dir}{direct peak discharge, claculated by
#' the difference of HQ and baseflow_peak} \item{comments}{a short note if the
#' event is overlaid, or first respectively second part of a double-peaked
#' flood event.}
#' @note Important note: If a double-peaked or an overlaid event occurs, the
#' whole event is listed in the output first, followed by the splitted partial
#' events.
#' @author Svenja Fischer
#' @references Klein, B. (2009): Ermittlung von Ganglinien für die
#' risikoorientierte Hochwasserbemessung von Talsperren. Schriftenreihe des
#' Lehrstuhls Hydrologie, Wasserwirtschaft und Umwelttechnik, Ruhr-University
#' Bochum.
#' @references Fischer, S., Schumann, A., & Bühler, P. (2019):
#' Timescale-based flood typing to estimate temporal changes in flood frequencies.
#'  Hydrological Sciences Journal, 64(15), 1867–1892. https://doi.org/10.1080/02626667.2019.1679376
#' @keywords ~classif ~ts
#' @examples
#' \dontrun{
#'
#' dailyMQ<-data.frame(Date=seq(from=as.Date("01.01.2000", format="%d.%m.%Y"),
#' to=as.Date("01.01.2004", format="%d.%m.%Y"), by="days"),
#' discharge=rbeta(1462,2,20)*100)
#'
#' monthlyHQ<-data.frame(Date=seq(from=as.Date("01.01.2000", format="%d.%m.%Y"),
#' to=as.Date("01.01.2004", format="%d.%m.%Y"), by="months"),
#' discharge=dailyMQ$discharge[(0:48)*12+1]+rnorm(49,5,1))
#'
#' eventsep(dailyMQ, monthlyHQ)
#' }
#' @importFrom stats approxfun integrate kmeans lm quantile sd var
#' @export eventsep
eventsep <- function(dailyMQ, monthlyHQ=NULL, dvar=3, gamma=1, theta=0.25, ddur=40,omega=2,
  Kappa=0.4, eta=0.1, delta=0.2, usemed=FALSE, medbf=0.5, NA_mode = NULL, Messages=TRUE){

  if(is.null(monthlyHQ)){
    monthlyHQ <- data.frame(NA,NA)
  }else{
    stopifnot(any(class(monthlyHQ[[1]]) %in% c("POSIXct", "POSIXt","Date")))
  }
  monthlyHQ <- as.data.frame(monthlyHQ)

  daten<-as.data.frame(dailyMQ[,1:2])
  stopifnot(any(class(daten[[1]]) %in% c("POSIXct", "POSIXt","Date")))

  if(any(class(daten[[1]]) == "Date")){
    convol <- 1L
  }else{
    convol <- 60^2
  }

  if(!is.null(NA_mode)){
    #Interpolate single values
    daten_NA <-daten[,2]
    daten_NA[!is.na(daten_NA)] <- 0
    daten_NA[is.na(daten_NA)] <- 1
    rl <- rle(daten_NA)
    to_int <- which(rl$lengths <= NA_mode & rl$values==1)
    if(length(to_int)>=1){
      for(i in 1:length(to_int)){
        inds_to_interp <- (sum(rl$lengths[1:(to_int[i]-1)])) : (sum(rl$lengths[1:to_int[i]])+1)
        daten[inds_to_interp,2] <- seq(daten[inds_to_interp[1],2], daten[inds_to_interp[length(inds_to_interp)],2],
          length.out=length(inds_to_interp))
      }
    }
    if(Messages) cat("Interpolated",  length(to_int), "NA-gaps\n")

    daten_NA <-daten[,2]
    daten_NA[!is.na(daten_NA)] <- 0
    daten_NA[is.na(daten_NA)] <- 1

    spl <- rep(NA, length(daten_NA))
    lastNA <- FALSE
    cc=1
    for(i in 1:length(daten_NA)){
      if(daten_NA[i]==0 & lastNA==TRUE){
        cc=cc+1
        spl[i] <- cc
        lastNA <- FALSE
      }else if(daten_NA[i]==0 & lastNA==FALSE) {
        spl[i] <- cc
        lastNA <- FALSE
      }else{
        spl[i] <- NA
        lastNA <- TRUE
      }
    }
    daten_list <- split(daten, spl)
    if(length(daten_list)>1){
      if(Messages) cat("Splitted timeseries into",  length(daten_list), "parts, but appending results together\n")
    }
  }else{
    spl <- rep(1, nrow(daten))
    if(!identical(unique(diff(daten[,1])), 1)) stop("Timeseries not continious!\n")
    if(any(is.na(daten[,2]))) stop("Timeseries does contain NA, but NA_mode is not set!\n")
    daten_list <- list(daten)
  }



  #calculate the window-variance with window length dvar
  data_temp_var3d <- daten
  var3d<-rep(0,length(data_temp_var3d[,1]))
  for(i in dvar:length(data_temp_var3d[,1])){
    var3d[i]<-var(data_temp_var3d[(i-(dvar-1)):i,2],na.rm = TRUE)
  }
  var3d_list <- split(var3d, spl)

  #calculate the theshold for the variance
  thvar<-mean(var3d[dvar:length(var3d)], na.rm = TRUE)+theta*sd(var3d[dvar:length(var3d)], na.rm = TRUE)

  #Start actual sep
  for(list_it in seq_along(daten_list)){
    daten <- daten_list[[list_it]]
    var3d <- var3d_list[[list_it]]

    diffs<-diff(daten[,2], lag=1)
    diffs<-c(0,diffs)



    #claculate the cumulative Lag 1 differences
    if(any(is.na(diffs))){
      cumdiffs<-numeric(length(diffs))
      cumdiffs[1]<-diffs[1]
      for(k in 2:length(diffs)){
        if(is.na(diffs[k])){
          cumdiffs[k]<-0
        }else{
          cumdiffs[k]<-cumdiffs[k-1]+diffs[k]
        }
      }
    }else{
      cumdiffs<-cumsum(diffs)
    }



    #start iterating the days until the variance threshold is exceeded
    events <- data.frame(NULL)
    n2<-0
    i<- 10
    while(i < length(var3d)){
      old_start<-0
      i<-i+1
      if(!is.na(var3d[i])){
        if(var3d[i]>thvar){
          var3dpart<-var3d[(i):length(var3d)]
          endvar<-min(which(var3dpart<thvar)[1],length(var3dpart), na.rm=TRUE)
          peak_ind<-min((i + which.max(var3dpart[1:endvar])-1), (length(var3d)-10))
          while(daten[peak_ind-1,2]>daten[peak_ind,2]){
            peak_ind<-peak_ind-1
          }


          #chosse the event start as the days where the lag 1  differences are negative the last time
          pos_start<-max(which(diffs[1:(peak_ind-1)]<0),3)

          #modify start according to the assumptions
          while(((daten[pos_start+1,2]-daten[pos_start,2])<
              ((daten[peak_ind,2]-daten[pos_start,2])*eta)) && (pos_start<peak_ind-1)) {
            pos_start<-pos_start+1
          }

          gammaneu<-min(gamma,pos_start-2)
          maxstart<-matrix(NA, nrow=gammaneu, ncol=gammaneu+1)
          for(z in 1:gammaneu){
            for (v in (z+1):(gammaneu+1)){
              maxstart[z,v]<-daten[pos_start-z,2]-daten[pos_start-v,2]
            }
          }

          if((max(maxstart, na.rm = TRUE)>((daten[peak_ind,2]-daten[pos_start,2])*Kappa)) &&
              (daten[pos_start-2,2]<daten[peak_ind,2])){
            pos_start<-max(pos_start-which(maxstart == max(maxstart, na.rm=TRUE), arr.ind = TRUE)[2],3)
          }

          if(daten[pos_start,2]>daten[pos_start+1,2]) pos_start<-pos_start+1
          while(daten[peak_ind,2]<=daten[peak_ind+1,2]&& peak_ind < (length(var3d)-2)){
            peak_ind<-peak_ind+1
          }

          #calculate the sum of the rising limb
          incsum<- sum(diffs[(pos_start+1):peak_ind])



          #define the end of the event
          pos_end<-peak_ind+1


          basefl1<-approxfun(c(daten[pos_start,1],daten[pos_end,1]),c(daten[pos_start,2],daten[pos_end,2]))
          base_diff<-(basefl1(daten[pos_start:pos_end,1])-daten[pos_start:pos_end,2])
          base_rel<-(basefl1(daten[pos_end,1])-basefl1(daten[pos_start,1]))/(pos_end-pos_start)


          #use median baseflow difference?
          if(usemed){
            while((((((sum(diffs[(peak_ind+1):(pos_end+omega)], na.rm = TRUE)-sum(diffs[(peak_ind+1):(pos_end)], na.rm = TRUE))/
                sum(diffs[(peak_ind+1):(pos_end)], na.rm = TRUE))>(1+delta))||
                any(base_diff>0) || (base_rel>(2*medbf)) ||
                ((incsum+sum(diffs[(peak_ind+1):pos_end], na.rm = TRUE))>(1*daten[pos_start,2]))))){

              if(is.na(cumdiffs[pos_end+1])){break}
              pos_end<-pos_end+1

              basefl1<-approxfun(c(daten[pos_start,1],daten[pos_end,1]),c(daten[pos_start,2],daten[pos_end,2]))
              base_diff<-(basefl1(daten[pos_start:pos_end,1])-daten[pos_start:pos_end,2])
              base_rel<-(basefl1(daten[pos_end,1])-basefl1(daten[pos_start,1]))/(pos_end-pos_start)
            }
          }else{
            while((((((sum(diffs[(peak_ind+1):(pos_end+omega)], na.rm = TRUE)-sum(diffs[(peak_ind+1):(pos_end)], na.rm = TRUE))/
                sum(diffs[(peak_ind+1):(pos_end)], na.rm = TRUE))>(1+delta))||
                any(base_diff>0)  ||
                ((incsum+sum(diffs[(peak_ind+1):pos_end], na.rm = TRUE))>(1*daten[pos_start,2])))) ){
              if(is.na(cumdiffs[pos_end+1])){break}
              pos_end<-pos_end+1

              basefl1<-approxfun(c(daten[pos_start,1],daten[pos_end,1]),c(daten[pos_start,2],daten[pos_end,2]))
              base_diff<-(basefl1(daten[pos_start:pos_end,1])-daten[pos_start:pos_end,2])
              base_rel<-(basefl1(daten[pos_end,1])-basefl1(daten[pos_start,1]))/(pos_end-pos_start)
            }
          }

          if(pos_end<length(daten[,1])) pos_end<-pos_end+1

          if(daten[pos_end,2]>=daten[pos_end-1,2]) pos_end<-pos_end-1

          while(daten[pos_end,2]<daten[pos_start,2]) pos_end<-pos_end-1


          No_peaks_all<-sum(diff(sign(diff(daten[pos_start:pos_end,2]))) < -1)

          no_max<-diff(sign(diff(var3d[pos_start:pos_end])))
          no_peaks<-max(1,sum((no_max< -1) & (var3d[(pos_start+1):(pos_end-1)]>thvar) &
              (diff(sign(diff(daten[pos_start:pos_end,2])))< -1)))

          peak_ind<-which.max(daten[pos_start:pos_end,2])+pos_start-1


          #create output for event
          if(!((peak_ind == pos_start) || (No_peaks_all==0) || (ncol(events)>=3 && daten[peak_ind,1] %in% events[[3]]))){

            basefl<-approxfun(c(daten[pos_start,1],daten[pos_end,1]),c(daten[pos_start,2],daten[pos_end,2]))
            start<-daten[pos_start,1]
            end<-daten[pos_end,1]
            peak_MQ<-max(daten[(pos_start):(pos_end),2])
            peak_date<-daten[peak_ind,1]
            Volume<-sum(daten[(pos_start):(pos_end),2])*24*60*60/1000000
            Baseflow_peak<-basefl(peak_date)
            Base_vol<-integrate(basefl, lower=daten[pos_start,1], upper=daten[pos_end,1])$value*24*60*60/1000000
            Vol_dir<-Volume-Base_vol
            base_start<-basefl(start)
            base_end<-basefl(end)
            HQ<-monthlyHQ[which((monthlyHQ[,1]>=(peak_date-1)) & (monthlyHQ[,1]<=(peak_date+1))),2 ]
            if(length(HQ)<1){HQ<-NA}else{HQ<-max(HQ)}
            if(!is.na(HQ) && (HQ<peak_MQ)){HQ<-NA}
            HQ_dir<-HQ-Baseflow_peak
            comm<-" "

            if(any(end<events$End)) comm<-"aufgesetzt"

            event_temp <- data.frame(start, end, peak_date,peak_MQ, Volume, Vol_dir, Baseflow_peak,base_start,base_end,no_peaks,HQ, HQ_dir, comm)
            names(event_temp) <- c("Begin", "End", "Peak_date", "DailyMQ", "Volume", "dir_Volume",
              "baseflow_peak","baseflow_begin","baseflow_end", "No_Peaks", "HQ", "HQ_dir", "Comments")
            events<-rbind(events, event_temp)
          }

          #multiple peak event?
          # if(No_peaks_all>=400) browser()

          if(No_peaks_all>1){

            #possible double-peaked event
            if((pos_end-pos_start+1)<ddur){
              peaks<-pos_start+which(diff(sign(diffs[pos_start:pos_end]) ) < -1)-1

              old_start<-pos_start
              old_end<-pos_end

              maxis<-mapply(function(x,y){max(daten[x,2],daten[y,2])},x=peaks[1:(length(peaks)-1)],y=peaks[2:length(peaks)])
              minmax<-mapply(function(x,y){min(daten[x,2],daten[y,2])},x=peaks[1:(length(peaks)-1)],y=peaks[2:length(peaks)])

              minis<-mapply(function(x,y){min(daten[x:y,2])},x=peaks[1:(length(peaks)-1)],y=peaks[2:length(peaks)])

              #test the condition for double-peaked events
              if(any(((0.4*maxis)>= minis) & ((maxis*0.2)<=minmax) & (minis <= (0.7*minmax)))){ ####Fall?berlagerung

                max_diff<-minmax-minis

                ind_diff<-which(((0.4*maxis)>= minis) & ((maxis*0.2)<=minmax) & (minis <= (0.7*minmax)))

                ind2<-which.max(max_diff[ind_diff])

                peak_ind<-peaks[ind_diff[ind2]]

                pos_start<-old_start

                pos_end<-which.min(daten[peak_ind:peaks[ind_diff[ind2]+1],2])+peak_ind-1

                ##construct first wave
                wave<-daten[pos_start:pos_end,]

                n1<-length(wave[,1])
                n_save<-n1

                while(daten[old_end,2]<wave[n1,2]){

                  next_ind<-which(daten[(pos_start+n1+1):old_end,2]< wave[n1,2])[1]+pos_start+n1 ##positiv!!!

                  wave<-rbind(wave, daten[next_ind,])

                  n1<-length(wave[,1])

                }

                no_max<-diff(sign(diff(var3d[old_start:(old_start+n1-1)])))
                no_peaks<-max(1,sum((no_max< -1) & (var3d[(old_start+1):(old_start+n1-2)]>thvar) & (diff(sign(diff(daten[old_start:(old_start+n1-1),2])))< -1)))

                peak_ind<-which.max(wave[,2])
                #characteristics of first wave
                if((peak_ind != 1)){
                  basefl<-approxfun(c(1,n1),c(wave[1,2],wave[n1,2]))
                  start<-wave[1,1]
                  end<-start+((n1-1)*convol)
                  peak_MQ<-max(wave[,2])
                  peak_date<-wave[peak_ind,1]
                  Volume<-sum(wave[,2])*24*60*60/1000000
                  Baseflow_peak<-basefl(peak_ind)
                  Base_vol<-integrate(basefl, lower=1, upper=n1)$value*24*60*60/1000000
                  Vol_dir<-Volume-Base_vol
                  base_start<-basefl(1)
                  base_end<-basefl(n1)
                  HQ<-monthlyHQ[which((monthlyHQ[,1]>=(peak_date-1)) & (monthlyHQ[,1]<=(peak_date+1))),2 ]
                  if(length(HQ)<1){HQ<-NA}else{HQ<-max(HQ)}
                  if(!is.na(HQ) && (HQ<peak_MQ)){HQ<-NA}
                  HQ_dir<-HQ-Baseflow_peak
                  comm<-"first wave"

                  event_temp <- data.frame(start, end, peak_date,peak_MQ, Volume, Vol_dir, Baseflow_peak,base_start,base_end,no_peaks,HQ, HQ_dir, comm)
                  names(event_temp) <- c("Begin", "End", "Peak_date", "DailyMQ", "Volume", "dir_Volume",
                    "baseflow_peak","baseflow_begin","baseflow_end", "No_Peaks", "HQ", "HQ_dir", "Comments")
                  events<-rbind(events, event_temp)
                }


                # construct second wave
                sec_wave<-daten[pos_end:old_end,]
                sec_wave[1:(n1-n_save+1),2]<-sec_wave[1:(n1-n_save+1),2]-wave[(n_save):n1,2]

                n2<-length(sec_wave[,2])

                no_max<-diff(sign(diff(var3d[(old_start+n1-1):old_end])))
                no_peaks<-max(1,sum((no_max< -1) & (var3d[(old_start+n1):(old_end-1)]>thvar) & (diff(sign(diff(daten[(old_start+n1-1):old_end,2])))< -1)))


                peak_ind<-which.max(daten[pos_end:old_end,2])

                #characteristics of second wave
                if(peak_ind != 1){
                  basefl<-approxfun(c(1,n2),c(sec_wave[1,2],sec_wave[n2,2]))
                  start<-sec_wave[1,1]
                  end<-start+((n2-1)*convol)
                  peak_MQ<-max(sec_wave[,2])
                  peak_date<-sec_wave[peak_ind,1]
                  Volume<-sum(sec_wave[,2])*24*60*60/1000000
                  Baseflow_peak<-basefl(peak_ind)
                  Base_vol<-integrate(basefl, lower=1, upper=n2)$value*24*60*60/1000000
                  Vol_dir<-Volume-Base_vol
                  base_start<-basefl(1)
                  base_end<-basefl(n2)
                  HQ<-monthlyHQ[which((monthlyHQ[,1]>=(peak_date-1)) & (monthlyHQ[,1]<=(peak_date+1))),2 ]
                  if(length(HQ)<1){HQ<-NA}else{HQ<-max(HQ)}
                  if(!is.na(HQ) && (HQ<peak_MQ)){HQ<-NA}
                  HQ_dir<-HQ-Baseflow_peak
                  comm<-"second wave"

                  event_temp <- data.frame(start, end, peak_date,peak_MQ, Volume, Vol_dir, Baseflow_peak,base_start,base_end,no_peaks,HQ, HQ_dir, comm)
                  names(event_temp) <- c("Begin", "End", "Peak_date", "DailyMQ", "Volume", "dir_Volume",
                    "baseflow_peak","baseflow_begin","baseflow_end", "No_Peaks", "HQ", "HQ_dir", "Comments")
                  events<-rbind(events, event_temp)

                }
              }


              #possible overlaid event
            }else{
              peaks<-pos_start+which(diff(sign(diffs[pos_start:pos_end])) < -1)-1
              real_peaks<-peaks[daten[peaks,2]>=(max(daten[peaks,2])*0.2)]
              old_start<-pos_start
              old_end<-pos_end

              #check the assumptions on overlaid events
              if(any(diff(daten[real_peaks,1])>7)){
                ind_ev<-match(real_peaks[which(diff(daten[real_peaks,1])>7)], peaks)


                #split the overlaid waves from the main event
                for(j in 1:length(ind_ev)){
                  if((0.5*max(daten[peaks[ind_ev[j]],2],daten[peaks[ind_ev[j]+1],2]))>= min(daten[peaks[ind_ev[j]]:(peaks[ind_ev[j]+1]),2])){

                    pos_end<-peaks[ind_ev[j]]+1

                    while(diffs[pos_end]<=0){
                      if(is.na(diffs[pos_end])){break}
                      pos_end<-pos_end+1

                    }


                    if(daten[pos_end,2]>daten[pos_end-1,2]){pos_end<-pos_end-1}
                    peak_ind<-max(peaks[max(ind_ev[j-1]+1,1)])

                    pos_start<-peak_ind-1
                    while(diffs[pos_start]>=0){
                      if(is.na(diffs[pos_start])){break}
                      pos_start<-pos_start-1

                    }
                    while(daten[pos_start,2]<daten[pos_end,2]){
                      pos_start<-pos_start+1

                    }
                    pos_start<-pos_start-1

                    if(daten[pos_start,2]>=daten[pos_start+1,2]){pos_start<-pos_start+1}

                    if(any(daten[(pos_start+1):peaks[ind_ev[j]],2]<daten[pos_start,2])){
                      pos_start<-max(which(daten[(pos_start+1):peaks[ind_ev[j]],2]==min(daten[(pos_start+1):peaks[ind_ev[j]],2])))+pos_start
                    }


                    no_max<-diff(sign(diff(var3d[pos_start:pos_end])))
                    no_peaks<-max(1,sum((no_max< -1) & (var3d[(pos_start+1):(pos_end-1)]>thvar) & (diff(sign(diff(daten[pos_start:pos_end,2])))< -1)))


                    peak_ind<-which.max(daten[pos_start:pos_end,2])+pos_start-1

                    if(peak_ind != pos_start){
                      basefl<-approxfun(c(daten[pos_start,1],daten[pos_end,1]),c(daten[pos_start,2],daten[pos_end,2]))
                      start<-daten[pos_start,1]
                      end<-daten[pos_end,1]
                      peak_MQ<-max(daten[(pos_start):(pos_end),2])
                      peak_date<-daten[peak_ind,1]
                      Volume<-sum(daten[(pos_start):(pos_end),2])*24*60*60/1000000
                      Baseflow_peak<-basefl(peak_date)
                      Base_vol<-integrate(basefl, lower=daten[pos_start,1], upper=daten[pos_end,1])$value*24*60*60/1000000
                      Vol_dir<-Volume-Base_vol
                      base_start<-basefl(start)
                      base_end<-basefl(end)
                      HQ<-monthlyHQ[which((monthlyHQ[,1]>=(peak_date-1)) & (monthlyHQ[,1]<=(peak_date+1))),2 ]
                      if(length(HQ)<1){HQ<-NA}else{HQ<-max(HQ)}
                      if(!is.na(HQ) && (HQ<peak_MQ)){HQ<-NA}
                      HQ_dir<-HQ-Baseflow_peak
                      comm<-"overlaid"

                      event_temp <- data.frame(start, end, peak_date,peak_MQ, Volume, Vol_dir, Baseflow_peak,base_start,base_end,no_peaks,HQ, HQ_dir, comm)
                      names(event_temp) <- c("Begin", "End", "Peak_date", "DailyMQ", "Volume", "dir_Volume",
                        "baseflow_peak","baseflow_begin","baseflow_end", "No_Peaks", "HQ", "HQ_dir", "Comments")
                      events<-rbind(events, event_temp)
                    }
                  }
                }

                #remaining part of overlaid wave
                if(any((peaks> pos_end) & (peaks>(peak_ind+7)) & (daten[peaks,2]>0.2*max(daten[peaks,2])))){
                  peak_ind<-which((peaks> pos_end) & (peaks>(peak_ind+7)) & (daten[peaks,2]>0.2*max(daten[peaks,2])))
                  peak_ind<-peaks[peak_ind[which.max(daten[peak_ind,2])]]

                  pos_end<-peaks[length(peaks)]+1

                  while(diffs[pos_end]<=0){
                    if(is.na(diffs[pos_end+1])) break
                    pos_end<-pos_end+1
                  }

                  if(daten[pos_end,2]>daten[pos_end-1,2])pos_end<-pos_end-1
                  if(pos_end>old_end)pos_end<-old_end

                  pos_start<-peaks[ind_ev[length(ind_ev)]+1]
                  while(diffs[pos_start]>=0){
                    if(is.na(diffs[pos_start])){break}
                    pos_start<-pos_start-1

                  }
                  while(daten[pos_start,2]<daten[pos_end,2]) pos_start<-pos_start+1

                  pos_start<-pos_start-1

                  if(daten[pos_start,2]>=daten[pos_start+1,2]) pos_start<-pos_start+1

                  if(any(daten[(pos_start+1):(which.max(daten[pos_start:pos_end,2])+pos_start-1),2]<daten[pos_start,2])){
                    pos_start<-max(which(daten[(pos_start+1):(which.max(daten[pos_start:pos_end,2])+pos_start-1),2]==
                        min(daten[(pos_start+1):(which.max(daten[pos_start:pos_end,2])+pos_start-1),2])))+pos_start
                  }


                  no_max<-diff(sign(diff(var3d[pos_start:pos_end])))
                  no_peaks<-max(1,sum((no_max< -1) & (var3d[(pos_start+1):(pos_end-1)]>thvar) & (diff(sign(diff(daten[pos_start:pos_end,2])))< -1)))


                  peak_ind<-which.max(daten[pos_start:pos_end,2])+pos_start-1

                  if((peak_ind != pos_start)){
                    basefl<-approxfun(c(daten[pos_start,1],daten[pos_end,1]),c(daten[pos_start,2],daten[pos_end,2]))
                    start<-daten[pos_start,1]
                    end<-daten[pos_end,1]
                    peak_MQ<-max(daten[(pos_start):(pos_end),2])
                    peak_date<-daten[peak_ind,1]
                    Volume<-sum(daten[(pos_start):(pos_end),2])*24*60*60/1000000
                    Baseflow_peak<-basefl(peak_date)
                    Base_vol<-integrate(basefl, lower=daten[pos_start,1], upper=daten[pos_end,1])$value*24*60*60/1000000
                    Vol_dir<-Volume-Base_vol
                    base_start<-basefl(start)
                    base_end<-basefl(end)
                    HQ<-monthlyHQ[which((monthlyHQ[,1]>=(peak_date-1)) & (monthlyHQ[,1]<=(peak_date+1))),2 ]
                    if(length(HQ)<1){HQ<-NA}else{HQ<-max(HQ)}
                    if(!is.na(HQ) && (HQ<peak_MQ)){HQ<-NA}
                    HQ_dir<-HQ-Baseflow_peak
                    comm<-"overlaid"

                    event_temp <- data.frame(start, end, peak_date,peak_MQ, Volume, Vol_dir, Baseflow_peak,base_start,base_end,no_peaks,HQ, HQ_dir, comm)
                    names(event_temp) <- c("Begin", "End", "Peak_date", "DailyMQ", "Volume", "dir_Volume",
                      "baseflow_peak","baseflow_begin","baseflow_end", "No_Peaks", "HQ", "HQ_dir", "Comments")
                    t<-rbind(events, event_temp)

                  }
                }
              }
            }
          }

          i<-max(pos_end, i+endvar, old_start+n2-1)

        }
      }
    }

    if(list_it==1){
      events_all <- events
    }else{
      events_all <- rbind(events_all,events)
    }
  }

  return(events_all)
}
