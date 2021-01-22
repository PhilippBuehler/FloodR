#M1 Test for independence






#' Determine the begin of event precipitation
#'
#' The cumulative precipitation is used to estimate the begin of the event
#' precipitation using a change point estimation. For this, the time series is
#' divided into two subsamples and the slope is estimated for each subsample
#' using linear regression and least squares estimation. This is done for all
#' possible combinations of two consecutive subsamples, such that the change
#' point can be estimated as the point where the maximum difference between the
#' slopes of the two subsamples occurs.
#'
#' We want to estimated the event precipitation by comparing the rising limb of
#' the discharge with the rising limb of precipitation. Here, we assumed that
#' the begin of the increase of the hydrograph indicates the begin of the event
#' precipitation, whereas the end of the flood event automatically defines the
#' end of the event precipitation. Due to catchment reaction times, a possible
#' delay of the begin of event precipitation and the begin of the flood event
#' has to be considered. For this purpose a buffer time period b between the
#' begin of the precipitation and the flood event was used. Here, b was defined
#' as \eqn{b=\max (rt,7)}, where rt is the duration of the rising limb of the
#' flood event. Then the begin of event precipitation \eqn{hat{k}} was defined
#' by the change-point of the slope of the cumulative precipitation sum in this
#' period: \deqn{\hat{k}=\arg {{\max }_{{{t}_{start}}-b\le k\le
#' {{t}_{peak}}+b}}\{{{\beta }_{k:{{t}_{peak}}+b}}-{{\beta
#' }_{{{t}_{start}}-b:k}}\},} where \eqn{\beta_{i:j}} is the slope of the
#' linear regression for the sum of all precipitation values
#' \eqn{P_i,\ldots,P_j} derived by least-squares method. This means, we divided
#' the cumulative sums of the precipitation beginning b days before the begin
#' of the flood event and ending b days after the day of the flood peak into
#' two parts and used the time step where the difference of the slope of these
#' two parts was maximal to define the begin of the precipitation. Each part
#' had to consist of at least three days.  The end of the event precipitation
#' was defined equally to one day before the end of flood event.
#'
#' @param X A dataframe with the first column equal to the date (d.m.Y) and the
#' second column consisting of the daily precipitation sums [mm].
#' @param indT An integer vector with three entries: first the index of date in
#' X of the begin of the flood event, second the index of date in X of the end
#' of the flood event and third the index of the date in X of the peak of the
#' flood event.
#' @param min_step The minimum required number of data points used for the
#' linear regression to determine the slope. At default it is set to 3 and it
#' is recommended to use no shorter length.
#' @return A date that indicates the estimated begin of the event
#' precipitation.
#' @author Philipp Bühler, Svenja Fischer
#' @keywords ~classif ~ts
#' @examples
#' \dontrun{
#'
#' dailyprec<-data.frame(Date=seq(from=as.Date("01.01.2000", format="%d.%m.%Y"),
#' to=as.Date("30.04.2000", format="%d.%m.%Y"), by="days"),
#' discharge=rbeta(121,2,20)*100)
#' indT<-c(15,30,14+which.max(dailyprec[15:30,2]))
#'
#' PreconeCP(X=dailyprec,indT)
#' }
#'
#' @export PreconeCP
PreconeCP<-function(X,indT,min_step=3){
  X[,2]<-cumsum(X[,2])
  X[,1]<-X[,1]
  s1=numeric()
  s2=numeric()
  for (j in (min_step:(indT[1]-1))){
    sample1=X[1:j,2]
    sample2=X[j:indT[3],2]
    s1[j]=summary(lm(sample1~seq_along(sample1)))$coefficients[2]
    s2[j]=summary(lm(sample2~seq_along(sample2)))$coefficients[2]
    xwm=which.max(s2-s1)

  }

  P_M1=data.frame(x=X[c((xwm+1)),1])

  return(P_M1)
}


  #M2 Test3dim






#' Determine the begin of event precipitation
#'
#' The cumulative precipitation is used to estimate the begin of the event
#' precipitation using a change point estimation. For this, the time series is
#' divided into three subsamples and the slope is estimated for each subsample
#' using linear regression and least squares estimation. This is done for all
#' possible combinations of three consecutive subsamples, such that the change
#' point can be estimated as the point where the maximum difference between the
#' slopes of the first two subsamples occurs.
#'
#' Since pre-floods and pre-event precipitation often causes falsified starting
#' point, the estimated begin with the one-change-point-method (function
#' PreconeCP) is extended with a begin estimated by a twofold change-point
#' consideration. For this, the precipitation time series is splitted into
#' three sub-sequences. The first point where the cumulative precipitation is
#' splitted has to be located before the flood peak and the second point after
#' the peak. Then the points, for which the differences between the slopes of
#' the first and the second as well as the second and the third part are
#' maximal, are chosen as change points. The begin of the event precipitation
#' then is defined as the first of these two change points.
#'
#' @param X A dataframe with the first column equal to the date (d.m.Y) and the
#' second column consisting of the daily precipitation sums [mm].
#' @param indT An integer vector with three entries: first the index of date in
#' X of the begin of the flood event, second the index of date in X of the end
#' of the flood event and third the index of the date in X of the peak of the
#' flood event.
#' @param s_p Integer that gives the number of days after the peak that are
#' considered in the estimation of the slope, by default set to 4 days.
#' @param min_step The minimum required number of data points used for the
#' linear regression to determine the slope. By default it is set to 3 days and
#' it is recommended to use no shorter length.
#' @return A date that indicates the estimated begin of the event
#' precipitation.
#' @author Philipp Bühler
#' @keywords ~classif ~ts
#' @examples
#' \dontrun{
#'
#' dailyprec<-data.frame(Date=seq(from=as.Date("01.01.2000", format="%d.%m.%Y"),
#' to=as.Date("30.04.2000", format="%d.%m.%Y"), by="days"),
#' discharge=rbeta(121,2,20)*100)
#' indT<-c(15,30,14+which.max(dailyprec[15:30,2]))
#'
#' PrectwoCP(X=dailyprec,indT)
#' }
#'
#' @export PrectwoCP
PrectwoCP<-function(X,indT,s_p=4,min_step=3){
  X[,2]<-cumsum(X[,2])
  X[,1]<-as.Date(X[,1], format="%d.%m.%Y")
  Mat_3=matrix(data=NA,nrow=length(1:(indT[3]+s_p)),ncol=length(1:(indT[3]+s_p)))
  s1=numeric()
  s2=numeric()
  s3=numeric()

  #  for (j in (min_step:(indT[3]+s_p-(2*min_step)))){
  for (j in (min_step:(indT[1]-1))){
    for (k in ((j+(min_step-1)):(indT[3]+s_p-(min_step)))){
      sample1=X[1:j,2]
      sample2=X[j:k,2]
      sample3=X[k:(indT[3]+s_p),2]
      s1=summary(lm(sample1~seq_along(sample1)))$coefficients[2]
      s2=summary(lm(sample2~seq_along(sample2)))$coefficients[2]
      s3=summary(lm(sample3~seq_along(sample3)))$coefficients[2]
      #Mat_3[j,k]=abs(s2-s1)+abs(s2-s3)
      #Mat_3[j,k]=s2-s1+s2-s3
      Mat_3[j,k]=abs((s1+s3)/(2*s2))

    }
  }

  Mat_3[is.infinite(Mat_3)]=9999
  #Exp
  xwm=which(Mat_3 == min(Mat_3, na.rm=TRUE), arr.ind = TRUE)
  P_M2=data.frame(x=X[c((xwm[1]+1)),1])

  return(P_M2)
}
