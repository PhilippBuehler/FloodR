\name{PreconeCP}
\alias{PreconeCP}

\title{
Determine the begin of event precipitation
}
\description{
The cumulative precipitation is used to estimate the begin of the event precipitation using a change point estimation. For this, the time series is divided into two subsamples and the slope is estimated for each subsample using linear regression and least squares estimation. This is done for all possible combinations of two consecutive subsamples, such that the change point can be estimated as the point where the maximum difference between the slopes of the two subsamples occurs.
}
\usage{
PreconeCP(X,indT,min_step=3)
}
\arguments{
  \item{X}{
A dataframe with the first column equal to the date (d.m.Y) and the second column consisting of the daily precipitation sums [mm].
}
\item{indT}{
An integer vector with three entries: first the index of date in X of the begin of the flood event, second the index of date in X of the end of the flood event and third the index of the date in X of the peak of the flood event.
}
\item{min_step}{
The minimum required number of data points used for the linear regression to determine the slope. At default it is set to 3 and it is recommended to use no shorter length.
}
}
\details{
We want to estimated the event precipitation by comparing the rising limb of the discharge with the rising limb of precipitation. Here, we assumed that the begin of the increase of the hydrograph indicates the begin of the event precipitation, whereas the end of the flood event automatically defines the end of the event precipitation. Due to catchment reaction times, a possible delay of the begin of event precipitation and the begin of the flood event has to be considered.
For this purpose a buffer time period b between the begin of the precipitation and the flood event was used. Here, b was defined as \eqn{b=\max (rt,7)}, where rt is the duration of the rising limb of the flood event.
Then the begin of event precipitation \eqn{hat{k}} was defined by the change-point of the slope of the cumulative precipitation sum in this period:
\deqn{\hat{k}=\arg {{\max }_{{{t}_{start}}-b\le k\le {{t}_{peak}}+b}}\{{{\beta }_{k:{{t}_{peak}}+b}}-{{\beta }_{{{t}_{start}}-b:k}}\},}
where  \eqn{\beta_{i:j}} is the slope of the linear regression for the sum of all precipitation values \eqn{P_i,\ldots,P_j} derived by least-squares method. This means, we divided the cumulative sums of the precipitation beginning b days before the begin of the flood event and ending b days after the day of the flood peak into two parts and used the time step where the difference of the slope of these two parts was maximal to define the begin of the precipitation. Each part had to consist of at least three days.
 The end of the event precipitation was defined equally to one day before the end of flood event.


}
\value{
A date that indicates the estimated begin of the event precipitation.
}

\author{
Philipp Bühler, Svenja Fischer
}
\examples{
dailyprec<-data.frame(Date=seq(from=as.Date("01.01.2000", format="\%d.\%m.\%Y"),
to=as.Date("30.04.2000", format="\%d.\%m.\%Y"), by="days"),
discharge=rbeta(121,2,20)*100)
indT<-c(15,30,14+which.max(dailyprec[15:30,2]))

PreconeCP(X=dailyprec,indT)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~ts }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~classif }% __ONLY ONE__ keyword per line
