\name{eventsep}
\alias{eventsep}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Flood event separation based on variance
}
\description{
Based on daily mean discharges, a window-variance is calculated. If this variance exceeds a certain threshold, a flood event has been detected. To determine the begin of the flood event, the lag 1 differences are considered. The point, where the differences are positive for the last time before the variance exceeds the threshold, is defined as the starting point (including natural variability). The end of the flood event is defined as the point where the sum of the rising limb of the event equals approximately the sum of the falling limb.
}
\usage{
eventsep(dailyMQ,monthlyHQ,dvar,theta, ddur,Kappa,eta,delta usemed, medbf=0.5, NA_mode = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dailyMQ}{
  A dataframe where the date (d.m.Y) is given in the first and the daily mean discharges are given in the second column.
}
\item{monthlyHQ}{
  A dataframe where the date (d.m.Y) is given in the first and the monthly maximum discharge peaks are given in the second column.
}
\item{dvar}{
  integer: The window length of the moving variance in days. By default, it is set to three days.
}
\item{theta}{
  numeric: The proportion of the sample variance of the dvar-day-window variance on the threshold. By default it is set to theta=0.25.
}
\item{ddur}{
  integer: The approximate minimum length of an overlaid event in days. By default it is set to ddur=40.
}
\item{Kappa}{
  numeric: Threshold parameter for defining the begin of a flood event. By default it is set to Kappa=0.4.
}
\item{eta}{
  numeric: Threshold parameter for defining the begin of a flood event. By default it is set to eta=0.1.
}
\item{delta}{
  numeric: Threshold parameter for defining the end of a flood event. By default it is set to delta=0.2.
}
\item{usemed}{
  logical: Should the median of relative difference between the baseflow at the end and the begin be considered to determine the end of the flood event.
}
\item{medbf}{
  numeric: The median of relative difference between the baseflow at the end and the begin of all separated flood events. Used to define the end of the event, if usemed=TRUE.
}
\item{NA_mode}{
  integer: If the timeseries does contain any NA values, NA_mode can be used to interpolate all gaps of continious NA-values by linear interpolation. Only gaps  smaller or equal to NA_mode (>=0) will be interpolated. 
  Additionally, if the timeseries contains larger gaps, the timeseries is splited into subsamples with each being seperated intependend. The resulting separation table will still be append together.
}


}
\details{
To characterize the flood regime, it is necessary to estimate single events and to separate the amount of direct runoff from the total volume. For this purposes, a procedure is developed, which is based on the following three assumptions:

•	A flood event is an exceedance of the normal discharge within a time span. For every flood event beginning and end has to be specified.

•	A flood event is characterized by significantly increased discharge dynamics. Especially around the flood peaks, the  sample variance of daily discharges within a moving time window will be higher than for other time periods with the same length.

•	For all flood events, the volume of the rising limb of the hydrograph has to be equal the volume of the falling limb.

An assumed increase of base flow during the event has to be considered.
The automatic event separation is based on the Lag one (1 day) differences of the daily mean discharges \eqn{QD_i= Q_i-Q_{i-1}} , with \eqn{QD_1=0}, and the sample variance  of daily discharges within a moving window  of dvar days:
\deqn{Var_{d_{var}}(i)=Var(Q_{i-(d_{var}-1)},\ldots,Q_i)}
\deqn{Var_{d_{var}}(j)=0, for j=1,…,dvar.}
The appropriated window length depends on the reaction time of the watershed and in this way from the catchment size. In our studies we modified it between 3 and 7 days, but 3 days were appropriated for nearly all catchments.  To indicate the time span of the flood event, the sample variance has to exceed a certain threshold. We defined this threshold with the mean plus \eqn{0.25 \sigma} variance of all sample variances which could be derived within the long discharge series: \deqn{T{{H}_{var}}=\overline{{{V}_{{{d}_{var}}}}}+0.25\cdot \sqrt{Var({{V}_{{{d}_{var}}}})}.}
The peak is located within the time span, where the sample variance exceeds this threshold.  The starting point of the flood event was defined as the time step in front of the peak for which the lag one-difference  QD is negative for the last time: \deqn{t_{start}=\max\lbrace 1\leq i < t_{peak} | QD_i <0 \rbrace.}
From this point on, discharges increase until the peak is reached.  However, this increase does not always indicate the true start of the flood event. Instead, the natural variability in discharge can lead to an increase. To consider this, a temporal  decrease between two discharges in succession of 10 percent was accepted. In such cases, the starting point was shifted further in the future (\eqn{Q_{t_{start}}= Q_{t_{start}+1}}) until the following condition was fulfilled:  \deqn{|({{Q}_{{{t}_{s}}_{tart}}}-{{Q}_{{{t}_{start}}+1}})/{{Q}_{{{t}_{start}}+1}}|<0.1.}
The rising limb of a flood event is not steady,  often pre-floods occured. We handled this by setting \eqn{t_{start}=t_{start}-2}, if  \eqn{(Q_{t_{start}-1}-Q_{t_{start}-2})> 0.4 (Q_{t_{peak}}-Q_{t_{start}})}. This condition ensures that the rising limb is included in total in the flood event. For the case where the rising limb of the two days before the peak has been larger than \eqn{40\%} of the actual limb we included these days in the flood event since they define a pre-flood.
To define the end of the flood event we needed to find the time for which the differences between the volumes of the rising limb (SI) and the falling limb is approximately zero. Since also a possible increase of the baseflow during the flood event has to be considered, we define the end by
\deqn{{{t}_{end}}= \min \left\{ {{t}_{peak}}+1\le t\le n|\left( {{\beta }_{B;k}}\le median({{\beta }_{B}}) \right)\vee \left( \left( \sum\limits_{i={{t}_{peak}}+1}^{t}{Q}{{D}_{i}}\le {{Q}_{{{t}_{start}}}} \right)\wedge \\\left( 0.2\left( SI-\sum\limits_{i={{t}_{peak}}+1}^{t}{Q}{{D}_{i}} \right)\ge (Q{{D}_{t+1}}+Q{{D}_{t+2}}) \right) \right) \right\}, }
where \eqn{\beta_{B;k}} is the slope of the baseflow of the event. This includes two criteria: the volume of the falling limb is smaller than the baseflow at the beginning of the flood and the following two days the difference between the volume of the rising limb and the falling limb is not reduced by more than \eqn{20\%}. Alternatively, also the slope of the baseflow between start and end of the hydrograph is smaller than the median of the slope of the baseflow for all other flood events of the considered gauge.
The number of peaks of the flood event then can be estimated by the number of time periods of exceedance of the variance threshold.

A special case that cannot be addresses directly by the procedure described above are flood events with multiple peaks. Here, we want to divide into two main cases: the superposition of two flood events and superimposed floods. In the first case, a second flood event begins before the recession of the first event has ended. This leads to an enhancement of the second event. The second case corresponds to a time period of an already increased baseflow, e.g. due to snowmelt. If flood events occur during this period, they are superimposed on this increased baseflow. Of course, not every flood event with multiple peaks belong to either of this cases.
We define that a flood event consists potentially of superpositioned events of the originally separated event has a duration of less than 40 days. This parameter can be chosen according to the catchment size. Since the Mulde basin only consist of mesoscale catchments, this threshold is sufficient for our purpose. We then apply the independence criterion by Klein (2009) to validate if there are independent peaks within this event, i.e. two peaks
-	That deviate by less than five times the smaller peak
-	For which the larger peak is more than 2.5 times as large as the smallest discharge between both peaks
-	For which \eqn{70\%} of the smaller peak are larger than the smalles discharge between two peaks.

This criterion ensures that both peak are large enough to be separate flood event and that the recession of the first event is large enough to state them as independent. For all peaks that fulfill this criterion we choose the two peaks with the largest recession in between. According to this, two sub-events are defined, the first one with starting point equal to the original event begin. The end of this event is estimated by the lowest discharge between both peaks to which the recession of the original event is added (that are all discharge values smaller than the values of the valley) to reconstruct the overlaid recession of the first event. The second events begins with the smallest discharge value between both peaks and end with the original ending.
Superimposed events can occur if the originally separated event has a duration larger than 40 days. Again, all peaks are checked for independence and are separated again. Since the separation of multiple peak events, especially of superimposed events is rather difficult, a manual check afterwards is recommended.



}
\value{
A dataframe is returned with the following columns, where each row is a flood event:

\item{Begin}{date (d.m.Y) of the begin of the flood event}
\item{End}{date (d.m.Y) of the end of the flood event}
\item{Peak_date}{date (d.m.Y) of the maximum daily discharge during the flood event}
\item{DailyMQ}{maximum daily mean discharge [m³/s] during the flood event}
\item{Volume}{volume [Mio. m³] of the flood event calculated by the sum of all daily mean discharges during the flood event}
\item{dir_Volume}{direct volume [Mio. m³] calculated by the difference of the volume and the baseflow. The baseflow is estimated by a straight line between the discharge at the beginning and the end of the flood event.}
\item{baseflow_peak}{baseflow [m³/s] at the day of the flood peak, calculated by the straight-line method.}
\item{baseflow_begin}{baseflow [m³/s] at the beginning of the flood event, equal to the daily mean discharge of the beginning of the flood event.}
\item{baseflow_end}{baseflow [m³/s] at the end of the flood event, equal to the daily mean discharge of the end of the flood event.}
\item{No_peaks}{number of peaks during the flood event}
\item{HQ}{peak discharge [m³/s] of the flood event taken from the monthly maximum discharges. If no monthly maximum discharge is available for the flood event, it is set to NA.}
\item{HQ_dir}{direct peak discharge, claculated by the difference of HQ and baseflow_peak}
\item{comments}{a short note if the event is overlaid, or first respectively second part of a double-peaked flood event.}


}
\references{
Klein, B. (2009): Ermittlung von Ganglinien für die risikoorientierte Hochwasserbemessung von Talsperren. Schriftenreihe des Lehrstuhls Hydrologie, Wasserwirtschaft und Umwelttechnik, Ruhr-University Bochum.
}
\author{
Svenja Fischer
}
\note{
Important note: If a double-peaked or an overlaid event occurs, the whole event is listed in the output first, followed by the splitted partial events.
}

\examples{
dailyMQ<-data.frame(Date=seq(from=as.Date("01.01.2000", format="\%d.\%m.\%Y"),
to=as.Date("01.01.2004", format="\%d.\%m.\%Y"), by="days"),
discharge=rbeta(1462,2,20)*100)

monthlyHQ<-data.frame(Date=seq(from=as.Date("01.01.2000", format="\%d.\%m.\%Y"),
to=as.Date("01.01.2004", format="\%d.\%m.\%Y"), by="months"),
discharge=dailyMQ$discharge[(0:48)*12+1]+rnorm(49,5,1))

eventsep(dailyMQ, monthlyHQ)
}

\keyword{ ~ts }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~classif }% __ONLY ONE__ keyword per line
