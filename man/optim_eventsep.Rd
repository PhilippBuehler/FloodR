\name{optim_eventsep}
\alias{optim_eventsep}
\title{
  Optimize the parameters of eventsep 
}
\description{
  Optimize the parameters of eventsep by maximising flood events during high quantiles and minimizing flood events during low quantiles
}
\usage{
  optim_eventsep(params, Discharge, upper_TH=0.95, lower_TH=0.5, lower_tolerance=0.01, NA_mode=NULL)
}
\arguments{
  \item{params}{
Vector: Named vector of the stating parameters and their name, e.g. c("gamma"=1, "eta"=0.1)
}
\item{Discharge}{
Data.frame: Discharge timeseries with Date amd Discharge column
}

}
\author{
  Philipp Bühler
}
