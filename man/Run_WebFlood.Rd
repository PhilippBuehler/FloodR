\name{Run_WebFlood}
\alias{Run_WebFlood}
\title{Graphical web interface for browsing trough floodevents in daily resolution and correcting them}
\description{
This shiny app opens a graphical web interface, in which floodevents are shown with discharge and Precipitation. With the buttons you can browse trough all the floodevents supplemented in the flood file. The floods can be changed (begin and end) and the current properties are calculated. (see Details)

}
\usage{
Run_WebFlood(Discharge,Precipitation=NULL,Catchment_Properties=NULL,language="en")
}

\arguments{

\item{Discharge}{
A list of dataframes with the discharge data. Each dataframe must contain the Dischare data for a specific catchment with two columns:

Column 1: Date in a continious daily timeseries (R "date" format)

Column 2: Meassured Data of Discharge

The list entries need to be named after the catchment-identifier (eg. name)
}

\item{Precipitation}{
[OPTIONAL]:
A list of dataframes with the precipitation data. Each dataframe must contain the precipitation data for a specific catchment with two columns:

Column 1: Date in a continious daily timeseries (R "date" format)

Column 2: Meassured Data of Precipitation

The list entries need to be named after the catchment-identifier (eg. name)

}

\item{Catchment_Properties}{
[OPTIONAL]:
A dataframe with the properties for each catchment you want to use. This dataframe is only used to show information about the catchment.
The dataframe must contain the following columns:

Column "Name": Unique identifier (eg. Catchment Name) connecting the columns to the names of the list of discharge and precipitation.

optional: Column "Area": Area size of the Catchments


}
\item{language}{
The language for the graphical interface.
Default is English ("en"), the other option is German: ("de").

Important: The table headers don't change and use english naming.
}
}

\details{
This shiny app opens a graphical web interface, in which floodevents are shown with discharge and Precipitation. With the buttons you can browse trough all the floodevents supplemented in the flood file. The floods can be changed (begin and end) and the current properties are calculated. (see Details)
The plotting output can be changed with a buffer.
Uppon opening the interface, you are promted to open a ".csv"-file (";"-separator) which contains the flood information (begin,end and peak date). The naming convention of the file is "CATCHMENTNAME_your_comments.csv".
It is important to have the exact catchment name before the underscore. After the underscore, anything can be written, followed by the .csv extension.
The file needs the following columns:

Column "Begin": The date of the begin of the floodevent as sting (either in "%d.%m.%Y"or in "%Y-%m-%d")

Column "End": The date of the end of the floodevent as sting (either in "%d.%m.%Y"or in "%Y-%m-%d")

Column "Peak_date": The date of the peak of the floodevent as sting (either in "%d.%m.%Y"or in "%Y-%m-%d")

optional:

Column "HQ": A value for the "true" peak of the flood, as if it was meassured in continious small timesteps. If the value is given, the direct HQ and the TQ value are computed
}

\author{
Philipp Buehler
}
\examples{
Discharge <-list(A=data.frame(Date=seq(from=as.Date("01.01.2000", format="\%d.\%m.\%Y"),
                               to=as.Date("01.01.2004", format="\%d.\%m.\%Y"), by="days"),
                      discharge=rbeta(1462,2,20)*100),
                 B=data.frame(Date=seq(from=as.Date("01.01.2000", format="\%d.\%m.\%Y"),
                                       to=as.Date("01.01.2004", format="\%d.\%m.\%Y"), by="days"),
                              discharge=rbeta(1462,2,20)*100))

Precipitation <-list(A=data.frame(Date=seq(from=as.Date("01.01.2000", format="\%d.\%m.\%Y"),
                                       to=as.Date("01.01.2004", format="\%d.\%m.\%Y"), by="days"),
                                  prec=rbeta(1462,2,20)*100),
                 B=data.frame(Date=seq(from=as.Date("01.01.2000", format="\%d.\%m.\%Y"),
                                       to=as.Date("01.01.2004", format="\%d.\%m.\%Y"), by="days"),
                              prec=rbeta(1462,2,20)*100))


Catchment_Properties <-data.frame(Name=c("A","B"),Area=c(10,100),
Height=c(100,1000),stringsAsFactors = FALSE)
  language<-"en"

#Run_WebFlood(Discharge,Precipitation,Catchment_Properties)
#Use the provided file "A_Floodevents_example.csv" to test

}

\keyword{ ~ts }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~classif }% __ONLY ONE__ keyword per line
