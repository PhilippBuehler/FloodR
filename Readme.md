<!-- README.md is generated from README.Rmd. Please edit that file -->

# FloodR Package

## Description

Flood event separation on the basis of daily and hourly mean discharges
using a variance based threshold. The monthly maximum discharges are
used as peak values. To define the event separation, first an
interpolation algorithm based on Thiessen polygons is used to transform
station precipitation into areal precipitation. With the areal
precipitation, the event precipitation for each event can be defined
using a change-point based approach applied to slope of the cummulative
sums of the precipitation.

## Installation

``` r
devtools::install_github(repo = "PhilippBuehler/FloodR")
```

## Usage

``` r
library("FloodR")
```

    #> Warning: replacing previous import 'data.table::shift' by 'spatstat::shift' when
    #> loading 'FloodR'

### separation of Flood events

``` r
dailyMQ <- data.frame(Date=seq(from=as.Date("01.01.2000", format="%d.%m.%Y"),
  to=as.Date("01.01.2004", format="%d.%m.%Y"), by="days"),
  discharge=rbeta(1462,2,20)*100)

monthlyHQ <- data.frame(Date=seq(from=as.Date("01.01.2000", format="%d.%m.%Y"),
  to=as.Date("01.01.2004", format="%d.%m.%Y"), by="months"),
  discharge=dailyMQ$discharge[(0:48)*12+1]+rnorm(49,5,1))

Flood_events <- eventsep(dailyMQ, monthlyHQ)
head(Flood_events)
```

    #>        Begin        End  Peak_date  DailyMQ   Volume dir_Volume baseflow_peak
    #> 1 2000-01-13 2000-01-15 2000-01-14 23.78706 3.454390   2.055202      8.097152
    #> 2 2000-01-16 2000-01-19 2000-01-18 19.79175 4.736676   2.348734     10.667915
    #> 3 2000-02-03 2000-02-05 2000-02-04 24.10499 4.687610   2.082671     15.074873
    #> 4 2000-02-22 2000-02-24 2000-02-23 17.98488 2.746100   1.553894      6.899343
    #> 5 2000-02-27 2000-03-02 2000-02-29 10.20679 2.984117   1.653268      3.850837
    #> 6 2000-03-03 2000-03-05 2000-03-04 16.39870 1.972322   1.416848      3.214550
    #>   baseflow_begin baseflow_end No_Peaks      HQ   HQ_dir Comments
    #> 1       6.806688     9.387616        1      NA       NA         
    #> 2       4.847215    13.578266        1      NA       NA         
    #> 3       6.689083    23.460663        1      NA       NA         
    #> 4       4.996846     8.801839        1      NA       NA         
    #> 5       3.096781     4.604892        1 16.2416 12.39076         
    #> 6       2.202032     4.227068        1      NA       NA

``` r
# Run the Web separation on the dummy Catchment
Run_WebFlood()
```

### Typing of flood events

``` r
# Open the Data
data("Sample_Flood_events")
head(Sample_Flood_events)
```

    #>        Begin        End  Peak_date Sum_SM  Sum_N dir_Volume  HQ_dir PSI_SM
    #> 1 1950-02-10 1950-02-14 1950-02-11   0.73  14.81      18.45  146.02   0.22
    #> 2 1951-01-18 1951-01-26 1951-01-20   1.37  38.45      26.56  108.25   0.12
    #> 3 1952-09-12 1952-09-18 1952-09-14   0.09  68.45      22.05  142.18   0.06
    #> 4 1953-01-28 1953-02-11 1953-01-30  28.68  52.64     124.25  353.18   0.28
    #> 5 1953-02-19 1953-03-04 1953-02-23  17.05   6.27     113.31  209.12   0.89
    #> 6 1954-07-08 1954-07-24 1954-07-10   0.09 197.43     545.35 1513.91   0.50
    #>    TQDir SM_rel
    #> 1  35.10   0.05
    #> 2  68.15   0.03
    #> 3  43.08   0.00
    #> 4  97.72   0.35
    #> 5 150.51   0.73
    #> 6 100.06   0.00

``` r
# Type the floods
# We need a Flood table with at least the following columns:
# Sum_SM: Sum of snowmelt during the floodevent in mm
# Sum_N: Sum of precipitation during the floodevent in mm
# dir_Volume: Direct volume of the flood event (Volume minus baseflow) in Mio. m³/s
# HQ_dir: Direct peak (intantanious flood peak minus baseflow) in m³/s
# PSI_SM: Runoff coefficient of the flood event WITH snowmelt+precipitation

Floods_typed <- make_typing_of_floods(Floods = Sample_Flood_events, n_G = 3, Type_3_min_samplesize = 10)

table(Floods_typed$Typ)
```

    #> 
    #> R1 R2 R3 S1 S2 
    #> 30 75  9 26  9

``` r
Floods_Rain <- Floods_typed[Floods_typed$Typ %in% c("R1", "R2", "R3"),]

library(ggplot2)
ggplot(Floods_Rain)+
  geom_point(aes(x=dir_Volume, y=HQ_dir, fill=Typ), colour="black", shape=21, size=2)+
  scale_fill_manual(values = c("R1"="#D7191C", "R2"="#FDAE61", "R3"= "#1A9641"))
```

![](figure/unnamed-chunk-5-1.png)
