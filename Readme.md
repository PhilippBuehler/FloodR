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
#> Warning: replacing previous import 'data.table::shift' by 'spatstat::shift' when
#> loading 'FloodR'
```

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
#>        Begin        End  Peak_date  DailyMQ     Volume dir_Volume baseflow_peak
#> 1 2000-01-31 2003-12-31 2003-11-16 37.70478 1134.00165  537.72875     6.2032774
#> 2 2000-05-21 2002-02-06 2001-12-08 36.45263  515.14155  459.46406     1.5941554
#> 3 2002-05-25 2002-10-01 2002-09-16 30.02814  106.02729   99.27757     0.9294540
#> 4 2003-03-01 2003-04-20 2003-04-18 26.75778   45.11435   35.38626     4.1959579
#> 5 2003-05-01 2003-07-03 2003-05-05 27.11696   48.59595   44.34614     0.4023404
#> 6 2003-07-25 2003-11-10 2003-10-08 29.90799   77.75010   64.84043     1.5929662
#>   baseflow_begin baseflow_end No_Peaks HQ HQ_dir Comments
#> 1      3.3564020     6.295775       81 NA     NA         
#> 2      0.3307497     1.728085       40 NA     NA overlaid
#> 3      0.1835961     1.027593        8 NA     NA overlaid
#> 4      0.1387399     4.365009        2 NA     NA overlaid
#> 5      0.3472981     1.214214        5 NA     NA overlaid
#> 6      0.8448533     1.922136        5 NA     NA overlaid
```

``` r
# Run the Web separation on the dummy Catchment
Run_WebFlood()
```

### Typing of flood events

``` r
# Open the Data
data("Sample_Flood_events")
head(Sample_Flood_events)
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

# Type the floods
# We need a Flood table with at least the following columns:
# Sum_SM: Sum of snowmelt during the floodevent in mm
# Sum_N: Sum of precipitation during the floodevent in mm
# dir_Volume: Direct volume of the flood event (Volume minus baseflow) in Mio. m³/s
# HQ_dir: Direct peak (intantanious flood peak minus baseflow) in m³/s
# PSI_SM: Runoff coefficient of the flood event WITH snowmelt+precipitation

Floods_typed <- make_typing_of_floods(Floods = Sample_Flood_events, n_G = 3, Type_3_min_samplesize = 10)

table(Floods_typed$Typ)
#> 
#> R1 R2 R3 S1 S2 
#> 30 75  9 26  9
Floods_Rain <- Floods_typed[Floods_typed$Typ %in% c("R1", "R2", "R3"),]

library(ggplot2)
ggplot(Floods_Rain)+
  geom_point(aes(x=dir_Volume, y=HQ_dir, fill=Typ), colour="black", shape=21, size=2)+
  scale_fill_manual(values = c("R1"="#D7191C", "R2"="#FDAE61", "R3"= "#1A9641"))
```

![](figure/unnamed-chunk-5-1.png)
