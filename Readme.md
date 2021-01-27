<!-- README.md is generated from README.Rmd. Please edit that file -->

# FloodR

## Description

This package provides tools for: \* Separation of flood events from
discharge timeseries \* Spatial interpolation of precipitation \*
Separation of storm events from precipitation timeseries \* Flood event
typology + Typing of rain events by TQ-Value + Typing of snow events by
clustering \* TMPS: Floodtype-based Mixture Model of Partial Duration
Series TMPS + typewise statistical estimate of return periods + typewise
statistical estimate of quantiles

## Installation and usage

``` r
devtools::install_github(repo = "PhilippBuehler/FloodR")
```

``` r
library("FloodR")
```

    ## Warning: replacing previous import 'data.table::shift' by 'spatstat::shift' when
    ## loading 'FloodR'

### Separation of flood events

For the separation of flood events from a discharge timeseries, we need
the daily discharge.

``` r
# Create a data.frame with continuous daily discharge
dailyMQ <- data.frame(Date=seq(from=as.Date("01.01.2000", format="%d.%m.%Y"),
  to=as.Date("01.01.2004", format="%d.%m.%Y"), by="days"),
  discharge=rbeta(1462,2,20)*100)

#Run the separation
Flood_events <- eventsep(dailyMQ)
# The Separation might still contain overlaid flood events which need to be corrected
head(Flood_events)
```

    ##        Begin        End  Peak_date  DailyMQ   Volume dir_Volume baseflow_peak
    ## 1 2000-01-26 2000-01-28 2000-01-27 30.60427 4.506806   2.644209     10.778915
    ## 2 2000-02-09 2000-02-13 2000-02-11 17.73265 3.492782   2.296727      3.460807
    ## 3 2000-02-26 2000-03-08 2000-03-01 22.65300 9.374362   6.435898      2.561523
    ## 4 2000-02-26 2000-02-28 2000-02-27 16.45204 1.732740   1.421456      1.801411
    ## 5 2000-02-28 2000-03-08 2000-03-01 22.65300 7.641622   5.683534      1.119164
    ## 6 2000-03-09 2000-03-16 2000-03-11 25.64213 8.060056   7.424781      1.024097
    ##   baseflow_begin baseflow_end No_Peaks HQ HQ_dir    Comments
    ## 1      8.8598209    12.698009        1 NA     NA            
    ## 2      2.9411774     3.980437        1 NA     NA            
    ## 3      1.1474012     5.036236        1 NA     NA            
    ## 4      1.1474012     2.455421        1 NA     NA  first wave
    ## 5      0.0000000     5.036236        1 NA     NA second wave
    ## 6      0.9890412     1.111738        1 NA     NA

### Correction of flood events

Run the Web separation on the dummy Catchment As input to the function a
minimum of discharge data is used. The Flood event tables can be opened
from within the User-Interface

``` r
Run_WebFlood()
```

### Separation of precipitation

After the flood events are corrected (if needed), the precipitation
belonging to flood event need to be estimated For the both functions, a
daily precipitation timeseries is needed, as well as the parameter indT,
which has the position indices of begin, end and peak of the FLOOD event
as vector.

``` r
# create a sample dataset
dailyprec <- data.frame(Date=seq(from=as.Date("01.01.2000", format="%d.%m.%Y"),
  to=as.Date("30.04.2000", format="%d.%m.%Y"), by="days"),
  discharge=rbeta(121,2,20)*100)

# Create indices of beginning, end and peak of the flood event
indT <- c(15, 30, 14+which.max(dailyprec[15:30,2])) 

# Run the separation for both methods
Date1 <- PreconeCP(dailyprec, indT = indT)
Date2 <- PrectwoCP(dailyprec, indT = indT)
print(c("Method1" = Date1, "Method2" = Date2))
```

    ##      Method1      Method2 
    ## "2000-01-15" "2000-01-15"

### Typing of flood events

For the typing of the flood event, multiple characteristics for each
flood event must be calculated before: \* Sum\_SM: Sum of snowmelt
during the floodevent in mm \* Sum\_N: Sum of precipitation during the
floodevent in mm \* dir\_Volume: Direct volume of the flood event
(Volume minus baseflow) in Mio. m³/s \* HQ\_dir: Direct peak
(instantaneous flood peak minus baseflow) in m³/s \* PSI\_SM: Runoff
coefficient of the flood event WITH snowmelt+precipitation

``` r
# Open the sample flood event data
data("Sample_Flood_events")
head(Sample_Flood_events)
```

    ##        Begin        End  Peak_date Sum_SM  Sum_N dir_Volume  HQ_dir PSI_SM
    ## 1 1950-02-10 1950-02-14 1950-02-11   0.73  14.81      18.45  146.02   0.22
    ## 2 1951-01-18 1951-01-26 1951-01-20   1.37  38.45      26.56  108.25   0.12
    ## 3 1952-09-12 1952-09-18 1952-09-14   0.09  68.45      22.05  142.18   0.06
    ## 4 1953-01-28 1953-02-11 1953-01-30  28.68  52.64     124.25  353.18   0.28
    ## 5 1953-02-19 1953-03-04 1953-02-23  17.05   6.27     113.31  209.12   0.89
    ## 6 1954-07-08 1954-07-24 1954-07-10   0.09 197.43     545.35 1513.91   0.50
    ##    TQDir SM_rel       HQ
    ## 1  35.10   0.05  171.622
    ## 2  68.15   0.03  130.075
    ## 3  43.08   0.00  167.398
    ## 4  97.72   0.35  399.498
    ## 5 150.51   0.73  241.032
    ## 6 100.06   0.00 1676.301

``` r
# Run the event typing
Floods_typed <- Flood_typology(Floods = Sample_Flood_events, n_G = 3, Type_3_min_samplesize = 10)

table(Floods_typed$Type)
```

    ## 
    ## R1 R2 R3 S1 S2 
    ## 30 74 10 26  9

``` r
# Plot the event typing
Floods_Rain <- Floods_typed[Floods_typed$Type %in% c("R1", "R2", "R3"),]

suppressMessages(library(ggplot2))
ggplot(Floods_Rain)+
  geom_point(aes(x=dir_Volume, y=HQ_dir, fill=Type), colour="black", shape=21, size=2)+
  scale_fill_manual(values = c("R1"="#D7191C", "R2"="#FDAE61", "R3"= "#1A9641"))
```

![](Readme_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

### Typewise and combined return period estimation with the TMPS model

For the TMPS model, multiple characteristics for each flood event must
be present in the input Floods\_typed: \* Peak\_date: Date of the peak
of the flood event \* HQ: Direct peak in m³/s \* Type: Flood type of the
flood event, preferably as factor

Also, a Discharge timeseries with a “Date” and “Discharge” is needed

``` r
# Open the discharge data
data("Discharge")
head(Discharge)
```

    ##          Date Discharge
    ## 1: 1950-01-01  144.2951
    ## 2: 1950-01-02  140.8438
    ## 3: 1950-01-03  143.0167
    ## 4: 1950-01-04  151.4559
    ## 5: 1950-01-05  145.1900
    ## 6: 1950-01-06  141.6746

``` r
Floods_typed <- Floods_typed[, c("Peak_date","HQ", "Type")]

TMPS_quantiles <- qTMPS(p = c(2, 5, 10, 20, 25, 50, 100, 200, 500, 1000), 
  Flood_events = Floods_typed, Daily_discharge = Discharge, 
  return_TMPS = c("TMPS", "R1", "R2", "R3", "S1", "S2"))

TMPS_quantiles
```

    ##                 2          5         10          20        25        50
    ## TMPS    549.47952   708.2928   897.8590 1259.595304 1397.7360 1911.5217
    ## R1       67.97982   236.4081   403.3624  623.690372  709.2526 1033.0840
    ## R2      162.70548   402.2311   616.2980  876.384545  972.2912 1316.1713
    ## R3   -20331.06676 -3982.4597 -1055.8595   -5.890942  165.1651  457.6625
    ## S1      382.76268   472.8958   518.1573  553.210856  562.8823  588.9411
    ## S2    -2367.98158  -239.8321   286.4022  525.593018  571.6013  661.6797
    ##            100       200       500      1000
    ## TMPS 2586.8131 3479.0707 5122.3917 6853.7263
    ## R1   1468.9022 2056.6692 3160.7198 4341.5197
    ## R2   1743.8436 2277.0708 3190.4865 4083.7582
    ## R3    571.6516  616.4844  637.1909  642.3974
    ## S1    610.0375  627.2005  645.1311  655.8382
    ## S2    705.6182  727.2192  739.9945  744.1972

``` r
# Calculate the AMS with TWO SIMPLIFICATIONS FOR EASYNESS : 
# 1) no hydrological years 
# 2) asummption instantaneous peaks == annual max of daily discharge * 1.1

AMS <- aggregate((Discharge$Discharge*1.1), list(as.numeric(format(Discharge$Date,"%Y"))), max, na.rm=TRUE)$x

suppressMessages(library(fExtremes))
suppressMessages(library(reshape2))
suppressMessages(library(ggplot2))

AMS_params <- gevFit(AMS, type="pwm")@fit$par.ests  
AMS <- qgev(1-1/c(2,5,10,20,25,50,100,200,500,1000), mu=AMS_params[2], xi=AMS_params[1], beta=AMS_params[3])

Results <- as.data.frame(rbind(TMPS_quantiles, AMS=AMS))
Results[Results < 0] <- NA
Results <- cbind(Results, "Method"=rownames(Results))
Results_melt <- melt(Results, id.vars = "Method", variable.name = "Annuality")
Results_melt$Annuality <- as.numeric(as.character(Results_melt$Annuality))

cols <- c("AMS"="black",c(R1 = "#D7191C", R2 = "#FDAE61", R3 = "#1A9641", S1 = "#6BAED6", S2 = "#2171B5"),  "TMPS"="darkorchid1")

ggplot(Results_melt)+ 
  theme_bw()+
  geom_line(aes(x=Annuality, y=value, colour = Method ), size=1.5)+ 
  scale_color_manual(values=cols)+
  scale_x_log10()+
  labs(x="T[a]",y="Peak discharge [m³/s]")+
  annotation_logticks(sides = "b")
```

    ## Warning: Removed 6 row(s) containing missing values (geom_path).

![](Readme_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
# Calculate 
TMPS_probs_from_AMS_quants <- pTMPS(q = AMS, 
  Flood_events = Floods_typed, Daily_discharge = Discharge, 
  return_TMPS = c("TMPS", "R1", "R2", "R3", "S1", "S2"))
TMPS_probs_from_AMS_quants
```

    ##      410.243366436869 588.700179151269 736.69352773846 906.138726802705
    ## TMPS         1.822065         2.416035        6.299805         10.18202
    ## R1          10.252145        18.139515       26.741355         39.02185
    ## R2           5.143333         9.208549       14.028988         21.46882
    ## R3          42.321812       121.531047             Inf              Inf
    ## S1           2.504886        49.644531             Inf              Inf
    ## S2          13.421183        27.587468      359.750085              Inf
    ##      966.324434322802 1173.97154113023 1417.99019205185 1705.56456617459
    ## TMPS         11.56833         17.23996         25.79147         38.71494
    ## R1           44.03284         64.03584         93.13863        135.59150
    ## R2           24.66608         38.21834         59.82133         94.53598
    ## R3                Inf              Inf              Inf              Inf
    ## S1                Inf              Inf              Inf              Inf
    ## S2                Inf              Inf              Inf              Inf
    ##      2166.75357125051 2589.61044207795
    ## TMPS         66.48178         100.2510
    ## R1          223.14952         325.7015
    ## R2          175.38196         282.3063
    ## R3                Inf              Inf
    ## S1                Inf              Inf
    ## S2                Inf              Inf
