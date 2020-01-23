# FloodR
Separation of Flood Event and Event Precipitation

## Description

Flood event separation on the basis of daily mean discharges using a variance based threshold. The monthly maximum discharges are used as peak values. To define the event separation, first an interpolation algorithm based on Thiessen polygons is used to transform station precipitation into areal precipitation. With the areal precipitation, the event precipitation for each event can be defined using a change-point based approach applied to slope of the cummulative sums of the precipitation.

## Installation
To install the package, the best method is to use the devtools-package:
- R-Studio version 3.5 is required
- install.packages(devtools)
- library(devtools
- install_github(repo = "PhilippBuehler/FloodR")
