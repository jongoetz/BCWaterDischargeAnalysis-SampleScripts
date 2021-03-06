# BCWaterDischargeAnalysis-SampleScripts


This package was developed to help analyze stream discharge data. 

This package was written for the B.C. Ministry of Environment
by Carl James Schwarz (cschwarz.stat.sfu.ca@gmail.com).


## Sample Scripts
Sample scripts show how to use the BCWaterDischargeAnalysis package.

Directory | Script
----------|-----------
08HA0811   | Basic analysis of water discharge data from station 08HA0811


## Installing the package.

To install the latest version of BCWaterDischargeAnalysis:


```{r, eval = FALSE}
library(devtools)   # has the install_github() function
# This will take about 5 minutes to install
install_github("bcgov/BCWaterDischargeAnalysis", build_vignettes = TRUE) 
# Using `build_vignettes = TRUE` will slow down the install, but is necessary if 
# you want to read the vignette, which is recommended
library(BCWaterDischargeAnalysis)
```

## Vignettes

You can see the vignette names using
```{r, eval=FALSE}
vignette(package="BCWaterDischargeAnalysis")
```

You can read the vignette using:
```{r, eval=FALSE}
vignette("example-analysis", package="BCWaterDischargeAnalysis")
```


## Help on functions
Use the help function of R in the usual way.
```{r}
help(package="BCWaterDischargeAnalysis") # list of all functions in package
help(compute.Q.stat.annual)  # help on individual function
```

## Data
Create a data frame (e.g. __df__) with two varibles:

Variable | Description
---------|-------------
Date   | The data of the reading in standard R date class format. For example to convert a 4-digit year, numeric month, and numeric day of the month use __df$Date  <- as.Date{paste(df$Year,'-',df$Month,'-',df$Day,sep="")}__.
Q  | The average daily flow as a numeric value.

Other variables in the data frame will be ignored.

Missing values can be indicated by leaving out dates with missing
discharge values, or by setting the value of __Q__ to missing (NA).

The treatment of missing values  is explained in more
detail in the vignette.




