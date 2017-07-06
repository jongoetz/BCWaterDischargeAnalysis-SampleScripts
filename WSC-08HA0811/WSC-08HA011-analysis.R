# Copyright 2017 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

# Sample script for the analysis of Station WSC-08HA011


library(ggplot2)    # for plots
library(plyr)       # for summary reports
library(reshape2)   # for melting

# load the functions used in the analysis
# This will also load the relevant libraries used by the package
library(BCWaterDischargeAnalysis)


options(width=200)  # how wide to print output


# Define variables that provide options for the analysis

Station.Code <- 'WSC-08HA011'
Station.Name <- 'Cowichan River near Duncan'
Station.Area <- 826    # square km's

start.year   <- 1965  # when do you want the analysis to start at.
end.year     <- 2012  # what is last year with complete data




# Get the data
# We will create a data frame (flow) with two varibles:
#
#  Variable | Description
#  ---------|-------------
#  Date     | The data of the reading in standard R date class format.
#           | For example to convert a 4-digit year, numeric month, and numeric day
#           | of the month use
#           |    flow$Date  <- as.Date{paste(flow$Year,'-',flow$Month,'-',flow$Day,sep="").
#           | To convert a character string (e.g. '1/15/2001') use the
#           |    flow$Date <- as.Date(flow$chardate, "%d/%m/%Y") .
#           | The formatting codes (%Y etc) are explained in the help for the strptime() function.
#
# Q         | The average daily flow as a numeric value.

# Other variables in the data frame will be ignored.

# Missing values can be indicated by leaving out dates with missing
# discharge values, or by setting the value of __Q__ to missing (NA).

flow <- read.csv(file.path("WSC-08HA011_STREAMFLOW_SUMMARY.csv"), header=TRUE, as.is=TRUE, strip.white=TRUE)

# Make sure that the Q is numeric
flow$Q <- as.numeric(flow$Q)

# Create a date variable
flow$Date <- as.Date(paste(flow$Year,flow$Month, flow$Day, sep="-"), "%Y-%m-%d")

# basic structure of flow
str(flow)

# Create the directory for the results of the analysis
# If you are comparing to the spreadsheet, the spreadsheet must also be in this directory.
# Similarly, if you are comparing to the HEC-SSP results, the vfa.rpt file must also be in this directory

report.dir <- 'AnalysisOutput'  # current director
dir.create(report.dir)
cat("Reports and saved files will be found in ", report.dir, "\n")

#----------------------------------------------------------------
# Some preliminary screening

# Are there any illegal dates that could not be created?
cat("Number of illegal date values ", sum(is.na(flow$Date)), "\n")
flow[ is.na(flow$Date),]     # sorry Microsoft, but feb 29 1900 is NOT a leap year

dim(flow)  # size before removing illegal dates
flow <- flow[ !is.na(flow$Date),]
dim(flow)  # size after removing illegal dates

flow$Year <- as.numeric(format(flow$Date, "%Y"))
# Table of simple statistics by year to help check for outliers, etc
flow.sum <- plyr::ddply(flow[ flow$Year >= start.year & flow$Year <=end.year,], "Year", plyr::summarize,
         n.days   = length(Year),
         n.Q      = sum (!is.na(Q)),
         n.miss.Q = sum ( is.na(Q)),
         min.Q    = min (Q, na.rm=TRUE),
         max.Q    = max (Q, na.rm=TRUE),
         mean.Q   = mean(Q,na.rm=TRUE),
         sd.Q     = sd  (Q,na.rm=TRUE))
flow.sum

# visuallize the min, max, and mean
plotdata <- reshape2::melt(flow.sum,
             id.var='Year',
             measure.var=c("min.Q","max.Q","mean.Q","sd.Q"),
             variable.name='Statistic',
             value.name='Value')
ggplot2::ggplot(data=plotdata, aes(x=Year, y=Value))+
  ggtitle("Summary statistics about Q over time")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line()+
  facet_wrap(~Statistic, ncol=2, scales="free_y")


#------------------------------------------------------------------------------
# Some preliminary plots to check for outliers etc
ggplot2::ggplot(data=flow[flow$Year >= start.year & flow$Year <=end.year,], aes(x=Date, y=Q))+
   ggtitle("Q by date")+
   theme(plot.title = element_text(hjust = 0.5))+
   geom_line(aes(group=Year))

ggplot2::ggplot(data=flow[flow$Year >= start.year & flow$Year <=end.year,], aes(x=Date, y=log(Q)))+
   ggtitle("log(Q) by date")+
   theme(plot.title = element_text(hjust = 0.5))+
   geom_line(aes(group=Year))

ggplot2::ggplot(data=flow[flow$Year >= start.year & flow$Year <=end.year,], aes(x=Date, y=log(Q)))+
   ggtitle("Q by date")+
   theme(plot.title = element_text(hjust = 0.5))+
   geom_line()+
   facet_wrap(~Year, scales="free_x")




#--------------------------------------------------------------
# Compute the statistics on an annual basis


na.rm=list(na.rm.global=TRUE)

help(compute.Q.stat.annual)

# the defaults are usually good enough
stat.annual <- compute.Q.stat.annual(Station.Code=Station.Code,
                          Station.Area=Station.Area,
                          flow=flow,
                          start.year=start.year,
                          end.year=end.year,
                          report.dir=report.dir)

# but you have control over which files to be written and NA removal etc. See help file
stat.annual <- compute.Q.stat.annual(Station.Code=Station.Code,
                          Station.Area=Station.Area,
                          flow=flow,
                          start.year=start.year,
                          end.year=end.year,
                          write.wy.stat.csv=FALSE,        # write out statistics?
                          write.stat.trans.csv=FALSE,  # write out statistics in transposed format
                          plot.stat.trend=TRUE,
                          report.dir=report.dir,
                          na.rm=na.rm)


names(stat.annual)

head(stat.annual$Q.stat.annual)
tail(stat.annual$Q.stat.annual)

head(stat.annual$Q.stat.annual.trans)

head(stat.annual$dates.missing.flows)

stat.annual$file.stat.csv
stat.annual$file.stat.trans.csv
stat.annual$file.stat.trend.pdf


#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
# Compute the long-term statistics

stat.longterm <- compute.Q.stat.longterm(Station.Code=Station.Code,
                          flow=flow,
                          start.year=start.year,
                          end.year=end.year,
                          report.dir=report.dir)

names(stat.longterm)

head(stat.longterm$Q.cy.stat.longterm)
tail(stat.longterm$Q.cy.stat.longterm)

head(stat.longterm$Q.cy.stat.longterm.trans)

stat.longterm$file.cy.stat.csv
stat.longterm$file.cy.stat.trans.csv




#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
# Compute the long-term percentile statistics

percentile.longterm <- compute.Q.percentile.longterm(Station.Code=Station.Code,
                          flow=flow,
                          start.year=start.year,
                          end.year=end.year,
                          report.dir=report.dir)


names(percentile.longterm)

head(percentile.longterm$Q.cy.percentile.stat)
tail(percentile.longterm$Q.cy.percentile.stat)

head(percentile.longterm$Q.cy.percentile.stat.trans)

percentile.longterm$file.cy.stat.csv
percentile.longterm$file.cy.stat.trans.csv



#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
# Compute the volume frequency analysis on the calendar year

vfa.analysis <- compute.volume.frequency.analysis(
                      Station.Code=Station.Code,
                      flow        =flow,
                      start.year  =start.year,
                      end.year    =end.year,
                      report.dir  =report.dir)

help(compute.volume.frequency.analysis)


# Compute the volume frequency analysis on the water year
vfa.analysis <- compute.volume.frequency.analysis(
                      Station.Code=Station.Code,
                      flow        =flow,
                      start.year  =start.year,
                      end.year    =end.year,
                      use.water.year=TRUE,  # here is the change
                      use.log=FALSE,
                      use.max=FALSE,
                      fit.distr="PIII",
                      write.stat.csv=TRUE,
                      write.plotdata.csv=TRUE,
                      write.quantiles.csv=TRUE,
                      report.dir=report.dir)
names(vfa.analysis)

vfa.analysis$file.stat.csv

head(vfa.analysis$Q.stat)
head(vfa.analysis$Q.stat.trans)

vfa.analysis$freqplot
ggsave(plot=vfa.analysis$freqplot,
       file=file.path(report.dir, paste(Station.Code,"-annual-vfa-frequency-plot.png",sep="")), h=6, w=6, units="in", dpi=300)

vfa.analysis$fitted.quantiles.trans

