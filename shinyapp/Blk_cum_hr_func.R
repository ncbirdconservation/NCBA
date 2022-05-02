### NCBA - Block Completion Assessment
# Scott M. Pearson, 29 April 2022
# This R function computes metrics from NCBA Mongo data: cumulative survey hours,
# cumulative nocturnal hours. It creates a summary figure.
# Cumulative hours are separated by month and season.

### Usage: block_hrs(input)
#  Input is a dataframe of eBird checklists that has these required fields:
#	DURATION_MINUTES, LATITUDE, LONGITUDE, MONTH, OBSERVATION_DATE,
#	TIME_OBSERVATIONS_STARTED, YEAR (additional fields are ignored)
# Function returns a list with 4 items: (a) blk_hrs: dataframe of cumulative hours by month and year,
#	(b) total_hr: total cumulative survey hours, (c) noc_hr: total nocturnal survey hours,
#	(d) hr_plot: stacked bar plot of hours by month and year with total_hr and noc_hr notation.

library(tidyverse)
library(lubridate)
library(ggplot2)
library(suncalc)
library(grid)

block_hrs <- function(d){	# pass a dataframe of eBird checklists
  d.t <- d[,c("DURATION_MINUTES", "LATITUDE", "LONGITUDE", "MONTH", "OBSERVATION_DATE",
	"TIME_OBSERVATIONS_STARTED", "YEAR")]	# fields of interest

  d.t <- distinct(d.t)		# Save only one record per checklist; filter out shared checklists
  d.t$date <- paste(d.t$OBSERVATION_DATE, d.t$TIME_OBSERVATIONS_STARTED, sep=" ")

  ## Diurnal/nocturnal
  # remove records where TIME_OBSERVATIONS_STARTED is missing
  sum(d.t$TIME_OBSERVATIONS_STARTED=="")	# missing for 979 records
  d.t <- d.t[!(d.t$TIME_OBSERVATIONS_STARTED)=="",]
  # Treating all times as "local" to avoid problem with daylight savings time.
  # Converting all local times to UTC
  q24dyx <- data.frame(date=as.Date(d.t$date, tz="EST"), lat=d.t$LATITUDE, 
				lon=d.t$LONGITUDE)
  q24dyx$date <- as.Date(with_tz(q24dyx$date, tzone="UTC"), tz="UTC")	# convert obs times to UTC
  tm <- getSunlightTimes(data=q24dyx, keep=c("sunrise", "sunset"), tz="UTC")
	# sunrise, sunset times
  tm$obs.tm <- with_tz(d.t$date, tzone="UTC") # convert observation times to UTC
  tm$obs.rise <- as.numeric(difftime(tm$sunrise, tm$obs.tm, units="hours", tz="UTC"))
  tm$obs.set <- as.numeric(difftime(tm$sunset,tm$obs.tm, units="hours", tz="UTC"))
  tm$diur.noc <- "diurnal"
  tm$diur.noc[tm$obs.rise>(0.5) | tm$obs.set<(-0.5)] <- "nocturnal"
	# nocturnal = >half hour before sunrise or >half hour after sunset
  d.t$diur.noc <- tm$diur.noc
  rm(tm, q24dyx)

  ## Survey hours by month and year
  out <- by(d.t$DURATION_MINUTES, INDICES=list(d.t$YEAR,d.t$MONTH), FUN=sum)
  out[is.na(out)] <- 0	# convert NAs to 0
  out.hr <- out/60		# convert minutes to hours
  # convert by object to dataframe
  out.a <- array(out.hr, dim(out.hr), dimnames(out.hr))
  blk.hr <- data.frame(block=dimnames(out.hr)[[1]], out.a)
  names(blk.hr) <- c("year", dimnames(out.hr)[[2]])
  row.names(blk.hr) <- NULL

  # add 0 data for missing months
  if (ncol(blk.hr)<13){
	flds <- names(blk.hr)[-1]
	mdat <- data.frame(matrix(data=NA, ncol=12, nrow=nrow(blk.hr)))
	names(mdat) <- as.character(1:12)
	j <- match(flds, names(mdat))
	mdat[,j] <- blk.hr[,flds]
	mdat[is.na(mdat)] <- 0
	blk.hr <- cbind(blk.hr[,1],mdat)
	names(blk.hr)[1] <- "year"
	rm(mdat, flds, j)
			    }	# close if ncol(blk.hr)

  # total cumlative hours
  total.hr <- apply(blk.hr[,2:13],1,sum)
  # total nocturnal hours
  noc.hr <- ( sum(d.t$DURATION_MINUTES[d.t$diur.noc == "nocturnal"]) )/60

  ## Stacked bar: hours by month, years stacked
  # reconfigure blk.hr for plotting
  blkhr.df <- gather(blk.hr, year)
  blkhr.df$month <- as.numeric(blkhr.df$year)
  blkhr.df$month <- factor(blkhr.df$month)
  blkhr.df$Year <- factor(blk.hr$year, levels=sort(unique(blk.hr$year), decreasing=T))
  # put cumulative total hrs  and nocturnal hrs on plot
  txt <- paste("Total cumulative hours: ", round(total.hr,1), "\n",
  		"Total nocturnal hours: ", round(noc.hr,2),sep="")
  grob <- grobTree(textGrob(txt, x=0.1,  y=0.92, hjust=0,
     gp=gpar(col="black", fontsize=10, fontface="italic")))				# font size
  hr.plt <- ggplot(blkhr.df, aes(fill=Year, y=value, x=month) ) + 
    geom_bar(position="stack", stat="identity") +
    scale_fill_brewer(palette="Accent") + 
    labs(x="Month", y="Hours", title="Survey Hours") +
    theme(axis.text=element_text(size=11), axis.title=element_text(size=12),	# font size
        	plot.title=element_text(size=12,face="bold")) +				# font size
    annotation_custom(grob)

  blkhr.list <- list(blk_hrs=blk.hr, total_hr=total.hr, noc_hr=noc.hr, hr_plot=hr.plt)
  return(blkhr.list) }

