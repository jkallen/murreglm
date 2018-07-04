library(lubridate)
# Code for processing and producing wave/storm indices

# read in datafiles and replace NA
setwd("C:/Users/Tim Jones/Desktop/COASST/Jazzmine data/Waveheight info")
s46029 <- read.csv(file="S46029.csv")
s46029$WVHT[s46029$WVHT > 90] <- NA
s46029$WVHT[s46029$WVHT < 0.25] <- NA		# removes very low values that are likely errors
s46041 <- read.csv(file="S46041.csv")
s46041$WVHT[s46041$WVHT > 90] <- NA
s46041$WVHT[s46041$WVHT < 0.25] <- NA		# removes very low values that are likely errors

# convert y-m-d-h to date for initial plot checks
chardate <- paste(s46029$YYYY, "-", s46029$MM, "-", s46029$DD, " ", s46029$hh, ":00:00", sep="")
s46029$date <- as.numeric(strptime(chardate, "%Y-%m-%d %H:%M:%S",tz="GMT"))

chardate <- paste(s46041$YYYY, "-", s46041$MM, "-", s46041$DD, " ", s46041$hh, ":00:00", sep="")
s46041$date <- as.numeric(strptime(chardate, "%Y-%m-%d %H:%M:%S",tz="GMT"))

date.ax <- paste(2002:2017, "-01-01 00:00:00", sep="")
date.ax <- as.numeric(strptime(date.ax, "%Y-%m-%d %H:%M:%S",tz="GMT"))

plot(WVHT ~ date, data=s46029, type="l", xaxt="n", col=rgb(0,0,0,0.3))
lines(WVHT ~ date, data=s46041, type="l", xaxt="n", col=rgb(0.9,0,0,0.3))
axis(side=1, at=as.numeric(date.ax), labels=2002:2017)

# combine datasets but ensuring that dates and times match
# create empty dataframe of all dates from 2002 to 2016

dates <- seq(from=as.Date("2002-01-01"), to=as.Date("2016-12-31"), by=1)
hours <- 0:23

time.table <- data.frame(expand.grid(hours, dates))
names(time.table) <- c("hh", "date")
time.table$YY <- year(time.table$date)
time.table$MM <- month(time.table$date)
time.table$DD <- day(time.table$date)

# create a unique identifier in each dataset based on y-m-d-h for merging
time.table$merg <- paste(time.table$YY, time.table$MM, time.table$DD, time.table$hh, sep="-")
s46029$merg <- paste(s46029$YYYY, s46029$MM, s46029$DD, s46029$hh, sep="-")
s46041$merg <- paste(s46041$YYYY, s46041$MM, s46041$DD, s46041$hh, sep="-")

m.29 <- s46029[,c("WVHT", "merg")]
names(m.29)[1] <- "WVHT.29"
m.41 <- s46041[,c("WVHT", "merg")]
names(m.41)[1] <- "WVHT.41"

# Merge datasets
merge1 <- merge(x=time.table, y=m.29, all.x=TRUE, by="merg")
merge2 <- merge(x=merge1, y=m.41, all.x=TRUE, by="merg")
comb.dat <- merge2

# convert y-m-d-h to date
chardate <- paste(comb.dat$YY, "-", comb.dat$MM, "-", comb.dat$DD, " ", comb.dat$hh, ":00:00", sep="")
comb.dat$date <- as.numeric(strptime(chardate, "%Y-%m-%d %H:%M:%S",tz="GMT"))

comb.dat <- comb.dat[order(comb.dat$date),]

# Examine data and compare to original plot to make sure that merge has worked
par(mfrow=c(2,1))
# upper panel - processed data
plot(WVHT.29 ~ date, data=comb.dat, type="l", xaxt="n", col=rgb(0,0,0,0.3))
lines(WVHT.41 ~ date, data=comb.dat, type="l", xaxt="n", col=rgb(0.9,0,0,0.3))
axis(side=1, at=as.numeric(date.ax), labels=2002:2017)
# lower panel - original data
plot(WVHT ~ date, data=s46029, type="l", xaxt="n", col=rgb(0,0,0,0.3))
lines(WVHT ~ date, data=s46041, type="l", xaxt="n", col=rgb(0.9,0,0,0.3))
axis(side=1, at=as.numeric(date.ax), labels=2002:2017)

#############################################################################################################
# Now look at correlation between datasets for interpolation purposes
# Plot one against the other

plot(sqrt(WVHT.29) ~ sqrt(WVHT.41), data=comb.dat, type="p", col=rgb(0,0,0,0.2), cex=0.2,pch=16, ylim=c(0,4), xlim=c(0,4), asp=1)
abline(a=0, b=1, col=2)

# fit a lm to examine how correlated they are

lm1 <- lm(sqrt(WVHT.29) ~ 0+sqrt(WVHT.41), data=comb.dat)
lm2 <- lm(sqrt(WVHT.41) ~ 0+sqrt(WVHT.29), data=comb.dat)

# Fill in missing values

beta29from41 <- lm1$coefficients
beta41from29 <- lm2$coefficients

comb.dat$WVHT.29[is.na(comb.dat$WVHT.29)] <- (beta29from41 * sqrt(comb.dat$WVHT.41[is.na(comb.dat$WVHT.29)]))^2
comb.dat$WVHT.41[is.na(comb.dat$WVHT.41)] <- (beta41from29 * sqrt(comb.dat$WVHT.29[is.na(comb.dat$WVHT.41)]))^2

plot(WVHT.29 ~ date, data=comb.dat, type="l", xaxt="n", col=rgb(0,0,0,0.3))
lines(WVHT.41 ~ date, data=comb.dat, type="l", xaxt="n", col=rgb(0.9,0,0,0.3))
axis(side=1, at=as.numeric(date.ax), labels=2002:2017)


################################################################################################################
# Now calculate the average of the two stations
comb.dat$ave.wvht <- rowMeans(comb.dat[,c("WVHT.29", "WVHT.41")])



################################################################################################################
# Define function for calculating indices

Wave.index.fun <- function(month.include, wave.threshold, event.minlength){
	# FUNCTION DESCRIPTION
	# This function calculates three indices for each year of the dataset provided specific to the months specified
	# Index 1: average significant wave height - this is simply the mean Hsig across the months asked for
	# Index 2: proportion of time Hsig exceeds X, where X is defined by the input wave.threshold 
	# Index 3: number of storm 'events', where an event is defined as wave height >= X for a minimum of Y hours. X and Y are specified as wave.threshold and event.minlength
	
	# INPUTS
	# month.include: a list describing the month numbers to be included in the analysis i.e. c(5,6,7,8) would calculate the indices for May - August
	# wave.threshold: the wave height threshold used to calculate the second index
	# event.minlength: defines the minimum amount of time that waves should be in excess of wave.threshold continuously to constitute an event
	
	# DATA PREP
	# split the data into Nyear data frames for subsequent calculations
	# calculate the number of missing values and present values
	years <- unique(comb.dat$YY)
	Nyears <- length(years)
	Nmiss <- rep(0, Nyears)
	Npres <- rep(0, Nyears)
	data.store <- list()
	for(i in 1:Nyears){
		data.store[[i]] <- comb.dat[comb.dat$MM %in% month.include & comb.dat$YY==years[i],]
		Nmiss[i] <- nrow(comb.dat[comb.dat$MM %in% month.include & comb.dat$YY==years[i] & is.na(comb.dat$ave.wvht),])
		Npres[i] <- nrow(comb.dat[comb.dat$MM %in% month.include & comb.dat$YY==years[i] & !is.na(comb.dat$ave.wvht),])
	}
		
	# Index 1 - average significant wave height
	Ave.Hsig <- rep(0, Nyears)
	for(i in 1:Nyears){
		Ave.Hsig[i] <- mean(data.store[[i]]$ave.wvht, na.rm=T)
	}
	
	# Index 2 - proportion of time Hsig > wave.threshold
	Prop.Hsig <- rep(0, Nyears)
	for(i in 1:Nyears){
		tmp <- data.store[[i]]
		Len.gtX <- nrow(tmp[tmp$ave.wvht >= wave.threshold & !is.na(tmp$ave.wvht),])
		Len.tot <- nrow(tmp[!is.na(tmp$ave.wvht),])
		Prop.Hsig [i] <- Len.gtX/Len.tot
	}
	
	# Index 3 - number of storm events
	Nevent.Hsig <- rep(0, Nyears)
	
	for(i in 1:Nyears){
		tmp <- data.store[[i]]
		
		Q1 <- tmp[tmp$ave.wvht >= wave.threshold & !is.na(tmp$ave.wvht),]
		
		# Identify continuous "events"
		# Order queried data by date
		NOBS <- nrow(Q1)
		if(NOBS >= 1) {
			Q2 <- Q1[order(Q1$date),]
			
			# loop through observations and identify events (i.e. are observations continuous) and number them
			
			event.num <- rep(0, nrow(Q1))
			event.num[1] <- 1
			
			for(j in 2:nrow(Q2)){
			
				time.interval <- Q2$date[j] - Q2$date[j-1]			# time interval between observations i and i-1
			
				if(time.interval == 3600){			# if time interval is equal to 1 hour (3600 seconds) then assign event # as continuation of previous
					event.num[j] <- event.num[j-1]
				} else {							# else create new event and label it as such
					event.num[j] <- event.num[j-1] + 1
				}
			}
			
			Q2 <- data.frame(Q2, event.num)
			
			# Now identify the length of each event
			Event.length <- tapply(X=Q2$hh, INDEX=Q2$event.num, FUN=length)
			Nevent.Hsig[i] <- length(Event.length[Event.length >= event.minlength])
		} else {
			Nevent.Hsig[i] <- 0
		}
	}
	
	outdat <- data.frame(years, Nmiss, Npres, Ave.Hsig, Prop.Hsig, Nevent.Hsig)
	return(outdat)
}


Wave.index.fun (month.include=c(7,8,9,10), wave.threshold=3, event.minlength=6)








