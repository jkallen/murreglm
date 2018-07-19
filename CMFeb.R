# COMMON MURRE MORPHOMETRICS PROJECT
setwd("C:/Users/jka3/Desktop/COASST/COASST R Work/murreglm")
fs <- read.csv ("../Dependent variables/Coasst data with molt field.csv")
fs <- data.frame (fs)

# Separates COMU from everyone else.
allcm<-fs[which(fs$Verifier.Species.Name=="Common Murre"),]

notcm <-fs[which(fs$Verifier.Species.Name!="Common Murre"),]

# Separates new finds from re-finds.
cmnew<- allcm[which(allcm$Bird.Refound == "No"), ]

cmre<- allcm[which(allcm$Bird.Refound == "Yes"), ]

# Separates lower 48 states from other areas.
lower48<- cmnew[which (!(cmnew$Region == "Aleutian Islands" |
                            cmnew$Region == "Bering Sea" |
                            cmnew$Region == "Chukchi" |
                            cmnew$Region == "Gulf of Alaska"|
                            cmnew$Region == "Southeast Alaska")),]

notlower48<- cmnew[which (cmnew$Region == "Aleutian Islands" |
                            cmnew$Region == "Bering Sea" |
                            cmnew$Region == "Chukchi" |
                            cmnew$Region == "Gulf of Alaska"|
                            cmnew$Region == "Southeast Alaska"),]

# Separates birds with wing chord measurements from those without, or
# with measurement errors.
realwings <- lower48[which(lower48$Wing.Length < 23 & 
                       lower48$Wing.Length > 0), ]

zerowings <- lower48[which(lower48$Wing.Length <= 0), ]

giantwings <- lower48[which(lower48$Wing.Length >= 23), ]

# Separates birds with tarsus or beak measurement from those without.
tarbeak <- realwings[which(realwings$Bill.Length > 0 |    
                       realwings$Tarsus.Length > 0), ]

notarbeak <- realwings [which(realwings$Bill.Length <= 0 &
                                realwings$Tarsus.Length <= 0),]

#nabeak <- tarbeak [which(is.na(tarbeak$Bill.Length)),]

# Separates adults from juveniles.
adults <- tarbeak [which(tarbeak$Wing.Length > 12 &
                           (tarbeak$Bill.Length >= 40 |
                            tarbeak$Tarsus.Length >= 33)), ]

juveniles <- tarbeak [which(tarbeak$Wing.Length < 18 &
                           tarbeak$Bill.Length < 40 & 
                           tarbeak$Tarsus.Length < 33), ]
#Separate molting from non-molting adults by size.

moltsize <- adults[which(adults$Wing.Length < 18 &
                                  (!(adults$Month == "Jan" |
                                       adults$Month == "Feb" |
                                       adults$Month == "Mar" |
                                       adults$Month == "Apr" |
                                       adults$Month == "May"  |
                                       adults$Month == "Dec"))),] 

nomoltsize <- adults[which((adults$Month == "Jan" |
                                      adults$Month == "Feb" |
                                      adults$Month == "Mar" |
                                      adults$Month == "Apr" |
                                      adults$Month == "May"  |
                                      adults$Month == "Dec") |
                                     (adults$Wing.Length > 18 &
                                        (!(adults$Month == "Jan" |
                                             adults$Month == "Feb" |
                                             adults$Month == "Mar" |
                                             adults$Month == "Apr" |
                                             adults$Month == "May"  |
                                             adults$Month == "Dec")))),] 

nomoltsize$molt.size<- 0
moltsize$molt.size<- 1
coadults<- rbind(moltsize, nomoltsize)
# Separate out coadults by year.
#ninetynine <-coadults[which(coadults$Year == "1999"), ]
#zero <-coadults[which(coadults$Year == "2000"), ]
#one <-coadults[which(coadults$Year == "2001"), ]
library(lubridate)

coadults$yday <- yday(coadults$Survey.Date)
two <-coadults[which(coadults$Year == "2002"), ]
two$yday <- yday(two$Survey.Date)
two<- two[which(two$yday>99),]
two$newday <-two$yday
twoend <-coadults[which(coadults$Year == "2003" &
                          coadults$yday<36),]
twoend$newday <- twoend$yday + 365
two <- rbind.data.frame(two,twoend)
three <-coadults[which(coadults$Year == "2003"), ]
three$yday <- yday(three$Survey.Date)
three<- three[which(three$yday>99),]
four <-coadults[which(coadults$Year == "2004"), ]
four$yday <- yday(four$Survey.Date)
four <- four[which(four$yday>99),]
five <-coadults[which(coadults$Year == "2005"), ]
five$yday <- yday(five$Survey.Date)
five <- four[which(five$yday>99),]
six <-coadults[which(coadults$Year == "2006"), ]
six$yday <- yday(six$Survey.Date)
six <- six[which(six$yday>99),]
seven <-coadults[which(coadults$Year == "2007"), ]
seven$yday <- yday(seven$Survey.Date)
six <- seven[which(seven$yday>99),]
eight <-coadults[which(coadults$Year == "2008"), ]
eight$yday <- yday(eight$Survey.Date)
eight <- eight[which(eight$yday>99),]
nine <-coadults[which(coadults$Year == "2009"), ]
ten <-coadults[which(coadults$Year == "2010"), ]
eleven <-coadults[which(coadults$Year == "2011"), ]
twelve <-coadults[which(coadults$Year == "2012"), ]
thirteen <-coadults[which(coadults$Year == "2013"), ]
fourteen <-coadults[which(coadults$Year == "2014"), ]
fifteen <-coadults[which(coadults$Year == "2015"), ]
sixteen <-coadults[which(coadults$Year == "2016"), ]

two <-coadults[which(coadults$Year == "2002"), ]
two$yday <- yday(two$Survey.Date)
two<- two[which(two$yday>99),]
two$newday <-two$yday
twoend <-coadults[which(coadults$Year == "2003" &
                          coadults$yday<36),]
twoend$newday <- twoend$yday + 365
two <- rbind.data.frame(two,twoend)
three <-coadults[which(coadults$Year == "2003"), ]
three$yday <- yday(three$Survey.Date)
three<- three[which(three$yday>99),]
three$newday <-three$yday
threeend <-coadults[which(coadults$Year == "2004" &
                          coadults$yday<36),]
threeend$newday <- threeend$yday + 365
three <- rbind.data.frame(three,threeend)
four <-coadults[which(coadults$Year == "2004"), ]
four$yday <- yday(four$Survey.Date)
four<- four[which(four$yday>99),]
four$newday <-four$yday
fourend <-coadults[which(coadults$Year == "2005" &
                            coadults$yday<36),]
fourend$newday <- fourend$yday + 365
four <- rbind.data.frame(four,fourend)
five <-coadults[which(coadults$Year == "2005"), ]
five$yday <- yday(five$Survey.Date)
five<- five[which(five$yday>99),]
five$newday <-five$yday
fiveend <-coadults[which(coadults$Year == "2006" &
                           coadults$yday<36),]
fiveend$newday <- fiveend$yday + 365
five <- rbind.data.frame(five,fiveend)

six <-coadults[which(coadults$Year == "2006"), ]
six$yday <- yday(six$Survey.Date)
six<- six[which(six$yday>99),]
six$newday <-six$yday
sixend <-coadults[which(coadults$Year == "2007" &
                           coadults$yday<36),]
sixend$newday <- sixend$yday + 365
six <- rbind.data.frame(six,sixend)

seven <-coadults[which(coadults$Year == "2007"), ]
seven$yday <- yday(seven$Survey.Date)
seven<- seven[which(seven$yday>99),]
seven$newday <-seven$yday
sevenend <-coadults[which(coadults$Year == "2008" &
                          coadults$yday<36),]
sevenend$newday <- sevenend$yday + 365
seven <- rbind.data.frame(seven,sevenend)

eight <-coadults[which(coadults$Year == "2008"), ]
eight$yday <- yday(eight$Survey.Date)
eight<- eight[which(eight$yday>99),]
eight$newday <-eight$yday
eightend <-coadults[which(coadults$Year == "2009" &
                            coadults$yday<36),]
eightend$newday <- eightend$yday + 365
eight <- rbind.data.frame(eight,eightend)
nine <-coadults[which(coadults$Year == "2009"), ]
nine$yday <- yday(nine$Survey.Date)
nine<- nine[which(nine$yday>99),]
nine$newday <-nine$yday
nineend <-coadults[which(coadults$Year == "2010" &
                            coadults$yday<36),]
nineend$newday <- nineend$yday + 365
nine <- rbind.data.frame(nine,nineend)
ten <-coadults[which(coadults$Year == "2010"), ]
ten$yday <- yday(ten$Survey.Date)
ten<- ten[which(ten$yday>99),]
ten$newday <-ten$yday
tenend <-coadults[which(coadults$Year == "2011" &
                           coadults$yday<36),]
tenend$newday <- tenend$yday + 365
ten <- rbind.data.frame(ten,tenend)
eleven <-coadults[which(coadults$Year == "2011"), ]
eleven$yday <- yday(eleven$Survey.Date)
eleven<- eleven[which(eleven$yday>99),]
eleven$newday <-eleven$yday
elevenend <-coadults[which(coadults$Year == "2012" &
                          coadults$yday<36),]
elevenend$newday <- elevenend$yday + 365
eleven <- rbind.data.frame(eleven,elevenend)
twelve <-coadults[which(coadults$Year == "2012"), ]
twelve$yday <- yday(twelve$Survey.Date)
twelve<- twelve[which(twelve$yday>99),]
twelve$newday <-twelve$yday
twelveend <-coadults[which(coadults$Year == "2013" &
                             coadults$yday<36),]
twelveend$newday <- twelveend$yday + 365
twelve <- rbind.data.frame(twelve,twelveend)
thirteen <-coadults[which(coadults$Year == "2013"), ]
thirteen$yday <- yday(thirteen$Survey.Date)
thirteen<- thirteen[which(thirteen$yday>99),]
thirteen$newday <-thirteen$yday
thirteenend <-coadults[which(coadults$Year == "2014" &
                             coadults$yday<36),]
thirteenend$newday <- thirteenend$yday + 365
thirteen <- rbind.data.frame(thirteen,thirteenend)
fourteen <-coadults[which(coadults$Year == "2014"), ]
fourteen$yday <- yday(fourteen$Survey.Date)
fourteen<- fourteen[which(fourteen$yday>99),]
fourteen$newday <-fourteen$yday
fourteenend <-coadults[which(coadults$Year == "2015" &
                               coadults$yday<36),]
fourteenend$newday <- fourteenend$yday + 365
fourteen <- rbind.data.frame(fourteen,fourteenend)
fifteen <-coadults[which(coadults$Year == "2015"), ]
fifteen$yday <- yday(fifteen$Survey.Date)
fifteen<- fifteen[which(fifteen$yday>99),]
fifteen$newday <-fifteen$yday
fifteenend <-coadults[which(coadults$Year == "2016" &
                               coadults$yday<36),]
fifteenend$newday <- fifteenend$yday + 365
fifteen <- rbind.data.frame(fifteen,fifteenend)
sixteen <-coadults[which(coadults$Year == "2016"), ]
sixteen$yday <- yday(sixteen$Survey.Date)
sixteen<- sixteen[which(sixteen$yday>99),]
sixteen$newday <-sixteen$yday
sixteenend <-coadults[which(coadults$Year == "2017" &
                              coadults$yday<36),]
sixteenend$newday <- sixteenend$yday + 365
sixteen <- rbind.data.frame(sixteen,sixteenend)



library(gam)
#library(mgcv)
# Plotting one for each year
ngam<-gam(molt.size~s(yday(coadults$Survey.Date)), family="binomial", data= coadults, gamma=1)
newd <- data.frame(yday(coadults$Survey.Date))
pred <- data.frame(predict.gam(ngam, type="response"))
nepred<- cbind.data.frame(newd, pred)

plot(nepred$yday.coadults.Survey.Date, nepred$predict.gam.ngam..type....response..,
     xlab = "Day of Year", ylab = "Probability",
     xaxt='n',
     ylim = c(0, .4),
     col="darkcyan")
axis(side=1, at=(seq(0,365, 30.08)))

#Plotting separate one for each year
#1999,2000, 2001 too small
library(mgcv)
#$start here

library(mgcv)
sixteen$yday <- yday(sixteen$Survey.Date)

sixteengam<-gam(molt.size~s(yday), family="binomial", data= sixteen, gamma=6)
sixteend <- data.frame(yday(sixteen$Survey.Date))
sixteenpred <- data.frame(predict.gam(sixteengam, type="response"))
sixteenresult<- cbind.data.frame(sixteend, sixteenpred)
pred.df <- data.frame(yday=1:365)
p16 <- predict.gam(sixteengam, se.fit = TRUE, newdata=pred.df)
fit16 <- p16$fit
upr16 <- p16$fit + (2 * p16$se.fit)
lwr16 <- p16$fit - (2 * p16$se.fit)
library(boot)
fit16 <- exp(fit16)/(1+exp(fit16))
upr16<- exp(upr16)/(1+exp(upr16))
lwr16 <- exp(lwr16)/(1+exp(lwr16))
plot(sixteenresult,col="darkblue",
      xlab="Survey Date", ylab="Proportion in Molt", ylim=c(0,1))
lines(upr16,type="l",col="darkgreen")
lines(lwr16,type="l",col="darkgreen")


library(mgcv)
gammatron3000<- function(year) {
  year$yday <- yday(year$Survey.Date)
  yeargam<-gam(molt.size~s(yday), family="binomial", data= year, gamma=6)
  yeard <- data.frame(yday(year$Survey.Date))
  yearpred <- data.frame(predict.gam(yeargam, type="response"))
  yearresult<- cbind.data.frame(yeard, yearpred)
  pred.df <- data.frame(yday=0:365)
  p <- predict.gam(yeargam, se.fit = TRUE, newdata=pred.df)
  fit <- p$fit
  upr <- p$fit + (2 * p$se.fit)
  lwr <- p$fit - (2 * p$se.fit)
  fit <- exp(fit)/(1+exp(fit))
  upr<- exp(upr)/(1+exp(upr))
  lwr <- exp(lwr)/(1+exp(lwr))
  plot(yearresult,col="darkblue",
       xlab="Survey Date", ylab="Proportion in Molt", ylim=c(0,1))
  lines(upr,type="l",col="purple")
  lines(lwr,type="l",col="green")
}
#gammatron5000(two)
gammatron3000(three)
gammatron3000(four)
gammatron3000(five)
gammatron3000(six)
gammatron3000(seven)
gammatron3000(eight)
gammatron3000(nine)
gammatron3000(ten)
gammatron3000(eleven)
gammatron3000(twelve)
gammatron3000(thirteen)
gammatron3000(fourteen)
gammatron3000(fifteen)
gammatron3000(sixteen)



library(mgcv)
gammatron5000<- function(year) {
  year$yday <- yday(year$Survey.Date)
#year <- two
  yeargam<-gam(molt.size~s(newday), family="binomial", data= year, gamma=6)
  day <- (year$newday)
  #yeard <- data.frame(newday(year$Survey.Date))
  yearpred <- data.frame(predict.gam(yeargam, type="response"))
  yearresult<- cbind.data.frame(day, yearpred)
  pred.df <- data.frame(newday=100:400)
  p <- predict.gam(yeargam, se.fit = TRUE, newdata=pred.df)
  fit <- p$fit
  upr <- p$fit + (2 * p$se.fit)
  lwr <- p$fit - (2 * p$se.fit)
  fit <- exp(fit)/(1+exp(fit))
  upr<- exp(upr)/(1+exp(upr))
  lwr <- exp(lwr)/(1+exp(lwr))
upr <- cbind.data.frame(upr, pred.df)
lwr <- cbind.data.frame(lwr, pred.df)
  #plot(yearresult,col="darkturquoise",
   #    xlab="Survey Date", ylab="Proportion in Molt", ylim=c(0,1))
  #lines(upr,type="l",col="purple")
  #lines(lwr,type="l",col="green")
  #lines(upr$newday, upr$upr, col="purple")
  #lines(lwr$newday, lwr$lwr, col="green")
  return(yearresult)
}
#gam2<- gammatron5000(two)
uppatron5000<- function(year) {
  year$yday <- yday(year$Survey.Date)
  #year <- two
  yeargam<-gam(molt.size~s(newday), family="binomial", data= year, gamma=6)
  day <- (year$newday)
  #yeard <- data.frame(newday(year$Survey.Date))
  yearpred <- data.frame(predict.gam(yeargam, type="response"))
  yearresult<- cbind.data.frame(day, yearpred)
  pred.df <- data.frame(newday=100:400)
  p <- predict.gam(yeargam, se.fit = TRUE, newdata=pred.df)
  fit <- p$fit
  upr <- p$fit + (2 * p$se.fit)
  lwr <- p$fit - (2 * p$se.fit)
  fit <- exp(fit)/(1+exp(fit))
  upr<- exp(upr)/(1+exp(upr))
  lwr <- exp(lwr)/(1+exp(lwr))
  upr <- cbind.data.frame(upr, pred.df)
  lwr <- cbind.data.frame(lwr, pred.df)
  #plot(yearresult,col="darkturquoise",
  #    xlab="Survey Date", ylab="Proportion in Molt", ylim=c(0,1))
  #lines(upr,type="l",col="purple")
  #lines(lwr,type="l",col="green")
  #lines(upr$newday, upr$upr, col="purple")
  #lines(lwr$newday, lwr$lwr, col="green")
  return(upr)
}
upr2<- uppatron5000(two)

downatron5000<- function(year) {
  year$yday <- yday(year$Survey.Date)
  #year <- two
  yeargam<-gam(molt.size~s(newday), family="binomial", data= year, gamma=6)
  day <- (year$newday)
  #yeard <- data.frame(newday(year$Survey.Date))
  yearpred <- data.frame(predict.gam(yeargam, type="response"))
  yearresult<- cbind.data.frame(day, yearpred)
  pred.df <- data.frame(newday=100:400)
  p <- predict.gam(yeargam, se.fit = TRUE, newdata=pred.df)
  fit <- p$fit
  upr <- p$fit + (2 * p$se.fit)
  lwr <- p$fit - (2 * p$se.fit)
  fit <- exp(fit)/(1+exp(fit))
  upr<- exp(upr)/(1+exp(upr))
  lwr <- exp(lwr)/(1+exp(lwr))
  upr <- cbind.data.frame(upr, pred.df)
  lwr <- cbind.data.frame(lwr, pred.df)
  #plot(yearresult,col="darkturquoise",
  #    xlab="Survey Date", ylab="Proportion in Molt", ylim=c(0,1))
  #lines(upr,type="l",col="purple")
  #lines(lwr,type="l",col="green")
  #lines(upr$newday, upr$upr, col="purple")
  #lines(lwr$newday, lwr$lwr, col="green")
  return(lwr)
}
#lwr2<- downatron5000(two)
#plot(gam2$day, gam2$predict.gam.yeargam..type....response.., col="darkturquoise",
 #    xlab="Survey Date", ylab="Proportion in Molt", ylim=c(0,1))
#lines(upr2$newday, upr2$upr, col="purple")
#lines(lwr2$newday, lwr2$lwr, col="green")


gam3 <- gammatron5000(three)
upr3 <- uppatron5000(three)
lwr3<- downatron5000(three)
plot(gam3$day, gam3$predict.gam.yeargam..type....response.., col="darkturquoise",
     xlab="Survey Date", ylab="Proportion in Molt", ylim=c(0,1))
lines(upr3$newday, upr3$upr, col="purple")
lines(lwr3$newday, lwr3$lwr, col="green")
abline(h=.1)


gam4 <- gammatron5000(four)
upr4 <- uppatron5000(four)
lwr4<- downatron5000(four)
plot(gam4$day, gam4$predict.gam.yeargam..type....response.., col="darkturquoise",
     xlab="Survey Date", ylab="Proportion in Molt", ylim=c(0,1))
lines(upr4$newday, upr4$upr, col="purple")
lines(lwr4$newday, lwr4$lwr, col="green")
abline(h=.1)

gam5 <- gammatron5000(five)
upr5 <- uppatron5000(five)
lwr5<- downatron5000(five)
plot(gam5$day, gam5$predict.gam.yeargam..type....response.., col="darkturquoise",
     xlab="Survey Date", ylab="Proportion in Molt", ylim=c(0,1))
lines(upr5$newday, upr5$upr, col="purple")
lines(lwr5$newday, lwr5$lwr, col="green")
abline(h=.1)

gam6 <- gammatron5000(six)
upr6 <- uppatron5000(six)
lwr6<- downatron5000(six)
upr6[which(upr6$newday < 175),] = min(upr6$upr,.06) # BIrds don't molt in March!
plot(gam6$day, gam6$predict.gam.yeargam..type....response.., col="darkturquoise",
     xlab="Survey Date", ylab="Proportion in Molt", ylim=c(0,1))
lines(upr6$newday, upr6$upr, col="purple")
lines(lwr6$newday, lwr6$lwr, col="green")
min(upr6)
abline(h=.1)
#hist(upr6$upr)

gam7 <- gammatron5000(seven)
upr7 <- uppatron5000(seven)
lwr7<- downatron5000(seven)
plot(gam7$day, gam7$predict.gam.yeargam..type....response.., col="darkturquoise",
     xlab="Survey Date", ylab="Proportion in Molt", ylim=c(0,1))
lines(upr7$newday, upr7$upr, col="purple")
lines(lwr7$newday, lwr7$lwr, col="green")
abline(h=.1)

gam8 <- gammatron5000(eight)
upr8 <- uppatron5000(eight)
lwr8<- downatron5000(eight)
plot(gam8$day, gam8$predict.gam.yeargam..type....response.., col="darkturquoise",
     xlab="Survey Date", ylab="Proportion in Molt", ylim=c(0,1))
lines(upr8$newday, upr8$upr, col="purple")
lines(lwr8$newday, lwr8$lwr, col="green")
abline(h=.1)

gam9 <- gammatron5000(nine)
upr9 <- uppatron5000(nine)
lwr9<- downatron5000(nine)
plot(gam9$day, gam9$predict.gam.yeargam..type....response.., col="darkturquoise",
     xlab="Survey Date", ylab="Proportion in Molt", ylim=c(0,1))
lines(upr9$newday, upr9$upr, col="purple")
lines(lwr9$newday, lwr9$lwr, col="green")
abline(h=.1)

gam10 <- gammatron5000(ten)
upr10 <- uppatron5000(ten)
lwr10<- downatron5000(ten)
plot(gam10$day, gam10$predict.gam.yeargam..type....response.., col="darkturquoise",
     xlab="Survey Date", ylab="Proportion in Molt", ylim=c(0,1))
lines(upr10$newday, upr10$upr, col="purple")
lines(lwr10$newday, lwr10$lwr, col="green")
abline(h=.1)

gam11 <- gammatron5000(eleven)
upr11 <- uppatron5000(eleven)
lwr11<- downatron5000(eleven)
plot(gam11$day, gam11$predict.gam.yeargam..type....response.., col="darkturquoise",
     xlab="Survey Date", ylab="Proportion in Molt", ylim=c(0,1))
lines(upr11$newday, upr11$upr, col="purple")
lines(lwr11$newday, lwr11$lwr, col="green")
abline(h=.1)

gam12 <- gammatron5000(twelve)
upr12 <- uppatron5000(twelve)
lwr12<- downatron5000(twelve)
plot(gam12$day, gam12$predict.gam.yeargam..type....response.., col="darkturquoise",
     xlab="Survey Date", ylab="Proportion in Molt", ylim=c(0,1))
lines(upr12$newday, upr12$upr, col="purple")
lines(lwr12$newday, lwr12$lwr, col="green")
abline(h=.1)

gam13 <- gammatron5000(thirteen)
upr13 <- uppatron5000(thirteen)
lwr13<- downatron5000(thirteen)
plot(gam13$day, gam13$predict.gam.yeargam..type....response.., col="darkturquoise",
     xlab="Survey Date", ylab="Proportion in Molt", ylim=c(0,1))
lines(upr13$newday, upr13$upr, col="purple")
lines(lwr13$newday, lwr13$lwr, col="green")
abline(h=.1)

gam14 <- gammatron5000(fourteen)
upr14 <- uppatron5000(fourteen)
lwr14<- downatron5000(fourteen)
plot(gam14$day, gam14$predict.gam.yeargam..type....response.., col="darkturquoise",
     xlab="Survey Date", ylab="Proportion in Molt", ylim=c(0,1))
lines(upr14$newday, upr14$upr, col="purple")
lines(lwr14$newday, lwr14$lwr, col="green")
abline(h=.1)

gam15 <- gammatron5000(fifteen)
upr15 <- uppatron5000(fifteen)
lwr15<- downatron5000(fifteen)
plot(gam15$day, gam15$predict.gam.yeargam..type....response.., col="darkturquoise",
     xlab="Survey Date", ylab="Proportion in Molt", ylim=c(0,1))
lines(upr15$newday, upr15$upr, col="purple")
lines(lwr15$newday, lwr15$lwr, col="green")
abline(h=.1)

gam16 <- gammatron5000(sixteen)
upr16 <- uppatron5000(sixteen)
lwr16<- downatron5000(sixteen)
plot(gam16$day, gam16$predict.gam.yeargam..type....response.., col="darkturquoise",
     xlab="Survey Date", ylab="Proportion in Molt", ylim=c(0,1), xlim = c(100,400))
lines(upr16$newday, upr16$upr, col="purple")
lines(lwr16$newday, lwr16$lwr, col="green")
abline(h=.1)

#year$yday <- yday(year$Survey.Date)
#year <- two
#yeargam<-gam(molt.size~s(newday), family="binomial", data= year, gamma=6)
#day <- (year$newday)
#yeard <- data.frame(newday(year$Survey.Date))
#yearpred <- data.frame(predict.gam(yeargam, type="response"))
#yearresult<- cbind.data.frame(day, yearpred)
#pred.df <- data.frame(newday=100:400)
#p <- predict.gam(yeargam, se.fit = TRUE, newdata=pred.df)
#fit <- p$fit
#upr <- p$fit + (2 * p$se.fit)
#lwr <- p$fit - (2 * p$se.fit)
#fit <- exp(fit)/(1+exp(fit))
#upr<- exp(upr)/(1+exp(upr))
#lwr <- exp(lwr)/(1+exp(lwr))
#upr <- cbind.data.frame(upr, pred.df)

###Plotting all curves on same graph
plot(gam16,col="darkblue",
     xlab="Survey Date", ylab="Proportion in Molt", ylim=c(0:1))
lines(gam2, type = "p", col="lightblue")
lines(upr2$newday, upr2$upr, col="lightblue")
lines(lwr2$newday, lwr2$lwr, col="lightblue")

lines(gam3, type = "p", col="green")
lines(gam4, type = "p", col="darkgreen")
lines(gam5, type = "p", col="orange")
lines(gam6, type = "p", col="red")
lines(gam7, type = "p", col="yellow")
lines(gam8, type = "p", col="pink")
lines(gam9, type = "p", col="purple")
lines(gam10, type = "p", col="grey")
lines(gam11, type = "p", col="black")
lines(gam12, type = "p", col="deeppink")
lines(gam13, type = "p", col="brown")
lines(gam14, type = "p", col="darkblue")
lines(gam15, type = "p", col="plum")
lines(gam16, type = "p", col="bisque")

plot(upr2$newday, upr2$upr, col="lightblue", type="l", lty="solid",
     xlab="Survey Date", ylab="Proportion in Molt", ylim=c(0:1), lwd=2)
lines(lwr2$newday, lwr2$lwr, col="lightblue", lty="dotdash", lwd=3)

lines(upr3$newday, upr3$upr, col="green", lwd=2)
lines(lwr3$newday, lwr3$lwr, col="green", lty="dotdash", lwd=3)

lines(upr4$newday, upr4$upr, col="darkblue", lwd=2)
lines(lwr4$newday, lwr4$lwr, col="darkblue", lty="dotdash", lwd=3)

lines(upr5$newday, upr5$upr, col="orange", lwd=2)
lines(lwr5$newday, lwr5$lwr, col="orange", lty="dotdash", lwd=3)

lines(upr6$newday, upr6$upr, col="red", lwd=2)
lines(lwr6$newday, lwr6$lwr, col="red", lty="dotdash", lwd=3)

lines(upr7$newday, upr7$upr, col="purple", lwd=2)
lines(lwr7$newday, lwr7$lwr, col="purple", lty="dotdash", lwd=3)

lines(upr8$newday, upr8$upr, col="pink", lwd=2)
lines(lwr8$newday, lwr8$lwr, col="pink", lty="dotdash", lwd=3)

plot(upr9$newday, upr9$upr, col="lightblue", type="l", lty="solid",
     xlab="Survey Date", ylab="Proportion in Molt", ylim=c(0:1), lwd=2)
lines(lwr9$newday, lwr9$lwr, col="lightblue", lty="dotdash", lwd=3)

lines(upr10$newday, upr10$upr, col="grey", lwd=2)
lines(lwr10$newday, lwr10$lwr, col="grey", lty="dotdash", lwd=3)

lines(upr11$newday, upr11$upr, col="black",  lwd=2)
lines(lwr11$newday, lwr11$lwr, col="black", lty="dotdash", lwd=3)

lines(upr12$newday, upr12$upr, col="deeppink",  lwd=2)
lines(lwr12$newday, lwr12$lwr, col="deeppink", lty="dotdash", lwd=3)

lines(upr13$newday, upr13$upr, col="darkgreen",  lwd=2)
lines(lwr13$newday, lwr13$lwr, col="darkgreen", lty="dotdash", lwd=3)

lines(upr14$newday, upr14$upr, col="darkorange", lwd=2)
lines(lwr14$newday, lwr14$lwr, col="darkorange", lty="dotdash", lwd=3)

lines(upr15$newday, upr15$upr, col="plum",  lwd=2)
lines(lwr15$newday, lwr15$lwr, col="plum", lty="dotdash", lwd=3)

lines(upr16$newday, upr16$upr, col="bisque",  lwd=2)
lines(lwr16$newday, lwr16$lwr, col="bisque" , lty="dotdash", lwd=3)



#Now trying to extract peak 
gam2[which.max(gam2$predict.gam.yeargam..type....response..),]
upr2[which(upr2$newday=="254"),]
lwr2[which(lwr2$newday=="254"),]
gam3[which.max(gam3$predict.gam.yeargam..type....response..),]
upr3[which(upr3$newday=="270"),]
lwr3[which(lwr3$newday=="270"),]
gam4[which.max(gam4$predict.gam.yeargam..type....response..),]
upr4[which(upr4$newday=="250"),]
lwr4[which(lwr4$newday=="250"),]
gam5[which.max(gam5$predict.gam.yeargam..type....response..),]
upr5[which(upr5$newday=="284"),]
lwr5[which(lwr5$newday=="284"),]
gam6[which.max(gam6$predict.gam.yeargam..type....response..),]
upr6[which(upr6$newday=="273"),]
lwr6[which(lwr6$newday=="273"),]
gam7[which.max(gam7$predict.gam.yeargam..type....response..),]
upr7[which(upr7$newday=="280"),]
lwr7[which(lwr7$newday=="280"),]
gam8[which.max(gam8$predict.gam.yeargam..type....response..),]
upr8[which(upr8$newday=="243"),]
lwr8[which(lwr8$newday=="243"),]
gam9[which.max(gam9$predict.gam.yeargam..type....response..),]
upr9[which(upr9$newday=="250"),]
lwr9[which(lwr9$newday=="250"),]
gam10[which.max(gam10$predict.gam.yeargam..type....response..),]
upr10[which(upr10$newday=="275"),]
lwr10[which(lwr10$newday=="275"),]
gam11[which.max(gam11$predict.gam.yeargam..type....response..),]
upr11[which(upr11$newday=="266"),]
lwr11[which(lwr11$newday=="266"),]
gam12[which.max(gam12$predict.gam.yeargam..type....response..),]
upr12[which(upr12$newday=="266"),]
lwr12[which(lwr12$newday=="266"),]
gam13[which.max(gam13$predict.gam.yeargam..type....response..),]
upr13[which(upr13$newday=="249"),]
lwr13[which(lwr13$newday=="249"),]
gam14[which.max(gam14$predict.gam.yeargam..type....response..),]
upr14[which(upr14$newday=="250"),]
lwr14[which(lwr14$newday=="250"),]
gam15[which.max(gam15$predict.gam.yeargam..type....response..),]
upr15[which(upr15$newday=="273"),]
lwr15[which(lwr15$newday=="273"),]
gam16[which.max(gam16$predict.gam.yeargam..type....response..),]
upr16[which(upr16$newday=="268"),]
lwr16[which(lwr16$newday=="268"),]
#newgam<- data.frame(predict.gam(ngam, type="response"))

# Doing sideways CI
library("coenocliner")
newdd<- as.matrix(newd)

two$yday <- yday(two$Survey.Date)
twogam<-gam(molt.size~s(newday), family="binomial", data= two, gamma=6)

twodd <- as.matrix(two)
twoXp <- predict(object=twogam, model=two, type="lpmatrix") ## map coefs to fitted curves
twobeta <- coef(twogam)
twoVb   <- vcov(twogam)
n <- 10000
library("MASS") ## for mvrnorm
set.seed(10)
twomrand <- mvrnorm(n, twobeta, twoVb)
twoopt <- rep(NA, n)
twoilink <- family(twogam)$linkinv
for (i in seq_len(n)) { 
  twopred   <-twoilink(twoXp %*% twomrand[i, ])
  twoopt[i] <- two$yday[which.max(twopred)]
}
gam2$day[which.max(gam2$predict.gam.yeargam..type....response..)]
twoci <- quantile(twoopt, c(.05,.95), na.rm = TRUE) ## get 95% CI
(twoci)

three$yday <- yday(three$Survey.Date)
threegam<-gam(molt.size~s(newday), family="binomial", data= three, gamma=6)

threedd <- as.matrix(three)
threeXp <- predict(object=threegam, model=three, type="lpmatrix") ## map coefs to fitted curves
threebeta <- coef(threegam)
threeVb   <- vcov(threegam)
n <- 10000
library("MASS") ## for mvrnorm
set.seed(10)
threemrand <- mvrnorm(n, threebeta, threeVb)
threeopt <- rep(NA, n)
threeilink <- family(threegam)$linkinv
for (i in seq_len(n)) { 
  threepred   <-threeilink(threeXp %*% threemrand[i, ])
  threeopt[i] <- three$yday[which.max(threepred)]
}
gam3$day[which.max(gam3$predict.gam.yeargam..type....response..)]
threeci <- quantile(threeopt, c(.05,.95), na.rm = TRUE) ## get 95% CI
(threeci)

four$yday <- yday(four$Survey.Date)
fourgam<-gam(molt.size~s(newday), family="binomial", data= four, gamma=6)

fourdd <- as.matrix(four)
fourXp <- predict(object=fourgam, model=four, type="lpmatrix") ## map coefs to fitted curves
fourbeta <- coef(fourgam)
fourVb   <- vcov(fourgam)
n <- 10000
library("MASS") ## for mvrnorm
set.seed(10)
fourmrand <- mvrnorm(n, fourbeta, fourVb)
fouropt <- rep(NA, n)
fourilink <- family(fourgam)$linkinv
for (i in seq_len(n)) { 
  fourpred   <-fourilink(fourXp %*% fourmrand[i, ])
  fouropt[i] <- four$yday[which.max(fourpred)]
}
gam4$day[which.max(gam4$predict.gam.yeargam..type....response..)]
fourci <- quantile(fouropt, c(.05,.95), na.rm = TRUE) ## get 95% CI
(fourci)


five$yday <- yday(five$Survey.Date)
fivegam<-gam(molt.size~s(newday), family="binomial", data= five, gamma=6)

fivedd <- as.matrix(five)
fiveXp <- predict(object=fivegam, model=five, type="lpmatrix") ## map coefs to fitted curves
fivebeta <- coef(fivegam)
fiveVb   <- vcov(fivegam)
n <- 10000
library("MASS") ## for mvrnorm
set.seed(10)
fivemrand <- mvrnorm(n, fivebeta, fiveVb)
fiveopt <- rep(NA, n)
fiveilink <- family(fivegam)$linkinv
for (i in seq_len(n)) { 
  fivepred   <-fiveilink(fiveXp %*% fivemrand[i, ])
  fiveopt[i] <- five$yday[which.max(fivepred)]
}
gam5$day[which.max(gam5$predict.gam.yeargam..type....response..)]
fiveci <- quantile(fiveopt, c(.05,.95), na.rm = TRUE) ## get 95% CI
(fiveci)

six$yday <- yday(six$Survey.Date)
sixgam<-gam(molt.size~s(newday), family="binomial", data= six, gamma=6)

sixdd <- as.matrix(six)
sixXp <- predict(object=sixgam, model=six, type="lpmatrix") ## map coefs to fitted curves
sixbeta <- coef(sixgam)
sixVb   <- vcov(sixgam)
n <- 10000
library("MASS") ## for mvrnorm
set.seed(10)
sixmrand <- mvrnorm(n, sixbeta, sixVb)
sixopt <- rep(NA, n)
sixilink <- family(sixgam)$linkinv
for (i in seq_len(n)) { 
  sixpred   <-sixilink(sixXp %*% sixmrand[i, ])
  sixopt[i] <- six$yday[which.max(sixpred)]
}
gam6$day[which.max(gam6$predict.gam.yeargam..type....response..)]
sixci <- quantile(sixopt, c(.05,.95), na.rm = TRUE) ## get 95% CI
(sixci)

seven$yday <- yday(seven$Survey.Date)
sevengam<-gam(molt.size~s(newday), family="binomial", data= seven, gamma=6)

sevendd <- as.matrix(seven)
sevenXp <- predict(object=sevengam, model=seven, type="lpmatrix") ## map coefs to fitted curves
sevenbeta <- coef(sevengam)
sevenVb   <- vcov(sevengam)
n <- 10000
library("MASS") ## for mvrnorm
set.seed(10)
sevenmrand <- mvrnorm(n, sevenbeta, sevenVb)
sevenopt <- rep(NA, n)
sevenilink <- family(sevengam)$linkinv
for (i in seq_len(n)) { 
  sevenpred   <- sevenilink(sevenXp %*% sevenmrand[i, ])
  sevenopt[i] <- seven$yday[which.max(sevenpred)]
}
gam7$day[which.max(gam7$predict.gam.yeargam..type....response..)]
sevenci <- quantile(sevenopt, c(.05,.95), na.rm = TRUE) ## get 95% CI
(sevenci)

eight$yday <- yday(eight$Survey.Date)
eightgam<-gam(molt.size~s(newday), family="binomial", data= eight, gamma=6)

eightdd <- as.matrix(eight)
eightXp <- predict(object=eightgam, model=eight, type="lpmatrix") ## map coefs to fitted curves
eightbeta <- coef(eightgam)
eightVb   <- vcov(eightgam)
n <- 10000
library("MASS") ## for mvrnorm
set.seed(10)
eightmrand <- mvrnorm(n, eightbeta, eightVb)
eightopt <- rep(NA, n)
eightilink <- family(eightgam)$linkinv
for (i in seq_len(n)) { 
  eightpred   <- eightilink(eightXp %*% eightmrand[i, ])
  eightopt[i] <- eight$yday[which.max(eightpred)]
}
gam8$day[which.max(gam8$predict.gam.yeargam..type....response..)]
eightci <- quantile(eightopt, c(.05,.95), na.rm = TRUE) ## get 95% CI
(eightci)

nine$yday <- yday(nine$Survey.Date)
ninegam<-gam(molt.size~s(newday), family="binomial", data= nine, gamma=6)

ninedd <- as.matrix(nine)
nineXp <- predict(object=ninegam, model=nine, type="lpmatrix") ## map coefs to fitted curves
ninebeta <- coef(ninegam)
nineVb   <- vcov(ninegam)
n <- 10000
library("MASS") ## for mvrnorm
set.seed(10)
ninemrand <- mvrnorm(n, ninebeta, nineVb)
nineopt <- rep(NA, n)
nineilink <- family(ninegam)$linkinv
for (i in seq_len(n)) { 
  ninepred   <- nineilink(nineXp %*% ninemrand[i, ])
  nineopt[i] <- nine$yday[which.max(ninepred)]
}
gam9$day[which.max(gam9$predict.gam.yeargam..type....response..)]
nineci <- quantile(nineopt, c(.05,.95), na.rm = TRUE) ## get 95% CI
(nineci)

ten$yday <- yday(ten$Survey.Date)
tengam<-gam(molt.size~s(newday), family="binomial", data= ten, gamma=6)

tendd <- as.matrix(ten)
tenXp <- predict(object=tengam, model=ten, type="lpmatrix") ## map coefs to fitted curves
tenbeta <- coef(tengam)
tenVb   <- vcov(tengam)
n <- 10000
library("MASS") ## for mvrnorm
set.seed(10)
tenmrand <- mvrnorm(n, tenbeta, tenVb)
tenopt <- rep(NA, n)
tenilink <- family(tengam)$linkinv
for (i in seq_len(n)) { 
  tenpred   <- tenilink(tenXp %*% tenmrand[i, ])
  tenopt[i] <- ten$yday[which.max(tenpred)]
}
gam10$day[which.max(gam10$predict.gam.yeargam..type....response..)]
tenci <- quantile(tenopt, c(.05,.95), na.rm = TRUE) ## get 95% CI
(tenci)

eleven$yday <- yday(eleven$Survey.Date)
elevengam<-gam(molt.size~s(newday), family="binomial", data= eleven, gamma=6)

elevendd <- as.matrix(eleven)
elevenXp <- predict(object=elevengam, model=eleven, type="lpmatrix") ## map coefs to fitted curves
elevenbeta <- coef(elevengam)
elevenVb   <- vcov(elevengam)
n <- 10000
library("MASS") ## for mvrnorm
set.seed(10)
elevenmrand <- mvrnorm(n, elevenbeta, elevenVb)
elevenopt <- rep(NA, n)
elevenilink <- family(elevengam)$linkinv
for (i in seq_len(n)) { 
  elevenpred   <- elevenilink(elevenXp %*% elevenmrand[i, ])
  elevenopt[i] <- eleven$yday[which.max(elevenpred)]
}
gam11$day[which.max(gam11$predict.gam.yeargam..type....response..)]
elevenci <- quantile(elevenopt, c(.05,.95), na.rm = TRUE) ## get 95% CI
(elevenci)

twelve$yday <- yday(twelve$Survey.Date)
twelvegam<-gam(molt.size~s(newday), family="binomial", data= twelve, gamma=6)

twelvedd <- as.matrix(twelve)
twelveXp <- predict(object=twelvegam, model=twelve, type="lpmatrix") ## map coefs to fitted curves
twelvebeta <- coef(twelvegam)
twelveVb   <- vcov(twelvegam)
n <- 10000
library("MASS") ## for mvrnorm
set.seed(10)
twelvemrand <- mvrnorm(n, twelvebeta, twelveVb)
twelveopt <- rep(NA, n)
twelveilink <- family(twelvegam)$linkinv
for (i in seq_len(n)) { 
  twelvepred   <- twelveilink(twelveXp %*% twelvemrand[i, ])
  twelveopt[i] <- twelve$yday[which.max(twelvepred)]
}
gam12$day[which.max(gam12$predict.gam.yeargam..type....response..)]
twelveci <- quantile(twelveopt, c(.05,.95), na.rm = TRUE) ## get 95% CI
(twelveci)


thirteen$yday <- yday(thirteen$Survey.Date)
gam13$day[which.max(gam13$predict.gam.yeargam..type....response..)]

thirteengam<-gam(molt.size~s(newday), family="binomial", data= thirteen, gamma=6)

thirteendd <- as.matrix(thirteen)
thirteenXp <- predict(object=thirteengam, model=thirteen, type="lpmatrix") ## map coefs to fitted curves
thirteenbeta <- coef(thirteengam)
thirteenVb   <- vcov(thirteengam)
n <- 10000
library("MASS") ## for mvrnorm
set.seed(10)
thirteenmrand <- mvrnorm(n, thirteenbeta, thirteenVb)
thirteenopt <- rep(NA, n)
thirteenilink <- family(thirteengam)$linkinv
for (i in seq_len(n)) { 
  thirteenpred   <- thirteenilink(thirteenXp %*% thirteenmrand[i, ])
  thirteenopt[i] <- thirteen$yday[which.max(thirteenpred)]
}
gam13$day[which.max(gam13$predict.gam.yeargam..type....response..)]
thirteenci <- quantile(thirteenopt, c(.05,.95), na.rm = TRUE) ## get 95% CI
(thirteenci)

fourteen$yday <- yday(fourteen$Survey.Date)
gam14$day[which.max(gam14$predict.gam.yeargam..type....response..)]
fourteengam<-gam(molt.size~s(newday), family="binomial", data= fourteen, gamma=6)

fourteendd <- as.matrix(fourteen)
fourteenXp <- predict(object=fourteengam, model=fourteen, type="lpmatrix") ## map coefs to fitted curves
fourteenbeta <- coef(fourteengam)
fourteenVb   <- vcov(fourteengam)
n <- 10000
library("MASS") ## for mvrnorm
set.seed(10)
fourteenmrand <- mvrnorm(n, fourteenbeta, fourteenVb)
fourteenopt <- rep(NA, n)
fourteenilink <- family(fourteengam)$linkinv
for (i in seq_len(n)) { 
  fourteenpred   <- fourteenilink(fourteenXp %*% fourteenmrand[i, ])
  fourteenopt[i] <- fourteen$yday[which.max(fourteenpred)]
}
gam14$day[which.max(gam14$predict.gam.yeargam..type....response..)]
fourteenci <- quantile(fourteenopt, c(.05,.95), na.rm = TRUE) ## get 95% CI
(fourteenci)


fifteen$yday <- yday(fifteen$Survey.Date)
gam15$day[which.max(gam15$predict.gam.yeargam..type....response..)]

fifteengam<-gam(molt.size~s(newday), family="binomial", data= fifteen, gamma=6)

fifteendd <- as.matrix(fifteen)
fifteenXp <- predict(object=fifteengam, model=fifteen, type="lpmatrix") ## map coefs to fitted curves
fifteenbeta <- coef(fifteengam)
fifteenVb   <- vcov(fifteengam)
n <- 10000
library("MASS") ## for mvrnorm
set.seed(10)
fifteenmrand <- mvrnorm(n, fifteenbeta, fifteenVb)
fifteenopt <- rep(NA, n)
fifteenilink <- family(fifteengam)$linkinv
for (i in seq_len(n)) { 
  fifteenpred   <- fifteenilink(fifteenXp %*% fifteenmrand[i, ])
  fifteenopt[i] <- fifteen$yday[which.max(fifteenpred)]
  }
gam15$day[which.max(gam15$predict.gam.yeargam..type....response..)]
fifteenci <- quantile(fifteenopt, c(.05,.95), na.rm = TRUE) ## get 95% CI
(fifteenci)

sixteendd <- as.matrix(sixteen)
gam16$day[which.max(gam16$predict.gam.yeargam..type....response..)]
sixteenXp <- predict(object=sixteengam, model=sixteen, type="lpmatrix") ## map coefs to fitted curves
sixteenbeta <- coef(sixteengam)
sixteenVb   <- vcov(sixteengam)
n <- 10000
library("MASS") ## for mvrnorm
set.seed(10)
sixteenmrand <- mvrnorm(n, sixteenbeta, sixteenVb)
opt <- rep(NA, n)
sixteenilink <- family(sixteengam)$linkinv
for (i in seq_len(n)) { 
  pred   <- sixteenilink(sixteenXp %*% sixteenmrand[i, ])
  opt[i] <- sixteend$yday.sixteen.Survey.Date.[which.max(pred)]
}
gam16$day[which.max(gam16$predict.gam.yeargam..type....response..)]
sixteenci <- quantile(opt, c(.05,.95)) ## get 95% CI
(sixteenci)

# Finding peak heights
gam3$predict.gam.yeargam..type....response..[which.max(gam3$predict.gam.yeargam..type....response..)]
gam4$predict.gam.yeargam..type....response..[which.max(gam4$predict.gam.yeargam..type....response..)]
gam5$predict.gam.yeargam..type....response..[which.max(gam5$predict.gam.yeargam..type....response..)]
gam6$predict.gam.yeargam..type....response..[which.max(gam6$predict.gam.yeargam..type....response..)]
gam7$predict.gam.yeargam..type....response..[which.max(gam7$predict.gam.yeargam..type....response..)]
gam8$predict.gam.yeargam..type....response..[which.max(gam8$predict.gam.yeargam..type....response..)]
gam9$predict.gam.yeargam..type....response..[which.max(gam9$predict.gam.yeargam..type....response..)]
gam10$predict.gam.yeargam..type....response..[which.max(gam10$predict.gam.yeargam..type....response..)]
gam11$predict.gam.yeargam..type....response..[which.max(gam11$predict.gam.yeargam..type....response..)]
gam12$predict.gam.yeargam..type....response..[which.max(gam12$predict.gam.yeargam..type....response..)]
gam13$predict.gam.yeargam..type....response..[which.max(gam13$predict.gam.yeargam..type....response..)]
gam14$predict.gam.yeargam..type....response..[which.max(gam14$predict.gam.yeargam..type....response..)]
gam15$predict.gam.yeargam..type....response..[which.max(gam15$predict.gam.yeargam..type....response..)]
gam16$predict.gam.yeargam..type....response..[which.max(gam16$predict.gam.yeargam..type....response..)]



## Variables for GLM
sprtrns <- read.csv ("./murrepredictors/spr_trns.csv")
peakht <- read.csv ("../Dependent variables/pk_heights.csv")
peakloc <- read.csv ("../Dependent variables/peakloc.csv")
stpht <- read.csv ("../Dependent variables/st_pht.csv")

glmpkloc <- glm(peakloc$DOY.PE~Spr.Trs,data=sprtrns,family=gaussian())
summary(glmpkloc) # display results
confint(glmpkloc) # 95% CI for the coefficients
exp(coef(glmpkloc)) # exponentiated coefficients
exp(confint(glmpkloc)) # 95% CI for exponentiated coefficients
predict(glmpkloc, type="response") # predicted values
residuals(glmpkloc, type="deviance") # residuals
anova(glmpkloc, test="Chisq")
library(car)
influencePlot(glmpkloc)
res <- residuals(glmpkloc, type="deviance")
plot(log(predict(glmpkloc)), res)
abline(h=0, lty=2)
qqnorm(res)
qqline(res)

glmstpht <- glm(peakht$Peak.Height~Spr.Trs,data=sprtrns,family=gaussian())
summary(glmstpht) # display results
confint(glmstpht) # 95% CI for the coefficients
exp(coef(glmstpht)) # exponentiated coefficients
exp(confint(glmstpht)) # 95% CI for exponentiated coefficients
predict(glmstpht, type="response") # predicted values
residuals(glmstpht, type="deviance") # residuals
anova(glmstpht, test="Chisq")
library(car)
influencePlot(glmstpht)
res <- residuals(glmstpht, type="deviance")
plot(log(predict(glmstpht)), res)
abline(h=0, lty=2)
qqnorm(res)
qqline(res)
++++++++++++++++++++++++++++++++++++++++
  library(lubridate)
# Code for processing and producing wave/storm indices

# read in datafiles and replace NA
setwd("./murrepredictors/Storm indices/")
s46029 <- read.csv(file="S46029.csv")
s46029$WVHT[s46029$WVHT > 90] <- NA
s46029$WVHT[s46029$WVHT < 0.25] <- NA		# removes very low values that are likely errors
s46041 <- read.csv(file="S46041.csv")
s46041$WVHT[s46041$WVHT > 90] <- NA
s46041$WVHT[s46041$WVHT < 0.25] <- NA		# removes very low values that are likely errors
setwd("../..")

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


waveindex<- Wave.index.fun (month.include=c(7,8,9,10), wave.threshold=3, event.minlength=6)
waveindex <- waveindex[-c(1), ]
# New GLMS
glmpht2 <- glm(peakht$Peak.Height~Spr.Trs+waveindex$Ave.Hsig+waveindex$Prop.Hsig+waveindex$Nevent.Hsig,data=sprtrns,family=gaussian())
summary(glmpht2) # display results
confint(glmpht2) # 95% CI for the coefficients
exp(coef(glmpht2)) # exponentiated coefficients
exp(confint(glmpht2)) # 95% CI for exponentiated coefficients
predict(glmpht2, type="response") # predicted values
residuals(glmpht2, type="deviance") # residuals
anova(glmpht2, test="Chisq")
library(car)
influencePlot(glmpht2)
res <- residuals(glmpht2, type="deviance")
plot(log(predict(glmpht2)), res)
abline(h=0, lty=2)
qqnorm(res)
qqline(res)

glmloc2 <- glm(peakloc$DOY.PE~Spr.Trs+waveindex$Ave.Hsig+waveindex$Prop.Hsig+waveindex$Nevent.Hsig,data=sprtrns,family=gaussian())
summary(glmloc2) # display results
confint(glmloc2) # 95% CI for the coefficients
exp(coef(glmloc2)) # exponentiated coefficients
exp(confint(glmloc2)) # 95% CI for exponentiated coefficients
predict(glmloc2, type="response") # predicted values
residuals(glmloc2, type="deviance") # residuals
anova(glmloc2, test="Chisq")
library(car)
influencePlot(glmloc2)
res <- residuals(glmloc2, type="deviance")
plot(log(predict(glmloc2)), res)
abline(h=0, lty=2)
qqnorm(res)
qqline(res)

coadultswa <- coadults[which (coadults$Region == "San Juan" |
                                coadults$Region == "Puget Sound" |
                                coadults$Region == "Strait" |
                                coadults$Region == "North Coast"
                                ),]
coadultsorca <- coadults[which (!(coadults$Region == "San Juan" |
                                coadults$Region == "Puget Sound" |
                                coadults$Region == "Strait" |
                                coadults$Region == "North Coast"
                                )),]


coadultswa1 <- coadultswa[which (coadultswa$molt.size=="1"),]
comuwa1ct<- tapply(X=coadultswa1$X.1, INDEX= coadultswa1$Year, FUN=length)
coadultswa0 <- coadultswa[which (coadultswa$molt.size=="0"),]
comuwa0ct<- tapply(X=coadultswa0$X.1, INDEX= coadultswa0$Year, FUN=length)

coadultsorca1 <- coadultsorca[which (coadultsorca$molt.size=="1"),]
comuorca1ct<- tapply(X=coadultsorca1$X.1, INDEX= coadultsorca1$Year, FUN=length)
coadultsorca0 <- coadultsorca[which (coadultsorca$molt.size=="0"),]
comuorca0ct<- tapply(X=coadultsorca0$X.1, INDEX= coadultsorca0$Year, FUN=length)
(comuwa1ct)
(comuwa0ct)
(comuorca1ct)
(comuorca0ct)

allwa<- data.frame(tapply(coadultswa$X.1, list(coadultswa$Month, coadultswa$Year), length))
allorca<- data.frame(tapply(coadultsorca$X.1, list(coadultsorca$Month, coadultsorca$Year), length))

#Add in upwelling indices
febcumup <- read.csv ("./murrepredictors/Cum Upwelling Indices/cum_upfeb_may.csv", header=TRUE)
juncumup <- read.csv ("./murrepredictors/Cum Upwelling Indices/cum_upjun_aug.csv", header=TRUE)

#Add in weights
glmwt <- read.csv ("../Dependent variables/PeakHtPeakLocWts.csv")

library(MuMIn)

glmcumupht <- glm(peakht$Peak.Height~Spr.Trs+waveindex$Ave.Hsig+waveindex$Prop.Hsig+waveindex$Nevent.Hsig+febcumup$Upwelling.Index+juncumup$Upwelling.index,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
summary(glmpht2) # display results
confint(glmpht2) # 95% CI for the coefficients
exp(coef(glmpht2)) # exponentiated coefficients
exp(confint(glmpht2)) # 95% CI for exponentiated coefficients
predict(glmpht2, type="response") # predicted values
residuals(glmpht2, type="deviance") # residuals
anova(glmcumupht, test="Chisq")
library(car)
influencePlot(glmpht2)
res <- residuals(glmpht2, type="deviance")
plot(log(predict(glmpht2)), res)
abline(h=0, lty=2)
qqnorm(res)
qqline(res)

library(qpcR)

#Set 1- AvgH and ST
htavgst1 <- glm(peakht$Peak.Height~1,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S1M1<- AICc(htavgst1)

htavgst2 <- glm(peakht$Peak.Height~waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S1M2<- AICc(htavgst2)

htavgst3 <- glm(peakht$Peak.Height~Spr.Trs,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S1M3 <-AICc(htavgst3)

htavgst4 <- glm(peakht$Peak.Height~Spr.Trs*waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S1M4 <-AICc(htavgst4)

htavgst5 <- glm(peakht$Peak.Height~Spr.Trs+waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S1M5<- AICc(htavgst5)

htavgst6 <- glm(peakht$Peak.Height~Spr.Trs*waveindex$Ave.Hsig+waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S1M6 <-AICc(htavgst6)

htavgst7 <- glm(peakht$Peak.Height~Spr.Trs*waveindex$Ave.Hsig+Spr.Trs,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S1M7 <- AICc(htavgst7)

htavgst8 <- glm(peakht$Peak.Height~Spr.Trs*waveindex$Ave.Hsig+Spr.Trs+waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S1M8 <-AICc(htavgst8)

S1M1 + S1M2+ S1M3+ S1M4+ S1M5+ S1M6 + S1M7 + S1M8
htaic <- unlist(list(S1M1, S1M2, S1M3, S1M4, S1M5, S1M6, S1M7, S1M8))
htaicweights <- akaike.weights(htaic)
(htaicweights$weights)
#Winner winner chicken dinner

#Set 2- AvgH and Feb
htavgfeb1 <- glm(peakht$Peak.Height~1,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S2M1<- AICc(htavgfeb1)

htavgfeb2 <- glm(peakht$Peak.Height~waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S2M2 <- AICc(htavgfeb2)

htavgfeb3 <- glm(peakht$Peak.Height~febcumup$Upwelling.Index,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S2M3 <-AICc(htavgfeb3)

htavgfeb4 <- glm(peakht$Peak.Height~febcumup$Upwelling.Index*waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S2M4 <-AICc(htavgfeb4)

htavgfeb5 <- glm(peakht$Peak.Height~febcumup$Upwelling.Index+waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S2M5 <-AICc(htavgfeb5)

htavgfeb6 <- glm(peakht$Peak.Height~febcumup$Upwelling.Index*waveindex$Ave.Hsig+waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S2M6 <-AICc(htavgfeb6)

htavgfeb7 <- glm(peakht$Peak.Height~febcumup$Upwelling.Index*waveindex$Ave.Hsig+febcumup$Upwelling.Index,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S2M7 <-AICc(htavgfeb7)

htavgfeb8 <- glm(peakht$Peak.Height~febcumup$Upwelling.Index*waveindex$Ave.Hsig+febcumup$Upwelling.Index+waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S2M8 <- AICc(htavgfeb8)

S2M1+ S2M2 + S2M3+ S2M4 + S2M5+ S2M6+ S2M7+ S2M8

#Set 3- AvgH and Jun
htavgjun1 <- glm(peakht$Peak.Height~1,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S3M1 <- AICc(htavgjun1)

htavgjun2 <- glm(peakht$Peak.Height~waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S3M2 <- AICc(htavgjun2)

htavgjun3 <- glm(peakht$Peak.Height~juncumup$Upwelling.index,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S3M3 <- AICc(htavgjun3)

htavgjun4 <- glm(peakht$Peak.Height~juncumup$Upwelling.index*waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S3M4 <- AICc(htavgjun4)

htavgjun5 <- glm(peakht$Peak.Height~juncumup$Upwelling.index+waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S3M5 <- AICc(htavgjun5)

htavgjun6 <- glm(peakht$Peak.Height~juncumup$Upwelling.index*waveindex$Ave.Hsig+waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S3M6 <- AICc(htavgjun6)

htavgjun7 <- glm(peakht$Peak.Height~juncumup$Upwelling.index*waveindex$Ave.Hsig+juncumup$Upwelling.index,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S3M7 <- AICc(htavgjun7)

htavgjun8 <- glm(peakht$Peak.Height~juncumup$Upwelling.index*waveindex$Ave.Hsig+juncumup$Upwelling.index+waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S3M8 <- AICc(htavgjun8)

S3M1+ S3M2 + S3M3+ S3M4 + S3M5+ S3M6+ S3M7+ S3M8
#Just terrible
#Set 4- PropH and ST
htpropst1 <- glm(peakht$Peak.Height~1,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S4M1 <- AICc(htpropst1)

htpropst2 <- glm(peakht$Peak.Height~waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S4M2 <- AICc(htpropst2)

htpropst3 <- glm(peakht$Peak.Height~Spr.Trs,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S4M3 <- AICc(htpropst3)

htpropst4 <- glm(peakht$Peak.Height~Spr.Trs*waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S4M4 <- AICc(htpropst4)

htpropst5 <- glm(peakht$Peak.Height~Spr.Trs+waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S4M5 <- AICc(htpropst5)

htpropst6 <- glm(peakht$Peak.Height~Spr.Trs*waveindex$Prop.Hsig+waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S4M6 <- AICc(htpropst6)

htpropst7 <- glm(peakht$Peak.Height~Spr.Trs*waveindex$Prop.Hsig+Spr.Trs,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S4M7 <- AICc(htpropst7)

htpropst8 <- glm(peakht$Peak.Height~Spr.Trs*waveindex$Prop.Hsig+Spr.Trs+waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S4M8 <- AICc(htpropst8)

S4M1+ S4M2 + S4M3+ S4M4 + S4M5+ S4M6+ S4M7+ S4M8
#Out
#Set 5- PropH and Feb
htpropfeb1 <- glm(peakht$Peak.Height~1,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S5M1 <- AICc(htpropfeb1)

htpropfeb2 <- glm(peakht$Peak.Height~waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S5M2 <- AICc(htpropfeb2)

htpropfeb3 <- glm(peakht$Peak.Height~febcumup$Upwelling.Index,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S5M3 <- AICc(htpropfeb3)

htpropfeb4 <- glm(peakht$Peak.Height~febcumup$Upwelling.Index*waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S5M4 <- AICc(htpropfeb4)

htpropfeb5 <- glm(peakht$Peak.Height~febcumup$Upwelling.Index+waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S5M5 <- AICc(htpropfeb5)

htpropfeb6 <- glm(peakht$Peak.Height~febcumup$Upwelling.Index*waveindex$Prop.Hsig+waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S5M6 <- AICc(htpropfeb6)

htpropfeb7 <- glm(peakht$Peak.Height~febcumup$Upwelling.Index*waveindex$Prop.Hsig+febcumup$Upwelling.Index,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S5M7 <- AICc(htpropfeb7)

htpropfeb8 <- glm(peakht$Peak.Height~febcumup$Upwelling.Index*waveindex$Prop.Hsig+febcumup$Upwelling.Index+waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S5M8 <- AICc(htpropfeb8)

S5M1+ S5M2 + S5M3+ S5M4 + S5M5+ S5M6+ S5M7+ S5M8
#Out
#Set 6- PropH and Jun
htpropjun1 <- glm(peakht$Peak.Height~1,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S6M1 <- AICc(htpropjun1)

htpropjun2 <- glm(peakht$Peak.Height~waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S6M2 <- AICc(htpropjun2)

htpropjun3 <- glm(peakht$Peak.Height~juncumup$Upwelling.index,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S6M3 <- AICc(htpropjun3)

htpropjun4 <- glm(peakht$Peak.Height~juncumup$Upwelling.index*waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S6M4 <- AICc(htpropjun4)

htpropjun5 <- glm(peakht$Peak.Height~juncumup$Upwelling.index+waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S6M5 <- AICc(htpropjun5)

htpropjun6 <- glm(peakht$Peak.Height~juncumup$Upwelling.index*waveindex$Prop.Hsig+waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S6M6 <- AICc(htpropjun6)

htpropjun7 <- glm(peakht$Peak.Height~juncumup$Upwelling.index*waveindex$Prop.Hsig+juncumup$Upwelling.index,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S6M7 <- AICc(htpropjun7)

htpropjun8 <- glm(peakht$Peak.Height~juncumup$Upwelling.index*waveindex$Prop.Hsig+juncumup$Upwelling.index+waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S6M8 <- AICc(htpropjun8)

S6M1+ S6M2 + S6M3+ S6M4 + S6M5+ S6M6+ S6M7+ S6M8
#Just horrible

#Set 7- Nevents and ST
htneventst1 <- glm(peakht$Peak.Height~1,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S7M1 <- AICc(htneventst1)

htneventst2 <- glm(peakht$Peak.Height~waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S7M2 <- AICc(htneventst2)

htneventst3 <- glm(peakht$Peak.Height~Spr.Trs,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S7M3 <- AICc(htneventst3)

htneventst4 <- glm(peakht$Peak.Height~Spr.Trs*waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S7M4 <- AICc(htneventst4)

htneventst5 <- glm(peakht$Peak.Height~Spr.Trs+waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S7M5 <- AICc(htneventst5)

htneventst6 <- glm(peakht$Peak.Height~Spr.Trs*waveindex$Nevent.Hsig+waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S7M6 <- AICc(htneventst6)

htneventst7 <- glm(peakht$Peak.Height~Spr.Trs*waveindex$Nevent.Hsig+Spr.Trs,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S7M7 <- AICc(htneventst7)

htneventst8 <- glm(peakht$Peak.Height~Spr.Trs*waveindex$Nevent.Hsig+Spr.Trs+waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S7M8 <- AICc(htneventst8)

S7M1+ S7M2 + S7M3+ S7M4 + S7M5+ S7M6+ S7M7+ S7M8
#Set 8- Nevents and Feb
htneventfeb1 <- glm(peakht$Peak.Height~1,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S8M1 <- AICc(htneventfeb1)

htneventfeb2 <- glm(peakht$Peak.Height~waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S8M2 <- AICc(htneventfeb2)

htneventfeb3 <- glm(peakht$Peak.Height~febcumup$Upwelling.Index,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S8M3 <- AICc(htneventfeb3)

htneventfeb4 <- glm(peakht$Peak.Height~febcumup$Upwelling.Index*waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S8M4 <- AICc(htneventfeb4)

htneventfeb5 <- glm(peakht$Peak.Height~febcumup$Upwelling.Index+waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S8M5 <- AICc(htneventfeb5)

htneventfeb6 <- glm(peakht$Peak.Height~febcumup$Upwelling.Index*waveindex$Nevent.Hsig+waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S8M6 <- AICc(htneventfeb6)

htneventfeb7 <- glm(peakht$Peak.Height~febcumup$Upwelling.Index*waveindex$Nevent.Hsig+febcumup$Upwelling.Index,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S8M7 <- AICc(htneventfeb7)

htneventfeb8 <- glm(peakht$Peak.Height~febcumup$Upwelling.Index*waveindex$Nevent.Hsig+febcumup$Upwelling.Index+waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S8M8 <- AICc(htneventfeb8)

S8M1+ S8M2 + S8M3+ S8M4 + S8M5+ S8M6+ S8M7+ S8M8
#Set 9- Nevents and June
htneventjun1 <- glm(peakht$Peak.Height~1,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S9M1 <- AICc(htneventjun1)

htneventjun2 <- glm(peakht$Peak.Height~waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S9M2 <- AICc(htneventjun2)

htneventjun3 <- glm(peakht$Peak.Height~juncumup$Upwelling.index,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S9M3 <- AICc(htneventjun3)

htneventjun4 <- glm(peakht$Peak.Height~juncumup$Upwelling.index*waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S9M4 <- AICc(htneventjun4)

htneventjun5 <- glm(peakht$Peak.Height~juncumup$Upwelling.index+waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S9M5 <- AICc(htneventjun5)

htneventjun6 <- glm(peakht$Peak.Height~juncumup$Upwelling.index*waveindex$Nevent.Hsig+waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S9M6 <- AICc(htneventjun6)

htneventjun7 <- glm(peakht$Peak.Height~juncumup$Upwelling.index*waveindex$Nevent.Hsig+juncumup$Upwelling.index,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S9M7 <- AICc(htneventjun7)

htneventjun8 <- glm(peakht$Peak.Height~juncumup$Upwelling.index*waveindex$Nevent.Hsig+juncumup$Upwelling.index+waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Peak.Ht.Wt, family=gaussian())
S9M8 <- AICc(htneventjun8)

S9M1+ S9M2 + S9M3+ S9M4 + S9M5+ S9M6+ S9M7+ S9M8
###############################Peak location#############################################

glmloc <- glm(peakloc$DOY.PE~Spr.Trs+waveindex$Ave.Hsig+waveindex$Prop.Hsig+waveindex$Nevent.Hsig,data=sprtrns,family=gaussian())


#Set 1- AvgH and ST
locavgst1 <- glm(peakloc$DOY.PE~1,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S1M1<- AICc(locavgst1)

locavgst2 <- glm(peakloc$DOY.PE~waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S1M2<- AICc(locavgst2)

locavgst3 <- glm(peakloc$DOY.PE~Spr.Trs,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S1M3 <-AICc(locavgst3)

locavgst4 <- glm(peakloc$DOY.PE~Spr.Trs*waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S1M4 <-AICc(locavgst4)

locavgst5 <- glm(peakloc$DOY.PE~Spr.Trs+waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S1M5<- AICc(locavgst5)

locavgst6 <- glm(peakloc$DOY.PE~Spr.Trs*waveindex$Ave.Hsig+waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S1M6 <-AICc(locavgst6)

locavgst7 <- glm(peakloc$DOY.PE~Spr.Trs*waveindex$Ave.Hsig+Spr.Trs,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S1M7 <- AICc(locavgst7)

locavgst8 <- glm(peakloc$DOY.PE~Spr.Trs*waveindex$Ave.Hsig+Spr.Trs+waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S1M8 <-AICc(locavgst8)

S1M1 + S1M2+ S1M3+ S1M4+ S1M5+ S1M6 + S1M7 + S1M8
#960.8521

#Set 2- AvgH and Feb
locavgfeb1 <- glm(peakloc$DOY.PE~1,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S2M1<- AICc(locavgfeb1)

locavgfeb2 <- glm(peakloc$DOY.PE~waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S2M2 <- AICc(locavgfeb2)

locavgfeb3 <- glm(peakloc$DOY.PE~febcumup$Upwelling.Index,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S2M3 <-AICc(locavgfeb3)

locavgfeb4 <- glm(peakloc$DOY.PE~febcumup$Upwelling.Index*waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S2M4 <-AICc(locavgfeb4)

locavgfeb5 <- glm(peakloc$DOY.PE~febcumup$Upwelling.Index+waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S2M5 <-AICc(locavgfeb5)

locavgfeb6 <- glm(peakloc$DOY.PE~febcumup$Upwelling.Index*waveindex$Ave.Hsig+waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S2M6 <-AICc(locavgfeb6)

locavgfeb7 <- glm(peakloc$DOY.PE~febcumup$Upwelling.Index*waveindex$Ave.Hsig+febcumup$Upwelling.Index,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S2M7 <-AICc(locavgfeb7)

locavgfeb8 <- glm(peakloc$DOY.PE~febcumup$Upwelling.Index*waveindex$Ave.Hsig+febcumup$Upwelling.Index+waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S2M8 <- AICc(locavgfeb8)

S2M1+ S2M2 + S2M3+ S2M4 + S2M5+ S2M6+ S2M7+ S2M8
#966.0488
#Set 3- AvgH and Jun
locavgjun1 <- glm(peakloc$DOY.PE~1,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S3M1 <- AICc(locavgjun1)

locavgjun2 <- glm(peakloc$DOY.PE~waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S3M2 <- AICc(locavgjun2)

locavgjun3 <- glm(peakloc$DOY.PE~juncumup$Upwelling.index,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S3M3 <- AICc(locavgjun3)

locavgjun4 <- glm(peakloc$DOY.PE~juncumup$Upwelling.index*waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S3M4 <- AICc(locavgjun4)

locavgjun5 <- glm(peakloc$DOY.PE~juncumup$Upwelling.index+waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S3M5 <- AICc(locavgjun5)

locavgjun6 <- glm(peakloc$DOY.PE~juncumup$Upwelling.index*waveindex$Ave.Hsig+waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S3M6 <- AICc(locavgjun6)

locavgjun7 <- glm(peakloc$DOY.PE~juncumup$Upwelling.index*waveindex$Ave.Hsig+juncumup$Upwelling.index,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S3M7 <- AICc(locavgjun7)

locavgjun8 <- glm(peakloc$DOY.PE~juncumup$Upwelling.index*waveindex$Ave.Hsig+juncumup$Upwelling.index+waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S3M8 <- AICc(locavgjun8)

S3M1+ S3M2 + S3M3+ S3M4 + S3M5+ S3M6+ S3M7+ S3M8
#967.2444
#Set 4- PropH and ST
locpropst1 <- glm(peakloc$DOY.PE~1,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S4M1 <- AICc(locpropst1)

locpropst2 <- glm(peakloc$DOY.PE~waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S4M2 <- AICc(locpropst2)

locpropst3 <- glm(peakloc$DOY.PE~Spr.Trs,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S4M3 <- AICc(locpropst3)

locpropst4 <- glm(peakloc$DOY.PE~Spr.Trs*waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S4M4 <- AICc(locpropst4)

locpropst5 <- glm(peakloc$DOY.PE~Spr.Trs+waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S4M5 <- AICc(locpropst5)

locpropst6 <- glm(peakloc$DOY.PE~Spr.Trs*waveindex$Prop.Hsig+waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S4M6 <- AICc(locpropst6)

locpropst7 <- glm(peakloc$DOY.PE~Spr.Trs*waveindex$Prop.Hsig+Spr.Trs,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S4M7 <- AICc(locpropst7)

locpropst8 <- glm(peakloc$DOY.PE~Spr.Trs*waveindex$Prop.Hsig+Spr.Trs+waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S4M8 <- AICc(locpropst8)

S4M1+ S4M2 + S4M3+ S4M4 + S4M5+ S4M6+ S4M7+ S4M8
#972.0568
#Set 5- PropH and Feb
locpropfeb1 <- glm(peakloc$DOY.PE~1,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S5M1 <- AICc(locpropfeb1)

locpropfeb2 <- glm(peakloc$DOY.PE~waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S5M2 <- AICc(locpropfeb2)

locpropfeb3 <- glm(peakloc$DOY.PE~febcumup$Upwelling.Index,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S5M3 <- AICc(locpropfeb3)

locpropfeb4 <- glm(peakloc$DOY.PE~febcumup$Upwelling.Index*waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S5M4 <- AICc(locpropfeb4)

locpropfeb5 <- glm(peakloc$DOY.PE~febcumup$Upwelling.Index+waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S5M5 <- AICc(locpropfeb5)

locpropfeb6 <- glm(peakloc$DOY.PE~febcumup$Upwelling.Index*waveindex$Prop.Hsig+waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S5M6 <- AICc(locpropfeb6)

locpropfeb7 <- glm(peakloc$DOY.PE~febcumup$Upwelling.Index*waveindex$Prop.Hsig+febcumup$Upwelling.Index,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S5M7 <- AICc(locpropfeb7)

locpropfeb8 <- glm(peakloc$DOY.PE~febcumup$Upwelling.Index*waveindex$Prop.Hsig+febcumup$Upwelling.Index+waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S5M8 <- AICc(locpropfeb8)

S5M1+ S5M2 + S5M3+ S5M4 + S5M5+ S5M6+ S5M7+ S5M8
#976.0663
#Set 6- PropH and Jun
locpropjun1 <- glm(peakloc$DOY.PE~1,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S6M1 <- AICc(locpropjun1)

locpropjun2 <- glm(peakloc$DOY.PE~waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S6M2 <- AICc(locpropjun2)

locpropjun3 <- glm(peakloc$DOY.PE~juncumup$Upwelling.index,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S6M3 <- AICc(locpropjun3)

locpropjun4 <- glm(peakloc$DOY.PE~juncumup$Upwelling.index*waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S6M4 <- AICc(locpropjun4)

locpropjun5 <- glm(peakloc$DOY.PE~juncumup$Upwelling.index+waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S6M5 <- AICc(locpropjun5)

locpropjun6 <- glm(peakloc$DOY.PE~juncumup$Upwelling.index*waveindex$Prop.Hsig+waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S6M6 <- AICc(locpropjun6)

locpropjun7 <- glm(peakloc$DOY.PE~juncumup$Upwelling.index*waveindex$Prop.Hsig+juncumup$Upwelling.index,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S6M7 <- AICc(locpropjun7)

locpropjun8 <- glm(peakloc$DOY.PE~juncumup$Upwelling.index*waveindex$Prop.Hsig+juncumup$Upwelling.index+waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S6M8 <- AICc(locpropjun8)

S6M1+ S6M2 + S6M3+ S6M4 + S6M5+ S6M6+ S6M7+ S6M8
#959.8331

#Set 7- Nevents and ST
locneventst1 <- glm(peakloc$DOY.PE~1,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S7M1 <- AICc(locneventst1)

locneventst2 <- glm(peakloc$DOY.PE~waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S7M2 <- AICc(locneventst2)

locneventst3 <- glm(peakloc$DOY.PE~Spr.Trs,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S7M3 <- AICc(locneventst3)

locneventst4 <- glm(peakloc$DOY.PE~Spr.Trs*waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S7M4 <- AICc(locneventst4)

locneventst5 <- glm(peakloc$DOY.PE~Spr.Trs+waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S7M5 <- AICc(locneventst5)

locneventst6 <- glm(peakloc$DOY.PE~Spr.Trs*waveindex$Nevent.Hsig+waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S7M6 <- AICc(locneventst6)

locneventst7 <- glm(peakloc$DOY.PE~Spr.Trs*waveindex$Nevent.Hsig+Spr.Trs,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S7M7 <- AICc(locneventst7)

locneventst8 <- glm(peakloc$DOY.PE~Spr.Trs*waveindex$Nevent.Hsig+Spr.Trs+waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S7M8 <- AICc(locneventst8)

S7M1+ S7M2 + S7M3+ S7M4 + S7M5+ S7M6+ S7M7+ S7M8
locaic <- unlist(list(S7M1, S7M2, S7M3, S7M4, S7M5, S7M6, S7M7, S7M8))
locaicweights <- akaike.weights(locaic)
(locaicweights$weights)
#964.5711
#winner
#Set 8- Nevents and Feb
locneventfeb1 <- glm(peakloc$DOY.PE~1,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S8M1 <- AICc(locneventfeb1)

locneventfeb2 <- glm(peakloc$DOY.PE~waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S8M2 <- AICc(locneventfeb2)

locneventfeb3 <- glm(peakloc$DOY.PE~febcumup$Upwelling.Index,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S8M3 <- AICc(locneventfeb3)

locneventfeb4 <- glm(peakloc$DOY.PE~febcumup$Upwelling.Index*waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S8M4 <- AICc(locneventfeb4)

locneventfeb5 <- glm(peakloc$DOY.PE~febcumup$Upwelling.Index+waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S8M5 <- AICc(locneventfeb5)

locneventfeb6 <- glm(peakloc$DOY.PE~febcumup$Upwelling.Index*waveindex$Nevent.Hsig+waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S8M6 <- AICc(locneventfeb6)

locneventfeb7 <- glm(peakloc$DOY.PE~febcumup$Upwelling.Index*waveindex$Nevent.Hsig+febcumup$Upwelling.Index,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S8M7 <- AICc(locneventfeb7)

locneventfeb8 <- glm(peakloc$DOY.PE~febcumup$Upwelling.Index*waveindex$Nevent.Hsig+febcumup$Upwelling.Index+waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S8M8 <- AICc(locneventfeb8)

S8M1+ S8M2 + S8M3+ S8M4 + S8M5+ S8M6+ S8M7+ S8M8
#977.0908
#Set 9- Nevents and June
locneventjun1 <- glm(peakloc$DOY.PE~1,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S9M1 <- AICc(locneventjun1)

locneventjun2 <- glm(peakloc$DOY.PE~waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S9M2 <- AICc(locneventjun2)

locneventjun3 <- glm(peakloc$DOY.PE~juncumup$Upwelling.index,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S9M3 <- AICc(locneventjun3)

locneventjun4 <- glm(peakloc$DOY.PE~juncumup$Upwelling.index*waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S9M4 <- AICc(locneventjun4)

locneventjun5 <- glm(peakloc$DOY.PE~juncumup$Upwelling.index+waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S9M5 <- AICc(locneventjun5)

locneventjun6 <- glm(peakloc$DOY.PE~juncumup$Upwelling.index*waveindex$Nevent.Hsig+waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S9M6 <- AICc(locneventjun6)

locneventjun7 <- glm(peakloc$DOY.PE~juncumup$Upwelling.index*waveindex$Nevent.Hsig+juncumup$Upwelling.index,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())
S9M7 <- AICc(locneventjun7)

locneventjun8 <- glm(peakloc$DOY.PE~juncumup$Upwelling.index*waveindex$Nevent.Hsig+juncumup$Upwelling.index+waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Peak.Loc.Wt, family=gaussian())

S9M1+ S9M2 + S9M3+ S9M4 + S9M5+ S9M6+ S9M7+ S9M8

############################################Duration##################################################################

duration <- read.csv ("../Dependent variables/duration.csv")

glmdur <- glm(duration$Duration~Spr.Trs+waveindex$Ave.Hsig+waveindex$Prop.Hsig+waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())

#Set 1- AvgH and ST
duravgst1 <- glm(duration$Duration~1,weights=glmwt$Dur.Wt, family=gaussian())
S1M1<- AICc(duravgst1)

duravgst2 <- glm(duration$Duration~waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S1M2<- AICc(duravgst2)

duravgst3 <- glm(duration$Duration~Spr.Trs,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S1M3 <-AICc(duravgst3)

duravgst4 <- glm(duration$Duration~Spr.Trs*waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S1M4 <-AICc(duravgst4)

duravgst5 <- glm(duration$Duration~Spr.Trs+waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S1M5<- AICc(duravgst5)

duravgst6 <- glm(duration$Duration~Spr.Trs*waveindex$Ave.Hsig+waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S1M6 <-AICc(duravgst6)

duravgst7 <- glm(duration$Duration~Spr.Trs*waveindex$Ave.Hsig+Spr.Trs,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S1M7 <- AICc(duravgst7)

duravgst8 <- glm(duration$Duration~Spr.Trs*waveindex$Ave.Hsig+Spr.Trs+waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S1M8 <-AICc(duravgst8)

S1M1 + S1M2+ S1M3+ S1M4+ S1M5+ S1M6 + S1M7 + S1M8
#1086.713
duraic <- unlist(list(S1M1, S1M2, S1M3, S1M4, S1M5, S1M6, S1M7, S1M8))
duraicweights<- akaike.weights(duraic)
(duraicweights$weights)
# Winner winner
#Set 2- AvgH and Feb
duravgfeb1 <- glm(duration$Duration~1,weights=glmwt$Dur.Wt, family=gaussian())
S2M1<- AICc(duravgfeb1)

duravgfeb2 <- glm(duration$Duration~waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S2M2 <- AICc(duravgfeb2)

duravgfeb3 <- glm(duration$Duration~febcumup$Upwelling.Index,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S2M3 <-AICc(duravgfeb3)

duravgfeb4 <- glm(duration$Duration~febcumup$Upwelling.Index*waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S2M4 <-AICc(duravgfeb4)

duravgfeb5 <- glm(duration$Duration~febcumup$Upwelling.Index+waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S2M5 <-AICc(duravgfeb5)

duravgfeb6 <- glm(duration$Duration~febcumup$Upwelling.Index*waveindex$Ave.Hsig+waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S2M6 <-AICc(duravgfeb6)

duravgfeb7 <- glm(duration$Duration~febcumup$Upwelling.Index*waveindex$Ave.Hsig+febcumup$Upwelling.Index,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S2M7 <-AICc(duravgfeb7)

duravgfeb8 <- glm(duration$Duration~febcumup$Upwelling.Index*waveindex$Ave.Hsig+febcumup$Upwelling.Index+waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S2M8 <- AICc(duravgfeb8)

S2M1+ S2M2 + S2M3+ S2M4 + S2M5+ S2M6+ S2M7+ S2M8
#1095.7
#Set 3- AvgH and Jun
duravgjun1 <- glm(duration$Duration~1,weights=glmwt$Dur.Wt, family=gaussian())
S3M1 <- AICc(duravgjun1)

duravgjun2 <- glm(duration$Duration~waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S3M2 <- AICc(duravgjun2)

duravgjun3 <- glm(duration$Duration~juncumup$Upwelling.index,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S3M3 <- AICc(duravgjun3)

duravgjun4 <- glm(duration$Duration~juncumup$Upwelling.index*waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S3M4 <- AICc(duravgjun4)

duravgjun5 <- glm(duration$Duration~juncumup$Upwelling.index+waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S3M5 <- AICc(duravgjun5)

duravgjun6 <- glm(duration$Duration~juncumup$Upwelling.index*waveindex$Ave.Hsig+waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S3M6 <- AICc(duravgjun6)

duravgjun7 <- glm(duration$Duration~juncumup$Upwelling.index*waveindex$Ave.Hsig+juncumup$Upwelling.index,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S3M7 <- AICc(duravgjun7)

duravgjun8 <- glm(duration$Duration~juncumup$Upwelling.index*waveindex$Ave.Hsig+juncumup$Upwelling.index+waveindex$Ave.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S3M8 <- AICc(duravgjun8)

S3M1+ S3M2 + S3M3+ S3M4 + S3M5+ S3M6+ S3M7+ S3M8
#1086.001

#Set 4- PropH and ST
durpropst1 <- glm(duration$Duration~1,weights=glmwt$Dur.Wt, family=gaussian())
S4M1 <- AICc(durpropst1)

durpropst2 <- glm(duration$Duration~waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S4M2 <- AICc(durpropst2)

durpropst3 <- glm(duration$Duration~Spr.Trs,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S4M3 <- AICc(durpropst3)

durpropst4 <- glm(duration$Duration~Spr.Trs*waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S4M4 <- AICc(durpropst4)

durpropst5 <- glm(duration$Duration~Spr.Trs+waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S4M5 <- AICc(durpropst5)

durpropst6 <- glm(duration$Duration~Spr.Trs*waveindex$Prop.Hsig+waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S4M6 <- AICc(durpropst6)

durpropst7 <- glm(duration$Duration~Spr.Trs*waveindex$Prop.Hsig+Spr.Trs,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S4M7 <- AICc(durpropst7)

durpropst8 <- glm(duration$Duration~Spr.Trs*waveindex$Prop.Hsig+Spr.Trs+waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S4M8 <- AICc(durpropst8)

S4M1+ S4M2 + S4M3+ S4M4 + S4M5+ S4M6+ S4M7+ S4M8
#1088.91
#Set 5- PropH and Feb
durpropfeb1 <- glm(duration$Duration~1,weights=glmwt$Dur.Wt, family=gaussian())
S5M1 <- AICc(durpropfeb1)

durpropfeb2 <- glm(duration$Duration~waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S5M2 <- AICc(durpropfeb2)

durpropfeb3 <- glm(duration$Duration~febcumup$Upwelling.Index,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S5M3 <- AICc(durpropfeb3)

durpropfeb4 <- glm(duration$Duration~febcumup$Upwelling.Index*waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S5M4 <- AICc(durpropfeb4)

durpropfeb5 <- glm(duration$Duration~febcumup$Upwelling.Index+waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S5M5 <- AICc(durpropfeb5)

durpropfeb6 <- glm(duration$Duration~febcumup$Upwelling.Index*waveindex$Prop.Hsig+waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S5M6 <- AICc(durpropfeb6)

durpropfeb7 <- glm(duration$Duration~febcumup$Upwelling.Index*waveindex$Prop.Hsig+febcumup$Upwelling.Index,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S5M7 <- AICc(durpropfeb7)

durpropfeb8 <- glm(duration$Duration~febcumup$Upwelling.Index*waveindex$Prop.Hsig+febcumup$Upwelling.Index+waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S5M8 <- AICc(durpropfeb8)

S5M1+ S5M2 + S5M3+ S5M4 + S5M5+ S5M6+ S5M7+ S5M8
#1101.386
#Set 6- PropH and Jun
durpropjun1 <- glm(duration$Duration~1,weights=glmwt$Dur.Wt, family=gaussian())
S6M1 <- AICc(durpropjun1)

durpropjun2 <- glm(duration$Duration~waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S6M2 <- AICc(durpropjun2)

durpropjun3 <- glm(duration$Duration~juncumup$Upwelling.index,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S6M3 <- AICc(durpropjun3)

durpropjun4 <- glm(duration$Duration~juncumup$Upwelling.index*waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S6M4 <- AICc(durpropjun4)

durpropjun5 <- glm(duration$Duration~juncumup$Upwelling.index+waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S6M5 <- AICc(durpropjun5)

durpropjun6 <- glm(duration$Duration~juncumup$Upwelling.index*waveindex$Prop.Hsig+waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S6M6 <- AICc(durpropjun6)

durpropjun7 <- glm(duration$Duration~juncumup$Upwelling.index*waveindex$Prop.Hsig+juncumup$Upwelling.index,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S6M7 <- AICc(durpropjun7)

durpropjun8 <- glm(duration$Duration~juncumup$Upwelling.index*waveindex$Prop.Hsig+juncumup$Upwelling.index+waveindex$Prop.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S6M8 <- AICc(durpropjun8)

S6M1+ S6M2 + S6M3+ S6M4 + S6M5+ S6M6+ S6M7+ S6M8
#1096.183

#Set 7- Nevents and ST
durneventst1 <- glm(duration$Duration~1,weights=glmwt$Dur.Wt, family=gaussian())
S7M1 <- AICc(durneventst1)

durneventst2 <- glm(duration$Duration~waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S7M2 <- AICc(durneventst2)

durneventst3 <- glm(duration$Duration~Spr.Trs,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S7M3 <- AICc(durneventst3)

durneventst4 <- glm(duration$Duration~Spr.Trs*waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S7M4 <- AICc(durneventst4)

durneventst5 <- glm(duration$Duration~Spr.Trs+waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S7M5 <- AICc(durneventst5)

durneventst6 <- glm(duration$Duration~Spr.Trs*waveindex$Nevent.Hsig+waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S7M6 <- AICc(durneventst6)

durneventst7 <- glm(duration$Duration~Spr.Trs*waveindex$Nevent.Hsig+Spr.Trs,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S7M7 <- AICc(durneventst7)

durneventst8 <- glm(duration$Duration~Spr.Trs*waveindex$Nevent.Hsig+Spr.Trs+waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S7M8 <- AICc(durneventst8)

S7M1+ S7M2 + S7M3+ S7M4 + S7M5+ S7M6+ S7M7+ S7M8
#1093.285
#Set 8- Nevents and Feb
durneventfeb1 <- glm(duration$Duration~1,weights=glmwt$Dur.Wt, family=gaussian())
S8M1 <- AICc(durneventfeb1)

durneventfeb2 <- glm(duration$Duration~waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S8M2 <- AICc(durneventfeb2)

durneventfeb3 <- glm(duration$Duration~febcumup$Upwelling.Index,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S8M3 <- AICc(durneventfeb3)

durneventfeb4 <- glm(duration$Duration~febcumup$Upwelling.Index*waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S8M4 <- AICc(durneventfeb4)

durneventfeb5 <- glm(duration$Duration~febcumup$Upwelling.Index+waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S8M5 <- AICc(durneventfeb5)

durneventfeb6 <- glm(duration$Duration~febcumup$Upwelling.Index*waveindex$Nevent.Hsig+waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S8M6 <- AICc(durneventfeb6)

durneventfeb7 <- glm(duration$Duration~febcumup$Upwelling.Index*waveindex$Nevent.Hsig+febcumup$Upwelling.Index,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S8M7 <- AICc(durneventfeb7)

durneventfeb8 <- glm(duration$Duration~febcumup$Upwelling.Index*waveindex$Nevent.Hsig+febcumup$Upwelling.Index+waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S8M8 <- AICc(durneventfeb8)

S8M1+ S8M2 + S8M3+ S8M4 + S8M5+ S8M6+ S8M7+ S8M8
#1099.328
#Set 9- Nevents and June
durneventjun1 <- glm(duration$Duration~1,weights=glmwt$Dur.Wt, family=gaussian())
S9M1 <- AICc(durneventjun1)

durneventjun2 <- glm(duration$Duration~waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S9M2 <- AICc(durneventjun2)

durneventjun3 <- glm(duration$Duration~juncumup$Upwelling.index,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S9M3 <- AICc(durneventjun3)

durneventjun4 <- glm(duration$Duration~juncumup$Upwelling.index*waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S9M4 <- AICc(durneventjun4)

durneventjun5 <- glm(duration$Duration~juncumup$Upwelling.index+waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S9M5 <- AICc(durneventjun5)

durneventjun6 <- glm(duration$Duration~juncumup$Upwelling.index*waveindex$Nevent.Hsig+waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S9M6 <- AICc(durneventjun6)

durneventjun7 <- glm(duration$Duration~juncumup$Upwelling.index*waveindex$Nevent.Hsig+juncumup$Upwelling.index,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S9M7 <- AICc(durneventjun7)

durneventjun8 <- glm(duration$Duration~juncumup$Upwelling.index*waveindex$Nevent.Hsig+juncumup$Upwelling.index+waveindex$Nevent.Hsig,data=sprtrns,weights=glmwt$Dur.Wt, family=gaussian())
S9M8 <- AICc(durneventjun8)

S9M1+ S9M2 + S9M3+ S9M4 + S9M5+ S9M6+ S9M7+ S9M8
#1094.24

# Bring in encounter rate data
encrates <- read.csv ("../Dependent variables/encrates.csv")

glmenc <- glm(encrates$Sum~Spr.Trs+waveindex$Ave.Hsig+waveindex$Prop.Hsig+waveindex$Nevent.Hsig,data=sprtrns,family=gaussian())


#Set 1- AvgH and ST
encavgst1 <- glm(encrates$Sum~1,weights = glmwt$Er.Wt, family=gaussian())
S1M1<- AICc(encavgst1)

encavgst2 <- glm(encrates$Sum~waveindex$Ave.Hsig,data=sprtrns,weights = glmwt$Er.Wt, family=gaussian())
S1M2<- AICc(encavgst2)

encavgst3 <- glm(encrates$Sum~Spr.Trs,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S1M3 <-AICc(encavgst3)

encavgst4 <- glm(encrates$Sum~Spr.Trs*waveindex$Ave.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S1M4 <-AICc(encavgst4)

encavgst5 <- glm(encrates$Sum~Spr.Trs+waveindex$Ave.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S1M5<- AICc(encavgst5)

encavgst6 <- glm(encrates$Sum~Spr.Trs*waveindex$Ave.Hsig+waveindex$Ave.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S1M6 <-AICc(encavgst6)

encavgst7 <- glm(encrates$Sum~Spr.Trs*waveindex$Ave.Hsig+Spr.Trs,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S1M7 <- AICc(encavgst7)

encavgst8 <- glm(encrates$Sum~Spr.Trs*waveindex$Ave.Hsig+Spr.Trs+waveindex$Ave.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S1M8 <-AICc(encavgst8)

S1M1 + S1M2+ S1M3+ S1M4+ S1M5+ S1M6 + S1M7 + S1M8
#616.0646

#Set 2- AvgH and Feb
encavgfeb1 <- glm(encrates$Sum~1, weights = glmwt$Er.Wt, family=gaussian())
S2M1<- AICc(encavgfeb1)

encavgfeb2 <- glm(encrates$Sum~waveindex$Ave.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S2M2 <- AICc(encavgfeb2)

encavgfeb3 <- glm(encrates$Sum~febcumup$Upwelling.Index,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S2M3 <-AICc(encavgfeb3)

encavgfeb4 <- glm(encrates$Sum~febcumup$Upwelling.Index*waveindex$Ave.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S2M4 <-AICc(encavgfeb4)

encavgfeb5 <- glm(encrates$Sum~febcumup$Upwelling.Index+waveindex$Ave.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S2M5 <-AICc(encavgfeb5)

encavgfeb6 <- glm(encrates$Sum~febcumup$Upwelling.Index*waveindex$Ave.Hsig+waveindex$Ave.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S2M6 <-AICc(encavgfeb6)

encavgfeb7 <- glm(encrates$Sum~febcumup$Upwelling.Index*waveindex$Ave.Hsig+febcumup$Upwelling.Index,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S2M7 <-AICc(encavgfeb7)

encavgfeb8 <- glm(encrates$Sum~febcumup$Upwelling.Index*waveindex$Ave.Hsig+febcumup$Upwelling.Index+waveindex$Ave.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S2M8 <- AICc(encavgfeb8)

S2M1+ S2M2 + S2M3+ S2M4 + S2M5+ S2M6+ S2M7+ S2M8
#619.5071
#Set 3- AvgH and Jun
encavgjun1 <- glm(encrates$Sum~1, weights = glmwt$Er.Wt, family=gaussian())
S3M1 <- AICc(encavgjun1)

encavgjun2 <- glm(encrates$Sum~waveindex$Ave.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S3M2 <- AICc(encavgjun2)

encavgjun3 <- glm(encrates$Sum~juncumup$Upwelling.index,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S3M3 <- AICc(encavgjun3)

encavgjun4 <- glm(encrates$Sum~juncumup$Upwelling.index*waveindex$Ave.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S3M4 <- AICc(encavgjun4)

encavgjun5 <- glm(encrates$Sum~juncumup$Upwelling.index+waveindex$Ave.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S3M5 <- AICc(encavgjun5)

encavgjun6 <- glm(encrates$Sum~juncumup$Upwelling.index*waveindex$Ave.Hsig+waveindex$Ave.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S3M6 <- AICc(encavgjun6)

encavgjun7 <- glm(encrates$Sum~juncumup$Upwelling.index*waveindex$Ave.Hsig+juncumup$Upwelling.index,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S3M7 <- AICc(encavgjun7)

encavgjun8 <- glm(encrates$Sum~juncumup$Upwelling.index*waveindex$Ave.Hsig+juncumup$Upwelling.index+waveindex$Ave.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S3M8 <- AICc(encavgjun8)

S3M1+ S3M2 + S3M3+ S3M4 + S3M5+ S3M6+ S3M7+ S3M8
#631.5018
#Set 4- PropH and ST
encpropst1 <- glm(encrates$Sum~1, weights = glmwt$Er.Wt, family=gaussian())
S4M1 <- AICc(encpropst1)

encpropst2 <- glm(encrates$Sum~waveindex$Prop.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S4M2 <- AICc(encpropst2)

encpropst3 <- glm(encrates$Sum~Spr.Trs,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S4M3 <- AICc(encpropst3)

encpropst4 <- glm(encrates$Sum~Spr.Trs*waveindex$Prop.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S4M4 <- AICc(encpropst4)

encpropst5 <- glm(encrates$Sum~Spr.Trs+waveindex$Prop.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S4M5 <- AICc(encpropst5)

encpropst6 <- glm(encrates$Sum~Spr.Trs*waveindex$Prop.Hsig+waveindex$Prop.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S4M6 <- AICc(encpropst6)

encpropst7 <- glm(encrates$Sum~Spr.Trs*waveindex$Prop.Hsig+Spr.Trs,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S4M7 <- AICc(encpropst7)

encpropst8 <- glm(encrates$Sum~Spr.Trs*waveindex$Prop.Hsig+Spr.Trs+waveindex$Prop.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S4M8 <- AICc(encpropst8)

S4M1+ S4M2 + S4M3+ S4M4 + S4M5+ S4M6+ S4M7+ S4M8
#610.4789
#Set 5- PropH and Feb
encpropfeb1 <- glm(encrates$Sum~1, weights = glmwt$Er.Wt, family=gaussian())
S5M1 <- AICc(encpropfeb1)

encpropfeb2 <- glm(encrates$Sum~waveindex$Prop.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S5M2 <- AICc(encpropfeb2)

encpropfeb3 <- glm(encrates$Sum~febcumup$Upwelling.Index,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S5M3 <- AICc(encpropfeb3)

encpropfeb4 <- glm(encrates$Sum~febcumup$Upwelling.Index*waveindex$Prop.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S5M4 <- AICc(encpropfeb4)

encpropfeb5 <- glm(encrates$Sum~febcumup$Upwelling.Index+waveindex$Prop.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S5M5 <- AICc(encpropfeb5)

encpropfeb6 <- glm(encrates$Sum~febcumup$Upwelling.Index*waveindex$Prop.Hsig+waveindex$Prop.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S5M6 <- AICc(encpropfeb6)

encpropfeb7 <- glm(encrates$Sum~febcumup$Upwelling.Index*waveindex$Prop.Hsig+febcumup$Upwelling.Index,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S5M7 <- AICc(encpropfeb7)

encpropfeb8 <- glm(encrates$Sum~febcumup$Upwelling.Index*waveindex$Prop.Hsig+febcumup$Upwelling.Index+waveindex$Prop.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S5M8 <- AICc(encpropfeb8)

S5M1+ S5M2 + S5M3+ S5M4 + S5M5+ S5M6+ S5M7+ S5M8
#608.4354
#Set 6- PropH and Jun
encpropjun1 <- glm(encrates$Sum~1, weights = glmwt$Er.Wt, family=gaussian())
S6M1 <- AICc(encpropjun1)

encpropjun2 <- glm(encrates$Sum~waveindex$Prop.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S6M2 <- AICc(encpropjun2)

encpropjun3 <- glm(encrates$Sum~juncumup$Upwelling.index,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S6M3 <- AICc(encpropjun3)

encpropjun4 <- glm(encrates$Sum~juncumup$Upwelling.index*waveindex$Prop.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S6M4 <- AICc(encpropjun4)

encpropjun5 <- glm(encrates$Sum~juncumup$Upwelling.index+waveindex$Prop.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S6M5 <- AICc(encpropjun5)

encpropjun6 <- glm(encrates$Sum~juncumup$Upwelling.index*waveindex$Prop.Hsig+waveindex$Prop.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S6M6 <- AICc(encpropjun6)

encpropjun7 <- glm(encrates$Sum~juncumup$Upwelling.index*waveindex$Prop.Hsig+juncumup$Upwelling.index,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S6M7 <- AICc(encpropjun7)

encpropjun8 <- glm(encrates$Sum~juncumup$Upwelling.index*waveindex$Prop.Hsig+juncumup$Upwelling.index+waveindex$Prop.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S6M8 <- AICc(encpropjun8)

S6M1+ S6M2 + S6M3+ S6M4 + S6M5+ S6M6+ S6M7+ S6M8
#631.2861

#Set 7- Nevents and ST
encceventst1 <- glm(encrates$Sum~1, weights = glmwt$Er.Wt, family=gaussian())
S7M1 <- AICc(encceventst1)

encceventst2 <- glm(encrates$Sum~waveindex$Nevent.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S7M2 <- AICc(encceventst2)

encceventst3 <- glm(encrates$Sum~Spr.Trs,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S7M3 <- AICc(encceventst3)

encceventst4 <- glm(encrates$Sum~Spr.Trs*waveindex$Nevent.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S7M4 <- AICc(encceventst4)

encceventst5 <- glm(encrates$Sum~Spr.Trs+waveindex$Nevent.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S7M5 <- AICc(encceventst5)

encceventst6 <- glm(encrates$Sum~Spr.Trs*waveindex$Nevent.Hsig+waveindex$Nevent.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S7M6 <- AICc(encceventst6)

encceventst7 <- glm(encrates$Sum~Spr.Trs*waveindex$Nevent.Hsig+Spr.Trs,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S7M7 <- AICc(encceventst7)

encceventst8 <- glm(encrates$Sum~Spr.Trs*waveindex$Nevent.Hsig+Spr.Trs+waveindex$Nevent.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S7M8 <- AICc(encceventst8)

S7M1+ S7M2 + S7M3+ S7M4 + S7M5+ S7M6+ S7M7+ S7M8
#591.3047
encaic <- unlist(list(S7M1, S7M2, S7M3, S7M4, S7M5, S7M6, S7M7, S7M8))
encaicweights <- akaike.weights(encaic)
(encaicweights$weights)
#Winner
#Set 8- Nevents and Feb
encceventfeb1 <- glm(encrates$Sum~1, weights = glmwt$Er.Wt, family=gaussian())
S8M1 <- AICc(encceventfeb1)

encceventfeb2 <- glm(encrates$Sum~waveindex$Nevent.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S8M2 <- AICc(encceventfeb2)

encceventfeb3 <- glm(encrates$Sum~febcumup$Upwelling.Index,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S8M3 <- AICc(encceventfeb3)

encceventfeb4 <- glm(encrates$Sum~febcumup$Upwelling.Index*waveindex$Nevent.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S8M4 <- AICc(encceventfeb4)

encceventfeb5 <- glm(encrates$Sum~febcumup$Upwelling.Index+waveindex$Nevent.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S8M5 <- AICc(encceventfeb5)

encceventfeb6 <- glm(encrates$Sum~febcumup$Upwelling.Index*waveindex$Nevent.Hsig+waveindex$Nevent.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S8M6 <- AICc(encceventfeb6)

encceventfeb7 <- glm(encrates$Sum~febcumup$Upwelling.Index*waveindex$Nevent.Hsig+febcumup$Upwelling.Index,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S8M7 <- AICc(encceventfeb7)

encceventfeb8 <- glm(encrates$Sum~febcumup$Upwelling.Index*waveindex$Nevent.Hsig+febcumup$Upwelling.Index+waveindex$Nevent.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S8M8 <- AICc(encceventfeb8)

S8M1+ S8M2 + S8M3+ S8M4 + S8M5+ S8M6+ S8M7+ S8M8
#600.6794
#Set 9- Nevents and June
encceventjun1 <- glm(encrates$Sum~1, weights = glmwt$Er.Wt, family=gaussian())
S9M1 <- AICc(encceventjun1)

encceventjun2 <- glm(encrates$Sum~waveindex$Nevent.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S9M2 <- AICc(encceventjun2)

encceventjun3 <- glm(encrates$Sum~juncumup$Upwelling.index,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S9M3 <- AICc(encceventjun3)

encceventjun4 <- glm(encrates$Sum~juncumup$Upwelling.index*waveindex$Nevent.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S9M4 <- AICc(encceventjun4)

encceventjun5 <- glm(encrates$Sum~juncumup$Upwelling.index+waveindex$Nevent.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S9M5 <- AICc(encceventjun5)

encceventjun6 <- glm(encrates$Sum~juncumup$Upwelling.index*waveindex$Nevent.Hsig+waveindex$Nevent.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S9M6 <- AICc(encceventjun6)

encceventjun7 <- glm(encrates$Sum~juncumup$Upwelling.index*waveindex$Nevent.Hsig+juncumup$Upwelling.index,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S9M7 <- AICc(encceventjun7)

encceventjun8 <- glm(encrates$Sum~juncumup$Upwelling.index*waveindex$Nevent.Hsig+juncumup$Upwelling.index+waveindex$Nevent.Hsig,data=sprtrns, weights = glmwt$Er.Wt, family=gaussian())
S9M8 <- AICc(encceventjun8)

S9M1+ S9M2 + S9M3+ S9M4 + S9M5+ S9M6+ S9M7+ S9M8
#631.2323
