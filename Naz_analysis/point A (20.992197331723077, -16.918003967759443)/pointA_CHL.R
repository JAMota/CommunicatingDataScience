require(TSA)
library(dplyr)

#########################################################################
# This file performs a Dominant Frequency State Analysis 
# over Chlorophyll concentration data.
#
# The DFSA Method is defined in: 
# Bruun, J. T., Icarus Allen, J., and Smyth, T. J. ( 2017), 
# Heartbeat of the Southern Oscillation explains ENSO climatic resonances, 
# J. Geophys. Res. Oceans, 122, 6746� 6772, doi:10.1002/2017JC012892.
# Copyright 2017. The Authors.
#########################################################################
# code is modified by Nazeera Meedin
#######################################################################
# Read in Chlorophyll data
file <- paste ("dataCHLA_OCCCI-v50.csv")
resid <- scan (file,what="raw",sep=",",skip=1)
resid.n <- length(resid)/3
dim(resid) <- c(3,resid.n)# dimension array matrix

#########################################################
# Model set-up construction
#Concentration data model set up-  
# 1st and 2nd - Noise term
# 3rd Term- constant
# others related to frequency harmonicas that will introduce later

mod1.sel <- c(NA,0,NA,NA,NA,NA,NA,NA,NA,NA,NA,0,0,NA)
	

##########################################################
#
nchoose <- 3   
#Concentration data column


############################################################################
# Create the time stamp and time series record for analysis
#
# This includes one way of dealing with small values that.
# creating time series object 
boxmean.ts <- ((ts(as.numeric(resid[nchoose,]),start=c(1998,01),end=c(2021,12),frequency=12)))
month.start <- start(boxmean.ts)[2]
idx.n <- seq(1,length(boxmean.ts))
idx.n.use <- idx.n[boxmean.ts <= 0]  #  identify the zeros
idx.tn.use <- idx.n[boxmean.ts > 0] 
# try replacement of zeros with a relevant small number...
boxmean.ts[idx.n.use] <- 10^mean(log10(boxmean.ts[idx.tn.use]))   # replace with min of actual recorded data
#
#boxmean.ts[idx.n.use] <- mean.adjust[nchoose]   # replace with min of actual recorded data
boxmean.ts <- log10(boxmean.ts)
head(boxmean.ts, 12)
#
boxmean2.ts <- boxmean.ts
boxmean2.ts[idx.n.use] <- NA   # replace with min of actual recorded data
#############################################################################


#############################################################################
# Set up Dominant Frequency State Analysis parameters
# to model inputs of
#   a) 6 month cycle 
#   b) 1 year cycle
#   c) a linear time trend 
xtu <<- boxmean.ts 
order.arima <- c(1,0,0)          #  AR(1)
order.sarima <- c(1,0,0)         #  SAR(1)
t.seq <- seq(0,1-(1/length(xtu)),1/length(xtu))  # linear trend component
# Harmonic terms
# ts.tm <- seq(1,length(xtu))
ts.tm <- seq(month.start,length(xtu)+(month.start-1))
# basic terms
f1 <- 1/(6)    #6 months
h.c1 <- cos(2*pi*f1*ts.tm)
h.s1 <- sin(2*pi*f1*ts.tm)
f2 <- 1/(12)    #1 year
h.c2 <- cos(2*pi*f2*ts.tm)
h.s2 <- sin(2*pi*f2*ts.tm)
f3 <- 1/(12*10)    #10 year
h.c3 <- cos(2*pi*f3*ts.tm)
h.s3 <- sin(2*pi*f3*ts.tm)
f4 <- 1/(4)    #4 month
h.c4 <- cos(2*pi*f4*ts.tm)
h.s4 <- sin(2*pi*f4*ts.tm)
f5 <- 1/(3)    #3 month
h.c5 <- cos(2*pi*f5*ts.tm)
h.s5 <- sin(2*pi*f5*ts.tm)

# modified terms with 5 , 6, 25,12 years
# add one by one in model set up and see the difference 
f6 <- 1/(12*5)    #5 year
h.c6 <- cos(2*pi*f6*ts.tm)
h.s6 <- sin(2*pi*f6*ts.tm)
f7 <- 1/(12*6)    #6 year
h.c7 <- cos(2*pi*f7*ts.tm)
h.s7 <- sin(2*pi*f7*ts.tm)
f8 <- 1/(12*12)    #12 year
h.c8 <- cos(2*pi*f8*ts.tm)
h.s8 <- sin(2*pi*f8*ts.tm)
f9 <- 1/(12*25)    #25 year
h.c9 <- cos(2*pi*f9*ts.tm)
h.s9 <- sin(2*pi*f9*ts.tm)

# correct it tomorrow

# Set up data /  parameter settings
drivers.df <- data.frame(h.c1,h.s1,h.c2,h.s2,h.c3,h.s3,h.c4,h.s4,h.c5,h.s5,t.seq)  # add / remove relevant inputs
drivers.order <- list(c(0,0),c(0,0),c(0,0),c(0,0),c(0,0),c(0,0),c(0,0),c(0,0),c(0,0),c(0,0),c(0,0)) # (a,b) a= AR order, b= MA order (-1 none)
#
# Set the parameter fields to be estimated
fixedB.pars <- mod1.sel
#  fixedB.pars <- mod1.sel
# This is for phytoplankton. Change the above if you select another plankton
# type: e.g. mod2.sel for Diatoms

# Estimate the frequency states
transferfunction.model <- 
  arimax(xtu,order=order.arima,seasonal=list(order=order.sarima,period=12),
         xtransf=drivers.df,transfer=drivers.order,fixed =fixedB.pars,method='ML')

transferfunction.model
transferfunction.model.fit <- fitted(transferfunction.model)     # fitted values
transferfunction.model.res <- residuals(transferfunction.model)  #  residual values
# 
#summary(transferfunction.mode1)                          # for run time diagnostics if needed
rsquared <- 1 - (transferfunction.model$sigma)/var(xtu)   #
rsquared                                                  # R^2 value
# print out 95% Confidence Intervals of parameters
coefs2s <- cbind(coefficients(transferfunction.model),confint(transferfunction.model,,level=0.95))  #plot coef & 95% CI
coefs2s

##############################################################################
#Extract the Annual model harmonic cycle
tsC.tm <- seq(1/52,12,length.out=52)
f1 <- 1/(6)    #6 months
hc.c1 <- cos(2*pi*f1*tsC.tm)
hc.s1 <- sin(2*pi*f1*tsC.tm)
f2 <- 1/(12)    #12 months
hc.c2 <- cos(2*pi*f2*tsC.tm)
hc.s2 <- sin(2*pi*f2*tsC.tm)
f4 <- 1/(4)    #4 month
hc.c4 <- cos(2*pi*f4*tsC.tm)
hc.s4 <- sin(2*pi*f4*tsC.tm)
f5 <- 1/(3)    #3 month
hc.c5 <- cos(2*pi*f5*tsC.tm)
hc.s5 <- sin(2*pi*f5*tsC.tm)

mean.level <- mean(boxmean.ts)
tot.climatology <- coefs2s[4,1]*hc.c1 + coefs2s[5,1]*hc.s1 + coefs2s[6,1]*hc.c2 + coefs2s[7,1]*hc.s2 + coefs2s[10,1]*hc.c4 + coefs2s[11,1]*hc.s4 + coefs2s[12,1]*hc.c5 + coefs2s[13,1]*hc.s5 + mean.level

####################################################################################
# Plot commands
#jpeg(file = "Phytoplankton.jpeg")
#windows()
# Either save to file or use interactive mode. 
# Note for reports - save R plots to jpeg and insert as a picture into your
# report. 

# Graphical parameters
par(mar= c(2,4,4,2), cex=1.0)
layout(matrix(c(1,2,3,3),2,2,byrow=TRUE))

# plot climatology
titletext <- paste('Climatology : \n Chlorophyll Concentration') 
plot(tsC.tm,10^tot.climatology,type='n',log='y',xlab='Month',ylab='Surface CHL concentration [mg / m3]',main=titletext)
lines(tsC.tm,10^tot.climatology,lwd=2)
#######################################################################
## Spectrum : to identify harmonic terms
##  examine the harmonic cycles with the Periodogram : a frequency spectrum tool
#
tot.r.pdA <-  periodogram(boxmean.ts,log='no',main="Spectral properties",plot=FALSE)
1/tot.r.pdA$freq[tot.r.pdA$spec== max(tot.r.pdA$spec)] # the periodicity of the largest spectral peak tot.r
plot(tot.r.pdA$freq,tot.r.pdA$spec,type="h",log="x",lwd=2,xlab='Frequency = 1/Months',
     ylab='Spectrum',main="Harmonic terms")
abline(v=1/12, lwd=0.5,col=2,lty=2)
abline(v=2/12, lwd=0.5,col=2,lty=2)
abline(v=3/12, lwd=0.5,col=2,lty=2)
abline(v=1/(12*5), lwd=0.5,col=2,lty=2)
#abline(v=1/(12*20), lwd=0.5,col=2,lty=2)
abline(v=1/(12*6), lwd=0.5,col="blue",lty=2)
abline(v=1/(12*25), lwd=0.5,col="blue",lty=2)
abline(v=1/(12*12), lwd=0.5,col="blue",lty=2)
#abline(v=1/(12*2), lwd=0.5,col="blue",lty=2)
#abline(v=2.5/(12), lwd=0.5,col="blue",lty=2)
#
# plot fitted model 
ts.plot(10^as.numeric(boxmean2.ts), 10^transferfunction.model.fit,
        gpars=list(xlab="",ylab="CHL concentration",main="Raw data (black line). Transfer function model (blue)",log='y',lwd=c(1,2.3), cex=1.1,cex.main=1.2,lty=c(1,1),col=c(1,4)))
#
#dev.off()
###############################################################################

####################################################################################
# Plot commands
#jpeg(file = "Phytoplankton.jpeg")
#windows()
# Either save to file or use interactive mode. 
# Note for reports - save R plots to jpeg and insert as a picture into your
# report. 

# Graphical parameters
par(mar= c(2,4,4,2), cex=1.0)
layout(matrix(c(1,1,2,2),2,2,byrow=TRUE))

## plot climatology
#titletext <- paste('Climatology : \n Phytoplankton') 
#plot(tsC.tm,10^tot.climatology,type='n',log='y',xlab='Month',ylab='Cells [# / ml]',main=titletext)
#lines(tsC.tm,10^tot.climatology,lwd=2)
#######################################################################
## Spectrum : to identify harmonic terms
##  examine the harmonic cycles with the Periodogram : a frequency spectrum tool
tot.r.pdA <-  periodogram(transferfunction.model.res,log='no',main="Spectral properties",plot=FALSE)
1/tot.r.pdA$freq[tot.r.pdA$spec== max(tot.r.pdA$spec)] # the periodicity of the largest spectral peak tot.r
plot(tot.r.pdA$freq,tot.r.pdA$spec,type="h",log="x",lwd=2,xlab='Frequency = 1/Months',
     ylab='Spectrum',main="Identify harmonic terms\n 20yr, 5yr, 12, 6, 4 months ?")
abline(v=1/12, lwd=0.5,col=2,lty=2)
abline(v=2/12, lwd=0.5,col=2,lty=2)
abline(v=3/12, lwd=0.5,col=2,lty=2)
abline(v=1/(12*5), lwd=0.5,col=2,lty=2)
abline(v=1/(12*20), lwd=0.5,col=2,lty=2)
#
# plot fitted model 
ts.plot(10^(transferfunction.model.res),
        gpars=list(xlab="",ylab="Residual Cells per ml",main="Raw data (black line)",log='y',lwd=c(1,2.3), cex=1.1,cex.main=1.2,lty=c(1,1),col=c(1,4)))
#
#dev.off()
###############################################################################

####################################################################################
# Plot commands
#jpeg(file = "Phytoplankton.jpeg")
#windows()
# Either save to file or use interactive mode. 
# Note for reports - save R plots to jpeg and insert as a picture into your
# report. 
# Graphical parameters
par(mar= c(2,4,4,2), cex=1.0)
layout(matrix(c(1,1,2,2),2,2,byrow=TRUE))
#
# plot fitted model 
ts.plot(10^as.numeric(boxmean2.ts), 10^transferfunction.model.fit,
        gpars=list(xlab="",ylab="Cells per ml",
                   main="Raw data (black line). oscillation model (blue)",
                   log='y',lwd=c(1,2.3),#xlim=c(1999,2002),
                   cex=1.1,cex.main=1.2,lty=c(1,1),col=c(1,4)))
for (i in 1992:2014)
{abline(v=i,lty=2,col="grey")}
abline(h=0)

# plot fitted model 
ts.plot(10^(transferfunction.model.res),
        gpars=list(xlab="",ylab="Residual",
                   main="",log='y',
                   lwd=c(1,2.3),#xlim=c(1999,2002),
                   cex=1.1,cex.main=1.2,lty=c(1,1),col=c(1,4)))
abline(h=1,lty=2)
for (i in 1992:2014)
{abline(v=i,lty=2,col="grey")}
abline(h=0)
#
runs(transferfunction.model.res)
#dev.off()
###############################################################################
####################################################################################
# Plot commands
#jpeg(file = "Phytoplankton.jpeg")
#windows()
# Either save to file or use interactive mode. 
# Note for reports - save R plots to jpeg and insert as a picture into your
# report. 
# Graphical parameters
par(mar= c(2,4,4,2), cex=1.0)
#layout(matrix(c(1,1,2,2),2,2,byrow=TRUE))
#
# plot fitted model 
ts.plot(boxmean2.ts, 
        gpars=list(xlab="",ylab="log[Cells per ml]",
                   main="Raw data (black line). ",
                   log='',lwd=c(1,2.3), #xlim=c(2005,2012),
                   cex=1.1,cex.main=1.2,lty=c(1,1),col=c(1,4)))
for (i in 1992:2014)
{abline(v=i,lty=2,col="grey")}
abline(h=0)

#dev.off()
###############################################################################