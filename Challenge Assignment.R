#CHALLENGE
load("/Users/daniellehatt/Desktop/Ecologyworkshop/NLM_Workshop.RData")
install.packages("nlstools")
library(nlstools)
plot (NEE ~ TA, data=night)


# Create a function to find initial values for the selfstart function:
# Selfstart for the trc:
trcModel <- function(TA, a, b) {
  y=a * exp(b*TA)
  return(y)
}
trc.int <- function (mCall, LHS, data){
  x <- data$TA
  y <- data$NEE
  
  a <-1.00703982 + -0.08089044* (min(na.omit(y)))
  b <- 0.051654 + 0.001400 * (min(na.omit(y))) 
  
  value = list(a, b)
  names(value) <- mCall[c("a", "b")]
  return(value)
}

# Selfstart Function
SS.trc <- selfStart(model=trcModel,initial= trc.int)

#Find initial values
iv <- getInitial(NEE ~ SS.trc('TA', "a", "b"), data = night[which(night$MONTH == 07),])
iv

y = nls( NEE ~ (a * exp(b * TA)), 
         night[which(day$MONTH == 07),], start=list(a= iv$a , b= iv$b),
         na.action=na.exclude, trace=F, control=nls.control(warnOnly=T))
summary(y)

#checking assumptions
res.trc <- nlsResiduals(y)
par(mfrow=c(2,2))
plot(res.trc, which=1)# Residulas vs fitted values (Constant Variance)
plot(res.trc, which=3) # Standardized residuals
plot(res.trc, which=4) # Autocorrelation
plot(res.trc, which=5) # Histogram (Normality)

#bootstrap
results.trc <- nlsBoot(y, niter=100) 
summary(results.trc)
#plot box plots
plot(results.trc, type = "boxplot")

#CHALLENGE WORKFLOW
#Creating dataframe
parms.Month <- data.frame(
  MONTH=numeric(),
  a=numeric(),
  b=numeric(),
  a.pvalue=numeric(),
  b.pvalue=numeric(),
  stringsAsFactors=FALSE, row.names=NULL)
parms.Month[1:12, 1] <- seq(1,12,1) # Adds months to the file

nee.night <- function(dataframe){y = nls( NEE ~ (a * exp(b * TA)), dataframe,
                                           start=list(a= iv$a , b= iv$b),
                                           na.action=na.exclude, trace=F, control=nls.control(warnOnly=T))
y.df <- as.data.frame(cbind(t(coef(summary(y)) [1:2, 1]), t(coef(summary(y)) [1:2, 4]))) 
names(y.df) <-c("a","b", "a.pvalue", "b.pvalue")
return (y.df )}

#Creating loop
try(for(j in unique(night$MONTH)){
  print(j)
  # Determines starting values:
  iv <- getInitial(NEE ~ SS.trc('PAR', "a", "b"), data = night[which(day$MONTH == j),])
  # Fits temperature response curve:
  y3 <- try(nee.night(night[which(night$MONTH == j),]), silent=T) # Extracts data and saves it in the dataframe
  try(parms.Month[c(parms.Month$MONTH == j ), 2:5 ] <- cbind(y3), silent=T) 
  rm(y3)
}, silent=T) 
parms.Month

#bootstrap
# Create file to store parms and se
boot.NEE <- data.frame(parms.Month[, c("MONTH")]); names (boot.NEE) <- "MONTH"
boot.NEE$a.est<- 0
boot.NEE$b.est<- 0
boot.NEE$a.se<- 0
boot.NEE$b.se<- 0

for ( j in unique(boot.NEE$MONTH)){
  print(j)
  y1 <-night[which(night$MONTH == j),]
  
  # Determines the starting values:
  iv <- getInitial(NEE ~ SS.trc('TA',"a", "b"), data = y1) 
  
  # Fit curve
  night.fit <- nls(NEE ~ a * exp(b*TA), 
                   data=y1, start=list(a= iv$a , b=iv$b ),
                   na.action=na.exclude, trace=F,
                   control=nls.control(warnOnly=T))
  
  # Bootstrap and extract values:
  try(results <- nlsBoot(night.fit, niter=100), silent=T)
  try(a <- t(results$estiboot)[1, 1:2], silent=T)
  try(names(a) <- c('a.est', 'b.est'), silent=T)
  try(b <- t(results$estiboot)[2, 1:2], silent=T)
  try(names(b) <- c('a.se', 'b.se'), silent=T)
  try(c <- t(data.frame(c(a,b))), silent=T)
  
  # Add bootstrap data to dataframe:
  try(boot.NEE[c(boot.NEE$MONTH == j), 2:5] <- c[1, 1:4], silent=T)
  try(rm(night.fit, a, b, c, results, y1), silent=T)
}

trc <- merge(parms.Month, boot.NEE) #merge dataframes
trc

test.nlsResiduals(res.trc)
