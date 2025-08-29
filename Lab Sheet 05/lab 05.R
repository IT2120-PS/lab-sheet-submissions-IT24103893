setwd("C:\\Users\\IT24103893\\Desktop\\IT24103893 Lab 05")
getwd()

data <-read.table("Data.txt",header = TRUE, sep = ",")
data()

fix(data)


names(data)<-c("X1","X2")
fix(data)

attach(data)

hist(X2,main="Histogram for no of shareholders")
histogram <- hist(X2,main="Histogram for no of shareholders", breaks = seq(130,270,length=8), right = FALSE)

?hist
breaks <- round(histogram$breaks)
breaks

freq <- histogram$counts
freq

mids <- histogram$mids
mids

classes <- c()

for(i in 1:length(breaks)-1) {
  classes[i] <- paste0("[", breaks[i], ",", breaks[i+1], "]")
}

cbind(Classes = classes, Frequency = freq)

lines(mids, freq)

plot(mids, freq, type = 'l', main = "Frequency Polygon for shareholders", xlab = "shareholders", ylab = "Frequency", ylim = c(0, max(freq)))
cum.freq <- cumsum(freq)

new <- c()

for(i in 1:length(breaks)) {
  if (i==1){
    new[i]=0
  }else{
    new[i] = cum.freq[i-1]
  }
}

plot(breaks, new, type ='l', main = "Cumalative Freuency Polygon for shareholders",
     xlab = "Shareholders", ylab = "Cumalative Frequency", ylim = c(0,max(cum.freq)))
cbind(upper = breaks, CumFreq = new)

