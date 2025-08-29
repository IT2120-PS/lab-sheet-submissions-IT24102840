setwd("C:\\Users\\it24102840\\Desktop\\IT24102840")

Delivery_Times <- read.table("C:\\Users\\it24102840\\Desktop\\IT24102840\\Exercise - Lab 05.txt", header = TRUE,sep = ",")
names(Delivery_Times) <- c("Delivery_Time")
fix(Delivery_Times)

attach(Delivery_Times)

histogram <- hist(Delivery_Time,
                  main = "Histogram for Delivery Times",
                  xlab = "Delivery Time (minutes)",
                  breaks = seq(20, 70, length = 10),   # 9 classes
                  right = FALSE)

breaks <- histogram$breaks
freq <- histogram$counts
mids <- histogram$mids

classes <- c()
for(i in 1:(length(breaks)-1)) {
  classes[i] <- paste0("[", breaks[i], ",", breaks[i+1], ")")
}
cbind(Classes = classes, Frequency = freq)

# Step 4: Cumulative frequency polygon (ogive)
cum.freq <- cumsum(freq)

new <- c()
for(i in 1:length(breaks)) {
  if(i == 1) {
    new[i] <- 0
  } else {
    new[i] <- cum.freq[i-1]
  }
}

plot(breaks, new, type = 'l',
     main = "Cumulative Frequency Polygon (Ogive) for Delivery Times",
     xlab = "Delivery Time (minutes)",
     ylab = "Cumulative Frequency",
     ylim = c(0, max(cum.freq)))

cbind(Upper = breaks, CumFreq = new)
