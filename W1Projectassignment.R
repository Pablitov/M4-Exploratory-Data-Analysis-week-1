
library(data.table)
library(curl)
library(lubridate)
library(ggplot2)

setwd("~/R/Module4 EDA")

furl = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download.file(furl,destfile = "data.zip")
unzip("data.zip")
filename <- "household_power_consumption.txt"
coln <- c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", 
          "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
pcdata <- read.table(text = grep("^[1,2]/2/2007", readLines(filename), value = TRUE), col.names = coln, sep = ";", header = TRUE)
#pcdata$Date <- dmy(pcdata$Date)
#pcdata$Time <- hms(pcdata$Time)

# Plot 1. Global Active Power Histogram
plot.new()
with(pcdata,hist(Global_active_power, col="red", main = "Global Active Power", xlab ="Global active power (kW)"))
dev.copy(png, file = "plot1.png")
dev.off()

# Plot 2. Time series of Global Active Power
plot.new()
datesvector <-paste(dmy(pcdata$Date),pcdata$Time)
dat <- parse_date_time(datesvector,"Y-m-d H:M:S")
plot(pcdata$Global_active_power ~ dat, type = "l", ylab = "Global Active Power (kilowatts)", xlab = "")
dev.copy(png, file = "plot2.png")
dev.off()

# Plot 3. Energy sub mettering
plot.new()
plot(pcdata$Sub_metering_1 ~ dat, type = "l", ylab = "Energy Sub Metering", xlab = "")
lines(pcdata$Sub_metering_2 ~ dat, type = "l",  col = "Red")
lines(pcdata$Sub_metering_3 ~ dat, type = "s",  col = "Blue")
legend("topright", col = c("black", "red", "blue"), lty = 1, lwd = 2, legend = c("Sub metering 1", "Sub metering 2", "Sub metering 3"))
dev.copy(png, file = "plot3.png")
dev.off()

# Plot 4 Multiple plot
plot.new()
par(mfrow=c(2,2),mar = c(4,4,2,1), oma = c(1,1,2,1))
plot(pcdata$Global_active_power ~ dat, type = "l", ylab = "Global Active Power (kilowatts)", xlab = "")
plot(pcdata$Voltage ~ dat, type = "l", ylab = "Voltage (V)", xlab = "Date & Time")
plot(pcdata$Sub_metering_1 ~ dat, type = "l", ylab = "Energy Sub Metering", xlab = "")
lines(pcdata$Sub_metering_2 ~ dat, type = "l",  col = "Red")
lines(pcdata$Sub_metering_3 ~ dat, type = "s",  col = "Blue")
legend("topright", col = c("black", "red", "blue"), lty = 1, lwd = 2, legend = c("Sub metering 1", "Sub metering 2", "Sub metering 3"))
plot(pcdata$Global_reactive_power ~ dat, type = "l", ylab = "Global reactive power", xlab = "Date & Time")
dev.copy(png, file = "plot4.png")
dev.off()
