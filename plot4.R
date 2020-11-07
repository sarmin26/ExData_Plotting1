plot4 <- function() {
  ## Aim of this function is to
  ## 1. read the household_power_consumption.txt file
  ## 2. subset for data taken from 2 days: 2007-02-01 and 2007-02-02
  ## 3. generate a plott of different submetering vs time
  
  ## Assume household_power_consumption.txt file located in working dir
  
  ## read data
  powerdata <- read.csv("C:/Users/Nipa/Desktop/coursera/household_power_consumption.txt", stringsAsFactors = FALSE, header = TRUE, sep = ";")
  
  ## creat column in table with date and time merged together
  FullTimeDate <- strptime(paste(powerdata$Date, powerdata$Time, sep = " "), "%d/%m/%Y %H:%M:%S")
  powerdata <- cbind(powerdata, FullTimeDate)
  
  ##Change class of all columns to correct class
  powerdata$Date <- as.Date(powerdata$Date, format = "%d/%m/%Y")
  powerdata$Time <- format(powerdata$Time, format = "%H/%M/%S")
  powerdata$Global_active_power <- as.numeric(powerdata$Global_active_power)
  powerdata$Global_reactive_power <- as.numeric(powerdata$Global_reactive_power)
  powerdata$Voltage <- as.numeric(powerdata$Voltage)
  powerdata$Global_intensity <- as.numeric(powerdata$Global_intensity)
  powerdata$Sub_metering_1 <- as.numeric(powerdata$Sub_metering_1)
  powerdata$Sub_metering_2 <- as.numeric(powerdata$Sub_metering_2)
  powerdata$Sub_metering_3 <- as.numeric(powerdata$Sub_metering_3)
  
  ##subset data from 2007-02-01 and 2007-02-02
  subsetdata <- subset(powerdata, Date == "2007-02-01" | Date == "2007-02-02")
  
  ##plot the 4 graps
  png("plot4.png", width = 480, height = 480)
  par(mfrow=c(2, 2))
  with(subsetdata, plot(FullTimeDate, Global_active_power, type = "1", xlab = "", ylab = "Global Active Power(kilowats)"))
  with(subsetdata, plot(FullTimeDate, Voltage, type = "1", xlab = "datetime", ylab = "Voltage"))
  with(subsetdata, plot(FullTimeDate, Sub_metering, type = "1", xlab = "", ylab = "Energy sub metering"))
  lines(subsetdata$FullTimeDate, subsetdata$Sub_metering_2, type = "1", col = "red")
  lines(subsetdata$FullTimeDate, subsetdata$Sub_metering_3, type = "1", col = "blue")
  legend(c("topright"), c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty = 1, lwd = 2, col = c("black", "red", "blue"))
  with(subsetdata, plot(FullTimeDate, Global_reactive_power, type = "1", xlab = "datetime", ylab = "Global_rective_power"))
  dev.off()
}