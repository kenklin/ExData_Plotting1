# https://class.coursera.org/exdata-013/human_grading/view/courses/973507/assessments/3/submissions

plot3.read <- function(fn) {
  df <- read.csv(fn, sep=";", stringsAsFactors=FALSE)
  return(df)
}

plot3.tidy <- function(df) {
  require(dplyr)
  tbl <- tbl_df(df)
  
  # We will only be using data from the dates 2007-02-01 and 2007-02-02.
  tbl$DateObj <- as.Date(tbl$Date, "%d/%m/%Y")
  tbl <- filter(tbl, DateObj >= "2007-02-01" & DateObj <= "2007-02-02")
  
  # Create new DateTime column
  tbl$DateTime <- strptime(paste(tbl$Date, tbl$Time), "%d/%m/%Y %H:%M:%S")
  
  # Coerce remaining variables
  tbl$Global_active_power   <- as.numeric(tbl$Global_active_power)
  tbl$Global_reactive_power <- as.numeric(tbl$Global_reactive_power)
  tbl$Voltage               <- as.numeric(tbl$Voltage)
  tbl$Global_intensity      <- as.numeric(tbl$Global_intensity)
  tbl$Sub_metering_1        <- as.numeric(tbl$Sub_metering_1)
  tbl$Sub_metering_2        <- as.numeric(tbl$Sub_metering_2)
  tbl$Sub_metering_3        <- as.numeric(tbl$Sub_metering_3)
  
  return(tbl)
}

plot3.plot <- function(tbl) {
  with(tbl, plot(DateTime, Sub_metering_1,
                 type="l", col="black",
                 ylab="Energy sub metering",
                 xlab=""))
  with(tbl, lines(DateTime, Sub_metering_2,
                  type="l", col="red"))
  with(tbl, lines(DateTime, Sub_metering_3,
                  type="l", col="blue"))
  
  legend("topright", 
         legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
         lty=c(1, 1, 1), # gives the legend appropriate symbols (lines)
         lwd=c(2.5, 2.5, 2.5), cex=0.75,
         col=c("black", "red", "blue")) 
}

setwd("/Users/klin/Documents/coursera/exdata-013/ExData_Plotting1")
input <- "data/household_power_consumption.txt"
output <- "plot3.png"

df <- plot3.read(input)
tbl <- plot3.tidy(df)
# To avoid screen to png copy anomolies, write directly to png per ...
# https://class.coursera.org/exdata-013/forum/thread?thread_id=14
png(output)
plot3.plot(tbl)
dev.off()
