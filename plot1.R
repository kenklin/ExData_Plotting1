plot1.read <- function(fn) {
  df <- read.csv(fn, sep=";", stringsAsFactors=FALSE)
  return(df)
}

plot1.tidy <- function(df) {
  require(dplyr)
  tbl <- tbl_df(df)
  
  # We will only be using data from the dates 2007-02-01 and 2007-02-02.
  tbl$Date <- as.Date(tbl$Date, "%d/%m/%Y")
  tbl <- filter(tbl, Date >= "2007-02-01" & Date <= "2007-02-02")
  
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

plot1.hist <- function(tbl) {
  with(tbl, hist(Global_active_power, 
                 main="Global Active Power",
                 xlab="Global Active Power (kilowatts)",
                 ylab="Frequency",
                 col="red"))
}

#setwd("/Users/klin/Documents/coursera/exdata-013/ExData_Plotting1")
#df <- plot1.read("data/household_power_consumption.txt")
tbl <- plot1.tidy(df)
plot1.hist(tbl)
