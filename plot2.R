## function allows to load data from household_power_consumption.txt file into 
hpc_LoadData <- function(fn='household_power_consumption.txt', startdate = '2007-02-01' , enddate = '2007-02-02') {
        # read file
        DF <- read.table(fn , na.strings = "?", sep = ";", header = TRUE, as.is = TRUE )
        # convert Date variable from character to Date
        DF$Date <- as.Date(DF$Date, format = "%d/%m/%Y")
        # Convert Date from factor to POSIXct
        DF$Time <- as.POSIXct(DF$Time , format="%H:%M:%S")
        #striftime from Date
        DF$Time <- strftime(DF$Time , format="%H:%M:%S")
        #recreate the new Date+Time
        DF$Time <- strptime( paste( as.character(DF$Date) , DF$Time) , format = "%Y-%m-%d %H:%M:%S", tz = "GMT")   
        sub.DF = subset(DF, as.Date(DF$Date) >= startdate &  as.Date(DF$Date) <= enddate )
        
        
}

## generate plot and save it to a png file
plot2topng <- function(data) {
        custompar=par(ps=12,font=2,font.axis=1,font.lab=1,mfrow = c(1,1))
        plot(data[,2],data[,3], xlab = "", ylab = "Global Active Power (kilowatts)", col = "black", type="l")
        custompar
        dev.copy(png,"plot2.png",width=480,height=480)
        dev.off()
}