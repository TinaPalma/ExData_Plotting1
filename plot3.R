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
plot3topng <- function(data) {
        custompar=par(ps=12,font=1,font.axis=1,font.lab=1,mfrow = c(1,1))
        plot(data[,2],data[,7], xlab = "", ylab = "Energy sub metering", col = "black", type="l")
        lines(data[,2],data[,8], col="red", lwd=1)
        lines(data[,2],data[,9], col="blue", lwd=1)
        legend(
                "topright", 
                legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
                lty=c(1,1,1),lwd=c(1,1,1),
                col=c("black","red","blue"),
                text.width=50000,
                cex=0.8,box.lty=0,inset=0.01)
        custompar
        dev.copy(png,"plot3.png",width=480,height=480)
        dev.off()
}
