## identify loaction of file to download and date range to plot
URL   <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
dates <- c("2007-02-01","2007-02-02")

## this function downloads file and formats dates
download <- function(URL) {
        temp <- tempfile()
        download.file(URL, temp)
        data <- read.csv2(unz(temp,"household_power_consumption.txt"), na.strings = '?')
                
        ## add variable with Date & Time combined and formated to "POSIXlt"
        data$DateTime = strptime(x = paste(data$Date, data$Time), format = "%d/%m/%Y %H:%M:%S")
        data$Date = as.Date(data$Date, format = "%d/%m/%Y") ## formats Date column
        
        return(data)
}

data <- download(URL)

## this function creates the plot and saves it as a png
plot4 <- function(dates) {
        
        ## subset to 2-day period
        plotdata <- subset(data, Date == dates[1] | Date == dates[2])
        
        ## save plot as PNG file
        png("plot4.png", height=480, width=480)
        
        ## build layout
        layout(matrix(1:4, 2, 2))
        layout.show(4)
        
        ## create plot 1
        x <- plotdata$DateTime
        y <- as.numeric(as.character(plotdata$Global_active_power))
        plot(x, y,type = "n",
             xlab = "",
             ylab = "Global Active Power (kilowatts)")
        
        ## add line to plot
        lines(x,y)
        
        
        ## create plot 2
        y1 <- as.numeric(as.character(plotdata$Sub_metering_1))
        y2 <- as.numeric(as.character(plotdata$Sub_metering_2))
        y3 <- as.numeric(as.character(plotdata$Sub_metering_3))
                
        plot(range(x, x, x), range(y1, y2, y3), type = "n",
             xlab = "", ylab = "Energy sub metering")
                
        ## add line to plot
        lines(x, y1)
        lines(x, y2, col = "red")
                lines(x, y3, col = "blue")
                
        ## add legend
        legend("topright", 
               legend = c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"), 
               lty=c(1,1,1), col=c("black", "blue","red"))
        
       
        ## create plot 3
        y4 <- as.numeric(as.character(plotdata$Voltage))
        plot(x, y4,type = "n",
             xlab = "datetime",
             ylab = "Voltage")
        
        ## add line to plot
        lines(x,y4)
        
        
        ## create plot 4
        y5 <- as.numeric(as.character(plotdata$Global_reactive_power))
        plot(x, y5,type = "n",
             xlab = "datetime",
             ylab = "Global_reactive_power")
        
        ## add line to plot
        lines(x,y5)
                
        
        dev.off()              
}

plot4(dates)