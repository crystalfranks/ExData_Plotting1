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
plot2 <- function(dates) {
        
        ## subset to 2-day period
        plotdata <- subset(data, Date == dates[1] | Date == dates[2])
        
        ## save plot as PNG file
        png("plot2.png", height=480, width=480)
        
        ## create plot
        x <- plotdata$DateTime
        y <- as.numeric(as.character(plotdata$Global_active_power))
        plot(x, y,type = "n",
             xlab = "",
             ylab = "Global Active Power (kilowatts)")
        
        ## add line to plot
        lines(x,y)
        
        dev.off()              
}

plot2(dates)