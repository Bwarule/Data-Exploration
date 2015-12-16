## create the Data for time stamp application 
## time series model development
library(forecast)
library(dplyr)
library(xts)

## https://stat.ethz.ch/R-manual/R-devel/library/base/html/seq.POSIXt.html
## •A character string, containing one of "sec", "min", "hour", "day", "DSTday", "week", "month", "quarter" or "year".
##  This can optionally be preceded by a (positive or negative) integer and a space, or followed by "s"

time_index <- seq(from = as.POSIXct("2011-05-15 07:00"), 
                  to = as.POSIXct("2012-05-17 18:00"), by = "min")
time_index <- time_index[sample(1:length(time_index),round(0.3* length(time_index)))]
Data <- data.frame(Type_serv="a", Time_stamp = time_index)				  

## Function argument as followed 
Time_stamp = "Time_stamp"
convert <- "qtr" ##("qtr", "Monthly", "Weekly", "day", "hourly")
Timestamp_timeseries <- function(Type_serv,Time_stamp="Time_stamp",Data,convert){
  ## Type_serv : It is kind of services offered 
  ## Time_stamp : it is time at which costumer enter in the DMV
  ## convert into the Monthly, Weekly, day wise, Hourly series.
  
  time_vec <- Data[,Time_stamp]
  date_qtr <- as.yearqtr(time_vec)
  date_Monthly <-  as.yearmon(time_vec, "%b %Y")
  date_Weekly <-  format(time_vec, '%Y.%W')
  date_day <- as.POSIXct(strptime(time_vec, "%Y-%m-%d"))
  date_hourly <-  as.POSIXct(strptime(time_vec, "%Y-%m-%d %H"))
  # format(time, '%Y-%m-%d %H')
  ## as.POSIXct(strptime("2010-10-31 01:30:00", "%Y-%m-%d %H"))
 
    Data_formated <- Data %>%
        mutate(date_hourly = date_hourly,
           date_day = date_day,
           date_Weekly = date_Weekly,
           date_Monthly= date_Monthly,
           date_qtr = date_qtr)
	date_var <- paste("date", convert, sep="_")	   
  
    Datan <- Data_formated %>%
		group_by(eval(parse(text=date_var))) %>%
		summarise( n = n())
    names(Datan)   <- c(date_var,"n") 
	
    if(convert== "Weekly"){
		week_num <- as.numeric(
		unlist(strsplit(Datan$date_Weekly[[1]],"\\."))[[2]])
		week_year <- as.numeric(
		unlist(strsplit(Datan$date_Weekly[[1]],"\\."))[[1]])
		eventdata <- 
		ts( Datan$n, frequency=52, start=c(week_year,week_num))
    }else{
		## eventdata <- xts( Datan$n, order.by = Datan$date_qtr)
		eventdata <- xts( Datan$n, order.by =  eval(parse(text=paste("Datan$",date_var,sep=""))))
    }
  
    return(eventdata)
  
}
