## How to process the Transaction data for bank
## Most Account Transaction are important in the analysis like ATM withdrawal, cash deposit, Fund transfer, etc.

## Below function are taken from the following link 
## http://www.r-bloggers.com/find-the-last-day-of-the-month/

end_of_Month <- function(date) {
	# date character string containing POSIXct date
	date.lt <- as.POSIXlt(date) # add a month, then subtract a day:
	mon <- date.lt$mon + 2 
	year <- date.lt$year
	year <- year + as.integer(mon==13) # if month was December add a year
	mon[mon==13] <- 1
	iso = ISOdate(1900+year, mon, 1, hour=0, tz=attr(date,"tz"))
	result = as.POSIXct(iso) - 86400 # subtract one day
	result + (as.POSIXlt(iso)$isdst - as.POSIXlt(result)$isdst)*3600
}
