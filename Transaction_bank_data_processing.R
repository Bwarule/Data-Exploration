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

book_ganesh <- read.csv("C:\\Users\\bharat.warule\\Desktop\\Book2_ganesh.csv", colClasses=c("Account.Id"="character"))
book_ganesh$Account.Id <- as.character(book_ganesh$Account.Id)
length(unique(book_ganesh$Account.Id))
book_ganesh$Activity.Date_new <- 
as.Date(as.character(book_ganesh$Activity.Date), format = "%m/%d/%Y")
# aggregate can be used for this type of thing
max_date = aggregate(book_ganesh$Activity.Date_new,by=list(book_ganesh$Account.Id),max)
names(max_date) <- c("Account.Id","Activity.Date_new")
### let merge the required data 
df2 = merge(max_date,book_ganesh,by = c("Account.Id","Activity.Date_new"))
dim(df2)
head(df2)
write.csv(df2,"Modified_data.csv",row.names = FALSE,quote =FALSE)
