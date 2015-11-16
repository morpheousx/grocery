# derek jones 11/15/2015

predict_hourly_c_ct <- function(con, month, day, year) # predicts hourly customer count 
{
	y <- attr(brain_model, "name")
	if(is.null(y)) {y<-0}
	y<-y+1

	library(forecast)
	
	c_h_DB <- getCountForDay(con,month,day,year)
	c_h_DB[is.na(c_h_DB)] <- 0 #this replaces NA values with the integer 0
	c_h_DB_TS <- ts(c_h_DB, start = 1, end = 24, frequency = 1)	
	
	brain_model <- tslm(customer_count ~ Males + houseowners + Married + Golden + 0, data = c_h_DB_TS)
	
	filename <- file.path(mw.imgDirectory, gettextf("hourlyTrafficPrediction_%d_%d_%d"), year, month, day)
	jpeg(file=filename)
	plot(forecast(brain_model,c_h_DB_TS), main = "projected hourly customer count", xlab ="hour of day", ylab ="# of customers")
	dev.off()

}

predict_daily_c_ct <- function(con,month,year) # predicts daily customer count
{
	y <- attr(brain_model, "name")
	if(is.null(y)) {y<-0}
	y <- y+1

	c_d_DB <- getCountForMonth(con, month ,year)
	c_d_DB[is.na(c_d_DB)] <- 0 #this replaces NA values with the integer 0 
	c_d_DB_TS <- ts(c_d_DB, start = 1, end = 30, frequency = 1)

	brain_model <- tslm(customer_count ~ Males + houseowners + Married + Golden + 0, data = c_d_DB_TS)

	filename <- file.path(mw.imgDirectory, gettextf("dailyTrafficPrediction_%d_%d_%d"), year, month)
        jpeg(file=filename)
	plot(forecast(brain_model,c_d_DB_TS), main = "projected daily customer count", xlab =" day of month", ylab ="# of customers")	
	dev.off()

}


predict_hourly_s_sales <- function(con, month, day, year) # predicts hourly sales 
{
	y <- attr(brain_model, "name")
        if(is.null(y)) {y<-0}
        y <- y+1

	s_h_DB <- getSalesForDay(con, month, day, year)
	s_h_DB[is.na(s_h_DB)] <- 0 # replaces NA entries with 0s
	s_h_DB_TS <- ts(s_h_DB, start = 1, end = 24, frequency = 1)


	brain_model <- tslm( sales ~ Males + houseowners + Married + Golden + 0, data = s_h_DB_TS)
	
	filename <- file.path(mw.imgDirectory, gettextf("hourlySalesPrediction_%d_%d_%d"), year, month, day)
        jpeg(file=filename)
	plot(forecast(brain_model,s_h_DB_TS), main = "projected hourly sales($)", xlab = "hour of the day", ylab = "store sales($)")
	dev.off()

}



predict_daily_s_sales <- function(con, month, year) # predicts daily sales
{
	y <- attr(brain_model, "name")
        if(is.null(y)) {y<-0}
        y <- y+1


	s_d_DB <- getSalesForMonth(con, month, year)
	s_d_DB[is.na(s_d_DB)] <- 0 # replaces NA entries with 0s
	s_d_DB_TS <- ts(s_d_DB, start = 1, end = 30, frequency = 1)

	brain_model <- tslm( sales ~ Males + houseowners + Married + Golden + 0, data = s_d_DB_TS)
	
	filename <- file.path(mw.imgDirectory, gettextf("dailySalesPrediction_%d_%d_%d"), year, month)
        jpeg(file=filename)
	plot(forecast(brain_model, s_d_DB_TS), main = "projected daily sales($)", xlab = "day of month", ylab ="store sales($)")
	dev.off()

}
