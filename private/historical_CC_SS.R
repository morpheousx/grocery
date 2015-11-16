# derek jones 11/15/2015


total_sales_by_hour <- function(con,time_id)
{
	a = rep ( 0 ,times =24)


	for( i in 0:24)
	{

		query <- paste("select sum(s.store_sales * s.unit_sales) as total_sales
 		from sales_fact_1997_p s inner join more_sales_fact_1997 t on t.transaction_id = s.transaction_id 
   		inner join time_by_day td on td.time_id = s.time_id where td.time_id = ",time_id," and t.hour =",i,";")
		result <- dbGetQuery(con,query)
		a[i] <- result # add if NA  push a 0
	}

	a <- matrix(a)

	plot(a, type='l', main="sales per hour on given day", xlab="hour of the day (0-24)", ylab = "store sales $")
	return(a)

}


total_sales_by_day <- function(con,time_id,month)
{
	month_sales = rep(0,times = 30)


	for( i in 1:30)
	{
		query = paste("select sum(s.store_sales * s.unit_sales) as total_sales from sales_fact_1997_p s
		inner join more_sales_fact_1997 t on t.transaction_id = s.transaction_id inner join time_by_day td on td.time_id = s.time_id where 
		td.day_of_month = ",i," and where td.month_of_year = month;")
		month_sales[i] <- dbGetQuery(con,query)
	}
	
	month_sales <- matrix(month_sales)
	
	plot(month_sales, type="l", main ="sales per day for given month", xlab="day of the month", ylab = "store sales($)")
	return(month_sales) 
}

customer_count_by_hour <- function(con, time_id)
{
	x <- get_hourlyCount(con,time_id)
	x <- unlist(x)
	hourly_count <- x[,c('hour','customer_count')]
	
	plot(hourly_count , main = "customers by hour for given day" ,xlab =" hour of the day ", ylab = "# of customers" ,type ="l")

	return(hourly_count)
}

customer_count_by_day <- function(con, time_id, month, year)
{

	daily_count <- rep(0, times = 30 ) 

	for( i in 0:30)
	{
		query <- paste("select count(DISTINCT c.customer_id) as tot_customer_count from sales_fact_1997_p s , customer c, time_by_day td 
			where c.customer_id = s.customer_id and s.time_id = td.time_id and td.the_year = ",year," and td.month_of_year = ",month," and td.day_of_month = ",i,";")

		x <- dbGetQuery(con,query)
		daily_count[i] <- x
	}

	daily_count <- unlist(daily_count)
	plot(daily_count, main = "customers by day for given month" , xlab ="day of month", ylab = "# of customers" ,type = "l")
	return (daily_count)
}


