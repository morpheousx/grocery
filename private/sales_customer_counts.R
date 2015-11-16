#nahom muluneh

get_hourlyCount <- function(con,time_id)
{
store_id = 17
x = matrix(0,24,14)
for (i in 0:24){
	query = paste('select count(distinct c.customer_id) as customer_count,',time_id,' as time_id,',i,' as hour,t.day_of_month,t.month_of_year,sa.store_sqft,sa.grocery_sqft,sa.frozen_sqft,sa.meat_sqft,sa.coffee_bar,sa.video_store,sa.salad_bar,sa.prepared_food,sa.florist from sales_fact_1997_p  s inner join store sa on sa.store_id = s.store_id inner join customer c on c.customer_id = s.customer_id inner join time_by_day t on t.time_id = s.time_id inner join more_sales_fact_1997 msa on msa.transaction_id = s.transaction_id where sa.store_id = ',store_id,' and msa.hour = ',i,' and s.time_id = ',time_id,';')
	 y <- dbGetQuery(con,query)
	 for (k in 1:12){
		x[i,k] <-  y[1,k]
	 }
	}
colnames(x) <- colnames(y)
return (x)
}


get_dailyCount <- function(con,time_id)
{
	store_id = 17
	x = matrix(0,24,14)
	for (i in 0:30){
        	query = paste('select count(distinct c.customer_id) as customer_count,',time_id,' as time_id,',i,
				' as hour,t.day_of_month,t.month_of_year,sa.store_sqft,sa.grocery_sqft,sa.frozen_sqft,sa.meat_sqft,sa.coffee_bar,
				sa.video_store,sa.salad_bar,sa.prepared_food,sa.florist from sales_fact_1997_p  s inner join store sa on sa.store_id = s.store_id 
				inner join customer c on c.customer_id = s.customer_id inner join time_by_day t on t.time_id = s.time_id inner join more_sales_fact_1997 
				msa on msa.transaction_id = s.transaction_id where sa.store_id = ',store_id,'and s.time_id = ',time_id,';')
         	y <- dbGetQuery(con,query)
        }
colnames(x) <- colnames(y)
return (x)
}





getCountForDay <- function(con,month,day,year)
{
x = matrix(0,24,10)
if (year == 1997){
	salesYear = 'sales_fact_1997_p'
	moreSales = 'more_sales_fact_1997'
} else if (year == 1998){
	salesYear = 'sales_fact_1998_p'
	moreSales = 'more_sales_fact_1998'
} else {
	salesYear = 'sales_fact_1997_p'
	moreSales = 'more_sales_fact_1997'
}
for (hour in 0:24){
	query = paste("select count(c.customer_id) as customer_count, sum(case when c.gender = 'M' then 1 else 0 end ) as 'Males', sum(case when c.gender = 'F' then 1 else 0 end ) as 'Females',  sum(case when c.education = 'Bachelors Degree' then 1 else 0 end ) as 'Bachelors Degree',  sum(case when c.education = 'High School Degree' then 1 else 0 end ) as 'High School Degree', sum(case when c.education = 'Graduate Degree' then 1 else 0 end ) as 'Graduate Degree', sum(case when c.houseowner = 'Y' then 1 else 0 end ) as 'houseowners', sum(case when c.marital_status = 'M' then 1 else 0 end ) as 'Married', sum(case when c.member_card = 'Golden' then 1 else 0 end ) as 'Golden', sum(case when c.member_card = 'Silver' then 1 else 0 end ) as 'Silver' from customer c  inner join ",salesYear," s on c.customer_id = s.customer_id  inner join store sa on sa.store_id = s.store_id inner join time_by_day t on t.time_id = s.time_id inner join ",moreSales," msa on msa.transaction_id = s.transaction_id where msa.hour = ",hour," and t.day_of_month = ",day," and t.month_of_year = ",month ,";" )
	 y <- dbGetQuery(con,query)
	 for (k in 1:10){
		x[hour,k] <-  y[1,k]
	 }
	}
colnames(x) <- colnames(y)
return (x)
}


getCountForMonth <- function(con,month,year)
{
x = matrix(0,30*24,10)
y = matrix(0,24,10)
for (i in 1:30){
	y = getCountForDay(con,month,i,year)
	for(j in 1:24){
		for (k in 1:10){
			x[(i-1)*24+j,k] <-  y[j,k]
		}
	 }
}
colnames(x) <- colnames(y)
return (x)
}


getSalesForDay <- function(con,month,day,year)
{
if (year == 1997){
	salesYear = 'sales_fact_1997_p'
	moreSales = 'more_sales_fact_1997'
} else if (year == 1998){
	salesYear = 'sales_fact_1998_p'
	moreSales = 'more_sales_fact_1998'
} else {
	salesYear = 'sales_fact_1997_p'
	moreSales = 'more_sales_fact_1997'
}

x = matrix(0,24,10)
for (hour in 0:24){
	query = paste("select sum(s.store_sales*s.unit_sales) as sales, sum(case when c.gender = 'M' then 1 else 0 end ) as 'Males', sum(case when c.gender = 'F' then 1 else 0 end ) as 'Females', sum(case when c.education = 'Bachelors Degree' then 1 else 0 end ) as 'Bachelors Degree',  sum(case when c.education = 'High School Degree' then 1 else 0 end ) as 'High School Degree', sum(case when c.education = 'Graduate Degree' then 1 else 0 end ) as 'Graduate Degree', sum(case when c.houseowner = 'Y' then 1 else 0 end ) as 'houseowners', sum(case when c.marital_status = 'M' then 1 else 0 end ) as 'Married', sum(case when c.member_card = 'Golden' then 1 else 0 end ) as 'Golden', sum(case when c.member_card = 'Silver' then 1 else 0 end ) as 'Silver' from customer c inner join ",salesYear," s on c.customer_id = s.customer_id inner join store sa on sa.store_id = s.store_id inner join time_by_day t on t.time_id = s.time_id inner join ",moreSales," msa on msa.transaction_id = s.transaction_id where msa.hour = ",hour,"and t.day_of_month = ",day," and t.month_of_year = ",month,";")
	
	y <- dbGetQuery(con,query)
	 for (k in 1:10){
		x[hour,k] <-  y[1,k]
	 }
	}
colnames(x) <- colnames(y)
return (x)
}


getSalesForMonth <- function(con,month,year)
{
x = matrix(0,30*24,10)
y = matrix(0,24,10)
for (i in 1:30){
	y = getSalesForDay(con,month,i,year)
	for(j in 1:24){
		for (k in 1:10){
			x[(i-1)*24+j,k] <-  y[j,k]
		}
	 }
}
colnames(x) <- colnames(y)
return (x)
}
