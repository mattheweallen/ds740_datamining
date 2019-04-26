#https://cloud.google.com/blog/products/gcp/google-cloud-platform-for-data-scientists-using-r-with-google-bigquery
#use google bigquery to get data
#install.packages("bigrquery")
#install.packages("readr")
library("bigrquery")
library("readr")
#https://db.rstudio.com/databases/big-query/
#https://console.cloud.google.com/home/dashboard?project=nyc-citibike-238810
#auth code: 4/OAFrOq6St9wo4ZSiYCL8C2m541Z9uRvza_JdXsS57OqJiezbSQCmyLc
project <- "nyc-citibike-238810" # put your project ID here
sql <- "select * FROM [bigquery-public-data.new_york.citibike_trips] LIMIT 5"
df <- query_exec(sql, project = project)

#write out example csv file for data
