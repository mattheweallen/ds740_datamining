#are all data points equally reliable?
set.seed(33) 
n = c(sample(10:20, 15, replace=T), sample(100:200,15, replace=T)) # number of customers per week at diff stores
avgtime = rep(-1, 30)
avgspent = rep(-1,30)
for(store in 1:30){
  time = runif(n[store], 5, 40)
  spent = .5*time + 5 + rnorm(n[store], 0, 5)
  avgtime[store] = mean(time)
  avgspent[store] = mean(spent)
}

plot(avgspent~avgtime, col=c("red", "black"), pch=c(4,21), xlab="Average time per customer (minutes)", ylab="Average $ spent per customer", las=1)
legend("topleft", legend=c("Store with 10-20 customers", "Store with 100-200 customers"), pch=c(4,21), col=c("red", "black"))
