if (!require (xlsx)) install.packages ("xlsx")
if (!require (dplyr)) install.packages ("dplyr")
if (!require (tidyr)) install.packages ("tidyr")
if (!require (ggplot2)) install.packages ("ggplot2")
if (!require (gtools)) install.packages ("gtools")

require (xlsx)
require (dplyr)
require (tidyr)
require (ggplot2)

####
####  --- Reading data sets  ----
####

mywd <- "C:/Users/Johnny/Documents/GitHub/test_task"
setwd (mywd)
getwd()
data_fact <- read.xlsx ("data/01_facts.xlsx", sheetIndex=1, header= FALSE) 
data <- read.csv2 ("data/02_Data_test.csv", header= TRUE) 
data_tac <- read.csv2 ("data/03_devices.csv", header= TRUE,sep = ",", quote = '"') 




####
####  --- Processing fact table  ----
####


# Searching for duplicates in fact table
data_fact <- as.data.frame(data_fact)
c1 <- as.data.frame(data_fact[,1])
c2 <- as.data.frame(data_fact [,2])

c1[duplicated(bind_rows(c1,c2)),]
as.character(data_fact[data_fact[,1]==158528850493,2])
rm(c1,c2)

# switching elements where in first column elem is greater than in second
# and ordering elements
for (i in 1:nrow(data_fact)){
  if (data_fact[i,1]>data_fact[i,2]){
    k <- data_fact[i,2]
    data_fact[i,2]  <- data_fact[i,1]
    data_fact[i,1] <- k
    }
}

data_fact <- data_fact[order(data_fact [,1]),]


####
####  --- Data preprocessing ----
####

# changing time to minutes
sec <- data[,'tstamp']/1000 
data[,'tstamp'] <- as.POSIXct(sec,origin = "1970-01-01",tz = "Europe/Moscow")


c1 <- round(data[,'tstamp'], units = c("mins"))
c1 <- as.data.frame(c1)
data[,'tstamp'] <- c1

all_msisdn <- unique(data[,'msisdn'])
all_msisdn <- all_msisdn[order(all_msisdn)]

z <- all_msisdn
all_comb <- data.frame(c(NULL,NULL))
#for(i in 1:(length(z)-1)){

for(i in 1:45){
  t <- as.data.frame(cbind(z[i], all_msisdn[all_msisdn>z[i]]))
  all_comb <- bind_rows (all_comb,t)
  all_comb <- as.data.frame(all_comb)
}

#cbind(z[1], all_msisdn[all_msisdn>z[1]])

####
####  --- Data preprocessing ----
####


gr1 <- c( 158530004641, 158528850493,158524011021)
all_msisdn[all_msisdn %in% gr1]

gr_sample <- data[data[,'msisdn'] %in% gr1,c("msisdn","tstamp","long","lat","start_angle","end_angle")]

class(gr_sample[,'msisdn']) <- 'character'
qplot(long, lat, data=gr_sample,color=msisdn, 
      xlab="Latitude", ylab="Longitude") 


