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

#mywd <- "C:/Users/Johnny/Documents/GitHub/test_task"
mywd <- "C:/Users/Ivan.Petrov/Documents/GitHub/test_task"
setwd (mywd)
getwd()
data_fact <- read.xlsx ("data/01_facts.xlsx", sheetIndex=1, header= FALSE) 
data <- read.csv2 ("data/02_Data_test.csv", header= TRUE) 
data_tac <- read.csv2 ("data/03_devices.csv", header= FALSE, quote = '"') 



####
####  --- Processing tac table  ----
####

tt <- gsub('"','',data_tac[,1], perl=TRUE)
col_names <- strsplit (tt[1],",",perl=TRUE)[[1]]
tt <- tt[2:length(tt)]

data_tac <- as.data.frame(tt) %>% separate("tt",into = col_names, sep = ",")


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
####  --- Preparing data table ----
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
####  --- Plotting paths ----
####


gr1 <- c( 158530004641, 158528850493,158524011021)


# plot all paths on one graph with imei codes. We are searching for data with matching imei codes
for (i in 1:nrow(data_fact[,])){

  #i <- 1
  gr1 <- data_fact[i,]  # msisdns that will be plotted
  gr_sample <- data[data[,'msisdn'] %in% gr1,c("msisdn","imei","tstamp","long","lat","start_angle","end_angle")]
  class(gr_sample[,'msisdn']) <- 'character'
  
  
  # generating plot labels and removing duplicated lables
  d_labels <- as.character(gr_sample[,'imei']) 
  labels_dupl <- duplicated (gr_sample[,c('imei','msisdn','long','lat')]) 
  d_labels[labels_dupl] <- ""
  
  v_jitter <- runif(nrow(gr_sample), 0, 3) # labels jitter
  
  track_plot <- ggplot(gr_sample, aes(x= long, y=lat)) + geom_point(alpha=0.2, aes(color=msisdn),size=8)
  track_plot <- track_plot + geom_point(shape = 1,size = 8,colour = "black")
  #track_plot <- track_plot+ geom_point() +geom_text(data=gr_sample, aes(x=long, y = lat,label=d_labels),size=4,hjust=0, vjust=v_jitter)
  track_plot <- track_plot + facet_grid(. ~ msisdn ) + ggtitle(paste0("Series # ",i))
  track_plot
  
  filename <- paste0(i,"_",data_fact[i,1],"_",data_fact[i,2],".jpg")
  ggsave(filename, plot = track_plot, path = "plots_imei",  dpi = 300)
}




i <- 85
gr1 <- data_fact[i,]  # msisdns that will be plotted
gr_sample <- data[data[,'msisdn'] %in% gr1,c("msisdn","imei","tstamp","long","lat","start_angle","end_angle")]
gr_sample <- data[data[,'msisdn'] %in% gr1,]


# generating plot labels and removing duplicated lables
class(gr_sample[,'msisdn']) <- 'character'
d_labels <- as.character(gr_sample[,'imei']) 
labels_dupl <- duplicated (gr_sample[,c('imei','msisdn','long','lat')]) 
d_labels[labels_dupl] <- ""

v_jitter <- runif(nrow(gr_sample), 0, 3) # labels jitter

track_plot <- ggplot(gr_sample, aes(x= long, y=lat)) + geom_point(alpha=0.2, aes(color=msisdn),size=8)
track_plot <- track_plot + geom_point(shape = 1,size = 8,colour = "black")
#track_plot <- track_plot+ geom_point() +geom_text(data=gr_sample, aes(x=long, y = lat,label=d_labels),size=4,hjust=0, vjust=v_jitter)
track_plot <- track_plot + facet_grid(. ~ msisdn ) + ggtitle(paste0("Series # ",i))
track_plot



