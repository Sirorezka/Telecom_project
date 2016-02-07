if (!require (xlsx)) install.packages ("xlsx")
if (!require (dplyr)) install.packages ("dplyr")
if (!require (tidyr)) install.packages ("tidyr")
if (!require (ggplot2)) install.packages ("ggplot2")
if (!require (gtools)) install.packages ("gtools")
if (!require (party)) install.packages ("party")
if (!require (rpart)) install.packages ("rpart")
if (!require (caret)) install.packages ("caret")
if (!require (geosphere)) install.packages ("geosphere")
if (!require (doParallel)) install.packages ("doParallel")
if (!require (foreach)) install.packages ("foreach")


require (xlsx)
require (dplyr)
require (tidyr)
require (ggplot2)
require (party)
require (rpart)
require (caret)
require (geosphere)
require (doParallel)
require (foreach)


source ('my_utils.R')

MAKE_PLOTS = F

####
####  --- Reading data sets  ----
####

mywd <- "C:/Users/Johnny/Documents/GitHub/test_task"
#mywd <- "C:/Users/Ivan.Petrov/Documents/GitHub/test_task"
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
col_names[2:4] <- c("device_vendor","device_platform","device_type")
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

# joining tac
data[,'tac'] <- substr(data[,'imei'],1,8)
data <- data %>% left_join (data_tac, by=("tac"))
data[is.na(data$device_type),"device_type"] = -1
data[is.na(data$device_platform),"device_platform"] = -1
data[is.na(data$device_vendor),"device_vendor"] = -1

# converting long, lat, start_angle, end_angle to numeric
data$long <- as.numeric.factor(data$long)
data$lat <- as.numeric.factor(data$lat)
data$start_angle <- as.numeric.factor(data$start_angle)
data$end_angle <- as.numeric.factor(data$end_angle)


#cbind(z[1], all_msisdn[all_msisdn>z[1]])

####
####  --- Plotting paths ----
####



# plot all paths on one graph with imei codes. We are searching for data with matching imei codes

if (MAKE_PLOTS) plot_all_fact_data (data_fact, data)



##
##  Generating train set
##

all_fact_ids <- c(data_fact[,1],data_fact[,2])
data_train <- data[data[,'msisdn'] %in% all_fact_ids,]
source ('my_utils.R')

y_train <- generate_all_combin (all_fact_ids)
y_train[,"class"] <- 0

## defining msisdn match class
aa <- paste0(y_train[,"V1"],"_",y_train[,"V2"], collapse = NULL)
bb <- paste0(data_fact[,1],"_",data_fact[,2], collapse = NULL)
class1 <- aa %in% bb
y_train[class1,"class"] <- 1

## we need to take all pairs from class 1 and make sample for class zero
y_train_1 <- y_train[y_train$class==1,]
y_train_0 <- y_train[y_train$class==0,]
y_train_0 <- y_train_0[sample(nrow(y_train_0),1100),]

y_train <- bind_rows(y_train_1,y_train_0)
rm(y_train_0,y_train_1)


if (MAKE_PLOTS) plot_all_y_train (y_train, data)




###
###    Generating factors
###

all_factors <- y_train[,c('V1','V2')]


# joining cids to train table
tt <- data[,c('msisdn','cid')] %>% group_by(msisdn) %>% summarise(cids = list(unique(cid)))

all_factors <- all_factors %>% left_join (tt, by = c('V1' = 'msisdn'), copy= T)
all_factors <- all_factors %>% left_join (tt, by = c('V2' = 'msisdn'), copy= T)
names(all_factors)[3:4] <- c('cids_V1','cids_V2') 


# computing SymmetricSimilarity
all_factors$cids_V1
all_factors <- all_factors %>% group_by(V1,V2) %>% 
    mutate(ss =length(intersect(cids_V1[[1]],cids_V2[[1]]))/ length(unique(cids_V1[[1]],cids_V2[[1]])))


# computing S2
all_factors <- all_factors %>% group_by(V1,V2) %>% 
  mutate(s2 =length(intersect(cids_V1[[1]],cids_V2[[1]]))/ min(length(cids_V1[[1]]),length(cids_V2[[1]])))


# computing S3
all_factors <- all_factors %>% group_by(V1,V2) %>% 
  mutate(s3 =length(intersect(cids_V1[[1]],cids_V2[[1]]))/ ( ( length(cids_V1[[1]]) +length(cids_V2[[1]]))/2 ))




write.table(y_train, "pr_data/y_train.csv", sep=",")

fact_short <- all_factors [,!(colnames(all_factors) %in% c("cids_V1","cids_V2"))]
write.table(y_train, "pr_data/y_train.csv", sep=",")
write.table(fact_short, "pr_data/factors.csv", sep=",")


y_pred <- read.csv2 ("pr_data/y_pred.csv", header= FALSE, quote = '"', colClasses= c('numeric'))
not_matched <- y_train[y_train[,'class'] != y_pred,]


nrow(not_matched)
if (MAKE_PLOTS) plot_all_y_train (not_matched, data, img_path="non_matched")




###
###  Processing lac areas - generating database with intersections
###


data_lac <- unique(data[, c('lac','cid','long','lat','max_dist','start_angle','end_angle')])


# Plotting all lac points
all_lac_plot <- ggplot(data=data_lac,aes(x=long, y=lat)) + geom_point(alpha=0.2,aes(color=lac), size=2)
all_lac_plot
ggsave ("all_cids.jpg", all_lac_plot, width = 10, height = 7)

# Histogram for lac max distance
hist(data_lac$max_dist, xlim = c(0,15000), breaks = 35)


# generating matrix with intersections
mm <- detectCores()
cl <- makeCluster (mm)
registerDoParallel (cl)

res <- foreach (i = 1:4, .combine =rbind ) %dopar% {
  
  require ("geosphere")
  require ("dplyr")
  
    
  lac_intersect <- data.frame(NULL)
  z <-  1: nrow(data_lac)
  t <- as.data.frame(cbind(z[i], z[z>z[i]]))
  
  for (j in 1:nrow(t)){
    
    point1 <- data_lac[t[j,1],]
    point2 <- data_lac[t[j,2],]
    
    p1_all_coord <- get_triangle_area (point1)
    p2_all_coord <- get_triangle_area (point2)
    
    # looking for points of intersection between two stations
    p_inter1 <- get_all_edges_intersect (p1_all_coord, p2_all_coord)
    p_inter2 <- get_all_inner_intersec (point1,point2, p1_all_coord, p2_all_coord)
    p_inter <- bind_rows (p_inter1, p_inter2)  
    
    df <- as.data.frame(t(c(data_lac$lac[t[j,1]],data_lac$lac[t[j,2]])))
    if (nrow(p_inter)>0)  bind_rows(lac_intersect,df)
    
    
  }
  
  return (lac_intersect)
}

stopCluster(cl)






cid_1 <- c('7743','11362')
cid_1 <- c('7736','550')
cid_2 <- c('7742','10379')



track_plot <- ggplot()  + geom_point(data= p_all_coord,alpha=0.2,aes(x= lon, y=lat,color=row.names(p_all_coord)), size=8)
track_plot
track_plot + geom_point(data = p_inter, aes(x = lon, y= lat),  size=8)







i <- 11
gr1 <- not_matched[i,]  # msisdns that will be plotted
gr_sample <- data[data[,'msisdn'] %in% gr1,c("msisdn","imei","tstamp","long","lat","start_angle","end_angle")]
gr_sample <- data[data[,'msisdn'] %in% gr1,]


# generating plot labels and removing duplicated lables
class(gr_sample[,'msisdn']) <- 'character'
d_labels <- as.character(gr_sample[,'imei']) 
labels_dupl <- duplicated (gr_sample[,c('imei','msisdn','long','lat')]) 
d_labels[labels_dupl] <- ""

v_jitter <- runif(nrow(gr_sample), 0, 3) # labels jitter

track_plot <- ggplot(gr_sample, aes(x= long, y=lat)) + geom_point(alpha=0.2, aes(color=device_type ),size=8)
track_plot <- track_plot + geom_point(shape = 1,size = 8,colour = "black")
#track_plot <- track_plot+ geom_point() +geom_text(data=gr_sample, aes(x=long, y = lat,label=d_labels),size=4,hjust=0, vjust=v_jitter)
track_plot <- track_plot + facet_grid(. ~ msisdn ) + ggtitle(paste0("Series # ",i))
track_plot