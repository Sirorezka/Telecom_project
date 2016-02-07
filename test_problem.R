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


#removing rows which aren't present in data table
del_rows <- data_fact$X1 %in% unique(data$msisdn)
data_fact <- data_fact[del_rows,]
del_rows <- data_fact$X2 %in% unique(data$msisdn)
data_fact <- data_fact[del_rows,]


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
all_factors <- all_factors %>% group_by(V1,V2) %>% 
    mutate(ss =length(intersect(cids_V1[[1]],cids_V2[[1]]))/ length(unique(cids_V1[[1]],cids_V2[[1]])))


# computing S2
all_factors <- all_factors %>% group_by(V1,V2) %>% 
  mutate(s2 =length(intersect(cids_V1[[1]],cids_V2[[1]]))/ min(length(cids_V1[[1]]),length(cids_V2[[1]])))


# computing S3
all_factors <- all_factors %>% group_by(V1,V2) %>% 
  mutate(s3 =length(intersect(cids_V1[[1]],cids_V2[[1]]))/ ( ( length(cids_V1[[1]]) +length(cids_V2[[1]]))/2 ))


# phones full name match and phone type match for pairs of msisdn's
phones <- unique(data[,c('msisdn','device_vendor','device_platform','device_type')])
phones$device_type [phones$device_type =="Phone"] = "SmartPhone"
phones <- phones %>% mutate(phone_full = paste0(device_vendor,"_",device_platform,"_",device_type))
phones <- phones[phones$device_type!=-1,]


phone_type <- unique(phones[,c('msisdn','device_type')])
phone_type$unit <- 1
phone_type <- phone_type %>% group_by(msisdn) %>% spread(key=device_type, value = unit, fill = 0)  

devices <- t(summarise_each (phone_type[,2:6],funs(sum)))
devices <- as.data.frame(devices)

if (MAKE_PLOTS)  {
  # ploting devices by number of occurences in data
  dev_plot <- ggplot(data=devices, aes(x=rownames(devices), y=V1))
  dev_plot <- dev_plot + geom_bar(stat="summary", fun.y=sum, aes(fill =rownames(devices)))
  dev_plot
}


phones <- phones[,c('msisdn','phone_full')]
phones <- phones %>% group_by(msisdn) %>% summarise(phone_lst = list(phone_full))
phones <- phones %>% left_join (phone_type, by = c('msisdn'))


all_factors <- all_factors %>% left_join (phones, by = c('V1' = 'msisdn'), copy= T, fill = 0)
all_factors <- all_factors %>% left_join (phones, by = c('V2' = 'msisdn'), copy= T, fill = 0)
names(all_factors)


col_del <- c(NULL) 
for (i in rownames(devices)){
  all_factors[is.na(all_factors[,paste0(i,".y")]),paste0(i,".y")] <-0
  all_factors[is.na(all_factors[,paste0(i,".x")]),paste0(i,".x")] <-0
  
  all_factors[,i] <- all_factors[,paste0(i,".x")] + all_factors[,paste0(i,".y")]
  col_del <- c(col_del,paste0(i,".x"),paste0(i,".y"))  
}

all_factors <- all_factors [,!(names(all_factors) %in% col_del)]

# full phone name match
all_factors <- all_factors %>% group_by(V1,V2) %>% 
  mutate(phone_matched =min(length(unlist(phone_lst.x)),length(unlist(phone_lst.y)),length(intersect(phone_lst.x,phone_lst.y))))


# phones vendor match

phone_vendor <- unique(data[,c('msisdn','device_vendor')])
phone_vendor <- phone_vendor[phone_vendor$device_vendor!=-1,]
phone_vendor <- phone_vendor %>% group_by(msisdn) %>% summarise(vend_lst = list(device_vendor))
  
all_factors <- all_factors %>% left_join (phone_vendor, by = c('V1' = 'msisdn'), copy= T, fill = 0)
all_factors <- all_factors %>% left_join (phone_vendor, by = c('V2' = 'msisdn'), copy= T, fill = 0)
all_factors <- all_factors %>% group_by(V1,V2) %>% 
  mutate(vendor_matched =min(length(unlist(vend_lst.x)),length(unlist(vend_lst.y)),length(intersect(vend_lst.x,vend_lst.y))))


all_factors <- all_factors [,!(names(all_factors) %in% c('vend_lst.x','vend_lst.y'))]



# ploting station usage - number of unique msisdn's registered in each cid
if (MAKE_PLOTS)  plot_cids_distrib(data,img_path = "ppt_plots")


##  Computing L1 and L2 distances between points 
##  


mm <- detectCores()
cl <- makeCluster (mm)
registerDoParallel (cl)


tb_res_dist <- foreach (i = 1:nrow(y_train), .combine =rbind ) %dopar%{
  print (i)
  require ("geosphere")
  v1 <- y_train[i,1][[1]]
  v2 <- y_train[i,2][[1]]
  path1 <- data [data$msisdn==v1,c('tstamp','long','lat')]
  path2 <- data [data$msisdn==v2,c('tstamp','long','lat')]
  
  tt <- cbind(path1[1:(nrow(path1)-1),],path1[2:nrow(path1),])
  tt$sq <- distCosine (tt[,2:3],tt[,5:6])
  l2_dist1 <- sqrt(sum(tt$sq[tt$sq!=0]^2)/nrow(tt))
  l1_dist1 <- sum(abs(tt$sq[tt$sq!=0]))/nrow(tt)

  path1 <- rbind(path1,path2)
  path1 <- path1[order(path1$tstamp),]
  
  tt <- cbind(path1[1:(nrow(path1)-1),],path1[2:nrow(path1),])
  tt$sq <- distCosine (tt[,2:3],tt[,5:6])
  l2_dist2 <- sqrt(sum(tt$sq[tt$sq!=0]^2)/nrow(tt))
  l1_dist2 <- sum(abs(tt$sq[tt$sq!=0]))/nrow(tt)
  
  tb_dist <- c(i, l2_dist1,l2_dist2,l1_dist1,l1_dist2)
  tb_dist
}

stopCluster(cl)

tb_res_dist <- as.data.frame(tb_res_dist)
tb_res_dist <- tb_res_dist[order(tb_res_dist$V1),]
all_factors[,'l2_dist_diff_1'] <- (tb_res_dist$V3 - tb_res_dist$V2)
all_factors[,'l1_dist_diff_1'] <- (tb_res_dist$V5 - tb_res_dist$V4)
all_factors[,'l2_dist_diff_2'] <- abs(tb_res_dist$V3 - tb_res_dist$V2)
all_factors[,'l1_dist_diff_2'] <- abs(tb_res_dist$V5 - tb_res_dist$V4)

rm(tb_res_dist)



###
###    Saving factor data in file for future use
###


write.table(y_train, "pr_data/y_train.csv", sep=",")

fact_short <- all_factors [,!(colnames(all_factors) %in% c("cids_V1","cids_V2","phone_lst.x","phone_lst.y"))]
write.table(fact_short, "pr_data/factors.csv", sep=",")


y_pred <- read.csv2 ("pr_data/y_pred.csv", header= FALSE, quote = '"', colClasses= c('numeric'))
not_matched <- y_train[y_train[,'class'] != y_pred,]
not_matched$pred <- y_pred[y_train[,'class'] != y_pred]

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






y_train [y_train$V1 == "158521282171",][1,]

i <- 3
gr1 <- y_train[i,]  # msisdns that will be plotted
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


data_reg <- unique(data [,c("lac","cid","long","lat")])
data_reg$msisdn <- "null"
data_reg$device_type <- "null"
class(data_reg)
data_reg <- bind_rows(data_reg,gr_sample[,c("lac","cid","long","lat","msisdn","device_type")])

track_plot <- ggplot(data_reg, aes(x= long, y=lat)) + geom_point(alpha=0.2, aes(color=msisdn ),size=8)
#track_plot <- track_plot + geom_point(shape = 1,size = 8,colour = "black")
#track_plot <- track_plot+ geom_point() +geom_text(data=gr_sample, aes(x=long, y = lat,label=d_labels),size=4,hjust=0, vjust=v_jitter)
#track_plot <- track_plot + facet_grid(. ~ msisdn ) + ggtitle(paste0("Series # ",i))
track_plot
