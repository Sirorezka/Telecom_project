#Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_73')  # for 64 bit Java
if (!require (rJava)) install.packages ("rJava")
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
if (!require(randomForest)) install.packages('randomForest')
if (!require(e1071)) install.packages('e1071')
if (!require(caret)) install.packages('caret')
if (!require(caret)) install.packages('pROC')


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
require(randomForest)
require(caret)
library(pROC)

##
##  --- Setting working directory
##


MAKE_PLOTS = F
USE_TEST_DATA = F


# mywd <- "C:/Users/Administrator/Desktop/Telecom_project"
  mywd <- "C:/Users/Johnny/Documents/GitHub/test_task"
# mywd <- "C:/Users/Ivan.Petrov/Documents/GitHub/test_task"
setwd (mywd)
getwd()
source ('my_utils.R')




####
####  --- Reading data sets  ----
####


data_fact <- read.xlsx ("data/01_facts.xlsx", sheetIndex=1, header= FALSE) 
data <- read.csv2 ("data/02_Data_test.csv", header= TRUE) 
data_tac <- read.csv2 ("data/03_devices.csv", header= FALSE, quote = '"') 

nrow(unique(data [,c('lac','cid')]))

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


# switching elements where id in first column is greater than id in second
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
####  --- Preprocessing data table ----
####


# changing time to minutes
sec <- data[,'tstamp']/1000 
data[,'tstamp'] <- as.POSIXct(sec,origin = "1970-01-01",tz = "Europe/Moscow")


c1 <- round(data[,'tstamp'], units = c("mins"))
c1 <- as.data.frame(c1)
data[,'tstamp'] <- c1

all_msisdn <- unique(data[,'msisdn'])
all_msisdn <- all_msisdn[order(all_msisdn)]


# joining tac (phone type data, vendor info, platform)
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


# we need to check if cid is unique and can be used as key for base station:
tb_cid_check <- unique(data[,c('lac','cid')])
tb_cid_check <- tb_cid_check %>% group_by(cid) %>% summarize (mm=length(list(lac)))
if (max(tb_cid_check$mm) == 1) print ("All cids are unique")





####
####  --- Plotting paths ---
####


# plot all data_fact pathes

if (MAKE_PLOTS) plot_all_y_train (data_fact, data, img_path ='plots_imei')



####
####  --- Generating train set ---
####


# Generating all posible combinations of msidns
all_fact_ids <- c(data_fact[,1],data_fact[,2])
sort(all_fact_ids)
y_train <- generate_all_combin (all_fact_ids)


# 'class' column will contain information if two msidn's match
y_train[,"class"] <- 0

# two msidn's match if they are present in fact_data
aa <- paste0(y_train[,"V1"],"_",y_train[,"V2"], collapse = NULL)
bb <- paste0(data_fact[,1],"_",data_fact[,2], collapse = NULL)
class1 <- aa %in% bb
y_train[class1,"class"] <- 1


# Sampling training data:
#
# - We need to take all pairs from class 1. 
# - And for clazz 0 we will sample some points that not present in original data

y_train_1 <- y_train[y_train$class==1,]
y_train_0 <- y_train[y_train$class==0,]
y_train_0 <- y_train_0[sample(nrow(y_train_0),1100),]

y_train <- bind_rows(y_train_1,y_train_0)
rm(y_train_0,y_train_1)


if (MAKE_PLOTS) plot_all_y_train (y_train, data)

length(unique(data$msisdn))

####
####  --- Generating test set ---
####


if (USE_TEST_DATA) {

  all_msisdn <- unique(data$msisdn)
  y_test <- generate_all_combin (all_msisdn)
  y_test[,"class"] <- -1
  
  y_train <- bind_rows (y_test,y_train)
  max(y_train$class)
  
}


data_cids <- data %>% group_by(msisdn) %>% summarize (base_st_visited = length(unique(cid)))

qplot(data_cids$base_st_visited, 
      xlim = c(-1,300),
      geom = "histogram",
      xlab = "Number of Base Stations visited by one msisdn",
      ylab = "group count")

###
###  ---  Generating factors  ---
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

ptm <- proc.time()
mm <- detectCores()
cl <- makeCluster (mm)
registerDoParallel (cl)


tb_res_dist <- foreach (i = 1:nrow(y_train), .combine =rbind, .packages =c('geosphere') ) %dopar%{
    #print (i)
  
    #i <- 910
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

print (proc.time()-ptm)

tb_res_dist <- as.data.frame(tb_res_dist)
tb_res_dist <- tb_res_dist[order(tb_res_dist$V1),]
all_factors[,'l2_dist_diff_1'] <- (tb_res_dist$V3 - tb_res_dist$V2)
all_factors[,'l1_dist_diff_1'] <- (tb_res_dist$V5 - tb_res_dist$V4)
all_factors[,'l2_dist_diff_2'] <- abs(tb_res_dist$V3 - tb_res_dist$V2)
all_factors[,'l1_dist_diff_2'] <- abs(tb_res_dist$V5 - tb_res_dist$V4)

rm(tb_res_dist)




## 
##  --- adding data points to short pathes ---
##
##  Descr: First select all 'msidns' which visited less or equal than 3 base stations.
##         Then for each of this stations we search for stations that have cross signals
##         with stations visited by this 'msidns'. We will call this new stations - possible locations.
##         We add later stations to the path list
##


CONST_SHORT_PATH_VAL <- 3

data_adj <- data    # we will add new rows to the copy of data
all_train_ids <- unique(c (y_train$V1,y_train$V2))
all_train_ids <- as.data.frame(all_train_ids)
names(all_train_ids) <- 'msisdn'

new_base_stat <- get_all_nearest_stations (data_adj,all_train_ids,CONST_SHORT_PATH_VAL)

nrow(new_base_stat)



nrow(data_adj)
data_adj <- bind_rows(data_adj, new_base_stat)
nrow(data_adj)



###
###  Calculating new SS, S2, S3 measures for data_adj
###

# tt will have all cids
tt <- data_adj[,c('msisdn','cid')] %>% group_by(msisdn) %>% summarise(cids_adj = list(unique(cid)))

all_factors <- all_factors %>% left_join (tt, by = c('V1' = 'msisdn'), copy= T)
all_factors <- all_factors %>% left_join (tt, by = c('V2' = 'msisdn'), copy= T)

colnames(all_factors)[(ncol(all_factors)-1):ncol(all_factors)] <- c('cids_adj_V1','cids_adj_V2')



# computing SymmetricSimilarity
all_factors <- all_factors %>% group_by(V1,V2) %>% 
  mutate(ss_adj =length(intersect(cids_adj_V1[[1]],cids_adj_V2[[1]]))/ length(unique(cids_adj_V1[[1]],cids_adj_V2[[1]])))


# computing S2
all_factors <- all_factors %>% group_by(V1,V2) %>% 
  mutate(s2_adj =length(intersect(cids_adj_V1[[1]],cids_adj_V2[[1]]))/ min(length(cids_adj_V1[[1]]),length(cids_adj_V2[[1]])))


# computing S3
all_factors <- all_factors %>% group_by(V1,V2) %>% 
  mutate(s3_adj =length(intersect(cids_adj_V1[[1]],cids_adj_V2[[1]]))/ ((length(cids_adj_V1[[1]]) +length(cids_adj_V2[[1]]))/2 ))




##    
##    Calculate nearest points for every path
##

CONST_SHORT_PATH_VAL <- 99

data_adj <- data    # we will add new rows to the copy of data
all_train_ids <- unique(c (y_train$V1,y_train$V2))
all_train_ids <- as.data.frame(all_train_ids)
names(all_train_ids) <- 'msisdn'

new_base_stat <- get_all_nearest_stations (data_adj,all_train_ids,CONST_SHORT_PATH_VAL)


print ("IMPORTANT STATS:")
nrow(new_base_stat)
nrow(data_adj)
data_adj <- bind_rows(data_adj, new_base_stat)
nrow(data_adj)



###
###  Calculating new SS, S2, S3 measures for data_adj
###



# tt will have all cids
tt <- data_adj[,c('msisdn','cid')] %>% group_by(msisdn) %>% summarise(cids_adj = list(unique(cid)))

all_factors <- all_factors %>% left_join (tt, by = c('V1' = 'msisdn'), copy= T)
all_factors <- all_factors %>% left_join (tt, by = c('V2' = 'msisdn'), copy= T)

names(all_factors)[21:22] <- c("cids_adj_V1_old", "cids_adj_V2_old")
colnames(all_factors)[(ncol(all_factors)-1):ncol(all_factors)] <- c('cids_adj_V1','cids_adj_V2')



# computing SymmetricSimilarity
all_factors <- all_factors %>% group_by(V1,V2) %>% 
  mutate(ss_adj_v2 =length(intersect(cids_adj_V1[[1]],cids_adj_V2[[1]]))/ length(unique(cids_adj_V1[[1]],cids_adj_V2[[1]])))


# computing S2
all_factors <- all_factors %>% group_by(V1,V2) %>% 
  mutate(s2_adj_v2 =length(intersect(cids_adj_V1[[1]],cids_adj_V2[[1]]))/ min(length(cids_adj_V1[[1]]),length(cids_adj_V2[[1]])))


# computing S3
all_factors <- all_factors %>% group_by(V1,V2) %>% 
  mutate(s3_adj_v2 =length(intersect(cids_adj_V1[[1]],cids_adj_V2[[1]]))/ ((length(cids_adj_V1[[1]]) +length(cids_adj_V2[[1]]))/2 ))





###
###    Saving factor data in file for future use
###


write.table(y_train, "pr_data/y_train.csv", sep=",")

fact_short <- all_factors [,!(colnames(all_factors) %in% c("cids_V1","cids_V2","phone_lst.x","phone_lst.y","cids_adj_V1","cids_adj_V2","cids_adj_V1_old","cids_adj_V2_old"))]
write.table(fact_short, "pr_data/factors.csv", sep=",")


y_pred <- read.csv2 ("pr_data/y_pred.csv", header= FALSE, quote = '"', colClasses= c('numeric'))
not_matched <- y_train[y_train[,'class'] != y_pred,]
not_matched$pred <- y_pred[y_train[,'class'] != y_pred]

nrow(not_matched)
if (MAKE_PLOTS) plot_all_y_train (not_matched, data, img_path="non_matched")




###
###   --- Building model --- 
###



# reading factors and train set
all_factors <- read.table("pr_data/factors.csv", sep=",")
y_train <- read.table("pr_data/y_train.csv", sep=",")
y_train$class <- as.factor(y_train$class)


# 1. designing cross validation
n_folds <- createFolds(y_train$class, k = 5, list = FALSE)

n_folds_auc <- data.frame(NULL)
for (i in unique(n_folds)){
  
  train_X <- all_factors[!(n_folds==i), 3:ncol(all_factors)]
  test_X <- all_factors[n_folds==i, 3:ncol(all_factors)]
  
  train_y <- y_train[!(n_folds==i),]
  test_y <- y_train[(n_folds==i),]

  clf <- randomForest(train_X,train_y$class,ntree=150,maxnodes=20)
  round(importance(clf), 2)
  
  y_pred <- predict(clf, test_X, type="response")
  cf_mat <- confusionMatrix(y_pred, test_y$class )
  cf_mat$byClass[]

  rocValues <- roc(y_pred, as.numeric.factor(test_y$class))
  
  n_folds_auc[i,'fold'] = i
  n_folds_auc[i,'auc'] = rocValues$auc
  n_folds_auc[i,'f1'] = f1_measure (y_pred, test_y$class)
}



# 2. making grid search
param_grid <- expand.grid(par_ntree = seq(50,300,10), par_maxnodes = seq(5,20,1))
overall_score <- data.frame(NULL)
names(overall_score) <- c("df","df")

for (g in 1:nrow(param_grid)){
  
  if (g %% 20 == 0) print (paste0("% completed: ",round(g/nrow(param_grid)*100,0)))
  
  params <-param_grid[g,]
  n_folds_auc <- data.frame(NULL)
  for (i in unique(n_folds)){
    
    #print (i)
    train_X <- all_factors[!(n_folds==i), 3:ncol(all_factors)]
    test_X <- all_factors[n_folds==i, 3:ncol(all_factors)]
    
    train_y <- y_train[!(n_folds==i),]
    test_y <- y_train[(n_folds==i),]
    
    clf <- randomForest(train_X,train_y$class,ntree=params[[1]],maxnodes=params[[2]])
    round(importance(clf), 2)
    
    y_pred <- predict(clf, test_X, type="response")
    cf_mat <- confusionMatrix(y_pred, test_y$class )
    cf_mat$byClass[]
    
    rocValues <- roc(y_pred, as.numeric.factor(test_y$class))
    
    n_folds_auc[i,'fold'] = i
    n_folds_auc[i,'auc'] = rocValues$auc
    n_folds_auc[i,'f1'] = f1_measure (y_pred, test_y$class)
  }
  
  auc_mean = mean(n_folds_auc$auc)
  f1_mean = mean(n_folds_auc$f1)
  f1_min = min(n_folds_auc$f1)
  d_row = c(params[[1]],params[[2]],auc_mean,f1_mean,f1_min)
  overall_score <- rbind(overall_score,d_row)
}


names (overall_score) <- c('p1','p2','auc_mean','f1_mean','f1_min')
overall_score






###
###   --- Generating final table with all results --- 
###


y_train <- read.csv2 ("pr_data_final/y_train.csv", header= TRUE, sep = ",")
y_pred <- read.csv2 ("pr_data_final/y_pred.csv", header= FALSE, sep = ",")
y_train$class <- as.numeric.factor (y_pred[,1])
y_train$prob <- as.numeric.factor(y_pred[,2])
rm(y_pred)



## calulating racall rate
get_recall <- function (y_train,data_fact){
  
    aa <- paste0(y_train[,"V1"],"_",y_train[,"V2"], collapse = NULL)
    bb <- paste0(data_fact[,1],"_",data_fact[,2], collapse = NULL)
    class1 <- aa %in% bb
    rm(aa)
    rm(bb)
    
    yy <- y_train[class1,]
    yy <- unique(yy[order(yy$V1),])
    yy[,'true_class'] <- 1
    print (paste0("% recall from the fact table: ",sum(yy$class == yy$true_class)/nrow(yy)))

}


## get intersecion with fact:
get_intersection <- function (y_train,data_fact){
  
  aa <- paste0(y_train[,"V1"],"_",y_train[,"V2"], collapse = NULL)
  bb <- paste0(data_fact[,1],"_",data_fact[,2], collapse = NULL)
  class1 <- aa %in% bb
  print (sum(class1))
}



y_train <- y_train[y_train$class ==1,]  # we only need matching phones
y_train <- unique(y_train)              # removes duplicated train data


y_train.filter()
tt <- y_train %>% filter(prob>0.95) %>% group_by(V1) %>% summarize(n_numb = length(V2))

tt <- y_train %>% filter(prob>0.95) 
get_intersection (tt, data_fact)    


plot_all_y_train (tt[1:1000,1:2], data, img_path="tests")


