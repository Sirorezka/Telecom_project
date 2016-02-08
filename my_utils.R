if (!require ("geosphere")) install.packages ("geosphere")
require ("geosphere")


generate_all_combin <- function (ids){
  
  z <- ids
  
  all_comb <- data.frame(c(NULL,NULL))
  for(i in 1:(length(z)-1)){
    t <- as.data.frame(cbind(z[i], z[z>z[i]]))
    all_comb <- bind_rows (all_comb,t)
    all_comb <- as.data.frame(all_comb)
  }
  
  all_comb
}


as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}





plot_all_y_train <- function (y_train, data, img_path ='plots_train_imei'){
  
  for (i in 1:nrow(y_train[,])){
    
    #i <- 400
    gr1 <- y_train[i,]  # msisdns that will be plotted
    #gr_sample <- data[data[,'msisdn'] %in% gr1,c("msisdn","imei","tstamp","long","lat","start_angle","end_angle")]
    gr_sample <- data[data[,'msisdn'] %in% gr1,]
    class(gr_sample[,'msisdn']) <- 'character'
    
    
    # generating plot labels and removing duplicated lables
    d_labels <- as.character(gr_sample[,'imei']) 
    labels_dupl <- duplicated (gr_sample[,c('imei','msisdn','long','lat')]) 
    d_labels[labels_dupl] <- ""
    
    v_jitter <- runif(nrow(gr_sample), 0, 3) # labels jitter
    
    track_plot <- ggplot(gr_sample, aes(x= long, y=lat)) + geom_point(alpha=0.2, aes(color=device_type),size=8)
    track_plot <- track_plot + geom_point(shape = 1,size = 8,colour = "black")
    #track_plot <- track_plot+ geom_point() +geom_text(data=gr_sample, aes(x=long, y = lat,label=d_labels),size=4,hjust=0, vjust=v_jitter)
    track_plot <- track_plot + facet_grid(. ~ msisdn ) + ggtitle(paste0("Series # ",i," -- class #",y_train[i,"class"]))
    track_plot
    
    filename <- paste0(i,"_",y_train[i,1],"_",y_train[i,2],".jpg")
    ggsave(filename, plot = track_plot, path = img_path, width = 14, height = 7,  dpi = 300)
  }
  

}


plot_all_fact_data <- function (data_fact, data, img_path ='plots_imei'){
  
  
  for (i in 1:nrow(data_fact[,])){
    
    #i <- 1
    gr1 <- data_fact[i,]  # msisdns that will be plotted
    #gr_sample <- data[data[,'msisdn'] %in% gr1,c("msisdn","imei","tstamp","long","lat","start_angle","end_angle")]
    gr_sample <- data[data[,'msisdn'] %in% gr1,]
    class(gr_sample[,'msisdn']) <- 'character'
    
    
    # generating plot labels and removing duplicated lables
    d_labels <- as.character(gr_sample[,'imei']) 
    labels_dupl <- duplicated (gr_sample[,c('imei','msisdn','long','lat')]) 
    d_labels[labels_dupl] <- ""
    
    v_jitter <- runif(nrow(gr_sample), 0, 3) # labels jitter
    
    track_plot <- ggplot(gr_sample, aes(x= long, y=lat)) + geom_point(alpha=0.2, aes(color=device_type),size=8)
    track_plot <- track_plot + geom_point(shape = 1,size = 8,colour = "black")
    #track_plot <- track_plot+ geom_point() +geom_text(data=gr_sample, aes(x=long, y = lat,label=d_labels),size=4,hjust=0, vjust=v_jitter)
    track_plot <- track_plot + facet_grid(. ~ msisdn ) + ggtitle(paste0("Series # ",i))
    track_plot
    
    filename <- paste0(i,"_",data_fact[i,1],"_",data_fact[i,2],".jpg")
    ggsave(filename, plot = track_plot, path = img_path, width = 14, height = 7,  dpi = 300)
  }
}



plot_cids_distrib <- function (data, img_path = "ppt_plots"){
 
  tb_cids_usage <- unique(data [,c('lac','cid','msisdn','long','lat')])
  class(tb_cids_usage$lac) <- 'string'
  tb_cids_usage <- tb_cids_usage %>% group_by(lac,cid,long,lat) %>% summarize(usage = length(msisdn))
  
  all_cids_plot <- ggplot(data=tb_cids_usage, aes(x=long, y=lat)) + geom_point(alpha=0.1,aes(color=usage), size=2)
  all_cids_plot
  ggsave("Cids_heat_map.jpg", plot = all_cids_plot, path = img_path, width = 10, height = 7,  dpi = 300)
  
  
  dev_plot <- ggplot(data=tb_cids_usage, aes(usage))
  dev_plot <- dev_plot + geom_histogram(binwidth = 1) + xlim(0, 25)
  dev_plot <- dev_plot + xlab("unique msisdn's registered") + ylab("number of cids") 
  dev_plot
  ggsave("Cids_histogram.jpg", plot = dev_plot, path = img_path, width = 10, height = 7,  dpi = 300)
  
  head(tb_cids_usage)
  
}

##
##  Function takes point as an input and builds 3x2 matrix with 'lon' and 'lat' 
##
##  input:
##  ***  point1 should have c('long','lat','max_dist','start_angle','end_angle')
##

get_triangle_area <- function (point1){
  
  p1_coord1 <- data.frame()
  p1_coord1[1,'lon'] <- point1$long 
  p1_coord1[1,'lat'] <- point1$lat 
  
  p1_coord2 <- destPoint(p1_coord1,point1$start_angle, point1$max_dist)
  p1_coord3 <- destPoint(p1_coord1,point1$end_angle, point1$max_dist)
  
  
  p1_all_coord <- bind_rows (as.data.frame((p1_coord1)),as.data.frame(p1_coord2),as.data.frame(p1_coord3))
  
  p1_all_coord
}



##
##  Function return ggplot of two triangles
##
##  - help to analyse if we have intersections
##
##  input:
##  ***  point1 should have c('long','lat','max_dist','start_angle','end_angle')
##

plot_triangle <- function (point1,point2){
  
  p1_all_coord <- get_triangle_area (point1)
  p2_all_coord <- get_triangle_area (point2)
  p_all_points <- bind_rows(p1_all_coord,p2_all_coord)
  
  
  track_plot <- ggplot() + geom_polygon(data=p1_all_coord, aes(x=lon, y=lat, fill="point1",alpha=0.5))
  track_plot <- track_plot + geom_polygon(data=p2_all_coord, aes(x=lon, y=lat, fill='point2',alpha=0.5))
  track_plot
  
}
  


##
##  Function return ggplot of two triangles
##
##  - help to analyse if we have intersections
##
##  input:
##  ***  point1 should have c('long','lat','max_dist','start_angle','end_angle')
##  ***  tb_intersect should be table of points with c('long','lat')

plot_triangle_and_intersect <- function (point1, tb_intersect){
  
  tb_intersect <- lac_intersect
  
  if ('lon' %in% names(tb_intersect)) names(tb_intersect[,c('lon')]) <- 'long' 
  
  p1_all_coord <- get_triangle_area (point1)

  
  track_plot <- ggplot() + geom_polygon(data=p1_all_coord, aes(x=lon, y=lat, fill="point1",alpha=0.4))
  track_plot <- track_plot + geom_point(data=p1_all_coord[1,], aes(x=lon, y=lat, colour="red", alpha=1),size=3)
  track_plot <- track_plot + geom_point(data=tb_intersect, aes(x=long, y=lat, fill='point2',alpha=1))
  
  for (i in 1:nrow(tb_intersect)){
    int_point <- tb_intersect[i,]
    p_all_coord <- get_triangle_area (int_point)
    
    track_plot <- track_plot + geom_polygon(data=p_all_coord, aes(x=lon, y=lat, fill='point2',alpha=0.2))
    
  }
  
  track_plot
  
}


##
##  Function search for intersections between edges of the triangles 
##
##  input:
##  ***  p1_all_coord and p2_all_coord are 3x2 matrixes with 'lon' and 'lat'
##


## search for intersection

get_all_edges_intersect <- function (p1_all_coord, p2_all_coord){
  
  p_intesect <- data.frame()
  all_comb <- list(c(1,2),c(1,3),c(2,3))
  
  
  ## Search for intersections between triangles
  for (i in all_comb)
  {
    for (j in all_comb){
      
      #i <- c(1,2)
      #j <- c(1,2)

      p1 <- p1_all_coord[i[1],]
      p2 <- p1_all_coord[i[2],]
      p3 <- p2_all_coord[j[1],]
      p4 <- p2_all_coord[j[2],]
      points <- gcIntersect(p1, p2, p3, p4)
      
      if (is.nan(points[1])) points <- c(0,0,0,0)
      k = 0
      x_inter = F
      
      if ( (points[1]-p1[1])*(points[1]-p2[1]) <= 1e-6) k=k+1
      if ( (points[2]-p1[2])*(points[2]-p2[2]) <= 1e-6) k=k+1
      if ( (points[1]-p3[1])*(points[1]-p4[1]) <= 1e-6) k=k+1
      if ( (points[2]-p3[2])*(points[2]-p4[2]) <= 1e-6) k=k+1
      
      if (k==4) {x_inter = T
      p_intesect <- bind_rows(p_intesect,data.frame(t(points[1:2])))
      }
      
      k = 0
      if ((points[3]-p1[1])*(points[3]-p2[1]) <= 1e-6)  k=k+1
      if ((points[4]-p1[2])*(points[4]-p2[2]) <= 1e-6) k=k+1
      if ((points[3]-p3[1])*(points[3]-p4[1]) <= 1e-6) k=k+1
      if ((points[4]-p3[2])*(points[4]-p4[2]) <= 1e-6) k=k+1
      
      if (k==4) {x_inter = T
      p_intesect <- bind_rows(p_intesect,data.frame(t(points[3:4])))
      }
      
      #print (i)
      #print (j)
      #print(p_intesect)
    }
  }
  
  p_intesect <- as.data.frame (p_intesect)
  if (nrow(p_intesect)>0) names(p_intesect) <- c("lon","lat")
  unique(p_intesect)

}



##
## Search for vertexes that lie inside one of the triangles
##
## input:
## ***  point1, point2 should have c('long','lat','max_dist','start_angle','end_angle')
## ***  p1_all_coord and p2_all_coord are 3x2 matrixes with 'lon' and 'lat'
##

get_all_inner_intersec <- function (point1,point2, p1_all_coord,p2_all_coord){
  
  ##p1_all_coord <- get_triangle_area (point1)
  ##p2_all_coord <- get_triangle_area (point2)
  
  
  p_intesect <- data.frame()

  ## Search for points that are inside the figure
  for (i in 1:3){

    cur_point <- point2
    p1 <- p2_all_coord[1,]
    p2 <- p1_all_coord[i,]
    dist <- distCosine(p1,p2)
    bear <- bearingRhumb(p1, p2)
    
    if (is.na(bear)) bear <- -1  # if two points match
    
    #print (dist)
    #print (bear)
    
    st_angle <- cur_point$start_angle
    en_angle <- cur_point$end_angle
    
    inside_sector <- F
    if (bear>st_angle & bear<en_angle) inside_sector = T
    if (bear>st_angle & en_angle<st_angle) inside_sector = T
    if (en_angle<st_angle & bear<en_angle) inside_sector = T
    
    if (dist<cur_point$max_dist & inside_sector) {
      
      p_intesect <- bind_rows(p_intesect,data.frame(p2))
    }
    
    # if two points match
    if (bear == -1 & dist<1e-6){
      p_intesect <- bind_rows(p_intesect,data.frame(p2))
    }
  }
  
  for (i in 1:3){
  
    #i <- 3  
    cur_point <- point1
    p1 <- p1_all_coord[1,]
    p2 <- p2_all_coord[i,]
    dist <- distCosine(p1,p2)
    bear <- bearingRhumb(p1, p2)

    if (is.na(bear)) bear <- -1  # if two points match
    #print (dist)
    #print (bear)
    
    st_angle <- cur_point$start_angle
    en_angle <- cur_point$end_angle
    
    inside_sector <- F
    if (bear>st_angle & bear<en_angle) inside_sector = T
    if (bear>st_angle & en_angle<st_angle) inside_sector = T
    if (en_angle<st_angle & bear<en_angle) inside_sector = T
    
    if (dist<cur_point$max_dist & inside_sector) {
      
      p_intesect <- bind_rows(p_intesect,data.frame(p2))
    }
    
    # if two points match
    if (bear == -1 & dist<1e-6){
      p_intesect <- bind_rows(p_intesect,data.frame(p2))
    }
    
    #print (i)
    #print (p_intesect)
  }
 
  p_intesect <- as.data.frame (p_intesect)
  if (nrow(p_intesect)>0) names(p_intesect) <- c("lon","lat")
  unique(p_intesect)
}


