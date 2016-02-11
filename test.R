


###
###   --- For tests --- 
###




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
