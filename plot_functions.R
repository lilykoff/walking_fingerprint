library(tidyverse)
library(dplyr)
library(ggplot2)
real_id <- data.frame(ID=seq(1:32), 
                      file=c("id00b70b13.csv", "id079c763c.csv", "id1165e00c.csv", "id1c7e64ad.csv", "id1f372081.csv",
                             "id34e056c8.csv", "id37a54bbf.csv", "id3e3e50c7.csv", "id4ea159a8.csv", "id5308a7d6.csv",
                             "id5993bf4a.csv", "id650857ca.csv", "id687ab496.csv", "id7c20ee7a.csv", "id82b9735c.csv",
                             "id86237981.csv", "id8af5374b.csv", "id8e66893c.csv", "id9603e9c3.csv", "ida61e8ddf.csv",
                             "idabd0c53c.csv", "idb221f542.csv", "idbae5a811.csv", "idc735fc09.csv", "idc91a49d0.csv",
                             "idd80ac2b4.csv", "idecc9265e.csv", "id
                             f1ce9a0f.csv", "idf540d82b.csv", "idf5e3678b.csv",
                             "idfc5f05e4.csv", "idff99de96.csv"))

# Raw and processed accelerometry data
plot_rawdata <- function(subject_data, seconds=20){
  if (seconds<15) {
    stop("The minimum time interval for plotting is 15s.")
  } else {
    sub <- subject_data[,1:5] %>% filter(activity==1) %>%
      mutate(vector_magnitude = sqrt(lw_x^2+lw_y^2+lw_z^2))
    
    p1 <- sub %>%
      filter(time_s>=sub$time_s[1] & time_s<sub$time_s[1]+seconds) %>% rename(c("x"="lw_x", "y"="lw_y", "z"="lw_z")) %>%
      gather(key = Axis, value = Amplitude, 
             c("x", "y", "z")) %>% 
      ggplot(aes(x=(time_s-sub$time_s[1]), y = Amplitude, group = Axis, colour = Axis)) + 
      geom_line(size=0.3) +
      facet_grid(Axis~., scales = "free_y") +
      theme_classic() +
      theme(plot.title = element_text(hjust=0.5)) +
      theme(legend.position = "none") +
      labs(x="Time (s)", y=expression("Acceleration" ~ "(" ~ m/s^2 ~ ")"))
    
    p2 <- sub %>% 
      filter(time_s>=sub$time_s[1] & time_s<sub$time_s[1]+seconds) %>%
      ggplot(aes(x=(time_s-sub$time_s[1]), y = vector_magnitude)) + 
      geom_line(size=0.3) +
      theme_classic() +
      theme(plot.title = element_text(hjust=0.5)) +
      theme(legend.position = "none") +
      geom_vline(xintercept=c(2, 3), color="darkorange", linetype="solid", size=0.6) +
      geom_vline(xintercept=c(10, 15), color="purple", linetype="solid", size=0.6) +
      labs(x="Time (s)", y=expression("Magnitude of Acceleration" ~ "(" ~ m/s^2 ~ ")"))
    
    p3 <- sub %>% 
      filter(time_s>=sub$time_s[1]+2 & time_s<sub$time_s[1]+3) %>%
      ggplot(aes(x=(time_s-sub$time_s[1]), y = vector_magnitude)) + 
      geom_line(size=0.5, color="darkorange") +
      theme_classic() +
      theme(plot.title = element_text(hjust=0.5)) +
      theme(legend.position = "none") +
      labs(x="Time (s)", y=expression("Magnitude of Acceleration" ~ "(" ~ m/s^2 ~ ")"))
    
    p4 <- sub %>% 
      filter(time_s>=sub$time_s[1]+10 & time_s<sub$time_s[1]+15) %>%
      ggplot(aes(x=(time_s-sub$time_s[1]), y = vector_magnitude)) + 
      geom_line(size=0.4, color="purple") +
      theme_classic() +
      theme(plot.title = element_text(hjust=0.5)) +
      theme(legend.position = "none") +
      labs(x="Time (s)", y=expression("Magnitude of Acceleration" ~ "(" ~ m/s^2 ~ ")"))
    library(ggpubr)
    ggarrange(p1, p3, p2, p4, ncol = 2, nrow = 2)
  }
}


# Translation of time series and creating 3D images
plot_3D <- function(subject_id, df_fit, time_lags=c(0.15, 0.3)){
  train_j <- 1:200
  df_train <- subset(df_fit, J %in% train_j)
  
  if (length(time_lags)!=2){
    stop("Two time lags are required.")
  } else{
    xdf <- xdft <- data.frame(time_group=seq(from=1, to=1*100, by=1), 
                              signal=as.vector(t(subset(df_train, id == subject_id)$smat_sub[3,])))
    xdft$time_group <- xdf$time_group+time_lags[1]*100
    xdf1 <- xdf %>%  mutate(Type = "Original") %>%
      bind_rows(xdft %>%
                  mutate(Type = "Time Lag"))
    
    p1 <- ggplot() + 
      geom_line(data=xdf, aes(x=time_group/100+2, y=signal, color="Original: v(s)", linetype = "Original: v(s)")) +
      geom_line(data=xdft, aes(x=time_group/100+2, y=signal, color="Lagged: v(s-u)", linetype = "Lagged: v(s-u)")) +
      scale_color_manual(values = c("Original: v(s)" = "black", "Lagged: v(s-u)" = "black")) +
      scale_linetype_manual(values=c("Original: v(s)"= "solid","Lagged: v(s-u)"="dashed")) +
      labs(linetype = "Time Series") +
      guides(color = "none")+theme_minimal()+
      labs(x="Time (s)", y=expression("Magnitude of Acceleration" ~ "(" ~ m/s^2 ~ ")")) +
      geom_vline(xintercept=2+time_lags[1]+0.01, color="brown2") +
      geom_point(data=xdf[xdf$time_group==time_lags[1]*100+1,], aes(x=time_lags[1]+0.01+2, y=signal), color="brown2", size=3) +
      geom_point(data=xdft[xdft$time_group==time_lags[1]*100+1,], aes(x=time_lags[1]+0.01+2, y=signal), color="brown2", size=3) +
      theme(legend.title=element_text(size=9), legend.text=element_text(size=7)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black")) +
      scale_x_continuous(breaks=c(2.00,2.15,2.30,2.45,2.60,2.75,2.90,3.05,3.20))
    
    xdf2 <- xdft2 <- data.frame(time_group=seq(from=1, to=1*100, by=1), 
                                signal=as.vector(t(subset(df_train, id == subject_id)$smat_sub[3,])))
    xdft2$time_group <- xdf2$time_group+time_lags[2]*100
    
    p2 <- ggplot()+ 
      geom_line(data=xdf2, aes(x=time_group/100+2, y=signal, color="Original: v(s)", linetype = "Original: v(s)")) +
      geom_line(data=xdft2, aes(x=time_group/100+2, y=signal, color="Lagged: v(s-u)", linetype = "Lagged: v(s-u)")) +
      scale_color_manual(values = c("Original: v(s)" = "black", "Lagged: v(s-u)" = "black")) +
      scale_linetype_manual(values=c("Original: v(s)"= "solid","Lagged: v(s-u)"="dashed")) +
      labs(linetype = "Time Series") +
      guides(color = "none")+theme_minimal()+
      labs(x="Time (s)", y=expression("Magnitude of Acceleration" ~ "(" ~ m/s^2 ~ ")")) +
      geom_vline(xintercept=2+time_lags[2]+0.01, color="brown2") +
      geom_point(data=xdf[xdf2$time_group==time_lags[2]*100+1,], aes(x=time_lags[2]+0.01+2, y=signal), color="brown2", size=3) +
      geom_point(data=xdft2[xdft2$time_group==time_lags[2]*100+1,], aes(x=time_lags[2]+0.01+2, y=signal), color="brown2", size=3) +
      theme(legend.title=element_text(size=9), legend.text=element_text(size=7)) +
      scale_x_continuous(breaks=c(2.00,2.15,2.30,2.45,2.60,2.75,2.90,3.05,3.20,3.35)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
    # full density
    get_density <- function(x, y, ...) {
      dens <- MASS::kde2d(x, y, ...)
      ix <- findInterval(x, dens$x)
      iy <- findInterval(y, dens$y)
      ii <- cbind(ix, iy)
      return(dens$z[ii])
    }
    library(viridis)
    
    xdfd1 <- xdftd1 <- data.frame(time_group=seq(from=1, to=200*100, by=1), 
                                  signal=as.vector(t(subset(df_train, id == subject_id)$smat_sub[1:200,])))
    xdftd1$time_group <- xdfd1$time_group+time_lags[1]*100
    xdf1d1 <- xdfd1 %>%  mutate(Type = "Original") %>%
      bind_rows(xdftd1 %>%
                  mutate(Type = "Time Lag"))
    
    xdf2d1 <- inner_join(xdfd1, xdftd1, by="time_group")
    names(xdf2d1) <- c("time_group", "s", "u")
    
    xdf2d1$density <- get_density(xdf2d1$u, xdf2d1$s, n=100)
    
    p3 <- ggplot(xdf2d1) +
      geom_point(aes(x=u, y=s, color=density)) +
      labs(x=expression("Magnitude of Acceleration from v(s-u)" ~ "(" ~ m/s^2 ~ ")"), 
           y=expression("Magnitude of Acceleration from v(s)" ~ "(" ~ m/s^2 ~ ")")) +
      scale_color_viridis(name = "Density", limits=c(0,2.0)) +
      theme_bw() + 
      geom_point(data=xdf2d1[xdf2d1$time_group==201+time_lags[1]*100,], aes(x=u, y=s), color="brown2", size=4) +
      theme(legend.title=element_text(size=9), legend.text=element_text(size=7)) +
      scale_x_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                         minor_breaks = seq(0, 3, 0.25)) +
      scale_y_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                         minor_breaks = seq(0, 3, 0.25)) 
    
    
    xdfd2 <- xdftd2 <- data.frame(time_group=seq(from=1, to=200*100, by=1), 
                                  signal=as.vector(t(subset(df_train, id == subject_id)$smat_sub[1:200,])))
    xdftd2$time_group <- xdfd2$time_group+time_lags[2]*100
    xdf1d2 <- xdfd2 %>%  mutate(Type = "Original") %>%
      bind_rows(xdftd2 %>%
                  mutate(Type = "Time Lag"))
    
    xdf2d2 <- inner_join(xdfd2, xdftd2, by="time_group")
    names(xdf2d2) <- c("time_group", "s", "u")
    
    xdf2d2$density <- get_density(xdf2d2$u, xdf2d2$s, n=100)
    
    p4 <- ggplot(xdf2d2) +
      geom_point(aes(x=u, y=s, color=density)) +
      labs(x=expression("Magnitude of Acceleration from v(s-u)" ~ "(" ~ m/s^2 ~ ")"), 
           y=expression("Magnitude of Acceleration from v(s)" ~ "(" ~ m/s^2 ~ ")")) +
      scale_color_viridis(name = "Density", limits=c(0,2.0)) +
      theme_bw() + 
      geom_point(data=xdf2d2[xdf2d2$time_group==201+time_lags[2]*100,], aes(x=u, y=s), color="brown2", size=4) +
      theme(legend.title=element_text(size=9), legend.text=element_text(size=7)) +
      scale_x_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                         minor_breaks = seq(0, 3, 0.25)) +
      scale_y_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                         minor_breaks = seq(0, 3, 0.25)) 
    library(ggpubr)
    ggarrange(p1+labs(title=paste("Time lag of ", time_lags[1], "s", sep="")), 
              p2+labs(title=paste("Time lag of ", time_lags[2], "s", sep="")), 
              p3, p4, ncol = 2, nrow = 2)
  }
}


# Grid cells with counts of observations

plot_counts <- function(subject_id, df_fit, time_lag=0.3){
  train_j <- 1:200
  df_train <- subset(df_fit, J %in% train_j)
  get_density <- function(x, y, ...) {
    dens <- MASS::kde2d(x, y, ...)
    ix <- findInterval(x, dens$x)
    iy <- findInterval(y, dens$y)
    ii <- cbind(ix, iy)
    return(dens$z[ii])
  }
  
  xdfd1 <- xdftd1 <- data.frame(time_group=seq(from=1, to=200*100, by=1), 
                                signal=as.vector(t(subset(df_train, id == subject_id)$smat_sub[1:200,])))
  xdftd1$time_group <- xdfd1$time_group+time_lag*100
  xdf1d1 <- xdfd1 %>%  mutate(Type = "Original") %>%
    bind_rows(xdftd1 %>%
                mutate(Type = "Time Lag"))
  xdf2d1 <- inner_join(xdfd1, xdftd1, by="time_group")
  names(xdf2d1) <- c("time_group", "s", "u")
  xdf2d1$density <- get_density(xdf2d1$u, xdf2d1$s, n=100)
  
  p1 <- ggplot(xdf2d1) +
    geom_point(aes(x=u, y=s, color=density)) +
    labs(x=expression("Magnitude of Acceleration from v(s-u)" ~ "(" ~ m/s^2 ~ ")"), 
         y=expression("Magnitude of Acceleration from v(s)" ~ "(" ~ m/s^2 ~ ")")) +
    scale_color_viridis(name = "Density", limits=c(0,2.0)) +
    theme_bw() + 
    theme(legend.title=element_text(size=9), legend.text=element_text(size=7)) +
    scale_x_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                       minor_breaks = seq(0, 3, 0.25)) +
    scale_y_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                       minor_breaks = seq(0, 3, 0.25)) 
  
  count <- xdf2d1 %>% mutate(cut_s = cut(s, breaks = seq(0, 3, by = 0.25), include.lowest = T),
                             cut_u = cut(u, breaks = seq(0, 3, by = 0.25), include.lowest = T)) %>% 
    drop_na() %>%
    count(cut_s, cut_u, .drop=FALSE) %>% mutate(per=n/sum(n))
  countp <- count %>% mutate(pern=case_when(per>=0.001~1, per<0.001~2))
  countp$n <- ifelse(countp$pern==2, 0, countp$n)
  
  # colored grids
  p2 <- ggplot(data=count, aes(x=cut_u, y=cut_s)) +
    geom_tile(aes(fill=n), color="grey", size=0.3) +
    labs(x=expression("Magnitude of Acceleration from v(s-u)" ~ "(" ~ m/s^2 ~ ")"), 
         y=expression("Magnitude of Acceleration from v(s)" ~ "(" ~ m/s^2 ~ ")"),
         fill="Number of Observations") +
    geom_text(aes(label = n), color = "white", size = 2.5) +
    scale_fill_gradientn(colors = c("white", "#443A83","#27AD81", "#FDE333"), values=c(0, 0.0001, 0.5, 1))
  
  p3 <- ggplot(data=countp, aes(x=cut_u, y=cut_s)) +
    geom_tile(aes(fill=n), color="grey", size=0.3) +
    labs(x=expression("Magnitude of Acceleration from v(s-u)" ~ "(" ~ m/s^2 ~ ")"), 
         y=expression("Magnitude of Acceleration from v(s)" ~ "(" ~ m/s^2 ~ ")"),
         fill="Key Predictors") +
    geom_text(aes(label = n), color = "white", size = 2.5) +
    scale_fill_gradientn(colors = c("white", "#453781","#27AD81","#FDE333"), values=c(0, 0.0001, 0.5, 1))
  
  library(ggpubr)
  ggarrange(p1+labs(title=paste("Time lag of ", time_lag, "s", sep=""))+theme(axis.title=element_text(size=8)), 
            p2+labs(title="Counts in each grid cell")+theme(axis.text=element_text(size=3.2), axis.title=element_text(size=8)), 
            p3+labs(title=paste("Key predictors for subject ", subject_id, sep=""))+theme(axis.text=element_text(size=3.2), axis.title=element_text(size=8)), 
            ncol = 3, nrow = 1, legend="none")
}

