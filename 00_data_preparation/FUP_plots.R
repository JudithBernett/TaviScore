source("00_data_preparation/create_time2event.R")

#make time plots, FUP distribution
timeTable <- timeTable[order(-timeTable$time)]
timeTable <- timeTable[!duplicated(timeTable$Patient)]

time2 <- data.table(time = timeTable$time, label = "Longest available\nfollow up")
time2 <- rbind(time2, data.table(time = allTimeTable$time, label = "Total number of\nfollow ups per\ntime interval"))

#Longest available FUP and Total Nr of FUPs in one plot
ggplot()+
  stat_density(data = time2[time2$label == "Longest available\nfollow up",],geom = "line", aes(x = time, color = "Longest available\nfollow up"))+
  stat_density(data = time2[time2$label == "Total number of\nfollow ups per\ntime interval",],geom = "line", aes(x = time, color = "Total number of\nfollow ups per\ntime interval"))+
  scale_color_manual(values = c("Longest available\nfollow up" = "#D55E00", "Total number of\nfollow ups per\ntime interval" = "#0072B2"), name = "")+
  scale_x_continuous(breaks =  c(30,365,730,1095, 1460, 1825,2190), 
                     labels = c("30 days", "1 year", "2 years", "3 years", "4 years", "5 years", "6 years"))+
  labs(x = "", y = "Density") +
  theme_bw()+
  theme(text = element_text(size=20)) 
#ggsave("/Users/judith_bernett/Desktop/Bachelorarbeit/Plots/density_time_distrCondensed.png",width = 12, height = 7)

#Only longest available FUP
ggplot(timeTable, aes(x = time))+
  stat_density(geom = "line", color = "dodgerblue2")+
  scale_x_continuous(name = "Longest available follow up",breaks =  c(30,365,730,1095, 1460, 1825,2190), 
                     labels = c("30 days", "1 year", "2 years", "3 years", "4 years", "5 years", "6 years"))+
  labs(y = "Density") +
  theme_bw()+
  theme(text = element_text(size=20)) 
#ggsave("/Users/judith_bernett/Desktop/Bachelorarbeit/Plots/density_time_distr.png",width = 12, height = 7)

#Only total nr of FUPs
ggplot(allTimeTable, aes(x = time))+
  stat_density(geom = "line", color = "dodgerblue2")+
  scale_x_continuous(name = "Total number of follow ups per time interval",breaks =  c(30,365,730,1095, 1460, 1825,2190), 
                     labels = c("30 days", "1 year", "2 years", "3 years", "4 years", "5 years", "6 years"))+
  labs(y = "Density") +
  theme_bw()+
  theme(text = element_text(size=20)) 
#ggsave("/Users/judith_bernett/Desktop/Bachelorarbeit/Plots/density_Alltime_distr.png",width = 12, height = 7)

