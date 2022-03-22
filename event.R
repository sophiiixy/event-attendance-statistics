library(readxl)
library(tidyverse)

event <- read.csv("C:/Users/sophi/Desktop/81244710872 - Attendee Report (1).csv")


hist(event$Time.in.Session..minutes., main="Histogram for Time in Event",
     xlab = "Time in Event (min)", ylab = "Frequency of attendees")
summary(event$Time.in.Session..minutes.)


df <- data.frame(
  group = c("Attended", "Not Attended"),
  value = c(34, 66)
)

bp<- ggplot(df, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")
bp

pie <- bp + coord_polar("y", start=0)
pie

pie + scale_fill_brewer("Blues") + 
  theme(axis.text.x=element_blank())+
  theme_bw() 



tab <- as.data.frame(table(event$How.did.you.hear.about.this.event.,event$University))
tab$Prop <- round(tab$Freq / 111 * 100,1)
colnames(tab)[1] <- "How did you hear about this event?"
colnames(tab)[2] <- "University"

ggplot(data=tab, aes(x= reorder(`How did you hear about this event?`, Prop)), 
                                 y=Prop,
                                 group=University,
                                 fill=University) +
  geom_bar(stat="identity", aes(y=Prop)) +
  coord_flip() +
  ylim(0,100) +
  #  geom_text(aes(label=Prop),hjust=-0.3)+
  xlab("How did you hear about this event?")+
  ylab("Proportion (%) of attendees responded")+
  theme_bw() +
  theme(text = element_text(size = 20)) +
 # scale_color_manual(values = c("#a4c639", "#007AFF")) +
 # scale_fill_manual("Device Type", values = c("Android"="#a4c639", "iOS"="#007AFF"))+
  theme(legend.position="bottom")


tab2 <- as.data.frame(table(event$University))
tab2$Prop <- round(tab2$Freq / 111 * 100,1)
colnames(tab2)[1] <- "University"

ggplot(data=tab2, aes(x= reorder(University, Prop)), 
       y=Prop,
       group=University,
       fill=University) +
  geom_bar(stat="identity", aes(y=Prop)) +
  coord_flip() +
  ylim(0,100) +
  #  geom_text(aes(label=Prop),hjust=-0.3)+
  xlab("University")+
  ylab("Proportion (%) of attendees")+
  theme_bw() +
  theme(text = element_text(size = 20)) +
  # scale_color_manual(values = c("#a4c639", "#007AFF")) +
  # scale_fill_manual("Device Type", values = c("Android"="#a4c639", "iOS"="#007AFF"))+
  theme(legend.position="bottom")



tab2 <- as.data.frame(table(event$University))
tab2$Prop <- round(tab2$Freq / 111 * 100,1)
colnames(tab2)[1] <- "University"

ggplot(data=tab2, aes(x= reorder(University, Prop)), 
       y=Prop,
       group=University,
       fill=University) +
  geom_bar(stat="identity", aes(y=Prop)) +
  coord_flip() +
  ylim(0,100) +
  #  geom_text(aes(label=Prop),hjust=-0.3)+
  xlab("University")+
  ylab("Proportion (%) of attendees")+
  theme_bw() +
  theme(text = element_text(size = 20)) +
  # scale_color_manual(values = c("#a4c639", "#007AFF")) +
  # scale_fill_manual("Device Type", values = c("Android"="#a4c639", "iOS"="#007AFF"))+
  theme(legend.position="bottom")






df <- data.frame(
  group = c("Alumnus/Professional", "MSc", "PharmD",
            "PhD","Postdoctoral Fellow",
            "Undergraduate"),
  value = c(15, 25,4,41,6,7 )
)

bp<- ggplot(df, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")
bp

pie <- bp + coord_polar("y", start=0)
pie

cbPalette <- c("#999999", "#56B4E9","#F0E442", "#0072B2", "#D55E00", "#CC79A7")

pie  +  scale_fill_manual(values=cbPalette)+
  theme(axis.text.x=element_blank())+
  theme_bw() 

library(tidyverse)



  
`Geolocation of attendees` <- c("Canada", "US", "India")
`Proportion of attendees` <- c(96, 3, 1)
longitude <- c(-96,	-100,77)
latitude <- c(54, 40,20)
	

df <- data.frame(`Geolocation of attendees`, `Proportion of attendees`,longitude, latitude)

world <- map_data("world")

ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region)) +
  theme_bw() +
  geom_point(
    data = df,
    aes(longitude, latitude, 
        color = `Geolocation of attendees`,
        size=`Proportion of attendees`),
    alpha = 0.5
  )