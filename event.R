library(readxl)
library(tidyverse)

#### fill in the directory and file name
event <- read.csv(".csv")

# produce histogram for time in session
hist(event$Time.in.Session..minutes., main="Histogram for Time in Event",
     xlab = "Time in Event (min)", ylab = "Frequency of attendees")
summary(event$Time.in.Session..minutes.)

# produce pie chart for attended vs. not attended students
df <- data.frame(
  group = c("Attended", "Not Attended"),
#### fill in the number
  value = c(, )
)

bp<- ggplot(df, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")

pie <- bp + coord_polar("y", start=0)

pie + scale_fill_brewer("Blues") + 
  theme(axis.text.x=element_blank())+
  theme_bw() 


# produce bar chart to show which channels attendees hear about this event from
tab <- as.data.frame(table(event$How.did.you.hear.about.this.event.))

tab$Prop <- round(tab$Freq / nrow(event) * 100,1)
colnames(tab)[1] <- "How did you hear about this event?"

ggplot(data=tab, aes(x= reorder(`How did you hear about this event?`, Prop)), 
                                 y=Prop) +
  geom_bar(stat="identity", aes(y=Prop)) +
  coord_flip() +
  ylim(0,100) +
  xlab("How did you hear about this event?")+
  ylab("Proportion (%) of attendees responded")+
  theme_bw() +
  theme(text = element_text(size = 20)) +
  theme(legend.position="bottom")

# produce bar chart to show which university attended by attendees 
tab2 <- as.data.frame(table(event$University))
tab2$Prop <- round(tab2$Freq / nrow(event) * 100,1)
colnames(tab2)[1] <- "University"

ggplot(data=tab2, aes(x= reorder(University, Prop)), 
       y=Prop) +
  geom_bar(stat="identity", aes(y=Prop)) +
  coord_flip() +
  ylim(0,100) +
  xlab("University")+
  ylab("Proportion (%) of attendees")+
  theme_bw() +
  theme(text = element_text(size = 20)) +
  theme(legend.position="bottom")

# produce pie chart for attendee professional
df2 <- data.frame(
  group = c("Alumnus/Professional", "MSc", "PharmD",
            "PhD","Postdoctoral Fellow",
            "Undergraduate"),
  value = c(15, 25,4,41,6,7 )
)

bp2 <- ggplot(df2, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")

pie2 <- bp2 + coord_polar("y", start=0)

# select a colour-blind-friendly colour scheme 
cbPalette <- c("#999999", "#56B4E9","#F0E442", "#0072B2", "#D55E00", "#CC79A7")

pie2  +  scale_fill_manual(values=cbPalette)+
  theme(axis.text.x=element_blank())+
  theme_bw() 

# produce a world map to visulize where attendees were joining from
`Geolocation of attendees` <- c("Canada", "US", "India")
`Proportion of attendees` <- c(96, 3, 1)
longitude <- c(-96,-100,77)
latitude <- c(54, 40,20)
	
df3 <- data.frame(`Geolocation of attendees`, `Proportion of attendees`,longitude, latitude)

world <- map_data("world")

ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region)) +
  theme_bw() +
  geom_point(
    data = df3,
    aes(longitude, latitude, 
        color = `Geolocation of attendees`,
        size=`Proportion of attendees`),
    alpha = 0.5
  )
