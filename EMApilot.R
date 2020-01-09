if (!require(tidyverse)) {install.packages('tidyverse')}
if (!require(irr)) {install.packages('irr')}
if (!require(scales)) {install.packages('scales')}
if (!require(lubridate)) {install.packages('lubridate')}
if (!require(hms)) {install.packages('hms')}
if (!require(ggstance)) {install.packages('ggstance')}
if (!require(ggrepel)) {install.packages('ggrepel')}
if (!require(janitor)) {install.packages('janitor')}
if (!require(psych)) {install.packages('psych')}
if (!require(rstan)) {install.packages('rstan')}
if (!require(ctsem)) {install.packages('ctsem')}
if (!require(googledrive)) {install.packages('googledrive')}


#colorblind friendly palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


EMAd <- read.csv2("survey_responses_4865.csv")

colnames(EMAd)

EMAd <- EMAd[,c(1,5,6,8:35)]

colnames(EMAd)

colnames(EMAd) <- c("ID","time","status", "ISvalence","ISarousal","ISdominance", "ISstress", "ISautonomy", "IScompetence", "ISsocial","EventBOO", "TimeSinceEvent", "Rvalence", "Rarousal","Rdominance", "Rstress", "Rautonomy","Rcompentece","Rsocial", "rumination", "distraction","reappraisall","suppression","sharing","acceptance","active.coping","planning", "instrumental.support","substance.use","disengagement","event.description")

#add day variable
EMAd$day <- rep("NA",113)
EMAd$day[grepl("2019-11-26",EMAd$time)] <- 1
EMAd$day[grepl("2019-11-27",EMAd$time)] <- 2
EMAd$day[grepl("2019-11-28",EMAd$time)] <- 3
EMAd$day[grepl("2019-11-29",EMAd$time)] <- 4

levels(EMAd$ISvalence) <- c("2", "0", "1", "2", "3", "4", "1", "2", "3", "4")
levels(EMAd$ISarousal) <- c("2", "1", "2", "3", "1", "2", "3")
levels(EMAd$ISdominance) <- c("2", "1", "2", "3", "4", "1", "2", "3", "4")

cols.num <- c("ISvalence", "ISarousal", "ISdominance")


EMAd[cols.num] <- sapply(EMAd[cols.num],as.character)
EMAd[cols.num] <- sapply(EMAd[cols.num],as.numeric)

#replacing with missing values, since Ethica codes responses to default in these cases
EMAd$ISvalence[EMAd$status=="Expired" | EMAd$status == "Blocked" & is.na(EMAd$ISstress)] <- NA
EMAd$ISarousal[EMAd$status=="Expired" | EMAd$status == "Blocked" & is.na(EMAd$ISstress)] <- NA
EMAd$ISdominance[EMAd$status=="Expired" | EMAd$status == "Blocked" & is.na(EMAd$ISstress)] <- NA

#Fixing one observation (Ethica marks as expired if user does not finnish themselves)
EMAd$ISvalence[34] <- 2
EMAd$ISarousal[34] <- 2
EMAd$ISdominance[34] <- 2

EMAd$status <- sapply(EMAd$status, as.character)
EMAd$status[34] <- 0

if (!require(gridExtra)) {install.packages('gridExtra')} #alternatively cowplot


plot1 <- ggplot(EMAd, aes(EMAd$ISvalence))+
  geom_bar()+
  scale_x_continuous(limits=c(-1,5), breaks=c(0,1,2,3,4))

plot2 <- ggplot(EMAd, aes(EMAd$ISarousal))+
  geom_bar()+
  scale_x_continuous(limits=c(-1,5), breaks=c(0,1,2,3,4))

plot3 <- ggplot(EMAd, aes(EMAd$ISdominance))+
  geom_bar()+
  scale_x_continuous(limits=c(-1,5), breaks=c(0,1,2,3,4))

#ggplot returns warning for the missing values

grid.arrange(plot1,plot2,plot3, ncol=3)

levels(EMAd$EventBOO) <- c("No","Yes","No")
nrow(EMAd[EMAd$EventBOO=="Yes",])
#13 is correct

#Recall emotion state variables

levels(EMAd$Rvalence) <- c("2", "0", "3", "4", "0", "1", "3", "4")
levels(EMAd$Rarousal) <- c("2", "1", "3", "4", "1", "3")
levels(EMAd$Rdominance) <- c("2", "0", "3", "1", "2", "3")

cols.num <- c("Rvalence", "Rarousal", "Rdominance")

EMAd[cols.num] <- sapply(EMAd[cols.num],as.character)
EMAd[cols.num] <- sapply(EMAd[cols.num],as.numeric)

#replacing with missing values, since Ethica codes responses to default in these cases
EMAd$Rvalence[EMAd$EventBOO=="No"] <- NA
EMAd$Rarousal[EMAd$EventBOO=="No"] <- NA
EMAd$Rdominance[EMAd$EventBOO=="No"] <- NA


plot1 <- ggplot(EMAd, aes(EMAd$Rvalence))+
  geom_bar()+
  scale_x_continuous(limits=c(-1,5), breaks=c(0,1,2,3,4))

plot2 <- ggplot(EMAd, aes(EMAd$Rarousal))+
  geom_bar()+
  scale_x_continuous(limits=c(-1,5), breaks=c(0,1,2,3,4))

plot3 <- ggplot(EMAd, aes(EMAd$Rdominance))+
  geom_bar()+
  scale_x_continuous(limits=c(-1,5), breaks=c(0,1,2,3,4))

#ggplot returns warning for the missing values

grid.arrange(plot1,plot2,plot3, ncol=3)

EMAd[EMAd$status == c("Blocked","Expired"),]

#Fix initial values for stress and needs variables
EMAd$ISstress[EMAd$status!="Expired" & EMAd$status != "Blocked" & is.na(EMAd$ISstress)==TRUE] <- 50
EMAd$ISautonomy[EMAd$status!="Expired" & EMAd$status != "Blocked" & is.na(EMAd$ISautonomy)==TRUE] <- 50
EMAd$IScompetence[EMAd$status!="Expired" & EMAd$status != "Blocked" & is.na(EMAd$IScompetence)==TRUE] <- 50 
EMAd$ISsocial[EMAd$status!="Expired" & EMAd$status != "Blocked" & is.na(EMAd$ISsocial)==TRUE]  <- 50

EMAd$Rstress[EMAd$EventBOO == "Yes" & is.na(EMAd$Rstress)==TRUE] <- 50
EMAd$Rautonomy[EMAd$EventBOO == "Yes" & is.na(EMAd$Rautonomy) == TRUE] <- 50
EMAd$Rcompentece[EMAd$EventBOO == "Yes" & is.na(EMAd$Rcompentece) == TRUE] <- 50
EMAd$Rsocial[EMAd$EventBOO == "Yes" & is.na(EMAd$Rsocial)== TRUE] <- 50
  
#Fix initial values for ER and coping variables (initial 0)

EMAd$rumination[EMAd$EventBOO == "Yes" & is.na(EMAd$rumination)==TRUE] <- 0
EMAd$distraction[EMAd$EventBOO == "Yes" & is.na(EMAd$distraction)==TRUE] <- 0
EMAd$reappraisall[EMAd$EventBOO == "Yes" & is.na(EMAd$reappraisall)==TRUE] <- 0
EMAd$suppression[EMAd$EventBOO == "Yes" & is.na(EMAd$suppression)==TRUE] <- 0  
EMAd$sharing[EMAd$EventBOO == "Yes" & is.na(EMAd$sharing)==TRUE] <- 0
EMAd$acceptance[EMAd$EventBOO == "Yes" & is.na(EMAd$acceptance)==TRUE] <- 0

EMAd$active.coping[EMAd$EventBOO == "Yes" & is.na(EMAd$active.coping)==TRUE] <- 0
EMAd$planning[EMAd$EventBOO == "Yes" & is.na(EMAd$planning)==TRUE] <- 0
EMAd$instrumental.support[EMAd$EventBOO == "Yes" & is.na(EMAd$instrumental.support)==TRUE] <- 0
EMAd$substance.use[EMAd$EventBOO == "Yes" & is.na(EMAd$substance.use)==TRUE] <- 0
EMAd$disengagement[EMAd$EventBOO == "Yes" & is.na(EMAd$disengagement)==TRUE] <- 0


pairs(EMAd[20:25])
pairs.panels(EMAd[20:25])
# time for descriptives! :) ####

EMAd.nm <- EMAd[EMAd$status != "Expired" & EMAd$status != "Blocked",]


#Retrospective event assessment as timepoint

EMA1 <- EMAd.nm

EMA1$time <- as.POSIXlt(EMA1$time)
EMA1$TimeSinceEvent <- EMA1$TimeSinceEvent * 60
EMA1$newtime <- EMA1$time-EMA1$TimeSinceEvent

EMA1r <- EMA1[c(1,11:33)]
EMA1r <- filter(EMA1r, EMA1r$EventBOO == "Yes")

oldnames <- c("Rvalence", "Rarousal", "Rdominance", "Rstress", "Rautonomy", "Rcompentece", "Rsocial", "newtime")
newnames <- c("ISvalence", "ISarousal", "ISdominance", "ISstress", "ISautonomy", "IScompetence", "ISsocial", "time")

EMA1r <- EMA1r %>% rename_at(vars(oldnames), ~ newnames)
EMA1r$time <- as.POSIXct(EMA1r$time)
EMA1$time <- as.POSIXct(EMA1$time)
EMA1r$retro <- paste("event")
EMA1$retro <- paste("in.situ")
EMA.merged <- full_join(EMA1, EMA1r)
EMA.merged[EMA.merged$retro=="retro",]

EMA.merged <- EMA.merged %>%
  group_by(., ID) %>%
  arrange(., time, .by_group=TRUE)

EMA.merged$ID <- recode(EMA.merged$ID, "User #19350" = "19350", "User #19427" = "19427", "User #19436" = "19436", "User #19444" = "19444", "User #19445" = "19445", "User #19453" = "19453")

######################################### THIS TO -->

testi1 <- EMA.merged %>%
  group_by(., ID) %>%
  mutate(TP = seq(1:length(ID)))

ok <- select(testi1, TP,retro)
names(ok)[names(ok) == "TP"] <- "time"
ok$time <- as.factor(ok$time)
ok$ID <- recode(ok$ID, "User #19350" = "19350", "User #19427" = "19427", "User #19436" = "19436", "User #19444" = "19444", "User #19445" = "19445", "User #19453" = "19453")
ok$time <- factor(ok$time, levels = ok$time[1:23])


testi12 <- select(testi1, ID, TP, ISvalence, ISarousal, ISdominance, retro, day)

#create dataframe where you can easily plot all variables from

testi12$ID <- recode(testi12$ID, "User #19350" = "19350", "User #19427" = "19427", "User #19436" = "19436", "User #19444" = "19444", "User #19445" = "19445", "User #19453" = "19453")


ISaffect <- valence <- testi12 %>%
  select(., ID, TP, ISvalence) %>%
  spread(., TP, ISvalence) %>%
  gather(time, ISvalence, 2:24, factor_key=FALSE)

#old 2:21


arousal <- testi12 %>%
  select(., ID, TP, ISarousal) %>%
  spread(., TP, ISarousal) %>%
  gather(time, ISarousal, 2:24, factor_key=FALSE)

dominance <- testi12 %>%
  select(., ID, TP, ISdominance) %>%
  spread(., TP, ISdominance) %>%
  gather(time, ISdominance, 2:24, factor_key=FALSE)

ISaffect$ISarousal <- arousal$ISarousal
ISaffect$ISdominance <- dominance$ISdominance

ISaffect <- ISaffect[order(ISaffect$ID),]
ISaffect$time <- as.factor(ISaffect$time)
ISaffect$time <- factor(ISaffect$time, levels = ISaffect$time[1:23])

ISaffectANNO <- full_join(ISaffect, ok)
colnames(EMA.merged)[2] <- "timePOSIX"

time.merge <- select(EMA.merged, ID, timePOSIX, day)
time.merge$ID <- recode(time.merge$ID, "User #19350" = "19350", "User #19427" = "19427", "User #19436" = "19436", "User #19444" = "19444", "User #19445" = "19445", "User #19453" = "19453")
time.merge <- time.merge %>%
  group_by(., ID) %>%
  mutate(time = row_number())
time.merge$time <- as.factor(time.merge$time)

ISaffectANNO <- left_join(ISaffectANNO, time.merge)
### THIS IS BASICALLY USELESS AS WE HAVE EMA.merged ALREADY!

#pd <- position_jitter(0.1, 0.1)
pd <- position_dodge(0.1)
#pd <- ggstance::position_dodgev(height = 0.2,)

ISaffectANNO <- drop_na(ISaffectANNO)
ISaffectANNO$ISvalence.jittered <- jitter(ISaffectANNO$ISvalence, amount=0.05)
ISaffectANNO$ISarousal.jittered <- jitter(ISaffectANNO$ISarousal, amount=0.05)
ISaffectANNO$ISdominance.jittered <- jitter(ISaffectANNO$ISdominance, amount=0.05)
levels(ISaffectANNO$retro)

EMA.merged$ISvalence.jittered <- jitter(EMA.merged$ISvalence, amount=0.05)
EMA.merged$ISarousal.jittered <- jitter(EMA.merged$ISarousal, amount=0.05)
EMA.merged$ISdominance.jittered <- jitter(EMA.merged$ISdominance, amount=0.05)
EMA.merged$ISstress.jittered <- jitter(EMA.merged$ISstress, amount=0.05)
EMA.merged$ISautonomy.jittered <- jitter(EMA.merged$ISautonomy, amount=0.05)
EMA.merged$IScompetence.jittered <- jitter(EMA.merged$IScompetence, amount=0.05)
EMA.merged$ISsocial.jittered <- jitter(EMA.merged$ISsocial, amount=0.05)

plot1 <- ggplot(ISaffectANNO, aes(x=timePOSIX, y=ISvalence.jittered, color=ID)) +
  geom_line(aes(group=ID))+
  geom_point(aes(group=ID))+
  scale_x_datetime(breaks = "1 hour", date_labels = "%H")+
  facet_grid(. ~ day, scales="free_x")+
  theme(aspect.ratio = 0.9, legend.position = "none")+
  geom_label_repel(data=subset(ISaffectANNO, retro=="event"),
                   aes(label=retro), size=2.5, alpha=0.6, label.padding=0.2,
                   nudge_y=0.2, nudge_x = 1000)+
  ylim(-1,5)

#2 data subsets to nudge_y over or under depending on y value??

#geom_text(aes(label=ifelse(retro=="retro",as.character("event"),'')),hjust=0,vjust=-.3)+


plot2 <- ggplot(ISaffectANNO, aes(x=timePOSIX, y=ISarousal.jittered, color=ID)) +
  geom_line(aes(group=ID))+
  geom_point(aes(group=ID))+
  scale_x_datetime(breaks = "1 hour", date_labels = "%H")+
  facet_grid(. ~ day, scales="free_x")+
  theme(aspect.ratio = 0.9, legend.position="none")+
  geom_label_repel(data=subset(ISaffectANNO, retro=="event"),
                   aes(label=retro), size=2.5, alpha=0.6, label.padding=0.2,
                   nudge_y=0.2, nudge_x = 1000)+
  ylim(-1,5)

plot3 <- ggplot(ISaffectANNO, aes(x=timePOSIX, y=ISdominance.jittered, color=ID)) +
  geom_line(aes(group=ID))+
  geom_point(aes(group=ID))+
  scale_x_datetime(breaks = "1 hour", date_labels = "%H")+
  facet_grid(. ~ day, scales="free_x")+
  theme(aspect.ratio = 0.9, legend.position = "none")+
  geom_label_repel(data=subset(ISaffectANNO, retro=="event"),
                   aes(label=retro), size=2.5, alpha=0.6, label.padding=0.2,
                   nudge_y=0.2, nudge_x = 1000)+
  ylim(-1,5)

plot4 <- ggplot(EMA.merged, aes(x=timePOSIX, y=ISstress.jittered, color=ID)) +
  geom_line(aes(group=ID))+
  geom_point(aes(group=ID))+
  scale_x_datetime(breaks = "1 hour", date_labels = "%H")+
  facet_grid(. ~ day, scales="free_x")+
  theme(aspect.ratio = 0.9, legend.position = "none")+
  geom_label_repel(data=subset(EMA.merged, retro=="event"),
                   aes(label=retro), size=2.5, alpha=0.6, label.padding=0.2,
                   nudge_y=0.2, nudge_x = 1000)+
  ylim(-1,101)

plot5 <- ggplot(EMA.merged, aes(x=timePOSIX, y=ISautonomy.jittered, color=ID)) +
  geom_line(aes(group=ID))+
  geom_point(aes(group=ID))+
  scale_x_datetime(breaks = "1 hour", date_labels = "%H")+
  facet_grid(. ~ day, scales="free_x")+
  theme(aspect.ratio = 0.9, legend.position = "none")+
  geom_label_repel(data=subset(EMA.merged, retro=="event"),
                   aes(label=retro), size=2.5, alpha=0.6, label.padding=0.2,
                   nudge_y=0.2, nudge_x = 1000)+
  ylim(-1,101)

plot6 <- ggplot(EMA.merged, aes(x=timePOSIX, y=IScompetence.jittered, color=ID)) +
  geom_line(aes(group=ID))+
  geom_point(aes(group=ID))+
  scale_x_datetime(breaks = "1 hour", date_labels = "%H")+
  facet_grid(. ~ day, scales="free_x")+
  theme(aspect.ratio = 0.9, legend.position = "none")+
  geom_label_repel(data=subset(EMA.merged, retro=="event"),
                   aes(label=retro), size=2.5, alpha=0.6, label.padding=0.2,
                   nudge_y=0.2, nudge_x = 1000)+
  ylim(-1,101)

plot7 <- ggplot(EMA.merged, aes(x=timePOSIX, y=ISsocial.jittered, color=ID)) +
  geom_line(aes(group=ID))+
  geom_point(aes(group=ID))+
  scale_x_datetime(breaks = "1 hour", date_labels = "%H")+
  facet_grid(. ~ day, scales="free_x")+
  theme(aspect.ratio = 0.9, legend.position = c(1.5,0.15), legend.direction = "horizontal")+
  geom_label_repel(data=subset(EMA.merged, retro=="event"),
                   aes(label=retro), size=2.5, alpha=0.6, label.padding=0.2,
                   nudge_y=0.2, nudge_x = 1000)+
  ylim(-1,101)

grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, nrow=4, ncol=2)

#geom_text(aes(label=hms::as.hms(timePOSIX)),hjust=0, vjust=0, size=3)
#scale_x_time(breaks = hms::as.hms(ISaffectANNO$timePOSIX), date_breaks())

##problem: positioning fucks up lines and points  
  

## THESE PLOTS ARE NOT NEEDED ####

plot1 <- ggplot(ISaffectANNO, aes(x=time, y=ISvalence, color=ID))+
  geom_line(aes(group=ID), position=pd)+
  geom_point(position=pd)+
  ylim(-1,5)+
  theme(legend.position = "none")+
  geom_text(aes(label=ifelse(retro=="retro",as.character("event"),'')),hjust=0,vjust=0)

plot2 <- ggplot(ISaffectANNO, aes(x=time, y=ISarousal, color=ID))+
  geom_line(aes(group=ID), position=pd)+
  geom_point(position=pd)+
  ylim(-1,5) + 
  theme(legend.position = "none")+
  geom_text(aes(label=ifelse(retro=="retro",as.character("event"),'')),hjust=0,vjust=0)

plot3 <- ggplot(ISaffectANNO, aes(x=time, y=ISdominance, color=ID))+
  geom_line(aes(group=ID), position=pd)+
  geom_point(position=pd)+
  ylim(-1,5)+
  theme(legend.position = c(0.9,0.1))+
  geom_text(aes(label=ifelse(retro=="retro",as.character("event"),'')),hjust=0,vjust=0)

grid.arrange(plot1,plot2,plot3, ncol=3)

## END OF THESE PLOTS ARE NOT NEEDED ###

#per subject

subj1 <- filter(EMA.merged, ID == "19350")
subj1 <- select(subj1, ID, timePOSIX, day, retro, ISvalence, ISarousal, ISdominance, ISstress, ISautonomy, IScompetence, ISsocial)
subj1 <- gather(subj1, "variable", "value", 5:11)
subj1$variable <- type.convert(subj1$variable)
subj1 <- subj1 %>%
  group_by(., variable) %>%
  mutate(value=scale(value)) %>%
  mutate(value=jitter(value, amount=0.05))

plot1 <- ggplot(subj1, aes(x=timePOSIX, y=value, color=variable)) +
  geom_line(aes(group=variable),alpha=0.6)+
  geom_vline(data=subset(subj1, retro=="event"), aes(xintercept=as.numeric(c(timePOSIX))), linetype="dashed")+
  geom_point(aes(group=variable), size=3, alpha=0.6)+
  scale_x_datetime(breaks="1 hour", date_labels="%H")+
  facet_grid(. ~ day, scales="free_x")+
  theme(legend.position = "none", aspect.ratio = 0.9)+
  scale_colour_manual(values=cbPalette)+
  ylim(-4,4)

subj2 <- filter(EMA.merged, ID == "19427")
subj2 <- select(subj2, ID, timePOSIX, day, retro, ISvalence, ISarousal, ISdominance, ISstress, ISautonomy, IScompetence, ISsocial)
subj2 <- gather(subj2, "variable", "value", 5:11)
subj2$variable <- type.convert(subj2$variable)
subj2 <- subj2 %>%
  group_by(., variable) %>%
  mutate(value=scale(value)) %>%
  mutate(value=jitter(value, amount=0.05))

plot2 <- ggplot(subj2, aes(x=timePOSIX, y=value, color=variable)) +
  geom_line(aes(group=variable),alpha=0.7)+
  geom_vline(data=subset(subj2, retro=="event"), aes(xintercept=as.numeric(c(timePOSIX))), linetype="longdash")+
  geom_point(aes(group=variable), size=3,alpha=0.7)+
  scale_x_datetime(breaks="1 hour", date_labels="%H")+
  facet_grid(. ~ day, scales="free_x")+
  theme(legend.position = c(0.7,0.2), legend.direction = "horizontal",legend.title = element_blank(),
        legend.key = element_rect(colour = "transparent", fill = "white"), legend.background=element_blank(), aspect.ratio = 0.9)+
  scale_colour_manual(values=cbPalette)+
  ylim(-4, 4)


subj3 <- filter(EMA.merged, ID == "19436")
subj3 <- select(subj3, ID, timePOSIX, day, retro, ISvalence, ISarousal, ISdominance, ISstress, ISautonomy, IScompetence, ISsocial)
subj3 <- gather(subj3, "variable", "value", 5:11)
subj3$variable <- type.convert(subj3$variable)
subj3 <- subj3 %>%
  group_by(., variable) %>%
  mutate(value=scale(value)) %>%
  mutate(value=jitter(value, amount=0.05))

plot3 <- ggplot(subj3, aes(x=timePOSIX, y=value, color=variable)) +
  geom_line(aes(group=variable), alpha=0.7)+
  geom_vline(data=subset(subj3, retro=="event"), aes(xintercept=as.numeric(c(timePOSIX))), linetype="dashed")+
  geom_point(aes(group=variable), size=3, alpha=0.7)+
  scale_x_datetime(breaks="1 hour", date_labels="%H")+
  facet_grid(. ~ day, scales="free_x")+
  theme(legend.position = "none", aspect.ratio = 0.9)+
  scale_colour_manual(values=cbPalette)+
  ylim(-4, 4)


subj4 <- filter(EMA.merged, ID == "19444")
subj4 <- select(subj4, ID, timePOSIX, day, retro, ISvalence, ISarousal, ISdominance, ISstress, ISautonomy, IScompetence, ISsocial)
subj4 <- gather(subj4, "variable", "value", 5:11)
subj4$variable <- type.convert(subj4$variable)
subj4 <- subj4 %>%
  group_by(., variable) %>%
  mutate(value=scale(value)) %>%
  mutate(value=jitter(value, amount=0.05))

plot4 <- ggplot(subj4, aes(x=timePOSIX, y=value, color=variable)) +
  geom_line(aes(group=variable), alpha=0.7)+
  geom_vline(data=subset(subj4, retro=="event"), aes(xintercept=as.numeric(c(timePOSIX))), linetype="dashed")+
  geom_point(aes(group=variable), size=3, alpha=0.7)+
  scale_x_datetime(breaks="1 hour", date_labels="%H")+
  facet_grid(. ~ day, scales="free_x")+
  theme(legend.position = "none", aspect.ratio = 0.9)+
  scale_colour_manual(values=cbPalette)+
  ylim(-4, 4)

subj5 <- filter(EMA.merged, ID == "19445")
subj5 <- select(subj5, ID, timePOSIX, day, retro, ISvalence, ISarousal, ISdominance, ISstress, ISautonomy, IScompetence, ISsocial)
subj5 <- gather(subj5, "variable", "value", 5:11)
subj5$variable <- type.convert(subj5$variable)
subj5 <- subj5 %>%
  group_by(., variable) %>%
  mutate(value=scale(value)) %>%
  mutate(value=jitter(value, amount=0.05))

plot5 <- ggplot(subj5, aes(x=timePOSIX, y=value, color=variable)) +
  geom_line(aes(group=variable), alpha=0.7)+
  geom_vline(data=subset(subj5, retro=="event"), aes(xintercept=as.numeric(c(timePOSIX))), linetype="dashed")+
  geom_point(aes(group=variable), size=3, alpha=0.7)+
  scale_x_datetime(breaks="1 hour", date_labels="%H")+
  facet_grid(. ~ day, scales="free_x")+
  theme(legend.position = "none", aspect.ratio = 0.9)+
  scale_colour_manual(values=cbPalette)+
  ylim(-4, 4)

subj6 <- filter(EMA.merged, ID == "19453")
subj6 <- select(subj6, ID, timePOSIX, day, retro, ISvalence, ISarousal, ISdominance, ISstress, ISautonomy, IScompetence, ISsocial)
subj6 <- gather(subj6, "variable", "value", 5:11)
subj6$variable <- type.convert(subj6$variable)
subj6 <- subj6 %>%
  group_by(., variable) %>%
  mutate(value=scale(value)) %>%
  mutate(value=jitter(value, amount=0.05))

plot6 <- ggplot(subj6, aes(x=timePOSIX, y=value, color=variable)) +
  geom_line(aes(group=variable), alpha=0.7)+
  geom_vline(data=subset(subj6, retro=="event"), aes(xintercept=as.numeric(c(timePOSIX))), linetype="dashed")+
  geom_point(aes(group=variable),size=3, alpha=0.7)+
  scale_x_datetime(breaks="1 hour", date_labels="%H")+
  facet_grid(. ~ day, scales="free_x")+
  theme(legend.position = "none", aspect.ratio = 0.9)+
  scale_colour_manual(values=cbPalette)+
  ylim(-4, 4)

  grid.arrange(plot1,plot2,plot3,plot4,plot5,plot6, ncol=2)

## Add sleep diary and oura data to data set ####

#sleep diary data convert ###

sleep.diary <- readxl::read_excel("sleep diary.xlsx")
sleep.diary[,c(1,2)] <- sleep.diary[,c(1,2)] %>% mutate_if(sapply(., is.double), as.factor)
sleep.diary$typical.BOO <- as.factor(sleep.diary$typical.BOO)
sleep.diary$sleep.time.est <- as.POSIXct(sleep.diary$sleep.time.est)
sleep.diary$wake.time.est <- as.POSIXct(sleep.diary$wake.time.est)


#baseline questionnaire data convert ###

baseline <- read.csv2("baseline.csv")
baseline <- as_tibble(baseline)
baseline <- rename(baseline, ID = SubjectNumber.1)
baseline <- rename(baseline, autonomy = BPNSNF.3, compe = BPNSNF.7, support = BPNSNF.12)
colnames(baseline)[6:32] <- c("SNF.1",  "SNF.2",  "autonomy",  "SNF.3",  "SNF.4",  "SNF.5",  "compe", "SNF.6",  "SNF.7", "SNF.8", "SNF.9", "support", 
                              "SNF.10", "SNF.11", "SNF.12", "SNF.13", "SNF.14", "SNF.15", "SNF.16", "SNF.17", "SNF.18", "SNF.19", "SNF.20", "SNF.21", 
                              "SNF.22", "SNF.23", "SNF.24")


baseline <- baseline %>%
  group_by(., ID) %>%
  mutate(., autoSAT = (SNF.1+SNF.7+SNF.13+SNF.19)/4, autoFRU = (SNF.2+SNF.8+SNF.14+SNF.20)/4, relaSAT = (SNF.3+SNF.9+SNF.15+SNF.21)/4, 
         relaFRU = (SNF.4+SNF.10+SNF.16+SNF.22)/4, compSAT = (SNF.5+SNF.11+SNF.17+SNF.23)/4, compFRU = (SNF.6+SNF.12+SNF.18+SNF.24)/4)
pairs.panels(baseline[, 36:41])
cor(baseline[, c("SNF.1","SNF.7","SNF.13", "SNF.19", "SNF.2", "SNF.8", "SNF.14", "SNF.20", "autonomy")])


outlier <- filter(baseline, ID != 19350)

alpha(as.data.frame(baseline[,c("SNF.4", "SNF.10", "SNF.16", "SNF.22")]), check.keys = TRUE)

#items 3 7 12 are momentary


#Oura data convert ###

oura <- readxl::read_excel("Oura combined.xlsx")

oura[, c(1, 2, 3)] <- oura[, c(1, 2, 3)] %>%
  mutate_if(sapply(., is.character), as.factor)

oura$ID <- as.factor(oura$ID) 
oura$`BedTime Start` <- excel_numeric_to_date(as.numeric(oura$`BedTime Start`), include_time = TRUE)
oura$`Bedtime End` <-  excel_numeric_to_date(as.numeric(oura$`Bedtime End`), include_time = TRUE)

oura <- oura %>%
mutate_if(sapply(., is.character), as.numeric)
wtf <- oura[order(oura$ID, oura$day, decreasing=FALSE),]
wtf <- select(wtf, ID, day, `Awake Time`)
wtf$join <- paste(1)

### CTSEM ####

ctEMA <- EMA.merged %>%
  group_by(., ID) %>%
  mutate(time = timePOSIX - timePOSIX[1])

ctEMA <- select(ctEMA, ID, day, time, ISvalence, ISarousal, ISdominance, ISstress, ISautonomy, IScompetence, ISsocial, rumination, distraction, reappraisall, suppression, sharing, acceptance, active.coping, planning, instrumental.support, substance.use, disengagement, retro)
ctEMA[ctEMA$retro=="in.situ",11:21] <- NA

testaus <- ctEMA %>%
  group_by(., ID, day) %>%
  slice(1)
testaus$join <- paste(1)

testaus <- left_join(ctEMA, testaus)
testaus <- left_join(testaus, wtf)
BPNS <- select(baseline, ID, autoSAT, autoFRU, relaSAT, relaFRU, compSAT, compFRU)
BPNS$ID <- as.factor(BPNS$ID)

testaus <- left_join(testaus, BPNS)
testaus$ID <- recode(testaus$ID, "19350" = "1", "19427" = "2", "19436" = "3", "19444" = "4", "19445" = "5", "19453" = "6")
testaus$id <- testaus$ID
ctEMAdat <- as.data.frame(testaus)
colnames(ctEMAdat)

ctEMAdat <- select(ctEMAdat, "id", "time", "ISvalence", "ISarousal", "ISdominance", "ISstress", "ISautonomy", 
                   "IScompetence", "ISsocial", "rumination", "distraction", "reappraisall", "suppression", "sharing", 
                   "acceptance", "active.coping", "planning", "instrumental.support", "substance.use", "disengagement", "Awake Time",
                   "autoSAT", "autoFRU", "relaSAT", "relaFRU", "compSAT", "compFRU")
ctEMAdat$time <- type.convert(ctEMAdat$time)
ctEMAdat[3:27] <- lapply(ctEMAdat[3:27], function(x) c(scale(x)))
ctEMAdat$time <- ctEMAdat$time/60 #from sec to min

wideexample <- ctLongToWide(datalong = ctEMAdat, id = "id",
                            time = "time", manifestNames = c("ISvalence", "ISarousal", "ISdominance", "ISstress", "ISautonomy", "IScompetence", "ISsocial"),
                            TDpredNames = c("rumination", "distraction", "reappraisall", "suppression", "sharing", "acceptance", "active.coping", "planning",
                            "instrumental.support", "substance.use", "disengagement", "Awake Time"), TIpredNames = c("autoSAT", "autoFRU", "relaSAT", "relaFRU", "compSAT", "compFRU"))


wide <- ctIntervalise(datawide = wideexample, Tpoints = 23, n.manifest = 7,
                      n.TDpred = 12, n.TIpred = 6, manifestNames = c("ISvalence", "ISarousal", "ISdominance", "ISstress", "ISautonomy", "IScompetence", "ISsocial"),
                      TDpredNames = c("rumination", "distraction", "reappraisall", "suppression", "sharing", "acceptance", "active.coping", "planning",
                                      "instrumental.support", "substance.use", "disengagement", "Awake Time"), TIpredNames =  c("autoSAT", "autoFRU", "relaSAT", "relaFRU", "compSAT", "compFRU"))

longexample <- ctWideToLong(datawide = wide, Tpoints = 23, n.manifest = 7, n.TDpred = 12, n.TIpred = 6,
             manifestNames = c("ISvalence", "ISarousal", "ISdominance", "ISstress", "ISautonomy", "IScompetence", "ISsocial"), 
             TDpredNames = c("rumination", "distraction", "reappraisall", "suppression", "sharing", "acceptance", "active.coping", "planning",
                             "instrumental.support", "substance.use", "disengagement", "Awake Time"), TIpredNames = c("autoSAT", "autoFRU", "relaSAT", "relaFRU", "compSAT", "compFRU"))

ctEMAlong <- ctDeintervalise(datalong = longexample, id='id', dT='dT')

##weird error -> contact Driver
#saveRDS(ctEMAlong, "ctEMAlong.rds")


example1model <- ctModel(type='stanct',
                           n.latent=7, latentNames=c('eta1','eta2', "eta3", "eta4", "eta5", "eta6", "eta7"),
                           n.manifest=7, manifestNames=c("ISvalence", "ISarousal", "ISdominance", "ISstress", "ISautonomy", "IScompetence", "ISsocial"),
                           n.TDpred=12, TDpredNames=c("rumination", "distraction", "reappraisall", "suppression", "sharing", "acceptance", "active.coping", "planning",
                            "instrumental.support", "substance.use", "disengagement", "Awake Time"), 
                           n.TIpred=6, TIpredNames=c("autoSAT", "autoFRU", "relaSAT", "relaFRU", "compSAT", "compFRU"),
                           LAMBDA=diag(7))

#example1fit <- ctStanFit(datalong = ctEMAlong, ctstanmodel = example1model, optimize=FALSE, iter = 1000, chains = 3)

#saveRDS(example1fit, "pilot run 1000it.rds")
#find and download the file from google drive using drive_ls(path="EMA pilot")
#drive_ls(path="EMA pilot")
#drive_download("pilot1000ite.rds")
#example1fit <- readRDS("pilot1000iter.rds")

#very useful names(extract(fit4$stanfit)) -> can see "pars" to use in eg pairs() or traceplot()fit
#launch_shinystan(fit4$stanfit)
#print(fit5$stanfit, pars=ctStanParnames(fit5, "pop_")) #stanfit object has many additional parameters, hence pars

summary <- summary(example1fit)

summary(example1fit, parmatrices=TRUE)$parmatrices

options(max.print=10000)

ctStanContinuousPars(example1fit,subjects = "all", calcfunc = quantile, calcfuncargs = list(probs=.975)) 

fit2s <- extract(fit20)
mean(fit2s$MANIFESTMEANS)

#fit1 <- readRDS("pilot1000iter.rds")
#fit2 <- readRDS("02pilotrun1000it.rds") #does not work


model02 <- ctModel(type='stanct',
                   n.latent=3, latentNames=c('eta1','eta2', "eta3"),
                   n.manifest=3, manifestNames=c("ISvalence", "ISarousal", "ISdominance"),
                   n.TDpred=1, TDpredNames="Awake Time", 
                   n.TIpred=1, TIpredNames="compSAT",
                   LAMBDA=diag(3))

#fit2 <- ctStanFit(datalong = ctEMAlong, ctstanmodel = model02, optimize=FALSE, iter = 100, chains = 1, cores ="maxneeded", savesubjectmatrices= TRUE, verbose=0, plot=TRUE)

summary(fit2)

#fit3 <- ctStanFit(datalong = ctEMAlong, ctstanmodel = model02, optimize=FALSE, iter = 100, chains = 1, cores ="maxneeded", savesubjectmatrices= TRUE, verbose=0, plot=TRUE, control = list(adapt_delta = .95))
efit3 <- extract(fit3)

model3 <- ctModel(type='stanct',
                   latentNames=c('eta1',"eta2"),
                   manifestNames=c("Y1", "Y2"),
                   CINT=matrix(c("cint1", "cint2"), nrow=2, ncol=1),
                   MANIFESTMEANS=matrix(c(0,0),nrow=2,ncol=1),
                   TDpredNames="TD1",
                   TIpredName="TI1",
                   LAMBDA=diag(2))

model3$pars$indvarying[7] <- TRUE
model3$pars$transform[7] <- "2*param-1" #allowing for positive auto-effects -> explosive growth, but still negative offset (so process declines after spike)
plot(model3, rows=7, rawpopsd=2)

fit4 <- ctStanFit(datalong = ctEMAlong, ctstanmodel = model3, optimize=FALSE, iter = 300, chains = 3, cores ="maxneeded", plot=TRUE, control = list(adapt_delta = .99))

model4 <- ctModel(type='stanct',
                   n.latent=1, latentNames='eta1',
                   n.manifest=1, manifestNames="ISvalence",
                   n.TDpred=0, 
                   n.TIpred=1, TIpredNames="compSAT",
                   LAMBDA=diag(1))

fit5 <- ctStanFit(datalong = ctEMAlong, ctstanmodel = model4, optimize=FALSE, iter = 300, chains = 1, cores ="maxneeded", plot=TRUE)

### TÄSSÄ MENNÄÄN ###

molp <- extract(example1fit$stanfit)$lp__

lp <-  as.data.frame(lp)
hist(popmeans$V5)
hist(lp$lp)

stanf <- example1fit$stanfit

ctStanContinuousPars(example1fit,subjects = "all", calcfunc = quantile, calcfuncargs = list(probs=.975)) 

traceplot(example1fit$stanfit, pars = c("DRIFT[1,1,1]","DRIFT[1,2,1]","DRIFT[1,1,2]"))

plot.ctsemfit()

plotz <- ctStanDiscretePars(example1fit, indices = "CR", times = seq(from = 0, to = 2, by = 0.1), plot = TRUE)
ctStanDiscreteParsPlot(plotz, indices = "AR")
ctStanDiscretePars(example1fit)

ctStanKalman(example1fit, timerange = "asdata",
         timestep = "asdata", subjects = 5, plot = TRUE, kalmanvecc("y", "ysmooth"), legend=FALSE)
ctKalman(fit5, subjects = 6, kalmanvec=c('y', 'etaprior'), plot=TRUE, plotcontrol=list(xaxs='i', main = 'Predicted'))



example2model <- ctModel(type='stanct',
                         n.latent=3, latentNames=c('eta1','eta2', "eta3"),
                         n.manifest=3, manifestNames=c("ISvalence", "ISarousal", "ISdominance"),
                         n.TDpred=1, TDpredNames= "Awake Time", 
                         n.TIpred=2, TIpredNames=c("autoSAT", "autoFRU"),
                         LAMBDA=diag(3))

example2fit <- ctStanFit(datalong = ctEMAlong, ctstanmodel = example2model, optimize=FALSE, iter = 1000, chains = 3)

saveRDS(example2fit, "02pilotrun1000it.rds")

######################################################################################################################################################

#ICC####
psych::ICC(ISaffect, missing=TRUE)
getwd()



icc()
EMAd.nm <- EMAd[EMAd$status != "Expired" & EMAd$status != "Blocked",]
str(EMAd.nm$ID)

dcast(EMAd.nm, ID ~ time, value.var="ISvalence")


testi1 <- EMAd.nm[c(1,2,4)]


spread(testi1, time, ISvalence)






############################ ROSKAKORI ###########################

EMAd %>% 
  filter(., EMAd$status == "Blocked")
         & is.na(EMAd$ISstress)) 


na_if(.$ISvalence==2)

EMAd$status == "Expired"

mutate(ISstress = replace_na(ISstress,50))

head(EMAd)

str(EMAd$time)

filter(., EMAd$status!="Expired" & EMAd$status != "Blocked") %>%
  mutate(ISstress = replace_na(ISstress,50))

EMAd[EMAd$EventBOO=="Yes",]


testi12 <- spread(testi12, TP, ISvalence)
testi12 <- as.data.frame(testi12)
testi12 <- testi12[2:21]
colnames(testi12) <- paste("T", 1:20, sep="")
rownames(testi12) <- paste("S", 1:6, sep="")
testi121 <- testi12[,1:11]

testi12$ID <- seq(1:6)
testi12w <- gather(testi12, time, ISvalence, 1:20, factor_key=FALSE)
testi12w <- testi12w[order(testi12w$ID),]
testi12w$time <- as.factor(testi12w$time)
testi12w$time <- factor(testi12w$time, levels = testi12w$time[1:20])
testi12w$ID <- as.factor(testi12w$ID)

pd <- position_jitter(0.1, 0.1)

ggplot(testi12w, aes(x=time, y=ISvalence, color=ID))+
  geom_line(aes(group=ID), position=pd)+
  geom_point(position=pd)


### tarkkana alla olevan kanssa
#EMA1 <- EMAd.nm

#EMA1$time <- as.POSIXlt(EMA1$time)
#EMA1$TimeSinceEvent <- EMA1$TimeSinceEvent * 60
#EMA1$newtime <- EMA1$time-EMA1$TimeSinceEvent

#EMA1r <- EMA1[c(1,11:33)]
#EMA1r <- filter(EMA1r, EMA1r$EventBOO == "Yes")

#oldnames <- c("Rvalence", "Rarousal", "Rdominance", "Rstress", "Rautonomy", "Rcompentece", "Rsocial", "newtime")
#newnames <- c("ISvalence", "ISarousal", "ISdominance", "ISstress", "ISautonomy", "IScompetence", "ISsocial", "time")

#EMA1r <- EMA1r %>% rename_at(vars(oldnames), ~ newnames)
#EMA1r$time <- as.POSIXct(EMA1r$time)
#EMA1$time <- as.POSIXct(EMA1$time)
#EMA1r$retro <- paste("retro")
#EMA1$retro <- paste("in.situ")
#EMA.merged <- full_join(EMA1, EMA1r)
#EMA.merged[EMA.merged$retro=="retro",]

#EMA.merged <- EMA.merged %>%
  #group_by(., ID) %>%
  #arrange(., time, .by_group=TRUE)

########### LOOP BACK! -RENAME TO EMAd.nm FOR REMAKING THE PLOTS WITH THIS DATA

#EMAd.nm <- EMA.merged

