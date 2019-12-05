if (!require(tidyverse)) {install.packages('tidyverse')}
if (!require(irr)) {install.packages('irr')}
if (!require(scales)) {install.packages('scales')}
if (!require(lubridate)) {install.packages('lubridate')}
if (!require(hms)) {install.packages('hms')}
if (!require(ggstance)) {install.packages('ggstance')}

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

# time for descriptives! :) ####
## need to figure out how to model the time in, for now we need a TIMEPOINT variable to just order the variables for calculation ICC

EMAd.nm <- EMAd[EMAd$status != "Expired" & EMAd$status != "Blocked",]

#EMAd.nm IS ATM THE MERGED FILE !!###########################################################
#############################################################################################

testi1 <- EMAd.nm %>%
  group_by(., ID) %>%
  mutate(TP = seq(1:length(ID)))

ok <- select(testi1, TP,retro)
names(ok)[names(ok) == "TP"] <- "time"
ok$time <- as.factor(ok$time)
ok$ID <- recode(ok$ID, "User #19350" = "19350", "User #19427" = "19427", "User #19436" = "19436", "User #19444" = "19444", "User #19445" = "19445", "User #19453" = "19453")
#ok$time <- factor(ok$time, levels = ok$time[1:23])


testi12 <- select(testi1, ID, TP, ISvalence, ISarousal, ISdominance, retro)

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

time.merge <- select(EMA.merged, ID, timePOSIX)
time.merge$ID <- recode(time.merge$ID, "User #19350" = "19350", "User #19427" = "19427", "User #19436" = "19436", "User #19444" = "19444", "User #19445" = "19445", "User #19453" = "19453")
time.merge <- time.merge %>%
  group_by(., ID) %>%
  mutate(time = row_number())
time.merge$time <- as.factor(time.merge$time)

ISaffectANNO <- left_join(ISaffectANNO, time.merge)


pd <- position_jitter(0.1, 0.1)
pd <- position_dodge(0.1)
pd <- ggstance::position_dodgev(height = 0.2,)

ISaffectANNO <- drop_na(ISaffectANNO)
ISaffectANNO$ISvalence.jittered <- jitter(ISaffectANNO$ISvalence, amount=0.05)

plot1 <- ggplot(ISaffectANNO, aes(x=timePOSIX, y=ISvalence.jittered, color=ID)) +
  geom_line(aes(group=ID))+
  geom_point(aes(group=ID))+
  scale_x_datetime(breaks = "1 hour", date_labels = "%H")+
  facet_grid(. ~ day, scales="free_x")+
  theme(aspect.ratio = 0.9)


geom_text(aes(label=hms::as.hms(timePOSIX)),hjust=0, vjust=0, size=3)
scale_x_time(breaks = hms::as.hms(ISaffectANNO$timePOSIX), date_breaks())

#problem: positioning fucks up lines and points  
  
plot1 <- ggplot(ISaffectANNO, aes(x=time, y=ISvalence, color=ID))+
  geom_line(aes(group=ID), position=pd)+
  geom_point(position=pd)+
  ylim(-1,5)+
  theme(legend.position = "none")+
  geom_text(aes(label=ifelse(retro=="retro",as.character("event"),'')),hjust=0,vjust=0)

plot2 <- ggplot(ISaffect, aes(x=time, y=ISarousal, color=ID))+
  geom_line(aes(group=ID), position=pd)+
  geom_point(position=pd)+
  ylim(-1,5) + 
  theme(legend.position = "none") 

plot3 <- ggplot(ISaffect, aes(x=time, y=ISdominance, color=ID))+
  geom_line(aes(group=ID), position=pd)+
  geom_point(position=pd)+
  ylim(-1,5)+
  theme(legend.position = c(0.9,0.9))

grid.arrange(plot1,plot2,plot3, ncol=3)


rownames(testi12) <- paste("S", 1:6, sep="")
testi121 <- testi12[,1:11]

#per subject

ISaff1 <- filter(ISaffect, ID == "19350")
ISaff1 <- gather(ISaff1, "variable", "value", 3:5)
ISaff1$variable <- type.convert(ISaff1$variable)
ISaff1$variable <- factor(ISaff1$variable, levels = c("ISvalence", "ISarousal", "ISdominance"))

pd <- position_jitter(0.05, 0.05)

plot1 <- ggplot(ISaff1, aes(x=time, y=value, color=variable))+
  geom_line(aes(group=variable), position=pd)+
  geom_point(aes(group=variable), position=pd) + 
  ylim(-1,5)+
  annotate("text", label = ISaff1$ID, x = 10, y = 5)

ISaff2 <- filter(ISaffect, ID == "19427")
ISaff2 <- gather(ISaff2, "variable", "value", 3:5)
ISaff2$variable <- type.convert(ISaff2$variable)
ISaff2$variable <- factor(ISaff2$variable, levels = c("ISvalence", "ISarousal", "ISdominance"))

plot2 <- ggplot(ISaff2, aes(x=time, y=value, color=variable))+
  geom_line(aes(group=variable), position=pd)+
  geom_point(aes(group=variable), position=pd) + 
  ylim(-1,5)+
  annotate("text", label = ISaff2$ID, x = 10, y = 5)

ISaff3 <- filter(ISaffect, ID == "19436")
ISaff3 <- gather(ISaff3, "variable", "value", 3:5)
ISaff3$variable <- type.convert(ISaff3$variable)
ISaff3$variable <- factor(ISaff3$variable, levels = c("ISvalence", "ISarousal", "ISdominance"))

plot3 <- ggplot(ISaff3, aes(x=time, y=value, color=variable))+
  geom_line(aes(group=variable), position=pd)+
  geom_point(aes(group=variable), position=pd) + 
  ylim(-1,5)+
  annotate("text", label = ISaff3$ID, x = 10, y = 5)

ISaff4 <- filter(ISaffect, ID == "19444")
ISaff4 <- gather(ISaff4, "variable", "value", 3:5)
ISaff4$variable <- type.convert(ISaff4$variable)
ISaff4$variable <- factor(ISaff4$variable, levels = c("ISvalence", "ISarousal", "ISdominance"))

plot4 <- ggplot(ISaff4, aes(x=time, y=value, color=variable))+
  geom_line(aes(group=variable), position=pd)+
  geom_point(aes(group=variable), position=pd) + 
  ylim(-1,5)+
  annotate("text", label = ISaff4$ID, x = 10, y = 5)

ISaff5 <- filter(ISaffect, ID == "19445")
ISaff5 <- gather(ISaff5, "variable", "value", 3:5)
ISaff5$variable <- type.convert(ISaff5$variable)
ISaff5$variable <- factor(ISaff5$variable, levels = c("ISvalence", "ISarousal", "ISdominance"))

plot5 <- ggplot(ISaff5, aes(x=time, y=value, color=variable))+
  geom_line(aes(group=variable), position=pd)+
  geom_point(aes(group=variable), position=pd) + 
  ylim(-1,5)+
  annotate("text", label = ISaff5$ID, x = 10, y = 5)

ISaff6 <- filter(ISaffect, ID == "19453")
ISaff6 <- gather(ISaff6, "variable", "value", 3:5)
ISaff6$variable <- type.convert(ISaff6$variable)
ISaff6$variable <- factor(ISaff6$variable, levels = c("ISvalence", "ISarousal", "ISdominance"))

plot6 <- ggplot(ISaff6, aes(x=time, y=value, color=variable))+
  geom_line(aes(group=variable), position=pd)+
  geom_point(aes(group=variable), position=pd) + 
  ylim(-1,5)+
  annotate("text", label = ISaff6$ID, x = 10, y = 5)

grid.arrange(plot1,plot2,plot3,plot4,plot5,plot6, ncol=3)


#ICC
psych::ICC(ISaffect, missing=TRUE)




icc()
EMAd.nm <- EMAd[EMAd$status != "Expired" & EMAd$status != "Blocked",]
str(EMAd.nm$ID)

dcast(EMAd.nm, ID ~ time, value.var="ISvalence")


testi1 <- EMAd.nm[c(1,2,4)]


spread(testi1, time, ISvalence)



##### ADD RETROSPECTIVE TIMEPOINTS AS OBSERVATIONS ########

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
EMA1r$retro <- paste("retro")
EMA1$retro <- paste("in.situ")
EMA.merged <- full_join(EMA1, EMA1r)
EMA.merged[EMA.merged$retro=="retro",]

EMA.merged <- EMA.merged %>%
  group_by(., ID) %>%
  arrange(., time, .by_group=TRUE)

########### LOOP BACK! -RENAME TO EMAd.nm FOR REMAKING THE PLOTS WITH THIS DATA

EMAd.nm <- EMA.merged

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


