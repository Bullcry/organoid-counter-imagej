library(dplyr)
library(tidyr)
library(ggplot2)

#set WD and load data
setwd("L:/kinderen/Algemeen/RMCU/PediatricGastroenterology/zz_LabData/201411_Jorik van Rijn/Lab Data/2017/17RIJN048_growth rate DGAT1_2")
dat <- read.table("./output/Summary_2.txt", header=TRUE, dec=".", na.strings=c("NA"))
head(dat)

#set variables
days <- unique(dat$DAY)
results <- data.frame()
sumindiv <- data.frame()

#set subsets, reshape data
results <- dat %>%
	group_by(GROUP, DAY) %>%
	summarise(MEAN_AREA=mean(AVERAGE_AREA), SD=sd(AVERAGE_AREA))
	
indiv <- dat %>%
	group_by(SAMPLE, DAY) %>%
	summarise(MEAN_AREA=mean(AVERAGE_AREA), SD=sd(AVERAGE_AREA))
	
indiv <- indiv %>%
	mutate(PROC_SD=SD/MEAN_AREA) %>%
	mutate(LOG2_MEAN_AREA=log2(MEAN_AREA))

results <- results %>%
	mutate(PROC_SD=SD/MEAN_AREA) %>%
	mutate(LOG2_MEAN_AREA=log2(MEAN_AREA))
	
dat2 <- dat %>%
	mutate(LOG2_AVERAGE_AREA=log2(AVERAGE_AREA))
	
#normalize subsets
days <- unique(indiv$DAY)
samples <- unique(indiv$SAMPLE)

for(i in samples){
	day1 <- (indiv %>%
		filter(SAMPLE==i,DAY==1) )[,which(colnames(indiv)=="MEAN_AREA")]
	innorm <- indiv %>%
		filter(SAMPLE==i) %>%
		mutate(NORM_MEAN_AREA = MEAN_AREA/unlist(day1))
	sumindiv <- bind_rows(sumindiv, innorm)
	}
	
for(i in samples){
	day1 <- (results %>%
		filter(SAMPLE==i,DAY==1) )[,which(colnames(results)=="MEAN_AREA")]
	innorm <- results %>%
		filter(SAMPLE==i) %>%
		mutate(NORM_MEAN_AREA = MEAN_AREA/unlist(day1))
	sumresults <- bind_rows(sumresults, resnorm)
	}

#ggplot2
pd <- position_dodge(0.1)
p1 <- ggplot(dat, aes(x=DAY, y=AVERAGE_AREA), group=SAMPLE)+
  #scale_y_continuous(limits=c(0,NA))+
  geom_point(aes(colour=SAMPLE), position=pd)+
  #facet_wrap(~ TARGET, scales = "free")+
  theme_classic()
  #geom_hline(yintercept=0, size=.5)

p2 <- ggplot(results, aes(DAY, MEAN_AREA, group=GROUP))+
	geom_line(aes(colour=GROUP), position=pd)+
	geom_point(aes(colour=GROUP), position=pd)+
	geom_errorbar(aes(ymin=MEAN_AREA-SD, ymax=MEAN_AREA+SD, colour=GROUP), width=.1, position=pd)+
	theme_classic()
	
p3 <- ggplot(indiv, aes(DAY, MEAN_AREA, group=SAMPLE))+
	geom_line(aes(colour=SAMPLE), position=pd)+
	geom_point(aes(colour=SAMPLE), position=pd)+
	geom_errorbar(aes(ymin=MEAN_AREA-SD, ymax=MEAN_AREA+SD, colour=SAMPLE), width=.1, position=pd)+
	theme_classic()	
	
p4 <- ggplot(sumindiv, aes(DAY, NORM_MEAN_AREA, group=SAMPLE))+
	geom_line(aes(colour=SAMPLE), position=pd)+
	geom_point(aes(colour=SAMPLE), position=pd)+
	#geom_errorbar(aes(ymin=NORM_MEAN_AREA-(NORM_MEAN_AREA*PROC_SD), ymax=MEAN_AREA+(NORM_MEAN_AREA*PROC_SD), colour=SAMPLE), width=.1, position=pd)+
	theme_classic()	
	
p5 <- ggplot(dat2, aes(DAY, LOG2_AVERAGE_AREA, group=SAMPLE))+
	#geom_line(aes(colour=SAMPLE), position=pd)+
	geom_smooth(aes(colour=SAMPLE), method = lm, se = FALSE)+
	geom_point(aes(colour=SAMPLE), position=pd)+
	#geom_errorbar(aes(ymin=log2(MEAN_AREA-SD, ymax=MEAN_AREA+SD, colour=SAMPLE), width=.1, position=pd)+
	theme_classic()	

p6 <- ggplot(dat2, aes(DAY, LOG2_AVERAGE_AREA, group=GROUP))+
	#geom_line(aes(colour=SAMPLE), position=pd)+
	geom_smooth(aes(colour=GROUP), method = lm, se = TRUE)+
	geom_point(aes(colour=GROUP), position=pd)+
	#geom_errorbar(aes(ymin=MEAN_AREA-SD, ymax=MEAN_AREA+SD, colour=SAMPLE), width=.1, position=pd)+
	theme_classic()+
	
ggsave(plot=p1, "scatter.pdf", width=10, height=10)
ggsave(plot=p2, "group_mean.pdf", width=10, height=10)
ggsave(plot=p3, "samples_mean.pdf", width=10, height=10)
ggsave(plot=p4, "normalized_samples_mean.pdf", width=10, height=10)
ggsave(plot=p5, "lograte_samples_mean.pdf", width=10, height=10)
ggsave(plot=p6, "lograte_group_mean.pdf", width=10, height=10)
