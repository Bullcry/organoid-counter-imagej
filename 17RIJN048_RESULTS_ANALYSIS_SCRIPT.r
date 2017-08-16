library(dplyr)
library(tidyr)
library(ggplot2)

#set WD and load data
setwd("L:/kinderen/Algemeen/RMCU/PediatricGastroenterology/zz_LabData/201411_Jorik van Rijn/Lab Data/2017/17RIJN048_growth rate DGAT1_2")
dat <- read.table("./output/Results_2.txt", header=TRUE, dec=".", na.strings=c("NA"))
head(dat)

#mutate data and create variables
dat <- mutate(dat, LOG2_AREA=log2(AREA))

#Loop through samples and create violin charts
samples <- unique(dat$SAMPLE)

for(i in samples){
	set <- dat %>%
		filter(SAMPLE==i)
		
	set$DAY <- as.factor(set$DAY) 
	
	setgraph <- ggplot(set, aes(x=DAY, y=LOG2_AREA))+
		scale_y_continuous(limits=c(0,NA))+
		#geom_dotplot(binaxis="y", stackdir="center", stackratio=0.5)+
		geom_violin(trim=FALSE)+
		theme_classic()+
		geom_boxplot(width=0.1)+
		stat_summary(fun.y=mean, geom="point", color="red")
	
	ggsave(plot=setgraph, paste("graphs/measurement_distribution_",i,".pdf"), width=10, height=10)
	
}

#Create summary of samples and groups.
#First take median of each replicate sample or group.
#Then assume normality for these replicates and calculate mean and sd for this distribution
sumsamp <- dat %>%
	group_by(SAMPLE, DAY, REPLICATE) %>%
	summarise(MEDIAN_LOG2_AREA=median(LOG2_AREA), MIN_LOG2_AREA=min(LOG2_AREA), MAX_LOG2_AREA=max(LOG2_AREA)) %>%
	group_by(SAMPLE, DAY) %>%
	summarise(MEAN_MEDIAN_LOG2_AREA=mean(MEDIAN_LOG2_AREA), SD=sd(MEDIAN_LOG2_AREA))
	
sumgroup <- dat %>%
	group_by(GROUP, DAY, REPLICATE) %>%
	summarise(MEDIAN_LOG2_AREA=median(LOG2_AREA), MIN_LOG2_AREA=min(LOG2_AREA), MAX_LOG2_AREA=max(LOG2_AREA)) %>%
	group_by(GROUP, DAY) %>%
	summarise(MEAN_MEDIAN_LOG2_AREA=mean(MEDIAN_LOG2_AREA), SD=sd(MEDIAN_LOG2_AREA))

#Plot graphs using ggplot2
pd <- position_dodge(0.5)
p1 <- ggplot(dat, aes(x=DAY, y=AREA), group=SAMPLE)+
  #scale_y_continuous(limits=c(0,NA))+
  geom_point(aes(colour=SAMPLE), alpha=0.5, position=pd)+
  #facet_wrap(~ TARGET, scales = "free")+
  theme_classic()
  #geom_hline(yintercept=0, size=.5)

pd <- position_dodge(0.1)
p2 <- ggplot(sumgroup, aes(DAY, MEAN_MEDIAN_LOG2_AREA, group=GROUP))+
	geom_line(aes(colour=GROUP), position=pd)+
	geom_point(aes(colour=GROUP), position=pd)+
	geom_errorbar(aes(ymin=MEAN_MEDIAN_LOG2_AREA-SD, ymax=MEAN_MEDIAN_LOG2_AREA+SD, colour=GROUP), width=.1, position=pd)+
	theme_classic()
  
p3 <- ggplot(sumsamp, aes(DAY, MEAN_MEDIAN_LOG2_AREA, group=SAMPLE))+
	geom_line(aes(colour=SAMPLE), position=pd)+
	geom_point(aes(colour=SAMPLE), position=pd)+
	geom_errorbar(aes(ymin=MEAN_MEDIAN_LOG2_AREA-SD, ymax=MEAN_MEDIAN_LOG2_AREA+SD, colour=SAMPLE), width=.1, position=pd)+
	theme_classic()	

pd <- position_dodge(0.5)
dat$DAY <- as.factor(dat$DAY) 
p4 <- ggplot(dat, aes(x=DAY, y=LOG2_AREA), group=SAMPLE)+
  #scale_y_continuous(limits=c(0,NA))+
  geom_boxplot(aes(colour=SAMPLE), width=.3,  position=pd)+
  #facet_wrap(~ TARGET, scales = "free")+
  theme_classic()
  #geom_hline(yintercept=0, size=.5)  

	
ggsave(plot=p1, "graphs/scatter.pdf", width=10, height=10)
ggsave(plot=p2, "graphs/group_mean.pdf", width=10, height=10)
ggsave(plot=p3, "graphs/samples_mean.pdf", width=10, height=10)
ggsave(plot=p4, "graphs/boxplots_samples.pdf", width=10, height=10)




