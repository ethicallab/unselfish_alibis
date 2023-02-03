# Julian De Freitas, 2021-2023

## clear workspace
rm(list = ls()) 

options(download.file.method="libcurl")

if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('ggplot2', #plotting
               'ggsignif', #plotting significance bars
               'lme4', #functions for fitting linear regression modesl
               'ggforce', #make ggplot even fancier
               'ggpubr', #arrange plots in a grid, if needed
               'ltm', #probably not using..
               'simr', # power analysis for mixed models
               'compute.es', # effect size package
               'effsize', # another effect size package
               'pwr', #package for power calculation
               'filesstrings', #create and move files
)
library("lmerTest")

##================================================================================================================
                                              ##IMPORT & PRE-PROCESS d##
##================================================================================================================

#read data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory
d_raw <- read.csv('e4_data.csv')
dim(d_raw)

d_raw <- subset(d_raw, d_raw$att1==2 & d_raw$att2==2)

#replace na values for comprehension checks with 3
d_raw$comp1[is.na(d_raw$comp1)] <- 3
d_raw$comp2[is.na(d_raw$comp2)] <- 3

#define new data frame that we'll extract preprocessed data into
d_subset <- array(dim=c(dim(d_raw)[1], 5))
colnames(d_subset) <- c('agent_cond','forced_cond', 'choice', 'comp1','comp2')

d_subset <- as.data.frame(d_subset, stringsAsFactors=FALSE)

#extract the good data from the middle part of the raw data
for(i in 1:dim(d_raw)[1]) {
    curr <- d_raw[i,21:26][!is.na(d_raw[i,21:26])] #for a given row, get only the non NA values
    d_subset[i,3:5] <- as.numeric(curr[curr!= ""]) #and only the non-empty values
    cond_names <- d_raw[i,34][!is.na(d_raw[i,34])]
    d_subset[i,1] <- strsplit(cond_names[[1]], "_")[[1]][1]
    d_subset[i,2] <- strsplit(cond_names[[1]], "_")[[1]][2]
}

#merge good data with first and last halves of the original data
d_raw <- cbind(d_subset, d_raw[,27:32])
d_raw$ss <- 1:dim(d_raw)[1]

#check that we have equal numbers for each of our various conditions
d_raw$vehicle_cond <- d_raw$agent_cond # I accidentally named vehicle cond as agent cond.
table(d_raw$vehicle_cond)
table(d_raw$forced_cond)

d_raw$vehicle_cond[d_raw$vehicle_cond=="human"] <- "car"

# number of subjects before exclusions
n_original <- dim(d_raw)[1]
n_original

#create dichotomous dv
d_raw$choice_dicot <- ifelse(d_raw$choice==2, 1, 0)

#convert variables to factors
d_raw$choice_dict <- as.factor(d_raw$choice_dicot)
d_raw$agent_cond <- as.factor(d_raw$agent_cond)
d_raw$forced_cond <- as.factor(d_raw$forced_cond)

##================================================================================================================
                                                ##EXCLUSIONS##
##================================================================================================================

#comprehension exclusions
d_raw$comp1_recode <- ifelse(d_raw$comp1==1, "car", ifelse(d_raw$comp1==2, "AV", "relative"))
d_raw$comp2_recode <- ifelse(d_raw$comp2==1, "forced", ifelse(d_raw$comp2==2, "unforced", "neither"))

d_raw$vehicle_num <- ifelse(d_raw$vehicle_cond=="car", 1, 2)
d_raw$forced_num <- ifelse(d_raw$forced_cond=="forced", 1, 2)

#perform exclusions based on attention and comprehension checks
failed_vehicle_comp <- sum(d_raw$comp1_recode != d_raw$vehicle_cond); failed_vehicle_comp
failed_choices_comp <- sum(d_raw$comp2_recode != d_raw$forced_cond); failed_choices_comp

d <- subset(d_raw, (d_raw$comp1_recode == d_raw$vehicle_cond) |
                   (d_raw$comp2_recode == d_raw$forced_cond))

#number of subjects after exclusions
n_after_exclusions <- dim(d)[1]; n_after_exclusions
n_original - n_after_exclusions
percent_excluded <- (n_original - n_after_exclusions)/n_original; percent_excluded

## mean age and gender
mean(d$age,na.rm = TRUE) 
table(d$gender)[2]/sum(table(d$gender)) 

table(d$agent_cond)
table(d$forced_cond)

##================================================================================================================
                                                  ##ANALYSIS##
##================================================================================================================

#run model
mylogit <- glm(choice_dicot ~ agent_cond*forced_cond, d = d, family = "binomial")
summary(mylogit)

#AV, forced v. unforced
av_only <- table(d$choice_dicot[d$agent_cond=="AV"], d$forced_cond[d$agent_cond=="AV"])
av_only_mod <- chisq.test(av_only); av_only_mod
sum(av_only) #num av subjects

av_1 <- av_only[2,1]/sum(av_only[,1]) #proportion selfish, forced
av_2 <- av_only[2,2]/sum(av_only[,2]) #proportion selfish, unforced
av_1 - av_2


#HDV, forced v. unforced
human_only <- table(d$choice_dicot[d$agent_cond=="human"], d$forced_cond[d$agent_cond=="human"])
human_only_mod <- chisq.test(human_only); human_only_mod
sum(human_only) #num human subjects

hum_1 <- human_only[2,1]/sum(human_only[,1]) #proportion selfish, forced
hum_2 <- human_only[2,2]/sum(human_only[,2]) #proportion selfish, unforced
hum_1 - hum_2

##=============================================================================================================
                                         ##PREPARE FOR PLOTTING##
##================================================================================================================

#make arrays for plotting infinity item
choice_mat <- array(0,dim=c(3,4))
colnames(choice_mat) <- c('vehicle_num', 'forced_num', 'choice', 'proportion')
choice_mat <- as.data.frame(choice_mat, stringsAsFactors=FALSE)

counter <- 1
for(i in 1:2) {
  for(j in 1:2) {
    for(k in 1:3) {
      choice_mat[counter,] <- c(i, j, k, length(d$choice[d$vehicle_num==i & d$forced_num==j & d$choice == k])/
                                         length(d$choice[d$vehicle_num==i & d$forced_num==j])*100)
      counter <- counter + 1
    }
  }
}

forced_labels <- c("Two", "Three")
choice_labels <- c("Save Pedestrian", "Save Passenger", "Egalitarian")

choice_car <- subset(choice_mat, choice_mat$vehicle_num==1); choice_car
choice_av <- subset(choice_mat, choice_mat$vehicle_num==2); choice_av

##=============================================================================================================
##PLOT##
##================================================================================================================

p1 <-ggplot(choice_car,aes(x=factor(forced_num),y=as.numeric(proportion),fill=factor(choice)),color=factor(choice)) +  
  stat_summary(position=position_dodge(),geom="bar")+
  theme_bw()+coord_cartesian(ylim=c(1,100)) 
p1 <- p1+theme(text = element_text(size=16),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_discrete(breaks = 1:length(forced_labels), labels=forced_labels)+
  ggtitle("Human-Driven Vehicle")+
  scale_fill_manual(values = c("#cccccc", "#333333", "#989898"),name= "Choice:",
                    labels=choice_labels, guide = guide_legend(reverse = FALSE))+
  theme_classic()+
  xlab("") + ylab ("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=14))+
  theme(axis.text.y = element_text(size=14))+
  theme(plot.title = element_text(size=20, hjust=0.5))+
  theme(legend.text = element_text(size=15))+
  theme(legend.title = element_text(size=16))
p1

p2 <-ggplot(choice_av,aes(x=factor(forced_num),y=as.numeric(proportion),fill=factor(choice)),color=factor(choice)) +  
  stat_summary(position=position_dodge(),geom="bar")+
  theme_bw()+coord_cartesian(ylim=c(1,100)) 
p2 <- p2+theme(text = element_text(size=16),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_discrete(breaks = 1:length(forced_labels), labels=forced_labels)+
  ggtitle("Autonomous Vehicle")+
  scale_fill_manual(values = c("#cccccc", "#333333","#989898"),name= "Choice:",
                    labels=choice_labels, guide = guide_legend(reverse = FALSE))+
  theme_classic()+
  xlab("") + ylab ("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=14))+
  theme(axis.text.y = element_text(size=14))+
  theme(plot.title = element_text(size=20, hjust=0.5))+
  theme(legend.text = element_text(size=15))+
  theme(legend.title = element_text(size=14))
p2

dev.new(width=13,height=6,noRStudioGD = TRUE)
figure<-ggarrange(p1, p2, nrow=1,ncol=2,common.legend = TRUE, legend="top", vjust = 1.0, hjust=0.5) 
annotate_figure(figure,left = text_grob("Proportion", color="black", face ="plain",size=20, rot=90),
                bottom = text_grob("Number of Response Options", color="black", face ="plain",size=20)) 

ggsave(
  "e4_unselfish_alibis.pdf",
  last_plot(),
  dpi = 500
)

dir.create(file.path('plots'))
plot_files <- list.files(pattern = c("(.pdf|.png)"))
file.move(plot_files, "./plots", overwrite = TRUE)

##================================================================================================================
##END##
##================================================================================================================




















