# Julian De Freitas, 2021-2023

## clear workspace
rm(list = ls()) 

options(download.file.method="libcurl")

if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('ggplot2', #plotting
               'ggsignif', #plotting significance bars
               'lme4', #functions for fitting linear regression modesl
               'ggforce', #make ggplot even fancier
               'ggpubr', #arrange plots in a grid, if neededd
               'ltm', #probably not using..
               'simr', # power analysis for mixed models
               'compute.es', # effect size package
               'effsize', # another effect size package
               'pwr', # package for power calculation
               'filesstrings' # move files
              )
library("lmerTest")

##================================================================================================================
                                              ##IMPORT & PRE-PROCESS d##
##================================================================================================================

# read d
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory
d_raw <- read.csv('e3_data.csv')

d_raw <- subset(d_raw, d_raw$att1==2 & d_raw$att2==2 & d_raw$Finished==1)

#replace na values for comprehension checks with 3
d_raw$comp1[is.na(d_raw$comp1)] <- 3
d_raw$comp2[is.na(d_raw$comp2)] <- 3

#define new d frame that we'll extract preprocessed d into
d_subset <- array(dim=c(dim(d_raw)[1], 5))
colnames(d_subset) <- c('agent_cond','forced_cond', 'choice', 'comp1','comp2')

d_subset <- as.data.frame(d_subset, stringsAsFactors=FALSE)

#extract the good d from the middle part of the raw d
for(i in 1:dim(d_raw)[1]) {
    curr <- d_raw[i,21:26][!is.na(d_raw[i,21:26])] #for a given row, get only the non NA values
    d_subset[i,3:5] <- as.numeric(curr[curr!= ""]) #and only the non-empty values
    cond_names <- d_raw[i,34][!is.na(d_raw[i,34])]
    d_subset[i,1] <- strsplit(cond_names[[1]], "_")[[1]][1]
    d_subset[i,2] <- strsplit(cond_names[[1]], "_")[[1]][2]
}

#merge good d with first and last halves of the original d
d_raw <- cbind(d_subset, d_raw[,27:32])
d_raw$ss <- 1:dim(d_raw)[1]

#check that we have equal numbers for each of our various conditions
table(d_raw$agent_cond)
table(d_raw$forced_cond)

# number of subjects before exclusions
n_original <- dim(d_raw)[1]; n_original

# add dichotomized variable and code conditions as factors
d_raw$choice_dicot <- ifelse(d_raw$choice==2, 1, 0) #1 = selfish
d_raw$choice_dicot <- as.factor(d_raw$choice_dicot)

d_raw$agent_cond <- as.factor(d_raw$agent_cond)
d_raw$forced_cond <- as.factor(d_raw$forced_cond)

##================================================================================================================
                                                ##EXCLUSIONS##
##================================================================================================================

#comprehension exclusions
d_raw$comp1_recode <- ifelse(d_raw$comp1==1, "TP", ifelse(d_raw$comp1==2, "FP", "neither"))
d_raw$comp2_recode <- ifelse(d_raw$comp2==1, "forced", ifelse(d_raw$comp2==2, "unforced", "neither"))
d_raw$agent_num <- ifelse(d_raw$agent_cond=="TP", 1, 2)
d_raw$forced_num <- ifelse(d_raw$forced_cond=="forced", 1, 2)

#perform exclusions based on comprehension checks
d <- subset(d_raw, (d_raw$comp1_recode == d_raw$agent_cond) |
                   (d_raw$comp2_recode == d_raw$forced_cond))

#number of subjects after exclusions
n_after_exclusions <- dim(d)[1]; n_after_exclusions
percent_excluded <- (n_original - n_after_exclusions)/n_original; percent_excluded

## mean age and gender
mean(d$age,na.rm = TRUE) 
table(d$gender)[2]/sum(table(d$gender)) 

table(d$agent_cond)
table(d$forced_cond)

##================================================================================================================
                                                  ##ANALYSIS##
##================================================================================================================

#create dichotomous dv

mylogit <- glm(choice_dicot ~ agent_cond*forced_cond, d = d, family = "binomial")
summary(mylogit)

forced_tab <- table(d$choice_dicot, d$forced_cond)
forced_mod <- chisq.test(forced_tab)
forced_mod

forced_tab[2,1]/(sum(forced_tab[,1]))
forced_tab[2,2]/(sum(forced_tab[,2]))

#repeat analysis without exclusions

mylogit <- glm(choice_dicot ~ agent_cond*forced_cond, d = d_raw, family = "binomial")
summary(mylogit)

forced_tab <- table(d_raw$choice_dicot, d_raw$forced_cond); forced_tab
#forced_mod <- chisq.test(forced_tab)
#forced_mod

forced_tab[2,1]/(sum(forced_tab[,1])) #selfish choices in forced cond
forced_tab[2,2]/(sum(forced_tab[,2])) #selfish choices in unforced cond

##=============================================================================================================
                                         ##PREPARE FOR PLOTTING##
##================================================================================================================

#make arrays for plotting infinity item
choice_mat <- array(0,dim=c(3,4))
colnames(choice_mat) <- c('agent_num', 'forced_num', 'choice', 'proportion')
choice_mat <- as.data.frame(choice_mat, stringsAsFactors=FALSE)

counter <- 1
for(i in 1:2) {
  for(j in 1:2) {
    for(k in 1:3) {
      choice_mat[counter,] <- c(i, j, k, length(d$choice[d$agent_num==i & d$forced_num==j & d$choice == k])/
                                  length(d$choice[d$agent_num==i & d$forced_num==j])*100)
      counter <- counter + 1
    }
  }
}

agent_labels <- c("TP", "FP")
forced_labels <- c("Two", "Three")
choice_labels <- c("Save Pedestrian", "Save Passenger", "Egalitarian")

choice_tp <- subset(choice_mat, choice_mat$agent_num==1); choice_tp
choice_fp <- subset(choice_mat, choice_mat$agent_num==2); choice_fp

##=============================================================================================================
                                                ##PLOT##
##================================================================================================================

p1 <-ggplot(choice_tp,aes(x=factor(forced_num),y=as.numeric(proportion),fill=factor(choice)),color=factor(choice)) +  
  stat_summary(position=position_dodge(),geom="bar")+
  theme_bw()+coord_cartesian(ylim=c(1,100)) 
p1 <- p1+theme(text = element_text(size=16),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_discrete(breaks = 1:length(forced_labels), labels=forced_labels)+
  ggtitle("Third Person")+
  scale_fill_manual(values = c("#cccccc", "#333333", "#989898"),name= "Choice:",
                    labels=choice_labels, guide = guide_legend(reverse = FALSE))+
  theme_classic()+
  xlab("") + ylab ("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=14))+
  theme(axis.text.y = element_text(size=14))+
  theme(plot.title = element_text(size=20, hjust=0.5))+
  theme(legend.text = element_text(size=15))+
  theme(legend.title = element_text(size=16))
#p1

p2 <-ggplot(choice_fp,aes(x=factor(forced_num),y=as.numeric(proportion),fill=factor(choice)),color=factor(choice)) +  
  stat_summary(position=position_dodge(),geom="bar")+
  theme_bw()+coord_cartesian(ylim=c(1,100)) 
p2 <- p2+theme(text = element_text(size=16),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_discrete(breaks = 1:length(forced_labels), labels=forced_labels)+
  ggtitle("First Person")+
  scale_fill_manual(values = c("#cccccc", "#333333", "#989898"),name= "Choice:",
                    labels=choice_labels, guide = guide_legend(reverse = FALSE))+
  theme_classic()+
  xlab("") + ylab ("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=14))+
  theme(axis.text.y = element_text(size=14))+
  theme(plot.title = element_text(size=20, hjust=0.5))+
  theme(legend.text = element_text(size=15))+
  theme(legend.title = element_text(size=14))
#p2

dev.new(width=13,height=6,noRStudioGD = TRUE)
figure<-ggarrange(p1, p2, nrow=1,ncol=2,common.legend = TRUE, legend="top", vjust = 1.0, hjust=0.5) 
annotate_figure(figure,left = text_grob("Proportion", color="black", face ="plain",size=20, rot=90),
                bottom = text_grob("Number of Response Options", color="black", face ="plain",size=20)) 


ggsave(
  "e3_unselfish_alibis.pdf",
  last_plot(),
  dpi = 500
)

dir.create(file.path('plots'))
plot_files <- list.files(pattern = c("(.pdf|.png)"))
file.move(plot_files, "./plots", overwrite = TRUE)

##================================================================================================================
##END##
##================================================================================================================




















