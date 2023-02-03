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
               'pwr', #package for power calculation
               'filesstrings', #moving files
               'lsr' #eta squared
)
library("lmerTest")

mediation <- FALSE

##================================================================================================================
                                              ##IMPORT & PRE-PROCESS d##
##================================================================================================================

#read d
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directoy to current directory
d <- read.csv('e1_data.csv')
dim(d)

d <- subset(d, d$att1==2 & d$att2==2)

#define new d frame that we'll extract preprocessed d into
d_subset <- array(dim=c(dim(d)[1], 16))
colnames(d_subset) <- c('intention_cond','person_cond', 
                        'blame_man','blame_av','blame_company','outrage_anger','outrage_punish','outrage_wrong',
                        'col_donate','col_volunteer','col_protest','col_socMedia','col_shareLike','col_shareNonLike',
                        'comp1','comp2')

d_subset <- as.data.frame(d_subset, stringsAsFactors=FALSE)

#extract the good d from the middle part of the raw d
for(i in 1:dim(d)[1]) {
    curr <- d[i,20:75][!is.na(d[i,20:75])] #for a given row, get only the non NA values
    d_subset[i,3:16] <- as.numeric(curr[curr!= ""]) #and only the non-empty values
    cond_names <- d[i,85:96][!is.na(d[i,85:96])]
    cond_names <- cond_names[cond_names!= ""] 
    d_subset[i,1] <- strsplit(cond_names[[1]], "_")[[1]][1]
    d_subset[i,2] <- strsplit(strsplit(cond_names[[1]], "_")[[1]][2], '\\|')[[1]][1]
}

#merge good d with first and last halves of the original d
d <- cbind(d[,18:19], d_subset, d[,76:81])

#check that we have equal numbers for each of our various conditions
table(d$person_cond)
table(d$intention_cond)

# number of subjects before exclusions
n_original <- dim(d)[1]
n_original

##================================================================================================================
                                                ##EXCLUSIONS##
##================================================================================================================

#comprehension exclusions
d$comp1_recode <- ifelse(d$comp1==1, "intentional", ifelse(d$comp1==2, "random", "neither"))
d$comp2_recode <- ifelse(d$comp2==1, "driver", ifelse(d$comp2==2, "pedestri", "neither"))

#perform exclusions based on attention and comprehension checks
failed_program_comp <- sum(d$comp1_recode != d$intention_cond); failed_program_comp
failed_victim_comp <- sum(d$comp2_recode != d$person_cond); failed_victim_comp

d <- subset(d, (d$comp1_recode == d$intention_cond) |
               (d$comp2_recode == d$person_cond))

#number of subjects after exclusions
n_after_exclusions <- dim(d)[1]; n_after_exclusions
n_original - n_after_exclusions
percent_excluded <- (n_original - n_after_exclusions)/n_original; percent_excluded

## mean age and gender
mean(d$age,na.rm = TRUE) 
table(d$gender)[2]/sum(table(d$gender)) 

table(d$intention_cond)
table(d$person_cond)

##================================================================================================================
                                                  ##d PREP##
##================================================================================================================

#assign simple variable names to measures of interest
#and convert them to factor or numeric
#also aggregate outrage and collective action items

#conds
d$intention_cond <- as.factor(d$intention_cond)
d$intention_cond_num <- as.numeric(ifelse(d$intention_cond=="intentional", 1, 0))

d$person_cond <- as.factor(d$person_cond)
d$person_cond_num <- as.numeric(ifelse(d$person_cond=="pedestri", 1, 0))

#blame
blame_man <- as.numeric(d$blame_man)
d$blame_man <- as.numeric(d$blame_man)
blame_av <- as.numeric(d$blame_av)
d$blame_av <- as.numeric(d$blame_av)
blame_company <- as.numeric(d$blame_company)
d$blame_company <- as.numeric(d$blame_company)

#outrage
outrage_anger <- as.numeric(d$outrage_anger)
outrage_punish <- as.numeric(d$outrage_punish)
outrage_wrong <- as.numeric(d$outrage_wrong)

outrage_mat <- array(0,dim=c(dim(d)[1],3)) #get cronbach's alpha, then average items
outrage_mat[,1] <- outrage_anger
outrage_mat[,2] <- outrage_punish
outrage_mat[,3] <- outrage_wrong
cronbach.alpha(outrage_mat) #0.93

outrage <- rowMeans(outrage_mat) 
d$outrage <- outrage

#collective action
col_volunteer <- as.numeric(d$col_volunteer)
col_protest <- as.numeric(d$col_protest)
col_socMedia <- as.numeric(d$col_socMedia)
col_shareLike <- as.numeric(d$col_shareLike)
col_shareNonLike <- as.numeric(d$col_shareNonLike)

col_mat <- array(0,dim=c(dim(d)[1],5)) #get cronbach's alpha, then average items
col_mat[,1] <- col_volunteer
col_mat[,2] <- col_protest
col_mat[,3] <- col_socMedia
col_mat[,4] <- col_shareLike
col_mat[,5] <- col_shareNonLike
cronbach.alpha(col_mat) 

col_action <- rowMeans(col_mat)
d$col_action <- col_action

##================================================================================================================
                                                      ##ANALYSIS##
##================================================================================================================

#define some labels
intention_labels <- c('intentional', 'random')
person_labels <- c('driver', 'pedestrian')

## (1) OUTRAGE COMPANY -------------------------------------------
outrage_mod <- aov(outrage ~ person_cond*intention_cond, d=d)
summary(outrage_mod)
etaSquared(outrage_mod)

# driver v pedestrian, intentional
var.test(d$outrage[d$intention_cond == 'intentional' & d$person_cond == 'driver'], 
         d$outrage[d$intention_cond == 'intentional' & d$person_cond == 'pedestri'])

outrage_1_t <- t.test(d$outrage[d$intention_cond == 'intentional' & d$person_cond == 'driver'], 
                      d$outrage[d$intention_cond == 'intentional' & d$person_cond == 'pedestri'],
                      var.equal=TRUE, paired=FALSE)
outrage_1_t

cohensD(d$outrage[d$intention_cond == 'intentional' & d$person_cond == 'driver'], 
        d$outrage[d$intention_cond == 'intentional' & d$person_cond == 'pedestri'])



var.test(d$outrage[d$intention_cond == 'random' & d$person_cond == 'driver'], 
         d$outrage[d$intention_cond == 'random' & d$person_cond == 'pedestri'])

outrage_2_t <- t.test(d$outrage[d$intention_cond == 'random' & d$person_cond == 'driver'], 
                      d$outrage[d$intention_cond == 'random' & d$person_cond == 'pedestri'],
                      var.equal=TRUE, paired=FALSE)
outrage_2_t

cohensD(d$outrage[d$intention_cond == 'random' & d$person_cond == 'driver'], 
        d$outrage[d$intention_cond == 'random' & d$person_cond == 'pedestri'])



# (2.1) BLAME FIRM ----------------------------------------------------
blame_company_mod <- aov(blame_company ~ person_cond*intention_cond, d=d)
summary(blame_company_mod)
etaSquared(blame_company_mod)

var.test(d$blame_company[d$person_cond == 'pedestri'], 
         d$blame_company[d$person_cond == 'driver'])

blame_firm_t <- t.test(d$blame_company[d$person_cond == 'pedestri'], 
                       d$blame_company[d$person_cond == 'driver'],
                       var.equal=FALSE, paired=FALSE)

tapply(d$blame_company, d$person_cond, mean)
tapply(d$blame_company, d$person_cond, sd)

blame_firm_t

cohensD(d$blame_company[d$person_cond == 'pedestri'], 
        d$blame_company[d$person_cond == 'driver'])






# (2.2) BLAME HUMAN --------------------------------------------------
blame_man_mod <- aov(blame_man ~ person_cond*intention_cond, d=d)
summary(blame_man_mod)
etaSquared(blame_man_mod)

var.test(d$blame_man[d$person_cond == 'pedestri'], 
         d$blame_man[d$person_cond == 'driver'])

blame_man_t <- t.test(d$blame_man[d$person_cond == 'pedestri'], 
                      d$blame_man[d$person_cond == 'driver'],
                      var.equal=TRUE, paired=FALSE)

blame_man_t

tapply(d$blame_man, d$person_cond, mean)
tapply(d$blame_man, d$person_cond, sd)

cohensD(d$blame_man[d$person_cond == 'pedestri'], 
        d$blame_man[d$person_cond == 'driver'])


# (2.3) BLAME AV -----------------------------------------------------
blame_av_mod <- aov(blame_av ~ person_cond*intention_cond, d=d)
summary(blame_av_mod)
etaSquared(blame_av_mod)


## (3) COLLECTIVE ACTION COMPANY --------------------------------- 
col_action_mod <- aov(col_action ~ person_cond*intention_cond, d=d)
summary(col_action_mod)
etaSquared(col_action_mod)

var.test(d$col_action[d$person_cond == 'pedestri'], 
         d$col_action[d$person_cond == 'driver'])

col_t <- t.test(d$col_action[d$person_cond == 'pedestri'], 
                      d$col_action[d$person_cond == 'driver'],
                      var.equal=TRUE, paired=FALSE)

col_t

tapply(d$col_action, d$person_cond, mean)
tapply(d$col_action, d$person_cond, sd)

cohensD(d$col_action[d$person_cond == 'pedestri'], 
        d$col_action[d$person_cond == 'driver'])

##=============================================================================================================
                                            ##MEDIATION ANALYSIS##
##================================================================================================================

source('../common_functions/process.r')

if(mediation) {

  source('../common_functions/process.R')
  
  process(data = d, y = "blame_company", x = "person_cond_num", 
          m =c("outrage"), w = "intention_cond_num", model = 7, effsize =1, total =1, stand =1, 
          contrast =1, boot = 10000 , modelbt = 1, seed = 654321)
  
  process(data = d, y = "blame_company", x = "person_cond_num", 
          m =c("outrage"), model = 4, effsize =1, total =1, stand =1, 
          contrast =1, boot = 10000 , modelbt = 1, seed = 654321)
}
  
##=============================================================================================================
                                         ##FIG 1: PLOT MAIN RESULTS##
##================================================================================================================

# Code variables for plotting
d$intention_cond <- as.factor(d$intention_cond)
d$blame_company <- as.numeric(d$blame_company)
d$outrage <- as.numeric(d$outrage)
legend_labels <- c("Driver", "Pedestrian")
person_labels <- c("Deliberate", "Random")

# Set plot sizes
tick_size <- 13
title_size <- 16
legend_size <- 15

BarPlotter <- function(d, x_var, x_lab, y_var, y_lab, group_var, x_tick_labs, leg_labels, title) { 
  
  plot<-ggplot(d,aes(x=factor(x_var),y=y_var,fill=factor(group_var)),color=factor(group_var)) +  
    stat_summary(fun.y=mean,position=position_dodge(),geom="bar")+
    theme_classic()+
    coord_cartesian(ylim=c(1,100))
  plot<- plot+theme(text = element_text(size=16))+
    ggtitle(title)+
    theme(title = element_text(hjust = 0.5))+
    scale_fill_manual(name="Victim:", labels = leg_labels, 
                      values = c("gray40", "gray85")) + 
    labs(x = "", y = "", fill = "") + 
    theme_classic()+
    theme(text = element_text(size = 25),
          axis.title.y = element_text(size = 25), 
          axis.title.x = element_text(size = 25, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
          axis.text.x = element_text(size = 20),
          legend.position="top") + 
    stat_summary(fun.data = "mean_cl_boot", color = "black", 
                 position = position_dodge(width = 0.9),
                 geom="errorbar", width = 0.2)+
    scale_x_discrete(labels=x_tick_labs)+
    theme(plot.title = element_text(hjust = 0.5))
  
  plot
  
  return(plot)
}

p2_1 <- BarPlotter(d, d$intention_cond, 'AV Programming', d$outrage, 'Mean Rating', d$person_cond, person_labels, legend_labels, 'Outrage at Firm')
p1_1 <- BarPlotter(d, d$intention_cond, 'AV Programming', d$blame_company, 'Mean Rating', d$person_cond, person_labels, legend_labels, 'Blame of Firm')


dev.new(width=13,height=6,noRStudioGD = TRUE)

figure <- ggarrange(p2_1, p1_1, nrow = 1, ncol = 2, common.legend = TRUE, legend = "top")
annotate_figure(figure, left = text_grob("Mean Rating", color = "black", face = "plain", size = 26, rot = 90),
                bottom = text_grob("AV Programming", color = "black", face = "plain", size = 26, hjust = 0.25, margin(b = 2)))

ggsave(
  "study1_unselfishAlibis.pdf",
  last_plot(),
  dpi = 500
)

plot_files <- list.files(pattern = c("(.pdf|.png)"))
file.move(plot_files, "./plots", overwrite = TRUE)

##================================================================================================================
##END##
##================================================================================================================




















