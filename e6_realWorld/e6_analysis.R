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
)
library("lmerTest")

##================================================================================================================
                                              ##IMPORT & PRE-PROCESS d##
##================================================================================================================

#get subjects from e1
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory

#read data
d_raw <- read.csv('e6_data.csv')
d_raw <- subset(d_raw, d_raw$att1==2 & d_raw$att2==2)

#define new d frame that we'll extract preprocessed d into
d_subset <- array(dim=c(dim(d_raw)[1], 2))
colnames(d_subset) <- c('order_cond', 'choice')

d_subset <- as.data.frame(d_subset, stringsAsFactors=FALSE)

#extract the good d from the middle part of the raw d
for(i in 1:dim(d_raw)[1]) {
    curr <- d_raw[i,c(14,16,18,20,22,24)][!is.na(d_raw[i,c(14,16,18,20,22,24)])] #for a given row, get only the non NA values
    d_subset[i,2] <- as.numeric(curr[curr!= ""]) #and only the non-empty values
    cond_names <- d_raw[i,37][!is.na(d_raw[i,37])]
    d_subset[i,1] <- strsplit(cond_names[[1]], "page")[[1]][2]
}

#merge good d with first and last halves of the original d
d_raw <- cbind(d_subset, d_raw[,26:36])

# number of subjects before exclusions
n_original <- dim(d_raw)[1]
n_original

##================================================================================================================
                                                ##EXCLUSIONS##
##================================================================================================================

#perform exclusions based on attention and comprehension checks
d <- subset(d_raw, (d_raw$comp1 == 1) |
                   (d_raw$comp2 == 2))

#number of subjects after exclusions
n_after_exclusions <- dim(d)[1]
n_after_exclusions
n_original-n_after_exclusions

percent_excluded <- (n_original - n_after_exclusions)/n_original #34%
percent_excluded

##================================================================================================================
                                                  ##ANALYSIS##
##================================================================================================================

# choices
results_table <- table(d$choice)
chisq.test(results_table)

results_table[1]/sum(results_table) #prosocial
results_table[2]/sum(results_table) #overt selfish
results_table[3]/sum(results_table) #unselfish alibi

results_table_subset <- table(d$choice)[c(2,3)]
chisq.test(results_table_subset)

#pk
mean(d$pk_egal_1)
sd(d$pk_egal_1)

mean(d$pk_selfish_1)
sd(d$pk_selfish_1)

mean(d$pk_alibi_1)
sd(d$pk_alibi_1)

var.test(d$pk_selfish_1, d$pk_alibi_1)
t.test(d$pk_selfish_1, d$pk_alibi_1, paired=TRUE, var.equal=TRUE)

#sk
mean(d$sk_egal_1)
sd(d$sk_egal_1)

mean(d$sk_selfish_1)
sd(d$sk_selfish_1)

mean(d$sk_alibi_1)
sd(d$sk_alibi_1)

var.test(d$pk_selfish_1, d$pk_alibi_1)
t.test(d$sk_selfish_1, d$sk_alibi_1, paired=TRUE, var.equal=TRUE)

#ck
mean(d$ck_egal_1)
sd(d$ck_egal_1)

mean(d$ck_selfish_1)
sd(d$ck_selfish_1)

mean(d$ck_alibi_1)
sd(d$ck_alibi_1)

var.test(d$ck_selfish_1, d$ck_alibi_1)
t.test(d$ck_selfish_1, d$ck_alibi_1, paired=TRUE, var.equal=TRUE)

##================================================================================================================
##END##
##================================================================================================================




















