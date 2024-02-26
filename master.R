### Association between deprivation, ethincity and maternal age on method of delivery in England ###
### Aquilina I et al. 2024  ###


### Define paths -----

rm(list=ls()) # Clear environment

# Specify project folder
path.project <- "C:/Users/sirsa/OneDrive/Documents/2024Aquilina"

# Create outputs folder
dir.create(file.path(getwd(), 'outputs'), recursive = TRUE)

### Select outcome

## Use the following selections

# Elective caesarian sections

i = 80
j = "Elective caesarian section"


### Load packages -----

list.of.packages <- c("data.table", "dplyr", "janitor", "reshape2", "readxl", "sandwich", "msm", "plyr", "ggplot2", "Cairo")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(data.table)
library(dplyr)
library(janitor)
library(reshape2)
library(readxl)
library(sandwich)
library(msm)
library(plyr)
library(ggplot2)
library(Cairo)

### Load data -----

# From https://digital.nhs.uk/data-and-information/publications/statistical/nhs-maternity-statistics
data <- fread("hosp-epis-stat-mat-msdscsv-2022-23.csv") %>%
  clean_names()

data.outcomes <- read_xlsx("hosp-epis-stat-mat-pla-2022-23.xlsx",
                           sheet = 11,
                           skip = 1) %>%
  clean_names()

### Filter and process required data -----

# Age

data.age <- data %>% 
  # Keep trust level data only
  filter(org_level == "Provider") %>% 
  # Keep data for age variable only
  filter(dimension == "AgeAtBookingMotherGroup") %>%
  # Remove unrequired columns
  select(-c(period, dimension, org_level, count_of)) %>%
  # Make age group into column headers
  dcast(org_code + org_name ~ measure) %>%
  # Clean names
  clean_names() %>%
  # Make a total column
  mutate(total = under_20 + x20_to_24 + x25_to_29 + x30_to_34 + x35_to_39 + x40_to_44 + x45_or_over) %>%
  # Calculate proportion of births in 35+
  mutate(prop.over35 = round((100*(x35_to_39 + x40_to_44 + x45_or_over)/total),1)) %>%
  # Remove unrequired columns
  select(-c(total, under_20, x20_to_24, x25_to_29, x30_to_34, x35_to_39, x40_to_44, x45_or_over))

# Ethnicity

data.ethnicity <- data %>%
  # Keep trust level data only
  filter(org_level == "Provider") %>% 
  # Keep data for age variable only
  filter(dimension == "EthnicCategoryMotherGroup") %>%
  # Remove unrequired columns
  select(-c(period, dimension, org_level, count_of)) %>%
  # Make ethnicity group into column headers
  dcast(org_code + org_name ~ measure) %>%
  # Clean names
  clean_names() %>%
  # Make a total column
  mutate(total = any_other_ethnic_group + asian_or_asian_british + black_or_black_british + missing_value_value_outside_reporting_parameters + mixed + not_known + not_stated + white) %>%
  # Calculate proportion of white ethnicity
  mutate(prop.white = round((100*(white)/total),1)) %>%
  # Remove unrequired columns
  select(-c(total, any_other_ethnic_group, asian_or_asian_british, black_or_black_british, missing_value_value_outside_reporting_parameters, mixed, not_known, not_stated, white))

# Deprivation

data.imd <- data %>%
  # Keep trust level data only
  filter(org_level == "Provider") %>% 
  # Keep data for age variable only
  filter(dimension == "DeprivationDecileAtBooking") %>%
  # Remove unrequired columns
  select(-c(period, dimension, org_level, count_of)) %>%
  # Make deprivation decile into column headers
  dcast(org_code + org_name ~ measure) %>%
  # Clean names
  clean_names() %>%
  # Make a total column
  mutate(total = x01_most_deprived + x02 + x03 + x04 + x05 + x06 + x07 + x08 + x09 + x10_least_deprived + missing_value_value_outside_reporting_parameters + pseudo_postcode_recorded_includes_no_fixed_abode_or_resident_overseas + resident_elsewhere_in_uk_channel_islands_or_isle_of_man) %>%
  # Calculate proportion in most deprived quintile
  mutate(prop.deprived = round((100*(x01_most_deprived + x02)/total),1)) %>%
  # Remove unrequired columns
  select(-c(total, x01_most_deprived, x02, x03, x04, x05, x06, x07, x08, x09, x10_least_deprived, missing_value_value_outside_reporting_parameters, pseudo_postcode_recorded_includes_no_fixed_abode_or_resident_overseas, resident_elsewhere_in_uk_channel_islands_or_isle_of_man))

# Outcome data

data.outcome.specific <- data.outcomes %>%
  # Select required columns
  dplyr::select(1,2,36,i) %>%
  # Rename columns
  dplyr::rename(org_code = 1, org_name = 2, outcome = 3, total = 4) %>%
  # Ensure only first three letters of organisation code are kept
  mutate(org_code2 = substr(org_code, start = 1, stop = 3)) %>%
  # Replace three-letter org code columns with the original
  dplyr::select(-c(org_code, org_name)) %>%
  dplyr::rename(org_code = org_code2) %>%
  # Ensure data is numeric
  mutate(total = as.numeric(total)) %>%
  mutate(outcome = as.numeric(outcome))
  


### Link data -----

data.all <- data.age %>%
  # Join deprivation data
  left_join(data.imd, by = c("org_code", "org_name")) %>%
  # Join ethnicity data
  left_join(data.ethnicity, by = c("org_code", "org_name")) %>%
  # Join outcome data
  left_join(data.outcome.specific, by = c("org_code")) %>%
  # Remove rows with NA data
  filter(!is.na(total)) %>%
  # Remove rows with small data (*)
  filter(total != "*")

### Process data -----

data.all <- data.all %>%
  # Create quintiles for each independent variable
  mutate(prop.deprived.quintile = ntile(prop.deprived, 5)) %>%
  mutate(prop.over35.quintile = ntile(prop.over35, 5)) %>%
  mutate(prop.white.quintile = ntile(prop.white, 5)) %>%
  # Calculate outcome rate per 1000
  mutate(outcome.rate = 1000*outcome/total)


# Ensure ntiles are categorical (factors)
data.all$prop.deprived.quintile <- as.factor(data.all$prop.deprived.quintile)
data.all$prop.over35.quintile <- as.factor(data.all$prop.over35.quintile)
data.all$prop.white.quintile <- as.factor(data.all$prop.white.quintile)

### Plot -----

# Function to give SE / CI of data, from http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#Helper%20functions
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- plyr::rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

## Deprivation plot

# Calculate 95% confidence intervals for outcome rate by % deprivation quintile and save
df <- summarySE(data.all,measurevar = "outcome.rate",groupvars = "prop.deprived.quintile")
df$lower.ci <- df$outcome.rate-df$ci #calculate lower and upper CIs
df$upper.ci <- df$outcome.rate+df$ci
dfA<-df[,c(1,2,3,7,8)] #subset data to save
names(dfA)[1] <- "% Most deprived quintile" #rename columns
names(dfA)[2] <- "n"
names(dfA)[3] <- "Outcomes per 1000 births"
names(dfA)[4] <- "Lower 95% CI"
names(dfA)[5] <- "Upper 95% CI"
dfA$`Outcomes per 1000 births`<-plyr::round_any(as.numeric(dfA$`Outcomes per 1000 births`), 0.01)
dfA$`Lower 95% CI`<-round_any(dfA$`Lower 95% CI`, 0.01)
dfA$`Upper 95% CI`<-round_any(dfA$`Upper 95% CI`, 0.01)
names(dfA)[3] <- paste0(j, " rate per 1000 births")
write.csv(dfA,paste0("outputs/Table 1A. ",j," by deprivation quintile.csv"),row.names = F)

# Set axis limits for bar chart
if(max(df$outcome.rate+df$ci)<150){   # set upper y-axis limit to nearest 10 if highest number is <150
  maxy=round_any(max(df$outcome.rate+df$ci), 10, f = ceiling)
}

if(max(df$outcome.rate+df$ci)>=150 && max(df$outcome.rate+df$ci)<=1500){   # set upper y-axis limit to nearest 100 if highest number is <1500
  maxy=round_any(max(df$outcome.rate+df$ci), 100, f = ceiling)
}

if(max(df$outcome.rate+df$ci)>1500){   # set upper y-axis limit to nearest 100 if highest number is >1500
  maxy=round_any(max(df$outcome.rate+df$ci), 1000, f = ceiling)
}

# Plot bar chart of outcome rate by % deprivation quintile (Q5 has the most people in the most deprived quintile)

Cairo(file=paste0("outputs/Figure 1A. ", j, " by deprivation quintile.png"), 
      type="png",
      units="in", 
      width=5, 
      height=4, 
      pointsize=6, 
      dpi=1200)

ggplot(df, aes(x=prop.deprived.quintile, y=outcome.rate)) + 
  geom_bar(position=position_dodge(), stat="identity",colour="black", size=0.4, fill=c("white","#9ecae1","#4292c6","#2171b5","#08306b")) +
  geom_errorbar(aes(ymin=outcome.rate-ci, ymax=outcome.rate+ci),
                width=.3, size=0.4,                   # Width of the error bars, width of actual bar lines
                position=position_dodge())+
  xlab("\n% Most deprived quintile") + ylab(paste0(j, " rate\nper 1000 births\n")) +
  ylim(0,maxy)+
  scale_x_discrete(breaks = unique(df$prop.deprived.quintile))+ 
  ggtitle(paste0(j, " rate\nby deprivation quintile\n"))+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(plot.title = element_text(hjust = 0.5, size = 12), axis.text = element_text(size = 10), axis.title = element_text(size= 10))

dev.off()

## Ethnicity plot

# Calculate 95% confidence intervals for outcome rate by % deprivation quintile and save
df <- summarySE(data.all,measurevar = "outcome.rate",groupvars = "prop.white.quintile")
df$lower.ci <- df$outcome.rate-df$ci #calculate lower and upper CIs
df$upper.ci <- df$outcome.rate+df$ci
dfA<-df[,c(1,2,3,7,8)] #subset data to save
names(dfA)[1] <- "% White quintile" #rename columns
names(dfA)[2] <- "n"
names(dfA)[3] <- "Outcomes per 1000 births"
names(dfA)[4] <- "Lower 95% CI"
names(dfA)[5] <- "Upper 95% CI"
dfA$`Outcomes per 1000 births`<-round_any(dfA$`Outcomes per 1000 births`, 0.01)
dfA$`Lower 95% CI`<-round_any(dfA$`Lower 95% CI`, 0.01)
dfA$`Upper 95% CI`<-round_any(dfA$`Upper 95% CI`, 0.01)
names(dfA)[3] <- paste0(j, " rate per 1000 births")
write.csv(dfA,paste0("outputs/Table 1B. ",j," by white ethnicity quintile.csv"),row.names = F)

# Set axis limits for bar chart
if(max(df$outcome.rate+df$ci)<150){   # set upper y-axis limit to nearest 10 if highest number is <150
  maxy=round_any(max(df$outcome.rate+df$ci), 10, f = ceiling)
}

if(max(df$outcome.rate+df$ci)>=150 && max(df$outcome.rate+df$ci)<=1500){   # set upper y-axis limit to nearest 100 if highest number is <1500
  maxy=round_any(max(df$outcome.rate+df$ci), 100, f = ceiling)
}

if(max(df$outcome.rate+df$ci)>1500){   # set upper y-axis limit to nearest 100 if highest number is >1500
  maxy=round_any(max(df$outcome.rate+df$ci), 1000, f = ceiling)
}

# Plot bar chart of outcome rate by % deprivation quintile (Q5 has the most people in the most deprived quintile)

Cairo(file=paste0("outputs/Figure 1B. ", j, "rate by white ethnicity quintile.png"), 
      type="png",
      units="in", 
      width=5, 
      height=4, 
      pointsize=6, 
      dpi=1200)

ggplot(df, aes(x=prop.white.quintile, y=outcome.rate)) + 
  geom_bar(position=position_dodge(), stat="identity",colour="black", size=0.4, fill=c("white","#9ecae1","#4292c6","#2171b5","#08306b")) +
  geom_errorbar(aes(ymin=outcome.rate-ci, ymax=outcome.rate+ci),
                width=.3, size=0.4,                   # Width of the error bars, width of actual bar lines
                position=position_dodge())+
  xlab("\n% White ethnicity quntile") + ylab(paste0(j, " rate\nper 1000 births\n")) +
  ylim(0,maxy)+
  scale_x_discrete(breaks = unique(df$prop.white.quintile))+ 
  ggtitle(paste0(j, " rate\nby White ethnicity quintile\n"))+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(plot.title = element_text(hjust = 0.5, size = 12), axis.text = element_text(size = 10), axis.title = element_text(size= 10))

dev.off()

## Age plot

# Calculate 95% confidence intervals for outcome rate by % deprivation quintile and save
df <- summarySE(data.all,measurevar = "outcome.rate",groupvars = "prop.over35.quintile")
df$lower.ci <- df$outcome.rate-df$ci #calculate lower and upper CIs
df$upper.ci <- df$outcome.rate+df$ci
dfA<-df[,c(1,2,3,7,8)] #subset data to save
names(dfA)[1] <- "% Over 35 quintile" #rename columns
names(dfA)[2] <- "n"
names(dfA)[3] <- "Outcomes per 1000 births"
names(dfA)[4] <- "Lower 95% CI"
names(dfA)[5] <- "Upper 95% CI"
dfA$`Outcomes per 1000 births`<-round_any(dfA$`Outcomes per 1000 births`, 0.01)
dfA$`Lower 95% CI`<-round_any(dfA$`Lower 95% CI`, 0.01)
dfA$`Upper 95% CI`<-round_any(dfA$`Upper 95% CI`, 0.01)
names(dfA)[3] <- paste0(j, " rate per 1000 births")
write.csv(dfA,paste0("outputs/Table 1C. ",j," by over-35yr quintile.csv"),row.names = F)

# Set axis limits for bar chart
if(max(df$outcome.rate+df$ci)<150){   # set upper y-axis limit to nearest 10 if highest number is <150
  maxy=round_any(max(df$outcome.rate+df$ci), 10, f = ceiling)
}

if(max(df$outcome.rate+df$ci)>=150 && max(df$outcome.rate+df$ci)<=1500){   # set upper y-axis limit to nearest 100 if highest number is <1500
  maxy=round_any(max(df$outcome.rate+df$ci), 100, f = ceiling)
}

if(max(df$outcome.rate+df$ci)>1500){   # set upper y-axis limit to nearest 100 if highest number is >1500
  maxy=round_any(max(df$outcome.rate+df$ci), 1000, f = ceiling)
}

# Plot bar chart of outcome rate by % deprivation quintile (Q5 has the most people in the most deprived quintile)

Cairo(file=paste0("outputs/Figure 1C. ", j, " by over-35yr quintile.png"), 
      type="png",
      units="in", 
      width=5, 
      height=4, 
      pointsize=6, 
      dpi=1200)

ggplot(df, aes(x=prop.over35.quintile, y=outcome.rate)) + 
  geom_bar(position=position_dodge(), stat="identity",colour="black", size=0.4, fill=c("white","#9ecae1","#4292c6","#2171b5","#08306b")) +
  geom_errorbar(aes(ymin=outcome.rate-ci, ymax=outcome.rate+ci),
                width=.3, size=0.4,                   # Width of the error bars, width of actual bar lines
                position=position_dodge())+
  xlab("\n% Over 35 years quntile") + ylab(paste0(j, " rate\nper 1000 births\n")) +
  ylim(0,maxy)+
  scale_x_discrete(breaks = unique(df$prop.over35.quintile))+ 
  ggtitle(paste0(j, " rate\nby age over 35 years quintile\n"))+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(plot.title = element_text(hjust = 0.5, size = 12), axis.text = element_text(size = 10), axis.title = element_text(size= 10))

dev.off()


### Analysis -----

## Univariate

# By % most deprived quintile

summary(m1 <- glm(outcome.rate ~ prop.deprived.quintile, family="quasipoisson", data=data.all))

# calculate robust standard errors (and related p values) to control for mild violation of the distribution assumption that the variance equals the mean
cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
               LL = coef(m1) - 1.96 * std.err,
               UL = coef(m1) + 1.96 * std.err)

# calculate incident rate ratios and SEs, with CIs, using Delta method
s <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4), ~ exp(x5)), 
                 coef(m1), cov.m1)
rexp.est <- exp(r.est[, -3]) # exponentiate old estimates dropping the p values
rexp.est[, "Robust SE"] <- s #replace SEs with estimates for exponentiated coefficients
rexp.est
table2a <- as.data.frame(rexp.est) #ready to save
table2a$Estimate <- round_any(table2a$Estimate,0.01) #round to 2dp
table2a$LL <- round_any(table2a$LL,0.01)
table2a$UL <- round_any(table2a$UL,0.01)
table2a <- table2a[,-2] #remove SE column
table2a[1, c(1,2,3)] <- c("Reference","","") #rename Q1 as reference
row.names(table2a)[1] <- "Deprivation Quintile 1" #rename row 1
row.names(table2a)[2] <- "Deprivation Quintile 2" #rename row 2
row.names(table2a)[3] <- "Deprivation Quintile 3" #rename row 3
row.names(table2a)[4] <- "Deprivation Quintile 4" #rename row 4
row.names(table2a)[5] <- "Deprivation Quintile 5" #rename row 5

  # NOTE estimates shows how many times more the IRR is compared to the reference group, or the % increase in IRR for every unit change in the predictor variable

# By % white quintile

summary(m1 <- glm(outcome.rate ~ prop.white.quintile, family="quasipoisson", data=data.all))

# calculate robust standard errors (and related p values) to control for mild violation of the distribution assumption that the variance equals the mean
cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
               LL = coef(m1) - 1.96 * std.err,
               UL = coef(m1) + 1.96 * std.err)

# calculate incident rate ratios and SEs, with CIs, using Delta method
s <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4), ~ exp(x5)), 
                 coef(m1), cov.m1)
rexp.est <- exp(r.est[, -3]) # exponentiate old estimates dropping the p values
rexp.est[, "Robust SE"] <- s #replace SEs with estimates for exponentiated coefficients
rexp.est
table2b <- as.data.frame(rexp.est) #ready to save
table2b$Estimate <- round_any(table2b$Estimate,0.01) #round to 2dp
table2b$LL <- round_any(table2b$LL,0.01)
table2b$UL <- round_any(table2b$UL,0.01)
table2b <- table2b[,-2] #remove SE column
table2b[1, c(1,2,3)] <- c("Reference","","") #rename Q1 as reference
row.names(table2b)[1] <- "% White Quintile 1" #rename row 1
row.names(table2b)[2] <- "% White Quintile 2" #rename row 2
row.names(table2b)[3] <- "% White Quintile 3" #rename row 3
row.names(table2b)[4] <- "% White Quintile 4" #rename row 4
row.names(table2b)[5] <- "% White Quintile 5" #rename row 5


# By % over 35yr quintile

summary(m1 <- glm(outcome.rate ~ prop.over35.quintile, family="quasipoisson", data=data.all))

# calculate robust standard errors (and related p values) to control for mild violation of the distribution assumption that the variance equals the mean
cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
               LL = coef(m1) - 1.96 * std.err,
               UL = coef(m1) + 1.96 * std.err)

# calculate incident rate ratios and SEs, with CIs, using Delta method
s <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4), ~ exp(x5)), 
                 coef(m1), cov.m1)
rexp.est <- exp(r.est[, -3]) # exponentiate old estimates dropping the p values
rexp.est[, "Robust SE"] <- s #replace SEs with estimates for exponentiated coefficients
rexp.est
table2c <- as.data.frame(rexp.est) #ready to save
table2c$Estimate <- round_any(table2c$Estimate,0.01) #round to 2dp
table2c$LL <- round_any(table2c$LL,0.01)
table2c$UL <- round_any(table2c$UL,0.01)
table2c <- table2c[,-2] #remove SE column
table2c[1, c(1,2,3)] <- c("Reference","","") #rename Q1 as reference
row.names(table2c)[1] <- "% Over-35yr Quintile 1" #rename row 1
row.names(table2c)[2] <- "% Over-35yr Quintile 2" #rename row 2
row.names(table2c)[3] <- "% Over-35yr Quintile 3" #rename row 3
row.names(table2c)[4] <- "% Over-35yr Quintile 4" #rename row 4
row.names(table2c)[5] <- "% Over-35yr Quintile 5" #rename row 5

# save univariate regression results

table2 <- rbind(table2a, table2b, table2c) # Combine results tables
write.csv(table2,paste0("outputs/Table 2. ", j, " - univariate regression.csv"),row.names = T)

## Multivariate

summary(m1 <- glm(outcome.rate ~ prop.deprived.quintile + prop.white.quintile + prop.over35.quintile, family="quasipoisson", data=data.all))

cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
               LL = coef(m1) - 1.96 * std.err,
               UL = coef(m1) + 1.96 * std.err)
s <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4), ~ exp(x5), ~ exp(x6), ~ exp(x7), ~ exp(x8), ~ exp(x9), ~ exp(x10), ~ exp(x11), ~ exp(x12), ~ exp(x13)), 
                 coef(m1), cov.m1)
rexp.est <- exp(r.est[, -3]) 
rexp.est[, "Robust SE"] <- s 
rexp.est
table3 <- as.data.frame(rexp.est) #ready to save
table3$Estimate <- round_any(table3$Estimate,0.01) #round to 2dp
table3$LL <- round_any(table3$LL,0.01)
table3$UL <- round_any(table3$UL,0.01)
table3 <- table3[,-2] #remove SE column
table3[1, c(1,2,3)] <- c("Reference","","") #rename Q1 as reference
ref <- data.frame(Estimate="Reference",LL="",UL="") #Create a row for reference
table3 <- rbind(table3[1:5,],ref,table3[6:9,],ref,table3[10:13,])
row.names(table3)[1] <- "Deprivation Quintile 1" # Rename rows
row.names(table3)[2] <- "Deprivation Quintile 2"
row.names(table3)[3] <- "Deprivation Quintile 3"
row.names(table3)[4] <- "Deprivation Quintile 4"
row.names(table3)[5] <- "Deprivation Quintile 5"
row.names(table3)[6] <- "% White ethnicity Quintile 1" 
row.names(table3)[7] <- "% White ethnicity Quintile 2" 
row.names(table3)[8] <- "% White ethnicity Quintile 3" 
row.names(table3)[9] <- "% White ethnicity Quintile 4" 
row.names(table3)[10] <- "% White ethnicity Quintile 5" 
row.names(table3)[11] <- "% Over-35yr Quintile 1"
row.names(table3)[12] <- "% Over-35yr Quintile 2"
row.names(table3)[13] <- "% Over-35yr Quintile 3"
row.names(table3)[14] <- "% Over-35yr Quintile 4"
row.names(table3)[15] <- "% Over-35yr Quintile 5"


# save multivariate regression results
write.csv(table3,paste0("outputs/Table 3. ", j, " - multivariate regression.csv"),row.names = T)







