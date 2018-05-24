
setwd("~/Desktop/TPM internship")
library(gridExtra)
library(stringr)
library(dplyr)
library(plyr)
library(shiny)
library(GGally)
library(geomnet)
library(ggnetwork)
library(ggraph)
library(networkly)
library(tidyr)
library(textclean)
library(sna)
library(data.table)
library(dtplyr)
library(reshape2)
library(reshape)
library(ggpubr)

## Read in the data, remove unnecessary columns & concatenate first and last names into full name
marsden_data <- read.csv('Marsden_Predicted_Gender.csv', stringsAsFactors = FALSE)
marsden_data <- marsden_data[, c(2:5,7:11)]
marsden_data$Investigator <- paste(marsden_data$FirstName, marsden_data$Surname, sep = ' ')

## Get list of unique investigators and their genders
marsden_people <- unique(marsden_data[, c('Investigator', 'Gender')])

## Add full name of panel 
marsden_data <- marsden_data %>% mutate(panel_full = case_when(Panel == 'BMS' ~ 'Biomedical Sciences',
                                                               Panel == 'ESA' ~ 'Earth Sciences & Astronomy', 
                                                               Panel == 'EEB' ~ 'Ecology, Evolution & Behaviour', 
                                                               Panel == 'PCB' ~ 'Physics, Chemistry & Biochemistry', 
                                                               Panel == 'EIS' ~ 'Engineering & Interdisciplinary Sciences',
                                                               Panel == 'SOC' ~ 'Social Sciences', 
                                                               Panel == 'HUM' ~ 'Humanities', 
                                                               Panel == 'MIS' ~ 'Mathematics & Information Sciences',
                                                               Panel == 'CMP' ~ 'Cellular, Molecular & Physiological Biology', 
                                                               Panel == 'EHB' ~ 'Economics & Human and Behavioural Sciences',
                                                               Panel == 'BMS' ~ 'Biomedical Sciences', 
                                                               Panel == 'PSE' ~ 'Physical Sciences & Engineering'))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Exploratory data analysis ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

## Total number of investigators
length(unique(marsden_data$Investigator))
## 2284

## Number of investigators by gender
table(marsden_people$Gender)
# F    M 
# 6  668 1610 

## Total number of proposals
length(unique(marsden_data$Proposal))
## 1029

## Number of proposals by panel
panel_proposal <- unique(marsden_data[, c('Proposal',"panel_full")])
test <- as.data.frame(table(panel_proposal$panel_full))
# 1                          Biomedical Sciences  102
# 2  Cellular, Molecular & Physiological Biology   95
# 3                   Earth Sciences & Astronomy  107
# 4               Ecology, Evolution & Behaviour  131
# 5   Economics & Human and Behavioural Sciences   82
# 6     Engineering & Interdisciplinary Sciences   77
# 7                                   Humanities   90
# 8           Mathematics & Information Sciences  110
# 9              Physical Sciences & Engineering   26
# 10           Physics, Chemistry & Biochemistry   77
# 11                             Social Sciences  133

# Average number of people in each proposal, by panel
setDT(marsden_data)
N_panel_proposal <- marsden_data[, .N, by = c('Proposal', 'panel_full')]
setDT(N_panel_proposal) 

N_panel_proposal[, (avg_by_panel = mean(N)), by = panel_full]
# 1:     Engineering & Interdisciplinary Sciences 2.896104
# 2:                   Earth Sciences & Astronomy 4.878505
# 3:            Physics, Chemistry & Biochemistry 2.441558
# 4:                          Biomedical Sciences 2.460784
# 5:           Mathematics & Information Sciences 2.481818
# 6:  Cellular, Molecular & Physiological Biology 2.863158
# 7:                                   Humanities 1.511111
# 8:                              Social Sciences 2.819549
# 9:               Ecology, Evolution & Behaviour 3.183206
# 10:  Economics & Human and Behavioural Sciences 2.743902
# 11:             Physical Sciences & Engineering 2.653846


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
##### Get dataframe of collaborations - used as edges #####
#     table in network and used in other analysis.        #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

proposals_unique <- unique(marsden_data$Proposal)

combinations_marsden = data.frame(personA = character(), personB = character(), 
                                  paper_ID = character(), panel = character(),
                                  Year = integer())

## Populate combinations_marsden with combinations of people working together
for (proposal in proposals_unique){
  Marsden_proposal <- marsden_data[marsden_data$Proposal == proposal,]
  
  if (nrow(Marsden_proposal) > 1) {
    combo_proposal <- combn(Marsden_proposal$Investigator, 2)
    
    for (i in 1:ncol(combo_proposal)){
      combinations_marsden <- rbind(combinations_marsden, 
                                    cbind(t(combo_proposal[,i]), 
                                          Marsden_proposal$Proposal[1], 
                                          Marsden_proposal$panel_full[1],
                                          Marsden_proposal$Year[1]))
    }
  }
  else{
  }
}

# Change column names & change classes of variables
colnames(combinations_marsden) <- c('personA','personB', 'proposal', 'panel', 'year')
combinations_marsden[, 1:4] <- sapply(combinations_marsden[,1:4],as.character)
combinations_marsden$year <- as.integer(as.character(combinations_marsden$year))

head(combinations_marsden)
#           personA            personB   proposal                                    panel year
# 1 Aaron Marshall   David Harrington 12-UOC-091 Engineering & Interdisciplinary Sciences 2012
# 2     Aaron Wech      Carolin Boese 13-VUW-101               Earth Sciences & Astronomy 2013
# 3     Aaron Wech       David Shelly 13-VUW-101               Earth Sciences & Astronomy 2013
# 4     Aaron Wech       John Townend 13-VUW-101               Earth Sciences & Astronomy 2013
# 5     Aaron Wech      Martha Savage 13-VUW-101               Earth Sciences & Astronomy 2013
# 6     Aaron Wech Sandra Bourguignon 13-VUW-101               Earth Sciences & Astronomy 2013


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
##### Compare number of collaborations of male and female researchers #####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

## Create adjacency matrix and get degrees of connectivity
g_combo=graph.data.frame(combinations_marsden[,1:2])
adj <- get.adjacency(g_combo,sparse=FALSE)

degrees_connection <- degree(adj)

# Get number of connections that each investigator has 
unique_personA <- as.data.frame(unique(combinations_marsden$personA))
unique_personB <- as.data.frame(unique(combinations_marsden$personB))

colnames(unique_personA) <- "Investigator"
colnames(unique_personB) <- "Investigator"

unique_persons <- unique(rbind(unique_personA, unique_personB))

## Check lengths are the same
dim(unique_persons) ## 2129 
length(degrees_connection) ## 2129

## Bind people to the number of connections they have 
connections <- data.frame(cbind(unique_persons, degrees_connection))

## Spot checks of connections - should all return true 
nrow(combinations_marsden[combinations_marsden$personA == 'Martha Savage' | 
                            combinations_marsden$personB == 'Martha Savage',]) == connections[connections$Investigator == 'Martha Savage',2]
nrow(combinations_marsden[combinations_marsden$personA == 'Stuart Wimbush' | 
                            combinations_marsden$personB == 'Stuart Wimbush',]) == connections[connections$Investigator == 'Stuart Wimbush',2]
nrow(combinations_marsden[combinations_marsden$personA == 'Charles Semple' | 
                            combinations_marsden$personB == 'Charles Semple',]) == connections[connections$Investigator == 'Charles Semple',2]
nrow(combinations_marsden[combinations_marsden$personA == 'Andrew Gorman' | 
                            combinations_marsden$personB == 'Andrew Gorman',]) == connections[connections$Investigator == 'Andrew Gorman',2]
nrow(combinations_marsden[combinations_marsden$personA == 'Ben Ruck' | 
                            combinations_marsden$personB == 'Ben Ruck',]) == connections[connections$Investigator == 'Ben Ruck',2]
nrow(combinations_marsden[combinations_marsden$personA == 'William Wilson' | 
                            combinations_marsden$personB == 'William Wilson',]) == connections[connections$Investigator == 'William Wilson',2]

# Add on the gender of each person 
connections_gender <- merge(connections, marsden_people, by = "Investigator")

## Plot number of connections by gender - shows that men & women have a similar number of connections
plot_male <- ggplot(connections_gender[connections_gender$Gender == "M",], aes(x = degrees_connection)) + 
  geom_histogram(aes(y=..count../sum(..count..)), binwidth = 2, fill = '#01579bff') +
  scale_x_continuous(limits = c(0, 30)) +
  scale_y_continuous(limits = c(0, 0.5)) +
  theme_bw() +
  theme(panel.border = element_blank(), axis.text=element_text(size=13), axis.title=element_text(size=15)) +
  xlab("Number of Collaborations") +
  ylab("Proportion of Male Researchers")

plot_female <- ggplot(connections_gender[connections_gender$Gender == "F",], aes(x = degrees_connection)) + 
  geom_histogram(aes(y=..count../sum(..count..)), binwidth = 2, fill = '#f46524ff') +
  scale_x_continuous(limits = c(0, 30)) +
  scale_y_continuous(limits = c(0, 0.5)) +
  theme_bw() +
  theme(panel.border = element_blank(), axis.text=element_text(size=13), axis.title=element_text(size=15)) +
  xlab("Number of Collaborations") +
  ylab("Proportion of Female Researchers")

grid.arrange(plot_male, plot_female, nrow = 1) 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
##### Get gender split of panels by panel and over time  #####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### By panel
panel_gender <- unique(marsden_data[,c("Investigator", "Gender", "panel_full")])
panel_gender_tbl <- prop.table(table(panel_gender$panel_full, panel_gender$Gender), margin = 1)

#                                                         F           M
# Biomedical Sciences                         0.000000000 0.306930693 0.693069307
# Cellular, Molecular & Physiological Biology 0.004347826 0.239130435 0.756521739
# Earth Sciences & Astronomy                  0.002512563 0.223618090 0.773869347
# Ecology, Evolution & Behaviour              0.000000000 0.250000000 0.750000000
# Economics & Human and Behavioural Sciences  0.005291005 0.396825397 0.597883598
# Engineering & Interdisciplinary Sciences    0.000000000 0.201970443 0.798029557
# Humanities                                  0.008196721 0.442622951 0.549180328
# Mathematics & Information Sciences          0.010050251 0.140703518 0.849246231
# Physical Sciences & Engineering             0.000000000 0.076923077 0.923076923
# Physics, Chemistry & Biochemistry           0.000000000 0.200000000 0.800000000
# Social Sciences                             0.000000000 0.513119534 0.486880466


panel_gender_df <- as.data.frame(panel_gender_tbl)
colnames(panel_gender_df) <- c('Panel', 'Gender', 'Proportion')

panel_gender_df$Gender <- as.character(panel_gender_df$Gender)
panel_gender_df[1:11, 2] <- 'Unknown'

# Plot gender split by panel
ggplot(panel_gender_df, aes(x=Panel, y = Proportion, fill = Gender)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values=c("#f46524ff", "#01579bff", "#27c7bdff")) +
  theme(axis.text=element_text(size=13), axis.title=element_text(size=15,face="bold"), 
        plot.title = element_text(size = 17, face = "bold", hjust = 0.5), 
        legend.text = element_text(size=13), legend.title = element_text(size = 15, face = "bold")) +
  ggtitle("Marsden Fund Panel by Gender of Recipients") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) 

### By year 
gender_year <- unique(marsden_data[,c("Investigator", "Gender", "Year")])
gender_year_tbl <- prop.table(table(gender_year$Year, gender_year$Gender), margin = 1)

#                   F           M
# 2008 0.003952569 0.229249012 0.766798419
# 2009 0.000000000 0.278125000 0.721875000
# 2010 0.003703704 0.244444444 0.751851852
# 2011 0.000000000 0.281746032 0.718253968
# 2012 0.000000000 0.261261261 0.738738739
# 2013 0.003086420 0.280864198 0.716049383
# 2014 0.000000000 0.289377289 0.710622711
# 2015 0.000000000 0.351449275 0.648550725
# 2016 0.006389776 0.297124601 0.696485623
# 2017 0.002512563 0.329145729 0.668341709

gender_year_df <- as.data.frame(gender_year_tbl)
colnames(gender_year_df) <- c('Year', 'Gender', 'Proportion')

gender_year_df$Gender <- as.character(gender_year_df$Gender)
gender_year_df$Year <- as.integer(as.character(gender_year_df$Year))

gender_year_df[1:10, 2] <- 'Unknown'

# Plot gender split over time 
ggplot(gender_year_df, aes(x = Year, y = Proportion, col = Gender)) + 
  geom_line(size=1) + 
  scale_colour_manual(values=c("#f46524ff", "#01579bff", "#27c7bdff")) +
  scale_x_continuous(breaks = seq(2008, 2017, 1)) +
  ggtitle('Recipients of the Marsden Fund by Gender over Time (2008 - 2017)') +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 17), legend.key = element_rect(fill = "white"),
        panel.background = element_blank(), panel.grid.major = element_line(colour = "grey", size=0.1), 
        axis.title=element_text(size=15), axis.text = element_text(size = 13), 
        legend.title = element_text(size=15), legend.text = element_text(size = 13))

### By year and panel 
panel_gender_yr <- unique(marsden_data[,c("Investigator", "Gender", "panel_full", "Year")])
panel_gender_yr_tbl <- table(panel_gender_yr[, c('Gender', 'panel_full', 'Year')])
panel_gender_year_df <- as.data.frame(panel_gender_yr_tbl)

panel_gender_yr_prop <- panel_gender_year_df %>%
  group_by(Year, panel_full) %>%
  mutate(countT= sum(Freq)) %>%
  group_by(Gender, add=TRUE) %>%
  mutate(per=Freq/countT)

panel_gender_yr_prop$Year <- as.integer(as.character(panel_gender_yr_prop$Year))

# Create custom colour scale for panels - colour
scale_colour_panel <- function(...){
  ggplot2:::manual_scale('colour', 
                         values = setNames(c('#1f78b4','#33a02c', '#00199b',
                                             '#ff51ae','#e31a1c','#f46524ff',
                                             '#9957e0', '#ffd400', '#964B00', 
                                             '#b21081', '#8e9b00'),
                                           unique(nodes_full_panel$panel_full)), 
                         ...)
}


## Plot by year and panel
ggplot(panel_gender_yr_prop[panel_gender_yr_prop$panel_full != 'Physical Sciences & Engineering' 
                            & panel_gender_yr_prop$Gender == 'F',], aes(x = Year, y = per, col = panel_full)) +
  geom_line() +
  facet_wrap(~panel_full, ncol = 2) +
  scale_colour_panel() +
  ggtitle("Proportion of Female Recipients of the Marsden Fund by Panel over Time (2008 - 2017)") +
  theme(legend.position = "none", panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "grey", size=0.2), 
        panel.border = element_rect(fill=NA, color = 'grey'),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
        axis.title = element_text(size = 13), 
        axis.text = element_text(size = 11), 
        strip.background =element_rect(fill="white"), 
        strip.text = element_text(size = 15)) +
  scale_x_continuous(breaks = seq(2008, 2017, 1)) +
  ylab("Proportion of Female Researchers ")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
##### Create nodes table for network analysis - #####
##### Each investigator is a node.              #####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Remove unnecessary columns
nodes <- unique(marsden_data[, c(10,9)])

head(nodes)
# Investigator Gender
# 1: Aaron Marshall      M
# 2:     Aaron Wech      M
# 3:      Abha Sood      F
# 4: Adam Patterson      M
# 5:       Adam Day      M
# 6:  Adam Hartland      M

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
##### Plot Marsden Network by Gender #####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

Mnet_gender <- fortify(as.edgedf(combinations_marsden), nodes)

set.seed(123)
g_gender <- ggplot(data = Mnet_gender, aes(from_id = from_id, to_id = to_id)) +
  geom_net(aes(color = Gender, alpha = year), layout.alg = "fruchtermanreingold", 
           size = 2, labelon = FALSE, vjust = -0.6, ecolour = "grey60",
           directed =FALSE, fontsize = 3, linewidth = 0.5) +
  scale_alpha_continuous(breaks = seq(2008, 2017, 1), super = ScaleContinuous) +
  ggtitle('Collaboration in Research for the Marsden Fund from 2008 - 2017, by Gender') +
  theme(plot.title = element_text(hjust = 0.5, size = 17, face = "bold")) +
  scale_colour_manual(values = c("M" = "#01579bff", "F" = "#f46524ff", "#27c7bd")) +
  xlim(c(-0.05, 1.05)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(legend.position = "bottom", legend.key = element_rect(fill = "white"), 
        legend.text=element_text(size=11), legend.title=element_text(size=13, face="bold")) +
  guides(colour=guide_legend(override.aes = list(size = 5)),
         alpha = guide_legend(nrow = 1, override.aes = list(size = 5))) +
  theme(legend.box = "horizontal") +
  labs(color = "Gender:", alpha = "Year:") 

print(g_gender)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
##### Plot Marsden Network by Panel #####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

## Create nodes data for plotting by panel - if one person has submitted to >1 panel, 
## each one is a seperate node
nodes_full_panel <- marsden_data[, c(10,1,4,9,11)]

head(nodes_full_panel)
#      Investigator Year   Proposal Gender                               panel_full
# 1: Aaron Marshall 2012 12-UOC-091      M Engineering & Interdisciplinary Sciences
# 2:     Aaron Wech 2013 13-VUW-101      M               Earth Sciences & Astronomy
# 3: Aaron Marshall 2017 17-UOC-064      M        Physics, Chemistry & Biochemistry
# 4:      Abha Sood 2013 13-GNS-016      F               Earth Sciences & Astronomy
# 5: Adam Patterson 2013 13-UOA-131      M                      Biomedical Sciences
# 6:       Adam Day 2013 13-VUW-017      M       Mathematics & Information Sciences

Mnet_panel <- fortify(as.edgedf(combinations_marsden), nodes_full_panel)

#Plot network by panel
set.seed(123)
g_panel <- ggplot(data = Mnet_panel, aes(from_id = from_id, to_id = to_id)) +
  geom_net(aes(color = panel_full, alpha = Year), layout.alg = "fruchtermanreingold", 
           size = 2, labelon = FALSE, vjust = -0.6, ecolour = "grey60",
           directed =FALSE, fontsize = 3, linewidth = 0.5) +
  scale_alpha_continuous(breaks = seq(2008, 2017, 1), super = ScaleContinuous) +
  scale_colour_panel() +
  ggtitle("Collaboration in Research for the Marsden Fund from 2008 - 2017, by Marsden Panel") +
  theme(plot.title = element_text(hjust = 0.5, size = 17, face = "bold")) +
  xlim(c(-0.05, 1.05)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(legend.position = "bottom", legend.key = element_rect(fill = "white"), 
        legend.text=element_text(size=11), legend.title=element_text(size=13, face="bold")) +
  guides(colour=guide_legend(nrow=4, override.aes = list(size = 5),  title.position = "top"),
         alpha = guide_legend(nrow = 1, override.aes = list(size = 5))) +
  labs(color = "Panel") +
  theme(legend.box = "vertical")

print(g_panel)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
##### Analysis of Homophily #####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Filter to people with 5 connections or more
many_connections_gender <- connections_gender %>% filter(degrees_connection >= 5)

## Calculate the ratio of actual vs. expected gender proportions. 
## Get proportion of women in each panel. Divide each persons proportion of female connections 
## by this proportion. > 1 indicates more female connections than expected. < 1 indicates
## fewer female connections than expected.

## Add gender information to combinations dataframe
combinations_gender <- combinations_marsden %>%  
  merge(nodes, by.x = 'personA', by.y = 'Investigator') %>%
  merge(nodes, by.x = 'personB', by.y = 'Investigator')

combinations_gender <- combinations_gender[, c(2, 6, 1, 7, 3, 4, 5)]

colnames(combinations_gender) <- c('personA', 'genderA', 'personB', 'genderB', 'proposal','panel', 'year')
head(combinations_gender)

## Spot checks - gender. Should all result in TRUE
nrow(combinations_gender[combinations_gender$personA == 'Andrew Gorman' | 
                           combinations_gender$personB == 'Andrew Gorman',]) == connections[connections$Investigator == 'Andrew Gorman',2]
nrow(combinations_gender[combinations_gender$personA == 'Martha Savage' | 
                           combinations_gender$personB == 'Martha Savage',]) == connections[connections$Investigator == 'Martha Savage',2]
nrow(combinations_gender[combinations_gender$personA == 'Stuart Wimbush' | 
                           combinations_gender$personB == 'Stuart Wimbush',]) == connections[connections$Investigator == 'Stuart Wimbush',2]
nrow(combinations_gender[combinations_gender$personA == 'Charles Semple' | 
                           combinations_gender$personB == 'Charles Semple',]) == connections[connections$Investigator == 'Charles Semple',2]
nrow(combinations_gender[combinations_gender$personA == 'Ben Ruck' | 
                           combinations_gender$personB == 'Ben Ruck',]) == connections[connections$Investigator == 'Ben Ruck',2]
nrow(combinations_gender[combinations_gender$personA == 'William Wilson' | 
                           combinations_gender$personB == 'William Wilson',]) == connections[connections$Investigator == 'William Wilson',2]

setDT(combinations_gender)

## Create copy of each row and reverse the order of columns so that the person is always in col1
combinations_gender1 <- copy(combinations_gender)
combinations_gender1[,personC := personA]
combinations_gender1[,genderC := genderA]
combinations_gender1[,personA := personB]
combinations_gender1[,genderA := genderB]
combinations_gender1[,personB := personC]
combinations_gender1[,genderB := genderC]
combinations_gender1[,personC := NULL]
combinations_gender1[,genderC := NULL]

comb <- rbind(combinations_gender,combinations_gender1)

person_gender_numbers <- comb[,.(.N),by=c("personA","genderB")]

setorder(person_gender_numbers, personA)

person_gender_numbers2 <- merge(person_gender_numbers[genderB == "M",c("personA","N"),with=F],person_gender_numbers[genderB == "F",c("personA","N"),with=F],
                                by = "personA",all = TRUE)

colnames(person_gender_numbers2) <- c('personA', 'N_Male', 'N_Female')

## Change NAs to zero - indicating no collaborations
person_gender_numbers2[is.na(person_gender_numbers2)] <- 0

## Calculate totals and proportions of collaborations by gender
person_gender_numbers2 <- person_gender_numbers2 %>% 
  group_by(personA) %>% 
  mutate(N_Total = sum(N_Male, N_Female)) %>%
  mutate(Prop_Male = N_Male / N_Total, Prop_Female = N_Female / N_Total)

# personA N_Male N_Female N_Total Prop_Male Prop_Female
# <chr>  <dbl>    <dbl>   <dbl>     <dbl>       <dbl>
# 1    Aaron Marshall       2        1       3 0.6666667   0.3333333
# 2        Aaron Wech       3        3       6 0.5000000   0.5000000
# 3         Abha Sood      10        0      10 1.0000000   0.0000000
# 4          Adam Day       1        0       1 1.0000000   0.0000000
# 5     Adam Hartland       2        0       2 1.0000000   0.0000000
# 6    Adam Middleton       1        0       1 1.0000000   0.0000000
# 7    Adam Patterson       3        0       3 1.0000000   0.0000000
# 8    Adam Treverrow       4        1       5 0.8000000   0.2000000
# 9   Adrian McDonald       7        0       7 1.0000000   0.0000000
# 10 Adrian Mulholland      1        0       1 1.0000000   0.0000000

### Plot proportion of collaborations by gender of collaborators

# Merge on data about the gender of personA
prop_w_gender <- merge(person_gender_numbers2, marsden_people, by.x = "personA", by.y = "Investigator")

## Plot gender split of collaborators, by gender of personA

# Female collaborators
ggplot(prop_w_gender[Gender == 'M' | Gender == 'F',], aes(x = Prop_Female, fill = Gender)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("M" = "#01579bff", "F" = "#f46524ff")) +
  xlab("Proportion of Collaborators who are Female") +
  ylab("Density") +
  ggtitle("Proportion of Collaborators who are Female, by Gender of Researcher") +
  labs(fill = "Gender of Researcher") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "grey", size=0.2), 
        panel.border = element_rect(fill=NA, color = 'grey'),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
        axis.title = element_text(size = 13), 
        axis.text = element_text(size = 11)) 

# Male collaborators
ggplot(prop_w_gender[Gender == 'M' | Gender == 'F',], aes(x = Prop_Male, fill = Gender)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("M" = "#01579bff", "F" = "#f46524ff")) +
  xlab("Proportion of Collaborators who are Male") +
  ylab("Density") +
  ggtitle("Proportion of Collaborators who are Male, by Gender of Researcher") +
  labs(fill = "Gender of Researcher") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "grey", size=0.2), 
        panel.border = element_rect(fill=NA, color = 'grey'),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
        axis.title = element_text(size = 13), 
        axis.text = element_text(size = 11)) 

### Analysis of actual vs. expected collaborations by field 

# Get information on which panel people submitted to the most
table_panel_ppl <- table(marsden_data$Investigator, marsden_data$Panel)
df_panel_ppl <- as.data.table(table_panel_ppl)

df_panel_ppl <- df_panel_ppl %>% filter(N != 0) 
colnames(df_panel_ppl) <- c('person', 'panel', 'N_proposals')

# Filter down to people with at least 5 collaborations 
person_panel_connections <- df_panel_ppl %>% merge(many_connections_gender, by.x = 'person', by.y = 'Investigator') %>%
  group_by(person) 

setDT(person_panel_connections)
setkey(person_panel_connections, N_proposals)

# Look only at the panel to which they submitted the most proposals
person_top_panel <- person_panel_connections[, head(.SD, 1), by = person] 

# Get gender proportions for each panel
panel_gender_prop <- dcast(panel_gender_df, Panel ~ Gender)
colnames(panel_gender_prop) <- c('Panel', 'F_prop_panel', 'M_prop_panel', 'Unknown_prop_panel')

# Merge together each persons gender split, the panel to which they submitted the 
# most and the gender split of that panel (i.e. actual vs. expected)

person_prop_panel <- merge(person_top_panel[, c('person', 'panel','degrees_connection', 'Gender')], 
                           prop_w_gender[, c('personA', 'Prop_Male', 'Prop_Female', 'Gender')], by.x = c('person', 'Gender'), by.y = c('personA', 'Gender')) %>%
  merge(unique(nodes_full_panel[, c('Panel', 'panel_full')]), by.x = 'panel', by.y = 'Panel') %>%
  merge(panel_gender_prop, by.x = 'panel_full', by.y = 'Panel')

# Calculate the ratio of actual vs. expected collaborations by gender, given 
# the field the person is in
person_prop_panel <- person_prop_panel %>% group_by(person) %>%
  mutate(ratio_M = Prop_Male / M_prop_panel, ratio_F = Prop_Female / F_prop_panel)

### Plot of analysis of homophily
# Female researchers
ggplot(person_prop_panel[Gender == 'M' | Gender == 'F'], aes(x = ratio_F, fill = Gender)) + 
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("M" = "#01579bff", "F" = "#f46524ff", '#27c7bdff')) +
  ggtitle('Collaboration with Female Researchers among Marsden Recipients, by Gender ') +
  xlab(label = 'Ratio of Actual Female Collaborators to Expected Female Collaborators') +
  ylab(label = 'Density') +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "grey", size=0.2), 
        panel.border = element_rect(fill=NA, color = 'grey'),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
        axis.title = element_text(size = 13), 
        axis.text = element_text(size = 11)) +
  scale_x_continuous(breaks = seq(0, 6, 0.5))

# Male researchers 
ggplot(person_prop_panel[Gender == 'M' | Gender == 'F'], aes(x = ratio_M, fill = Gender)) + 
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("M" = "#01579bff", "F" = "#f46524ff")) +
  ggtitle('Collaboration with Male Researchers among Marsden Recipients, by Gender ') +
  xlab(label = 'Ratio of Actual Male Collaborators to Expected Male Collaborators') +
  ylab(label = 'Density') +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "grey", size=0.2), 
        panel.border = element_rect(fill=NA, color = 'grey'),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
        axis.title = element_text(size = 13), 
        axis.text = element_text(size = 11)) +
  scale_x_continuous(breaks = seq(0, 2, 0.5))


### Calcuate median ratios of actual vs. expected by gender
median(person_prop_panel[person_prop_panel$Gender == 'F', ratio_F]) ##1.12987
median(person_prop_panel[person_prop_panel$Gender == 'M', ratio_F]) ##0.8285714

median(person_prop_panel[person_prop_panel$Gender == 'F', ratio_M]) ##0.9611047
median(person_prop_panel[person_prop_panel$Gender == 'M', ratio_M]) ##1.054688

### Number of people from each panel in the analysis of homophily

as.data.frame(table(person_prop_panel$panel_full))
# Var1 Freq
# 1                          Biomedical Sciences   28
# 2  Cellular, Molecular & Physiological Biology   61
# 3                   Earth Sciences & Astronomy  203
# 4               Ecology, Evolution & Behaviour  102
# 5   Economics & Human and Behavioural Sciences   39
# 6     Engineering & Interdisciplinary Sciences   42
# 7                                   Humanities    3
# 8           Mathematics & Information Sciences   29
# 9              Physical Sciences & Engineering   17
# 10           Physics, Chemistry & Biochemistry   17
# 11                             Social Sciences   94



