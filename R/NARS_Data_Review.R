
####NARS data review & water concentration calculation

#Written by: Matt Dunn (Tetra Tech) & Hannah Ferriby (Tetra Tech)
#Date created: 6/24/25
#Date updated: 7/9/25

library(tidyverse)
library(readxl)
library(ggplot2)
library(scales)


options(scipen=999) ### no scientific notation


####Load Data####
#concentration data 
data <- read_xlsx('Data/NARS/final-nla-2022-pfas-public-release-file-8-19-24_0.xlsx') %>%
  mutate(`EPA Sample ID` = as.factor(`EPA Sample ID`))

#fish info
species_data <- read_xlsx('Data/NARS/final-nla-2022-pfas-public-release-file-8-19-24_0_Species.xlsx') %>%
  mutate(`EPA Sample ID` = as.factor(`EPA Sample ID`)) %>%
  select(`EPA Sample ID`, Family, `Species - Scientific Name`, 
         `Species - Common Name`) %>%
  distinct(`EPA Sample ID`, .keep_all=TRUE)

####Join Datasets####

###combine species data with concentration data, this is now concentration and species for each site
#Convert NARS data to ng/kg
combined_data <- left_join(data, species_data, by="EPA Sample ID") %>%
  mutate(Amount = Amount * 1000, #ng/g to ng/kg
         `Units 1` = 'ng/kg',
         MDL = MDL * 1000,
         QL = QL * 1000) %>%
  select(!`Units 2`)

##count data 
unique_count_species <- length(unique(species_data$`Species - Scientific Name`))


####Analysis####
#BAF = Cbiota / Cwater
#BAF from Burkhard 2021
#Cbiota from NARS
#Solve for Cwater

pfoa_baf <- 2.16 #L/kg
pfoa_baf_std <- 0.85 #L/kg

pfos_baf <- 3.55 #L/kg
pfos_baf_std <- 0.83 #L/kg

#Remove non-detects (nd) from dataset
##2001 measurements left (2001/16520 = 87.8% were non detects, 12.2% were detects)
combined_data_no_nd <- combined_data %>%
  filter(!is.na(Amount)) %>%
  filter(Analyte %in% c('PFOA', "PFOS"))


Cwater_analysis <- combined_data_no_nd %>%
  mutate(Cwater = case_when(Analyte == 'PFOA' ~
                              Amount/(10^pfoa_baf), #ng/L
                            Analyte == 'PFOS' ~
                              Amount/(10^pfos_baf),
                            T ~ NA),
         Cwater_upper = case_when(Analyte == 'PFOA' ~
                                    Amount/(10^(pfoa_baf+pfoa_baf_std)), #ng/L
                                  Analyte == 'PFOS' ~
                                    Amount/(10^pfos_baf+pfos_baf_std),
                                  T ~ NA),
         Cwater_lower = case_when(Analyte == 'PFOA' ~
                                    Amount/(10^pfoa_baf-pfoa_baf_std), #ng/L
                                  Analyte == 'PFOS' ~
                                    Amount/(10^pfos_baf-pfos_baf_std),
                                  T ~ NA))

#boxplot with limits
#pfoa acute - 3100 ug/L
#pfoa chronic - 100 ug/L
#pfos acute - 71 ug/L
#pfos chronic - 0.25 ug/L

#scatterplot/line with std upper and lower


####Plots####

#####Frequency#####
freq_species_df <- as.data.frame(table(species_data$`Species - Scientific Name`))
freq_name_df <- as.data.frame(table(species_data$`Species - Common Name`))

ggplot(freq_species_df, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Frequency of Species Sampled", x = "Values", y = "Frequency") +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size =10))+
  #scale_x_discrete(limits = c("PFMBA", "3:3 FTCA", "5:3 FTCA", "7:3 FTCA", "PFBA", "PFPeA", "PFHpA", "PFOA", "PFNA", "PFDA", "PFUnA", "PFDoA", "PFTrDA", "PFTeDA", "PFHxS", "PFHpS", "PFOS", "N-EtFOSE", "N-MeFOSAA", "N-EtFOSAA", "PFOSA", "PFNS", "PFDS", "PFDoS"))+
  xlab("Species")+
  ylab("Frequency of Sampling")+
  theme(axis.text.y = element_text(size =10))

ggplot(freq_name_df, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Frequency of Species Sampled", x = "Values", y = "Frequency") +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size =10))+
  #scale_x_discrete(limits = c("PFMBA", "3:3 FTCA", "5:3 FTCA", "7:3 FTCA", "PFBA", "PFPeA", "PFHpA", "PFOA", "PFNA", "PFDA", "PFUnA", "PFDoA", "PFTrDA", "PFTeDA", "PFHxS", "PFHpS", "PFOS", "N-EtFOSE", "N-MeFOSAA", "N-EtFOSAA", "PFOSA", "PFNS", "PFDS", "PFDoS"))+
  xlab("Species")+
  ylab("Frequency of Sampling")+
  theme(axis.text.y = element_text(size =10))



#####Concentration#####
unique_count <- length(unique(data$Analyte))
unique_values <- unique(data$Analyte)
freq_table <- table(data$Analyte)


cleaned_data <- data %>%
  filter(!is.na(Amount))
##2001 measurements left (2001/16520 = 87.8% were non detects, 12.2% were detects)


unique_count_clean <- length(unique(cleaned_data$Analyte))
##24 compounds with actual data 

### remove NAs in analyte detection for combined data

cleaned_data_combined <- combined_data %>%
  filter(!is.na(Amount))
##2001 measurements left (2001/16520 = 87.8% were non detects, 12.2% were detects)

unique_count_clean <- length(unique(cleaned_data$Analyte))
##24 compounds with actual data 

unique_values_clean <- unique(cleaned_data$Analyte)
unique_values_clean
### created list of compounds
##PFCA: PFBA (3), PFPeA (1), PFHpA (5), PFOA (4), PFNA (95), PFDA (330), PFUnA (349), PFDoA (293), PFTrDA (205), PFTeDA (164)
##PFSA: PFHxS (18), PFHpS (14), PFOS (357), PFNS (7), PFDS (92), PFDoS (1)
##FTCA: 3:3 FTCA (1), 5:3 FTCA (1), 7:3 FTCA (7)
##FASA: PFOSA (10)
##FOSE: N-EtFOSE (18), N-MeFOSAA (15), N-EtFOSAA (9)
##Other: PFMBA (2)

freq_table_cleaned <- table(cleaned_data$Analyte)
freq_table_cleaned

barplot(freq_table_cleaned, main = "Frequency of Unique Values", col = "skyblue", xlab = "Values", ylab = "Frequency")

freq_df <- as.data.frame(table(cleaned_data$Analyte))

ggplot(freq_df, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Compounds Detected Above Detection Limits In Fish Tissue", x = "Values", y = "Frequency") +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size =10))+
  scale_x_discrete(limits = c("PFMBA", "3:3 FTCA", "5:3 FTCA", "7:3 FTCA", "PFBA", "PFPeA", "PFHpA", "PFOA", "PFNA", "PFDA", "PFUnA", "PFDoA", "PFTrDA", "PFTeDA", "PFHxS", "PFHpS", "PFOS", "N-EtFOSE", "N-MeFOSAA", "N-EtFOSAA", "PFOSA", "PFNS", "PFDS", "PFDoS"))+
  xlab("Analyte")+
  ylab("Frequency of Detection")+
  theme(axis.text.y = element_text(size =10))

  
unique_sites_clean <- unique(cleaned_data$`Site ID`)
unique_sites_clean
###383 sample IDs with atleast one data point above detection limits

unique_states_clean <- unique(cleaned_data$State)
unique_states_clean
###47 states (missing only Hawaii, Alaska, North Dakota)

freq_table_cleaned_states <- table(cleaned_data$State)
freq_table_cleaned_states
freq_state_df <- as.data.frame(table(cleaned_data$State))

ggplot(freq_state_df, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Detections per State", x = "Values", y = "Frequency") +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size =10))+
  xlab("State")+
  ylab("Frequency of Detection")+
  theme(axis.text.y = element_text(size =10))

#####Stats by group#####

result <- cleaned_data %>%
  group_by(Analyte) %>%
  summarise(Mean = mean(Amount), Median = median(Amount), st.dev = sd(Amount))
print(result)

#####Box Plot#####

cleaned_data$Analyte <- as.factor(cleaned_data$Analyte)
cleaned_data$Amount <- as.numeric(cleaned_data$Amount)

functional_df <- tibble(Analyte = c("PFMBA", "3:3 FTCA", "5:3 FTCA", "7:3 FTCA", "PFBA", "PFPeA", "PFHpA", "PFOA", "PFNA", "PFDA", "PFUnA", "PFDoA", "PFTrDA", "PFTeDA", "PFHxS", "PFHpS", "PFOS", "N-EtFOSE", "N-MeFOSAA", "N-EtFOSAA", "PFOSA", "PFNS", "PFDS", "PFDoS"), 
                        Type = c("PFECA", "FTCA", "FTCA", "FTCA", "PFCA", "PFCA", "PFCA", "PFCA", "PFCA", "PFCA", "PFCA", "PFCA", "PFCA", "PFCA", "PFSA", "PFSA", "PFSA", "FOSE", "FOSAA", "FOSAA", "FASA", "PFSA", "PFSA", "PFSA"))

plots<- left_join(cleaned_data, functional_df, by="Analyte")





p <- ggplot(plots, aes(x=Analyte, y=Amount, color=plots$Type, fill = plots$Type))+ 
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE)+
  scale_x_discrete(limits = c("PFMBA", "3:3 FTCA", "5:3 FTCA", "7:3 FTCA", "PFBA", "PFPeA", "PFHpA", "PFOA", "PFNA", "PFDA", "PFUnA", "PFDoA", "PFTrDA", "PFTeDA", "PFHxS", "PFHpS", "PFOS", "N-EtFOSE", "N-MeFOSAA", "N-EtFOSAA", "PFOSA", "PFNS", "PFDS", "PFDoS"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size =10))+
  ylab("Concentration (ppb)")+
  theme(axis.text.y = element_text(size =10))+
  ggtitle("Detectable Tissue Concentrations in Fish - 2022 (ppb)")+
  scale_y_log10()+
  scale_color_manual('Group', values=c('#A52A2A', '#720c99','darkgreen','#a76004','darkred','black','blue')) +
  scale_fill_manual('Group', values = c('#A0522D', 'purple','green','orange','red','gray50','#5b8cf0'))
  
p

cleaned_data

#### plots for just PFOA and PFOS 

plots_PFOA_PFOS <- plots%>%
  filter(Analyte %in% c('PFOA', 'PFOS'))

pp <- ggplot(plots_PFOA_PFOS, aes(x=Analyte, y=Amount, color=Type))+ 
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE)+
  #scale_x_discrete(limits = c("PFMBA", "3:3 FTCA", "5:3 FTCA", "7:3 FTCA", "PFBA", "PFPeA", "PFHpA", "PFOA", "PFNA", "PFDA", "PFUnA", "PFDoA", "PFTrDA", "PFTeDA", "PFHxS", "PFHpS", "PFOS", "N-EtFOSE", "N-MeFOSAA", "N-EtFOSAA", "PFOSA", "PFNS", "PFDS", "PFDoS"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size =14))+
  ylab("Concentration (ppb)")+
  theme(axis.text.y = element_text(size =14))+
  ggtitle("Detectable Tissue Concentrations in Fish - 2022")+
  scale_y_log10()+
  scale_color_manual('Group', values=c('red','blue'))

pp

