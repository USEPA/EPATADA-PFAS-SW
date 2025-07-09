#### initials NARS data review 
###Matt Dunn 6/24/25

library(EPATADA)
library(tidyverse)
library(sf)
library(ggplot2)
library(scales)


options(scipen=999) ### no scientific notation


###Loading in data files 

data <- final_nla_2022_pfas_public_release_file_8_19_24_0 ### concentration data 


species_data <- final_nla_2022_pfas_public_release_file_8_19_24_0_Species         ### fish info
species_only <- final_nla_2022_pfas_public_release_file_8_19_24_0_Species_Only    ### species only

data$`EPA Sample ID`<-as.factor(data$`EPA Sample ID`)
species_only$`EPA Sample ID`<-as.factor(species_only$`EPA Sample ID`)
species_data$`EPA Sample ID`<-as.factor(species_data$`EPA Sample ID`)


species_data_unique <- species_data %>%
  distinct(`EPA Sample ID`, .keep_all=TRUE)

species_data_unique

species_data_unique$`EPA Sample ID` <- as.factor(species_data_unique$`EPA Sample ID`)

###combine species data with concentration data, this is now concentration and species for each site

combined_data <- left_join(data, species_data_unique, by="EPA Sample ID")

combined_data

##count and plot species data 

unique_count_species <- length(unique(species_data$`Species - Scientific Name`))
unique_count_species
#21 Species 

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



##### concentration data summary 

data
unique_count <- length(unique(data$Analyte))
unique_count
### 40 different PFAS compounds in dataset 

unique_values <- unique(data$Analyte)
unique_values
###created list of compounds 

freq_table <- table(data$Analyte)
freq_table
###413 samples


## remove NAs in analyte detection

cleaned_data <- data[!is.na(data$Amount), ]
cleaned_data
##2001 measurements left (2001/16520 = 87.8% were non detects, 12.2% were detects)
unique_count_clean <- length(unique(cleaned_data$Analyte))
unique_count_clean
##24 compounds with actual data 

### remove NAs in analyte detection for combined data

cleaned_data_combined <- combined_data[!is.na(combined_data$Amount), ]
cleaned_data_combined
##2001 measurements left (2001/16520 = 87.8% were non detects, 12.2% were detects)
unique_count_clean <- length(unique(cleaned_data$Analyte))
unique_count_clean
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

###stats by group

result <- cleaned_data %>%
  group_by(Analyte) %>%
  summarise(Mean = mean(Amount), Median = median(Amount), st.dev = sd(Amount))
print(result)

###box plot 

cleaned_data$Analyte <- as.factor(cleaned_data$Analyte)
cleaned_data$Amount <- as.numeric(cleaned_data$Amount)

functional_df <- tibble(Analyte = c("PFMBA", "3:3 FTCA", "5:3 FTCA", "7:3 FTCA", "PFBA", "PFPeA", "PFHpA", "PFOA", "PFNA", "PFDA", "PFUnA", "PFDoA", "PFTrDA", "PFTeDA", "PFHxS", "PFHpS", "PFOS", "N-EtFOSE", "N-MeFOSAA", "N-EtFOSAA", "PFOSA", "PFNS", "PFDS", "PFDoS"), 
                        Type = c("PFECA", "FTCA", "FTCA", "FTCA", "PFCA", "PFCA", "PFCA", "PFCA", "PFCA", "PFCA", "PFCA", "PFCA", "PFCA", "PFCA", "PFSA", "PFSA", "PFSA", "FOSE", "FOSAA", "FOSAA", "FASA", "PFSA", "PFSA", "PFSA"))

plots<- left_join(cleaned_data, functional_df, by="Analyte")





p <- ggplot(plots, aes(x=Analyte, y=Amount, color=plots$Type))+ 
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE)+
  scale_x_discrete(limits = c("PFMBA", "3:3 FTCA", "5:3 FTCA", "7:3 FTCA", "PFBA", "PFPeA", "PFHpA", "PFOA", "PFNA", "PFDA", "PFUnA", "PFDoA", "PFTrDA", "PFTeDA", "PFHxS", "PFHpS", "PFOS", "N-EtFOSE", "N-MeFOSAA", "N-EtFOSAA", "PFOSA", "PFNS", "PFDS", "PFDoS"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size =10))+
  ylab("Concentration (ppb)")+
  theme(axis.text.y = element_text(size =10))+
  ggtitle("Detectable Tissue Concentrations in Fish - 2022 (ppb)")+
  scale_y_log10()+
  scale_color_manual('Group', values=c('brown', 'purple','green','orange','red','black','blue'))
  
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

