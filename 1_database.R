## analisi statistica materiali Arqua':

# set working directory:
setwd("D:/documenti_marta/lavoro/CV/online_CV/CV_pictures/azraq")
getwd()

# import data
data_xrf <- read.csv("azraq_xrf_tot.csv", header = TRUE, sep = ",")
data_fabric <- read.csv("azraq_fabric_tot.csv", header = TRUE, sep = ",")
str(data_xrf)
str(data_fabric)

# rename data:
data_xrf <- data_xrf %>% rename(sample_id = ï..)
data_fabric <- data_fabric %>% rename(sample_id = ï..)

# change NA data
which(is.na(data_xrf))
which(is.na(data_fabric))
which(data_xrf == "")

## Combine data_samples with data_xrf and data_xrd
identical(data_fabric$sample_id, data_xrf$sample_id)
data <- left_join(data_fabric, data_xrf,  by = "sample_id")%>% 
  mutate(group = as.factor(group))
length(new_data$sample_id)

data <- subset(data, group != "E" & group != "outliers")

new_data <- data %>%
  mutate(group = replace(as.character(group), group == "A1", "A"),
         group = replace(as.character(group), group == "A2", "A"),         
         group = replace(as.character(group), group == "D1", "D"),
         group = replace(as.character(group), group == "D2", "D")
         )

