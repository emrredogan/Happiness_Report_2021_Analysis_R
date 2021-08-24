library(dplyr)
library(ggplot2)
library(corrplot)
library(ggthemes)

#Upload the data file
happines_data <- read.csv("C:/Users/yemre/OneDrive/Masaüstü/DS_Projects/Hapiness_Countries_Project/world-happiness-report-2021.csv")

#Check NA values in the dataset
sum(is.na(happines_data))


glimpse(happines_data) 

#Rename all columns. Using columns' name will be easier 
colnames(happines_data) <- c("Country", "Region", "Ladder_Score", "Standard_Error_of_Ladder Score",
                      "Upperwhisker","Lowerwhisker","Logged_GDP_per_Capita",
                       "Social_Support", "Healthy_Life_Expectancy", "Freedom_Choice",
                       "Generosity", "Perceptions_of_Corruption", "Ladder_Score_in_Dystopia",
                       "Explained_by_Logged_GDP_per_Capita","Explained_by_Social_Support",
                      "Explained_by_Health_Life_Expectancy",
                       "Explained_by_Freedom_Choice", "Explained_by_Generosity",
                      "Explained_by_Perception_of_Corruption","DystopiaResidual")


summary(happines_data) #Describe the dataset 


#Draw graph to analyze the relationship between social support and perception of corruption
ggplot(happines_data,
       aes(x= Social_Support, 
           y = Perceptions_of_Corruption)) +
  geom_point(aes(color = Region),
             size = 2) +
  geom_smooth(method = "lm") +
  labs(x = "Social Support",
       y = "Perceptipons of Corruption",
       title = "The Relationship Between Social Support & Perception of Corruption")+
  theme_minimal()+
  theme(text = element_text(size=14))


#As we expect, there is a negative relationship between these two variables. 

#Check the correlation between Social Support and Perception of corruption
cor(happines_data$Social_Support, happines_data$Perceptions_of_Corruption) 


#Happiest and unhappiest countries 

hp <- happines_data %>% 
  filter(Ladder_Score > 7.3 | Ladder_Score < 3.5) 

ggplot(hp,
       aes(x = Country, y = Ladder_Score, fill = Country))+
  geom_col(size = 2)+
  theme_fivethirtyeight()+
  labs(x = "Countries", 
       y = "Ladder Scores",
       title = "Happiest and Unhappiest Countries")

#The most generous and ungenerous countries
hp2 <- happines_data %>% 
  filter(Generosity > 0.42 | Generosity < -0.24) 

ggplot(hp2,
       aes(x = Country, y = Generosity, fill = Country))+
  geom_col(size = 2)+
  theme_fivethirtyeight()+
  labs(x = "Countries", 
       y = "Generosity",
       title = "The most Generous and Ungenerous Countries")


#Relationship between Explained Logged GDP per Capita and Ladder Score
cor(happines_data$Explained_by_Logged_GDP_per_Capita, happines_data$Ladder_Score) 

ggplot(happines_data,
       aes(x = Explained_by_Logged_GDP_per_Capita,
           y = Ladder_Score))+
  geom_line(color = "#80CDC1",alpha = 0.8, size = 1.2)+
  theme(text= element_text(size = 12))+
  labs(x = "Explained By Logged GDP per Capita",
       y = " Ladder Scores of Countries",
       title = "The Relationship Between Ladder Scores & Explained By Logged GDP per Capita")
  
  


#Correlations among some variables

data_corr <-happines_data %>% 
  select(Ladder_Score, Logged_GDP_per_Capita, Healthy_Life_Expectancy, Freedom_Choice,
         Perceptions_of_Corruption, Social_Support, Explained_by_Logged_GDP_per_Capita,
         Explained_by_Social_Support, Explained_by_Health_Life_Expectancy, 
         Explained_by_Freedom_Choice, Explained_by_Perception_of_Corruption)

final_data_corr <- cor(data_corr, use = "complete", method = "pearson")  
corrplot(final_data_corr)  












