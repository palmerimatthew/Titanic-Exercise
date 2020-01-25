require(here)
require(tidyverse)
require(data.table)


train_data <- read.csv(here('Data', 'train.csv'))
test_data <- read.csv(here('Data', 'test.csv')) %>%
  mutate(Survived = NA_integer_) %>%
  select(PassengerId, Survived, Pclass:Embarked)

# want to have the same transformations for both datasets
full_data <- rbind(train_data, test_data)

#data_manipulation
for_model <- full_data %>%
  #categorical class variable
  mutate(Class_Categorical = case_when(Pclass == 1 ~ 'First',
                                       Pclass == 2 ~ 'Second',
                                       Pclass == 3 ~ 'Third')) %>%
  select(PassengerId:Pclass, Class_Categorical, Name:Embarked) %>%
  #title from Name
  separate(Name, into = c('Last_Name', 'First_Name'), sep = ',') %>%
  separate(First_Name, into = c('Raw_Title', 'First_Name'), sep = '\\.', extra = 'merge') %>%
  mutate(Name = trimws(paste(First_Name, Last_Name)),
         Raw_Title = trimws(Raw_Title),
         Title = case_when(Raw_Title == 'Mr' ~ 'Mr',
                           Raw_Title == 'Sir' ~ 'Mr',
                           Raw_Title == 'Lady' ~ 'Ms',
                           Raw_Title == 'Miss' ~ 'Ms',
                           Raw_Title == 'Mlle' ~ 'Ms',
                           Raw_Title == 'Mme' ~ 'Ms',
                           Raw_Title == 'Mrs' ~ 'Mrs',
                           Raw_Title == 'Ms' ~ 'Ms',
                           Raw_Title == 'Master' ~ 'Master',
                           TRUE ~ 'Other')) %>%
  select(PassengerId:Class_Categorical, Title, Raw_Title, Name, Sex:Embarked) %>%
  #Total family members
  mutate(Family_Size = Parch + SibSp + 1,
         Family_Size_Grouping = case_when(Family_Size == 1 ~ 'Solo',
                                          Family_Size <= 4 ~ 'Small',
                                          Family_Size <= 7 ~ 'Medium',
                                          TRUE ~ 'Large')) %>%
  select(PassengerId:Parch, Family_Size, Family_Size_Grouping, Ticket:Embarked) %>%
  #Ticket prefix and suffix
  separate(Ticket, into = c('Ticket_Prefix', 'Ticket_Suffix'), sep = ' ', extra = 'merge', fill = 'left') %>%
  separate(Ticket_Suffix, into = c('Ticket_temp', 'Ticket_Suffix'), sep = ' ', fill = 'left') %>%
  mutate(Ticket_Prefix = case_when(is.na(Ticket_Prefix) ~ NA_character_,
                                   !is.na(Ticket_Prefix) & is.na(Ticket_temp) ~ Ticket_Prefix,
                                   !is.na(Ticket_Prefix) & !is.na(Ticket_temp) ~ paste0(Ticket_Prefix, Ticket_temp))) %>%
  select(-Ticket_temp) %>%
  #Cabin and Room
  mutate(Cabin = as.character(Cabin),
         Cabin = if_else(Cabin == '', 'Z', Cabin),
         Room = Cabin,
         Cabin = substr(Cabin, 1, 1)) %>%
  select(PassengerId:Cabin, Room, Embarked)


temp <- filter(for_model, Fare != 0)
test <- lm(log(Fare) ~ Class_Categorical + Family_Size, data = temp)
for_model <- mutate(for_model, Fare = if_else(!is.na(Fare), Fare,
                                              exp(predict(test, for_model))))

temp <- filter(for_model, !is.na(Age) & !is.na(Fare))
test <- lm(Age ~ Class_Categorical + SibSp + Parch + Fare + Cabin + Family_Size_Grouping, data = temp)
for_model <- mutate(for_model, Age = if_else(!is.na(Age), Age,
                                             predict(test, for_model)))


#age_grouping
for_model <- for_model %>%
  mutate(Age_Grouping = case_when(Age <= 15 ~ '0-15',
                                  Age <= 30 ~ '15-30',
                                  Age <= 60 ~ '30-60',
                                  TRUE ~ '60-')) %>%
  select(PassengerId:Age, Age_Grouping, SibSp:Embarked)


fwrite(for_model, here('Data', 'data_for_model.csv'))


#For determining groupings for family size
for_model %>%
  filter(!is.na(Survived)) %>%
  group_by(Survived, Family_Size) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(total = ave(count, Family_Size, FUN=sum),
         proportion = count/total,
         Survived = if_else(Survived == 1, 'Yes', 'No')) %>%
  select(-count) %>%
  pivot_wider(names_from = 'Survived', values_from = 'proportion') %>%
  replace(., is.na(.), 0) %>%
  mutate(SE_proportion = 1.96*sqrt((Yes*No)/total)) %>%
  ggplot(aes(x = Family_Size, y = Yes)) + geom_line() + theme_classic() +
    geom_errorbar(aes(ymin = Yes-SE_proportion, ymax = Yes+SE_proportion))

#For determing groupings for age
for_model %>%
  filter(!is.na(Survived)) %>%
  mutate(age_grouping = case_when(Age <= 5 ~ 2.5,
                                  Age <= 10 ~ 7.5,
                                  Age <= 15 ~ 12.5,
                                  Age <= 20 ~ 17.5,
                                  Age <= 25 ~ 22.5,
                                  Age <= 30 ~ 27.5,
                                  Age <= 35 ~ 32.5,
                                  Age <= 40 ~ 37.5,
                                  Age <= 45 ~ 42.5,
                                  Age <= 50 ~ 47.5,
                                  Age <= 55 ~ 52.5,
                                  Age <= 60 ~ 57.5,
                                  Age <= 65 ~ 62.5,
                                  TRUE ~ 67.5)) %>%
  group_by(Survived, age_grouping) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(total = ave(count, age_grouping, FUN=sum),
         proportion = count/total,
         Survived = if_else(Survived == 1, 'Yes', 'No')) %>%
  select(-count) %>%
  pivot_wider(names_from = 'Survived', values_from = 'proportion') %>%
  replace(., is.na(.), 0) %>%
  mutate(SE_proportion = 1.96*sqrt((Yes*No)/total)) %>%
  ggplot(aes(x = age_grouping, y = Yes)) + geom_line() + theme_classic() +
  geom_errorbar(aes(ymin = Yes-SE_proportion, ymax = Yes+SE_proportion))
