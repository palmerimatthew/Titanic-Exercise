require(utils)
require(tidyverse)
require(magrittr)


train_data <- read.csv('train.csv')



#Data Understanding ----

###Cleaner Data
train_clean_data <- train_data %>%
  select(-Name)
mutate(Survived = if_else(Survived == 1,
                          'Yes',
                          'No'),
       Pclass = case_when(Pclass == 1 ~ 'First',
                          Pclass == 2 ~ 'Second',
                          Pclass == 3 ~ 'Third'))

number_survived_simple_bar <- train_clean_data %>%
  group_by(Survived) %>%
  summarise(count=n()) %>%
  ggplot(aes(x = Survived, y = count, fill = Survived)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c('#fa4141', '#37b025')) +
  guides(fill = guide_legend(reverse = T)) +
  coord_flip()

##Helper Functions

###Bar Chart for Categorical Variables
categorical_bar_chart_function <- function(data, variable) {
  data %>%
    group_by_('Survived', variable) %>%
    summarise(count=n()) %>%
    ungroup() %>%
    rename_('Variable' = variable) %>%
    mutate(total = ave(count, Variable, FUN = sum),
           percent = ((count/total*100) %>%
                        round() %>%
                        paste('%', sep = ''))) %>%
    ggplot(aes(x = Variable, y = count, fill = Survived)) +
    geom_bar(stat = 'identity') +
    geom_text(aes(label = percent), position = position_stack(vjust = .5)) +
    scale_fill_manual(values = c('#fa4141', '#37b025')) +
    labs(x = variable) +
    theme(axis.title.x = element_blank()) +
    guides(fill = guide_legend(reverse = T)) +
    coord_flip()
}

###Boxplot for Quantitative Variables
quantitative_boxplot_function <- function(data, variable) {
  data %>%
    rename_('Variable' = variable) %>%
    ggplot(aes(x = Survived, y = Variable, fill = Survived)) +
    geom_boxplot() +
    scale_fill_manual(values = c('#fa4141', '#37b025')) +
    labs(y = variable) +
    theme(legend.position = 'none')
}

###Transforms a categorical variable into dummy variables of each unique entry
spread_column <- function(data, column) {
  desired_column <- data %>%
    select_(column) %>%
    .[,1]
  
  unique_entries <- desired_column %>%
    unique()
  
  temp <- desired_column %>%
    lapply(function(x) table(factor(x, levels=unique_entries))) %>%
    do.call(rbind, .) %>%
    data.frame(stringsAsFactors = F)
  
  data %>%
    cbind(temp)
}

###Cluster Analysis function
cluster_analysis_graphic_function <- function(data, cluster_group) {
  temp <- data %>%
    group_by_(cluster_group, 'Survived') %>%
    summarise(count = n(),
              if(grepl('Age', cluster_group)) 
                {max_age = max(Age)}) %>%
    rename_('group' = cluster_group) %>%
    mutate(total = ave(count, group, FUN = sum),
           percent = count/total*100) %>%
    filter(Survived == 'Yes') %>%
    ggplot(aes(x = group, 
               y = percent)) +
    geom_bar(stat='identity') +
    coord_flip()
}


##Categorical Variables

###Gender difference
number_survived_gender_bar <- train_clean_data %>%
  categorical_bar_chart_function('Sex')

###Class difference
number_survived_class_bar <- train_clean_data %>%
  categorical_bar_chart_function('Pclass')

###Ticket difference
Ticket_count_table <- train_clean_data %>%
  group_by(Ticket) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(-count)


##Quantitative Variables

###Fare Difference
fare_difference_boxplot <- train_clean_data %>%
  quantitative_boxplot_function('Fare')

###Age Difference
age_difference_boxplot <- train_clean_data %>%
  quantitative_boxplot_function('Age')

age_by_year_histogram <- train_clean_data %>%
  mutate(Age = floor(Age)) %>%
  group_by(Age, Survived) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  .[order(.$Age),] %>%
  mutate(age = if_else(is.na(Age),
                       'NA',
                       as.character(Age)),
         total = ave(count, age, FUN = sum),
         percent = count/total*100) %>%
  filter(Survived == 'Yes') %>%
  ggplot(aes(x = reorder(age, Age), y = percent)) +
  geom_bar(stat = 'identity')

age_clustering_table <- train_clean_data %>%
  mutate(Age_group1 = case_when(Age < 5 ~ '0-5',
                                Age < 10 ~ '5-10',
                                Age < 15 ~ '10-15',
                                Age < 20 ~ '15-20',
                                Age < 25 ~ '20-25',
                                Age < 30 ~ '25-30',
                                Age < 35 ~ '30-35',
                                Age < 40 ~ '35-40',
                                Age < 45 ~ '40-45',
                                Age < 50 ~ '45-50',
                                Age < 55 ~ '50-55',
                                Age < 60 ~ '55-60',
                                Age < 65 ~ '60-65',
                                Age < 70 ~ '65-70',
                                Age < 75 ~ '70-75',
                                Age <= 80 ~ '75-80',
                                TRUE ~ 'NA'),
         Age_group2 = case_when(Age < 15 ~ '0-15',
                                is.na(Age) ~ 'NA',
                                TRUE ~ '15-80'))



age_clustering_analysis <- train_clean_data %>%
  mutate(age_group1 = case_when(Age < 5 ~ '0-5',
                               Age < 10 ~ '5-10',
                               Age < 15 ~ '10-15',
                               Age < 20 ~ '15-20',
                               Age < 25 ~ '20-25',
                               Age < 30 ~ '25-30',
                               Age < 35 ~ '30-35',
                               Age < 40 ~ '35-40',
                               Age < 45 ~ '40-45',
                               Age < 50 ~ '45-50',
                               Age < 55 ~ '50-55',
                               Age < 60 ~ '55-60',
                               Age < 65 ~ '60-65',
                               Age < 70 ~ '65-70',
                               Age < 75 ~ '70-75',
                               Age <= 80 ~ '75-80',
                               TRUE ~ 'NA'),
         age_group2 = case_when(Age < 15 ~ '0-15',
                                is.na(Age) ~ 'NA',
                                TRUE ~ '15-80')) %>%
  group_by(age_group1, Survived) %>%
  summarise(count = n()) %>%
  mutate(total = ave(count, age_group1, FUN = sum),
         percent = count/total*100) %>%
  filter(Survived == 'Yes') %>%
  ggplot(aes(x = age_group1, y = percent)) +
  geom_bar(stat='identity') +
  coord_flip()




#Interaction term analysis ----

###Gender/Class combo difference
number_survived_gender_class_bar <- train_clean_data %>%
  mutate(Sex_Pclass = paste(Sex, Pclass)) %>%
  group_by(Survived, Sex_Pclass) %>%
  summarise(count=n()) %>%
  ungroup() %>%
  mutate(total = ave(count, Sex_Pclass, FUN = sum),
         percent = ((count/total*100) %>%
                      round() %>%
                      paste('%', sep = ''))) %>%
  ggplot(aes(x = Sex_Pclass, y = count, fill = Survived)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = percent), position = position_stack(vjust = .5)) +
  scale_fill_manual(values = c('#fa4141', '#37b025')) +
  guides(fill = guide_legend(reverse = T)) +
  coord_flip()




