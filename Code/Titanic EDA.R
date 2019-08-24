require(utils)
require(tidyverse)
require(magrittr)


train_data <- read.csv('train.csv')



#Data Understanding ----

###Cleaner Data
train_clean_data <- train_data %>%
  select(-Name) %>%
  mutate(Survived = if_else(Survived == 1,
                            'Yes',
                            'No'),
         Pclass = case_when(Pclass == 1 ~ 'First',
                            Pclass == 2 ~ 'Second',
                            Pclass == 3 ~ 'Third'),
         Cabin_Start = substr(Cabin, 1, 1))

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
categorical_bar_chart_function <- function(data, variable, remove_NAs = T) {
  data %>%
    group_by_('Survived', variable) %>%
    summarise(count=n()) %>%
    ungroup() %>%
    rename_('Variable' = variable) %>%
    mutate(total = ave(count, Variable, FUN = sum),
           percent = ((count/total*100) %>%
                        round() %>%
                        paste('%', sep = '')),
           Variable = if_else(is.na(Variable),
                              'NA',
                              as.character(Variable))) %>%
    {if(remove_NAs) filter(., .$Variable != 'NA') else .} %>%
    ggplot(aes(x = Variable, y = count, fill = Survived)) +
    geom_bar(stat = 'identity') +
    geom_text(aes(label = percent), position = position_stack(vjust = .5)) +
    scale_fill_manual(values = c('#fa4141', '#37b025')) +
    labs(x = variable) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
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
cluster_analysis_graphic_function <- function(data, cluster_group, remove_NAs) {
  group_name <- cluster_group %>%
    gsub('_.*$', '', .)
  
  ageTF <- grepl('Age', cluster_group)
  
  temp <- data %>%
    rename_('group' = cluster_group) %>%
    {if(remove_NAs) filter(., .$group != 'NA') else .} %>%
    group_by(group, Survived) %>%
    summarise(count = n(),
              max_age = if_else(ageTF,
                                max(Age),
                                0)) %>%
    mutate(total = ave(count, group, FUN = sum),
           percent = ((count/total*100) %>%
                        round() %>%
                        paste('%', sep =''))) %>%
    ggplot(aes(x = reorder(group, -max_age), 
               y = count,
               fill = Survived)) +
    geom_bar(stat='identity') +
    geom_text(aes(label = percent), position = position_stack(vjust = .5)) +
    labs(x = group_name) +
    scale_fill_manual(values = c('#fa4141', '#37b025')) +
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

specific_ticket <- train_clean_data %>%
  filter(Ticket == ((Ticket_count_table) %$%
                      Ticket %>%
                      as.character() %>%
                      .[2]))


###Cabin difference
number_survived_cabin_bar <- train_clean_data %>%
  categorical_bar_chart_function('Cabin_Start')

###Embarked difference
number_survived_embarked_bar <- train_clean_data %>%
  categorical_bar_chart_function('Embarked')

###


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

age_clustering_1 <- age_clustering_table %>%
  cluster_analysis_graphic_function('Age_group1', remove_NAs = T)

age_clustering_2 <- age_clustering_table %>%
  cluster_analysis_graphic_function('Age_group2', F)
  






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




