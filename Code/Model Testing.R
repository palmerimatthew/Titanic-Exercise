require(here)
require(magrittr)
require(tidyverse)
require(InformationValue)
require(car)


# Datasets and functions----
full_data <- read.csv(here('Data', 'train.csv')) %>%
  select(-Name) %>%
  mutate(Pclass = case_when(Pclass == 1 ~ 'First',
                            Pclass == 2 ~ 'Second',
                            Pclass == 3 ~ 'Third'),
         Cabin_Start = substr(Cabin, 1, 1),
         Has_Children = if_else(Age >= 21,
                                Parch > 0,
                                F),
         Has_Children = if_else(is.na(Has_Children),
                                F,
                                Has_Children),
         Age_Group = case_when(Age < 15 ~ '0-15',
                               is.na(Age) ~ 'NA',
                               TRUE ~ '15-80'),
         Sex = as.character(Sex),
         Ticket = as.character(Ticket),
         Cabin = as.character(Cabin),
         Embarked = as.character(Embarked),
         Sex_Pclass = paste(Sex, Pclass),
         Class_Children = paste(Pclass, Has_Children),
         Sex_Children = paste(Sex, Has_Children))

## Gotten from the 'smbinning' package (didn't load properly, copied from github here: https://github.com/cran/smbinning/blob/master/R/smbinning.R)

smbinning = function(df,y,x,p=0.05){
  # Check data frame and formats
  if (!is.data.frame(df)){ # Check if data.frame
    return("Data not a data.frame")
  } else if (is.numeric(y) | is.numeric(x)){ # Check if target variable is numeric
    return("Column name not string")
  } else if (grepl("[.]",y) | grepl("[.]",x)){ # Check if there is a dot
    return("Column name with a dot [.]")
  } else 
    i=which(names(df)==y) # Find Column for dependant
  j=which(names(df)==x) # Find Column for independant
  if (!is.numeric(df[,i])){ 
    return("Target (y) not found or it is not numeric")
  } else if (max(df[,i],na.rm=T)!=1){
    return("Maximum not 1")
  } else if (tolower(y)=="default"){
    return("Field name 'default' not allowed")
  } else if (fn$sqldf("select count(*) from df where cast($x as text)='Inf' or cast($x as text)='-Inf'")>0){
    return("Characteristic (x) with an 'Inf' value (Divided by Zero). Replace by NA")  
  } else if (min(df[,i],na.rm=T)!=0){
    return("Minimum not 0")
  } else if (p<=0 | p>0.5){
    return("p must be greater than 0 and lower than 0.5 (50%)")
  } else if (!is.numeric(df[,j])){
    return("Characteristic (x) not found or it is not a number")
  } else if (length(unique(df[,j]))<5){
    return("Uniques values < 5")  
  } else { 
    ctree=ctree(formula(paste(y,"~",x)),
                data=df, 
                na.action=na.exclude,
                control=ctree_control(minbucket=ceiling(round(p*nrow(df)))))
    bins=width(ctree)
    if (bins<2){return("No significant splits")}
    # Append cutpoinstop()ts in a table (Automated)
    cutvct=data.frame(matrix(ncol=0,nrow=0)) # Shell
    n=length(ctree) # Number of nodes
    for (i in 1:n) {
      cutvct=rbind(cutvct,ctree[i]$node$split$breaks)
    }
    cutvct=cutvct[order(cutvct[,1]),] # Sort / converts to a ordered vector (asc)
    cutvct=ifelse(cutvct<0,trunc(10000*cutvct)/10000,ceiling(10000*cutvct)/10000) # Round to 4 dec. to avoid borderline cases
    # Build Information Value Table #############################################
    # Counts per not missing cutpoint
    ivt=data.frame(matrix(ncol=0,nrow=0)) # Empty table
    n=length(cutvct) # Number of cutpoits
    for (i in 1:n) {
      cutpoint=cutvct[i]
      ivt=rbind(ivt,
                fn$sqldf(
                  "select '<= $cutpoint' as Cutpoint,
                  NULL as CntRec,
                  NULL as CntGood,
                  NULL as CntBad,
                  sum(case when $x <= $cutpoint and $y in (1,0) then 1 else 0 end) as CntCumRec,
                  sum(case when $x <= $cutpoint and $y=1 then 1 else 0 end) as CntCumGood,
                  sum(case when $x <= $cutpoint and $y=0 then 1 else 0 end) as CntCumBad,
                  NULL as PctRec,
                  NULL as GoodRate,
                  NULL as BadRate,
                  NULL as Odds,
                  NULL as LnOdds,
                  NULL as WoE,
                  NULL as IV
                  from df where $x is not NULL and $y is not NULL")
      )
    }
    cutpoint=max(df[,j],na.rm=T) # Calculte Max without Missing
    cutpoint=ifelse(cutpoint<0,trunc(10000*cutpoint)/10000,ceiling(10000*cutpoint)/10000) # Round to 4 dec. to avoid borderline cases
    maxcutpoint=max(cutvct) # Calculte Max cut point
    mincutpoint=min(df[,j],na.rm=T) # Calculte Min without Missing for later usage
    mincutpoint=ifelse(mincutpoint<0,trunc(10000*mincutpoint)/10000,ceiling(10000*mincutpoint)/10000) # Round to 4 dec. to avoid borderline cases 
    ivt=rbind(ivt,
              fn$sqldf(
                "select '> $maxcutpoint' as Cutpoint,
                NULL as CntRec,
                NULL as CntGood,
                NULL as CntBad,
                sum(case when $x <= $cutpoint and $y in (1,0) then 1 else 0 end) as CntCumRec,
                sum(case when $x <= $cutpoint and $y=1 then 1 else 0 end) as CntCumGood,
                sum(case when $x <= $cutpoint and $y=0 then 1 else 0 end) as CntCumBad,
                NULL as PctRec,
                NULL as GoodRate,
                NULL as BadRate,
                NULL as Odds,
                NULL as LnOdds,
                NULL as WoE,
                NULL as IV
                from df where $x is not NULL and $y is not NULL")
    )
    # Missing Data
    x.na=fn$sqldf("select count(*) from df where $x is null")  
    y.na=fn$sqldf("select count(*) from df where $y is null")
    if(x.na>0){
      ivt=rbind(ivt,
                fn$sqldf(
                  "select 'Missing' as Cutpoint,
                  sum(case when $x is NULL and $y in (1,0) then 1 else 0 end) as CntRec,
                  sum(case when $x is NULL and $y=1 then 1 else 0 end) as CntGood,
                  sum(case when $x is NULL and $y=0 then 1 else 0 end) as CntBad,
                  NULL as CntCumRec,
                  NULL as CntCumGood,
                  NULL as CntCumBad,
                  NULL as PctRec,
                  NULL as GoodRate,
                  NULL as BadRate,
                  NULL as Odds,
                  NULL as LnOdds,
                  NULL as WoE,
                  NULL as IV
                  from df where $y is not NULL")
      )
    } 
    
    else {
      ivt=rbind(ivt,
                c("Missing",0,0,0,NA,NA,NA,NA,NA,NA,NA,NA,NA))}
    
    # Total
    ivt=rbind(ivt,
              fn$sqldf(
                "select 'Total' as Cutpoint,
                count(*) as CntRec,
                sum(case when $y=1 then 1 else 0 end) as CntGood,
                sum(case when $y=0 then 1 else 0 end) as CntBad,
                NULL as CntCumRec,
                NULL as CntCumGood,
                NULL as CntCumBad,
                NULL as PctRec,
                NULL as GoodRate,
                NULL as BadRate,
                NULL as Odds,
                NULL as LnOdds,
                NULL as WoE,
                NULL as IV
                from df where $y is not NULL")
    )
    
    # Covert to table numeric
    options(warn=-1)
    ncol=ncol(ivt)
    for (i in 2:ncol){
      ivt[,i]=as.numeric(ivt[,i])
    }
    options(warn=0)
    
    # Complete Table 
    ivt[1,2]=ivt[1,5] # Nbr Records
    ivt[1,3]=ivt[1,6] # Nbr Goods
    ivt[1,4]=ivt[1,7] # Nbr Bads
    
    # From 2nd row
    n=nrow(ivt)-2
    for (i in 2:n){ivt[i,2]=ivt[i,5]-ivt[i-1,5]
    ivt[i,3]=ivt[i,6]-ivt[i-1,6]
    ivt[i,4]=ivt[i,7]-ivt[i-1,7]}
    
    ivt[2,2]=ivt[2,5]-ivt[1,5]
    ivt[2,3]=ivt[2,6]-ivt[1,6]
    ivt[2,4]=ivt[2,7]-ivt[1,7]
    
    # Missing row.  Update: Added "if" statement
    ivt[i+1,5]=ivt[i,5]+ivt[i+1,2]
    ivt[i+1,6]=ivt[i,6]+ivt[i+1,3]
    ivt[i+1,7]=ivt[i,7]+ivt[i+1,4]
    
    # Calculating metrics
    options(scipen=999) # Remove Scientific Notation
    ivt[,8]=round(ivt[,2]/ivt[i+2,2],4) # PctRec
    ivt[,9]=round(ivt[,3]/ivt[,2],4) # GoodRate
    ivt[,10]=round(ivt[,4]/ivt[,2],4) # BadRate
    ivt[,11]=round(ivt[,3]/ivt[,4],4) # Odds
    ivt[,12]=round(log(ivt[,3]/ivt[,4]),4) # LnOdds
    G=ivt[i+2,3]
    B=ivt[i+2,4]
    LnGB=log(G/B) # IV Part 1
    ivt[,13]=round(log(ivt[,3]/ivt[,4])-LnGB,4) # WoE
    ivt[,14]=round(ivt[,13]*(ivt[,3]/G-ivt[,4]/B),4) # Mg IV
    # ivt[i+2,14]=round(sum(ivt[,13]*(ivt[,3]/G-ivt[,4]/B),na.rm=T),4) -- Old Calculation
    # Calculates Information Value even with undefined numbers
    ivt[i+2,14]=0.0000
    for (k in 1:(nrow(ivt)-1))
    {
      if(is.finite(ivt[k,14])) {mgiv=ivt[k,14]} else {mgiv=0.0000}
      ivt[i+2,14]=ivt[i+2,14]+mgiv
    }
    iv=ivt[i+2,14]
    # End Inf. Value Table ###################################################### 
  }
  bands=append(mincutpoint,cutvct)
  bands=append(bands,cutpoint)
  list(ivtable=ivt,iv=iv,ctree=ctree,bands=bands,x=x,col_id=j,cuts=cutvct)
}

smbinning.factor = function(df,y,x,maxcat=10){
  # Check data frame and formats
  if (!is.data.frame(df)){ # Check if data.frame
    return("Data not a data.frame")
  } else if (is.numeric(y) | is.numeric(x)){ # Check if target vable is numeric
    return("Column name not string")
  } else if (grepl("[.]",y) | grepl("[.]",x)){ # Check if there is a dot
    return("Column name with a dot [.]")
  } else 
    i=which(names(df)==y) # Find Column for dependant
  j=which(names(df)==x) # Find Column for independant
  if (!is.numeric(df[,i])){ 
    return("Target (y) not found or it is not numeric")
  } else if (max(df[,i],na.rm=T)!=1){
    return("Maximum not 1")
  } else if (any(grepl(",", df[,j]))){
    return("Values contain comma")
  } else if (tolower(y)=="default"){
    return("Field name 'default' not allowed")
  } else if (fn$sqldf("select count(*) from df where cast($x as text)='Inf' or cast($x as text)='-Inf'")>0){
    return("Characteristic (x) with an 'Inf' value (Divided by Zero). Replace by NA")  
  } else if (min(df[,i],na.rm=T)!=0){
    return("Minimum not 0")
  } else if (!is.factor(df[,j])){
    return("Characteristic (x) not found or it is not a factor")
  } else if (length(unique(df[,j]))<=1){
    return("Characteristic (x) requires at leats 2 uniques categories")  
  } else if (length(unique(df[,j]))>maxcat){
    return("Too many categories")    
  } else { 
    # Append cutpoints in a table (Automated)
    # cutvct=data.frame(matrix(ncol=0,nrow=0)) # Shell
    cutvct=c()
    cuts=fn$sqldf("select distinct $x from df where $x is not NULL")
    cuts=as.vector(as.matrix(cuts))
    n=length(cuts) # Number of cutpoints
    if (n<1){return("No Bins")} # At least 1 cutpoint
    for (i in 1:n) {
      cutvct=rbind(cutvct,cuts[i])
    }
    cutvct=cutvct[order(cutvct[,1]),] # Sort / converts to a ordered vector (asc)
    # Build Information Value Table #############################################
    # Counts per not missing cutpoint
    ivt=data.frame(matrix(ncol=0,nrow=0)) # Shell
    n=length(cutvct) # Number of cutpoits
    for (i in 1:n) {
      cutpoint=cutvct[i]
      ivt=rbind(ivt,
                fn$sqldf(
                  "select '= ''$cutpoint''' as Cutpoint,
                  sum(case when $x = '$cutpoint' and $y in (1,0) then 1 else 0 end) as CntRec,
                  sum(case when $x = '$cutpoint' and $y=1 then 1 else 0 end) as CntGood,
                  sum(case when $x = '$cutpoint' and $y=0 then 1 else 0 end) as CntBad,
                  NULL as CntCumRec,
                  NULL as CntCumGood,
                  NULL as CntCumBad,
                  NULL as PctRec,
                  NULL as GoodRate,
                  NULL as BadRate,
                  NULL as Odds,
                  NULL as LnOdds,
                  NULL as WoE,
                  NULL as IV
                  from df where $x is not NULL and $y is not NULL")
      )
    }
    # Missing Data
    x.na=fn$sqldf("select count(*) from df where $x is null")  
    y.na=fn$sqldf("select count(*) from df where $y is null")
    if(x.na>0){
      ivt=rbind(ivt,
                fn$sqldf(
                  "select 'Missing' as Cutpoint,
                  sum(case when $x is NULL and $y in (1,0) then 1 else 0 end) as CntRec,
                  sum(case when $x is NULL and $y=1 then 1 else 0 end) as CntGood,
                  sum(case when $x is NULL and $y=0 then 1 else 0 end) as CntBad,
                  NULL as CntCumRec,
                  NULL as CntCumGood,
                  NULL as CntCumBad,
                  NULL as PctRec,
                  NULL as GoodRate,
                  NULL as BadRate,
                  NULL as Odds,
                  NULL as LnOdds,
                  NULL as WoE,
                  NULL as IV
                  from df where $y is not NULL")
      )
    } else {
      ivt=rbind(ivt,
                c("Missing",0,0,0,NA,NA,NA,NA,NA,NA,NA,NA,NA))
    }
    # Total
    ivt=rbind(ivt,
              fn$sqldf(
                "select 'Total' as Cutpoint,
                count(*) as CntRec,
                sum(case when $y=1 then 1 else 0 end) as CntGood,
                sum(case when $y=0 then 1 else 0 end) as CntBad,
                NULL as CntCumRec,
                NULL as CntCumGood,
                NULL as CntCumBad,
                NULL as PctRec,
                NULL as GoodRate,
                NULL as BadRate,
                NULL as Odds,
                NULL as LnOdds,
                NULL as WoE,
                NULL as IV
                from df where $y is not NULL")
    )
    
    # Covert table to numeric
    options(warn=-1)
    ncol=ncol(ivt)
    for (i in 2:ncol){
      ivt[,i]=as.numeric(ivt[,i])
    }
    options(warn=0)
    
    # Complete Table: 1st row 
    ivt[1,5]=ivt[1,2] # Nbr Cum. Records
    ivt[1,6]=ivt[1,3] # Nbr Cum. Goods
    ivt[1,7]=ivt[1,4] # Nbr Cum. Bads
    
    # From 2nd row
    n=nrow(ivt)-2
    for (i in 2:n){ivt[i,5]=ivt[i,2]+ivt[i-1,5]
    ivt[i,6]=ivt[i,3]+ivt[i-1,6]
    ivt[i,7]=ivt[i,4]+ivt[i-1,7]}
    
    ivt[2,5]=ivt[2,2]+ivt[1,5]
    ivt[2,6]=ivt[2,3]+ivt[1,6]
    ivt[2,7]=ivt[2,4]+ivt[1,7]
    
    # Missing row
    ivt[i+1,5]=ivt[i,5]+ivt[i+1,2]
    ivt[i+1,6]=ivt[i,6]+ivt[i+1,3]
    ivt[i+1,7]=ivt[i,7]+ivt[i+1,4]
    
    # Calculating metrics
    options(scipen=999) # Remove Scientific Notation
    ivt[,8]=round(ivt[,2]/ivt[i+2,2],4) # PctRec
    ivt[,9]=round(ivt[,3]/ivt[,2],4) # GoodRate
    ivt[,10]=round(ivt[,4]/ivt[,2],4) # BadRate
    ivt[,11]=round(ivt[,3]/ivt[,4],4) # Odds
    ivt[,12]=round(log(ivt[,3]/ivt[,4]),4) # LnOdds
    G=ivt[i+2,3]
    B=ivt[i+2,4]
    LnGB=log(G/B) # IV Part 1
    ivt[,13]=round(log(ivt[,3]/ivt[,4])-LnGB,4) # WoE
    ivt[,14]=round(ivt[,13]*(ivt[,3]/G-ivt[,4]/B),4) # Mg IV
    # ivt[i+2,14]=round(sum(ivt[,13]*(ivt[,3]/G-ivt[,4]/B),na.rm=T),4) -- Old Calculation
    # Calculates Information Value even with undefined numbers
    ivt[i+2,14]=0.0000
    for (k in 1:(nrow(ivt)-1))
    {
      if(is.finite(ivt[k,14])) {mgiv=ivt[k,14]} else {mgiv=0.0000}
      ivt[i+2,14]=ivt[i+2,14]+mgiv
    }
    iv=ivt[i+2,14]
    # End Inf. Value Table ##################################################### 
  }
  list(ivtable=ivt,iv=iv,x=x,col_id=j,cuts=cutvct)
}





# Logistic Regression ----
require(InformationValue)
set.seed(1)

table(full_data$Survived)
#uneven classes

# producing training set with even classes
train_yes <- filter(full_data, Survived == 1)
train_no <- filter(full_data, Survived == 0)

index_yes <- sample(1:nrow(train_yes), 0.7*nrow(train_yes))
index_no <- sample(1:nrow(train_no), 0.7*nrow(train_yes))

#training set
train_logit <- rbind(train_yes[index_yes,], train_no[index_no,])

#testing set
test_logit <- rbind(train_yes[-index_yes,], train_no[-index_no,])



#Informational Values analysis

columns <- names(select(train_logit, - Survived))

IV_df <- data.frame(field = columns, iv = numeric(length(columns)))

for(column in columns) {
  data_type <- train_logit %>%
    .[, column] %>%
    typeof()
  
  if(data_type == 'integer' | data_type == 'double') {
    smb <- smbinning(train_logit, y = 'Survived', x = column)
    if(typeof(smb) != 'character') {
      IV_df[IV_df$field == column, 2] <- smb$iv
    }
  } else if (data_type == 'character' | data_type == 'logical') {
    temp <- train_logit %>%
      rename('desire' = column) %>%
      mutate(desire = as.factor(desire))
    smb <- smbinning.factor(temp, y = 'Survived', x = 'desire')
    if(typeof(smb) != 'character') {
      IV_df[IV_df$field == column, 2] <- smb$iv
    }
  }
}


#Model construction

train_logit <- train_logit %>%
  mutate()

logit <- glm(Survived ~ Pclass + Sex + Sex_Pclass + Has_Children + Sex_Children + Fare,
             data = train_logit, family=binomial(link="logit"))


predicted <- predict(logit, test_logit, type = 'response')


# Model validation and cut-off decisions

opt_cutoff <- optimalCutoff(test_logit$Survived, predicted)

plotROC(test_logit$Survived, predicted)
true_positive <- sensitivity(test_logit$Survived, predicted, threshold = opt_cutoff)
true_negative <- specificity(test_logit$Survived, predicted, threshold = opt_cutoff)
precision <- precision(test_logit$Survived, predicted, threshold = opt_cutoff)
mis_class_rate <- misClassError(test_logit$Survived, predicted, threshold = opt_cutoff) 
ks_stat <- ks_stat(test_logit$Survived, predicted)
ks_plot(test_logit$Survived, predicted)





# Random Forest ----