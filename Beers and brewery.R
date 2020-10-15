library(class)
library(caret)
library(e1071)
library(magrittr)
library(XML) 
library(dplyr)
library(tidyr)
library(stringi)
library(rvest) 
library(ggplot2)
library(RCurl) 
library(plotly)
library(gridExtra)
library(httr)
library(jsonlite)
library(e1071)
library(naniar)
library(scales)
library(gghighlight)
library(stringr)
library(knitr)
library(mice)
library(Hmisc)
library(ggpubr)
library(maps)
library(usmap)
knitr::opts_chunk$set(error = TRUE)
beers=read.csv('/Users/renfengwang/Documents/SMU\ Data\ Science\ Program/Doing\ Data\ Science/Project\ 1/Beers.csv',header=T)
brewery=read.csv('/Users/renfengwang/Documents/SMU\ Data\ Science\ Program/Doing\ Data\ Science/Project\ 1/Breweries.csv',header=T)
head(beers)
head(brewery)
#Question 1
brewery %>% arrange(State) %>% ggplot(aes(x=State),count=Name)+geom_bar()+geom_text(aes(label=..count..),stat='count',vjust=-.5) +
  xlab('States')+ ylab('Brewery Numbers') + ggtitle('Numbers of Brewery by State')  #Bar plot to count the brewery numbers in each state

#Question 2
colnames(beers)[colnames(beers)=='Brewery_id']='Brew_ID'  #Change one of the data frame column names before merging two data sets.
df_beer=merge(beers, brewery, by='Brew_ID', all=T)  #Outer join two data frames.
colnames(df_beer)[colnames(df_beer)=='Name.x']='Beer_Name'
colnames(df_beer)[colnames(df_beer)=='Name.y']='Brewery_Name'

head(df_beer) 
tail(df_beer)

#Question 3
df_beerall=df_beer %>% replace_with_na_all(condition = ~.x=='')
df_beerall=df_beerall %>% replace_with_na_all(condition = ~.x==' ')
gg_miss_var(df_beerall)

table(is.na(df_beerall$IBU))
table(is.na(df_beerall$ABV))
table(is.na(df_beerall$ABV | df_beerall$IBU))

imp=mice(df_beerall, method='norm.predict', m=5)
beer_imp=complete(imp)
gg_miss_var(beer_imp)
table(is.na(beer_imp$IBU))
table(is.na(beer_imp$ABV))
table(is.na(beer_imp$Style))


#Question 4
Median_ABV_IBU=beer_imp %>% arrange(State) %>% group_by(State) %>% summarise(Median_ABV=median(ABV, na.rm=TRUE), Median_IBU=median(IBU,na.rm=TRUE))

Median_ABV_IBU %>% ggplot(aes(x=State, y=Median_ABV,width=.5))+geom_bar(stat='identity') + 
  geom_text(aes(label=percent(Median_ABV, accuracy = 0.01)),vjust=-.5,size=2.5,check_overlap = T) +
  xlab('States') +ylab('Median Alcoholic Content')+ggtitle('Median Alcoholic Content by State')

Median_ABV_IBU %>% ggplot(aes(x=State, y=Median_IBU, width=.5))+geom_bar(stat='identity') + 
  geom_text(aes(label=sprintf("%0.1f", round(Median_IBU, digits = 1))),vjust=-.5,size=2.5,check_overlap = T) +
  xlab('States') +ylab('Median International Bitterness')+ggtitle('Median International Bitterness by State') 

#Question 5
Max_ABV_IBU=beer_imp %>% arrange(State) %>% group_by(State) %>% summarise(Max_ABV=max(ABV, na.rm=TRUE), Max_IBU=max(IBU,na.rm=T))
Max_ABV_IBU %>% ggplot(aes(x=State, y=Max_ABV,width=.5))+geom_bar(stat='identity') + 
  geom_text(aes(label=percent(Max_ABV, accuracy = 0.01)),vjust=-.5,size=2.5,check_overlap = T) +
  xlab('States') +ylab('Max Alcoholic Content')+ggtitle('Max Alcoholic Content by State')

Max_ABV_IBU %>% ggplot(aes(x=State, y=Max_IBU,width=.5))+geom_bar(stat='identity') + 
  geom_text(aes(label=sprintf("%0.1f", round(Max_IBU, digits = 1))),vjust=-.5,size=2.5,check_overlap = T) +
  xlab('States') +ylab('Max International Bitterness')+ggtitle('Max International Bitterness by State')

#Question 6
beer_imp%>% summarise(Mean=mean(ABV, na.rm=TRUE),
                                             Median=median(ABV,na.rm=T),
                                             Min=min(ABV,na.rm=T),
                                             Max=max(ABV,na.rm=T),
                                             SD=sd(ABV,na.rm=T),
                                             N=n())

beer_imp %>% filter(!is.na(ABV)) %>% ggplot(aes(x=ABV))+geom_histogram(aes(y=..density..),colour='black',fill='white')+
  geom_density(alpha=.5, fill='#FF6666')

#Question 7
beer_imp %>% filter(!is.na(ABV) &!is.na(IBU)) %>% 
  ggplot(aes(y=ABV, x=IBU))+geom_point(position='jitter')+geom_smooth(method=loess)

rcorr(beer_imp$ABV, beer_imp$IBU,type='pearson')
ggscatter(beer_imp,x='IBU', y='ABV',add='reg.line',conf.int=T,cor.coef = T,cor.method='pearson')

#Question 8  KNN

beer_imp %>% filter(is.na(beer_imp$Style))

beer_imp$Style[beer_imp$Beer_ID=='2210']='Red Ale - American Amber/Red'
beer_imp$Style[beer_imp$Beer_ID=='2527']='Lager - Märzen/Oktoberfest'
beer_imp$Style[beer_imp$Beer_ID=='1635']='Scottish Ale'
beer_imp$Style[beer_imp$Beer_ID=='1796']='IPA - AMERICAN'
beer_imp$Style[beer_imp$Beer_ID=='1790']='FRUITED ALE'
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
df_beer_IPA=beer_imp %>% filter(!is.na(ABV) &!is.na(IBU)) %>% 
  filter(str_detect(Style, regex(str_c('\\b','IPA','\\b',sep=''), ignore_case = T)))

df_beer_IPA$Style=as.factor('IPA')

df_beer_Ale=beer_imp %>% filter(!is.na(ABV) &!is.na(IBU)) %>% 
  filter(str_detect(Style, regex(str_c('\\b','Ale','\\b',sep=''), ignore_case = T)))

df_beer_Ale$Style=as.factor('Ale')

df_beer_test=rbind(df_beer_IPA, df_beer_Ale)

df_beer_test %>% ggplot(aes(x=ABV, y=IBU)) +geom_point(aes(colour=Style))

iterations = 500
numks = 30
splitPerc = .7
masterAcc = matrix(nrow = iterations, ncol = numks)
set.seed(6)
trainIndices = sample(1:dim(df_beer_test)[1],round(splitPerc * dim(df_beer_test)[1]))
beer_train = df_beer_test[trainIndices,]
beer_test = df_beer_test[-trainIndices,]
classifications=knn(beer_train[,c(4,5)],beer_test[,c(4,5)],beer_train$Style, prob = TRUE, k = 5)
CM = confusionMatrix(table(classifications,beer_test$Style))
classifications
CM 

for(j in 1:iterations)
{
  accs = data.frame(accuracy = numeric(30), k = numeric(30))
  trainIndices = sample(1:dim(df_beer_test)[1],round(splitPerc * dim(df_beer_test)[1]))
  beer_train = df_beer_test[trainIndices,]
  beer_test = df_beer_test[-trainIndices,]
  for(i in 1:numks)
  {
    classifications = knn(beer_train[,c(4,5)],beer_test[,c(4,5)],beer_train$Style, prob = TRUE, k = i)
    table(classifications,beer_test$Style)
    CM = confusionMatrix(table(classifications,beer_test$Style))
    masterAcc[j,i] = CM$overall[1]
  }
}
MeanAcc = colMeans(masterAcc)
p=ggplot(mapping=aes(x=seq(1,numks,1), y=MeanAcc))+geom_line()
ggplotly(p)


#Naive Bayes--------------

iterations = 500
masterAcc = matrix(nrow = iterations)
masterSen = matrix(nrow = iterations)
masterSpec = matrix(nrow = iterations)
splitPerc = .7 
for(j in 1:iterations)
{
  
  trainIndices = sample(1:dim(df_beer_test)[1],round(splitPerc * dim(df_beer_test)[1]))
  beer_train = df_beer_test[trainIndices,]
  beer_test = df_beer_test[-trainIndices,]
  model = naiveBayes(beer_train[,c(4,5)],as.factor(beer_train$Style),laplace = 1)
  table(predict(model,beer_test[,c(4,5)]),as.factor(beer_test$Style))
  CM = confusionMatrix(table(predict(model,beer_test[,c(4,5)]),as.factor(beer_test$Style)))
  masterAcc[j] = CM$overall[1]
  masterSen[j] = CM$byClass[1]
  masterSpec[j] = CM$byClass[2]
}
MeanAcc = colMeans(masterAcc)
MeanSen = colMeans(masterSen)
MeanSpec = colMeans(masterSpec)
MeanAcc
MeanSen
MeanSpec



#Question 9 ABV and IBU average by state
Mean_ABV_IBU=beer_imp %>% group_by(State) %>% summarise(Mean_ABV=mean(ABV, na.rm=TRUE), Mean_IBU=mean(IBU,na.rm=TRUE))
colnames(Mean_ABV_IBU)[colnames(Mean_ABV_IBU)=='State']='abbr'
statedata= statepop[,c(2,3)]
statedata$abbr=as.factor(statedata$abbr)
Mean_ABV_IBU$abbr=as.factor(Mean_ABV_IBU$abbr)
statedata1=merge(statedata,Mean_ABV_IBU,by='abbr',all.x=T)
statedata2=cbind(statedata1,Mean_ABV_IBU)
statedata3=statedata2[,c(1,2,6,7)]
colnames(statedata3)[colnames(statedata3)=='full']='state'

plot_usmap(data = statedata3, values = "Mean_ABV", color = "red") + 
  scale_fill_continuous(
    low = "white", high = "red", name = "Mean_ABV", label = scales::comma
  ) + theme(legend.position = "right") + ggtitle('Mean ABV by State')

plot_usmap(data = statedata3, values = "Mean_IBU", color = "black") + 
  scale_fill_continuous(
    low = "white", high = "blue", name = "Mean_IBU", label = scales::comma
  ) + theme(legend.position = "right") + ggtitle('Mean IBU by State')

#Back to Question 7
df_beer_study=rbind(df_beer_sort, df_beer_Ale)
df_beer_study %>% ggplot(aes(y=ABV, x=IBU))+geom_point(position='jitter')+geom_smooth() +facet_wrap(~Style)

