library(dplyr)
library(tidyr)
library(tree)
library(randomForest)
library(cvTools)



setwd("C:/Users/vibhu/Desktop/R Projects/Real Estate")

h_train=read.csv("housing_train.csv")

h_test= read.csv("housing_test.csv")

h_test$Price=NA

h_train$data='train'
h_test$data='test'

hg=rbind(h_train,h_test)


# hg = hg %>% mutate_all(na_if,"")
# hg = hg %>% fill(CouncilArea)


CreateDummies=function(data,var,freq_cutoff=0){
  # data= ld_all
  # var= "State"
  # freq_cutoff=100
  
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    #cat='FL'
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)

}

table(hg$CouncilArea)

# hg %>% select(Suburb>100)

table(hg$CouncilArea)

hg=CreateDummies(hg,"Type",0)
hg=CreateDummies(hg,"Method",0)
hg=CreateDummies(hg,"CouncilArea",120)
hg=CreateDummies(hg,"Suburb",150)
hg=CreateDummies(hg,"SellerG",150)




# hg = as.numeric(hg$CouncilArea)
# hg <- fill(hg, hg=='',NA)


lapply(hg,function(x) sum(is.na(x)))
glimpse(hg)


hg=hg[!(is.na(hg$Address)),]

for(col in names(hg)){
  
  if(sum(is.na(hg[,col]))>0 & !(col %in% c("data","Price"))){
    
    hg[is.na(hg[,col]),col]=mean(hg[,col],na.rm=T)
  }
  
}


h_train=hg %>% filter(data=='train') %>% select(-data)
h_test=hg %>% filter(data=='test') %>% select(-data,-Price)

##

set.seed(5)
s=sample(1:nrow(h_train),0.7*nrow(h_train))
h_train1=h_train[s,]
h_train2=h_train[-s,]

hgm=lm(Price~.-Address,data=h_train1)

library(car)
# we'll take vif cutoff as 5

sort(vif(hgm),decreasing = T)

hgm=lm(Price~.-Address-Method_S,data=h_train1)

sort(vif(hgm),decreasing = T)

hgm=lm(Price~.-Address-Method_S-CouncilArea_,data=h_train1)

sort(vif(hgm),decreasing = T)

# p-value take the cutoff .05


summary(hgm)

hgm=step(hgm)

## AIC score 

summary(hgm)

formula(hgm)

hgm=lm(log(Price ~ Rooms + Distance + Postcode + Bedroom2 + Bathroom + Car + 
         Landsize + BuildingArea + YearBuilt + Type_u + Type_h + Method_VB + 
         Method_SP + Method_PI + CouncilArea_Whitehorse + CouncilArea_Brimbank + 
         CouncilArea_HobsonsBay + CouncilArea_Bayside + CouncilArea_Banyule + 
         CouncilArea_PortPhillip + CouncilArea_Maribyrnong + CouncilArea_Stonnington + 
         CouncilArea_MooneeValley + CouncilArea_Moreland + CouncilArea_Boroondara + 
         Suburb_Essendon + Suburb_SouthYarra + Suburb_StKilda + Suburb_Preston + 
         Suburb_Richmond + Suburb_Reservoir + SellerG_Miles + SellerG_Sweeney + 
         SellerG_RT + SellerG_Biggin + SellerG_Ray + SellerG_Marshall + 
         SellerG_hockingstuart + SellerG_Jellis, data = h_train1))


val.pred=predict(hgm,newdata=h_train2)

errors=h_train2$Price-val.pred

errors**2 %>% mean() %>% sqrt()


hgm.final=hgm=lm(Price ~ .-Address,
                 data=h_train)

hgm.final=step(hgm.final)

summary(hgm.final)

test.pred=predict(hgm.final,newdata=h_test)

write.csv(test.pred,"submision1.csv",row.names = F)
##------------------------------------------------------------------------------
plot(hgm.final,1) 
plot(hgm.final,2) 
plot(hgm.final,3) 
plot(hgm.final,4) 

#Using Decision Tree
set.seed(5)
s=sample(1:nrow(h_train),0.8*nrow(h_train))
h_train1=h_train[s,]
h_train2=h_train[-s,]

ld.tree=tree(Price~.-Address,data=h_train1)

ld.tree

plot(ld.tree)
text(ld.tree)

## Performance on validation set

val.IR=predict(ld.tree,newdata = h_train2)

rmse_val=((val.IR)-(h_train2$Price))^2 %>% mean() %>% sqrt()
rmse_val

ld.tree.final=tree(Price~.-Address,data=h_train)
test.pred=predict(ld.tree.final,newdata=h_test)
write.csv(test.pred,"mysubmission_Real_Estate.csv",row.names = F)

##-----------------------------------------------------------------------------
##Random Forest

param=list(mtry=c(5,10,15,20,25),
           ntree=c(50,100,200,500,700),
           maxnodes=c(5,10,15,20,30,50),
           nodesize=c(1,2,5,10))

## Function for selecting random subset of params

subset_paras=function(full_list_para,n=10){ #i'm going to 10 parameters out of param randomly 
  
  all_comb=expand.grid(full_list_para)# expand the grid for all the parameters 
  
  s=sample(1:nrow(all_comb),n)#take sample of the param rows and select n no of rows u want 
  
  subset_para=all_comb[s,]#store the no of rows to object subset_para
  
  return(subset_para)
}

## 

#no of combinations I want from param is 10 
num_trials=50 #you can try 50 combinations as well

my_params=subset_paras(param,num_trials)


my_params
myerror=9999999
my_params[1,]


for(i in 1:num_trials){
  # print(paste0('starting iteration:',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  print(i)
  #we dont do shortlisting of variables in RF algorithm is powerful enough
  k=cvTuning(randomForest,Price~.-Address,
             data=h_train,#we dont split the data into test & train here
             tuning =params,
             folds = cvFolds(nrow(h_train), K=10, type = "random"), #we are randomly going to subset the data
             #how many folds it is creating?
             seed =2) #creating memory location to store the info
 
  score.this=k$cv[,2]#default value of RSME wud be here
  
  if(score.this<myerror){ #is ur rsme score.this lesser than myerror= 9999999
    # print(params)
    # print(score.this)
    # uncomment the line above to keep track of progress
    myerror=score.this #if so then store it in myerror
    # print(myerror)
    # uncomment the line above to keep track of progress
    best_params=params
  }
  
  #print('DONE')
  # uncomment the line above to keep track of progress
}

best_params
myerror

housing.rf.final=randomForest(Price~.-Address,
                         mtry=best_params$mtry,
                         ntree=best_params$ntree,
                         maxnodes=best_params$maxnodes,
                         nodesize=best_params$nodesize,
                         data=h_train)
test.pred=predict(housing.rf.final,newdata = h_test)


d=importance(housing.rf.final)
d=as.data.frame(d)
d$VariableName=rownames(d)
options(scipen=999) #this diables the scientific donation
d %>% arrange(desc(IncNodePurity))



varImpPlot(housing.rf.final)


var='Rooms'

pred.resp = predict(housing.rf.final,newdata=h_train)
myvar = h_train[,var]

trend.data=data.frame(Response=pred.resp,myvar=myvar)
library(ggplot2)

trend.data %>% ggplot(aes(y=Response,x=myvar))+
  geom_smooth()
#method = "loess" used in this case
# Warning message:
# Computation failed in `stat_smooth()`:
# x has insufficient unique values to support 10 knots: reduce k. 


write.csv(test.pred,"Real_Estate_Prices.csv",row.names = F)


