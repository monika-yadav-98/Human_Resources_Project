library(corrplot)
library(dplyr)
library(car)
library(randomForest)
library(tree)
library(pROC)

hr_train=read.csv("\\Users\\dell\\OneDrive\\Documents\\DataAnalytics_IITK\\Predictive Analysis in R\\Project4_Human_Resources\\hr_train.csv")
hr_test=read.csv("\\Users\\dell\\OneDrive\\Documents\\DataAnalytics_IITK\\Predictive Analysis in R\\Project4_Human_Resources\\hr_test.csv")

sum(is.na(hr_train))

colSums(is.na(hr_train))


##Data Preparation

hr_test$left= NA

hr_train$data = 'train'
hr_test$data = 'test'

all= rbind(hr_train,hr_test)
glimpse(all)

apply(all,2,function(x) length(unique(x)))


glimpse(all)


##Dummy creation

CreateDummies=function(data,var,freq_cutoff=100){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    name=gsub("/","_",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
} 

glimpse(all)

table(all$promotion_last_5years)

all$promotion_last_5years = as.factor(all$promotion_last_5years)
all$Work_accident = as.factor(all$Work_accident)

hr_train = all %>% filter(data == 'train') %>% select(-data) 
#View(hr_train)
hr_test= all %>% filter(data == 'test') %>% select(-left, -data) 
#View(hr_test)

##Model Building

set.seed(2)
s=sample(1:nrow(hr_train),0.75*nrow(hr_train))
train_75=hr_train[s,] 
test_25=hr_train[-s,] 

## check performance using decision Tree

tree.model=tree(left ~.,data=train_75)
#Check performance
test.score=predict(tree.model,newdata = test_25,type='vector')
pROC::roc(test_25$left,test.score) #AUC comes 0.8332 


##check performance using random Forest

rf.model=randomForest(left ~.,data=train_75)

#Check performance
test.score=predict(rf.model,newdata = test_25)
pROC::roc(test_25$left,test.score)  #AUC comes 0.8535

##So Use Random Forest model on entire train data to predict the values

fit_hr= randomForest(as.factor(left)~.,data=hr_train)
fit_hr


##Final prediction on entire data set

score=predict(fit_hr,newdata= hr_test, type="prob")[,2]
write.csv(score,'Monika_Yadav_P4_part2.csv',row.names = F)

importance(fit_hr)

varImpPlot(fit_hr)


##PLOTS

##Correlation
#Salary vs Turnover

vis_1<-table(all$salary,all$left)
vis_1

d_vis_1<-as.data.frame(vis_1)
d_vis_1


d_vis_1$leftString[d_vis_1$Var2 ==  1] = 'Left'
d_vis_1$leftString[d_vis_1$Var2 ==  0] = 'Not Left'

library(ggplot2)

p<-ggplot(d_vis_1, aes(x=Var1,y=Freq,fill=leftString)) +
  xlab("Salary") + ylab("Frequency")+
  geom_bar(position="dodge",stat='identity') + coord_flip()+ labs(title="Salary V.S. Turnover") 
p

#Majority of employees who left either had low or medium salary
#Barely any employees left with high salary

##Department V.S. Turnover

vis_2<-table(all$sales,all$left)
vis_2

d_vis_2<-as.data.frame(vis_2)
d_vis_2<-subset(d_vis_2,Var2==1)
d_vis_2

library(ggplot2)
d_vis_2$Var1 <- factor(d_vis_2$Var1, levels = d_vis_2$Var1[order(-d_vis_2$Freq)])
p<-ggplot(d_vis_2, aes(x=Var1,y=Freq,fill=Var1)) +
  xlab("Department") + ylab("Frequency")+
  geom_bar(stat='identity') +theme(axis.text.x = element_text(angle = 90, hjust = 1))
p


#The sales, technical, and support department were the top 3 departments to have employee turnover
#The management department had the smallest amount of turnover


#3.Turnover V.S. ProjectCount

vis_3<-table(all$number_project,all$left)
vis_3

d_vis_3<-as.data.frame(vis_3)
d_vis_3

d_vis_3$leftString[d_vis_3$Var2 ==  1] = 'Left'
d_vis_3$leftString[d_vis_3$Var2 ==  0] = 'Not Left'

library(ggplot2)
p<-ggplot(d_vis_3, aes(x=Var1,y=Freq,fill=leftString)) +
  xlab("Number of project") + ylab("Frequency")+
  geom_bar(position="dodge",stat='identity') + coord_flip()
p

#More than half of the employees with 2,6, and 7 projects left the company
#Majority of the employees who did not leave the company had 3,4, and 5 projects
#All of the employees with 7 projects left the company
#There is an increase in employee turnover rate as project count increases

## 4. Turnover V.S. Evaluation

left_data<-subset(all,left==1)
stay_data<-subset(all,left==0)

ggplot() + geom_density(aes(x=last_evaluation), colour="red", data=left_data) + 
  geom_density(aes(x=last_evaluation), colour="blue", data=stay_data)

#There is a biomodal distribution for those that had a turnover.
#Employees with low performance tend to leave the company more
#Employees with high performance tend to leave the company more
#The sweet spot for employees that stayed is within 0.6-0.8 evaluation

# 5.Turnover V.S. AverageMonthlyHours


ggplot() + geom_density(aes(x=average_montly_hours), colour="red", data=left_data) + 
  geom_density(aes(x=average_montly_hours), colour="blue", data=stay_data)

#Employees who had less hours of work (~150hours or less) left the company more
#Employees who had too many hours of work (~250 or more) left the company
#Employees who left generally were underworked or overworked.

## 6. #Turnover V.S. Satisfaction

ggplot() + geom_density(aes(x=satisfaction_level), colour="red", data=left_data) + 
  geom_density(aes(x=satisfaction_level), colour="blue", data=stay_data)

#There is a tri-modal distribution for employees that turnovered
#Employees who had really low satisfaction levels (0.2 or less) left the company more
#Employees who had low satisfaction levels (0.3~0.5) left the company more
#Employees who had really high satisfaction levels (0.7 or more) left the company more


# Summary:
#   With all of this information, this is what Bob should know about his company and why his
# employees probably left:
#   
# 1. Employees generally left when they are underworked (less than 150hr/month or 6hr/day)
# 2. Employees generally left when they are overworked (more than 250hr/month or 10hr/day)
# 3. Employees with either really high or low evaluations should be taken into consideration
#  for high turnover rate
# 4. Employees with low to medium salaries are the bulk of employee turnover
# 5. Employees that had 2,6, or 7 project count was at risk of leaving the company
# 6. Employee satisfaction is the highest indicator for employee turnover
# 7. Employee that had 4 and 5 yearsAtCompany should be taken into consideration for 
# high turnover rate


hr_train %>%
  select(Work_accident, left) %>%
  filter(Work_accident == 1) %>%
  arrange(left) %>%
  group_by(left) %>%
  summarise(n = n())

270/(1245 + 270)

median(hr_train$time_spend_company)
#3

hr_train %>%
  select(sales, average_montly_hours) %>%
  arrange(sales) %>%
  group_by(sales) %>%
  summarise(median_hrs = median(average_montly_hours),
            n = n()) %>%
  arrange(-median_hrs)


