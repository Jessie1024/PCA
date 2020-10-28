# Q1
library(tidyverse)
library(dplyr)
library(gbm)

urlfile<-'https://raw.githubusercontent.com/Vincent-Toups/bios611-project1/master/source_data/datasets_26073_33239_weight-height.csv'
gender_classification<-read.csv(urlfile)

gender_classification$Gender<-as.factor(gender_classification$Gender)
gender_classification$Gender_factor[gender_classification$Gender=="Male"]=0
gender_classification$Gender_factor[gender_classification$Gender=="Female"]=1

model_split <- function(dfi, train_p, validate_p, test_p, col_name="exp_group"){
  dfi <- sample_n(dfi, nrow(dfi),replace=FALSE);
  p <- (seq(nrow(dfi))-1)/nrow(dfi);
  train_dfi <- dfi %>% filter(p < train_p);
  validate_dfi <- dfi %>% filter(p < train_p + validate_p & p >= train_p);
  test_dfi <- dfi %>% filter(p >= train_p + validate_p);
  train_dfi[[col_name]] <- "train";
  validate_dfi[[col_name]] <- "validate";
  test_dfi[[col_name]] <- "test";
  rbind(train_dfi, validate_dfi, test_dfi);
}

gender_classification<- rbind(model_split(gender_classification %>% filter(Gender=='Male'), 1/3, 1/3, 1/3),
                              model_split(gender_classification %>% filter(Gender=='Female'), 1/3, 1/3, 1/3));
gender_classification%>% group_by(Gender, exp_group) %>% tally()

train <- gender_classification %>% filter(exp_group=="train");
validate <- gender_classification %>% filter(exp_group=="validate");
test <- gender_classification %>% filter(exp_group=="test");

train$Gender_factor[train$Gender=="Male"]=0
train$Gender_factor[train$Gender=="Female"]=1
gbm <- gbm(Gender_factor~Height+Weight, distribution="bernoulli",
           data=train,
           n.trees = 100,
           interaction.depth = 2,
           shrinkage = 0.1);
pred_bgm <- predict(gbm, newdata=validate, type="response");
pred_gbm<-as.data.frame(pred_bgm)
accuracy_gbm<-sum((pred_bgm>0.5) == validate$Gender_factor)/nrow(validate);

# Q2
urlfile2<-'https://raw.githubusercontent.com/Vincent-Toups/bios611-project1/master/source_data/datasets_38396_60978_charcters_stats.csv'
superhero<-read.csv(urlfile2)
# 1 omit the missing data
library(stringr)
superhero<-superhero%>%
          na.omit()%>% 
          filter(!superhero$Total==5)
#2 we need two components to get 85% of the variation in the dataset
superhero_num<-superhero[c(3,4,5,6,7,8)]
pcs<-prcomp(superhero_num)
summary(pcs)

#3 Normalization? yes
pcs_norm<-prcomp(superhero_num,scale. = T)
summary(pcs_norm)

#4 Is the Total really the total? yes
superhero<-superhero%>%
          mutate(sum=rowSums(.[3:8]))%>%
          mutate(dif=sum-Total)
totaldif<-sum(superhero$dif)

#5 
total_super<-superhero[c(3,4,5,6,7,8,9)]
pcs_total<-prcomp(total_super,scale. = T)
summary(pcs_total)
pcs_total$rotation

#6
library(ggplot2)
biplot(pcs_norm,scale = 0)

#Q3
library(ggplot2)
df_no<-read.csv(file= 'df_no.csv')
lowd<-read.csv(file= 'lowd.csv')
lowd$cluster[lowd$cluster==1]<-2
lowd$cluster[lowd$cluster==0]<-1
ggplot(lowd,aes(X1,X2))+geom_point(color=lowd$cluster)+ 
  theme(legend.position = "right")

#Q5
library(caret)
model_split <- function(dfi, train_p, validate_p, test_p, col_name="exp_group"){
  dfi <- sample_n(dfi, nrow(dfi),replace=FALSE);
  p <- (seq(nrow(dfi))-1)/nrow(dfi);
  train_dfi <- dfi %>% filter(p < train_p);
  validate_dfi <- dfi %>% filter(p < train_p + validate_p & p >= train_p);
  test_dfi <- dfi %>% filter(p >= train_p + validate_p);
  train_dfi[[col_name]] <- "train";
  validate_dfi[[col_name]] <- "validate";
  test_dfi[[col_name]] <- "test";
  rbind(train_dfi, validate_dfi, test_dfi);
}

names(superhero)<-tolower(names(superhero))

tidy_model <- rbind(model_split(superhero %>% filter(alignment=="good"), 1/3, 1/3, 1/3),
                    model_split(superhero %>% filter(alignment=="bad"), 1/3, 1/3, 1/3));
tidy_model %>% group_by(alignment, exp_group) %>% tally()

train.data<-tidy_model%>%filter(exp_group=="train");
validate.data <- tidy_model %>% filter(exp_group=="validate");
test.data <- tidy_model %>% filter(exp_group=="test");

set.seed(123)

train.control<-trainControl(method = "repeatedcv",
                            number=10,
                            repeats = 10
                  )


train.data$alignment<-as.factor(train.data$alignment)

gbm.fit<-train(alignment~ intelligence+strength+speed+durability+power+combat,
               data = train.data,
               method='gbm',
               trControl = train.control,
               verbose=FALSE
)   

pred_gbm <- predict(gbm.fit, newdata=test.data, type="raw")
pred_gbm<-as.data.frame(pred_gbm)
accuracy_gbm<-sum(pred_gbm == test.data$alignment)/nrow(test.data)




