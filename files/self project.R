library(tidyverse)
library(scales)
library(ROSE)
library(cutpointr)
library(ROCR)
library(glue)
library(rpart)
library(rpart.plot)
library(vip)
library(randomForest)
library(caret)
library(class)
library(knitr)
library(gridExtra)
#library(Metrics)
set.seed(seed=123)
# remotes::install_github("cran/DMwR")

#Data installation
df_raw <- read_csv("C:\\Users\\Saheli\\Desktop\\self project\\diabetes health ind.csv")
nm <- names(df_raw) 
catnm <- nm[nm != 'BMI'] #catagorical column names
df_raw %>% mutate_at(catnm, as.factor) -> df1

# Checking for NA value:
df_raw %>% is.na() %>% sum()

#Barplot of Diabetes_012
df1 %>% count(Diabetes_012) %>% 
  ggplot(aes(x = Diabetes_012, y = n/sum(n))) +
  geom_bar(stat = 'identity', width = 0.3,
           colour = 'black', fill = 'yellow') +
  labs(title = 'Frequency distribution of the Target variable (before modification)',
       x = 'Diabetes status', y = 'Density')

#Merging Diabetes==1 and Diabetes==2
df <- df_raw %>%
  filter(df_raw$Diabetes_012 != 1)
df$Diabetes_012[df$Diabetes_012 == 2] <- 1
df %>% mutate_at(catnm, as.factor) -> df

#Plot of Diabetes_012 after the change
df %>% count(Diabetes_012) %>% 
  ggplot(aes(x = Diabetes_012, y = n/sum(n))) +
  geom_bar(stat = 'identity', width = 0.3,
           colour = 'black', fill = 'yellow') +
  labs(title = 'Frequency distribution of the Target variable (after modification)',
       x = 'Diabetes status', y = 'Density')

# Plots of categorical variables:
catnm2 <- catnm[catnm != "Diabetes_012"]
par(mar = c(rep(2.5,4)))
par(mfrow = c(4,5))
for(i in catnm2){
  table(df[i]) %>% plot(main = paste(i),
                        xlab = '', ylab = '',
                        col = 'red')
}

# BMI ~ Categorical(particular)
catnm3 <- c('Sex', 'Age', 'Smoker', 'Stroke', 'HighChol',
            'HighBP', 'Veggies', 'Fruits', 'MentHlth', 'PhysHlth')
f2 <- function(var){       # violin plot
  df %>% ggplot(aes(x = {{var}}, y = BMI)) +
    geom_violin(draw_quantiles = c(0.25,0.5,0.75),
                fill = 'red', alpha = 0.4,
                linewidth = 1) +
    scale_y_continuous(n.breaks = 10) + theme_bw()
}

attach(df)
f2(Sex)

f2(HighChol)
detach(df)


ggplot(data = df,aes(y=BMI))+
  stat_boxplot(geom = "errorbar")+
  geom_boxplot(color='black',fill='4')+
  theme(panel.background = element_rect(fill='azure2'))+
  labs(title = 'Boxplot of BMI')+
  theme(plot.title = element_text(hjust=0.5))

#Obtaining Outliers
out <- boxplot(df$BMI, plot = F)$out
df_out <- df %>% filter((df$BMI %in% out))

#Barplot of Diabetes in outlier set
df_out %>% count(Diabetes_012) %>% 
  ggplot(aes(x = Diabetes_012, y = n/sum(n))) +
  geom_bar(stat = 'identity', width = 0.3,
           colour = 'black', fill = 'yellow') +
  labs(title = 'Frequency distribution of the Target variable in outlying data (w.r.t "BMI")',
       x = 'Diabetes status', y = 'Density')

#Bar plot Function
f3 <- function(var1, var2, option){
  
  if(option == 'prop'){
    df %>% group_by({{var1}}, {{var2}}) %>% 
      count() %>% group_by({{var1}}) %>%
      mutate(percentage = n/sum(n)) %>% 
      ggplot(aes(x = {{var1}}, y = percentage, fill = {{var2}})) +
      geom_bar(stat = 'identity', position = position_dodge2()) +
      scale_y_continuous(labels = percent) +
      theme_light()
  }else if(option == 'count'){
    df %>% group_by({{var1}}, {{var2}}) %>% 
      count() %>% ggplot(aes(x = {{var1}}, 
                             y = n, fill = {{var2}})) +
      geom_bar(stat = 'identity', position = position_dodge2()) +
      scale_y_continuous(n.breaks = 10) + theme_light()
  }
}

attach(df)
f3(Income, GenHlth, option = 'prop')

f3(Smoker, HeartDiseaseorAttack, option = 'prop')

#Chi square test summary
M <- data.frame(matrix(ncol = 3, nrow = 0))

for(i in catnm2){
  df %>% select('Diabetes_012', all_of(i)) %>% 
    table() %>% chisq.test() -> ch
  M %>% rbind(c(paste('Diabetes -', i),
                ch$statistic %>% round(3), 
                ch$p.value)) -> M
}

colnames(M) <- c('Pairs','Statistic','p-value')

#Under sampling
df_u <- ovun.sample(Diabetes_012~., data = df,
                    p = 0.5, method = 'under')$data %>% 
  as_tibble()

df %>% count(Diabetes_012) %>%
  mutate("percentage" = percent(n/sum(n))) %>% as.matrix

df_u %>% count(Diabetes_012) %>% 
  mutate("percentage" = percent(n/sum(n))) %>% as.matrix()

# Function for PRECISION & RECALL:
stats <- function(C_train, C_test, model){      # C := Confusion Matrix and Statistics
  t <- C_test$table
  t1 <- C_train$table
  
  acc_train <- C_train$overall[[1]]
  acc_test <- C_test$overall[[1]]
  pre <- t[2,2]/(t[2,2]+t[2,1])
  rec <- t[2,2]/(t[2,2]+t[1,2])
  f1 <- 2*(rec*pre)/(rec+pre)
  
  matrix(c(acc_train,acc_test,pre,rec,f1), byrow = T,
         dimnames = list(c('Train Accuracy','Test Accuracy','Precision',
                           'Recall','F1-Score'),
                         paste(model, ""))) -> M
  return(list('TestConfusionMatrix' = t,
              'TrainConfusionMatrix' = t1,
              'Metrics' = M))
}

## Train - Test SPLIT:
n <- nrow(df_u); set.seed(42)
rs <- sample(1:n, size = 0.75*n, replace = F)
train_data <- df_u[rs,]
test_data <- df_u[-rs,]

#GENERALISED LINEAR MODEL

link <- c('probit','logit','cauchit')
M <- matrix(ncol = 2, nrow = 3,
            dimnames = list(link, c("AIC","Deviance")))

for(i in 1:3){
  glm(Diabetes_012 ~ ., data = train_data,
      family = binomial(link = link[i])) %>% 
    summary() -> s
  M[i,] <- c(s$aic, s$deviance)
}

#### Fitting (INITIAL) :
glm(Diabetes_012 ~ ., data = train_data,
    family = binomial(link = 'logit')) %>% 
  summary() -> s1
s1$coefficients[1:15,]

insig_vars <- c('Fruits','Veggies','MentHlth','PhysHlth',
                'AnyHealthcare','Education') # to be removed from the model
train_data1 <- train_data %>% select(!all_of(insig_vars))
test_data1 <- test_data %>% select(!all_of(insig_vars))

# Re-formatting GenHlth:
#u1 <- train_data %>% pull(GenHlth) %>% unique(); u1
train_data1 %>% mutate(GenHlth = case_when(
  GenHlth %in% 1:3 ~ "<= 3",
  GenHlth %in% 4:5 ~ "> 3"
)) -> train_data1

test_data1 %>% mutate(GenHlth = case_when(
  GenHlth %in% 1:3 ~ "<= 3",
  GenHlth %in% 4:5 ~ "> 3"
)) -> test_data1

# Re-formatting Age:
#u2 <- train_data %>% pull(Age) %>% unique(); u2
train_data1 %>% mutate(Age = case_when(
  Age %in% 1:4 ~ "< 5",
  Age %in% 5:9 ~ "5-9",
  Age %in% 10:13 ~ "> 9"
)) -> train_data1

test_data1 %>% mutate(Age = case_when(
  Age %in% 1:4 ~ "< 5",
  Age %in% 5:9 ~ "5-9",
  Age %in% 10:13 ~ "> 9"
)) -> test_data1

# Re-formatting Income:
#u3 <- train_data %>% pull(Income) %>% unique(); u3
train_data1 %>% mutate(Income = case_when(
  Income %in% 1:4 ~ "Low",
  Income %in% 5:8 ~ "High"
)) -> train_data1

test_data1 %>% mutate(Income = case_when(
  Income %in% 1:4 ~ "Low",
  Income %in% 5:8 ~ "High"
)) -> test_data1

glm(Diabetes_012 ~ ., data = train_data1,
    family = binomial(link = 'logit')) -> g

g %>% summary() -> sl
sl$coefficients[1:12,]

## Optimum cut-off selection:
metric_func <- function(data, phat){  # function to store the 
  cut_points <- seq(0.01,0.99,0.01)    # necessary metrics
  
  d <- data.frame(matrix(nrow = length(cut_points),
                         ncol = 4, dimnames = list(
                           paste(1:length(cut_points)),
                           c('p_opt','Accuracy',
                             'Sensitivity','Specificity')
                         )))
  
  for(i in 1:length(cut_points)){
    C <- confusionMatrix(
      if_else(phat >= cut_points[i], 1, 0) %>% as.factor(),
      data$Diabetes_012)
    
    d[i,] <- c(cut_points[i], C$overall[[1]],
               C$byClass[[1]],C$byClass[[2]])
  }
  
  d$sens_spec <- d[,3]*d[,4]
  return(d)
}

#Training and obtaing the optimum cut off
phat_train <- g %>% predict.glm(type = 'response')

m_train <- metric_func(train_data1,phat_train) 

p1_opt <- m_train[which.max((m_train$Accuracy)),]$p_opt
p2_opt <- m_train[which.max((m_train$sens_spec)),]$p_opt

### ROC curves:
ROC_func <- function(m){
  plot(1 - m$Specificity, m$Sensitivity, type = 'l',
       main = 'ROC curve', 
       ylab = 'Specificity (TPR)',
       xlab = '1-Sensitivity (FPR)', lwd = 2, las = 1)
  abline(a = 0, b = 1, h = 0:1, v = 0:1, lty = 2)
}

ROC_func(m_train)

# Considering the average of cut points

p_avg <- mean(c(p1_opt, p2_opt))

### On test data:
phat_test <- predict.glm(g, newdata = test_data1,
                         type = 'response')

confusionMatrix(ifelse(phat_test >= p_avg, 1, 0) %>% 
                  as.factor(), test_data1$Diabetes_012) -> C_tst
confusionMatrix(ifelse(phat_train >= p_avg, 1, 0) %>% 
                  as.factor(), train_data1$Diabetes_012) -> C_trn

stats(C_trn, C_tst, "Logistic")$Metrics -> M_l
M_l

# Decision Tree
folds <- createMultiFolds(train_data$Diabetes_012, k = 5, times = 5)
control <- trainControl(method = "repeatedcv", index = folds)
classifier_cv <- train(Diabetes_012 ~ ., data = train_data, 
                       method = "rpart", trControl = control)

classifier_cv$finalModel -> dtree_f # final model

rpart.plot(dtree_f, extra = 4) # visualizing tree

#plot of important variable
imp <- vip(dtree_f)
print(imp)

#Metrics on Train data and Test data
diab_pred_trn <- predict(classifier_cv, newdata = train_data)
diab_pred_tst <- predict(classifier_cv, newdata = test_data)
confusionMatrix(test_data$Diabetes_012,diab_pred_tst) -> C_tst
confusionMatrix(train_data$Diabetes_012,diab_pred_trn) -> C_trn
stats(C_trn, C_tst, 'Decision Tree')$Metrics -> M_dt

#Random Forest
rf_initial <- randomForest(Diabetes_012 ~ ., data = train_data,
                           ntree = 300, mtry=4) 

varImpPlot(rf_initial)

# Plot of errors:
plot(rf_initial, lwd = 2, lty = 1)   
grid(lty = 3, col = 'black')

rf <- randomForest(Diabetes_012 ~ ., data = train_data,
                   ntree = 150) 


# Prediction:

diab_pred_tst <- predict(rf, newdata = test_data,
                         type = 'class')
diab_pred_trn <- predict(rf, newdata = train_data,
                         type = 'class')


# Model metrics:
confusionMatrix(test_data$Diabetes_012, diab_pred_tst) -> C_tst
confusionMatrix(train_data$Diabetes_012, diab_pred_trn) -> C_trn
stats(C_trn, C_tst, 'Random Forest')$Metrics -> M_rf

#KNN 
# Choosing optimal value of "k" :
error <- array(0)
k <- seq(1, 49, by = 2)

for(i in 1:length(k)){
  knn_model <- knn(train_data[,-1], test_data[,-1], 
                   cl = train_data$Diabetes_012, k = k[i])
  error[i] <- mean(test_data$Diabetes_012 != knn_model)
}

# Error plot:
data.frame('k' = k, 'ErrorRate' = error) %>% 
  ggplot(aes(x = k, y = ErrorRate)) + 
  geom_line(colour = 'red') + geom_point() +
  scale_y_continuous(labels = percent, n.breaks = 10) +
  scale_x_continuous(n.breaks = length(error)) +
  theme_minimal()

k_opt <- 35

# Final model:
knn_model <- knn(train_data[,-1], test_data[,-1], 
                 cl = train_data$Diabetes_012, k = k_opt)


confusionMatrix(knn_model,as.factor(test_data$Diabetes_012)) -> C
stats1 <- function(C, model){      # C := Confusion Matrix and Statistics
  t <- C$table
  
  acc<- C$overall[[1]]
  pre <- t[2,2]/(t[2,2]+t[2,1])
  rec <- t[2,2]/(t[2,2]+t[1,2])
  f1 <- 2*(rec*pre)/(rec+pre)
  
  matrix(c(acc,pre,rec,f1), byrow = T,
         dimnames = list(c('Accuracy','Precision',
                           'Recall','F1-Score'),
                         paste(model, ""))) -> M
  return(list('TestConfusionMatrix' = t,
              'Metrics' = M))
}

stats1(C, "KNN")$Metrics -> M_knn

#Comparision between the Models
acc <- c(M_l[2,], M_dt[2,], M_rf[2,], M_knn[1,])
preci <- c(M_l[3,], M_dt[3,], M_rf[3,], M_knn[2,])
rec <- c(M_l[4,], M_dt[4,], M_rf[4,], M_knn[3,])
model <- c("Logistic", "Decision Tree", "Random Forest", "KNN")
df_metrics <- data.frame(acc,preci,rec,model)
colnames(df_metrics) <- c("Accuracy", "Precision", "Recall","Model")
df_metrics$Model <- 
  factor(df_metrics$Model, levels = model)

plot_list <- list()

for (i in colnames(df_metrics)[-4]) {
  ggplot(data = df_metrics, aes(x = Model, y = .data[[i]], fill = Model)) +
    geom_bar(stat = "identity", width = 0.2) +
    labs(title = paste(i, "of Different Models"),
         x = "Model", y = i) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> p
  
  plot_list[[i]] <- p
}
grid.arrange(grobs = plot_list, ncol = 3)

