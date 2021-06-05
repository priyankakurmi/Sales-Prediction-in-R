install.packages("xgboost")
install.packages("cowplot")

library(data.table)
library(dplyr)
library(ggplot2)
library(xgboost)
library(corrplot)
library(cowplot)

train = fread('train_v9rqX0R.csv')
test = fread('test_AbJTz2l.csv')
submission = fread('sample_submission_8RXa3c6.csv')
dim(train);dim(test)

names(train)
names(test)

str(train)
str(test)

test[,Item_Outlet_Sales := NA]
combi <- rbind(train, test)
dim(combi)

#Data Visualization
ggplot(train)+geom_histogram(aes(train$Item_Outlet_Sales), binwidth = 100, fill= "darkgreen") + xlab("Item_Outlet_Sales") #dependent variable is right skewed so, some data transformation is needed to treat it's skewness

summary(combi[,sapply(combi[,1:12], typeof) == "numbers"])
summary(combi[,sapply(combi[,1:12], typeof) == "integer"])
summary(combi[,sapply(combi[,1:12], typeof) == "double"])
summary(combi[,sapply(combi[,1:12], typeof) == "character"])

#Numeric Independent variables
p1 = ggplot(combi)+geom_histogram(aes(train$Item_Weight), binwidth = 0.5, fill="blue")
p2 = ggplot(combi)+geom_histogram(aes(train$Item_Visibility), binwidth = 0.005, fill="blue")
p3 = ggplot(combi)+geom_histogram(aes(train$Item_MRP), binwidth = 1, fill="blue")
plot_grid(p1, p2, p3, nrow = 1)


#categorical Independent variables
ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count=n())) + 
geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")

combi$Item_Fat_Content[combi$Item_Fat_Content=="LF"]="Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content=="low fat"]="Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content=="reg"]="Regular"
ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count=n())) + 
  geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")


#treat other categorical variables
p4 = ggplot(combi %>% group_by(Item_Type) %>% summarise(Count=n())) + 
  geom_bar(aes(Item_Type, Count), stat = "identity", fill = "coral1") +
  xlab("") + geom_label(aes(Item_Type, Count, label=Count), vjust=0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + ggtitle("Item_type")

p5 = ggplot(combi %>% group_by(Outlet_Identifier) %>% summarise(Count=n())) + 
  geom_bar(aes(Outlet_Identifier, Count), stat = "identity", fill = "coral1") +
  xlab("") + geom_label(aes(Outlet_Identifier, Count, label=Count), vjust=0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

p6 = ggplot(combi %>% group_by(Outlet_Size) %>% summarise(Count=n())) + 
  geom_bar(aes(Outlet_Size, Count), stat = "identity", fill = "coral1") +
  xlab("") + geom_label(aes(Outlet_Size, Count, label=Count), vjust=0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

second_row = plot_grid(p5, p6, nrow=1)
plot_grid(p4, second_row, ncol = 1)

#plot for Outlet_Establishment_Year
p7 = ggplot(combi %>% group_by(Outlet_Establishment_Year) %>% summarise(Count=n())) + 
  geom_bar(aes(factor(Outlet_Establishment_Year), Count), stat = "identity", fill = "coral1") +
  xlab("Outlet_Establishment_Year") + geom_label(aes(factor(Outlet_Establishment_Year), Count, label=Count), vjust=0.5) +
  theme(axis.text.x = element_text(size=8.5))

#Plot for Outlet_Type 
p8 = ggplot(combi %>% group_by(Outlet_Type ) %>% summarise(Count=n())) + 
  geom_bar(aes(Outlet_Type, Count), stat = "identity", fill = "coral1") +
  xlab("Outlet_Type ") + geom_label(aes(Outlet_Type , Count, label=Count), vjust=0.5) +
  theme(axis.text.x = element_text(size=8.5))

plot_grid(p7, p8, ncol=2)

#Bivariate Plots
#Target Variables v/s independent numeric variable
train = combi[1:nrow(train)] # extracting data of Train from combi
p9 = ggplot(train) + geom_point(aes(Item_Weight,Item_Outlet_Sales), colour = 'violet', alpha=0.3) +
  theme(axis.title = element_text(size=8.5))

p10 = ggplot(train) + geom_point(aes(Item_Visibility,Item_Outlet_Sales), colour = 'violet', alpha=0.3) +
  theme(axis.title = element_text(size=8.5))

p11 = ggplot(train) + geom_point(aes(Item_MRP,Item_Outlet_Sales), colour = 'violet', alpha=0.3) +
  theme(axis.title = element_text(size=8.5))

second_row_2 = plot_grid(p10, p11, ncol=2)
plot_grid(p9, second_row_2, ncol=2)

# Target Variables v/s independent categorical variables

p12 = ggplot(train) + geom_violin(aes(Item_Type,Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle=45, hjust=1), axis.text = element_text(size=6), axis.title = element_text(size=8.5))

p13 = ggplot(train) + geom_violin(aes(Item_Fat_Content,Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle=45, hjust=1), axis.text = element_text(size=6),
        axis.title = element_text(size=8.5))

p14 = ggplot(train) + geom_violin(aes(Outlet_Identifier, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle=45, hjust=1), axis.text = element_text(size=6),
        axis.title = element_text(size=8.5))

second_row_3 = plot_grid(p13, p14, ncol=2)
plot_grid(p12, second_row_3, ncol=1)
 #######
ggplot(train) + geom_violin(aes(Outlet_Size,Item_Outlet_Sales), fill = "magenta")

p15 = ggplot(train) + geom_violin(aes(Outlet_Location_Type ,Item_Outlet_Sales), fill = "magenta")
p16 = ggplot(train) + geom_violin(aes(Outlet_Type,Item_Outlet_Sales), fill = "magenta")

plot_grid(p15, p16, ncol=1)

#Missing Value treatment
sum(is.na(combi))
sum(is.na(combi$Item_Outlet_Sales))

missing_index = which(is.na(combi$Item_Weight))
for (i in missing_index) {item = combi$Item_Identifier[i]
combi$Item_Weight[i] = mean(combi$Item_Weight[combi$Item_Identifier==item], na.rm=T)
  }

sum(is.na(combi$Item_Weight))

#Replacing 0's in Item_visibility column
zero_index = which(combi$Item_Visibility == 0)
for (i in zero_index) {item = combi$Item_Identifier[i]
combi$Item_Visibility[i] = mean(combi$Item_Visibility[combi$Item_Identifier==item], na.rm=T)
}

ggplot(combi)+geom_histogram(aes(Item_Visibility), bin = 100)

glimpse(combi$Item_Type)
perishable = c("Breads", "Breakfast", "Dairy", "Meat", "Fruits and Vegetables", "Seafood")

non_perishable = c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", "Health and Hygiene", "Households", "Soft Drinks")

#Create a new feature Item_Type_New
combi[, Item_Type_New := ifelse(Item_Type %in% perishable, "perishable", 
                                ifelse(Item_Type %in% non_perishable, "non_perishable", "not_sure"))]

table(combi$Item_Type, substr(combi$Item_Identifier,1,2))

combi[,Item_Category := substr(combi$Item_Identifier,1,2)]
combi$Item_Fat_Content[combi$Item_Category=='NC'] == 'Non-Edible'
# Year of operations for outlet
combi[, Outlet_Years := 2013 - Outlet_Establishment_Year]
combi$Outlet_Establishment_Year = as.factor(combi$Outlet_Establishment_Year)

#Price per unit weight
combi[, Price_Per_Unit_Weight := Item_MRP/Item_Weight]

# creating new MRP variable Item_MRP_Clusters
combi[, Item_MRP_Clusters := ifelse(Item_MRP < 69 , "1st",
      ifelse(Item_MRP >= 69 & Item_MRP < 136, "2nd",
      ifelse(Item_MRP>=136 & Item_MRP<203, "3rd", "4th")))]

combi[, Outlet_Size_num := ifelse(Outlet_Size=="Small", 0,
                                  ifelse(Outlet_Size=="Medium",1,2))]

combi[, Outlet_Location_Type_num := ifelse(Outlet_Location_Type=="Tier 3", 0,
                                  ifelse(Outlet_Location_Type=="Tier 2",1,2))]
#Removing categorical variable after label encoding
combi[, c("Outlet_Size", "Outlet_Location_Type") := NULL]

#One hot Encoding with categorical variables
ohe = dummyVars("~.", data = combi[, -c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")],
                fullRank = T)
ohe_df = data.table(predict(ohe,combi[, -c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")]))
combi = cbind(combi[, "Item_Identifier"], ohe_df)

#Data Skewness Treatment
combi[, Item_Visibility := log(Item_Visibility + 1)]
combi[, Price_Per_Unit_Weight := log(Price_Per_Unit_Weight + 1)]

#Scaling Numeric Predictor which is required for linear regression

num_vars = which(sapply(combi, is.numeric))
num_vars_names = names(num_vars)
combi_numeric = combi[, setdiff(num_vars_names, "Item_outlet_Sales"), with = F]
prep_num = preProcess(combi_numeric, method=c("center", "scale"))
combi_numeric_norms = predict(prep_num, combi_numeric)

combi[, setdiff(num_vars_names, "Item_outlet_Sales"):= NULL]
combi = cbind(combi, combi_numeric_norms)

# Splitting the combined data in to train and test data
train = combi[1:nrow(train)]
test = combi[(nrow(train)+1):nrow(combi)]
test[, Item_Outlet_Sales := NULL]

cor_train = cor(train[, -c("Item_Identifier")])
corr_plot = corrplot(cor_train, method = "pie", type = "lower", tl.cex = 0.9)

#Modeling
linear_reg_model = lm(Item_Outlet_Sales~., data = train[, -c("Item_Identifier")])
submission$Item_Outlet_Sales = predict(linear_reg_model, test[, -c("Item_Identifier")])
write.csv(submission, "Linear_reg_submit.csv", row.names = F)

summary(linear_reg_model)

#LASSO Regression
set.seed(1235)
my_control = trainControl(method = "CV", number = 5)
Grid = expand.grid(alpha=1, lambda=seq(0.001, 0.1, by=0.0002))
lasso_linear_reg_mod = train(x=train[, -c("Item_Identifier", "Item_Outlet_Sales")],
                             y = train$Item_Outlet_Sales, method='glmnet', trControl=my_control, tuneGrid=Grid)
lasso_linear_reg_mod

#Ridge Regression
set.seed(1236)
my_control = trainControl(method = "CV", number = 5)
Grid = expand.grid(alpha=0, lambda=seq(0.001, 0.1, by=0.0002))
ridge_linear_reg_mod = train(x=train[, -c("Item_Identifier", "Item_Outlet_Sales")],
                             y = train$Item_Outlet_Sales, method='glmnet', trControl=my_control, tuneGrid=Grid)

ridge_linear_reg_mod

#RandomForest
set.seed(1237)
my_control = trainControl(method = "CV", number = 5)
tgrid = expand.grid(.mtry=c(3:10),
                    .splitrule="variance",
                    .min.node.size=c(10,15,20))

rf_mod = train(x=train[, -c("Item_Identifier", "Item_Outlet_Sales")],
               y = train$Item_Outlet_Sales, method='ranger', trControl=my_control,
               tuneGrid=tgrid, num.trees=400, importance="permutation")


plot(rf_mod)
plot(varImp(rf_mod))


