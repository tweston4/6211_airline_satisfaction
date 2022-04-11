library(VIM)
library(mice)
library(tidyverse)
library(caret)
library(factoextra)
library(randomForest)
library(ggplot2)
library(ggcorrplot)
library(caret)
library(pROC)
library(rpart)
library(rpart.plot)
library(dplyr)
set.seed(100)
setwd("C:/Users/Travis Weston/OneDrive - University of North Carolina at Charlotte/Workspace/R/DSBA-6211/airline/6211_airline_satisfaction")
filename <- "data/passenger_satisfaction.csv"
df <- read_csv(filename)

summary(df)
str(df)
head(df)

# Convert all character columns to factor
df[sapply(df,is.character)] <- lapply(df[sapply(df,is.character)],as.factor)
summary(df)

# Satisfactory columns 8 - 21
df[8:21] <- lapply(df[8:21],as.factor)
summary(df)


# Handle Missing Data
ar_delay_avg <- mean(df$arrival_delay, na.rm = TRUE)

md.pattern(df, rotate.names = TRUE)

aggr_plot <- aggr(df, col=c("forestgreen","darkgoldenrod1"), numbers=TRUE, sortVars=TRUE, labels=names(df), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

mice_res <- mice(df, method = 'rf', maxit= 10)

summary(mice_res$data)
imp_df <- complete(mice_res,1)
summary(imp_df)

xyplot(mice_res,arrival_delay ~ departure_delay,pch=18,cex=1)

# Data Plots 
ggplot(imp_df, aes(x=reorder(satisfaction, satisfaction, function(x)-length(x)))) +
  geom_bar(fill=c("forestgreen","darkgoldenrod1")) +  labs(x='Satisfaction')

# Stacked
ggplot(imp_df, aes(fill=cleanliness, x=satisfaction)) + 
  geom_bar(position="stack")

ggplot(imp_df, aes(fill=gender, x=satisfaction)) + 
  geom_bar(position="stack")

ggplot(imp_df, aes(fill=class, x=satisfaction)) + 
  geom_bar(position="stack")


ggplot(imp_df, aes(fill=checkin_service, x=satisfaction)) + 
  geom_bar(position="stack")

ggplot(imp_df, aes(x=satisfaction, y=departure_delay)) +  geom_boxplot(fill=c("forestgreen","darkgoldenrod1"))

ggplot(imp_df, aes(x=satisfaction, y=flight_distance)) +  geom_boxplot(fill=c("forestgreen","darkgoldenrod1"))


chisq.test(imp_df$travel_type,imp_df$satisfaction)

numeric_df <- dplyr::select_if(imp_df, is.numeric)

r <- cor(numeric_df,use="complete.obs")

ggcorrplot(r)

# Models


# Dimension Reduction 
df.train.numeric <- imp_df[22:23]
pca <- prcomp(df.train.numeric,center=TRUE,scale.=TRUE)
print(pca)
plot(pca)
summary(pca)

fviz_eig(pca)
fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

pred_pca <- predict(pca,newdata = df.train.numeric)

df.pca <- cbind.data.frame(imp_df[,c(2:21,24)],pred_pca[,1])
colnames(df.pca)[22] <- "delay_pca"
# Standard Partition with imputed data
trainIndex <-
  createDataPartition(imp_df$satisfaction,
                      p = .7,
                      list = FALSE,
                      times = 1)

df.train <- imp_df[trainIndex, ]
df.valid <- imp_df[-trainIndex, ]
# PCA Partition with Imputed data
trainIndex <-
  createDataPartition(df.pca$satisfaction,
                      p = .7,
                      list = FALSE,
                      times = 1)

pca.train <- df.pca[trainIndex, ]
pca.valid <- df.pca[-trainIndex, ]

# Logistic Regression
base.model <- train(satisfaction~. -id
                    ,data = df.train
                    ,method = "glm"
                    ,family = "binomial")
summary(base.model)
pca.base.model <- train(satisfaction~.
                        ,data = pca.train
                        ,method = "glm"
                        ,family = "binomial")
summary(pca.base.model)


# Score base model
