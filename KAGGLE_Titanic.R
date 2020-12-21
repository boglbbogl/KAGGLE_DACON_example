library(dplyr)
library(graphics)
library(ggplot2)
library(gridExtra)
library(randomForest)

train <- read.csv("train.csv", stringsAsFactors = F, header = T)
test <- read.csv("test.csv", stringsAsFactors = F, header = T)
# titanic <- bind_rows(train, test) # rbind
# titanic <- titanic %>% mutate(Survived=factor(Survived), Age=factor(Age), Ticket=factor(Ticket), Embarked=factor(Embarked),
                   # Sex=factor(Sex), Name=factor(Name), Pclass=factor(Pclass, ordered=T)) #ordered:순위
# EDA 시각화
str(train)
str(test)
summary(train, test)
titanic_fare <- ggplot(train, aes(x=Fare, y=..density..), xlab="Fare Distribution") +
  geom_density(fill="blue", alpha=0.2)
titanic_age <- ggplot(train, aes(x=Age, y=..density..), xlab="Age Distribution") +
  geom_density(fill="blue", alpha=0.2)
grid.arrange(titanic_fare, titanic_age, nrow=1, ncol=2)
par(mfrow=c(1,2))
mosaicplot(table(ifelse(train$Survived==1, "Survived", "Dead"), train$Sex), main="", cex=1.2, color = T)
mosaicplot(table(ifelse(train$Survived==1, "Survived", "Dead"), train$Pclass), main="", cex=1.2, color =T)
boxplot(Age~Survived, train, xlab="Survival", ylab="Age", cex=1.2)
plot(Age~jitter(Survived), train, cex=1.2, xlab="Survival")
ggplot(train, aes(Age, log(Fare), color=factor(Survived), shape=factor(Sex))) + geom_point() + geom_jitter()

# EDA RandomForest
titanic.train <- train
titanic.test <- test
titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet <- FALSE
names(titanic.train)
names(titanic.test)
test$Survived <- NA
titanic.full <- rbind(titanic.train, titanic.test)
table(titanic.full$IsTrainSet)

table(titanic.full$Embarked)
titanic.full[titanic.full$Embarked=='', "Embarked"] <- 'S'

table(is.na(titanic.full$Age))
age.median <- median(titanic.full$Age, na.rm=T)
titanc.full[is.na(titanc.full$Age), "Age"] <- age.median

fare.median <- median(titanic.full$Fare, na.rm = TRUE)
titanic.full[is.na(titanc.full$Fare), "Fare"] <- fare.median

titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)

titanic.train <- titanic.full[titanic.full$IsTrainSet==T,]
titanic.test <- titanic.full[titanic.full$IsTrainSet==F,]
titanic.train$Survived <- as.factor(titanic.train$Survived)

# Modeling
survied.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survied.formula <- as.formula(survied.equation)

titanic.model <- randomForest(formula=survied.formula, data = titanic.train, ntree=500, mtry=3, nodesize=0.01*nrow(titanic.test))

Survived <- predict(titanic.model, newdata=titanic.test)

PassengerId <- titanic.test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived

write.csv(output.df, file="Kaggle_submission.csv", row.names = F)

table(output.df$Survived)
