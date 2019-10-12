#read in data, ensure working directory is set beforehand
college = read.csv("College.csv", header = T)

#set the rownames as the first column of the data
#which provides the names of the colleges
rownames(college) = college[,1]

#get rid of the first column; it's already stored in rownames
college = college[,-1]

#find a numerical summary of each variable in the data set
summary(college)

#create a scatterplot matrix of the first ten columns of the data set with pairs,
pairs(college[, 1:10])

#create side-by-side boxplots of Outstate versus Private
plot(as.factor(college$Private), college$Outstate, xlab = "Private",
     ylab = "Out of State Tuition")

#we create a new variable called Elite
#this variable divides the colleges into two groups based on whether 
#the proportion of students coming from the top 10% of their high school 
#classes exceeds 50%
Elite = rep("No", nrow(college))
Elite[college$Top10perc > 50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)

#find the number of elite colleges (78)
summary(college$Elite)

#find the out-of-state tuition given whether the college is elite
plot(college$Elite, college$Outstate, xlab = "Elite", ylab = "Out of State Tuition")

#looking at histograms of Outstate, S.F.Ratio, Grad.Rate, and Terminal
par(mfrow = c(2,2))
hist(college$Outstate, xlab = "Out of State Tuition", breaks = 20, main = "")
hist(college$S.F.Ratio, xlab = "Student-Faculty Ratio", breaks = 25, main = "")
hist(college$Grad.Rate, xlab = "Graduation Rate", breaks = 25, main = "")
hist(college$Terminal, 
     xlab = "Percent of Faculty with Terminal Degrees", breaks = 25, main = "")

#look at the Student-Faculty ratio by Elite
par(mfrow = c(1,1))
plot(college$Elite, college$S.F.Ratio, xlab = "Elite", ylab = "Student-Faculty Ratio")
#student-faculty ratio generally lower for Elite schools

#look at the graduation rate by Elite
plot(college$Elite, college$Grad.Rate, xlab = "Elite", ylab = "Graduation Rate")
#graduation rate typically higher for Elite colleges

#look at the Terminal rate by Elite
plot(college$Elite, college$Terminal, 
     xlab = "Elite", ylab = "Percent of Faculty with Terminal Degrees")
#elite schools have more faculty with terminal degrees

#divide data into training and test sets
#drop Top10perc from training set, because this is used to find Elite
#drop Top25perc, because this is correlated with Top10perc
set.seed(2)
train.rows = sample(nrow(college), floor(nrow(college)*0.75))
train = college[train.rows, -c(5,6)]
test = college[-train.rows,]

#fit a model with all predictors
model1 = glm(Elite ~ ., data = train, family = "binomial")
#algorithm does not converge

#get rid of predictors that are highly correlated with another

#first compute the correlation matrix of the predictors
cor.mat = cor(train[,-c(1,17)])

#load the caret library
library(caret)

#use findCorrelation to determine variables to drop
#drops the variable with the highest average absolute correlation 
#when two variables are highly correlated
vars.to.drop = findCorrelation(cor.mat, cutoff = 0.75, names = T, exact = F)

#drop these variables from train
train.dropped = train[, !(colnames(train) %in% vars.to.drop)]

#now refit the model
model2 = glm(Elite ~ ., data = train.dropped, family = "binomial")

#get a summary
summary(model2)
contrasts(train.dropped$Elite)
#model is predicting Yes

#predict with logistic regression model
preds.log = predict(model2, test)
preds.label = ifelse(preds.log > 0.5, "Yes", "No")
mean(preds.label == test$Elite)
#about a 98% accuracy rate 

#try to fit a boosted model
library(gbm)
set.seed(1)
boost.college = gbm(Elite ~ ., data = train, distribution = "multinomial", 
                    n.trees = 5000, interaction.depth = 4)
summary(boost.college)

#predict whether or not the test set colleges are Elite
test.preds = predict.gbm(boost.college, test, n.trees = 5000, type = "response")
labels = colnames(test.preds)[apply(test.preds, 1, which.max)]
mean(labels == test$Elite)
#97% accuracy on test set, less than logistic model