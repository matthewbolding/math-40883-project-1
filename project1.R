data <- read.table("am_com_survey.txt", header = T, sep = ",")

pairs(data)
round(cor(data), 2)

# gender & hrs_work
female.hrs_work <- data[which(data$gender == 1), ]$hrs_work
male.hrs_work <- data[which(data$gender == 0), ]$hrs_work
boxplot(female.hrs_work, male.hrs_work,
        ylab = "Hours Worked",
        xlab = "Gender",
        names = c("Female", "Male"),
        main = "Gender and Hours Worked")

# age & married
unmarried <- data[which(data$married == 1), ]$age
married <- data[which(data$married == 0), ]$age
boxplot(married, unmarried,
        ylab = "Age",
        xlab = "Marital Status",
        names = c("Married", "Unmarried"),
        main = "Age and Martial Status")

# hrs_work & log_inc
plot(data$hrs_work, data$log_inc,
     main = 'Hours Worked vs. Log. Income',
     xlab = 'Hours Worked', ylab = 'Log. Income',
     xlim = range(data$hrs_work), ylim = range(data$log_inc))

# age & log_inc
plot(data$age, data$log_inc,
     main = 'Age vs. Log. Income',
     xlab = 'Age', ylab = 'Log. Income',
     xlim = range(data$age), ylim = range(data$log_inc))

# edu & log_inc
college <- data[which(data$edu == 0), ]$log_inc
high_school <- data[which(data$edu == 1), ]$log_inc
boxplot(college, high_school,
        ylab = "Log. Income",
        xlab = "Education Level",
        names = c("College", "High School"),
        main = "Education Level and Income")


# gender & log_inc
female.log_inc <- data[which(data$gender == 1), ]$log_inc
male.log_inc <- data[which(data$gender == 0), ]$log_inc
boxplot(female.log_inc, male.log_inc,
        ylab = "Log. Income",
        xlab = "Gender",
        names = c("Female", "Male"),
        main = "Gender and Log. Income")

female.unmarried <- nrow(data[which(data$married == 0 & data$gender == 1), ])
female.married <- nrow(data[which(data$married == 1 & data$gender == 1), ])
male.unmarried <- nrow(data[which(data$married == 0 & data$gender == 0), ])
male.married <- nrow(data[which(data$married == 1 & data$gender == 0), ])

male <- nrow(data[which(data$gender == 0), ])
female <- nrow(data[which(data$gender == 1), ])

X <- c(female.unmarried, female.married, male.unmarried, male.married)
L <- c("female.unmarried", "female.married", "male.unmarried", "male.married")

pie(X, L)

# discuss gender and married, but don't show a graph of it, please for the love of all that's good

# Age and Education
model1 <- lm(log_inc ~ age + edu + age:edu, data = data)
model1 <- lm(log_inc ~ age + edu, data = data)
summary(model1)

# Hours Worked and Gender
model2 <- lm(log_inc ~ hrs_work + gender + hrs_work:gender, data = data)
summary(model2)

# Age and Married
model3 <- lm(log_inc ~ age + married + age:married, data = data)
summary(model3)

##################

model.back.sel <- lm(log_inc ~ hrs_work + race + age + gender + time_to_work + married + edu, data = survey)
p_values <- summary(model.back.sel)$coefficients[ ,4]
p_values[p_values == max(p_values)]

model.back.sel <- lm(log_inc ~ hrs_work + race + age + gender + married + edu, data = survey)

model.back.sel <- lm(log_inc ~ hrs_work + age + gender + married + edu, data = survey)

summary(model.back.sel)

round(coef(model.back.sel), 4)

model.back.sel$coefficients

summary(model1)
summary(model2)
summary(model3)



## colinarity is what the cor matrix shows for between predictos

model.1 <- lm(log_inc ~ hrs_work + gender + hrs_work:gender, data = data)
model.2 <- lm(log_inc ~ age + edu + age:edu, data = data)
model.2 <- lm(log_inc ~ age + edu, data = data)
model.3 <- lm(log_inc ~ age + married + age:married, data = data)
model.3 <- lm(log_inc ~ age + married, data = data)

summary(model.1)$adj.r.squared
summary(model.2)$adj.r.squared
summary(model.3)$adj.r.squared
summary(model.back.sel)$adj.r.squared


library(MASS)
stu.res <- studres(model.back.sel)
plot(stu.res, ylab = "Student Residuals", main = "Student Residuals vs. Index")
abline(3, 0, col="red")
abline(0, 0, col="blue")
abline(-3, 0, col="red")

hat.vals <- hatvalues(model.back.sel)
plot(hat.vals, ylab = "Hat Values", main = "Hat Values vs. Index")
p <- length(data) - 1
n <- nrow(data)
abline((p+1)/n, 0, col="blue")

outlier <- which(abs(stu.res) > 3)
leverage <- which(hat.vals > 0.03)
Reduce(intersect,list(outlier, leverage))