survey <- read.table("am_com_survey.txt", header = T, sep = ",")
round(cor(survey), 2)

female.hrs_work <- survey[which(survey$gender == 1), ]$hrs_work
male.hrs_work <- survey[which(survey$gender == 0), ]$hrs_work
boxplot(female.hrs_work, male.hrs_work,
        ylab = "Hours Worked",
        xlab = "Gender",
        names = c("Female", "Male"),
        main = "Gender and Hours Worked")

unmarried <- survey[which(survey$married == 1), ]$age
married <- survey[which(survey$married == 0), ]$age
boxplot(married, unmarried,
        ylab = "Age",
        xlab = "Marital Status",
        names = c("Married", "Unmarried"),
        main = "Age and Martial Status")

# hrs_work & log_inc
plot(survey$hrs_work, survey$log_inc,
     main = 'Hours Worked vs. Log. Income',
     xlab = 'Hours Worked', ylab = 'Log. Income',
     xlim = range(survey$hrs_work), ylim = range(survey$log_inc))

# age & log_inc
plot(survey$age, survey$log_inc,
     main = 'Age vs. Log. Income',
     xlab = 'Age', ylab = 'Log. Income',
     xlim = range(survey$age), ylim = range(survey$log_inc))

# edu & log_inc
college <- survey[which(survey$edu == 0), ]$log_inc
high_school <- survey[which(survey$edu == 1), ]$log_inc
boxplot(college, high_school,
        ylab = "Log. Income",
        xlab = "Education Level",
        names = c("College", "High School"),
        main = "Education Level and Log. Income")

# gender & log_inc
female.log_inc <- survey[which(survey$gender == 1), ]$log_inc
male.log_inc <- survey[which(survey$gender == 0), ]$log_inc
boxplot(female.log_inc, male.log_inc,
        ylab = "Log. Income",
        xlab = "Gender",
        names = c("Female", "Male"),
        main = "Gender and Log. Income")

nrow(survey[which(survey$married == 0 & survey$gender == 1), ]) ## unmarried females
nrow(survey[which(survey$married == 1 & survey$gender == 1), ]) ## married females
nrow(survey[which(survey$married == 0 & survey$gender == 0), ]) ## unmarried males
nrow(survey[which(survey$married == 1 & survey$gender == 0), ]) ## married males

model.hrs_work.gender <- lm(log_inc ~ hrs_work + gender + hrs_work:gender, data = survey)
summary(model.hrs_work.gender)$coefficients[,4]

model.age.edu <- lm(log_inc ~ age + edu + age:edu, data = survey)
summary(model.age.edu)$coefficients[,4]
model.age.edu <- lm(log_inc ~ age + edu, data = survey)

model.age.married <- lm(log_inc ~ age + married + age:married, data = survey)
summary(model.age.married)$coefficients[,4]
model.age.married <- lm(log_inc ~ age + married, data = survey)


model.back.sel <- lm(log_inc ~ hrs_work + race + age + gender + time_to_work + 
                       married + edu, data = survey)
p_values <- summary(model.back.sel)$coefficients[ ,4]
p_values[p_values == max(p_values)]

model.back.sel <- lm(log_inc ~ hrs_work + race + age + gender + married + edu, data = survey)
p_values <- summary(model.back.sel)$coefficients[ ,4]
p_values[p_values == max(p_values)]

model.back.sel <- lm(log_inc ~ hrs_work + age + gender + married + edu, data = survey)
p_values <- summary(model.back.sel)$coefficients[ ,4]
p_values[p_values == max(p_values)]


round(coef(model.back.sel), 4)
round(coef(model.hrs_work.gender), 4)
round(coef(model.age.edu), 4)
round(coef(model.age.married), 4)

summary(model.hrs_work.gender)$adj.r.squared
summary(model.age.edu)$adj.r.squared
summary(model.age.married)$adj.r.squared
summary(model.back.sel)$adj.r.squared

library(MASS)
## Outliers
stu.res <- studres(model.back.sel)
plot(stu.res, ylab = "Student Residuals", main = "Student Residuals vs. Index (Outliers)")
abline(3, 0, col="red")
abline(0, 0, col="blue")
abline(-3, 0, col="red")

## Leverage
hat.vals <- hatvalues(model.back.sel)
p <- length(survey) - 1
n <- nrow(survey)
plot(hat.vals, ylab = "Hat Values", main = "Hat Values vs. Index (Leverage)")
abline((p + 1)/n, 0, col="blue")

outlier <- which(abs(stu.res) > 3)
leverage <- which(hat.vals > 0.03)
Reduce(intersect,list(outlier, leverage))
