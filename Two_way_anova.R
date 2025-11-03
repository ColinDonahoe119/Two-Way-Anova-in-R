# Two way Anova
install.packages("emmeans")
library(readxl)

data_path <- "C:/Users/colin/Downloads/DSS 445/twoway_anova_salary.xlsx"
major.gender.salary <- read_excel(data_path)

major.gender.salary$Major <- factor(major.gender.salary$Major)
major.gender.salary$Gender <- factor(major.gender.salary$Gender)

table(major.gender.salary$Major, major.gender.salary$Gender)

boxplot(Salary ~ Gender, data = major.gender.salary,
        main = "Salary by Gender", ylab = "Salary(USD)")


boxplot(Salary ~ Major, data = major.gender.salary,
        main = "Salary by Major", ylab = "Salary(USD)")

with(major.gender.salary,
     interaction.plot(x.factor = Major, trace.factor = Gender,
                      response = Salary, fun = mean,
                      ylab= "Mean Salary (USD)", xlab = "Major",
                      main = "Interaction Plot: Major x Gender"))

# Two Way anova with Interaction
# When Interaction (Major:Gender) is significant, individual factor
# p-values don't matter. When Interation isn't significant, then you
# look at individual factors
fit_int <- aov(Salary~ Major * Gender, data = major.gender.salary)
summary(fit_int)

# Two way anova with no Interaction
fit_add <- aov(Salary ~ Major + Gender, data = major.gender.salary)
summary(fit_add)

library(emmeans)

emmeans(fit_int, pairwise ~ Major | Gender)

emmeans(fit_int, pairwise ~ Gender| Major)

# Check that distribution is relatively normal
shapiro.test(residuals(fit_int))

library(car)
leveneTest(Salary ~ Major * Gender, data = major.gender.salary)

par(mfrow = c(1,2))
plot(fit_int, which = 1)
plot(fit_int, which = 2)
par(mfrow = c(1,1))
