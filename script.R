world.happiness <- read.csv("final.csv")

# ANOVA
table(world.happiness$region)

# Distributions of each region
africa_happiness <- world.happiness$happiness.score[world.happiness$region == "Africa"]
asia_happiness <- world.happiness$happiness.score[world.happiness$region == "Asia"]
europe_happiness <- world.happiness$happiness.score[world.happiness$region == "Europe"]
namerica_happiness <- world.happiness$happiness.score[world.happiness$region == "North America"]
oceania_happiness <- world.happiness$happiness.score[world.happiness$region == "Oceania"]
samerica_happiness <- world.happiness$happiness.score[world.happiness$region == "South America"]



# Create a dataframe with only two columns: mean happiness and region (continent)
anova = data.frame("Happiness Score" = world.happiness$happiness.score, "Continents" = world.happiness$region)

# Apply the aov
summary(aov(Happiness.Score ~ Continents, data = anova))

fit <- lm(Happiness.Score ~ Continents, data = anova)
summary(fit)

# TODO - change the reference factor to another group
# NOTE this line was not working
summary(lm(Happiness.Score  ~ C(Continents,contr.treatment("Asia")), data=anova ) )

#----
# Model diagnostics
# Checking the normality of our groups for variance null hypothesis:
library("car")
par(mfrow=c(2,3))
qqPlot(europe_happiness)
qqPlot(namerica_happiness)
qqPlot(africa_happiness)
qqPlot(asia_happiness)
qqPlot(oceania_happiness)
qqPlot(samerica_happiness)
dev.off()

# Apply bartlett and levene test to see whether pooled variance is an acceptable assumption
bartlett.test(Happiness.Score ~ Continents, data = anova)
leveneTest(Happiness.Score ~ Continents, data = anova)
# Checking to confirm the variances are actually different.
max(var(africa_happiness), var(asia_happiness), var(europe_happiness), var(namerica_happiness), var(oceania_happiness), var(samerica_happiness)) / min(var(africa_happiness), var(asia_happiness), var(europe_happiness), var(namerica_happiness), var(oceania_happiness), var(samerica_happiness))


# %%% ------------------------------- %%% #
# MLR
  # Descriptive Stats and Scatterplot matrix:
  # TODO: 5 Number summaries of each predictor


#----
# Model Fitting
mlr.frame <- data.frame("GDP" = world.happiness$GDP, "social" = world.happiness$social, "health" = world.happiness$health,
                        "freedom" = world.happiness$freedom, "trust" = world.happiness$trust, "generosity" = world.happiness$generosity)

#lm(GDP ~ . , data=mlr.frame)
full_fit <- lm(GDP ~ social + health + freedom + trust + generosity, data=mlr.frame)
summary(full_fit)

#Arriving at the same coefficients using matrix vector operations to confirm its correctness:
data_mat <- data.matrix(mlr.frame)
p <- 5
n <- nrow(data_mat)
design_mat <- cbind(rep(1,n),data_mat[,c("social", "health", "freedom", "trust", "generosity")])

y.vector <- as.vector(data_mat[,"GDP"])

# Using r matrix operations to solve the beta hat vector
betahat.vector <- solve(t(design_mat) %*% design_mat) %*% t(design_mat) %*% y.vector

# Finding SSE and residual vector
e.residual <- y.vector - design_mat %*% betahat.vector
SSE <- t(e.residual) %*% e.residual
s.square <- SSE / (n-p-1)
s <- sqrt(s.square)


#----
# Hypothesis tests
# H_0: beta_social  - beta_trust = 0
a = c(0, 1, 0, 0, -1, 0)
theta_hat <-  t(a) %*% betahat.vector
t.calc = theta_hat / (s * sqrt(t(a) %*% solve( t(design_mat) %*% design_mat) %*% a))
p.value = pt(t.calc, df = n-p-1)
# REJECT yes, a unit of trust and social contribute a different amount to GDP

# Does social + trust equal 1
# H_0: beta_social  + beta_trust = 1
a2 = c(0, 1, 0, 0, 1, 0)
theta_hat2 <-  t(a2) %*% betahat.vector
t.calc2 = (theta_hat2 - 1) / (s * sqrt(t(a2) %*% solve( t(design_mat) %*% design_mat) %*% a2))
p.value2 = pt(t.calc2, df = n-p-1)
# FAIL to reject

# Is the coefficient on health greater than 1?


# Is the coefficient on trust greater than 0.5



#----
# Model diagnostic
# Residual plots of each predictor
plot(world.happiness$social, e.residual, xlab = "Social Contribution", ylab = "residuals")
abline(h=0)
plot(world.happiness$health, e.residual, xlab = "Health Contribution", ylab = "residuals")
abline(h=0)
plot(world.happiness$freedom, e.residual, xlab = "Freedom Contribution", ylab = "residuals")
abline(h=0)
plot(world.happiness$trust, e.residual, xlab = "Trust Contribution", ylab = "residuals")
abline(h=0)
plot(world.happiness$generosity, e.residual, xlab = "Generosity Contribution", ylab = "residuals")
abline(h=0)

# 6 Diagnostic plots - BEFORE variable selection
# Normality of residuals - looks good!
qqPlot(e.residual)

par(mfrow = c(2,3))
plot(full_fit , which=1:6)


dev.off()

# Variable selection and model refinement
  # Using the criterion of AIC
step(full_fit, data = world.happiness, direction = "backward",k=2)
step(full_fit, data = world.happiness, direction = "both",k=2)

step(full_fit, data = world.happiness, direction = "backward",k=log(nrow(world.happiness)))
step(full_fit, data = world.happiness, direction = "both",k=log(nrow(world.happiness)))
# To do forward, creating null model
null_fit <- lm(GDP ~ 1, data = world.happiness)
step(null_fit, scope = list(lower=null_fit, upper=full_fit), data = world.happiness, direction = "forward", k = 2)


# 2 candidate models:
# Model 1 - social + health + trust + generosity
cand1 <- lm(GDP ~ social + health + trust + generosity, data = world.happiness)
summary(cand1)
#Cp Statistic

# Using the matrix operation to get SSE
p <- 4
n <- nrow(data_mat)
design_mat <- cbind(rep(1,n),data_mat[,c("social", "health", "trust", "generosity")])

y.vector <- as.vector(data_mat[,"GDP"])
beta_hat.cand1 <- solve( t(design_mat) %*% design_mat) %*% t(design_mat) %*% y.vector
e.cand1 <- y.vector - design_mat %*% beta_hat.cand1

SSE = t(e.cand1) %*% e.cand1
c(SSE, sum(cand1$residuals^2))
# 0.2346 is the residual standard error of full_fit
Cp <- SSE / (0.2346)^2 - (n-2*p)


# Model 2 - social + health + trust
cand2 <- lm(GDP ~ social + health + trust, data = world.happiness)
summary(cand2)
# Cp statistic
design_mat <- cbind(rep(1,n),data_mat[,c("social", "health", "trust")])

y.vector <- as.vector(data_mat[,"GDP"])
beta_hat.cand2 <- solve( t(design_mat) %*% design_mat) %*% t(design_mat) %*% y.vector
e.cand2 <- y.vector - design_mat %*% beta_hat.cand2

SSE = t(e.cand2) %*% e.cand2
c(SSE, sum(cand2$residuals^2))
p <- 3
Cp <- SSE / (0.2346)^2 - (n-2*p)


# 6 Diagnostic plots - AFTER variable selection algorithm. Leverage and cooks distance
plot(cand2 , which=1:6)

# Which model to choose?
