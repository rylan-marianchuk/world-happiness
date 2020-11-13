world.happiness <- read.csv("final.csv")

# ANOVA
table(world.happiness$region)

africa_happiness <- world.happiness$happiness.score[world.happiness$region == "Africa"]
asia_happiness <- world.happiness$happiness.score[world.happiness$region == "Asia"]
europe_happiness <- world.happiness$happiness.score[world.happiness$region == "Europe"]
namerica_happiness <- world.happiness$happiness.score[world.happiness$region == "North America"]
oceania_happiness <- world.happiness$happiness.score[world.happiness$region == "Oceania"]
samerica_happiness <- world.happiness$happiness.score[world.happiness$region == "South America"]

anova = data.frame("Happiness Score" = world.happiness$happiness.score, "Continents" = world.happiness$region)

summary(aov(Happiness.Score ~ Continents, data = anova))

fit <- lm(Happiness.Score ~ Continents, data = anova)
summary(fit)

summary(lm(Happiness.Score  ~ C(Continents,contr.treatment("Asia")), data=anova ) )
# TODO - change the reference factor
# Model diagnostics

#----
# MLR
  # Descriptive Stats and Scatterplot matrix:
  # TODO: 5 Number summaries of each predictor

#----
# Model Fitting
mlr.frame <- data.frame("GDP" = world.happiness$GDP, "social" = world.happiness$social, "health" = world.happiness$health,
                        "freedom" = world.happiness$freedom, "trust" = world.happiness$trust, "generosity" = world.happiness$generosity)

lm(GDP ~ . , data=mlr.frame)
full_fit <- lm(GDP ~ social + health + freedom + trust + generosity, data=mlr.frame)
summary(full_fit)

#Arriving at the same coefficients using matrix vector operations to confirm its correctness:
data_mat <- data.matrix(mlr.frame)
p <- 5
n <- nrow(data_mat)
design_mat <- cbind(rep(1,n),data_mat[,c("social", "health", "freedom", "trust", "generosity")])

y.vector <- as.vector(data_mat[,"GDP"])

betahat.vector <- solve(t(design_mat) %*% design_mat) %*% t(design_mat) %*% y.vector

e.residual <- y.vector - design_mat %*% betahat.vector
SSE <- t(e.residual) %*% e.residual
s.square <- SSE / (n-p-1)
s <- sqrt(s.square)

#----
# Model diagnostic
# Residual plots of each predictor
# TODO: fix lengths so can plot
plot(world.happiness$social, full_fit$residuals, xlab = "Social Contribution", ylab = "residuals")

