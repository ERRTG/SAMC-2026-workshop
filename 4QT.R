library(readxl)
library(gRm)

four_QT <- as.data.frame(read_excel("h:/GRM/examples/4QT/4Q_data excell Klargjort til RUMM.xlsx", col_names = FALSE))
names(four_QT) <- c("ID","agegr","gender","diag","cough","time","food","voice")

# variable names
names(four_QT)

# four items
itlist <- c("cough","time","food","voice")
RASCHplot::BARplot(four_QT[, itlist], freq = FALSE)

# Rasch model in eRm
items <- four_QT[, itlist]
Rasch <- eRm::RM(items[complete.cases(items),])
Rasch$betapar
eRm::LRtest(object = Rasch, splitcr = "mean")

# three covariates (exogenous variables)
exolist <- c("agegr","gender","diag")

# define analysis
analysis <- gRm(
  data = four_QT, 
  items = itlist, 
  exogenous = exolist, 
  id = "ID",
  groups = c(0,1,4)
)
summary(analysis, which = "data")

# define model (simplest model: Rasch model)
model1 <- gllrm(analysis)
summary(model1, which = "model")

# fit the model
fit1 <- fit(model1)

summary(fit1, which = "fit")
summary(fit1, which = "parameters")
# summary(fit1, which = "terms")

# test global model fit
gfit <- global_homogeneity(fit1)
summary(gfit, which = "tests")

# test item fit
ifit <- item_fit(fit1)
ifittable <- summary(ifit, which = "tests")


iarm::item_restscore(Rasch)



