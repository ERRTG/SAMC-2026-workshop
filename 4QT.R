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


beta cough  beta time  beta food beta voice 
 0.3083363  1.0135196 -0.6222906 -0.6995652 

Andersen LR-test: 
LR-value: 6.092 
Chi-square df: 3 
p-value:  0.107 

# three covariates (exogenous variables)
exolist <- c("agegr","gender","diag")

# define analysis
analysis <- gRm(
  data = four_QT, 
  items = itlist, 
  exogenous = exolist, 
  id = "ID",
  score_cuts = c(0,1,4)
)
summary(analysis, which = "data")

DIGRAM analysis

data
n_rows n_items n_exogenous                    items              exogenous score_groups
    73       4           3 cough, time, food, voice    agegr, gender, diag      0, 1, 4

# define model (simplest model: Rasch model)
model1 <- gllrm(analysis)
summary(model1, which = "model")

# fit the model
fit1 <- fit(model1)
summary(fit1, which = "fit")
summary(fit1, which = "parameters")


DIGRAM Rasch fit

fit
model_type log_likelihood n_parameters likelihood_n converged iterations      delta
     rasch        63.5422            3           55      TRUE          6 1.4948e-05

DIGRAM Rasch fit

parameters
item   location  midpoint   target info_at_target info_per_step
cough -0.308337 -0.308337 -0.30832           0.25            NA
time  -1.013520 -1.013520 -1.01352           0.25            NA
food   0.622291  0.622291  0.62228           0.25            NA
voice  0.699565  0.699565  0.69956           0.25            NA


# test global model fit
gfit <- global_homogeneity(fit1, groups = c(0,2,4))
summary(gfit, which = "tests")

# test item fit
ifit <- item_fit(fit1)
ifittable <- summary(ifit, which = "tests")
ifittable

iarm::item_restscore(Rasch)

# test DIF
dif <- dif(fit1)
summary(dif, which = "selected")

# test LD
LD <- local_dependence(fit1)
summary(LD, which = "selected")

# define graphical Rasch model

model2 <- gllrm(analysis, ld = ~ food:voice, dif = ~ food:diag)
summary(model2, which = "model")

# fit the model
fit2 <- fit(model2)
summary(fit2, which = "fit")
summary(fit2, which = "parameters")


# test global model fit
gfit2 <- global_homogeneity(fit2, groups = c(0,1,4))
summary(gfit2, which = "tests")

# test item fit
ifit2 <- item_fit(fit2)
ifittable2 <- summary(ifit2, which = "tests")
ifittable2

iarm::item_restscore(Rasch)

# test DIF

dif2 <- dif(fit2)
summary(dif2, which = "tests")

# test LD

LD2 <- local_dependence(fit2)
summary(LD2, which = "selected")
summary(LD2, which = "tests")

graph <- gRm::model_graph(fit2)
plot(graph, 
     x_spacing = 3,
     vertex_size = 3.5)


