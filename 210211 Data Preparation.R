#------------------------------------------------------------------------------------------------------------
# Inspect Data
# M.Dekker
# Aedes Benchmark
# 210211

# https://glmnet.stanford.edu/articles/glmnet.html#multinomial-regression-family-multinomial-
#------------------------------------------------------------------------------------------------------------

library(ggforce)
library(ggplot2)
library(tidyr)
library(tibble)
library(dplyr)
library(forcats)
library(mice)
library(VIM)
library(MASS)
library(nnet)
library(randomForest)
library(car)
library(tree)
library(rpart)
library(rpart.plot)
library(xgboost)
library(e1071)

#-----
# Load 
#-----

setwd("~/Trivire/R/Git/AedesBencmark/AedesBenchmark/Data")

#Algemene kenmerken
Df_algemene_kenmerken <- read.csv('Algemene Kenmerken  - Woningcorporaties.csv', sep = ';', stringsAsFactors = T)
Df_algemene_kenmerken[Df_algemene_kenmerken==""]<-NA #replace empty string with NA
Df_algemene_kenmerken <- droplevels.data.frame(Df_algemene_kenmerken) #Drop empty levels

# Huurderoordeel
Df_Huurderoordeel <- read.csv('Huurdersoordeel  - Woningcorporaties 2017_2020.csv', sep = ';', stringsAsFactors = T)
Df_Huurderoordeel[Df_Huurderoordeel==""]<-NA #replace empty string with NA
Df_Huurderoordeel <- droplevels.data.frame(Df_Huurderoordeel) #Drop empty levels
Df_Huurderoordeel[,c(6:10)] <- lapply(Df_Huurderoordeel[,c(6:10)], function(x) as.numeric(gsub(",", ".", x)))
Df_Huurderoordeel[,c(12:21)] <- lapply(Df_Huurderoordeel[,c(12:21)], function(x) as.numeric(gsub(",", ".", x)))
Df_Huurderoordeel[,c(23:39)] <- lapply(Df_Huurderoordeel[,c(23:39)], function(x) as.numeric(gsub(",", ".", x)))
# Df_Huurderoordeel$Letter.huurdersoordeel..Letters..3.klasse.. <- factor(Df_Huurderoordeel$Letter.huurdersoordeel..Letters..3.klasse.., levels=c("C", "B", "A"), ordered=TRUE) #Ordered Factor
# Df_Huurderoordeel$Deelletter.nieuwe.huurders..Letters..3.klasse.. <- factor(Df_Huurderoordeel$Deelletter.nieuwe.huurders..Letters..3.klasse.., levels=c("C", "B", "A"), ordered=TRUE) #Ordered Factor
# Df_Huurderoordeel$Deelletter.vertrokken.huurders..Letters..3.klasse.. <- factor(Df_Huurderoordeel$Deelletter.vertrokken.huurders..Letters..3.klasse.., levels=c("C", "B", "A"), ordered=TRUE) #Ordered Factor

#Bedrijfslasten
Df_Bedrijfslasten <- read.csv('Geharmoniseerde Beinvloedbare Bedrijfslasten (en ruisfactoren) - Woningcorporaties 2016_2019.csv', sep = ';', stringsAsFactors = T)
Df_Bedrijfslasten[Df_Bedrijfslasten==""]<-NA #replace empty string with NA
Df_Bedrijfslasten <- droplevels.data.frame(Df_Bedrijfslasten) #Drop empty levels


#Onderhoud
Df_Onderhoud <- read.csv('Onderhoud en Verbetering  - Woningcorporaties 2016_2019.csv', sep = ';', stringsAsFactors = T)
Df_Onderhoud[Df_Onderhoud==""]<-NA #replace empty string with NA
Df_Onderhoud <- droplevels.data.frame(Df_Onderhoud) #Drop empty levels
Df_Onderhoud$Referentiewaarde.technische.woningkwaliteit..score. <- as.numeric(gsub(",", ".", gsub("\\.", "",Df_Onderhoud$Referentiewaarde.technische.woningkwaliteit..score.))) # rapportcijfer als numeriek
Df_Onderhoud$Referentiewaarde.ervaren.woningkwaliteit..score. <- as.numeric(gsub(",", ".", gsub("\\.", "",Df_Onderhoud$Referentiewaarde.ervaren.woningkwaliteit..score.))) #rapportcijfer als numeriek
Df_Onderhoud$Ervaren.woningkwaliteit..score. <- as.numeric(gsub(",", ".", gsub("\\.", "",Df_Onderhoud$Ervaren.woningkwaliteit..score.))) #rapportcijfer als numeriek

#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
# Prepare Model Matrix
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------

#Prepare model matrix
Df_Huurderoordeel_jaar <- Df_Huurderoordeel %>% filter(Jaar == 2019) #Change for different year
prop.table(table(Df_Huurderoordeel_jaar$Letter.huurdersoordeel..Letters..3.klasse..)) #size per class
names(Df_Huurderoordeel_jaar[c(5,6,12,18)]) # Predictors & Repsonse variables
Model_Df <- Df_Huurderoordeel_jaar[,c(5,6,12,18)]
names(Model_Df) <- c("Y","X_NH","X_R","X_VH") #Rename for readability 
row.names(Model_Df) <- Df_Huurderoordeel_jaar$Corporatie #Corporatienaam Rownames 

#Deal with missing
table(complete.cases(Model_Df)) # 269 #126 incomplete. Meeste missen alle waarden. Sommige enkele en hebben desondanks wel een score.
Model_Df <- Model_Df[-c(which(is.na(Model_Df$Y))),] #Use only cases with Y score. Drop empty rows 
which(is.na(Model_Df$X_VH)) # 62 en 172 missing predictor
Model_Df[-c(which(complete.cases(Model_Df$X_VH))),]

#Impute missing 
imp <- mice(Model_Df, m=1, maxit = 50, method = 'pmm', seed = 500)
summary(imp)
imp$imp$X_VH #Imputed values
Model_Df <- complete(imp)
Model_Df[-c(which(complete.cases(Model_Df$X_VH))),]

# Scatterplot Correlation Matrix
scatterplotMatrix(Model_Df[,c(2:4)]) # Slightly positively correlated
cor(Model_Df[,c(2:4)])
?scatterplotMatrix

#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
# Fit the Conceptual Benchmark model Huurdersoordeel (should be 100% correct)
# Random Forest
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------

# Fit tree model
set.seed(100481)
Tree <- rpart(Y~., data=Model_Df)
rpart.plot(Tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)
summary(Tree)


# Confusion Matrix
fit.val <- predict(Tree, type = "class")
cm <- table(actual = Model_Df$Y, fitted = fit.val)
cm

# Fit Random Forest
rf <- randomForest(
  Y ~ .,
  data=Model_Df, importance=TRUE
)
rf #5 Misclassified. Not exactly 100%. How?  

View(rf$confusion)
varImpPlot(rf, type=1)
varImpPlot(rf, type=2)

# Check misclassified
indices <- vector(mode = "logical", length = length(Model_Df$Y))
for (i in 1:length(Model_Df$Y)) { if (Model_Df$Y[i] == rf$predicted[i]) { indices[i] <- TRUE } 
  else indices[i] <- FALSE
}
Check_False <- Model_Df[which(!indices),]
Check_False$Predicted <- rf$predicted[which(!indices)] #Vertaling van Deelletter naar eindscore?
View(Check_False)

# Fit SVM
SupVecMach = svm(Y ~ .,
                 data = Model_Df)
summary(SupVecMach)

# Confusion Matrix
fit.val <- predict(SupVecMach, type = "class")
cm <- table(actual = Model_Df$Y, fitted = fit.val)
cm #Worse than RF

#-----
#Plots
#-----

ggplot(Model_Df, aes(x = Model_Df$X_R, fill=Model_Df$Y)) + geom_density(alpha=0.6) + labs(title="Huurdersoordeel", x="Oordeel Reparatie") + theme_bw()

# Histogram with density plot
ggplot(Model_Df, aes(x=X_R, fill = Y)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2) +
  geom_vline(data=Model_Df, aes(xintercept=mean(Model_Df$X_R), color="red"), linetype="dashed") +
  labs(title = "Huurderoordeel", legend="Score",x="Oordeel Reparatie") + 
  theme_bw() 

# Boxplot
Long <- gather(Model_Df,Deelmeting,Score,X_NH:X_VH, factor_key=TRUE)
ggplot(Long, aes(x = jitter(Score), y = Deelmeting, fill = Y)) + geom_boxplot(alpha=.2) + theme_bw()


#-----------------------------------------------------------
# Prepare again with different Letter deelscore as predictor
#-----------------------------------------------------------

#Prepare model matrix
#Df_Huurderoordeel_jaar <- Df_Huurderoordeel %>% filter(Jaar == 2020) #Change for different year
#prop.table(table(Df_Huurderoordeel_jaar$Letter.huurdersoordeel..Letters..3.klasse..)) #size per class
#names(Df_Huurderoordeel_jaar[c(5,6,12,18)]) # Predictors & Repsonse variables
#Model_Df <- Df_Huurderoordeel_jaar[,c(5,11,22,40)]
#names(Model_Df) <- c("Y","X_NH","X_VH","X_R") #Rename for readability 
#row.names(Model_Df) <- Df_Huurderoordeel_jaar$Corporatie #Corporatienaam Rownames 

#Deal with missing
#table(complete.cases(Model_Df)) # 269 #126 incomplete. Meeste missen alle waarden. Sommige enkele en hebben desondanks wel een score.
#Model_Df <- Model_Df[-c(which(is.na(Model_Df$Y))),] #Use only cases with Y score. Drop empty rows 
#which(is.na(Model_Df$X_VH)) # 62 en 172 missing predictor
#Model_Df[-c(which(complete.cases(Model_Df$X_VH))),]

#Impute missing 
#imp <- mice(Model_Df, m=1, maxit = 50, method = 'pmm', seed = 500)
#summary(imp)
#imp$imp$X_VH #Imputed values
#Model_Df <- complete(imp)
#Model_Df[-c(which(complete.cases(Model_Df$X_VH))),]

#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
# Fit the Conceptual Benchmark model Huurdersoordeel (should be 100% correct)
# Random Forest
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------

# Fit tree model
#set.seed(100481)
#Tree <- rpart(Y~., data=Model_Df)
#rpart.plot(Tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)
#summary(Tree)

# Confusion Matrix
#fit.val <- predict(Tree, type = "class")
#cm <- table(actual = Model_Df$Y, fitted = fit.val)
#cm

#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------
#Merge on Year and L-nummer Large DF new model_Df without previous huurdersoordeel predictors vars
#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------

#Merge
Df <- merge(Df_Huurderoordeel, Df_algemene_kenmerken[,-1], by="L.Nummer")
Df <- merge(Df, Df_Bedrijfslasten[,-c(2,3,4)], by="Rec_ID") #merge without existing columns
Df <- merge(Df, Df_Onderhoud[c(2,3,4)], by="Rec_ID") #duurzaamheid, beschikbaarheid en betaalbaarheid ontbreken nog 

#----------
#Prepare Df
#----------

#Check Duplicates
table(duplicated(Df)) #zero duplicates

# Remove Huurdersooordeel Predictor Variables
names(Df)[c(6,12,18,11,22,40)]
Df <- Df[,-c(6,12,18,11,22,40)]


#Analyse Missing
Miss <- Df
names(Miss) <- c(1:length(names(Miss))) #give numers als colum names for plot readability
names(Miss)

Missing_plot <- aggr(Miss, col=c('navyblue','yellow'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(Miss), cex.axis=.5,
                    gap=3, ylab=c("Missing data","Pattern"))
Missing_plot #What to do?

#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
# Prepare Model Matrix
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------

#Prepare model matrix
Model_Df2 <- Df %>% filter(Jaar == 2019) #Change for different year
names(Model_Df2)[5] <- "Y"
row.names(Model_Df2) <- Model_Df2$Corporatie #Corporatienaam Rownames 
Model_Df2 <- Model_Df2[,-c(1:4)]

#Deal with missing
Miss <- Model_Df2[,-18]
names(Miss) <- c(1:length(names(Miss))) #give numers als colum names for plot readability
names(Miss)

Missing_plot <- aggr(Miss, col=c('navyblue','yellow'),
                     numbers=TRUE, sortVars=TRUE,
                     labels=names(Miss), cex.axis=.5,
                     gap=3, ylab=c("Missing data","Pattern"))
Missing_plot #What to do?
names(Model_Df2)[18] # 100% missing
names(Model_Df2)[c(27,24,28,29,25,26)] #> 50% missing

Model_Df2 <- Model_Df2[,-18] #Do more? ##########################################
Model_Df3 <- na.exclude(Model_Df2) # Te kort door de bocht

#Impute missing 
#imp <- mice(Model_Df, m=1, maxit = 50, method = 'pmm', seed = 500)
#summary(imp)
#imp$imp$X_VH #Imputed values
#Model_Df <- complete(imp)
#Model_Df[-c(which(complete.cases(Model_Df$X_VH))),]

#-----------
# Fit model
#----------

#Fit Random Forest

rf2 <- randomForest(
  Y ~ .,
  data=Model_Df3, importance=TRUE
)
rf2

View(rf2$confusion)
varImpPlot(rf2, type=1)
varImpPlot(rf2, type=2)

Model_Df4 <- Model_Df3[,-c(2:29)]  #Afgeleide huurdersoordeel predictors. Verwijderen

#Fit Random Forest

rf3 <- randomForest(
  Y ~ .,
  data=Model_Df4, importance=TRUE
)
rf3

View(rf$confusion)
varImpPlot(rf3, type=1)

# Check misclassified
indices <- vector(mode = "logical", length = length(Model_Df3$Y))
for (i in 1:length(Model_Df3$Y)) { if (Model_Df3$Y[i] == rf3$predicted[i]) { indices[i] <- TRUE } 
  else indices[i] <- FALSE
}
Check_False <- Model_Df3[which(!indices),]
Check_False$Predicted <- rf3$predicted[which(!indices)] 
View(Check_False[,c("Y","Predicted")]) # Welke?


#---------------------------------
# Cluster Analyse ( k nearest n)
#--------------------------------

# Let op Factors als dummy
# Let op Schalen van Data!
# K nearest neighbours








