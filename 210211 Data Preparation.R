#------------------------------------------------------------------------------------------------------------
# V1.1
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
library(factoextra)

library(rsample)
library(ggrepel)
library(ggpubr)
library(class)
library(caret)


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
ggplot(Long, aes(x = jitter(Score), y = Deelmeting, fill = Y)) + geom_boxplot(alpha=.2) + labs(x = "Rapportcijfer") + theme_bw() + theme(legend.position = "none")


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
Df <- merge(Df, Df_Onderhoud[,-c(2,3,4)], by="Rec_ID") #duurzaamheid, beschikbaarheid en betaalbaarheid ontbreken nog 


#----------
#Prepare Df
#----------

#Check Duplicates
table(duplicated(Df)) #merge created duplicates.
Df <- unique(Df) #Keep unique

#---------------
#Analyse Missing
#---------------

Miss <- Df[,-c(27,34:39,113:116)] # >0.5 missing
names(Miss) <- c(1:length(names(Miss))) #give numbers as column names for plot readability
names(Miss)

Missing_plot <- aggr(Miss, col=c('navyblue','yellow'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(Miss), cex.axis=.5,
                    gap=3, ylab=c("Missing data","Pattern"))
Missing_plot #What to do?

Df <- Df[,-c(27,34:39,113:116)] # Keep <0.5 missing

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
Miss <- Model_Df2[] 
names(Miss) <- c(1:length(names(Miss))) #give numbers as column names for plot readability


Missing_plot <- aggr(Miss, col=c('navyblue','yellow'),
                     numbers=TRUE, sortVars=TRUE,
                     labels=names(Miss), cex.axis=.5,
                     gap=3, ylab=c("Missing data","Pattern"))
Missing_plot #What to do?
Missing_plot

names(Model_Df2)[c(9,15,21,22,23,24,25)] #> 50% missing

Model_Df2 <- Model_Df2[,-c(9,15,21,22,23,24,25)] #Do more? ##########################################
Model_Df3 <- na.exclude(Model_Df2) # Te kort door de bocht ##########################################

#Impute missing 
#imp <- mice(Model_Df, m=1, maxit = 50, method = 'pmm', seed = 500)
#summary(imp)
#imp$imp$X_VH #Imputed values
#Model_Df <- complete(imp)
#Model_Df[-c(which(complete.cases(Model_Df$X_VH))),]

#----------
# Fit model
#----------

# Remove Huurdersooordeel Predictor Variables
names(Model_Df3[c(2:22,72,73)])
Model_Df4 <- Model_Df3[,-c(2:22,72,73)] 

#Fit Random Forest
set.seed(0)
rf2 <- randomForest(
  Y ~ .,
  data=Model_Df4, importance=TRUE
)
rf2
View(rf2$confusion)
varImpPlot(rf2, n.var = 10, type=1) # Roughly 60%
?varImpPlot


# Check misclassified
indices <- vector(mode = "logical", length = length(Model_Df4$Y))
for (i in 1:length(Model_Df4$Y)) { if (Model_Df4$Y[i] == rf2$predicted[i]) { indices[i] <- TRUE } 
  else indices[i] <- FALSE
}
Check_False <- Model_Df4[which(!indices),]
Check_False$Predicted <- rf2$predicted[which(!indices)] 
View(Check_False[,c("Y","Predicted")]) # Welke?

#---------------------------------
# Principal Component Analysis 
#--------------------------------

# Df 108 variabelen
# Exploratie onderliggende principale componenenten (directions where there is the most variance)

# Prepare 
Df2019 <- Df %>% dplyr::filter(Jaar == "2019") #work with 2019 data
row.names(Df2019) <- Df2019$Corporatie
Df2019 <- Df2019[complete.cases(Df2019),] # only complete cases
nums <- unlist(lapply(Df2019, is.numeric))  
Df2019 <- Df2019[ ,nums] #Only numeric vars. 90 variables remaining
Df2019 <- Df2019[,-c(1,27,62,63,64,65,67)] # remove near zero variance vars

# PCA
df.pca <- prcomp(Df2019, center = TRUE,scale. = TRUE) #scale en center
summary(df.pca)
fviz_eig(df.pca) #Scree plot. Ca 3 PCA

# Graph of individuals. Individuals with a similar profile are grouped together.
fviz_pca_ind(df.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#Graph of variables. Positive correlated variables point to the same side of the plot. Negative correlated variables point to opposite sides of the graph.
fviz_pca_var(df.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


#Biplot of individuals and variables
fviz_pca_biplot(df.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969",  # Individuals color
                label = c("ind","quanti.sup")
)


#-----------------------------------------------
# Cluster Analyse ( k nearest n) segment analyse
# https://rpubs.com/ysittaa/cust_segmentation
#-----------------------------------------------
# Let op Factors als dummy
# Let op Schalen van Data!












