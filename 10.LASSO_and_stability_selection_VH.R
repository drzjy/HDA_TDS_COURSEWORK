rm(list=ls())

#Sources:
#https://glmnet.stanford.edu/articles/glmnet.html
#CE Week 2 practical

library(tidyverse)
library(glmnet)

#load dataframe - RF imputed + 1:5 matched + hot encoded
df <- readRDS("RF_imputed_outliers_matched_1to5_Hotencoded_PRS_2903.rds")
#df_whole

#obtain 20% of dataset to work on (keep proportion of outcome)
#df_list <- createDataPartition(df_whole$final, p = 0.2,
#                          list = FALSE,
#                          times = 1)
#
#df <- df_whole[df_list,]

names(df)

#assign index name
rownames(df) <- df$Row.names
df$Row.names <- NULL
df$IDs <- NULL

#Y
Y <- df$final
#Controls = 0, Cases = 1
#Y <- ifelse(Y == 'controls', 0, 1)
#as.factor
Y <- as.character(Y)
Y <- as.numeric(Y)

#Assign X
X <- df
#Female = 0, Male = 1
X$sex <- ifelse(X$sex=='Female', 0, 1)

X$skin_colour <- ifelse(X$skin_colour=='Light', 0, 1)

#Remove Y from variables dataframe (X)
X$final <- NULL

#mutate_all as.numeric
X <- mutate_all(X, as.numeric)

#variable scaling
X <- mutate_all(X, scale)

#convert to matrix
X <- as.matrix(X)


t0 <- Sys.time()
#Lasso
model.lasso <- cv.glmnet(x=X, y=Y, alpha = 1, 
                         family = "binomial", 
                         type.measure = 'auc',
                         keep=TRUE) 
# type.measure = "auc"

t1 <- Sys.time()
print(t1 - t0)

png(file="RF_tuning_lambda.png",
    width=1000, height=1000)
plot(model.lasso, cex = 3)
dev.off()

#Spaghetti plot
#t0 <- Sys.time()
#model.lasso.path <- glmnet(x=X, y=Y, alpha=1)
#plot(model.lasso.path)
#t1 <- Sys.time()
#print(t1 - t0)



table(coef(model.lasso, s = "lambda.min")[-1] != 0)


betas = coef(model.lasso, s = "lambda.min")[-1]

png(file="RF_lasso_coef.png",
    width=1500, height=1000)
par(mar=c(20,5,2,2)+0.1)
names(betas) = rownames(coef(model.lasso, s = "lambda.min"))[-1]
plot(betas[betas != 0], type = "h", col = "navy", lwd = 3,
     xaxt = "n", xlab = "", ylab = expression(beta), cex.axis = 1, cex.lab = 2)
axis(side = 1, at = 1:sum(betas != 0), labels = names(betas)[betas !=0], las = 2)
abline(h = 0, lty = 2)
dev.off()


if(FALSE){
  #####
  #Anaconda glmnet package on HPC cannot run roc.glmnet() for some reason
  #Test roc.glmnet function copy and pasted above
  #####
  #ROC curves
  png(file="RF_auc_roc.png",
      width=1000, height=1000)
  rocs <- roc.glmnet(model.lasso$fit.preval, newy = Y)
  par(mar=c(5,5,5,5))
  best <- model.lasso$index["min",]
  plot(rocs[[best]], type = "l", cex = 3)
  invisible(sapply(rocs, lines, col="grey"))
  lines(rocs[[best]], lwd = 2,col = "red")
  
  dev.off()
  
}

#####################################
#Stability analysis
#######################################

t0 <- Sys.time()
#Stability analysis
LassoSub = function(k = 1, Xdata, Ydata, family = "binomial",
                    penalty.factor = NULL) {
  if (is.null(penalty.factor)) {
    penalty.factor = rep(1, ncol(Xdata))
  }
  set.seed(k)
  s = sample(nrow(Xdata), size = 0.8 * nrow(Xdata))
  Xsub = Xdata[s, ]
  Ysub = Ydata[s]
  model.sub = cv.glmnet(x = Xsub, y = Ysub, alpha = 1,
                        family = family, penalty.factor = penalty.factor)
  coef.sub = coef(model.sub, s = "lambda.min")[-1]
  return(coef.sub)
}
niter = 1000
lasso.stab = sapply(1:niter, FUN = LassoSub, Xdata = X,
                    Ydata = Y)


#Model size for each iteration
apply(lasso.stab, 2, FUN = function(x) {
  sum(x != 0)
})


#Proportion of selection of each variable can be computed as follows:
lasso.prop = apply(lasso.stab, 1, FUN = function(x) {
  sum(x != 0)/length(x)
})
names(lasso.prop) = colnames(X)



#Visualise coefs selected in stability analysis with barplot
png(file="RF_stability_analysis_niter1000_SCALED.png",
    width=2400, height=1600)

lasso.prop = sort(lasso.prop, decreasing = TRUE)
par(mar=c(2,5,2,2))
plot(lasso.prop[lasso.prop > 0.2], type = 'h', col = "navy",
     lwd = 3, xaxt = "n", xlab = "", ylab = 'Selection Proportion', cex.lab = 2,
     ylim = c(0, 1.4), las = 0,
     cex.axis = 3)
text(lasso.prop[lasso.prop > 0.2] + 0.07, labels = names(lasso.prop[lasso.prop >
                                                                      0.2]), pos = 4, srt = 90, cex = 2)

abline(h=0.8, lwd =1, lty =2, col = 'red')

t1 <- Sys.time()
print(t1-t0)

dev.off()

lasso.prop  #to look at selection proportions
write.csv(lasso.prop, 'lasso_prop.csv')
