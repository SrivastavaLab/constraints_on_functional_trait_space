# rm(list=ls())
require(ade4)
require(geometry)


# Diaz et al 2016 Nature
# ftp://pbil.univ-lyon1.fr/pub/datasets/dray/Diaz_Nature/

## selection of a proportion of data
subselect.data <- function(df, percent = 0.95){
  df0 <- scale(df, scale = FALSE)
  di <- rowSums(df0^2)
  thresh <- quantile(di, percent)
  idx <- which(di>thresh)
  if(length(idx) > 0)
    df <- df[-idx,]
  return(df)
}

## permutation models
nullmodel <- function(tab, model){
  if(model == 1){
    res <- apply(tab, 2, function(x) runif(nrow(tab), min = min(x), max = max(x)))
  }
  if(model == 2){
    res <- scale(matrix(rnorm(ncol(tab) * nrow(tab)), nrow(tab), ncol(tab)))
  }
  if(model == 3){
    res <- apply(tab,2,sample)
  }
  if(model == 4){
    corM <- cor(tab)
    res <- scale(scale(matrix(rnorm(ncol(tab) * nrow(tab)), nrow(tab), ncol(tab)))%*%chol(corM))
  }
  return(res)
}

# load the full dataset
axes.raw <- read.table("Current Results - 0.7.7/RES_pca_individuals_0.7.7_ranks.txt", h = TRUE, row.names = 1)

axes <- axes.raw[,1:4]
head(axes)
dim(axes)

axes<-unique(axes) # Select unique scores # 143
dim(axes)

axes <- scale(axes, center = TRUE, scale = TRUE) # scale for mean zero and standard deviation one
apply(axes, 2 , mean)
apply(axes, 2 , sd)
cor(axes)

axes95 <- subselect.data(axes, 0.95) # select 0.95 of dataset 
dim(axes95)

# Compute observed convex hull
blob95 <- convhulln(axes95,"FA")
obs.vol95 <- blob95$vol
obs.vol95

# function for apply the null models "runs" times
run<-function(x, runs, model){
  res<-matrix(NA, runs,1)
  for(i in 1:runs){
    axes_null<-subselect.data(nullmodel(x, model), 0.95) # select 0.95 
    res[i,] <- convhulln(axes_null,"FA")$vol
  }
return(res)
}

# run the null models  
res_vol_model_1 <- run(axes, 999, 1)
res_vol_model_2 <- run(axes, 999, 2)
res_vol_model_3 <- run(axes, 999, 3)

# Compute p value manually
(sum(res_vol_model_1<=obs.vol95)+1)/(999+1)
(sum(res_vol_model_2<=obs.vol95)+1)/(999+1)
(sum(res_vol_model_3<=obs.vol95)+1)/(999+1)

# Compute p value automatically
res1 <- as.randtest(obs=obs.vol95,sim=res_vol_model_1[,1], alter="less")
res2 <- as.randtest(obs=obs.vol95,sim=res_vol_model_2[,1], alter="less")
res3 <- as.randtest(obs=obs.vol95,sim=res_vol_model_3[,1], alter="less")
res1
res2
res3

#  Ratio between observed volume and null models
Ratio1<-100 - mean(obs.vol95/res_vol_model_1[,1]) * 100
Ratio2<-100 - mean(obs.vol95/res_vol_model_2[,1]) * 100
Ratio3<-100 - mean(obs.vol95/res_vol_model_3[,1]) * 100
Ratio1
Ratio2
Ratio3
# save.image("workspace_PCA_nullmodels_0.7.7")

Res<-matrix(NA,3,6)
Res[1,]<-cbind(res1$obs, res1$expvar[1], res1$expvar[2], res1$expvar[3],  res1$pvalue, Ratio1)
Res[2,]<-cbind(res2$obs, res2$expvar[1], res2$expvar[2], res2$expvar[3],  res3$pvalue, Ratio2)
Res[3,]<-cbind(res3$obs, res3$expvar[1], res3$expvar[2], res3$expvar[3],  res3$pvalue, Ratio3)
colnames(Res)<-c("VolObs", "Std.Obs", "Expectation", "Variance", "pvalue", "Ratio.Obs.Null")
rownames(Res)<-c("Model 1", "Model 2", "Model 3")
Res

setwd("./Current Results - 0.7.7")
# write.table(Res,"results_convhulln_unique_scores_0.7.7_ranks.txt")
# write.table(Res,"results_convhulln_852_scores_0.7.7_ranks.txt")