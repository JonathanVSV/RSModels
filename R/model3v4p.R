#' @title Modeling function 3v4p
#'
#' @description This function constructs every possible linear model with three independent variables
#' (y = mx + ny + oz + b) for nx number of dependent variables.
#'
#' @param model.data=data.frame  Data.frame that contains both the dependent and independent variables
#'
#' @param ny=number Number of dependent variables to be tested.
#'
#' @param nx=number Number of independent variables to be tested. Do not confuse number of available independent
#' variables with number of independent variables OF THE MODEL.
#'
#' @param CV=boolean T if a cross-validation should be performed, F if not.
#'
#' @param CV_n=number Number of observations to be left out in the cross-validation process. For example
#' CV = T & CV_n = 1 will perform a 1-leave-out-cross-validation, while the same command with CV_n = 2, will
#' performa a 2-leave-out-cross-validation
#'
#' @param r2random=boolean T if an R^2 maximum random distribution should be computed with the data. Calculating
#' this random distribution enables a comparison of the observed R^2 values of the best models with a completely
#' random scenario. This distribution shows if the goodness-of-fit values obtained in the models correspond to a
#' significntly higher value than the expected at random (>= value of percentile 95) or not.
#'
#' @param runs=number Number indicating the number of runs to perform the goodness-of-fit random distribution.
#'
#' @examples model3v4p(df, ny=9, nx=16)
#'
#' @export model3v4p
#'

model3v4p<-function(model.data, ny, nx, CV=F, CV_n=1, r2random=F, runs=1000)
{

  #Set model parameters
  p <- 4
  n <- nrow(model.data)

  #Possible combinations for independent variables
  combs.var <- combn(nx, 3)
  #Possible combinations for cross validation procedure
  combs.data <- combn(n, CV_n)

  timer <- round(proc.time()[3])

  #-------------------Descriptive models------------------------------------------------------
  results <- array(dim = c(ny,choose(nx, 3), 10))
  results.ord <- array(dim = c(ny,choose(nx, 3), 10))
  for(i in 1:ny)
  {
    y <- i + 1
    for(j in 1:choose(nx,3))
    {
      x1 <- combs.var[1,j]+ny+1
      x2 <- combs.var[2,j]+ny+1
      x3 <- combs.var[3,j]+ny+1
      model <- lm(model.data[, y] ~ model.data[, x1] + model.data[, x2] + model.data[, x3])
      coef <- model$coef
      r2 <- summary(model)$r.squared
      AICc <- AIC(model) + (2 * p * (p - 1)) / (n - p - 1)
      F.p.value.x1 <- summary(aov(model))[[1]][, 5][1]
      F.p.value.x2 <- summary(aov(model))[[1]][, 5][2]
      F.p.value.x3 <- summary(aov(model))[[1]][, 5][3]
      results[i, j, ] <- c(paste(names(model.data)[y], "vs", names(model.data)[x1], "&", names(model.data)[x2],
                              "&",names(model.data)[x3]), r2, AICc, F.p.value.x1, F.p.value.x2, F.p.value.x3,coef[1],
                        coef[2], coef[3], coef[4])
    }
    results.ord[i, , ]<-results[i, order(as.numeric(results[i,,2]),decreasing=T),]
    write.table(rbind(results.ord[i, , ]),paste("Results-var3par4", names(model.data)[i+1], "", ".xls", sep = ""),
                sep = "\t",row.names = F,col.names = c("Y vs X1 & X2 & X3", "r2", "AICc", "F.p.value.x1", "F.p.value.x2",
                                                       "F.p.value.x3", "Intercept", "Slope x1 (coef2)", "Slope x2 (coef3)",
                                                       "Slope x3 (coef4)"))
  }

  #----------------------------------------Models with highest R2----------------------------------------------
  mejores<-matrix(nrow = ny,ncol = 13)#En results[,,2] están los R^2
  colnames(mejores)<-c("Atributo", "Var imagen 1", "Var imagen 2", "Var imagen 3", "R2", "F. p-value 1",
                       "F. p-value 2", "F. p-value 3", "AIC", "Intercept", "Slope x1", "Slope x2",
                       "Slope x3")
  for (i in 1:ny)#Cada [i,] es el conjunto de modelos posibles para cada atributo de la comunidad
    {
      mejormo <- which(as.numeric(results[, , 2][i, ]) == max(as.numeric(results[, , 2][i, ])))
      mejores[i, 1] <- results[, , 1][i,mejormo]
      mejores[i, 5] <- max(as.numeric(results[, , 2][i, ]))
      mejores[i, 6] <- results[, , 4][i, mejormo]
      mejores[i, 7] <- results[, , 5][i, mejormo]
      mejores[i, 8] <- results[, , 6][i, mejormo]
      mejores[i, 9] <- results[, , 3][i, mejormo]
      mejores[i, 10] <- results[, , 7][i, mejormo]
      mejores[i, 11] <- results[, , 8][i, mejormo]
      mejores[i, 12] <- results[, , 9][i, mejormo]
      mejores[i, 13] <- results[, , 10][i, mejormo]
    }
  
  for (i in 1:ny)#Cada [i,] es el conjunto de modelos posibles para cada atributo de la comunidad
  {
    colsa <- unlist(strsplit(mejores[i,1], " vs "))
    mejores[i, 1] <- colsa[1]
    colsa2 <- unlist(strsplit(colsa[2], " & "))
    mejores[i, 2] <- colsa2[1]
    mejores[i, 3] <- colsa2[2]
    mejores[i, 4] <- colsa2[3]
  }

  write.table(as.table(mejores), paste("Best mod R2-var3par4",".xls",sep = ""), sep = "\t", row.names = F,
              col.names = colnames(mejores))

  #-------------------------------------Cross-validation-------------------------------
  if(CV == T)
  {
    combs.data.CV <- combs.data

    pred <- vector(length = CV_n)
    CV.CV <- vector(length = choose(n, CV_n))
    r2.CV <- vector(length = choose(n, CV_n))
    r2.data.vs.pred <- vector(length = choose(n, CV_n))
    time <- vector(length = choose(n, CV_n))
    means <- array(data = 0, dim = c(ny, choose(nx, 3), 8))
    means.ord <- array(data = 0, dim = c(ny,choose(nx, 3), 8))

    coefs <- matrix(nrow = choose(n, CV_n), ncol = p)
    mean.coefs <- vector(length = p)

    for(i in 1:ny)
    {
      y <- i + 1
      mean.y <- mean(model.data[, y])
      mean.sstt <- sum((model.data[, y] - mean.y)^2) / n
      for(j in 1:choose(nx, 3))
      {
        x1 <- combs.var[1, j] + ny + 1
        x2 <- combs.var[2, j] + ny + 1
        x3 <- combs.var[3, j] + ny + 1
        for(k in 1:choose(n, CV_n))
        {
          time1 <- proc.time()[3]
          print(paste("vary = ", i, ", varx1 = ", combs.var[1,j], ", varx2 = ", combs.var[2,j], ", varx3 = ",
                      combs.var[3, j], ", comb = ", k, sep = ""))
          validation.data <- model.data[combs.data.CV[, k], ]
          calibration.data <- model.data[- combs.data.CV[, k], ]
          model <- lm(calibration.data[, y] ~ calibration.data[, x1] + calibration.data[, x2] + calibration.data[, x3])
          coef <- model$coef
          for(l in 1:CV_n)
          {
            pred[l] <- coef[1] + coef[2] * validation.data[l, x1] + coef[3] * validation.data[l, x2] + coef[4] * validation.data[l, x3]
          }
          mean.y.validation <- mean(validation.data[, y])
          msse.validation <- sum((validation.data[, y] - pred)^2) / CV_n
          CV.CV[k] <- sqrt(msse.validation) / mean.y.validation
          r2.CV[k] <- 1 - msse.validation / mean.sstt
          r2.data.vs.pred[k] <- (cor(validation.data[, y], pred,method = "pearson"))^2
          coefs[k,] <- coef
          time2 <- proc.time()[3]
          time[k] <- time2 - time1
        }
        mean.time <- mean(time)
        print(paste("mean time = ",mean.time,sep = ""))
        mean.CV.CV <- mean(CV.CV)
        mean.r2.CV <- mean(r2.CV)
        mean.coefs[1] <- mean(coefs[, 1])
        mean.coefs[2] <- mean(coefs[, 2])
        mean.coefs[3] <- mean(coefs[, 3])
        mean.coefs[4] <- mean(coefs[, 4])
        mean.r2.data.vs.pred <- mean(r2.data.vs.pred)
        means[i, j, ] <- c(paste(names(model.data)[y], "vs", names(model.data)[x1], "&", names(model.data)[x2], "&",
                              names(model.data)[x3]), mean.r2.CV, mean.r2.data.vs.pred, mean.CV.CV, mean.coefs[1],
                        mean.coefs[2], mean.coefs[3], mean.coefs[4])
      }
      means.ord[i, , ] <- means[i, order(as.numeric(means[i, , 2]), decreasing = T), ]
      write.table(as.table(means.ord[i, , ]), paste("Results-var3par4-CV", names(model.data)[i + 1], "", timer, ".xls",
                                                 sep = ""), sep = "\t", row.names = F, col.names = c("Y vs X1 & X2 & X3",
                                                                                                  "mean r2.CV",
                                                                                                  "mean r2 data vs pred",
                                                                                                  "mean CV.CV",
                                                                                                  "mean Interc",
                                                                                                  "mean Slope",
                                                                                                  "mean Slope 2",
                                                                                                  "mean Slope 3"))
    }
  }else{
    print("No Cross Validation performed")
  }

  #------------------------- Random highest R2 distribution -------------------------------
  if(r2random == T)
  {
    #Base de datos donde se meten results
    model.data.permut <- model.data
    r2.max.permut <- matrix(nrow = ny, ncol = runs)
    r2.model.pvalue <- matrix(nrow = ny, ncol = choose(nx, 3))
    r2 <- array(dim = c(ny,choose(nx, 3), runs))
    for(k in 1:runs)
    {
      print(paste("run = ",k,sep = ""))
      for(i in 2:(nx + ny + 1))
        model.data.permut[, i] <- gtools::permute(model.data[, i])
      for(i in 1:ny)
      {
        for(j in 1:choose(nx, 3))
        {
          y <- i + 1
          x1 <- combs.var[1, j] + ny + 1
          x2 <- combs.var[2, j] + ny + 1
          x3 <- combs.var[3, j] + ny + 1
          model <- lm(model.data.permut[, y] ~ model.data.permut[, x1] + model.data.permut[, x2] + model.data.permut[, x3])
          r2[i, j, k] <- summary(model)$r.squared
        }
        r2.max.permut[i, k] <- max(r2[i, , k])
      }
    }
    for(i in 1:ny)
      r2.max.permut[i, ] <- sort(r2.max.permut[i, ], decreasing = T)
    
    colnames <- vector(length = (choose(nx, 3)))
    for(i in 1:choose(nx, 3))
    {
      x1 <- combs.var[1,i] + ny + 1
      x2 <- combs.var[2,i] + ny + 1
      x3 <- combs.var[3,i] + ny + 1
      colnames[i] <- paste(names(model.data)[x1], "&", names(model.data)[x2], "&", names(model.data)[x3])
    }

    #Pa escribir chido las tablas, arreglar el desface de columnas
    rownames(r2.max.permut) <- names(model.data)[2:(ny + 1)]
    colnames(r2.max.permut) <- seq(1:runs)
    write.table(as.table(t(r2.max.permut)), paste("Results-var3par4-r2maxpermut", ".xls",sep = ""), sep = "\t",
                col.names = NA, row.names = T)
    rownames(r2.model.pvalue) <- names(model.data)[2:(ny + 1)]
    colnames(r2.model.pvalue) <- colnames
    write.table(as.table(r2.model.pvalue), paste("Results-var3par4-r2modelpvalue", ".xls", sep = ""), sep = "\t",
                col.names = NA, row.names = T)

  }else{
    print("No R2 distribution at random performed")
  }
  print(paste0("Process finished. Files can be found in the following directory: ", getwd()))
}
