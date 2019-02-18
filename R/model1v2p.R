#' @title Modeling function 1v2p
#'
#' @description This function constructs every possible linear model with one independent variable (y = mx^2 + b)
#' for nx number of dependent variables and testing every independent variable (nx).
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
#' @examples model1v2p(df,ny=9,nx=16)
#'
#' @export model1v2p

model1v2p<-function(model.data,ny,nx,CV=F,CV_n=1,r2random=F,runs=1000)
{
  #Set model parameters
  p = 2
  n = nrow(model.data)

  #Possible combinations for independent variables
  combs.var = combn(nx,1)
  #Possible combinations for cross validation procedure
  combs.data = combn(n,CV_n)

  timer = round(proc.time()[3])

  #------------------------------Descriptive models---------------------------------------
  results = array(dim = c(ny,choose(nx,1),6))
  results.ord = array(dim = c(ny,choose(nx,1),6))
  for(i in 1:ny)
  {
    y = i+1
    for(j in 1:choose(nx,1))
    {
      x1 = combs.var[1,j]+ny+1
      model = lm(model.data[,y]~model.data[,x1])
      coef = model$coef
      r2 = summary(model)$r.squared
      AICc = AIC(model)+(2*p*(p-1))/(n-p-1)
      F.p.value.x1 = summary(aov(model))[[1]][,5][1]

      results[i,j,] = c(paste(names(model.data)[y],"vs",names(model.data)[x1]),r2,AICc,F.p.value.x1,
                        coef[1],coef[2])
    }
    #Export results from higher to lower R2 values
    results.ord[i,,]<-results[i,order(as.numeric(results[i,,2]),decreasing=T),]
    write.table(as.table(results.ord[i,,]),paste("Results-var1par2",names(model.data)[i+1],"",".xls",sep = ""),
                sep = "\t",row.names = F,col.names = c("Y vs X1","r2","AICc","F.p.value.x1",
                                                       "Intercept (coef1)","Slope (coef2)"))
  }

  #-------------------------Models with highest R2--------------------------------------
  i<-1
  mejores=matrix(nrow=ny,ncol=7)
  colnames(mejores)<-c("Attribute","Var1","R2","F. p-value","AIC","Intercept","Slope")
  for (i in 1:ny)
  {
    mejormo<-which(as.numeric(results[,,2][i,])==max(as.numeric(results[,,2][i,])))
    mejores[i,1]<-results[,,1][i,mejormo]
    mejores[i,3]<-max(as.numeric(results[,,2][i,]))
    mejores[i,4]<-results[,,4][i,mejormo]
    mejores[i,5]<-results[,,3][i,mejormo]
    mejores[i,6]<-results[,,5][i,mejormo]
    mejores[i,7]<-results[,,6][i,mejormo]
  }

  for (i in 1:ny)
  {
    colsa<-unlist(strsplit(mejores[i,1], " vs "))
    mejores[i,1]<-colsa[1]
    mejores[i,2]<-colsa[2]
  }

  write.table(as.table(mejores),paste("Best mod R2-var1par2",".xls",sep = ""),sep = "\t",row.names = F,
              col.names = colnames(mejores))

  #-------------------------------------Models with lower AIC----------------------------------------
  aicmej = array(dim = c(ny,choose(nx,1),6))
  for(i in 1:ny)
  {
    rowaic<-which(as.numeric(results[i,,3])<=(min((as.numeric(results[i,,3]))+2)))
    aicmej<-results[i,rowaic,]
    if(length(rowaic)==1)
    {
      aicmejord<-t(aicmej)
    }	else
    {
      aicmejord<-aicmej[order(as.numeric(aicmej[,3])),]
    }
    write.table(aicmejord,paste("Best-mod-AIC-1var2par",names(model.data)[i+1],"",".xls",sep = ""),
                sep = "\t",row.names = F,col.names = c("Y vs X1","r2","AICc","F.p.value.x1",
                                                       "coef1","coef2"))
  }


  #--------------------------------------- Cross-validation----------------------------------------
  if(CV==T)
  {
    print("Cross validation under process")
    combs.data.CV = combs.data

    #Bases de resultados
    pred = vector(length = CV_n)
    CV.CV = vector(length = choose(n,CV_n))
    r2.CV = vector(length = choose(n,CV_n))
    r2.data.vs.pred = vector(length = choose(n,CV_n))
    time = vector(length = choose(n,CV_n))
    means = array(data = 0,dim = c(ny,choose(nx,1),6))
    means.ord = array(data = 0,dim = c(ny,choose(nx,1),6))

    coefs=matrix(nrow=choose(n,CV_n),ncol=p)
    mean.coefs=vector(length=p)

    #Cross validation process
    for(i in 1:ny)
    {
      y = i+1
      mean.y = mean(model.data[,y])#Media de metricas de imagen
      mean.sstt = sum((model.data[,y]-mean.y)^2)/n #Media de Suma de cuadrados
      for(j in 1:choose(nx,1))
      {
        x1 = combs.var[1,j]+ny+1
        for(k in 1:choose(n,CV_n))
        {
          time1 = proc.time()[3]
          print(paste("vary = ",i,", varx1 = ",combs.var[1,j],", comb = ", k,sep = ""))
          validation.data = model.data[combs.data.CV[,k],]#Datos omitidos del modelo
          calibration.data = model.data[-combs.data.CV[,k],]#Datos pa armar el modelos sin los de validación
          model = lm(calibration.data[,y]~calibration.data[,x1])
          coef = model$coef
          for(l in 1:CV_n)
          {
            pred[l] = coef[1]+coef[2]*validation.data[l,x1]
          }
          mean.y.validation = mean(validation.data[,y])
          msse.validation = sum((validation.data[,y]-pred)^2)/CV_n
          CV.CV[k] = sqrt(msse.validation)/mean.y.validation
          r2.CV[k] = 1-msse.validation/mean.sstt
          r2.data.vs.pred[k] = (cor(validation.data[,y],pred,method = "pearson"))^2
          coefs[k,] = coef
          time2 = proc.time()[3]
          time[k] = time2-time1
        }
        mean.time = mean(time)
        print(paste("mean time = ",mean.time,sep = ""))
        mean.CV.CV = mean(CV.CV)
        mean.r2.CV = mean(r2.CV)
        mean.coefs[1] = mean(coefs[,1])
        mean.coefs[2] = mean(coefs[,2])
        mean.r2.data.vs.pred = mean(r2.data.vs.pred)
        means[i,j,] = c(paste(names(model.data)[y],"vs",names(model.data)[x1]),mean.r2.CV,mean.r2.data.vs.pred,mean.CV.CV,mean.coefs[1],mean.coefs[2])
      }
      means.ord[i,,]<-means[i,order(as.numeric(means[i,,2]),decreasing=T),]
      write.table(as.table(means.ord[i,,]),paste("Results-var1par2-CV-",names(model.data)[i+1],"",".xls",sep = ""),sep = "\t",row.names = F,col.names = c("Y vs X1","mean r2.CV","mean r2 data vs pred","mean CV.CV","mean Interc","mean Slope"))
    }
  }else{
    print("No Cross Validation performed")
  }


  #--------------------------Random highest R2 distribution-------------------------------
  if(r2random==T)
  {
    print("Performing R2 distribution at random")

    model.data.permut = model.data
    r2.max.permut = matrix(nrow = ny,ncol = runs)
    r2.model.pvalue = matrix(nrow = ny,ncol = choose(nx,1))
    r2 = array(dim = c(ny,choose(nx,1),runs))

    #r2 max permut
    for(k in 1:runs)
    {
      print(paste("run = ",k,sep = ""))
      for(i in 2:(nx+ny+1))
        model.data.permut[,i] = gtools::permute(model.data[,i]) #permuta posiciones de toda la matriz de datos
      for(i in 1:ny)
      {
        for(j in 1:choose(nx,1))
        {
          y = i+1
          x1 = combs.var[1,j]+ny+1
          model = lm(model.data.permut[,y]~model.data.permut[,x1])
          r2[i,j,k] = summary(model)$r.squared
        }
        r2.max.permut[i,k] = max(r2[i,,k])
      }
    }

    #r2 model pvalue
    for(i in 1:ny)
      #Order from higher to lower
      r2.max.permut[i,] = sort(r2.max.permut[i,],decreasing = T)

    #Colnames fix
    colnames = vector(length = (choose(nx,1)))
    for(i in 1:choose(nx,1))
    {
      x1 = combs.var[1,i]+ny+1
      colnames[i] = paste(names(model.data)[x1])
    }

    #Export to tables
    rownames(r2.max.permut)<-names(model.data)[2:(ny+1)]
    colnames(r2.max.permut)<-seq(1:runs)
    write.table(as.table(t(r2.max.permut)),paste("Results-var1par2-r2maxpermut",".xls",sep = ""),sep = "\t",
                col.names = NA,row.names = T)
    rownames(r2.model.pvalue)<-names(model.data)[2:(ny+1)]
    colnames(r2.model.pvalue)<-colnames
    write.table(as.table(r2.model.pvalue),paste("Results-var1par2-r2modelpvalue",".xls",sep = ""),sep = "\t",
                col.names = NA,row.names = T)
    write.table(as.table(r2),paste("Results-var1par2-r2ordenada",".xls",sep = ""),sep = "\t",
                col.names = c("Atrib comun","Metrics image","Runs","R2"),row.names = F)

  }else{
    print("No R2 distribution at random performed")
  }

  print(paste0("Process finished. Files can be found in the following directory: ",getwd()))
}

