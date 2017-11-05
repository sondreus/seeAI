#' Animate glmnet
#'
#' This function visualizes the a cv.glmnet machine learning process.
#' @param cv.glmnet An object of class 'cv.glmnet'
#' @param plot.cvm Should cross-validation error be plotted? Defaults to TRUE.
#' @param plot.cv.folds Should cross-validation folds be plotted? Defaults to TRUE.
#' @param total.time Desired time of animation in seconds. Defaults to 10.
#' @param new.save Should this animation be saved as a new object rather than overwrite the preceeding animation? Defaults to TRUE.
#' @param save.html Save as HTML? Defaults to TRUE. If FALSE, saves GIF.
#' @param debug Only plot subset of lambda values? Defaults to FALSE.
#' @param debug.n If plotting subset of lambda values, sets number of values to plot. Defaults to 100. 
#' @param ... Options passed to saveHTML or saveGIF functions. See ?animate::saveHTML and ?animate::saveGIF 
#' @keywords glmnet lasso cv.glmnet machinelearning seeAI
#' @export
#' @examples
#' # See also: ?cv.glmnet: 
#' set.seed(1010)
#' n=1000;p=100
#' nzc=trunc(p/10)
#' x=matrix(rnorm(n*p),n,p)
#' beta=rnorm(nzc)
#' fx= x[,seq(nzc)] %*% beta
#' eps=rnorm(n)*5
#' y=drop(fx+eps)
#' px=exp(fx)
#' px=px/(1+px)
#' ly=rbinom(n=length(px),prob=px,size=1)
#' set.seed(1011)
#' cvob1=cv.glmnet(x,y)
#' animate_glmnet(cvob1)


animate_glmnet <- function(cv.glmnet, plot.cvm = TRUE, plot.cv.folds = TRUE, total.time = 10, new.save = TRUE, save.html = TRUE, debug = FALSE, debug.n = 100, ...) {
  
  # ... are passed to saveGIF or save HTML.
  
  require(animation)
  
  # Debug
  if(debug == TRUE){
    cv.glmnet <- cv.glmnet(x, y, nfolds = cv.folds, lambda = sample(cv.glmnet$lambda, debug.n))
  }
  
  if(class(cv.glmnet) != "cv.glmnet"){
      message("Please supply an object of class cv.glmnet")
      stop()
  }
  
  ymax <-   100 * 1.2
  ymin <- - 100 * 1.2
  
  lambda.min <- which(cv.glmnet$glmnet.fit$lambda == cv.glmnet$lambda.min)
  itr <- length(cv.glmnet$lambda)
  
  cat(paste("Lambda Min:", lambda.min, " / ", length(cv.glmnet$lambda)))
  
  # Getting number of possible coefficients
  n <- nrow(coef(cv.glmnet$glmnet.fit)[-1, ])
  
  pause <- round(itr/2, 0)
  
  if(plot.cvm == TRUE){
    if(plot.cv.folds == TRUE){
      total.frames <- itr*2 + pause  
    } else {
      total.frames <- itr*2 + pause
    }
  } else {
    total.frames <- itr
  }
  
  
  wait.time <- total.time/total.frames
  
  oopt <- ani.options(nmax = total.frames, interval = wait.time)
  
  ani.record(reset = TRUE)
  
  
  # Define Percentile Function
  percentile <- function (dat) 
  {
    pt1 <- quantile(dat, probs = seq(0, 1, by = 0.01), type = 7)
    pt2 <- unique(as.data.frame(pt1), fromLast = TRUE)
    pt3 <- rownames(pt2)
    pt4 <- as.integer(strsplit(pt3, "%"))
    datp <- pt4[as.integer(cut(dat, c(0, pt2$pt1), labels = 1:length(pt3)))]
    return(datp)
  }
  
  # Get cross-validation error
  cvm <- rev((cv.glmnet$cvm - min(cv.glmnet$cvm))^.2)
  cvm <- cvm/max((cv.glmnet$cvm - min(cv.glmnet$cvm))^.2)
  cvm.df <- cbind.data.frame(cvm = cvm, xpos = n*((1:itr)/itr))
  cvm.df$cvm <- (cvm.df$cvm)*0.15*ymax + ymax*.80
  cvm.df$cvm.ymax <- ymax 
  
  cvm.min.coefs <- as.numeric(coef(cv.glmnet$glmnet.fit, s = cv.glmnet$lambda[lambda.min])[-1]) 
  
  if(length(cvm.min.coefs[cvm.min.coefs > 0]) > 0){cvm.min.coefs[cvm.min.coefs > 0] <- percentile(cvm.min.coefs[cvm.min.coefs > 0]) }
  if(length(cvm.min.coefs[cvm.min.coefs < 0]) > 0){cvm.min.coefs[cvm.min.coefs < 0] <- - percentile( - cvm.min.coefs[cvm.min.coefs < 0]) }
  
  
  # Constructing matrix of coefficient values and mapping these to their percentiles to their percentiles
  coef.values <- data.frame()
  plot.n <- n
  for(i in 1:itr){
    coef.values <- rbind(coef.values, cbind.data.frame(label = paste("Coefficient", 1:plot.n), value = as.numeric(coef(cv.glmnet$glmnet.fit, s=rev(cv.glmnet$lambda)[i])[-1]), coef.number = 1:plot.n))
  }
  if(length(coef.values$value[coef.values$value > 0]) > 0){coef.values$value[coef.values$value > 0] <- percentile(coef.values$value[coef.values$value > 0]) }
  if(length(coef.values$value[coef.values$value < 0]) > 0){coef.values$value[coef.values$value < 0] <- - percentile( - coef.values$value[coef.values$value < 0]) }
  
  
  ##
  # Generate animation
  ##
  index <- 1
  
  pb <- txtProgressBar(min = 0, max = total.frames, style = 3)
  
  for(i in 1:itr){
    #print(paste("Model fit - Percent complete:", 100*round(i/itr, 2), "%"))
    
    plot.data <- coef.values[(1:plot.n) + plot.n * (i - 1), ]
    
    
    p1 <- ggplot(plot.data, aes(y=value, x=coef.number)) + coord_polar(start = 0, direction = 1)
    
    # Adding shadows of previous previous fit
    if(i > 2){
      p1 <- p1 + geom_polygon(aes(y=old_old_value, x=plot.data$coef.number), fill = NA, alpha = 0.3, color = "gray")
    }
    
    # Adding shadows of previous fit
    if(i > 1){
      p1 <- p1 + geom_polygon(aes(y=old_value, x=plot.data$coef.number), fill = NA, alpha = 0.6, color = "gray")+xlim(0, plot.n)
      old_old_value <- old_value
    }
    
    p1 <- p1 + geom_polygon(aes(y=value + ifelse(plot.data$value == 0, 0, ifelse(plot.data$value > 0, 0.1*ymax, -0.1*ymax))), fill = NA, alpha=0.7, color = "black")+theme_bw()+geom_segment(aes(x=max(coef.number)*(i/itr)), xend=0, yend = ymax, y=ymax, size = 2, col = "skyblue", alpha=0.3)+xlab("")+ylab("")+geom_segment(aes(y=value, xend=coef.number), yend=0, color = ifelse(plot.data$value > 0, "darkgreen", "skyblue"))+theme(axis.text.x = element_blank(), axis.text.y = element_blank(), panel.border = element_blank(), axis.ticks.y = element_blank())+ylim(ymin*1.1, ymax*1.1)
    
    # if(captions == TRUE){
    #   p1 <- p1 + 
    # }
    
    print(p1)
    
    old_value <- plot.data$value + ifelse(plot.data$value == 0, 0, ifelse(plot.data$value > 0, 0.1*ymax, -0.1*ymax))
    
    ani.record()
    ani.pause(wait.time)
    
    setTxtProgressBar(pb, index)
    
    index <- index + 1
    
    
  }
  
  if(plot.cvm == TRUE){ 
    cvm.index <- 1 
    
    for(k in 1:itr){ 
      #print(paste("Cross-validation - percent complete:", 100*round(i/itr, 2), "%"))
      
      # plot.data <- cbind.data.frame(label = paste("Coefficient", 1:plot.n), value = as.numeric(coef(cv.glmnet$glmnet.fit, s=cv.glmnet$glmnet.fit$lambda[k])[-1]), coef.number = as.numeric(1:plot.n))
      # if(length(plot.data$value[plot.data$value > 0]) > 0){plot.data$value[plot.data$value > 0] <- percentile(plot.data$value[plot.data$value > 0]) }
      # if(length(plot.data$value[plot.data$value < 0]) > 0){plot.data$value[plot.data$value < 0] <- - percentile( - plot.data$value[plot.data$value < 0]) }
      
      plot.data <- coef.values[(nrow(coef.values):1)[(1:plot.n) + plot.n * (k - 1)], ]
      
      p1 <- ggplot(plot.data, aes(y=value, x=coef.number)) + geom_polygon(aes(y=value + ifelse(plot.data$value == 0, 0, ifelse(plot.data$value > 0, 0.1*ymax, -0.1*ymax))), fill = NA, alpha=0.7, color = "gray")+ coord_polar(start = 0, direction = 1)+theme_bw()+geom_segment(aes(x=max(coef.number)), xend=0, yend = ymax, y=ymax, size = 2, col = "skyblue", alpha=0.3) +xlab("")+ylab("")+theme(axis.text.x = element_blank(), axis.text.y = element_blank(), panel.border = element_blank(), axis.ticks.y = element_blank())+ylim(ymin*1.1, ymax*1.1)+xlim(0, plot.n)
      
      # Add CVM
      cvm.temp <- cvm.df[1:min(k,nrow(cvm.df)), ]
      
      suppressWarnings(
        p2 <- p1 + geom_ribbon(data = cvm.temp, aes(x=xpos, ymin=cvm, ymax=cvm.ymax, y= 0), fill = "deepskyblue", color = "deepskyblue")
      )
      
      if(plot.cv.folds == TRUE){
        if(cv.folds > 50){
          plot.cv.folds.n <- 50
        } else {
          plot.cv.folds.n <- cv.folds
        }
        #for(cv in 1:cv.folds){
        p3 <- p2 + geom_point(data = cbind.data.frame(x=((1:plot.cv.folds.n)*(plot.n/plot.cv.folds.n) - 0.5*(plot.n/plot.cv.folds.n)), y = rep(-0.1*ymax)), aes(x=x, y=y), shape = 23, color = "gray", size = 2, fill = "white")
        p3 <- p3 + geom_point(data = cbind.data.frame(x=((sample(1:plot.cv.folds.n, 1))*(plot.n/plot.cv.folds.n) - 0.5*(plot.n/plot.cv.folds.n)), y = rep(-0.1*ymax)), aes(x=x, y=y), shape = 23, fill = "skyblue", color = "gray", size = 2)
        
        setTxtProgressBar(pb, index)
        index <- index + 1
        
        print(p3)
        
        ani.record()
        ani.pause(wait.time)
        
        # }
      } else { 
        
        
        print(p2)
        
        ani.record()
        ani.pause(wait.time)
        
        setTxtProgressBar(pb, index)
        index <- index + 1
        
        
      }
      
      
    }
    
    # Add best model in skyblue once discovered
    k <- itr - lambda.min + 1
    
    p2 <- p2 + geom_segment(aes(y=ymax+0.1*ymax, yend=ymax-0.35*ymax, x=cvm.temp$xpos[k], xend =cvm.temp$xpos[k]), color = "skyblue", size = 1.2) + geom_segment(aes(y=ymax+0.1*ymax, yend=ymax+0.1*ymax-(ymax+0.1*ymax - (ymax-0.35*ymax))*(min(cv.glmnet$cvm)), x=cvm.temp$xpos[k], xend =cvm.temp$xpos[k]), color = "darkblue", size = 1.2)
    
    p.ind <- 1
    
    for(p in 1:pause){
      p3 <- p2 + geom_polygon(aes(y=cvm.min.coefs), fill = NA, alpha=0.7, color = "skyblue", size = 1)
      
      if(p.ind %in% c(1,2,3)){
        
        print(p3)
        p.ind <- p.ind + 1
        
      } else {
        
        print(p2)
        p.ind <- 1  
        
      }
      
      ani.record()
      ani.pause(wait.time)
      
      setTxtProgressBar(pb, index)
      index <- index + 1
      
    }
  }
  
  close(pb) 
  
  
  #ani.replay()
  
  if(!exists("html.index")){
    html.index <- 0
  } 
  
  if(new.save == FALSE){
    html.index <- 0
  }
  
  html.index <- html.index + 1
  
  if(save.html == TRUE){
    saveHTML(ani.replay(), htmlfile = paste0(html.index, "_glmnet.html"), title = paste0(html.index, "_glmnet.html"), ...)
  } else {
    saveGIF(ani.replay(), movie.name = paste0(html.index, "_glmnet.gif"), cmd.fun = system, ...)
  }
  if(replay == TRUE){
    print(ani.replay())
  }
}

