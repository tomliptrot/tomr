#Stime series
###cleaning functions####

	###these take Zoo objects as their arguments and clean in various ways
	
	clean.rm<- function(x, k=28, Tol=2.5, index = NULL) {
			if (!is.zoo(x)) x <- zoo(x, index, frequency=1)
			rm <- rollmean(x, k, na.pad = TRUE, align = "center")
			resids <- abs(scale(x-rm))
			x[which(resids>Tol)]<-NA
			return(list(x=x, cleaned=index(x)[which(resids>Tol)]))
		#x
	}

	clean.mean<- function(x, Tol=2.5) {
		resids <- abs(scale(x))
		x[which(resids>Tol)]<-NA
		x
	}
	
	clean.arima<- function(x,Tol=2.5){
		mod<-arima(as.vector(log(x)), order=c(1,0,0), seasonal = list(order = c(0,0,0), period = 7),
			 xreg=model.matrix(~as.factor(weekdays(index(x)))-1),include.mean=FALSE)
		resids<-abs(scale(mod$residuals))
		x[which(resids>Tol)]<-NA
		x
	}

cwp <- function (object){
#
# cwp <--> ``coefficients with p-values''
# object is of class arima 
     coef <- coef(object)
     if (length(coef) > 0) {
         mask <- object$mask
         sdev <- sqrt(diag(vcov(object)))
         t.rat <- rep(NA, length(mask))
         t.rat[mask] <- coef[mask]/sdev
         pt <- 2 * pnorm(-abs(t.rat))
         setmp <- rep(NA, length(mask))
         setmp[mask] <- sdev
         sum <- rbind(coef, setmp, t.rat, pt)
         dimnames(sum) <- list(c("coef", "s.e.", "t ratio", "p-value"),
             names(coef))
         return(sum)
     } else return(NA)
}


# weeks since first Sunday after Epoch
# assumes week starts on Sunday.  Change 3 to 4 for Monday.
fmt <- function(x) {
        weeks <- function(x) as.numeric(x + 3) %/% 7 + 1
        sprintf("%s%05d", format(x, "%Y/%m/Week"), weeks(x) - weeks(x[1]) + 1)
}
