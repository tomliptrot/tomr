
mypar=function( pch=20,	mgp=c(2,0.5,0), mar = c(3,3,3,1), tck=-.01, las=1, bty='l',cex.axis=0.7,...){
	par(pch=pch, mgp=mgp, mar = mar, tck=tck, las=las, bty=bty,cex.axis=cex.axis,...) 
	}
mp <- mypar

#setHook('plot.new', function(...) mp(...))



freqPlot <- function(fmla, data, na.action = NULL, labs = NULL, sort_tab = TRUE, line_lab = 2,output  =FALSE,filename = 'test.wmf',...){
	op <- par()
	mf <- model.frame(fmla, data = data, na.action = na.action)
	if(is.null(labs)) labs = colnames(mf)
	fac_list <- llply(mf, function(.x) {
		c(plotFactor(.x, sort_tab = !is.ordered(.x), silent = TRUE), ' ' = NA)
		})
	names(fac_list) <- NULL
	#testFun <- function() c(x, NA)
	combined <- do.call('c', fac_list)
	title_pos = which(is.na(combined))
	lcolor = rep(1, length(combined))
	lcolor[title_pos] <- 0
	if(output) {
		win.metafile(filename =filename, height = length(combined)* .18)
		par(op)
	}
	dotchart(combined, pch = 19, lcolor  =lcolor, xlab = 'Number of patients',...)
	mtext(labs, line =line_lab, side = 2, at = title_pos, adj = 0, font = 2, col = 2)
	mtext(combined, line =3, side = 4, at = 1:length(combined), adj = 1, font = 2, col = 1)
	mtext('N', line =3, side = 4, at =length(combined)+0.5 , adj = 1, font = 2, col = 1)
	if(output) 	dev.off()
	invisible(combined)
}

 boxplot <- function(... , whisklty = 1, staplelty = 0){
	graphics::boxplot(..., whisklty = whisklty, staplelty = staplelty)
 }

 box.dot.plot <- function(fmla, data, jit = 0,pch.p = 3, print.n = TRUE,  ...) {
	bp <- boxplot(fmla, data = data, whisklty = 1, staplelty = 0, boxwex = 0.5, ...)
	mf <- model.frame(fmla, data)
	mm <- model.matrix(fmla, data)
	x = aaply(mm, 1, function(.x) which(.x == 1))
	points( x = x, y = jitter(model.extract(mf, "response"), amount = jit), pch = pch.p)
	if (print.n) mtext(paste('n =', bp$n), at  = 1:ncol(mm), side = 1, padj = , cex = 0.8)
	bp
}

boxplot2 <- function(x, grp = rep(1, length(x)), col = 1, reorder = TRUE, orderfun = median, 
	pch.p = 3,print.n = TRUE,jitter_x = FALSE, amount = 0,add_offset = 0,n_adj = 2, 
	horizontal = FALSE, add_means = FALSE,add_plot = FALSE, ...){
	
	bp_df <- na.omit(data.frame(x = x, grp = as.factor(grp), col = col))
	if(reorder){
		new_order <- ddply(bp_df, .(grp), function(.x){
			orderfun(.x$x)
			})
		bp_df$grp <- reorder.factor(bp_df$grp, order(new_order$V1))
	}
	bp <- boxplot(x ~ grp, bp_df,horizontal = horizontal,add = add_plot, ...)
	if(jitter_x) bp_df$x <- jitter(bp_df$x , amount = amount)
	if(horizontal)
		points( y = as.numeric(bp_df$grp) + add_offset, x = bp_df$x, pch = pch.p, col = bp_df$col)
	else
		points( x = as.numeric(bp_df$grp) + add_offset, y = bp_df$x, pch = pch.p, col = bp_df$col)
	if (print.n) 
		mtext(paste('n =', bp$n), at  = 1:nlevels(bp_df$grp )+ add_offset, side = ifelse(horizontal, 2,1), padj = n_adj, cex = 0.8)
	if (add_means){
		means <- tapply(bp_df$x, bp_df$grp,mean)
		points(y = means, x = 1:length(means) + add_offset,col="red",pch=20, cex = 3)
	}
}

orderboxplot<- function(x, grp, method=c("mean", "median"), ...){
	###function to make box plot in order of means or medians
	### x is a vector of data
	###grp is a vector of factors of the same length as x
	## (...) passes paramaters onto boxplot
	
	tMedians <- switch(method, 
					mean   = aggregate(x, list(grp), mean, na.rm = TRUE), 
					median = aggregate(x, list(grp), median, na.rm = TRUE) 
					)
	grp <- factor(grp, levels = tMedians[order(tMedians$x), 1]) 
	boxplot(x ~ grp,..., whisklty = 1, staplelty = 0)  ### Ordered by decreasing median or mean
	}
	
dist.plot<- function(means,  length=NULL,bounds=NULL,cex=0.8,r=2,print.n=TRUE,sort_2 =TRUE, pch = 19,
	dotcol=1,CIcol =1, group = NULL,group.titles = NULL,verticalline = 0,xlim = NULL,raxpos = -4,labelsat = c(2, 4),labs = NULL, add.n = NULL,...){
        #makes a plot of mean showing 95% CI's
        ## 'means' is a 2 column matrix
        ## Column 1: Means
        ## Coulmn 2: Standard Deviations
		# # rownames - the names of the vaibles
		## other arguments are: 
		## length - the length of the x axis - defaults to 1.1 *  the maximum absolute value
		## cex - text size, 
		## r - digits to round print out, 
		## print.n - logical - print the means on the plot or not
		## sort - logical - sort by mean values or not
		
		if(is.null(xlim)) xlim = range(c(means[,1] + 2 * means[,2], means[,1] - 2 * means[,2]))
			I <- 1:nrow(means)
		if (sort_2){ 
			I <- order(means[,1, drop = FALSE])
			if(!is.null(group)) I <- order(group, -means[,1], decreasing = TRUE)
			means<-means[I,, drop = FALSE]
			if (!is.null(bounds)) bounds <- bounds[I,]
		}  #sorts by means
		
		if(!is.null(group)){
			df <- data.frame(variable = rownames(means), means = means, 
				group = factor(group[I], levels = unique(group[I])), bounds = bounds)#, n = add.n[I])			
			df <- ddply(df, .(group), function(.x){
				 rbind(.x, NA,NA, NA)
			})
			means <- as.matrix(df[,2:3])
			rownames(means) <- df$variable
			if(!is.null(group.titles) ) rownames(means)[is.na(means[,1])] <- group.titles
			bounds <- as.matrix(df[,5:6])
			rownames(bounds) <- df$variable
		}
		
		if (is.null(length)) {length = 1.1*max(abs(means[,1]), na.rm = TRUE)}
		
		seq <- 1:dim(means)[1]
		#
		plot(seq ~ means[,1], pch=pch, xlim=xlim,ylim=c(0.8,dim(means)[1]+0.2),
		col=dotcol, yaxt='n', ylab="", bty='n', ...)  ##makes main plot with dots
		
		if (is.null(bounds)){
			
			segments(means[,1], seq,means[,1]+1.96*means[,2], seq,col=CIcol) #adds confidence intervals
			segments(means[,1],seq, means[,1]-1.96*means[,2],col=CIcol)
		}
		else {
			segments(bounds[,2], seq,bounds[,1], seq, col=CIcol) #adds confidence intervals
			#arrows(means[,1],seq,bounds[,2], seq, angle=90, length=0.05, col=CIcol)
		}
		
		#abline(v=verticalline, lty=2, col=CIcol) ##adds line at zero
		#change font for titles
		font <- rep(1, dim(means)[1])
		font[is.na(means[,1])] <- 2
		#text(y=seq+0.22, x=0.65*(0.9*length), rownames(means), cex=cex, adj=c(0,1), font = font) #adds variable names from rownames()
		
		if (is.null(labs)) labs = rownames(means)
		else labs = labs[I]
		for(i in labelsat){
		axis(side = labelsat, at = seq, las = 1, labels =labs, font = font, line = raxpos, lwd.ticks =0)
		}
		
		if(!is.null(add.n)){
			axis(4, at = 1:length(df$n), labels = df$n,font = 2, 
			cex.axis = 0.8,  line = -1, tcl = 0, lty = 0, col.axis = 1, lwd.ticks =0)
		}
		
		#axis(side = 4, at = seq, las = 1, labels =rownames(means), font = font, line = raxpos)
		#if(print.n) {text(y=seq+0.22, x=0.75*length, round(means[,1], r), cex=cex, adj=c(0,1))} #adds numbers
		invisible(means)
 }
 
 
scatter.plot <- function (x,y, regline=TRUE, col.line='lightgrey',sig=3, cor = c("lm", "spearman"),
	zeroint = FALSE, xlab = NULL, ylab = NULL, cex.reg = 0.8, smooth = FALSE,...) {
	
	if (is.null(xlab) ) xlab = deparse(substitute(x))
	if (is.null(ylab) ) ylab = deparse(substitute(y))
	plot(y~x, xlab = xlab,ylab = ylab,...)
	if (regline)	add.reg.line(y = y, x = x, cor = cor, sig = sig, col.line = col.line, zeroint = zeroint,  cex = cex.reg)
	if(smooth) {
		temp <- na.omit(data.frame(y, x))
		lines(lowess(temp$x, temp$y), col = 2,lwd = 2)
	}
}

scatter.text<- function(x,y, labels,cex=1,regline=TRUE,
			xlab = NULL, ylab = NULL, cor = c("lm", "spearman"), 
			col.line="lightgrey",lty=2,lwd=1,font=1, sig = 3, zeroint = FALSE, grid=FALSE,col.text = 1, ...){
	if (is.null(xlab) ) xlab = deparse(substitute(x))
	if (is.null(ylab) ) ylab = deparse(substitute(y))
	plot(x, y, pch="",bty="n",xlab=xlab, ylab = ylab, ...)
	if (grid) grid(col='white', lty=1)
	text(x, y, labels=labels,cex=cex, font=font, col = col.text)
	if (regline)	add.reg.line(y = y, x = x, cor = cor, sig = sig, col.line = col.line, zeroint = zeroint)
}

add.reg.line <- function(y, x, zeroint = FALSE, cor = c("lm", "spearman"), sig = 3, col.line = "lightgrey", cex = 0.8) {
	reg = lm(y ~ x)
	if(zeroint) 	{
		reg = lm(y ~ x - 1)
		cor = "spearman"
	}
		abline(reg=reg, col=col.line, lty=2)
		cor = match.arg(cor)
		txt = switch(cor,
					lm = paste(" Slope = ",signif(coef(reg)[2], sig)," (", 
					signif(confint(reg)[2,1],sig),", ", signif(confint(reg)[2,2],sig) , " )" , sep="" ) ,
					spearman = paste(" Correlation = ", signif(cor(y, x, "complete.obs", method="spearman"), sig )))
											
		mtext(txt, side = 3, line = 0,adj = 0, padj =1, cex=cex)
	}

rline <- 	add.reg.line 

MCplot = function(draw, ...) {
	plot(draw, type='l', ...)
	lines(cumsum(draw) / 1:(length(draw)), col=2)
	}

panel.reg <- function(x, y, col.line=2, reglwd = 1,smooth = TRUE, equality_line = FALSE, ...){
	points(x, y, ...)
	reg=lm(y~x)
	abline(reg=reg, col=col.line, lty=2, lwd= reglwd)
	if(smooth) {
		temp <- na.omit(data.frame(y, x))
		lines(lowess(temp$x, temp$y), col = 2,lwd = 2)
	}
	if(equality_line) abline(a= 0, b=1)
}
panel.reg.equal <- function(x, y, col.line=2, reglwd = 1,smooth = TRUE, ...){
	points(x, y, ...)
	reg=lm(y~x)
	abline(reg=reg, col=col.line, lty=2, lwd= reglwd)
	if(smooth) {
		temp <- na.omit(data.frame(y, x))
		lines(lowess(temp$x, temp$y), col = 2,lwd = 2)
	}
	abline(a= 0, b=1)
}

panel.factor <- function(x, y, by){
	points(x[by], y[by], col = 2, pch = 19)
	points(x[!by], y[!by], col = 3, pch = 19)
}

panel.factor <- function(x, y, by, ...){
	points(x, y, col = unclass(by), pch = 19)
}
	
	
panel.hist <- function(x, ...){
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col=2, ...)
	
}
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...){
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- cor(x, y, use = 'complete.obs')
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * abs(r))
}

panel.text <- function(x, y, labs, ...){
	text(x, y, labels=labs, ...)
	}


lplot <-  function(mat, j =0, lwd = 2, legend = TRUE, lgd.cex = 0.8, fun = function(){}, ...){
	n <- dim(mat)[2]
	t <- dim(mat)[1]
	#mat <- round(mat, 2)
	mat <- sweep(mat, 2, (1:n * j), '+')
	ymax = max(mat, na.rm=TRUE)
	if(legend) ymax = 3 * max( mat[1: (floor(t/2)),  ] )
	ylimit = c(min(mat, na.rm=TRUE), ymax)
	plot(mat[,1], type='l', ylim= ylimit, lwd=lwd, ...)
	fun()
	for ( i in n:1) { lines(mat[,i], col=i, lwd=lwd)}
	if(is.null(colnames(mat))) legend = FALSE
	if(legend) legend("topleft", colnames(mat), col = 1:n, lty=1, lwd=lwd, cex=lgd.cex, bty='n', inset = 0)
}

sqplot <- function(df, ...) {
		lim = c(min(df, na.rm = TRUE), max(df, na.rm = TRUE))
		plot(df, xlim = lim, ylim = lim, bty='o', ...)
		abline(a=0, b=1)
		}
		
coefplot2 <- function(lm.out,  add_intercept = FALSE, trans = NULL, ...){
	require(arm)
	intercept <- na.omit(coef(lm.out)[1])
	coefs <- na.omit(coef(lm.out)[-1])
	if(add_intercept) coefs  <- coefs + intercept
	sds <- sqrt(diag(vcov(lm.out)))[-1][order(coefs)]
	if(!is.null(trans)) {
		coefs <- trans(coefs)
		sds <- trans(sds)
	}
	coefs <- coefs[order(coefs)]
	#mp(mar = c(1,11,3,1))
	coefplot(coefs, sds = sds, varnames = names(coefs),...)
}

legend2 <- function(pos = 'topright', text , fill = 2:3, bty = 'n', cex = 1.2, ...){
	legend(pos, legend=text, fill = fill, bty = bty, cex = cex, ...)
}


coefplot.pfs <- function(fit, fit2 = NULL, whichCoefs = 1:length(coef(fit)), ...){
	coefs <- exp(coef(fit))
	coefs <- cbind(coefs, 1)[whichCoefs,]
	CI <- exp(confint(fit))[whichCoefs,]
	plot_means <- cbind(coefs, 1)
	if(!is.null(fit2)){
		overall <- c(exp(coef( fit2))[1], 1)
		plot_means <- rbind(overall, plot_means)
		CI <- rbind(exp(confint(fit2))[1,], CI)
	}
		
	dist.plot(plot_means, bounds = CI, verticalline = 1, ...)
	#TODO add n's to plot
}

coefplot.interactions <- function(fit, fit2 = NULL, fit2_i = 2, whichCoefs = 1:length(coef(fit)), ...){
	coefs <- coef(fit)
	coefs <- cbind(coefs, 1)[whichCoefs,]
	CI <- confint(fit)[whichCoefs,]
	plot_means <- cbind(coefs, 1)
	if(!is.null(fit2)){
		overall <- c(coef( fit2)[fit2_i], 1)
		plot_means <- rbind(overall, plot_means)
		CI <- rbind(confint(fit2)[fit2_i,], CI)
	}
		
	dist.plot(plot_means, bounds = CI,  ...)
	#TODO add n's to plot
}

shade <- function(x = 1:length(y), y , ...){
	polygon(c(x,x[length(x)]), c(y, y[1]),...)
}

makeTransparent<-function(someColor, alpha=100)
{
  newColor <- col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
    blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

gamLine <- function(y, x, from = -20, to = 20,show.se = FALSE, fillcol = makeTransparent(2), ...){
	require(mgcv)
	fit <- gam(y ~ s(x),...)
	x_new <- seq(from, to, length.out = 300)
	p <- predict(fit, type="response",se.fit=TRUE, newdata = data.frame(x = x_new  ))
	y_new <- p[[1]]
	if(show.se){
		se <- p[[2]] * 2
		polygon(c(x_new, rev(x_new)), c(y_new + se, rev(y_new - se)), 
			col = fillcol, border = NA)
		}
	lines(x = x_new, y = y_new, ...)
}

LMCILine <- function(y, x, from = -20, to = 20, show.se = TRUE,fillcol = makeTransparent(2),  ...){
	fit <- lm(y ~ x)
	x_new <- seq(from, to, length.out = 300)
	p <- predict(fit,se.fit=TRUE, newdata = data.frame(x = x_new  ))
	y_new <- p[[1]]
	if(show.se){
			se <- p[[2]] * 2
			polygon(c(x_new, rev(x_new)), c(y_new + se, rev(y_new - se)), 
					col = fillcol, border = NA)
			}
	lines(x = x_new, y = y_new, ...)
}

sigmoidLine <- function(y, x, from = -20, to = 20, weights = NULL,...){

	  # function needed for visualization purposes
    sigmoid = function(params, x) {
		params[1]*x^params[2] / (1 + x^params[2])
		}
    # fitting code
    fit = nls(y ~ SSlogis( x , Asym = 1, xmid, scal) , start = list(xmid = -3.4, scal = 2),weights = weights)

    y_new = predict(fit, data.frame(x = x_new ))
	lines(x = x_new, y = y_new, ...)
	
	# se.fit <- sqrt(apply(attr(predict(fit, data.frame(x = x_new)),"gradient"),1,
	# function(x) sum(vcov(mod1)*outer(x,x))))
	
	# outer(se.fit,qnorm(c(.5, .025,.975))

	list(fit = fit, coef(fit), cbind(x_new, y_new))
	}

smooth_line <- function(y, x,...){
	temp <- na.omit(data.frame(y, x))
	lines(lowess(temp$x, temp$y),...)
	}

two_density_plot <- function(var1, var2, xlim = NULL, ylim = NULL,col = 2:3,...){
	d1 <- density(var1, na.rm = TRUE)
	d2 <- density(var2, na.rm = TRUE)
	if(is.null(xlim)) xlim = range(d1$x, d2$x)
	if(is.null(ylim))ylim = range(d1$y, d2$y)
	
	plot (NA, 	xlim = xlim, ylim = ylim, ...)
	
	polygon(d1, col = makeTransparent(col[1]), border = NA)
	polygon(d2,	col = makeTransparent(col[2]), border = NA)
	}
	
p_title <- function(p) mtext(paste('p =', signif(p, 3),p_to_stars(p)), line = -0.65, cex = 0.8)


	
