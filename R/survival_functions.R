make_survival_object <- function(start_date, end_date, status, census_date = NULL){
	require(survival)
	x = data.frame(start_date, end_date, status)
	x$start_date <- as.Date(x$start_date )
	x$end_date <- as.Date(x$end_date )
	
	x$last_follow_up = x$end_date
	
	if(!is.null(census_date)){
		x$last_follow_up[is.na(x$last_follow_up)] <- census_date 
	}
	
	x$follow_up_time = as.numeric(x$last_follow_up - x$start_date)
	
	negative_follow_up = x$follow_up_time < 0
	if(any(negative_follow_up)) {
		i_negative = which(negative_follow_up)
		warning(paste('Negative follow up in', length(i_negative), 'case(s): removing cases\n' ))
		x$follow_up_time[i_negative] <- NA
		}
	
	Surv(x$follow_up_time, x$status )
	}
	

survplot2 <- function (fit, xlim, ylim, xlab, ylab, time.inc, conf = c("bands", 
    "bars", "none"), add = FALSE, label.curves = TRUE, abbrev.label = FALSE, 
    levels.only = FALSE, lty, lwd = par("lwd"), col = 1, col.fill = gray(seq(0.95, 
        0.75, length = 5)), loglog = FALSE, fun, n.risk = FALSE, 
    logt = FALSE, dots = FALSE, dotsize = 0.003, grid = FALSE, 
    srt.n.risk = 0, sep.n.risk = 0.056, adj.n.risk = 1, y.n.risk, 
    cex.n.risk = 0.6, pr = FALSE,drawaxis = TRUE, ...) 
{
require(rms)
    conf <- match.arg(conf)
    polyg <- ordGridFun(grid = grid)$polygon
    conf.int <- fit$conf.int
    if (!length(conf.int) | conf == "none") 
        conf.int <- 0
    units <- fit$units
    if (!length(units)) 
        units <- "Day"
    maxtime <- fit$maxtime
    if (!length(maxtime)) 
        maxtime <- max(fit$time)
    mintime <- min(fit$time, 0)
    pret <- pretty(c(mintime, maxtime))
    maxtime <- max(pret)
    mintime <- min(pret)
    if (missing(time.inc)) {
        time.inc <- switch(units, Day = 30, Month = 1, Year = 1, 
            (maxtime - mintime)/10)
        if (time.inc > maxtime) 
            time.inc <- (maxtime - mintime)/10
    }
    if (n.risk && !length(fit$n.risk)) {
        n.risk <- FALSE
        warning("fit does not have number at risk\nIs probably from a parametric model\nn.risk set to F")
    }
    trans <- loglog | !missing(fun)
    if (missing(ylab)) {
        if (loglog) 
            ylab <- "log(-log Survival Probability)"
        else if (trans) 
            ylab <- ""
        else ylab <- "Survival Probability"
    }
    if (loglog) 
        fun <- function(w) logb(-logb(ifelse(w == 0 | w == 1, 
            NA, w)))
    else if (!trans) 
        fun <- function(w) w
    if (missing(xlab)) {
        if (logt) 
            xlab <- paste("log Survival Time in ", units, "s", 
                sep = "")
        else xlab <- if (units == " ") 
            ""
        else paste(units, "s", sep = "")
    }
    if (missing(xlim)) 
        xlim <- if (logt) 
            logb(c(maxtime/100, maxtime))
        else c(mintime, maxtime)
    if (trans) {
        fit$surv <- fun(fit$surv)
        fit$surv[is.infinite(fit$surv)] <- NA
        if (conf.int > 0) {
            fit$lower <- fun(fit$lower)
            fit$upper <- fun(fit$upper)
            fit$lower[is.infinite(fit$lower)] <- NA
            fit$upper[is.infinite(fit$upper)] <- NA
            if (missing(ylim)) 
                ylim <- range(c(fit$lower, fit$upper), na.rm = TRUE)
        }
        else if (missing(ylim)) 
            ylim <- range(fit$surv, na.rm = TRUE)
    }
    else if (missing(ylim)) 
        ylim <- c(0, 1)
    if (grid) {
        dots <- FALSE
        if (is.logical(grid)) 
            grid <- 0.05
    }
    if (logt | trans) {
        dots <- FALSE
        grid <- FALSE
    }
    olev <- slev <- names(fit$strata)
    if (levels.only) 
        slev <- gsub(".*=", "", slev)
    sleva <- if (abbrev.label) 
        abbreviate(slev)
    else slev
    ns <- length(slev)
    slevp <- ns > 0
    labelc <- is.list(label.curves) || label.curves
    if (!slevp) 
        labelc <- FALSE
    ns <- max(ns, 1)
    y <- 1:ns
    stemp <- if (ns == 1) 
        rep(1, length(fit$time))
    else rep(1:ns, fit$strata)
    if (n.risk | (conf.int > 0 & conf == "bars")) {
        stime <- seq(mintime, maxtime, time.inc)
        v <- summary(fit, times = stime, print.it = FALSE)
        vs <- if (ns > 1) 
            as.character(v$strata)
    }
    xd <- xlim[2] - xlim[1]
    yd <- ylim[2] - ylim[1]
    if (n.risk && !add) {
        mar <- par()$mar
        if (mar[4] < 4) {
            mar[4] <- mar[4] + 2
            par(mar = mar)
        }
    }
    lty <- if (missing(lty)) 
        seq(ns + 1)[-2]
    else rep(lty, length = ns)
    lwd <- rep(lwd, length = ns)
    col <- rep(col, length = ns)
    if (labelc || conf == "bands") 
        curves <- vector("list", ns)
    Tim <- Srv <- list()
    oxpd <- par("xpd")
    par(xpd = NA)
    on.exit(par(xpd = oxpd))
	xend <- double(ns)
	yend <- double(ns)
    for (i in 1:ns) {
        st <- stemp == i
        time <- fit$time[st]
        surv <- fit$surv[st]
        if (logt) 
            time <- logb(time)
        s <- !is.na(time) & (time >= xlim[1])
        if (i == 1 & !add) {
            plot(time, surv, xlab = xlab, xlim = xlim, ylab = ylab, 
                ylim = ylim, type = "n", axes = FALSE)
			if(drawaxis){
				mgp.axis(1, at = if (logt) 
					pretty(xlim)
				else seq(xlim[1], max(pretty(xlim)), time.inc), labels = TRUE)
				mgp.axis(2, at = pretty(ylim))
			}
            if (dots | grid) {
                xlm <- pretty(xlim)
                xlm <- c(xlm[1], xlm[length(xlm)])
                xp <- seq(xlm[1], xlm[2], by = time.inc)
                yd <- ylim[2] - ylim[1]
                if (yd <= 0.1) 
                  yi <- 0.01
                else if (yd <= 0.2) 
                  yi <- 0.025
                else if (yd <= 0.4) 
                  yi <- 0.05
                else yi <- 0.1
                yp <- seq(ylim[2], ylim[1] + if (n.risk && missing(y.n.risk)) 
                  yi
                else 0, by = -yi)
                if (dots) 
                  for (tt in xp) symbols(rep(tt, length(yp)), 
                    yp, circles = rep(dotsize, length(yp)), inches = dotsize, 
                    add = TRUE)
                else abline(h = yp, v = xp, col = grid)
            }
        }
        tim <- time[s]
        srv <- surv[s]
        if (conf.int > 0 && conf == "bands") {
            blower <- fit$lower[st][s]
            bupper <- fit$upper[st][s]
        }
        if (max(tim) > xlim[2]) {
            srvl <- srv[tim <= xlim[2] + 1e-06]
            s.last <- srvl[length(srvl)]
            k <- tim < xlim[2]
            tim <- c(tim[k], xlim[2])
            srv <- c(srv[k], s.last)
            if (conf.int > 0 && conf == "bands") {
                low.last <- blower[time <= xlim[2] + 1e-06]
                low.last <- low.last[length(low.last)]
                up.last <- bupper[time <= xlim[2] + 1e-06]
                up.last <- up.last[length(up.last)]
                blower <- c(blower[k], low.last)
                bupper <- c(bupper[k], up.last)
            }
        }
		xend[i] <- max(tim)
		yend[i] <- min(srv)
        if (logt) {
            if (conf != "bands") 
                lines(tim, srv, type = "s", lty = lty[i], col = col[i], 
                  lwd = lwd[i])
            if (labelc || conf == "bands") 
                curves[[i]] <- list(tim, srv)
        }
		
        else {
            xxx <- c(mintime, tim)
            yyy <- c(fun(1), srv)
            if (conf != "bands") 
                lines(xxx, yyy, type = "s", lty = lty[i], col = col[i], 
                  lwd = lwd[i])
            if (labelc || conf == "bands") 
                curves[[i]] <- list(xxx, yyy)
        }
        if (pr) {
            zest <- rbind(time[s], surv[s])
            dimnames(zest) <- list(c("Time", "Survival"), rep("", 
                sum(s)))
            if (slevp) 
                cat("\nEstimates for ", slev[i], "\n\n")
            print(zest, digits = 3)
        }
        if (conf.int > 0) {
            if (conf == "bands") {
                if (logt) 
                  polyg(x = c(tim, max(tim), rev(tim)), y = c(blower, 
                    rev(bupper), max(bupper)), col = col.fill[i], 
                    type = "s")
                else polyg(x = c(max(tim), tim, rev(c(tim, max(tim)))), 
                  y = c(fun(1), blower, rev(c(fun(1), bupper))), 
                  col = col.fill[i], type = "s")
            }
            else {
                j <- if (ns == 1) 
                  TRUE
                else vs == olev[i]
                tt <- v$time[j]
                ss <- v$surv[j]
                lower <- v$lower[j]
                upper <- v$upper[j]
                if (logt) 
                  tt <- logb(ifelse(tt == 0, NA, tt))
                tt <- tt + xd * (i - 1) * 0.01
                errbar(tt, ss, upper, lower, add = TRUE, lty = lty[i], 
                  col = col[i])
            }
        }
        if (n.risk) {
            j <- if (ns == 1) 
                TRUE
            else vs == olev[i]
            tt <- v$time[j]
            nrisk <- v$n.risk[j]
            tt[1] <- xlim[1]
            if (missing(y.n.risk)) 
                y.n.risk <- ylim[1]
            yy <- y.n.risk + yd * (ns - i) * sep.n.risk
            nri <- nrisk
            nri[tt > xlim[2]] <- NA
            text(tt[1], yy, nri[1], cex = cex.n.risk, adj = adj.n.risk, 
                srt = srt.n.risk)
			#browser()
			if (length(nri[-1] > 0))
				text(tt[-1], yy, nri[-1], cex = cex.n.risk, adj = 1)
            if (slevp) 
                text(xlim[2] + xd * 0.025, yy, adj = 0, sleva[i], 
                  cex = cex.n.risk, col = col[i], font = 2)
			if(i == ns){  
				text(tt[1], y.n.risk + yd * (ns) * sep.n.risk,'Number at risk', 
				cex = cex.n.risk, c(0.5, NA), offset = -1, font = 2)
			}
			
        }
    }
    if (conf == "bands") 
        for (i in 1:ns) lines(curves[[i]][[1]], curves[[i]][[2]], 
            lty = lty[i], lwd = lwd[i], col = col[i], type = "s")
    if (labelc) 
        labcurve(curves, sleva, type = "s", lty = lty, lwd = lwd, 
            opts = label.curves, col. = col)
    invisible(list(x = xend, y = yend, slev = slev))
}


survplot3 <- function(fit, col = 1:surv_groups + 1, col.fill = makeTransparent(1:surv_groups + 1, 50), 
		n.risk = TRUE, cex.n.risk = 0.8,y.n.risk = -0.4,
		time.inc = 365/2, xlim = c(0, 620), ylim = c(-0.05, 1), drawaxis = FALSE, 
		lty = 1, lwd = 3,levels.only=TRUE, label.curves= FALSE, ylab = 'Overall Survival (%)', xlab = '',plot_survival = TRUE, ...){
		require(rms)
	if(is.null(fit$strata)) surv_groups <- 1
	else surv_groups <- length(fit$strata	)
	fit_summary <- as.data.frame(summary(fit)[2:11])
	#max_time <- daply(fit_summary, .(strata), function(.x) max(.x$time))
	#min_surv <- daply(fit_summary, .(strata), function(.x) min(.x$surv))
	group_names <- sub(".*=", "",names(fit $ strata))
	s <- survplot2(fit,col = col, col.fill = col.fill, 
		n.risk = n.risk, cex.n.risk = cex.n.risk, y.n.risk = y.n.risk,
		time.inc = time.inc, xlim = xlim, ylim = ylim, drawaxis =drawaxis , 
		lty = lty, lwd = lwd,levels.only=levels.only, label.curves= label.curves, 
		ylab = ylab, xlab = xlab,...)
	#axis(1, at = 0:4 * 365 / 2, labels = c(0, 0.5, 1, 1.5, 2), pos = 0)
	#max_x = xlim[2] / time.inc
	
	axis(1, at = 0:floor(max(xlim) / time.inc) * time.inc , labels = 0:floor(max(xlim) / time.inc) , pos = 0)
	
	if(plot_survival) axis(2, at = 0:10 * 0.1, labels = 0:10 * 10)
	else axis(2, at = 0:10 * 0.1, labels = 10:0 * 10)
	
	abline(h = 0.5, col = makeTransparent(1,150), lty = 2, lwd = 1)
	op <- par(xpd = NA)
	if(surv_groups > 1){
		text(x = s$x, y = s$y,
			labels = group_names, col = 1:surv_groups + 1, pos = 4, font = 2)
		}
	par(op)
	}
	
	GetMedianSurv <- function(fit){
	tab <- summary(fit)$table
	if(is.null(nrow(tab)))
		med_surv <- summary(fit)$table[c('median', '0.95LCL', '0.95UCL')]
	else
		med_surv <- summary(fit)$table[,c('median', '0.95LCL', '0.95UCL')]
		
	return(med_surv)
}
	