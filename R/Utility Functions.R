#require(bayesm)
require(plyr)
#require(mvtnorm)
#require(abind)
###utility functions######

# remove_html <- function(htmlString) {
  # string <- gsub("<.*?>", "", htmlString)
  # string <- gsub("&nbsp;", "", string)
# }


FactorCountIndex <- function(fac, n){
	#returns a logical vector showing where the count of the levls in fac is greater then n
	fac %in% levels(fac)[ table(fac) > n]
}

find_names <- function(x = '.', data){
   names(data)[grep(x, names(data))]
   }
   
text_wrap = function(string, width = 80){
	out = string %>%
		strwrap(width = width) %>%
		paste(collapse = '\n...') 
		
	out = gsub('\\r', '\n', out)
		
	cat(out, '\n')
	}
 


plotFactor <- function(fac, n = NULL,sort_tab = TRUE,percent =FALSE,xlim = c(0, max(tab_plus_other, na.rm = TRUE)),print_n = TRUE, silent = FALSE, ... ){
	a <- c(table(fac, useNA =  "ifany"))
	if(is.null(n)) n <- length(a)
	if (sort_tab) a <- sort(a)
	names(a)[is.na(names(a))] <- 'Missing'
	top_n <- tail(a, n)
	tab_plus_other <- top_n
	if(length(a) > n){
		tab_plus_other <- c( sum(a) - sum(top_n), top_n)
		names(tab_plus_other)[1] <- 'Other'
		}
	if(percent) tab_plus_other <- 100 * tab_plus_other  / sum(tab_plus_other)
	
	if('Missing' %in%  names(tab_plus_other)){
		i_mis <- grep('Missing' , names(tab_plus_other))
		tab_plus_other <- tab_plus_other[ c(i_mis, (1:length(tab_plus_other))[-i_mis]) ]
	}

	if(!silent){
		dotchart(tab_plus_other, xlim = xlim,... )
		if(print_n) axis(side = 4, at = 1:length(tab_plus_other), labels = tab_plus_other, lty = 0)
		}
	tab_plus_other
}

asDate <- function(dates, formats){
	res <- rep(NA, length(dates))
	class(res) <- 'Date'
	for(i in 1:length(formats)){
		form = formats[i]
		res[is.na(res)] <- as.Date(dates[is.na(res)], format = form)
		}
	res
}	
gsub2 <- function(pattern, replacement, x, ...) {
	for(i in 1:length(replacement))
	x[i] <- gsub(pattern, replacement[i], x[i], ...)
	x
}
	 
dupesTab <- function(x){
	table(x)[table(x) > 1]
	}

html_to_txt <- function(string, encoding = 'UTF-8') {
	require(XML)
	require(stringr)
	
	if(nchar(string) == 0) return(string)
     string <- xpathApply(htmlParse(string, asText=TRUE,  encoding = encoding),
                 "//body//text()", 
                 xmlValue)
	 
	 string = paste(string, collapse = ' ')
	 
	 #replace Non-breaking space with space for ANSI compatibility
	 string <- gsub(intToUtf8(160), ' ', string)
	 #if(remove_non_graph) string <- str_replace_all(string,"[^[:graph:]]", " ") 
	 string
}


html_to_text_2 = function(x) {
	require(rvest)
	x_html = try(rvest::html(x))
	if("try-error" %in% class(x_html)) return(x)
	x_txt = rvest::html_text(x_html, encoding = 'UTF-8')
	x <- gsub(intToUtf8(160), ' ', x)
	x_txt
	}



get_age <- function(birth_date, age_date){
	require(lubridate)
	# birth_date <- as.Date(dmy(birth_date))
	# age_date <- as.Date(dmy(age_date))
	interval <- new_interval(start = birth_date, end = age_date)
	age = year(as.period(interval, units = 'years'))
	return(age)
	}
	
ls.size <- function(pos=1, sorted=TRUE, n=10) {
	#returns the size the n largest objects in the workspace
    .result <- sapply(ls(pos = pos, all.names = TRUE), function(..x)
		object.size(eval(as.symbol(..x))))
    if (sorted) {
         .result <- rev(sort(.result))
    }
	.result <- round(.result/1000000, 1)
	if (n > length(.result)) n = length(.result)
	.mat <- as.matrix(.result,ncol=1)[1:n, ,drop=FALSE]
	colnames(.mat) = "Object Size (Mb)"
	.mat
	}


setdiff2 <- function(x,y) {
	#returns a list of the elements of x that are not in y and the elements of y that are not in x (not the same thing...)
	Xdiff = setdiff(x,y)
	Ydiff = setdiff(y,x)
	list(X_not_in_Y=Xdiff, Y_not_in_X=Ydiff)
}

allSame <- function(x) length(unique(x)) == 1
#allSame <- function(x) min(x) == max(x)
	
unfactor<-function(f, as = as.character){as(levels(f)[as.integer(f)])} 

copy <- function(x,...) write.table(x, "clipboard", sep="\t", col.names=NA, ...)

scale.to <- function(x, to) {
  ##scales x to the mean and sd of to
  x <- as.matrix(x)
  to <- as.matrix(to)
  if(dim(x)[2] != dim(to)[2]) stop("X and to do not have the same number of columns")
  x <- scale(x)
  x[is.nan(x)] <- 0  #replaces any unchanging colums with zeros
  out <-  sweep(x, 2, sd(to), "*")
  out <- sweep(out, 2, colMeans(to), "+")
  out
  } 

drop.levels <- function(object,...){
	UseMethod("drop.levels")
	}
drop.levels.factor <- function(x) factor = x[, drop=TRUE] 
drop.levels.data.frame <- function(x) data.frame(lapply(x, function(y) y[, drop=TRUE]))
drop.levels.list <- function(x) lapply(x, function(y) drop.levels(y))	

get.names <- function(...) { 
	#gets tha names of all of the objects in ...
   sapply(match.call()[-1], deparse) 
} 

n.na <- function(x) table(is.na(x))

reorder.factor <- function(x, new.order){
	#takes a factor and reorders its levels to new.order
	if(is.numeric(new.order)) 
	return(factor(x, levels(x)[new.order]))
	else return(factor(x, new.order))
}

duplicates <- function(x){
	#retunrs all of the duplicates in x
	duplicated(x) | duplicated(x, fromLast = TRUE)
}

logit <- function(x) log(x / (1 - x))
inverse.logit <- function(x) exp(x) / (1 + exp(x))

prop <- function(x, na.rm = TRUE){
	#x is a logical vector
	#returns the proportion of x that are true
	sum(x, na.rm = na.rm) / length(x)
}
	
RobustScale <- function (x){
	#Scales x to median  = 0 and mad = 1 (unless mad = 0 then used sd = 1)
	require('robustbase')
	m = mad(x)
	if(m == 0) m = sd(x)
	x <- (x - median(x))/m
	x
}

CleanNames <- function(df){
	n <- names(df)
	n <- tolower(n)
	n <- gsub('\\.{2,}', '.',n)
	n <-  gsub('^\\Qx.\\E', '', n)
	n <- gsub('\\.', '_', n)
	n <- gsub(' ', '_', n)
	n <- gsub('\\(', '', n)
	n <- gsub('\\)', '', n)
	n <- gsub('_$','',n)
	n <- gsub('\\?','',n)
	n <- gsub('__','_',n)
	n <- gsub('-', '_', n)
	n
}

clean_names <- CleanNames

asDateRegex <- function(dates, 
	#selects strings from the vector dates using regexes and converts these to Dates
	regexes = c('[0-9]{2}/[0-9]{2}/[0-9]{4}', #dd/mm/yyyy
		'[0-9]{2}/[0-9]{2}/[0-9]{2}$', #dd/mm/yy
		'[0-9]{2}-[[:alpha:]]{3}-[0-9]{4}'), #dd-mon-yyyy
	orders = 'dmy',
	...){
	require(lubridate)
	new_dates <- as.Date(rep(NA, length(dates)))
	for(reg in regexes){
		new_dates[grep(reg, dates)] <- as.Date(parse_date_time(dates[grep(reg, dates)], order = orders))
	}
	new_dates
}

mem <- function() {
  bit <- 8L * .Machine$sizeof.pointer
  if (!(bit == 32L || bit == 64L)) {
    stop("Unknown architecture", call. = FALSE)
  }
  
  node_size <- if (bit == 32L) 28L else 56L
  
  usage <- gc()
  sum(usage[, 1] * c(node_size, 8)) / (1024 ^ 2)
}


dupesTab <- function(x){
	table(x)[table(x) > 1]
	}
	
p_to_stars <- function(pv){
	if (all(is.na(pv))) return(NA)
	symnum(pv, corr = FALSE, na = TRUE, 
               cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
               symbols = c("***", "**", "*", ".", " ")) 	
	}
###end utility functions####