## crossTab-----------------------------2018-06-07
## Summarize z using crosstab values y.
## Hadley and package 'reshape' deprecated.
## ---------------------------------------------RH
crossTab = function(x=PBSdat, y=c("year","major"), 
   z="landed", func=function(x){sum(x)/1000.}, na.val=99, hadley=FALSE, ...)
{
	if (hadley && !requireNamespace("reshape", quietly = TRUE)) stop("`reshape` package is required")
	flds=names(x)
	if (!all(is.element(setdiff(y,"year"),flds)))
		stop ("Not all specified 'z' in dataframe")
	if (is.element("year",y) && !is.element("year",names(x))) {
		if (is.element("date",flds)) x$year=convFY(x$date,1)
		else stop("Need 'date' field to calculate 'year'") }

	if (hadley) {
		Y=reshape::melt.data.frame(x,y,z)
		expr=paste("Z=reshape::cast(Y,", paste(paste(ifelse(length(y)==1,"~",""),y,sep=""),collapse="~"), ",func,...)",sep="")
		eval(parse(text=expr))
	} else {
		#Yvals = gatherVals(x,c(y,z))
		#Ylist = split(Yvals,Yvals$key)
		#Y     = cbind(sapply(y,function(i){Ylist[[i]][,"value"]}),Ylist[[z]])

		X = x[,unique(c(y,z))]
		#X = x[,c(y,z)]  ## if y & z have the same fields, only need to specify once, otherwise the duplicate field becomes 'fld.1'
		X[,y][is.na(X[,y])] = na.val
		## Need drop=FALSE when y is a single factor (in the non-R sense)
		xdim = sapply(X[,y,drop=FALSE],function(xx){length(.su(xx))})
		xnam = sapply(X[,y,drop=FALSE],function(xx){.su(xx)},simplify=FALSE)
		Z    = array(0, dim=xdim, dimnames=xnam )
		#X$ID = .createIDs(X,y)  ## doesn't work if one of the fields has a valid 0 (zero) code
		X$ID = .trimWhiteSpace(apply(X[,y,drop=FALSE],1,paste0,collapse="|")) ## sometimes paste adds whitespace depending on format of y-values.
		## vector summary of x by y using func (unless func returns more than one summary value)
		Zsum = sapply(split(X[,z],X$ID),func) #,simplify=FALSE)
		if (is.vector(Zsum)) {
			Zind = strsplit(names(Zsum),split="\\|"); names(Zind) = names(Zsum)
#browser();return()
			expr = paste0("sapply(names(Zsum), function(i){ Z[", paste0("Zind[[i]][",1:length(xdim),"]",collapse=","),"] <<- Zsum[i] })")
		} else {
			Z = array(0, dim=c(xdim,nrow(Zsum)), dimnames=c(xnam,list(pars=rownames(Zsum))))
			Zind = strsplit(colnames(Zsum),split="\\|"); names(Zind) = colnames(Zsum)
			expr = paste0("sapply(colnames(Zsum), function(i){ Z[", paste0("Zind[[i]][",1:length(xdim),"]",collapse=","),",] <<- Zsum[,i] })")
		}
#browser();return()
		eval(parse(text=expr))
	}
	return(Z)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~crossTab

#crossTab(wts396,c("year","ttype"),"wt",length)
#junk = crossTab(test.data,c("lon","lat"),"chl",function(x){x[1]})
#out = crossTab(pop.age,c("year","major"),"age",function(x){x[1]}) #length))
#out = crossTab(pop.age,c("year","srfa"),"age",function(x){length(x[x>0 & !is.na(x)])},hadley=T) #length))
#plt.noto = crossTab(ages,c("year","sex"),"SPID",function(x){length(.su(x))})  ## weightBio
#refpmr.h = crossTab(refdat,c("major","fid"),"ratio",getPMRrat, hadley=T)  ## buildCatch
#crossTab(test,c("year","gear"),"landed")
#print(crossTab(test,c("year"),"landed",length))
