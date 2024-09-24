get_length <- function(x,index){
    x_tmp <- split(x,index)
    x_tmp_length <- sapply(x_tmp,FUN=function(x){length(x[!is.na(x)])})

    return(x_tmp_length)
    }
get_value <-  function(x,index){
    x_tmp <- split(x,index)
    x_tmp_value <- sapply(x_tmp,mean,na.rm=T)

    return(x_tmp_value)
    }


#temp=ci@data$tmin
#dates=ci@dates
#jdays=ci@jdays 
#date.factor=ci@date.factors$annual
#threshold.outside.base=ci@quantiles$tmax$outbase$q10 
#base.thresholds=ci@quantiles$tmax$inbase$q10
#base.range=ci@base.range
#op=">"
#max.missing.days=ci@max.missing.days[['annual']]



get_mean_values_Var <- function(temp, dates, jdays, date.factor, threshold.outside.base, 
base.thresholds, base.range, op='<', max.missing.days,Var,Var_begin,Var_end){
  
f <- match.fun(op)

  dat <- f(temp, threshold.outside.base[jdays])
  
  inset <- dates >= base.range[1] & dates <= base.range[2]
  ## Don't use in-base thresholds with data shorter than two years; no years to replace with.
  if(sum(inset) > 0 && length(dates) >= 360 * 2) {
    jdays.base <- jdays[inset]
    years.base <- get.years(dates[inset])

    ## Get number of base years, subset temp data to base period only.
    temp.base <- temp[inset]
    years.base.range <- range(years.base)
    byrs <- (years.base.range[2] - years.base.range[1] + 1)

    ## Linearize thresholds, then compare them to the temperatures
    bdim <- dim(base.thresholds)
    dim(base.thresholds) <- c(bdim[1] * bdim[2], bdim[3])
    yday.byr.indices <- jdays.base + (years.base - get.years(base.range)[1]) * bdim[1]
    f.result <- f(rep(temp.base, byrs - 1), base.thresholds[yday.byr.indices,])
    dim(f.result) <- c(length(yday.byr.indices), bdim[3]) 

    df_dat           <- rep(dat,bdim[3])
    dim(df_dat)      <- c(length(dat),bdim[3])

    ## Chop up data along the 2nd dim into a list; sum elements of the list
    ##dat[inset] <- rowSums(f.result, na.rm=TRUE) / (byrs - 1) 
    df_dat[inset,]   <- f.result  
  }


  df_dat[is.nan(df_dat)] <- NA
  if(missing(date.factor)) 
    return(dat)
  na.mask <- get.na.mask(dat, date.factor, max.missing.days)
  ## FIXME: Need to monthly-ize the NA mask calculation, which will be ugly.
  
  Var_begin <- as.PCICt(Var_begin,cal="gregorian")
  Var_end   <- as.PCICt(Var_end,cal="gregorian")
  
sub_time  <- dates >=Var_begin & dates  <= Var_end

df_dat       <- df_dat[sub_time,]
tmp_temp     <- temp[sub_time]
df_temp      <- rep(tmp_temp,bdim[3])
dim(df_temp) <- c(length(tmp_temp),bdim[3])

df_date_factor <- date.factor[sub_time]
df_Var      <- rep(Var,bdim[3])
dim(df_Var) <- c(length(Var),bdim[3])
df_true      <- df_dat
df_false     <- df_dat
df_true[which(df_true==0,arr.ind=T)] <- NA
df_false[which(df_false==1,arr.ind=T)] <- NA
Var_extreme     <- df_Var*df_true
Var_no_extreme  <- df_Var*!df_false
Temp_extreme    <- df_temp*df_true
Temp_no_extreme <- df_temp*!df_false

tmp_extreme_nee     <- apply(Var_extreme,2,get_value,df_date_factor)
tmp_no_extreme_nee  <- apply(Var_no_extreme,2,get_value,df_date_factor)
tmp_extreme_tem     <- apply(Temp_extreme,2,get_value,df_date_factor)
tmp_noextreme_tem   <- apply(Temp_no_extreme,2,get_value,df_date_factor)
tmp_extreme_tem_L     <- apply(Temp_extreme,2,get_length,df_date_factor)
tmp_noextreme_tem_L   <- apply(Temp_no_extreme,2,get_length,df_date_factor)

df_extreme_nee      <- apply(tmp_extreme_nee,1,mean,na.rm=TRUE)
df_no_extreme_nee   <- apply(tmp_no_extreme_nee,1,mean,na.rm=TRUE) 
df_extreme_tem      <- apply(tmp_extreme_tem,1,mean,na.rm=TRUE)
df_no_extreme_tem   <- apply(tmp_noextreme_tem,1,mean,na.rm=TRUE)
df_extreme_tem_L      <- apply(tmp_extreme_tem_L,1,mean,na.rm=TRUE)
df_no_extreme_tem_L   <- apply(tmp_noextreme_tem_L,1,mean,na.rm=TRUE)

res    <- list(extreme_nee=df_extreme_nee,no_extreme_nee=df_no_extreme_nee,
               extreme_tem=df_extreme_tem,no_extreme_tem=df_no_extreme_tem,
			   extreme_tem_L=df_extreme_tem_L,no_extreme_tem_L=df_no_extreme_tem_L)
return(res)
    }



yuan_tx10p_var <- function(ci, freq=c("monthly", "annual"),Var,Var_begin,Var_end) { 
    stopifnot(!is.null(ci@data$tmax) && !is.null(ci@quantiles$tmax)); 
	
return(get_mean_values_Var(ci@data$tmax, ci@dates, ci@jdays, ci@date.factors[[match.arg(freq)]], 
ci@quantiles$tmax$outbase$q10, ci@quantiles$tmax$inbase$q10, ci@base.range, "<", 
ci@max.missing.days[match.arg(freq)],Var,Var_begin,Var_end))}

yuan_tx90p_var <- function(ci, freq=c("monthly", "annual"),Var,Var_begin,Var_end) { 
stopifnot(!is.null(ci@data$tmax) && !is.null(ci@quantiles$tmax)); 
return(get_mean_values_Var(ci@data$tmax, ci@dates, ci@jdays, ci@date.factors[[match.arg(freq)]], 
ci@quantiles$tmax$outbase$q90, ci@quantiles$tmax$inbase$q90, ci@base.range, ">",
ci@max.missing.days[match.arg(freq)],Var,Var_begin,Var_end))}


yuan_tn10p_var <-  function(ci, freq=c("monthly", "annual"),Var,Var_begin,Var_end) { 
 stopifnot(!is.null(ci@data$tmin) && !is.null(ci@quantiles$tmin)); 
 return(get_mean_values_Var(ci@data$tmin, ci@dates, ci@jdays, 
 ci@date.factors[[match.arg(freq)]], ci@quantiles$tmin$outbase$q10, 
 ci@quantiles$tmin$inbase$q10, ci@base.range, "<", ci@max.missing.days[match.arg(freq)],Var,Var_begin,Var_end))}

yuan_tn90p_var <- function(ci, freq=c("monthly", "annual"),Var,Var_begin,Var_end) { 
stopifnot(!is.null(ci@data$tmin) && !is.null(ci@quantiles$tmin)); 
return(get_mean_values_Var(ci@data$tmin, ci@dates, ci@jdays, 
ci@date.factors[[match.arg(freq)]], ci@quantiles$tmin$outbase$q90, 
ci@quantiles$tmin$inbase$q90, ci@base.range, ">", ci@max.missing.days[match.arg(freq)],Var,Var_begin,Var_end))}



