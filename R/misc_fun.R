#' Translates text in Shiny (German <-> English)
#'
#' Internal Function. Translates text in Shiny (German <-> English)
#'
#'
#' @param text which text to translate
#' @author Philipp Bühler
#' @keywords internal
tr <- function(text){ # translates text into current language
  sapply(text,function(s) translation_list[[s]][[language]], USE.NAMES=FALSE)
}







#' reads table and covnerts string to date
#'
#' Internal function. reads table and covnerts string to date
#'
#'
#' @param ... as in read.table
#' @param date.formats date.formats are preformatted data formats
#' @author Philipp Bühler
#' @keywords internal
my.read.table <- function(..., date.formats = c("%d.%m.%Y","%Y-%m-%d")) {
  dat <- utils::read.table(...,stringsAsFactors = FALSE)
  for (col.idx in seq_len(ncol(dat))) {
    x <- dat[, col.idx]
    if(!is.character(x) | is.factor(x)) next
    if (all(is.na(x))) next
    for (f in date.formats) {
      d <- as.Date(as.character(x), f)
      if (all(is.na(d[!is.na(x)]))) next
      dat[, col.idx] <- d
    }
  }
  dat
}







#' Adds Column to dataframe if nonexcistend
#'
#' Internal function. Adds Column to dataframe if nonexcistend
#'
#'
#' @param data data
#' @param cname columnname to add
#' @author Philipp Bühler
#' @keywords internal
fncols <- function(data, cname) {
  add <-cname[!cname%in%names(data)]
  if(length(add)!=0){
    data[add] <- ""
  }else{
    data[cname][is.na(data[cname])]<-""
  }
  return(data)
}





#' Calculating flood statistics for output in GUI and table
#'
#' Internal function. Calculating flood statistics for output in GUI and table
#'
#'
#' @param Floods Flood table
#' @param Q Discharge table
#' @keywords internal
#' @author Philipp Bühler
calc_stats<- function(Floods,Q) {
  a=Floods$Begin
  b=Floods$End
  ind=sapply(c(a,b),function(x) which(Q[,1]==x))

  S=data.frame(date=Q[ind[1]:ind[2],1],Q_org=Q[ind[1]:ind[2],2],scheit=FALSE)

  scheit=ifelse(length(which(S$date==Floods$Peak_date))!=0,which(S$date==Floods$Peak_date),which.max(S$Q))
  S[scheit,"Scheitel"]=TRUE
  #Volume
  Floods$Volume=round(sum(S$Q_org)/10^6 *24*60^2,3)

  S$baseflow=seq(S$Q_org[1],S$Q_org[nrow(S)],((S$Q_org[nrow(S)]-S$Q_org[1])/(nrow(S)-1)))

  Floods$Slope_of_Baseflow=round((S$Q_org[nrow(S)]-S$Q_org[1]) / (ind[2]-ind[1]),2)


  Floods$dir_Volume=round((Floods$Volume - (sum(S$baseflow)/10^6 *24*60^2)),2)

  Floods$baseflow_peak=round(S$baseflow[which(S$Scheitel==TRUE)],2)
  Floods$baseflow_begin=round(S$Q_org[1],2)
  Floods$baseflow_end=round(S$Q_org[nrow(S)],2)



  if(!any(Floods$HQ == "" | is.na(Floods$HQ))){
    Floods$HQ_dir=round(Floods$HQ-Floods$baseflow_peak,2)
    Floods$TQDir=round(Floods$dir_Volume/Floods$HQ_dir/60^2*10^6,2)
  }
  return(Floods)
}
