quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

opt_fun <- function(par, Discharge, Q_upper, Q_lower, lower_tolerance, NA_mode, par_names){
  param_set <-  c(dvar=3,gamma=1, theta=0.25, ddur=40,omega=2,  Kappa=0.4, eta=0.1, delta=0.2)
  param_set[par_names] <- par

  Floods <- as.data.table(quiet(eventsep(Discharge, dvar = param_set["dvar"], gamma = param_set["gamma"],
    theta = param_set["theta"], ddur = param_set["ddur"], omega = param_set["omega"],
    Kappa = param_set["Kappa"], eta = param_set["eta"], delta = param_set["delta"], NA_mode = NA_mode)))

  Floods$ID <- 1:nrow(Floods)
  Floods_seq <- Floods[,.(Datum=seq(Begin,End, by="days")),by=c("ID")]
  Floods_seq$is_Flood <- TRUE

  Discharge <- merge(Discharge, Floods_seq[,.(Datum, is_Flood)], all.x=TRUE, by="Datum")
  Discharge[is.na(is_Flood),"is_Flood"] <- FALSE



  G_upper <- nrow(Discharge[is_Flood==TRUE & Abfluss > Q_upper]) / nrow(Discharge[Abfluss > Q_upper])
  G_lower <- nrow(Discharge[is_Flood==TRUE & Abfluss < Q_lower]) / nrow(Discharge[Abfluss < Q_lower])

  G_all <- G_upper - max(0, (G_lower - lower_tolerance))
  if(G_all<0) G_all <- 0
  if(G_all>1) G_all <- 1

  cat(paste(par_names,par, collapse = "  ",sep= ": "), "  Goodness: ", G_all,"\n")

  return((1-G_all))
}









#' Optimize the parameters of eventsep
#'
#' Optimize the parameters of eventsep by maximising flood events during high
#' quantiles and minimizing flood events during low quantiles
#'
#'
#' @param params Vector: Named vector of the stating parameters and their name,
#' e.g. c("gamma"=1, "eta"=0.1)
#' @param Discharge Data.frame: Discharge timeseries with Date amd Discharge column
#' @param upper_TH numeric: Upper Threshold
#' @param lower_TH numeric: Lower Threshold
#' @param lower_tolerance numeric: Lower tolerance
#' @param NA_mode integer: NULL or the length of the gap in which NA-values are interpolated
#' @author Philipp Bühler
#' @export optimize_floodsep_parameters
optimize_floodsep_parameters <- function(params, Discharge,
  upper_TH=0.95, lower_TH=0.5, lower_tolerance=0.01, NA_mode=NULL){

  if(requireNamespace("rgenoud")){
    par_names <- names(params)

    lower_param_bound <- c(dvar=2,gamma=0, theta=0, ddur=1, omega=0, Kappa=0, eta=0, delta=0)[par_names]
    upper_param_bound <- c(dvar=8,gamma=4, theta=1, ddur=200, omega=5, Kappa=3, eta=1, delta=3)[par_names]
    param_types <- c(dvar="integer",gamma="numeric", theta="numeric", ddur="integer", omega="integer",
      Kappa="numeric", eta="numeric", delta="numeric")[par_names]

    if(all(param_types %in% "integer")){
      mode <- "integer"
    }else if(all(param_types %in% "numeric")){
      mode <- "numeric"
    }else{
      stop("Only parameter sets with the same data type (all integer OR all numeric) can be optimized!")
    }

    Q_upper <- quantile(Discharge$Abfluss, upper_TH,na.rm = TRUE)
    Q_lower <- quantile(Discharge$Abfluss, lower_TH,na.rm = TRUE)


    res <- rgenoud::genoud(fn=opt_fun, nvars=length(params), data.type.int=fifelse(mode=="integer",TRUE,FALSE),
      max.generations=20,pop.size = 100,
      Domains =cbind(lower_param_bound, upper_param_bound), Discharge=Discharge, Q_upper=Q_upper, Q_lower=Q_lower,
      lower_tolerance=lower_tolerance, NA_mode=NA_mode, par_names=par_names,mode == "numeric")

    return(list(Parameter=res$par, Goodness=(1-res$value)))
  }
}




# if(Method == "BOBYQA"){
#   require(nloptr)
#   res <- bobyqa(x0=params, fn = opt_fun,lower=lower_param_bound, upper=upper_param_bound, nl.info = FALSE,
#     control = list(), Discharge, Q_upper, Q_lower, lower_tolerance, NA_mode, par_names)
# }else{
#   if(Method !="L-BFGS-B"){
#     lower_param_bound =-Inf
#     upper_param_bound = Inf
#   }
#   res <- optim(par = params, fn = opt_fun, gr = NULL, Discharge, Q_upper, Q_lower, lower_tolerance, NA_mode, par_names,
#     method="L-BFGS-B",lower = lower_param_bound, upper = upper_param_bound,control=list(parscale=10))
# }
