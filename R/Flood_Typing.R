#' Make a typology for a set of flood events
#'
#' This functions uses already defined flood events with relevant characteristics
#' (like precipitation, snow-melt and peak-value) for creating a typology for the whole set of flood events
#'
#'
#' @param Floods data.frame or data.table: Table with Floods, including the columns
#' @param n_G integer: Number of rain flood-types, defaults to 3
#' @param fast_composition logical: use the partition package for fast combination of events
#' @param Type_3_min_samplesize integer: NULL or interger specifying the minimum number of R3 events
#' @param R_Seed integer: NULL or a R-Seed for reproductable flood typing
#' @author Philipp Bühler
#' @import data.table
#' @export make_typing_of_floods
make_typing_of_floods <- function(Floods, n_G = 3L, fast_composition = FALSE,
  Type_3_min_samplesize = NULL, R_Seed = NULL){


  Floods_all <- as.data.table(Floods)
  stopifnot(all(c("Sum_SM", "Sum_N", "dir_Volume", "HQ_dir","PSI_SM") %in% names(Floods_all)))


  Floods_all <- Floods_all[,SM_rel := Sum_SM/(Sum_SM+Sum_N)]
  Floods_all <- Floods_all[,TQDir := (dir_Volume/HQ_dir/60^2*10^6)]

  Floods_all <- Floods_all[order(Floods_all$TQDir),]
  Floods_all$ID <- 1:nrow(Floods_all)
  Floods_all_ref <- Floods_all

  Floods_all <- Floods_all[!is.na(HQ_dir)]


  #Regen Ereignisse
  Floods_R <- Floods_all[SM_rel<0.2][,Typ:=factor(NA,levels = paste0("R", 1:n_G))]

  n <- nrow(Floods_R)

  #Welche M?glichkeiten gibt es, die Floods_R in n_G Gruppen zu unterteilen?
  if(fast_composition){
    mat <- t(as.matrix(partitions::compositions(as.integer(n), as.integer(n_G), include.zero = FALSE)))
  }else{
    mat <- n_ways(as.integer(n), as.integer(n_G))
  }


  if(is.null(Type_3_min_samplesize)){
    min_sampleSize <- ceiling(n/(n_G*2))
    mat <- mat[apply(mat,1,function(x) all(x >= min_sampleSize)),]
  }else{
    min_sampleSize <- c(rep(ceiling(n/(n_G*2)), (n_G-1)), Type_3_min_samplesize)
    mat <- mat[apply(mat,1,function(x) all(x >= min_sampleSize)),]
  }

  Comb <- cbind(0, mat)
  Comb <- t(apply(Comb, 1, cumsum))


  OUT <- leastSqrRegression(Floods_R$HQ_dir, Floods_R$dir_Volume, Comb, intercept_zero = TRUE)

  R2_mat <- OUT[[3]]
  r2_sums <- rowSums(R2_mat) #r^2 innerhalb Unterteilungen addieren

  optlen <- mat[which.max(r2_sums),] # welche Unterteilung gibt das max. r^2?
  opt_R2 <- R2_mat[which.max(r2_sums),]
  opt_slope <- OUT[[2]][which.max(r2_sums),]
  optind <- cumsum(c(1, optlen))
  cut_labz <- cut(1:nrow(Floods_R),optind,include.lowest = TRUE)

  Floods_R$Typ <- factor(cut_labz, labels =  paste0("R", 1:n_G))


  #Schnee Ereignisse
  Floods_S <- Floods_all[SM_rel>=0.2,][,Typ:=factor(NA,levels = c("S1","S2"))]

  if(nrow(Floods_S) > 2){
    cluster_daten <- Floods_S[,.(Sum_N, Sum_SM, PSI_SM)]
    cluster_daten <- cluster_daten  # [1:(nrow(cluster_daten)-3)]

    if(!is.null(R_Seed)) set.seed(R_Seed)
    types <- kmeans(cluster_daten, centers=2)

    Floods_S$Typ <- paste0("S", types$cluster)

    G <- Floods_S[,.(Mean_N=mean(Sum_N), Mean_SM=mean(Sum_SM), mean_PSI = mean(PSI_SM)),by=Typ]
    statz <- apply(as.matrix(G[,.(Mean_N, Mean_SM, mean_PSI)]),2, function(x) which.min(x))
    if(sum(statz==1)>=1 & G$Typ[1] =="S2") Floods_S$Typ <- fifelse(Floods_S$Typ=="S1","S2","S1")
  }

  Floods_res <- rbind(Floods_R,Floods_S)[order(Begin)]


  Floods_all_ref <- merge(Floods_all_ref, Floods_res[,.(Typ, ID)], by="ID", all.x=TRUE,sort=FALSE)[order(Begin)]
  Floods_all_ref <- Floods_all_ref[,-"ID"]

  return(Floods_all_ref)
}



n_ways <- function(n, k){
  eg <- expand.grid(rep(list(1:n), k))
  return(unique(eg[which(rowSums(eg)==n),]))
}


