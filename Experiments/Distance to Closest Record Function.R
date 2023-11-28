# Function to calculate distance to closest record (DCR)
# Arguments:
#     orig_data: dataframe, original dataset
#     pert_data: dataframe, perturbed/protected/anonymized dataset
#     dist_metric: string, distance metric - options are L2 or L2rank
#     foreach_data: string, dataset for which DCR is calculated for each row - options are orig and pert
#     uselog: logical, whether log is used in the final risk metric calculation

# Notes: 
#     R(X,Y) from the Domingo-Ferrer, Ricci, and Soria-Comas paper would use the following arguments
#     dist_metric = "L2rank", foreach = "orig", uselog = TRUE
#
#     Syntegra uses the following version of DCR:
#     dist_metric = "L2", foreach = "orig", uselog = [unknown]
#
#     Mostly AI and Tonic uses the following version of DCR:
#     dist_metric = "L2", foreach = "pert", uselog = [unknown]
#     These companies compare the synthetic-real DCR values with the real-real DCR values, which is not done here


calc_DCR <- function(orig_data, pert_data, dist_metric = "L2dist", foreach_data = "pert", uselog = FALSE){
  # Checks
  stopifnot("Dimensions of orig_data and pert_data are not the same" = all.equal(dim(orig_data), dim(pert_data)))
  stopifnot("dist_metric must be \"dist\" or \"rankdist\"" = dist_metric %in% c("L2", "L2rank"))
  stopifnot("foreach_data must be \"orig\" or \"pert\"" = foreach_data %in% c("orig", "pert"))
  
  # Dimensions
  N <- nrow(orig_data)
  p <- ncol(orig_data)
  
  ######################################################
  if(dist_metric == "L2rank"){
    # Make matrices containing ranks for each variable
    rank_mat_orig <- matrix(nrow = N, ncol = p)
    rank_mat_pert <- matrix(nrow = N, ncol = p)
    for(i in 1:p){
      rank_mat_orig[,i] <- order(orig_data[,i])
      rank_mat_pert[,i] <- order(pert_data[,i])
    }
    
    # Distance between records d(x,y)
    mindist_vec <- vector(mode = "numeric", length = N)
    for(i in 1:N){
      # If foreach = "orig" find d(x,Y) = min d(x,y) for each x in X
      if(foreach_data == "orig") dist_vec <- pdist(rank_mat_orig[i,], rank_mat_pert)@dist
      
      # If foreach = "pert" find d(x,Y) = min d(x,y) for each y in Y
      if(foreach_data == "pert") dist_vec <- pdist(rank_mat_pert[i,], rank_mat_orig)@dist
      
      mindist_vec[i] <- dist_vec[which.min(dist_vec)]
    }
    
    # Calculate R(X,Y)
    if(uselog == TRUE) risk_metric <- (1/N) * log(sum(mindist_vec))
    else risk_metric <- (1/N) * sum(mindist_vec) 
  }
  ######################################################
  
  ######################################################
  if(dist_metric == "L2"){
    # Distance between records d(x,y)
    mindist_vec <- vector(mode = "numeric", length = N)
    
    # Find d(x,Y) = min d(x,y) for each x in X
    if(foreach_data == "orig"){
      for(i in 1:N){
        dist_vec <- pdist(orig_data[i,], pert_data)@dist
        mindist_vec[i] <- dist_vec[which.min(dist_vec)]
      }
    }
    
    # Find d(x,Y) = min d(x,y) for each y in Y
    if(foreach_data == "pert"){
      for(i in 1:N){
        dist_vec <- pdist(pert_data[i,], orig_data)@dist
        mindist_vec[i] <- dist_vec[which.min(dist_vec)]
      }
    }
    
    # Calculate R(X,Y)
    if(uselog == TRUE) risk_metric <- (1/N) * log(sum(mindist_vec))   
    else risk_metric <- (1/N) * sum(mindist_vec)
  }
  ######################################################
  
  return(risk_metric)
}

