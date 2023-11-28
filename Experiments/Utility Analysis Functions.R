
# Mean Absolute Difference between Correlation Matrices
corMeanAbsDif <- function(orig_data, pert_data){
  absdif_mat <- abs(cor(pert_data) - cor(orig_data))
  uppertri_vec <- absdif_mat[upper.tri(absdif_mat, diag = FALSE)]
  calc <- mean(uppertri_vec)
  return(calc)
}

# Mean Absolute Difference between PCA Loading Matrices
pcaloadMeanAbsDif <- function(orig_data, pert_data, pc_vec){
  p <- ncol(orig_data)
  npc <- length(pc_vec)
  if(missing(pc_vec)) pc_vec <- 1:p
  
  pca_orig <- prcomp(orig_data, retx = FALSE, center = TRUE, scale. = TRUE)
  pca_pert <- prcomp(pert_data, retx = FALSE, center = TRUE, scale. = TRUE)
  
  if(npc == 1){
    # avoid matrix being changed to vector
    loadmat_orig <- as.matrix(pca_orig$rotation[,pc_vec])
    loadmat_pert <- as.matrix(pca_pert$rotation[,pc_vec])
  }
  
  if(npc > 1){
    loadmat_orig <- pca_orig$rotation[,pc_vec]
    loadmat_pert <- pca_pert$rotation[,pc_vec]  
  }
  
  # corresponding columns in loading matrices can be of opposite signs
  totaldif <- 0
  for(j in 1:length(pc_vec)){
    coldif1 <- loadmat_orig[,j] - loadmat_pert[,j]
    coldif2 <- loadmat_orig[,j] - (-1*loadmat_pert[,j])
    colsum1 <- sum(abs(coldif1))
    colsum2 <- sum(abs(coldif2))
    colsum <- min(colsum1, colsum2)
    totaldif <- totaldif + colsum
  }
  meandif <- totaldif / (p*npc)
  return(meandif)
  #mean(abs(loadmat_orig - loadmat_pert))
}

# Multiinformation
calc_multiinformation <- function(data, disc_method = "equalwidth"){
  if(disc_method %in% c("equalwidth", "equalfreq")){
    data_discretized <- infotheo::discretize(data, disc = disc_method)
  }
  if(disc_method == "bf_custom"){
    data_discretized <- data
      # get structure of input data
      # all values to be replaced
    
    data_discretized$bodyfat_pct <- floor(data$bodyfat_pct / 5)
    data_discretized$age <- floor(data$age / 10)
    data_discretized$weight <- floor(data$weight / 20)
    data_discretized$height <- floor(data$height / 2)
    data_discretized$neck <- floor(data$neck / 3)
    data_discretized$chest <- floor(data$chest / 6)
    data_discretized$abdomen <- floor(data$abdomen / 8)
    data_discretized$hip <- floor(data$hip / 8)
    data_discretized$thigh <- floor(data$thigh / 5)
    data_discretized$knee <- floor(data$knee / 2)
    data_discretized$ankle <- floor(data$ankle / 2)
    data_discretized$biceps <- floor(data$biceps / 2)
    data_discretized$forearm <- floor(data$forearm / 2)
    data_discretized$wrist <- floor(data$wrist / 1)
  }
  
  multiinfo <- multiinformation(data_discretized)
  return(multiinfo)
}

# Regression Utility Metrics Specific to the bodyfat Dataset
reg_utilityMetrics <- function(orig_data, pert_data, metric, regvars){
  orig_regdata <- orig_data
  orig_regdata$bmi <- (703 * orig_regdata$weight) / (orig_regdata$height^2)
  orig_regdata <- as.data.frame(scale(orig_regdata, center = TRUE, scale = TRUE))
  
  pert_regdata <- pert_data
  pert_regdata$bmi <- (703 * pert_regdata$weight) / (pert_regdata$height^2)
  pert_regdata <- as.data.frame(scale(pert_regdata, center = TRUE, scale = TRUE))
  
  if(regvars == "fsvars"){
    # best subset chosen through forward selection
    reg_orig <- lm(bodyfat_pct ~ bmi + neck + chest + abdomen + hip - 1, data = orig_regdata)
    reg_pert <- lm(bodyfat_pct ~ bmi + neck + chest + abdomen + hip - 1, data = pert_regdata)  
  }
  
  if(regvars == "all"){
    reg_orig <- lm(bodyfat_pct ~ . - 1, data = orig_regdata)
    reg_pert <- lm(bodyfat_pct ~ . - 1, data = pert_regdata)
  }
  
  if(metric == "coefMeanAbsDif"){
    coefs_orig <- reg_orig$coefficients
    coefs_pert <- reg_pert$coefficients
    output <- mean(abs(coefs_orig - coefs_pert))
  }
  
  if(metric == "rsquared"){
    rsquared_pert <- summary(reg_pert)$r.squared
    output <- rsquared_pert
  }
  
  if(metric == "rsquaredDif"){
    rsquared_orig <- summary(reg_orig)$r.squared
    rsquared_pert <- summary(reg_pert)$r.squared
    output <- rsquared_orig - rsquared_pert
  }
  
  if(metric == "rsquaredAbsDif"){
    rsquared_orig <- summary(reg_orig)$r.squared
    rsquared_pert <- summary(reg_pert)$r.squared
    output <- abs(rsquared_orig - rsquared_pert)
  }
  
  return(output)
}

