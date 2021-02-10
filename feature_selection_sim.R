feature_selection_sim <- function(mydata, measure='luca', p=1) {  
  # -----------------------------------------------------------------------------------------
  # Feature selection method using similarity measure and fuzzy entroropy 
  # measures based on the article:
      # P. Luukka, (2011) Feature Selection Using Fuzzy Entropy Measures with
      # Similarity Classifier, Expert Systems with Applications, 38, pp. 4600-4607
  
  # Function call:
  # feature_selection_sim(data, measure, p)
  
  # OUTPUTS:
  # output_1      index_rem (index of removed feature in original data)
  # output_2      data_mod (data with removed feature)
  
  # INPUTS:
  # mydata        a dataframe, contains class values (class values must be numeric)
  # measure       fuzzy entropy measure, either 'luca' or 'park', default measure is 'luca'
  # p             parameter of Lukasiewicz similarity measure
  #               p in (0, \infty) as default p = 1.
  # -----------------------------------------------------------------------------------------
  
  l = max(mydata[,ncol(mydata)])    # number of classes
  m = dim(mydata)[1]                # number of samples
  t = dim(mydata)[2]-1              # number of features
  dataold.df <- mydata
  
  # forming idealvec using arithmetic mean
  idealvec_s = matrix(nrow = l, ncol = t)
  for (k in 1:l) {
    idealvec_s[k,] = colMeans(mydata[mydata[,ncol(mydata)]==k,1:t])
  }
  
  # scaling data between [0,1]
  data_v <- mydata[,1:t] # features
  data_c <- mydata[,ncol(mydata)] # class labels
  min_v  <- sapply(data_v, min)
  ones   <- matrix(1L, nrow = dim(data_v)[1], ncol = dim(data_v)[2]) 
  data_v <- data_v + ones%*%diag(abs(min_v)) # dot product 
  
  tmp    <- matrix(rep(min_v,l), ncol=t, byrow=T)
  idealvec_s <- idealvec_s + tmp
  max_v  <- sapply(data_v, max)
  data_v <- data.matrix(data_v)%*%diag(max_v^(-1)) # here data_v is converted to matrix form first
                                                   # before the matrix multiplication
  idealvec_s <- idealvec_s/matrix(rep(max_v,l), ncol=t, byrow=T)
  
  data_df <- data.frame(data_v,data_c)
  datalearn_s <- data_df[,1:t] # sample data
  
  # similarities
  sim <- array(0,c(t,m,l))
  for (j in 1:m) {
    for (i in 1:t) {
      for (k in 1:l) {
        sim[i,j,k] = (1-abs(idealvec_s[k,i]^p-datalearn_s[j,i])^p)^(1/p) 
      } 
    } 
  }
  
  # reduce the number of dimensions in sim
  sim <- t(matrix(sim, nrow = t, ncol = m*l))
  
  # possibility for two different entropy measures
  if (measure =='luca') {
    # moodifying zero and one values of the similarity values to work with De Luca's entropy measure
    delta       = 1e-10
    sim[sim==0] = delta
    sim[sim==1] = 1-delta
    H = colSums(-sim*log(sim)-(1-sim)*log(1-sim), na.rm = FALSE)
  }else if (measure =='park'){
    H = colSums(sin((pi/2)*sim)+sin(pi/2*(1-sim))-1, na.rm = FALSE)
  }
  
  # find the maximum feature
  index_rem <- which.max(H)
  
  # removing feature from the data
  data_mod <- subset(dataold.df, select = -c(index_rem))
  
  output <- list(index_rem, data_mod) # since we need to return two objects, a list is used
  return(output)
}