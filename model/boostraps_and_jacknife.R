
get_bstp <- function (data, b = 1000, y = NULL, param = c("mean","median","correlation","b0","b1"), na.rm = TRUE) {
  bstp <- c()
  n <- 0
  if (param %in% c("mean","median")) {
    if (!is.vector(data)) {
      cat("\nError : Data input harus vector")
      return(NULL)
      stop()
    }
  } else if (param %in% c("correlation","b0","b1")) {
    if (!is.data.frame(data)) {
      cat("\nError : Data input harus data frame")
      return(NULL)
      stop()
    }
    if (is.null(y)) {
      cat("\nError : Nama kolom untuk dependent variable harus ditulis didefinisikan di y")
      return(NULL)
      stop()
    }
  }
  if (is.data.frame(data)) {
    if (ncol(data) == 2) {
      n <- nrow(data)  
    } else {
      cat("\nError : Data harus 2 kolom (Y dan X)")
      return(NULL)
      stop()
    }
  } else if (is.vector(data)) {
    n <- length(data)
  } else {
    cat("\nError : Data input tidak valid")
    return(NULL)
    stop()
  }
  if (param == 'mean') {
    for(i in 1:b) {
      bstp <- c(bstp,base::mean(base::sample(data,size = n, replace = TRUE), na.rm = na.rm))
    }    
  } else if (param == 'median') {
    for(i in 1:b) {
      bstp <- c(bstp, median(base::sample(data,size = n, replace = TRUE), na.rm = na.rm))
    }    
  } else if (param == 'correlation') {
    for (i in 1:b) {
      bstp <- c(bstp,cor(data[sample(nrow(data),n,replace = TRUE),])[2])
    }
  } else if (param == 'b0') {
    for (i in 1:b) {
      form_lm <- as.formula(paste(y,"~."))
      model <- lm(formula = form_lm, data = data[sample(nrow(data),n,replace = TRUE),])
      bstp <- c(bstp,as.numeric(model$coefficients[1]))
    }
  } else if (param == 'b1') {
    for (i in 1:b) {
      form_lm <- as.formula(paste(y,"~."))
      model <- lm(formula = form_lm, data = data[sample(nrow(data),n,replace = TRUE),])
      bstp <- c(bstp,as.numeric(model$coefficients[2]))
    }
  }
  return(bstp)
}

get_jackknife <- function (data, b= 1000, y = NULL, param = c("mean","median","correlation","b0","b1"), na.rm = TRUE) {
  jackknife <- c()
  n <- 0
  if (param %in% c("mean","median")) {
    if (!is.vector(data)) {
      cat("\nError : Data input harus vector")
      return(NULL)
      stop()
    }
  } else if (param %in% c("correlation","b0","b1")) {
    if (!is.data.frame(data)) {
      cat("\nError : Data input harus data frame")
      return(NULL)
      stop()
    }
    if (is.null(y)) {
      cat("\nError : Nama kolom untuk dependent variable harus ditulis didefinisikan di y")
      return(NULL)
      stop()
    }
  }
  if (is.data.frame(data)) {
    if (ncol(data) == 2) {
      n <- nrow(data)  
    } else {
      cat("\nError : Data harus 2 kolom (Y dan X)")
      return(NULL)
      stop()
    }
  } else if (is.vector(data)) {
    n <- length(data)
  } else {
    cat("\nError : Data input tidak valid")
    return(NULL)
    stop()
  }
  if (param == 'mean') {
    for(i in 1:b) {
      jackknife <- c(jackknife,base::mean(data[-i], na.rm = na.rm))
    }    
  } else if (param == 'median') {
    for(i in 1:b) {
      jackknife <- c(jackknife, median(data[-i], na.rm = na.rm))
    }    
  } else if (param == 'correlation') {
    for (i in 1:b) {
      jackknife <- c(jackknife,cor(data[-i,1],data[-i,2]))
    }
  } else if (param == 'b0') {
    for (i in 1:b) {
      form_lm <- as.formula(paste(y,"~."))
      model <- lm(formula = form_lm, data = data[-i,])
      jackknife <- c(jackknife,as.numeric(model$coefficients[1]))
    }
  } else if (param == 'b1') {
    for (i in 1:b) {
      form_lm <- as.formula(paste(y,"~."))
      model <- lm(formula = form_lm, data = data[-i,])
      jackknife <- c(jackknife,as.numeric(model$coefficients[2]))
    }
  }
  return(jackknife)
}