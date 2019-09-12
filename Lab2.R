name<-"Vyshnavi Pisupati"
liuid<-"vyspi394"
#1.1.1
sheldon_game<-function(player1,player2){
    ch<-c("rock","scissors","paper","scissors","lizard","spock")
    result<-matrix(c('Draw!','Player 1 Wins!','Player 2 wins!','Player 2 wins!','Player 1 Wins!','Player 2 wins!','Draw!','Player 1 Wins!','Player 1 Wins!','Player 2 wins!','Player 1 Wins!','Player 2 Wins!','Draw!','Player 2 Wins!','Player 1 Wins!','Player 1 Wins!','Player 2 Wins!','Player 1 Wins!','Draw!','Player 2 Wins!','Player 2 Wins!','Player 1 Wins!','Player 2 Wins!','Player 1 Wins!','Draw!'),nrow=5,ncol=5,dimnames=list(c("rock","paper","scissors","lizard","spock"),c("rock","paper","scissors","lizard","spock")))
    if(player1%in% ch && player2%in%ch){
        return(result[player1,player2])
    }
    else{
      stop('Please enter valid input')
    }
}

#1.2.1
my_moving_median<-function(x,n,...){
  y<-c()
  for(i in 1:(length(x)-n)){
    y[i]<-median(x[i:(i+n)],na.rm=...)
  }
  return(y)
}

#1.2.2
for_mult_table<-function(from,to){
  if(is.numeric(from) && is.numeric(to)){
    result<-matrix(1:(to-from+1)*(to-from+1),nrow=to-from+1,ncol=to-from+1,dimnames = list(c(from:to),c(from:to)))
    for(i in 1:to-from+1){
      for(j in 1:to-from+1){
        result[i,j]<-(i+from-1)*(j+from-1)
      }
    }
  }
  else{
    print('Please enter numeric values')
    stop()
  }
  return(result)
}

#1.3.1
find_cumsum<-function(x,find_sum){
  if(is.vector(x)&&is.numeric(find_sum)){
    i=1
    sume=0
    while(i<=length(x)){
      sume=sum(sume,x[i])
      if(sume>find_sum){
        break()
      }
      else
        i<-i+1
    }
  }
  else{
    print("Enter valid arguments")
    stop()
  }
  return(sume)
}

#1.3.2
while_mult_table<-function(from,to){
  if(is.numeric(from) && is.numeric(to)){
    result<-matrix(1:(to-from+1)*(to-from+1),nrow=to-from+1,ncol=to-from+1,dimnames = list(c(from:to),c(from:to)))
    i<-1
    while(i<=to-from+1){
      j<-1
      while(j<=to-from+1){
        result[i,j]<-(i+from-1)*(j+from-1)
        j<-j+1
      }
      i<-i+1
    }
  }
  else{
    stop("Please enter numeric values")
  }
  return(result)
}

#1.4.1
repeat_find_cumsum<-function(x,find_sum){
  i=1
  sume=0
  repeat{
    if(i<=length(x) && sume<=find_sum){
      sume<-sume+x[i]
      i<-i+1
    }
    else
      break()
  }
  return(sume)
}

#1.4.2
repeat_my_moving_median<-function(x,n,...){
  y<-c()
  i=1
  repeat{
    if(i<=(length(x)-n)){
      y[i]<-median(x[i:(i+n)],na.rm=...)
      i<-i+1
    }
    else
      break()
  }
  return(y)
}

#1.5.1
in_environment<-function(env){
  ls(env,all.names=TRUE)
}

#1.6.1
cov<-function(X){
  if(is.data.frame(X)){
    unlist(lapply(X,function(x)(sd(x)/mean(x))))
  }
  else{
    stop("Please check your Input")
  }
}

#1.7.1
moment<-function(i){
  res<-0
  stopifnot(is.numeric(i))
  function(x){
    stopifnot(is.numeric(x))
    res<-(1/length(x))*(sum((x-mean(x))^i))
    return(res)
  }
}

