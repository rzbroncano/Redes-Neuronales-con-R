x <- data_nand[, c(2, 3)]
x[,3] <- -1
y <- data_nand[,4]

#tasa de aprendizaje
n <- 0.1

epocas <- 10
#funcion escalon
hardlim <-function(x) {
  y <- ifelse(x >= 0, 1, 0) 
  return (y);
}
#funcion perceptron unicapa o simple
perceptron <- function(x, y,n,nepoc) {
  weight <- rep(0, dim(x)[2] + 1)
  errors <- rep(0, nepoc)
  ei <- rep(0, dim(x)[1])
  for (epoch in 1:nepoc){
    for (q in 1:dim(y)[1]){
      ei[q] <- y[q,] -hardlim(sum(x[q,]*weight))
      weight <- weight + x[q,]*n*ei[q]
      if (ei[q] != 0.0) {
        errors[epoch] <- errors[epoch] + 1
      }
    }
  }
  print(weight)
  return (errors)
}

#se muestra un grafico con las cantidad de errores por epoca
err <- perceptron(x,y,n,epocas)
plot(1:epocas, err, type="l", lwd=1, col="red", xlab="epoch #", ylab="errors")
title("#Errores vs #epoca - tasa de aprendizaje = 0.1")


