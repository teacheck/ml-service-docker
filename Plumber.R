library(e1071)
library(caret)

#* The service for receiving students data, make the prediction about their status and return the results
#* @param data. A json array representing all the alumns from a class for the prediction.
#* @post /predict
function(data){
  modeloSemanal<-paste("modelos/modeloSemana",data$Semana[1],".rds",sep="")
  fit<-readRDS(modeloSemanal)
  
  #Convert data into the proper format
  data$EntregableC11 <- as.numeric(data$EntregableC11)
  data$Criterios <- as.numeric(data$Criterios)
  data$NotaC11 <- as.numeric(data$NotaC11)
  data$NotaEntregableC11 <- as.numeric(data$NotaEntregableC11)
  
  #Make the prediction
  predictions <- predict(fit, data[,3:11])
  predictions <- as.numeric(predictions)
  
  #Make correlations(all the students)
  data$Criterios <- predictions
  
  data[is.na(data)] <- 0
  cor1<-cor(data[12], data[3:11], method="kendall")
  
  #Keep the correlations into another variable to be returned
  resultsCor <- data.frame(matrix(ncol = 10, nrow = 1))
  resultsPred  <- data.frame(matrix(ncol=2,nrow=60))
  
  corResult <- c("Semana","corAsistencia","corNotaC11","corEntregableC11","corNotaEntregableC11","corHoras1","corAtencionP1","corMotivacionP1","corAtencionA1","corMotivacionA1")
  predictionsResult <- c("AlumnoId","predictions")
  
  colnames(resultsCor) <- corResult
  colnames(resultsPred) <- predictionsResult

  resultsPred$AlumnoId <- data$AlumnoID
  resultsPred$predictions <-predictions
  
  resultsCor$Semana <-data$Semana[1]
  resultsCor$corAsistencia <- cor1[1]
  resultsCor$corNotaC11 <- cor1[2]
  resultsCor$corEntregableC11 <- cor1[3]
  resultsCor$corNotaEntregableC11 <- cor1[4]
  resultsCor$corHoras1 <- cor1[5]
  resultsCor$corAtencionP1 <- cor1[6]
  resultsCor$corMotivacionP1 <- cor1[7]
  resultsCor$corAtencionA1 <- cor1[8]
  resultsCor$corMotivacionA1 <- cor1[9]
  
  
  #TotalResults = c(resultsPred,resultsCor)
  TotalResults = list(resultsPred,resultsCor)

  return(TotalResults)
}
