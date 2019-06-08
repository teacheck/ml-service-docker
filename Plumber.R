library(e1071)
library(caret)

#* The service for receiving students data, make the prediction about their status and return the results
#* @param data JsonObject representing all alumns class
#* @post /predict
function(data){
  modeloSemanal<-paste("modelos/modeloSemana",data$Semana[1],".rds",sep="")
  fit<-readRDS(modeloSemanal)
  extension <- ""
  if(data$Semana[1] == 2)extension <- ".5"
  if(data$Semana[1] == 3)extension <- ".10"
  if(data$Semana[1] == 4)extension <- ".15"
  if(data$Semana[1] == 5)extension <- ".20"  
  if(data$Semana[1] == 6)extension <- ".25"
  if(data$Semana[1] == 7)extension <- ".30"
  if(data$Semana[1] == 8)extension <- ".35"
  if(data$Semana[1] == 9)extension <- ".5"
  if(data$Semana[1] == 10)extension <- ".5"
  
  names(data)[names(data) == "Asistencia"] <- paste("Asistencia",extension,sep="")
  names(data)[names(data) == "NotaC11"] <- paste("NotaC11",extension,sep="")
  names(data)[names(data) == "EntregableC11"] <- paste("EntregableC11",extension,sep="")
  names(data)[names(data) == "NotaEntregableC11"] <- paste("NotaEntregableC11",extension,sep="")
  names(data)[names(data) == "Horas1"] <- paste("Horas1",extension,sep="")
  names(data)[names(data) == "AtencionA1"] <- paste("AtencionA1",extension,sep="")
  names(data)[names(data) == "MotivacionA1"] <- paste("MotivacionA1",extension,sep="")
  names(data)[names(data) == "AtencionP1"] <- paste("AtencionP1",extension,sep="")
  names(data)[names(data) == "MotivacionP1"] <- paste("MotivacionP1",extension,sep="")
  names(data)[names(data) == "Criterios"] <- paste("Criterios",extension,sep="")
  
  #Convert data into the proper format

  #data[12] <- as.numeric(data[12])
  
  #Make the prediction
  predictions <- predict(fit, data[,3:11])
  predictions <- as.numeric(predictions)
  #Make correlations(all the students)
  data$Criterios.5 <- predictions
  
  data$EntregableC11.5 <- as.numeric(data$EntregableC11.5)
  data$NotaEntregableC11.5 <- as.numeric(data$NotaEntregableC11.5)
  data$Criterios.5 <- as.numeric(data$Criterios.5)
  data$NotaC11.5 <- as.numeric(data$NotaC11.5)
  data$Horas1.5 <- as.numeric(data$Horas1.5)
  data$Asistencia.5 <- as.numeric(data$Asistencia.5)
  data$AtencionP1.5 <- as.numeric(data$AtencionP1.5)
  data$AtencionA1.5 <- as.numeric(data$AtencionA1.5)
  data$MotivacionP1.5 <- as.numeric(data$MotivacionP1.5)
  data$MotivacionA1.5 <- as.numeric(data$MotivacionA1.5)
  
  data[is.na(data)] <- 0
  cor1<-cor(data[12], data[3:11], method="kendall")

  #Keep the correlations into another variable to be returned
  resultsCor <- data.frame(matrix(ncol = 10, nrow = 1))
  resultsPred  <- data.frame(matrix(ncol=2,nrow=60))
  
  corResult <- c("Semana","corAsistencia","corNotaC11","corEntregableC11","corNotaEntregableC11","corHoras1","corAtencionP1","corMotivacionP1","corAtencionA1","corMotivacionA1")
  predictionsResult <- c("AlumnoId","predictions")
  
  colnames(resultsCor) <- corResult
  colnames(resultsPred) <- predictionsResult

  resultsPred$AlumnoId <- data[1]
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
