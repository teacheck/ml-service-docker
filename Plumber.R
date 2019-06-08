library(e1071)
library(caret)

#* The service for receiving students data, make the prediction about their status and return the results
#* @param data The message to echo
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
  
  if(data$Semana[1] == 2){
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
    data$Criterios.5 <- predictions
  }else if(data$Semana[1] == 3){
    data$EntregableC11.10 <- as.numeric(data$EntregableC11.10)
    data$NotaEntregableC11.10 <- as.numeric(data$NotaEntregableC11.10)
    data$Criterios.10 <- as.numeric(data$Criterios.10)
    data$NotaC11.10 <- as.numeric(data$NotaC11.10)
    data$Horas1.10 <- as.numeric(data$Horas1.10)
    data$Asistencia.10 <- as.numeric(data$Asistencia.10)
    data$AtencionP1.10 <- as.numeric(data$AtencionP1.10)
    data$AtencionA1.10 <- as.numeric(data$AtencionA1.10)
    data$MotivacionP1.10 <- as.numeric(data$MotivacionP1.10)
    data$MotivacionA1.10 <- as.numeric(data$MotivacionA1.10)
    data$Criterios.10 <- predictions
    
  }else if(data$Semana[1] == 4){
    data$EntregableC11.15 <- as.numeric(data$EntregableC11.15)
    data$NotaEntregableC11.15 <- as.numeric(data$NotaEntregableC11.15)
    data$Criterios.15 <- as.numeric(data$Criterios.15)
    data$NotaC11.15 <- as.numeric(data$NotaC11.15)
    data$Horas1.15 <- as.numeric(data$Horas1.15)
    data$Asistencia.15 <- as.numeric(data$Asistencia.15)
    data$AtencionP1.15 <- as.numeric(data$AtencionP1.15)
    data$AtencionA1.15 <- as.numeric(data$AtencionA1.15)
    data$MotivacionP1.15 <- as.numeric(data$MotivacionP1.15)
    data$MotivacionA1.15 <- as.numeric(data$MotivacionA1.15)
    data$Criterios.15 <- predictions
    
  }else if(data$Semana[1] == 5){
    data$EntregableC11.20 <- as.numeric(data$EntregableC11.20)
    data$NotaEntregableC11.20 <- as.numeric(data$NotaEntregableC11.20)
    data$Criterios.20 <- as.numeric(data$Criterios.20)
    data$NotaC11.20 <- as.numeric(data$NotaC11.20)
    data$Horas1.20 <- as.numeric(data$Horas1.20)
    data$Asistencia.20 <- as.numeric(data$Asistencia.20)
    data$AtencionP1.20 <- as.numeric(data$AtencionP1.20)
    data$AtencionA1.20 <- as.numeric(data$AtencionA1.20)
    data$MotivacionP1.20 <- as.numeric(data$MotivacionP1.20)
    data$MotivacionA1.20 <- as.numeric(data$MotivacionA1.20)
    data$Criterios.20 <- predictions
    
  }else if(data$Semana[1] == 6){
    data$EntregableC11.25 <- as.numeric(data$EntregableC11.25)
    data$NotaEntregableC11.25 <- as.numeric(data$NotaEntregableC11.25)
    data$Criterios.25 <- as.numeric(data$Criterios.25)
    data$NotaC11.25 <- as.numeric(data$NotaC11.25)
    data$Horas1.25 <- as.numeric(data$Horas1.25)
    data$Asistencia.25 <- as.numeric(data$Asistencia.25)
    data$AtencionP1.25 <- as.numeric(data$AtencionP1.25)
    data$AtencionA1.25 <- as.numeric(data$AtencionA1.25)
    data$MotivacionP1.25 <- as.numeric(data$MotivacionP1.25)
    data$MotivacionA1.25 <- as.numeric(data$MotivacionA1.25)
    data$Criterios.25 <- predictions
    
  }else if(data$Semana[1] == 7){
    data$EntregableC11.30 <- as.numeric(data$EntregableC11.30)
    data$NotaEntregableC11.30 <- as.numeric(data$NotaEntregableC11.30)
    data$Criterios.30 <- as.numeric(data$Criterios.30)
    data$NotaC11.30 <- as.numeric(data$NotaC11.30)
    data$Horas1.30 <- as.numeric(data$Horas1.30)
    data$Asistencia.30 <- as.numeric(data$Asistencia.30)
    data$AtencionP1.30 <- as.numeric(data$AtencionP1.30)
    data$AtencionA1.30 <- as.numeric(data$AtencionA1.30)
    data$MotivacionP1.30 <- as.numeric(data$MotivacionP1.30)
    data$MotivacionA1.30 <- as.numeric(data$MotivacionA1.30)
    data$Criterios.30 <- predictions
    
  }else if(data$Semana[1] == 8){
    data$EntregableC11.35 <- as.numeric(data$EntregableC11.35)
    data$NotaEntregableC11.35 <- as.numeric(data$NotaEntregableC11.35)
    data$Criterios.35 <- as.numeric(data$Criterios.35)
    data$NotaC11.35 <- as.numeric(data$NotaC11.35)
    data$Horas1.35 <- as.numeric(data$Horas1.35)
    data$Asistencia.35 <- as.numeric(data$Asistencia.35)
    data$AtencionP1.35 <- as.numeric(data$AtencionP1.35)
    data$AtencionA1.35 <- as.numeric(data$AtencionA1.35)
    data$MotivacionP1.35 <- as.numeric(data$MotivacionP1.35)
    data$MotivacionA1.35 <- as.numeric(data$MotivacionA1.35)
    data$Criterios.35 <- predictions
    
  }else if(data$Semana[1] == 9){
    data$EntregableC11.40 <- as.numeric(data$EntregableC11.40)
    data$NotaEntregableC11.40 <- as.numeric(data$NotaEntregableC11.40)
    data$Criterios.40 <- as.numeric(data$Criterios.40)
    data$NotaC11.40 <- as.numeric(data$NotaC11.40)
    data$Horas1.40 <- as.numeric(data$Horas1.40)
    data$Asistencia.40 <- as.numeric(data$Asistencia.40)
    data$AtencionP1.40 <- as.numeric(data$AtencionP1.40)
    data$AtencionA1.40 <- as.numeric(data$AtencionA1.40)
    data$MotivacionP1.40 <- as.numeric(data$MotivacionP1.40)
    data$MotivacionA1.40 <- as.numeric(data$MotivacionA1.40)
    data$Criterios.40 <- predictions
    
  }else if(data$Semana[1] == 10){
    data$EntregableC11.45 <- as.numeric(data$EntregableC11.45)
    data$NotaEntregableC11.45 <- as.numeric(data$NotaEntregableC11.45)
    data$Criterios.45 <- as.numeric(data$Criterios.45)
    data$NotaC11.45 <- as.numeric(data$NotaC11.45)
    data$Horas1.45 <- as.numeric(data$Horas1.45)
    data$Asistencia.45 <- as.numeric(data$Asistencia.45)
    data$AtencionP1.45 <- as.numeric(data$AtencionP1.45)
    data$AtencionA1.45 <- as.numeric(data$AtencionA1.45)
    data$MotivacionP1.45 <- as.numeric(data$MotivacionP1.45)
    data$MotivacionA1.45 <- as.numeric(data$MotivacionA1.45)
    data$Criterios.45 <- predictions
    
  }
  
  data[is.na(data)] <- 0
  cor1<-cor(data[12], data[3:11], method="kendall")
  
  #Keep the correlations into another variable to be returned
  resultsCor <- data.frame(matrix(ncol = 10, nrow = 1))
  resultsPred  <- data.frame(matrix(ncol=2,nrow=60))
  
  corResult <- c("Semana","corAsistencia","corNotaC11","corEntregableC11","corNotaEntregableC11","corHoras1","corAtencionP1","corMotivacionP1","corAtencionA1","corMotivacionA1")
  predictionsResult <- c("AlumnoID","predictions")
  
  colnames(resultsCor) <- corResult
  colnames(resultsPred) <- predictionsResult
  
  resultsPred$AlumnoID <- data$AlumnoID
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
