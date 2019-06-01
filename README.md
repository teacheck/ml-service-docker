# ml-service-docker

## Utilización
`docker run -p 8000:8000 --name ml devallday/teacheck-ml-service` Esto
lanzara un contenedor que a su vez es un servidor de Plumber
escuchando en el puerto 8000.

El servicio solo provee un endpoint, `/predict`. Para probar el servicio
haz una llamada POST a la seguiente dirección localhost:8000/predict y en el body utilizar el `example-data.json` que se puede encontrar en este repo.

