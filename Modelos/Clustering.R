#SCRIPT MODELO K-MEANS

#Librería RMySQL
  library("RMySQL")

set.seed(2876)

#Conexión con la base de datos

  con <- dbConnect(RMySQL::MySQL(),
                 host="localhost",
                 dbname = "mineriabd",
                 user = "root",
                 password = "")


#Clustering para popularidad con 3-means

  #Obtenemos popularidades

    popularidad <- dbGetQuery(con, "SELECT popularity FROM cancion")

  #Aplicamos kmeans
    
    resultado<-kmeans(popularidad, 3)


#Analisis valence, liveness y acousticness con clustering en popularidad
    
  #Obtenemos los datos de valence, liveness y acousticness del dw
    
    data1<-dbGetQuery(con, "SELECT acousticness, liveness, valence FROM cancion")
    
    #data1<-dbGetQuery(con, "SELECT acousticness, liveness, valence FROM cancion ORDER BY rand() LIMIT 2000")
    
  #Paquete plotly
    
    install.packages("plotly")
    require(plotly)    
    
    library("RMySQL")
    
  #Gráficas de este análisis
    
    fig1<-plot_ly(data1, x = data1$liveness, y = data1$acousticness, z = data1$valence, color = resultado$cluster)
    fig1<-fig1 %>% add_markers()
    fig1 <-fig1 %>% layout(scene = list(xaxis = list(title = 'acousticness'), yaxis = list(title = 'liveness'), zaxis = list(title = 'valence')))
    
    #Mostrar gráfica
    fig1
    
#Análisis tempo-bailabilidad
    
  #Obtenemos los datos
    
    data2<-dbGetQuery(con, "SELECT tempo, danceability, popularity FROM cancion")

  #Graficas
    
    fig2<-plot_ly(data3, x = data2$tempo, y = data2$danceability, z = data2$popularity, color = resultado$cluster)
    fig2<-fig2 %>% add_markers()
    fig2 <-fig2 %>% layout(scene = list(xaxis = list(title = 'tempo'), yaxis = list(title = 'danceability'), zaxis = list(title = 'popularity')))
    
    #Mostrar gráfica
    fig2
    
#Análisis clave-modo
    
    data3<-dbGetQuery(con, "SELECT clave, mode, popularity FROM cancion")

    fig3<-plot_ly(data3, x = data3$clave, y = data3$mode, z = data3$popularity, color = resultado$cluster)
    fig3<-fig3 %>% add_markers()
    fig3 <-fig3 %>% layout(scene = list(xaxis = list(title = 'clave'), yaxis = list(title = 'mode'), zaxis = list(title = 'popularity')))
    
    #Mostrar gráfica
    fig3
    
#Análisis genre-clave
    
    data4<-dbGetQuery(con, "SELECT clave, genero, popularity FROM cancion")
    
    fig4<-plot_ly(data4, x = data4$clave, y = data4$genero, z = data4$popularity, color = resultado$cluster)
    fig4<-fig4 %>% add_markers()
    fig4 <-fig4 %>% layout(scene = list(xaxis = list(title = 'clave'), yaxis = list(title = 'genero'), zaxis = list(title = 'popularity')))
    
    #Mostrar gráfica
    fig4
    
#Analisis speechiness-genero
    
    data5<-dbGetQuery(con, "SELECT speechiness, genero, popularity FROM cancion")

    fig5<-plot_ly(data5, x = data5$genero, y = data5$speechiness, z = data5$popularity, color = resultado$cluster)
    fig5<-fig5 %>% add_markers()
    fig5 <-fig5 %>% layout(scene = list(xaxis = list(title = 'genero'), yaxis = list(title = 'speechiness'), zaxis = list(title = 'popularity')))
    
    #Mostrar gráfica
    fig5

#Analisis energia-generos
    
    data6<-dbGetQuery(con, "SELECT energy, genero, popularity FROM cancion")
    
    fig6<-plot_ly(data6, x = data6$genero, y = data6$energy, z = data6$popularity, color = resultado$cluster)
    fig6<-fig6 %>% add_markers()
    fig6 <-fig6 %>% layout(scene = list(xaxis = list(title = 'genero'), yaxis = list(title = 'energy'), zaxis = list(title = 'popularity')))
    
    #Mostrar gráfica
    fig6
  
  #Sub-analisis R&B
    
    #Necesitamos realizar clustering solo con las popularidades de R&B
    
    data7<-dbGetQuery(con, "SELECT energy, genero, popularity FROM cancion WHERE genero = 'R&B'")
    result2<-kmeans(data7$popularity, 3)
    
    fig7<-plot_ly(data7, x = data7$genero, y = data7$energy, z = data7$popularity, color = result2$cluster)
    fig7<-fig7 %>% add_markers()
    fig7 <-fig7 %>% layout(scene = list(xaxis = list(title = 'genero'), yaxis = list(title = 'energy'), zaxis = list(title = 'popularity')))
    
    #Mostrar gráfica
    fig7
    
  #Sub-analisis Reggaeton
    
    #De igual forma que antes, será necesario realizar clustering con las popularidades de Reggaeton
    
    data8<-dbGetQuery(con, "SELECT energy, genero, popularity FROM cancion WHERE genero = 'Reggaeton'")
    result3<-kmeans(data8$popularity, 3)
    
    fig8<-plot_ly(data8, x = data8$genero, y = data8$energy, z = data8$popularity, color = result3$cluster)
    fig8<-fig8 %>% add_markers()
    fig8 <-fig8 %>% layout(scene = list(xaxis = list(title = 'genero'), yaxis = list(title = 'energy'), zaxis = list(title = 'popularity')))
    
    #Mostrar gráfica
    fig8

    
  #Sub-analisis artista Ozuna
    
    data9<-dbGetQuery(con, "SELECT energy, genero, popularity FROM cancion WHERE artista = 'Ozuna'")
    result4<-kmeans(data9$popularity, 3)
    
    fig9<-plot_ly(data9, x = data9$genero, y = data9$energy, z = data9$popularity, color = result4$cluster)
    fig9<-fig9 %>% add_markers()
    fig9 <-fig9 %>% layout(scene = list(xaxis = list(title = 'genero'), yaxis = list(title = 'energy'), zaxis = list(title = 'popularity')))
    
    #Mostrar gráfica
    fig9
    