#COMPRENSION DEL NEGOCIO

proyecto="C:\\Users\\ACER\\Desktop\\Proyecto analisis de datos sena\\Limpieza realizada Andres Felipe Jaramillo.csv"

p=read_delim(proyecto, delim= ';', escape_double = FALSE, trim_ws= TRUE, locale = locale(), col_names = TRUE)

p

#Saber los titulos de mi dataframe y saber variables a escoger
names(pf)



# <-"Predecir la mejor política gubernamental para la produccion y 
#venta del café evaluando las mejores prácticas para la economía 
#de los gremios cafeteros que a través del comportamiento histórico 
#de los últimos 3 periodos presidenciales en colombia tuvieron 
#impactos positivos y negativos, de los cuales se deben asumir
#las mejores decisiones para los próximos 3 años presidenciales."

#¿Determinar el mejor periodo presidencial en produccion de cafe? sacos de cafe producidos
#¿Determinar la mejor periodo presidencial en exportacion de cafe? sacos de cafe exportados
#¿Determinar el mejor periodo presidencial en venta en dolares de cafe? valor exportacion dolares
#¿Determinar el periodo que tenga el valor mas bajo de cosecha de cafe? valor cosecha pesos

#Realizar nuevo dataframe con las variables que se van a trabajar

pf=subset(p, select=c(Dia,Presidente,Sacos.de.cafe.producidos,Sacos.de.cafe.exportados,Valor.exportacion..Dolares.,Precio.interno.diario))

#visualizar nuevo dataframe
pf

#######################################################################################################

#COMPRENSION DE LOS DATOS

#Se realiza la limpieza de los datos bajo excel con el fin de optimizar los tiempos de tranbajo.


#Exploracion de las variables

#Exploracion observamos que quien recibio con la produccion mas baja 
#y aumento la produccion de cafe fue Juan Manuel Santos

fig1_sacos_producidos=plot_ly(pf, x=~Presidente, y=~Sacos.de.cafe.producidos,mode = 'lines')
fig1_sacos_producidos= fig1_sacos_producidos %>% layout(title = "Producción de cafe por presidente") 
fig1_sacos_producidos

#Produccion de cafe por año

Lineaproduccion=plot_ly(pf,x=~Dia, y=~Sacos.de.cafe.producidos,name = 'trace 0', type = 'scatter', mode = 'lines')
Lineaproduccion= Lineaproduccion %>% layout(title = "Producción de cafe por Año")
Lineaproduccion


#Grafico de barra presidente sacos de cafe exportados

barraexportado=plot_ly(pf, x=~Presidente, y=~Sacos.de.cafe.exportados,mode = 'lines')
barraexportado= barraexportado %>% layout(title = "Exportación de cafe por presidente") 
barraexportado

#Grafico de tiempo linea de cafe exportado
lineaexportacion=plot_ly(pf,x=~Dia, y=~Sacos.de.cafe.exportados,name = 'trace 0', type = 'scatter', mode = 'lines')
lineaexportacion= lineaexportacion %>% layout(title = "Exportación de cafe por Año")
lineaexportacion

#Linea de tiempo con valor de valor exportacion y valor de cosecha

lineaventaycosecha=plot_ly(pf, x=~Dia, y=~Valor.exportacion..Dolares.,name = 'Valor Exportacion', type = 'scatter', mode = 'lines' )
lineaventaycosecha= lineaventaycosecha%>% add_trace(y = ~Precio.interno.diario, name = 'Valor cosecha', mode = 'lines+markers')
lineaventaycosecha= lineaventaycosecha %>% layout(title = "Venta en dolares y valor cosecha en dolares")
lineaventaycosecha

#Analisis de tiempo valor cosecha
pcosecha=plot_ly(pf,x=~Dia, y=~Precio.interno.diario,name = 'Precio interno', type = 'scatter', mode = 'lines')
pcosecha= pcosecha %>% layout(title = "Precio de cosecha interno")
pcosecha

#Descripcion estadistica de la informacion
summary(pf)

#exportar el estadistico descriptivo en word
f=summary(pf)

capture.output(f,file="estadistico.doc")

#correlacion produccion cafe

substr(pf$Presidente,1,20)

santos1=filter(pf,Presidente=="Juan Manuel Santos (P1)")
santos1

lineasantos=plot(santos1$Sacos.de.cafe.exportados,santos1$Sacos.de.cafe.producidos,xlab='Exportacion', ylab='Produccion cafe') 
lineasantos + geom_smooth(method = "lm")


########################################################################

#MODELADO DE LOS DATOS


