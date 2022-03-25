# CARGAR LIBRERIAS ----
library(tidyr)
library(dplyr)
library(haven)
library(expss)
library(purrr)
library(magrittr)
library(stringr)
library(readxl)
library(foreign)
library(purrr)
library(tidyselect)
library(ggplot2)
library(ggrepel)
library(gridExtra)


# RESUMENES NUMERICOS -----------------------------------------------------
resumen.vars <- function(vars, base){
  
  resumen <- function(var, base){
    if(is.numeric(getElement(base,var))){
      
      
      tibble("Variable"=var,
             "Rango"=round(max(getElement(base,var),na.rm = TRUE),2)-round(min(getElement(base,var),na.rm = TRUE),2),
             "Promedio"=round(mean(getElement(base,var),na.rm = TRUE),2),
             "Mediana"=round(median(getElement(base,var),na.rm=TRUE),2),
             "Desv Est"=round(sd(getElement(base,var),na.rm = TRUE),2),
             "Perdidos"=sum(is.na(getElement(base,var))),
             "Etiqueta"= attr(getElement(base,var), "label"))
      
    }  
  } 
  
  lapply(vars, resumen, base)%>%
    bind_rows()
  
}


# SIGNIFICANCIA RELACIONES BIVARIADAS -------------------------------------

resumen <- function(var){
  var.md <- as.name(var)
  if(is.factor(getElement(base_modelo,var))){
    tabla <- tibble("Variable"=var,
                    "Etiqueta"=attr(getElement(base_modelo,var),"label"),
                    "p.value"=round(as.numeric(glance(lm(aov(s11_phq9~!!var.md,data = base_modelo)))$p.value),4),
                    "Función"="AOV")
  }else{
    tabla <- tibble("Variable"=var,
                    "Etiqueta"=attr(getElement(base_modelo,var),"label"),
                    "p.value"=round(as.numeric(glance(lm(s11_phq9~!!var.md,data = base_modelo))$p.value),4),
                    "Función"="LM")
  }
  
  return(tabla)
  
}

#### Tablas de contingencia ponderadas y con test bonferroni de diferencia de medias
tab_cpct_sig = . %>% tab_stat_cpct() %>% tab_last_sig_cpct(bonferroni = TRUE)

sig_w03 <- function(base,var_banner,variable){
  banner_1 <- base %>% tab_cols(total(),var_banner) %>% tab_weight(weight = ponderador02_w03)
  tabla_spss <- banner_1 %>% tab_cells(variable) %>% tab_cpct_sig %>% tab_pivot()
  tabla_spss <- tibble(tabla_spss)
  tabla_spss <- tabla_spss %>% pivot_longer(cols = -1, names_to = "variables", values_to = "values")
  tabla_spss <- tabla_spss %>% separate(values, into = c("values", "significancia"), sep = 4, convert = TRUE)
  tabla_spss <- tabla_spss %>% filter(variables != "#Total")
  tabla_spss <- tabla_spss %>% mutate(significancia = str_trim(significancia),
                                      significancia = case_when(significancia == "" ~ "",
                                                                TRUE ~ "*"))
  tabla_spss <- tabla_spss %>% separate(row_labels, into = c("borrar", "categoria"), sep = "([\\.\\|\\:])", convert = TRUE) %>% select(-borrar)
  tabla_spss <- tabla_spss %>% separate(variables, into = c("b_1", "variables","b_2"), sep = "([\\.\\|\\:])", convert = TRUE) %>% select(-starts_with("b_"))
  tabla_spss <- tabla_spss %>% filter(categoria != "#Total cases")
  return(tabla_spss)
}

sig_w04 <- function(base,var_banner,variable){
  banner_1 <- base %>% tab_cols(total(),var_banner) %>% tab_weight(weight = ponderador02_w04)
  tabla_spss <- banner_1 %>% tab_cells(variable) %>% tab_cpct_sig %>% tab_pivot()
  tabla_spss <- tibble(tabla_spss)
  tabla_spss <- tabla_spss %>% pivot_longer(cols = -1, names_to = "variables", values_to = "values")
  tabla_spss <- tabla_spss %>% separate(values, into = c("values", "significancia"), sep = 4, convert = TRUE)
  tabla_spss <- tabla_spss %>% filter(variables != "#Total")
  tabla_spss <- tabla_spss %>% mutate(significancia = str_trim(significancia),
                                      significancia = case_when(significancia == "" ~ "",
                                                                TRUE ~ "*"))
  tabla_spss <- tabla_spss %>% separate(row_labels, into = c("borrar", "categoria"), sep = "([\\.\\|\\:])", convert = TRUE) %>% select(-borrar)
  tabla_spss <- tabla_spss %>% separate(variables, into = c("b_1", "variables","b_2"), sep = "([\\.\\|\\:])", convert = TRUE) %>% select(-starts_with("b_"))
  tabla_spss <- tabla_spss %>% filter(categoria != "#Total cases")
  return(tabla_spss)
}
sig_w05 <- function(base,var_banner,variable){
  banner_1 <- base %>% tab_cols(total(),var_banner) %>% tab_weight(weight = ponderador02_w05)
  tabla_spss <- banner_1 %>% tab_cells(variable) %>% tab_cpct_sig %>% tab_pivot()
  tabla_spss <- tibble(tabla_spss)
  tabla_spss <- tabla_spss %>% pivot_longer(cols = -1, names_to = "variables", values_to = "values")
  tabla_spss <- tabla_spss %>% separate(values, into = c("values", "significancia"), sep = 4, convert = TRUE)
  tabla_spss <- tabla_spss %>% filter(variables != "#Total")
  tabla_spss <- tabla_spss %>% mutate(significancia = str_trim(significancia),
                                      significancia = case_when(significancia == "" ~ "",
                                                                TRUE ~ "*"))
  tabla_spss <- tabla_spss %>% separate(row_labels, into = c("borrar", "categoria"), sep = "([\\.\\|\\:])", convert = TRUE) %>% select(-borrar)
  tabla_spss <- tabla_spss %>% separate(variables, into = c("b_1", "variables","b_2"), sep = "([\\.\\|\\:])", convert = TRUE) %>% select(-starts_with("b_"))
  tabla_spss <- tabla_spss %>% filter(categoria != "#Total cases")
  return(tabla_spss)
}




# GRAFICOS ----------------------------------------------------------------

grafo<-function(var,ancho=20){
  
  var<- as.name(var)
  ggp<- ggplot(elsoc_2016,aes(x=factor(!!var), y=s11_phq9))+
    geom_boxplot()+
    labs(title=element_blank(),
         y="Puntaje PHQ-9",
         x=element_blank())+
    theme_classic()+
    scale_x_discrete(labels=stringr::str_wrap(levels(getElement(elsoc_2016,var)), width = ancho),na.translate=FALSE)
  
  return(ggp)
}

grafo.num<-function(var){
  var<-as.name(var)
  ggp<-ggplot(elsoc_2016,aes(x=!!var,y=s11_phq9))+
    geom_point()+
    labs(title =element_blank(),
         y="Puntaje Escala PHQ-9",
         x=element_blank())+
    theme_classic()
  return(ggp)
  
}


# R NIVEL DEL FACTOR TAUS -------------------------------------------------

tau.r<- function(mod,var){
  tau<- -sum(coefficients(mod)[grepl(names(coefficients(mod)),pattern = gsub(x=var,pattern = "_fac",replacement = ""))])
  contrastes<-contrasts(getElement(base_modelo,var))
  
  tabla<- tibble("Variable"=var,
                 "Nivel"=row.names(contrastes)[nrow(contrastes)],
                 "Coeficiente"=round(tau,4))
  return(tabla)
}


# ALGORITMO SELECCION FW --------------------------------------------------

seleccion_fw<-function(base,var_respuesta,vars_predictoras,contr.suma=TRUE){
  
  # SELECCIONAR VARIABLES Y BASE
  base_step <- select(base,!!var_respuesta,!!vars_predictoras )%>% drop_na()
  
  #PASAR A CONTRASTE SUMA LOS FACTORES
  if(contr.suma){
    factores <- which(sapply(1:ncol(base_step),function(i){is.factor(getElement(base_step,i))}))
    
    for(i in factores){
      contrasts(base_step[,i])<- contr.sum
    }
  }
  
  # HORIZONTE DE VARIABLES
  vars_names<-names(base_step[,-1])
  horizonte <- as.formula(paste("~",paste(vars_names,collapse = "+")))
  
  # OBTENER PRIMEROS DOS VALORES
  mod_step <- lm(as.formula(paste(paste(var_respuesta,"~"),1)),base_step)
  paso1<- add1(mod_step,horizonte,data=base_step,test = "F")
  
  sig<- sum(paso1$`Pr(>F)`< 0.05,na.rm = TRUE)
  uno<- row.names(paso1)[which.min(paso1$`Pr(>F)`)]
  #MENSAJE EN CONSOLA()  
  print(paste("Paso", 1,": Entra",uno,".Con ",sig,"variables significativas de",nrow(paso1)-1,"ingresadas"))
  
  mod_step <- lm(as.formula(paste(paste(var_respuesta,"~"),uno)),base_step)
  paso1<- add1(mod_step,horizonte,data=base_step,test = "F")
  sig<- sum(paso1$`Pr(>F)`< 0.05,na.rm = TRUE)
  dos<- row.names(paso1)[which.min(paso1$`Pr(>F)`)]
  # MENSAJE EN CONSOLA
  print(paste("Paso", 2,": Entra",dos,".Con ",sig,"variables significativas de",nrow(paso1)-1,"ingresadas"))
  # VALORES PARA INICIALIZAR
  vars_pred<- uno
  minimo <- dos
  
  i <- 2
  # WHILE LOOP
  while (sig >0){
    i <- i+1
    vars_pred<- c(vars_pred,minimo)
    mod_step <- lm(as.formula(paste(paste(var_respuesta,"~"),paste(vars_pred,collapse = "+"))),base_step)
    paso1<- add1(mod_step,horizonte,data=base_step,test = "F")
    
    sig<- sum(paso1$`Pr(>F)`< 0.05,na.rm = TRUE)
    sig
    minimo<- row.names(paso1)[which.min(paso1$`Pr(>F)`)]
    minimo
    if(sig !=0){
      print(paste("Paso", i,": Entra",minimo,".Con ",sig,"variables significativas de",nrow(paso1)-1,"ingresadas"))
    }
    
  }
  mod_final <- lm(as.formula(paste(paste(var_respuesta,"~"),paste(vars_pred,collapse = "+"))),base_step) 
  
  return(mod_final)
}


# SELECCION BACKWARD ------------------------------------------------------

seleccion_bw <- function(base,var_respuesta,vars_predictoras){
  base_step <- select(base, !!var_respuesta,!!vars_predictoras)%>% drop_na()
  # PREDICTORAS
  vars_names<-names(base_step[,-1])
  
  
  # PASO UNO
  mod_completo<- lm(as.formula(paste(paste(var_respuesta,"~"),paste(vars_names,collapse = "+"))),data=base_step)
  pasoB<- drop1(mod_completo,test = "F")
  sig <- sum(pasoB$`Pr(>F)`< 0.05,na.rm = TRUE)
  no.sig <- sum(pasoB$`Pr(>F)`> 0.05,na.rm = TRUE)
  maximo <-row.names(pasoB)[which.max(pasoB$`Pr(>F)`)]
  print(paste("Paso", 1,": Sale",maximo,".Con ",sig,"variables significativas de",nrow(pasoB)-1,"ingresadas"))
  
  # WHILE LOOP
  i<- 1
  while(no.sig>0){
    i <-i+1
    vars_names<- vars_names[vars_names != maximo]
    mod_completo<- lm(as.formula(paste(paste(var_respuesta,"~"),paste(vars_names,collapse = "+"))),data=base_step)
    pasoB<- drop1(mod_completo,test = "F")
    sig <- sum(pasoB$`Pr(>F)`< 0.05,na.rm = TRUE)
    no.sig <- sum(pasoB$`Pr(>F)`> 0.05,na.rm = TRUE)
    maximo <-row.names(pasoB)[which.max(pasoB$`Pr(>F)`)]
    if(no.sig !=0){
      print(paste("Paso", i,": Sale",maximo,".Con ",sig,"variables significativas de",nrow(pasoB)-1,"ingresadas"))
    }
    
  }
  return(mod_completo)
}
# GRAFICO PARA NORMALIDAD
grafo_normalidad <- function(modelo){
  tabla <- tibble("id" = 1:nrow(model.matrix(modelo)),
                  "r" = ls.diag(modelo)$std.res,
                  "y" = modelo$model$s11_phq9)
  
  ggp <- ggplot(tabla, aes(sample = r)) + 
    stat_qq() + 
    stat_qq_line(color = "skyblue", lty = 2, lwd = 1) +
    labs(title = "QQnorm de los residuos estandarizados",
         x = "Cuantiles teóricos",
         y = "Cuantiles muestrales") + 
    theme_classic() + 
    theme(plot.title = element_text(size = 13L, hjust = 0.5))
  
  return(ggp)
}

# GRAFICO LINEALIDAD DE LA MEDIA Y HOMOCEDASTICIDAD

grafo_homocedasticidad <- function(modelo){
  tabla <- tibble("t" = ls.diag(modelo)$stud.res,
                  "y_gorro" = modelo$fitted.values)
  ggp <- ggplot(tabla,aes(x = y_gorro, y = t))+
    geom_point() +
    labs(y="Residuos estudentizados",
         x="Valores ajustados", 
         title = "Gráfico de dispersión", 
         subtitle = "y_gorro v/s t")+
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5), 
          plot.subtitle = element_text(face = "bold.italic", hjust = 0.5)) +
    geom_abline(intercept = 0, slope = 0, col = "red", lty = 2) 
  return(ggp)
}

grafo_predictor <- function(modelo, predictor){
  tabla <- tibble("t" = ls.diag(modelo)$stud.res,
                  "x" = modelo$model[, predictor])
  if (class(tabla$x) == "factor"){
    ggp <- ggplot(tabla,aes(x = x, y = t))+
      geom_boxplot() +
      labs(y="Residuos estudentizados",
           x=paste0(predictor), 
           title = "Gráfico de boxplot", 
           subtitle = paste0(predictor," v/s t")) +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5), 
            plot.subtitle = element_text(face = "bold.italic", hjust = 0.5)) +
      geom_abline(intercept = 0, slope = 0, col = "red", lty = 2) 
  }else{
    ggp <- ggplot(tabla,aes(x = x, y = t))+
      geom_point() +
      labs(y="Residuos estudentizados",
           x=paste0(predictor), 
           title = "Gráfico de dispersión", 
          subtitle = paste0(predictor," v/s t"))+
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5), 
            plot.subtitle = element_text(face = "bold.italic", hjust = 0.5)) +
      geom_abline(intercept = 0, slope = 0, col = "red", lty = 2) 
  }
  return(ggp)
}

# GRAFICO PARA OUTLIERS
grafo_out <- function(modelo){
  # DIAGNOSTICOS
  diag_modelo <- ls.diag(modelo)
  
  #AGREGAR ID A LA BASE
  tabla <- tibble("id"=1:nrow(model.matrix(modelo)),
                  "ajustados"=modelo$fitted.values,
                  "res_estud"=diag_modelo$stud.res)
  #DEFINIR UMBRAL
  umbral_stud <-qt(0.975,nrow(tabla)-length(coefficients(modelo)) - 1) 
  
  tabla$out_stud <- abs(tabla$res_estud) > umbral_stud
  
  # GRAFICO
  ggp <- tabla %>%
    ggplot(aes(x=ajustados,y=res_estud))+
    ylim(c(-4,4))+
    geom_hline(yintercept=umbral_stud,lty=2, col="#CB4335")+
    geom_hline(yintercept=-umbral_stud, lty=2,col="#CB4335")+
    labs(y="Residuos estudentizados",
         x="Valores ajustados",
         title = "Gráfico de dispersión",
         subtitle = "y_gorro v/s t") +
    geom_point(shape=1,aes(color=out_stud))+
    scale_color_manual(values = c("#5DADE2", "#58D68D", "#CB4335")) +
    theme_classic()+
    theme(plot.title = element_text(hjust = 0.5), 
          plot.subtitle = element_text(face = "bold.italic", hjust = 0.5),
          legend.position = "none")
  return(ggp)
}


#GRAFICO PARA PUNTOS INFLUYENTES 
grafo_leverage <- function(modelo){
  # DIAGNOSTICOS
  diag_modelo <- ls.diag(modelo)
  #AGREGAR ID A LA BASE
  tabla <- tibble("id" = 1:nrow(model.matrix(modelo)),
                  "y_gorro" = modelo$fitted.values,
                  "e" = modelo$residuals,
                  "r" = diag_modelo$std.res,
                  "t"= diag_modelo$stud.res,
                  "hii" = diag_modelo$hat,
                  "cooks" = diag_modelo$cooks,
                  "dfits" = diag_modelo$dfits)
  n <- nrow(tabla)
  p <- length(coefficients(modelo))
  #DEFINIR UMBRAL
  umbral_leverage <- 2*p/n
  
  tabla$out_leverage <- (tabla$hii > umbral_leverage)
  max_hii <- sort(tabla$hii, decreasing  = TRUE)[1:6]
  indices_leverage <- which(tabla$hii %in% max_hii)
  puntos_leverage <- tabla[indices_leverage,] %>%
    mutate(tipo = "Alto leverage")
  ggp <- tabla %>%
    ggplot(aes(x=hii, y = t,color=out_leverage))+
    geom_vline(xintercept=umbral_leverage,lty=2, col="#CB4335")+
    labs(y="Residuos estudentizados",
         x="Leverage",
         title = "Gráfico de dispersión",
         subtitle = "hii v/s t")+
    geom_point(shape=1,aes(color=out_leverage))+
    scale_color_manual(values = c("#58D68D", "#5DADE2", "#58D68D")) +
    theme_classic()+
    theme(plot.title = element_text(hjust = 0.5), 
          plot.subtitle = element_text(face = "bold.italic", hjust = 0.5),
          legend.position = "none") + 
    geom_point(data = puntos_leverage, 
               aes(x = hii, y = t, fill = tipo, col = tipo), 
               alpha=0.7)+
    geom_text(data = puntos_leverage,
              aes(x = hii, y = t, label=as.character(id)),
              vjust = -1, parse = TRUE)
  
  return(ggp)
}

grafo_cooks <- function(modelo){
  # DIAGNOSTICOS
  diag_modelo <- ls.diag(modelo)
  #AGREGAR ID A LA BASE
  tabla <- tibble("id" = 1:nrow(model.matrix(modelo)),
                  "y_gorro" = modelo$fitted.values,
                  "e" = modelo$residuals,
                  "r" = diag_modelo$std.res,
                  "t"= diag_modelo$stud.res,
                  "hii" = diag_modelo$hat,
                  "cooks" = diag_modelo$cooks,
                  "dfits" = diag_modelo$dfits)
  n <- nrow(tabla)
  p <- length(coefficients(modelo))
  #DEFINIR UMBRAL
  umbral_cooks <- 4/(n - p - 1)
  
  tabla$out_cooks <- (tabla$cooks > umbral_cooks)
  max_cooks <- sort(tabla$cooks, decreasing  = TRUE)[1:6]
  indices_cooks <- which(tabla$cooks %in% max_cooks)
  puntos_cooks <- tabla[indices_cooks,] %>%
    mutate(tipo = "Distancia de Cooks")
  ggp <- tabla %>%
    ggplot(aes(x=cooks, y = t,color=out_cooks))+
    geom_vline(xintercept=umbral_cooks,lty=2, col="#CB4335")+
    labs(y="Residuos estudentizados",
         x="Distancia de Cooks",
         title = "Gráfico de dispersión",
         subtitle = "cooks v/s t")+
    geom_point(shape=1,aes(color=out_cooks))+
    scale_color_manual(values = c("#58D68D", "#5DADE2", "#58D68D")) +
    theme_classic()+
    theme(plot.title = element_text(hjust = 0.5), 
          plot.subtitle = element_text(face = "bold.italic", hjust = 0.5),
          legend.position = "none") + 
    geom_point(data = puntos_cooks, 
               aes(x = cooks, y = t, fill = tipo, col = tipo), 
               alpha=0.7)+
    geom_text(data = puntos_cooks,
              aes(x = cooks, y = t, label=as.character(id)),
              vjust = -1, parse = TRUE)
  return(ggp)
}

grafo_dfits <- function(modelo){
  # DIAGNOSTICOS
  diag_modelo <- ls.diag(modelo)
  #AGREGAR ID A LA BASE
  tabla <- tibble("id" = 1:nrow(model.matrix(modelo)),
                  "y_gorro" = modelo$fitted.values,
                  "e" = modelo$residuals,
                  "r" = diag_modelo$std.res,
                  "t"= diag_modelo$stud.res,
                  "hii" = diag_modelo$hat,
                  "cooks" = diag_modelo$cooks,
                  "dfits" = diag_modelo$dfits)
  n <- nrow(tabla)
  p <- length(coefficients(modelo))
  #DEFINIR UMBRAL
  umbral_dfits <- 2*sqrt(p/n)
  tabla$out_dfits <- (abs(tabla$dfits) > umbral_dfits)
  max_dfits <- sort(tabla$dfits, decreasing  = TRUE)[1:3]
  min_dfits <- sort(tabla$dfits, decreasing  = FALSE)[1:3]
  indices_dfits <- which(tabla$dfits %in% c(min_dfits, max_dfits))
  puntos_dfits <- tabla[indices_dfits,] %>%
    mutate(tipo = "Dfits")
  ggp <- tabla %>%
    ggplot(aes(x=dfits, y = t,color=out_dfits))+
    geom_vline(xintercept=umbral_dfits,lty=2, col="#CB4335")+
    geom_vline(xintercept=-umbral_dfits,lty=2, col="#CB4335")+
    labs(y="Residuos estudentizados",
         x="Dfits",
         title = "Gráfico de dispersión",
         subtitle = "Dfits v/s t")+
    geom_point(shape=1,aes(color=out_dfits))+
    scale_color_manual(values = c("#58D68D", "#5DADE2", "#58D68D")) +
    theme_classic()+
    theme(plot.title = element_text(hjust = 0.5), 
          plot.subtitle = element_text(face = "bold.italic", hjust = 0.5),
          legend.position = "none") + 
    geom_point(data = puntos_dfits, 
               aes(x = dfits, y = t, fill = tipo, col = tipo), 
               alpha=0.7)+
    geom_text(data = puntos_dfits,
              aes(x = dfits, y = t, label=as.character(id)),
              vjust = -1, parse = TRUE)
  return(ggp)
}
# FUNCION PARA QUITAR PUNTOS INFLUYENTES Y OUTLIERS

base_sin_outliers <- function(modelo){
  diag_modelo <- ls.diag(modelo)
  #AGREGAR ID A LA BASE
  tabla <- tibble("id" = 1:nrow(model.matrix(modelo)),
                  "y_gorro" = modelo$fitted.values,
                  "e" = modelo$residuals,
                  "r" = diag_modelo$std.res,
                  "t"= diag_modelo$stud.res,
                  "hii" = diag_modelo$hat,
                  "cooks" = diag_modelo$cooks,
                  "dfits" = diag_modelo$dfits)
  n <- nrow(tabla)
  p <- length(coefficients(modelo))
  #DEFINIR UMBRAL
  umbral_leverage <- 2*p/n
  umbral_cooks <- 4/(n - p - 1)
  umbral_dfits <- 2*sqrt(p/n)
  umbral_outlier <-qt(0.975,n - p - 1) 
  indices_leverage <- which(tabla$hii > umbral_leverage)
  indices_cooks <- which(tabla$cooks > umbral_cooks)
  indices_dfits <- which(abs(tabla$dfits) > umbral_dfits)
  indices_outlier <- which(abs(tabla$t) > umbral_outlier)
  influyentes <- c(indices_leverage, indices_cooks, indices_dfits) %>%
    unique() %>%
    sort()
  
  indices_outliers <- sort(indices_outlier)
  influyentes_outliers <- influyentes[influyentes %in% indices_outliers]
  
  base <- modelo$model
  base <- base %>% 
    mutate(id = 1:nrow(base)) %>%
    as_tibble()
  base <- base %>% 
    filter(!id %in% influyentes_outliers)
  resultados <- list(base = base,
                     puntos_inf_out = influyentes_outliers)
  return(resultados)
}

# GRAFO PHQ ---------------------------------------------------------------
grafo_phq <- function(base,vars,etiq){
  
  to_long <- function(var){
    var_tidy <- as.name(var)
    base <- select(base, !!var_tidy)
    base <- mutate(base,
                   "valor"=rep(var,nrow(base)))
    colnames(base)<- c("valor","variable")
    base$valor <- factor(base$valor,
                         labels=c('Nunca','Algunos Días','Más de la mitad de los días','Casi todos los días'))
    tabla <-  base%>%   
      group_by(valor)%>%
      summarise(count=n())%>%
      drop_na()%>%
      mutate(prop=count/sum(count),
             perc=as.character(round(prop*100,1)),
             variable=factor(rep(var,nrow(.))))
    return(tabla)
  }
  
  
  nombres.facet<- function(viejos, nuevos){
    var.labs <- nuevos
    names(var.labs) <-viejos
    return(var.labs)
  }
  
  
  base_grafo<- bind_rows(lapply(vars, to_long))
  
  ggp<- ggplot(base_grafo,aes(x=valor,y=prop))+
    geom_col(fill="#9932CC",color="white")+
    geom_text(aes(label=perc,vjust=-.5),color = "black",position=position_dodge())+
    labs(y="Proporción",
         x=element_blank())+
    
    scale_x_discrete(labels=stringr::str_wrap(levels(base_grafo$valor), width =7),na.translate=FALSE)+
    facet_wrap(~variable,
               labeller = labeller(variable=nombres.facet(viejos = vars,
                                                          nuevos = etiq)))+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
    theme_classic()
  return(ggp)
}


# TEST SUPUESTOS ----------------------------------------------------------


test.supuestos <- function(modelo){
  tibble("Test"=c("Shapiro-Wilks","Breusch-Pagan"),
         "Valor_P"=c(shapiro.test(MASS::stdres(modelo))$p.value,as.numeric(lmtest::bptest(modelo)$p.value)))
}


# OBTENER ALPHA PARA TRANSFORMACION DE PREDICTORES ------------------------

get_alpha <- function(modelo,var){
  
  base_call <- modelo$model
  
  v1<- base_call[,var]*log(base_call[,var])
  
  modelo_v1<-lm(as.formula(paste("s11_phq9~",
                                 paste(paste(labels(terms(modelo)),collapse = "+"),
                                       "I(v1)",sep  = "+"))),
                data=base_call)
  
  resumen <- summary(modelo_v1)
  beta1 <- coefficients(modelo_v1)[names(coefficients(modelo_v1)) ==var]
  eta<-coef(modelo_v1)[length(coefficients(modelo_v1))]
  
  alpha<-as.numeric( (eta/beta1) + 1)
  test_t_eta <- resumen$coefficients[length(coefficients(modelo_v1)),4]
  
  tabla <- tibble("Variable"= var,
                  "Alpha"=round(alpha,2), 
                  "Valor_P"=round(test_t_eta,4))  
  return(tabla)
}


# GRAFICO PARA FUNCION DE MEDIAS ------------------------------------------

grafo_fmedia <- function(modelo,var=FALSE,estud=FALSE){
  # DIAGNOSTICOS
  diag_modelo <- ls.diag(modelo)
  
  #AGREGAR ID A LA BASE
  tabla <- tibble("id"=1:nrow(model.matrix(modelo)),
                  "ajustados"=modelo$fitted.values,
                  "res_estud"=diag_modelo$stud.res,
                  "residuos"=residuals(modelo),
                  "predictor"=modelo$model[,var])
  
  # GRAFICO
  if(is.character(var)){
    ggp.base  <- 
      ggplot(tabla,aes(x=predictor,y=residuos))
  } else{
    ggp.base  <- 
      ggplot(tabla,aes(x=ajustados,y=residuos))
  }
  
  ggp <- ggp.base+
    #    labs(y="Residuos",
    #         x=ifelse(is.character(var),
    #                  attr(getElement(elsoc_2016,var),"label"),
    #                  "Valores Ajustados"))+
    geom_point(shape=1)+
    scale_color_manual(values=c("cornflowerblue", "red"))+
    theme_classic()+
    theme(legend.position = "none")
  
  return(ggp)
}

grafo_fmedia_est <- function(modelo,var=FALSE,estud=FALSE){
  # DIAGNOSTICOS
  diag_modelo <- ls.diag(modelo)
  
  #AGREGAR ID A LA BASE
  tabla <- tibble("id"=1:nrow(model.matrix(modelo)),
                  "ajustados"=modelo$fitted.values,
                  "res_estud"=diag_modelo$stud.res,
                  "residuos"=residuals(modelo),
                  "predictor"=modelo$model[,var])
  
  # GRAFICO
  if(is.character(var)){
    ggp.base  <- 
      ggplot(tabla,aes(x=predictor,y=res_estud))
  } else{
    ggp.base  <- 
      ggplot(tabla,aes(x=ajustados,y=res_estud))
  }
  
  ggp <- ggp.base+
    #    labs(y="Residuos",
    #         x=ifelse(is.character(var),
    #                  attr(getElement(elsoc_2016,var),"label"),
    #                  "Valores Ajustados"))+
    geom_point(shape=1)+
    scale_color_manual(values=c("cornflowerblue", "red"))+
    theme_classic()+
    theme(legend.position = "none")
  
  return(ggp)
}