# Regresion Avanzada: Proyecto Final
# Funciones Auxiliares

# Gráfica de series de tiempo
plot_ts<-function(datos,Fechas){
  
  output<-function(var,color,var.name,unidades){
    
    par(mar=c(5.1,2.1,3.1,2.1))
    nobs<-nrow(datos)
    
    col<-match(var,colnames(datos))
    y<-unlist(datos[,col])
    
    xmin<-1
    xmax<-nobs
    ymin<-min(y,na.rm=TRUE)
    ymax<-max(y,na.rm=TRUE)
    
    plot(1:nobs,y,type="l",lwd=2,lty=1,col=color,
         xaxt='n',ylim=c(ymin,ymax),xlim=c(xmin,xmax),
         xlab='',ylab=var.name,
         main=var.name)

    
    rect(xleft=match("ene-08",Fecha),ybottom=par("usr")[3],xright=match("jun-09",Fecha),ytop=par("usr")[4],col='#3333334D',border=NA)
    rect(xleft=match("abr-14",Fecha),ybottom=par("usr")[3],xright=match("ene-16",Fecha),ytop=par("usr")[4],col='#3333334D',border=NA)
    
    mtext(paste("(",unidades,")",sep=''), side=3,line=0,cex=0.8)
    
    axis(1,at=seq(1,nobs,6),labels=Fecha[seq(1,nobs,6)],las=2)
    # axis(1,at=c(seq(1,nobs,6),(n-m):n),labels=Fechas[c(seq(1,nobs,6),(n-m):n)],las=2)
    
    abline(h=seq(ymin,ymax,ceiling((ymax-ymin)/10)),lty=2,lwd=1,col='gray50')
    abline(v=match("jul-18",Fecha),lty=2,lwd=1,col='gray50')
  }
  
  output
}

# Probabilidad para tablas de coeficientes
prob<-function(x){
  out<-min(length(x[x>0])/length(x),length(x[x<0])/length(x))
  out
}

# Calcular la moda de un vector
getmode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

# Calculo de la pseud-R2
pseudoR2<-function(out.yp){
  (cor(datos_1$WTI[1:(n-m)],out.yp[1:(n-m),1]))^2 
}

pseudoR2_std<-function(out.yp){
  aux<-out.yp[1:(n-m),1]*sd(datos_1$WTI[1:(n-m)])+mean(datos_1$WTI[1:(n-m)])
  (cor(datos_1$WTI[1:(n-m)],aux))^2 
}

pseudoR2_transf<-function(out.yp){
  (cor(datos_1$WTI[1:(n-m)],exp(out.yp[1:(n-m),1])))^2 
}

# Grafica de regresores vs WTI
plot_RegvsWTI<-function(x.name,out.yp,pos_leg){
  
  col_reg<-match(x.name,colnames(datos))
  x<-unlist(datos[,col_reg])
  y<-datos$WTI
  x.name<-case_when(x.name=='JPM_Dollar_Index'~'JPM Dollar Index',
                    x.name=='VIX_Index'~'VIX Index',
                    x.name=='OPEP_TOTPROD'~'Prod. OPEP',
                    x.name=='OPEP_TOTDEM'~'Dem. Petroleo',
                    x.name=='TBILL_10YR'~'TBILL-10YR',
                    x.name=='TBILL_1YR'~'TBILL-1YR')
  
  ymin<-min(y,out.yp[,c(1,3,7)])
  ymax<-max(y,out.yp[,c(1,3,7)])
  xmin<-min(x)
  xmax<-max(x)
  
  plot(x,y,type="p",pch=16,col="grey50",ylim=c(ymin,ymax),
       main=paste(x.name," vs WTI"),xlab=x.name,ylab='WTI')
  
  points(x,out.yp[,1],col='firebrick1',pch=16,cex=0.8)
  
  segments(x,out.yp[,3],x,out.yp[,7],col=2)
  legend(pos_leg,legend=c('Observado','Ajustado'),pch=16,col=c('grey50','firebrick1'))
}

# Gráfica de ajuste y pronóstico WTI
plot_tsWTI<-function(out.yp,pos_leg){
  
  xmin<-1
  xmax<-n
  ymin<-min(c(datos$WTI,out.yp[,c(1,3,7)]))
  ymax<-max(c(datos$WTI,out.yp[,c(1,3,7)]))
  
  plot(1:n,datos$WTI,type="l",lwd=3,lty=1,col="grey50",xaxt='n',ylim=c(ymin,ymax),xlim=c(xmin,xmax),xlab='',ylab='WTI') # Observado
  
  rect(xleft=match("ene-08",Fecha),ybottom=par("usr")[3],xright=match("jun-09",Fecha),ytop=par("usr")[4],col='#3333334D',border=NA)
  rect(xleft=match("abr-14",Fecha),ybottom=par("usr")[3],xright=match("ene-16",Fecha),ytop=par("usr")[4],col='#3333334D',border=NA)
  
  lines(1:n,c(out.yp[1:(n-m+1),1],rep(NA,m-1)),lty=1,lwd=2,col='firebrick1') # Ajustado
  lines(1:n,c(out.yp[1:(n-m+1),3],rep(NA,m-1)),lty=2,lwd=1,col='firebrick1') # Ajuste Lower
  lines(1:n,c(out.yp[1:(n-m+1),7],rep(NA,m-1)),lty=2,lwd=1,col='firebrick1') # Ajuste Upper
  
  lines(1:n,c(rep(NA,n-m),out.yp[(n-m+1):n,1]),lty=1,lwd=2,col='royalblue1') # Pronosticado
  lines(1:n,c(rep(NA,n-m),out.yp[(n-m+1):n,3]),lty=1,lwd=2,col='royalblue1') # IP Lower
  lines(1:n,c(rep(NA,n-m),out.yp[(n-m+1):n,7]),lty=1,lwd=2,col='royalblue1') # IP Upper
  
  axis(1,at=seq(1,n,6),labels=Fecha[seq(1,n,6)],las=2)
  # axis(1,at=c(seq(1,nobs,6),(n-m):n),labels=Fechas[c(seq(1,nobs,6),(n-m):n)],las=2)
  
  abline(v=match("jul-18",Fecha),lty=2,lwd=1,col='gray50')

  legend(pos_leg,legend=c('Observado','Ajustado','Pronosticado'),lty=c(1,2,1),lwd=c(2,1,2),col=c('grey50','firebrick1','royalblue1'))
}

# Gráfica de serire de tiempo de coeficients
plot_beta<-function(out.beta,large.main=TRUE){
  
  plotfun<-function (x){
    
    # Etiquetas
    name.y<-case_when(x==1~'JPM Dollar Index',
                      x==2~'VIX Index',
                      x==3~'Prod. OPEP',
                      x==4~'Dem. Petróleo',
                      x==5~'TBILL-10YR',
                      x==6~'TBILL-1YR')
    
    
    ymin<-min(out.beta[seq(x,nrow(out.beta),k),c(1,3,7)])
    ymax<-max(out.beta[seq(x,nrow(out.beta),k),c(1,3,7)])
    xmin<-1
    xmax<-n
    
    plot(out.beta[seq(x,nrow(out.beta),k),1],
         type="l",lty=1,lwd=2,col='darkgoldenrod1',
         ylim=c(ymin,ymax),xlim=c(xmin,xmax),xaxt='n',
         xlab='',ylab=name.y,
         main=paste(ifelse(large.main,'Serie de tiempo del coeficiente estimado para ',''), name.y,sep="")) # Estimado (Media)
    
    rect(xleft=match("ene-08",Fecha),ybottom=par("usr")[3],xright=match("jun-09",Fecha),ytop=par("usr")[4],col='#3333334D',border=NA)
    rect(xleft=match("abr-14",Fecha),ybottom=par("usr")[3],xright=match("ene-16",Fecha),ytop=par("usr")[4],col='#3333334D',border=NA)
    
    lines(out.beta[seq(x,nrow(out.beta),k),3],lty=2,lwd=2,col='olivedrab') # IP inf
    
    lines(out.beta[seq(x,nrow(out.beta),k),7],lty=2,lwd=2,col='olivedrab') # IP sup
    
    abline(h=0,lty=3,lwd=2,col='gray30')
    
    axis(1,at=seq(1,n,6),labels=Fecha[seq(1,n,6)],las=2)
    # axis(1,at=c(seq(1,nobs,6),(n-m):n),labels=Fechas[c(seq(1,nobs,6),(n-m):n)],las=2)
    
    legend('topright',legend=c('Media','Banda Conf. 95%'),lty=c(1,2),lwd=2,col=c('darkgoldenrod1','olivedrab'))
    
  }  
  
  plotfun
}

# color para matriz de correlaciones
colorCorrel<-function(x){
  
  ifelse(x>=0.4 & x<1, paste("\\color{red}{",formatC(x,dig=2,format="f"),"}"),
         ifelse(x>-1 & x<=-0.4, paste("\\color{red}{",formatC(x,dig=2,format="f"),"}"),
                paste("\\color{black}{",formatC(x,dig=2,format="f"),"}")))
}