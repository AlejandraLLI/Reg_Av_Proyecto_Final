# Regresion Avanzada: Examen Final
# Modelo Estático

#-Defining data-
data_estat_transf <- list("n"=n,"y"=c(datos$WTI[1:(n-3)],rep(NA,m)), "x"=select(datos, -WTI))

#-Defining inits-
inits<-function(){list(beta=rep(0,k),tau=1,yp=rep(1,n))}

#-Selecting parameters to monitor-
parameters<-c("beta","tau","yp")


#-Running code in JAGS-
set.seed(semilla)
mod_estat_transf.sim<-jags(data_estat_transf,inits,parameters,model.file="Modelos/mod_estatico_sin_intercepto.txt",
                    n.iter=niter,n.chains=nchains,n.burnin=nburning,n.thin=1,
                    progress.bar='none')


#-Monitoring the chains-

#JAGS Output
out_estat_transf<-mod_estat_transf.sim$BUGSoutput$sims.list

# # Chain for coefficients
# z<-out_estat_transf$beta[,1]
# par(mfrow=c(2,2))
# plot(z,type="l")
# plot(cumsum(z)/(1:length(z)),type="l")
# hist(z,freq=FALSE)
# acf(z)

#-Coefficient Summary-

# Simulations
out_estat_transf.sum<-mod_estat_transf.sim$BUGSoutput$summary

# Modes
modas_estat_transf<-apply(out_estat_transf$beta,2,getmode)

# Summary
out_estat_transf.sum.t<-cbind(out_estat_transf.sum[grep("beta",rownames(out_estat_transf.sum)),c(1,3,5,7)],
                       modas_estat_transf)
out_estat_transf.sum.t<-cbind(out_estat_transf.sum.t,apply(cbind(out_estat_transf$alpha,out_estat_transf$beta),2,prob))
out_estat_transf.sum.t<-out_estat_transf.sum.t[,c(1,3,5,2,4,6)]
colnames(out_estat_transf.sum.t)<-c("Media","Mediana","Moda","2.5%","97.5%","Prob.")
rownames(out_estat_transf.sum.t)<-c('JPM Dollar Ind.','VIX Ind','Prod. OPEP','Dem. OPEP','T-Bill 10YR','T-Bill 1YR')

#-DIC-
out_estat_transf.dic<-mod_estat_transf.sim$BUGSoutput$DIC

#-Predictions-
out_estat_transf.yp<-out_estat_transf.sum[grep("yp",rownames(out_estat_transf.sum)),]

# -pseudoR2-
pseudoR2_estat_transf<-pseudoR2_transf(out.yp=out_estat_transf.yp)
