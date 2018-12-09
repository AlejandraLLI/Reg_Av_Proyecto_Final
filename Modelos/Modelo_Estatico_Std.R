# Regresion Avanzada: Examen Final
# Modelo Estático

#-Defining data-
data_estat_std <- list("n"=n,"y"=c(datos$WTI[1:(n-3)],rep(NA,m)), "x"=select(datos, -WTI))

#-Defining inits-
inits<-function(){list(alpha=0,beta=rep(0,k),tau=1,yp=rep(1,n))}

#-Selecting parameters to monitor-
parameters<-c("alpha","beta","tau","yp")


#-Running code in JAGS-
set.seed(semilla)
mod_estat_std.sim<-jags(data_estat_std,inits,parameters,model.file="Modelos/mod_estatico.txt",
                    n.iter=niter,n.chains=nchains,n.burnin=nburning,n.thin=1,
                    progress.bar='none')


#-Monitoring the chains-

#JAGS Output
out_estat_std<-mod_estat_std.sim$BUGSoutput$sims.list

# # Chain for coefficients
# z<-out_estat_std$beta[,1]
# par(mfrow=c(2,2))
# plot(z,type="l")
# plot(cumsum(z)/(1:length(z)),type="l")
# hist(z,freq=FALSE)
# acf(z)

#-Coefficient Summary-

# Simulations
out_estat_std.sum<-mod_estat_std.sim$BUGSoutput$summary

# Modes
modas_estat_std<-apply(cbind(out_estat_std$alpha,out_estat_std$beta),2,getmode)

# Summary
out_estat_std.sum.t<-cbind(rbind(out_estat_std.sum[grep("alpha",rownames(out_estat_std.sum)),c(1,3,5,7)],
                             out_estat_std.sum[grep("beta",rownames(out_estat_std.sum)),c(1,3,5,7)]),
                       modas_estat_std)
out_estat_std.sum.t<-cbind(out_estat_std.sum.t,apply(cbind(out_estat_std$alpha,out_estat_std$beta),2,prob))
out_estat_std.sum.t<-out_estat_std.sum.t[,c(1,3,5,2,4,6)]
colnames(out_estat_std.sum.t)<-c("Media","Mediana","Moda","2.5%","97.5%","Prob.")
rownames(out_estat_std.sum.t)<-c('Intercepto','JPM Dollar Ind.','VIX Ind','Prod. OPEP','Dem. OPEP','T-Bill 10YR','T-Bill 1YR')


#-DIC-
out_estat_std.dic<-mod_estat_std.sim$BUGSoutput$DIC

#-Predictions-
out_estat_std.yp<-out_estat_std.sum[grep("yp",rownames(out_estat_std.sum)),]

# -Pseudo R2-
pseudoR2_estat_std<-pseudoR2_std(out_estat_std.yp)

