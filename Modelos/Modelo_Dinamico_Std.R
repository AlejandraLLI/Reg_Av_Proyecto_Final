# Regresion Avanzada: Examen Final
# Modelo Dinamico


#-Defining data-
data_dinam_std <- list("n"=n,"y"=c(datos$WTI[1:(n-3)],rep(NA,m)), "x"=select(datos, -WTI))

#-Defining inits-
inits<-function(){list(alpha=rep(0,n),beta=matrix(0,k,n),tau.y=1,tau.a=1,tau.b=rep(1,k),yp=rep(1,n))}

#-Selecting parameters to monitor-
parameters<-c("alpha","beta","tau.y","tau.a","tau.b","yp")


#-Running code in JAGS-
set.seed(semilla)
mod_dinam_std.sim<-jags(data_dinam_std,inits,parameters,model.file="Modelos/mod_dinamico1.txt",
                    n.iter=niter,n.chains=nchains,n.burnin=nburning,n.thin=1,
                    progress.bar='none')

#-Monitoring the chains-

#JAGS Output
out_dinam_std<-mod_dinam_std.sim$BUGSoutput$sims.list

# # Chain for coefficients
# z<-out_dinam_std$beta[,1,1] # simulaciones de beta 1 al tiempo 1
# par(mfrow=c(2,2))
# plot(z,type="l")
# plot(cumsum(z)/(1:length(z)),type="l")
# hist(z,freq=FALSE)
# acf(z)

#-Coefficient Summary-

# Simulations
out_dinam_std.sum<-mod_dinam_std.sim$BUGSoutput$summary

# Modes
modas_dinam_std_alpha<-apply(out_dinam_std$alpha,2,getmode)
modas_dinam_std_beta<-unlist(lapply(1:n,function(x){apply(out_dinam_std$beta[,,x],2,getmode)}))
probs_dinam_std_beta<-unlist(lapply(1:n,function(x){apply(out_dinam_std$beta[,,x],2,prob)}))


# Summary
out_dinam_std.sum.t_alpha<-cbind(out_dinam_std.sum[grep("alpha",rownames(out_dinam_std.sum)),c(1,3,5,7)],modas_dinam_std_alpha)
out_dinam_std.sum.t_alpha<-cbind(out_dinam_std.sum.t_alpha,apply(out_dinam_std$alpha,2,prob))
out_dinam_std.sum.t_alpha<-out_dinam_std.sum.t_alpha[,c(1,3,5,2,4,6)]
colnames(out_dinam_std.sum.t_alpha)<-c("Media","Mediana","Moda","2.5%","97.5%","Prob.")
rownames(out_dinam_std.sum.t_alpha)<-paste('Intercepto t=',1:n,sep='_')


out_dinam_std.sum.t_beta<-cbind(out_dinam_std.sum[grep("beta",rownames(out_dinam_std.sum)),c(1,3,5,7)],modas_dinam_std_beta, probs_dinam_std_beta)
# out_dinam_std.sum.t_beta<-cbind(out_dinam_std.sum.t_beta,apply(out_dinam_std$beta,2,prob))
out_dinam_std.sum.t_beta<-out_dinam_std.sum.t_beta[,c(1,3,5,2,4,6)]
colnames(out_dinam_std.sum.t_beta)<-c("Media","Mediana","Moda","2.5%","97.5%","Prob.")
rownames(out_dinam_std.sum.t_beta)<-paste(rep(c('JPM Dollar Ind.','VIX Ind','Prod. OPEP','Dem. OPEP','T-Bill 10YR','T-Bill 1YR'),n),rep(1:n,each=k),sep=' t=')

#-DIC-
out_dinam_std.dic<-mod_dinam_std.sim$BUGSoutput$DIC

#-Predictions-
out_dinam_std.yp<-out_dinam_std.sum[grep("yp",rownames(out_dinam_std.sum)),]

#-alpha-
out_dinam_std.alpha<-out_dinam_std.sum[grep("alpha",rownames(out_dinam_std.sum)),]

#-Betas-
out_dinam_std.beta<-out_dinam_std.sum[grep("beta",rownames(out_dinam_std.sum)),]

#-PseudoR2-
pseudoR2_dinam_std<-pseudoR2_std(out.yp=out_dinam_std.yp)

