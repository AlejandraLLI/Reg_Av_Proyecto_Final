# Regresion Avanzada: Examen Final
# Modelo dinámico con intercepto fijo con suavizamiento (lambda=10)


#-Defining data-
data_dinam_std_suav_100 <- list("n"=n,"y"=c(datos$WTI[1:(n-3)],rep(NA,m)), "x"=select(datos, -WTI))

#-Defining inits-
inits<-function(){list(alpha=0,beta=matrix(0,k,n),tau.y=1,yp=rep(1,n))}

#-Selecting parameters to monitor-
parameters<-c("alpha","beta","tau.y","tau.b","yp")

#-Running code in JAGS-
set.seed(semilla)
mod_dinam_std_suav_100.sim<-jags(data_dinam_std_suav_100,inits,parameters,model.file="Modelos/suavizamiento_lambda_100.txt",
                             n.iter=niter,n.chains=nchains,n.burnin=nburning,n.thin=1,
                             progress.bar='none')


#-Monitoring the chains-

#JAGS Output
out_dinam_std_suav_100<-mod_dinam_std_suav_100.sim$BUGSoutput$sims.list

# Simulations
out_dinam_std_suav_100.sum<-mod_dinam_std_suav_100.sim$BUGSoutput$summary

# Modes
modas_dinam_std_suav_100_beta<-unlist(lapply(1:n,function(x){apply(out_dinam_std_suav_100$beta[,,x],2,getmode)}))
prob_dinam_std_suav_100_beta<-unlist(lapply(1:n,function(x){apply(out_dinam_std_suav_100$beta[,,x],2,prob)}))

# Summary

out_dinam_std_suav_100.sum.t_beta<-cbind(out_dinam_std_suav_100.sum[grep("beta",rownames(out_dinam_std_suav_100.sum)),c(1,3,5,7)],modas_dinam_std_suav_100_beta, prob_dinam_std_suav_100_beta)
#out_dinam_std_suav_100.sum.t_beta<-cbind(out_dinam_std_suav_100.sum.t_beta,apply(out_dinam_std_suav_100$beta,2,prob))
out_dinam_std_suav_100.sum.t_beta<-out_dinam_std_suav_100.sum.t_beta[,c(1,3,5,2,4,6)]
colnames(out_dinam_std_suav_100.sum.t_beta)<-c("Media","Mediana","Moda","2.5%","97.5%","Prob.")
rownames(out_dinam_std_suav_100.sum.t_beta)<-paste(rep(c('JPM Dollar Ind.','VIX Ind','Prod. OPEP','Dem. OPEP','T-Bill 10YR','T-Bill 1YR'),n),rep(1:n,each=k),sep=' t=')

#-DIC-
out_dinam_std_suav_100.dic<-mod_dinam_std_suav_100.sim$BUGSoutput$DIC

#-Predictions-
out_dinam_std_suav_100.yp<-out_dinam_std_suav_100.sum[grep("yp",rownames(out_dinam_std_suav_100.sum)),]

# #-alpha-
# out_dinam_std_suav_100.alpha<-out_dinam_std_suav_100.sum[grep("alpha",rownames(out_dinam_std_suav_100.sum)),]

#-Betas-
out_dinam_std_suav_100.beta<-out_dinam_std_suav_100.sum[grep("beta",rownames(out_dinam_std_suav_100.sum)),]

# - Pseudo R2 - 
pseudoR2_dinam_std_suav_100<-pseudoR2_std(out.yp = out_dinam_std_suav_100.yp)
