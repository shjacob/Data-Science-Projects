library(pracma)

## Input Variables --> Pik , EAD , PVk , ACCSK
## Single Value Parameters -->LTheta , NumberofSimulations , NumObligors , lowerValue , UpperValue
## Multi Value Parameters --> N
## Output --> PDi , ComputedTheta , AcctSkeys 
#Pik - probability of default of exposure 'i' condition on a particular world state - Computed in the credit risk engine - available for the 
#EAD - Exposure of obligors - From the database
#PVk : is the probability density at Vk=(k1,k2)
#ACCSK - Unique Identifier of each of the accounts - from database
#LTheta -Portfolio Loass (L) for a given risk appetite - 
#NumberofSimulations - Number of simulations to be performed 
	
DBG<-"FLSE"
LI<-"FLSE"
if(DBG == "FLSE") {
	#Formulate the function for Portfolio Loss Level as per the formulae and methods 
	#mentioned in the Economic Capital POC Test methodology document (for NAB, Melbourne)			  
	FormulateExpression<-function(Theta){
		 interimMatrix<-matrix(log( 1-Pik+Pik*exp(Theta*EAD)), nrow=NumberofSimulations)
		 interimSum<-apply(interimMatrix, 1, sum)

		V1<-PVk*exp(interimSum) *(Pik * exp (Theta * EAD))/(1 - Pik + Pik * exp (Theta * EAD))

		posVec1<-seq(1,length(PVk), by=NumberofSimulations)
		splitPattern1<-rep(seq_along(posVec1), times=diff(c(posVec1,length(PVk)+1)))
						
		l_V1<-split(V1, splitPattern1)
		l_VS1<-lapply(l_V1, sum)
		VS1<-unlist(l_VS1)
										
		V2<-PVk*exp(interimSum)
		
		posVec2<-seq(1,length(PVk), by=NumberofSimulations)
		splitPattern2<-rep(seq_along(posVec2), times=diff(c(posVec2,length(PVk)+1)))

		l_V2<-split(V2, splitPattern2)
		l_VS2<-lapply(l_V2, sum)
		VS2<-unlist(l_VS2)
						  
		#Multiply VS1 and the EAD value now - for every exposure
		#this yields say VS1EAD
		dfEADACCSK<-data.frame(EAD,ACCSK)
		VS1EAD<-VS1*dfEADACCSK[!duplicated(dfEADACCSK$ACCSK),]$EAD
						
		VS3<-sum(VS1EAD)
		VS4<-VS3/VS2[1]

		LExp <- VS4 - LTheta
		return(LExp)
	}
} else {
	FormulateExpressionDBG<-function(Theta){
		logHandle<-file("JMJ_DBGFile",open="a") # Openning connection to log file
		interimMatrix<-matrix(log( 1-Pik+Pik*exp(Theta*EAD)), nrow=NumberofSimulations)

		interimSum<-apply(interimMatrix, 1, sum)

		V1<-PVk*exp(interimSum) *(Pik * exp (Theta * EAD))/(1 - Pik + Pik * exp (Theta * EAD))
		cat("V1:\n", V1, "\n",file=logHandle)
		cat("############################################################\n",file=logHandle)
		cat("length(PVk): ",file=logHandle)
		cat(length(PVk),file=logHandle)

		posVec1<-seq(1,length(PVk), by=NumberofSimulations)
		splitPattern1<-rep(seq_along(posVec1), times=diff(c(posVec1,length(PVk)+1)))
						
		l_V1<-split(V1, splitPattern1)
		l_VS1<-lapply(l_V1, sum)
		VS1<-unlist(l_VS1)

		##cat("VS1:\n", VS1, "\n",file=logHandle)
		##cat("############################################################\n",file=logHandle)

		cat("\nVS1:", file=logHandle)

		for(j in 1 : 10000) {
		cat("\n",file=logHandle)
		cat(VS1[j],file=logHandle)
		 }
																					
		V2<-PVk*exp(interimSum)

		cat("V2:\n", V2, "\n",file=logHandle)
		cat("############################################################\n",file=logHandle)

		posVec2<-seq(1,length(PVk), by=NumberofSimulations)
		splitPattern2<-rep(seq_along(posVec2), times=diff(c(posVec2,length(PVk)+1)))

		l_V2<-split(V2, splitPattern2)
		l_VS2<-lapply(l_V2, sum)
		VS2<-unlist(l_VS2)

		cat("VS2:\n", VS2, "\n",file=logHandle)
		cat("############################################################\n",file=logHandle)
				  
		#Multiply VS1 and the EAD value now - for every exposure
		#this yields say VS1EAD
		dfEADACCSK<-data.frame(EAD,ACCSK)
		VS1EAD<-VS1*dfEADACCSK[!duplicated(dfEADACCSK$ACCSK),]$EAD
						
		VS3<-sum(VS1EAD)
		VS4<-VS3/VS2[1]

		cat("VS3:\n", VS3, "\n",file=logHandle)
		cat("############################################################\n",file=logHandle)

		cat("VS1EAD:\n", VS1EAD, "\n",file=logHandle)
		cat("############################################################\n",file=logHandle)

		cat("VS2[1]:\n", VS2[1], "\n",file=logHandle)
		cat("############################################################\n",file=logHandle)

		LExp <- VS4 - LTheta
		cat("LExp:\n", LExp, "\n",file=logHandle)
		cat("############################################################\n",file=logHandle)

		close (logHandle)
		return(LExp)
	}
}

if(LI == "FLSE") {
    #Solve the function using brent-dekker
    #loaded the additionally required library (from CRAN), pracma
    #Invoke brent-dekker on the function and the range
    tol<-1.490116e-010
    ComputedTheta<- brent_dekker(FormulateExpression,lowerValue,UpperValue,maxiter=maxiter, tol=tol)
} else {
    #Solve the equation by perform linear interpolation
    solveFunction<-function(Theta1, Theta2) {
    
		Valu1<-FormulateExpression(Theta1)
		Valu2<-FormulateExpression(Theta2)
		
		#Theta - Theta1)/(Theta2-Theta1)<-(LTheta-Valu1)/(Valu2-Valu1)
		Theta<-(((LTheta-Valu1)/(Valu2-Valu1)) * (Theta2 - Theta1)) + Theta1
		return (Theta)
	}
	
    #Solve the equation - perform linear interpolation (solve) iteratively
    #this is not used now as dekker brent method yields accurate or 
    #close to accurate values - function converges to ~0, for most of the cases
    solveFunction<-function(a, b) {
		ter <- 1

		while(iter == maxiter) {
			F1<- FormulateExpression (a)
			F2<- FormulateExpression (b)
			c<-solve(F1, F2, a, b)
			F3<- FormulateExpression (c)
			
			if( F3 == LTheta ) {
				return ( c )
			} else if (c < LTheta) {
				a <- c
			} else {
				b <- c
			}
			iter<-iter+1
		   }

    return( c )
    }

    #Solve the equation by perform linear interpolation once
    #this does not seem to be the right method to use given this complex continuous function
    solveFunctionComb<-function(Theta1, Theta2) {
    Valu1<- FormulateExpression (Theta1)
    Valu2<- FormulateExpression (Theta2)

    Theta<-(((LTheta-Valu1)/(Valu2-Valu1)) * (Theta2 - Theta1)) + Theta1
    return (Theta)
	}

    #ComputedTheta<-solveFunction(lowerValue, UpperValue)
    #solveFunctionComb()
}

#Generate the Probability of Default values given Theta for every exposure
GeneratePDiValues<-function(Theta){
     #one expression for a state say K1
     #hence a total of NumberofSimulations expressions
     interimMatrix<-matrix(log( 1-Pik+Pik*exp(Theta*EAD)), nrow=NumberofSimulations)
     interimSum<-apply(interimMatrix, 1, sum)

     VV1<-PVk*exp(interimSum) *(Pik * exp (Theta * EAD))/(1 - Pik + Pik * exp (Theta * EAD))

     posVec1<-seq(1,length(PVk), by=NumberofSimulations)
     splitPattern1<-rep(seq_along(posVec1), times=diff(c(posVec1,length(PVk)+1)))

     l_VV1 <-split(VV1, splitPattern1)
                  
     l_VVS1 <-lapply(l_VV1, sum)
     VVS1 <-unlist(l_VVS1)

     VV2<-PVk*exp(interimSum)

     posVec2<-seq(1,length(PVk), by=NumberofSimulations)
                   splitPattern2<-rep(seq_along(posVec2), 	times=diff(c(posVec2,length(PVk)+1)))

     l_VV2 <-split(VV2, splitPattern2)
     l_VVS2 <-lapply(l_VV2, sum)
     VVS2 <-unlist(l_VVS2)

     VV3 <-VVS1/VVS2
     return(VV3)
}

#Tag the results with the account identifiers
AcctSkeys<-unique(ACCSK)

PDi<-GeneratePDiValues(ComputedTheta)
