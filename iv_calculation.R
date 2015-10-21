library(smbinning)

iv <- function(predit,target){
	data <- data.frame(predit,target);
	data_sort <- data[order(predit),]
	
	ttl_num <- length(target);
	bin <- 10;
	n <- ttl_num%/%bin;
	iv_bin <- rep(0,times=bin);
	good <- rep(0,times=bin);
	bad <- rep(0,times=bin);
	
	 for (i in 1:bin){
	 	 ## calculate PSI for ith bin
		 if(i!=bin){
		 	 good[i] <- sum(data_sort$target[((i-1)*n+1):(n*i)]);
			 bad[i] <- n - good[i]
		 }else{
		 	 good[i] <- sum(data_sort$target[((i-1)*n+1):ttl_num]);
			 bad[i] <- ttl_num - n*(i-1)-good[i]
		 }
	}
	
	good_pct <- good/sum(good)
	bad_pct <- bad/sum(bad)
	
	for (i in 1:bin){
		iv_bin[i] <- (bad_pct[i]-good_pct[i])*log(bad_pct[i]/good_pct[i])
	}
	
	iv=sum(iv_bin)
	return (iv)
}

## Variable type
DataType <- data.frame(VarType = sapply(data,class))
DataType$variable_name <- row.names(DataType) 
row.names(DataType)  <- NULL
DataType$Iv <- NA
DataType <- DataType[which(!(DataType$variable_name %in% c("Cust_ID","Rating"))),]

data$Rating <- as.numeric(data$Rating)
for(i in 1:length(DataType$VarType)){
	variable_name <- as.character(DataType$variable_name[i])
	VarType <- as.character(DataType$VarType[i])
	cat(variable_name, "Iv calculation","\n")
	if(VarType == "factor")
	{
		result = smbinning.factor(df=data,y="Rating",x=variable_name)
		iv_value <- result$iv
		DataType$Iv[i] <- iv_value
	}else{
		iv_value = iv(predit=data[,variable_name],target= data[,"Rating"])
		DataType$Iv[i] <- iv_value
	}
}

## Please select top 10  sort Iv value 
DataType <- DataType[order(-DataType$Iv),]
head(DataType)
