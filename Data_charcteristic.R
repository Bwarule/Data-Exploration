Data_charcteristic <- function(variable=NULL,dataIn,RespvaR=NULL,
	save_option,ploting_path=getwd(),Defult_idvars=NULL){
	
	## variable: It is either variable or variable list by user
	if(is.null(variable)){
	variable <- names(dataIn)}
	
	if(is.null(Defult_idvars)){
	Defult_idvars <- c("id","acct_id","dateval","account_opening_date")}
	
	variable <- variable[!(variable  %in% Defult_idvars)]
	no_plots <- length(variable)
		
	for(i in 1:no_plots){
		## save_option
		if(save_option=='pdf'){
			## save_option TO SAVE THE PLOT IN .png or pdf 
		 	pdf(file.path(ploting_path,
				paste(variable[i],".pdf", sep="" )))	
			}else{
			## png(file.path(ploting_path,
			##	paste(variable[i],".png", sep="" )))
			jpeg(file.path(ploting_path,
				paste(variable[i],".jpg", sep="" )))	
			}
		
		if(lapply(dataIn[variable[i]],class)=="numeric"){
			
			if(!is.null(RespvaR)){
				uniqueRvar <- names(table(dataIn[RespvaR]))
				addparam <- FALSE
				breaks <- 30
				for(j in 1:length(uniqueRvar)){
					dataIn1 <- dataIn[which(dataIn[,RespvaR]==uniqueRvar[j]),]
					aq <- hist(dataIn1[,variable[i]],breaks=breaks,
 						main=paste("histogram for ",variable[i],sep=""),
						xlab=variable[i], col = j +1 , ps=3,add=addparam, border=j)
					addparam <- TRUE
					breaks <- aq$breaks
					}
				
				}else{
				
				hist(dataIn[,variable[i]],main=paste("histogram for ",variable[i],sep=""),
			    xlab=variable[i], col ='blue', ps=3)
		        
				} 		
			
			}else{
			##... Bar plot for the ... Chatagorical variable ## 
			if(!is.null(RespvaR)){
				counts <- table(dataIn[,RespvaR],dataIn[,variable[i]])
				col1 <- 2:5
				}else{
				counts <- table(dataIn[variable[i]])
				col1 <- 1
				}
			
			
			barplot(counts, main=paste("Bar plot for ",variable[i],sep=""),
				   xlab=variable[i], col =col1,width=0.05) 
			
			## for only Response variable ........##
			if(variable[i]==RespvaR){
				result <- table(dataIn[variable[i]])
				test <- c()
				for(jk in 1:length(result)){
					p1 <- 
					table(dataIn[variable[i]])[[jk]]/sum(table(dataIn[variable[i]]))
					p1_test <- paste("Percentage of ",names(result)[jk],
						" is ",round(p1,3)*100,"%",sep="")
					test[jk] <- p1_test
				}
				
				legendText <- test
				legend("topright", inset=.025,cex = 0.85,
 					title="Summary ", c(legendText), 
					text.col='blue')
				
				### p1 <- table(dataIn[variable[i]])[[1]]/sum(table(dataIn[variable[i]]))
				## p2 <- table(dataIn[variable[i]])[[2]]/sum(table(dataIn[variable[i]]))
				## p1_test <-
 				## paste("Percentage of ",names(result)[1]," is ",round(p1,3)*100,"%",sep="")
				## p2_test <- paste("Percentage of ",
				##	       names(result)[2]," is ",round(p2,3)*100,"%",sep="")
				## mtext(paste(p1_test,"&",p2_test), side = 1, col='blue')
				}
			
			}
		
	dev.off() 
	}
	
	
}
