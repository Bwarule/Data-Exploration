missing_imputation <- function(datas,key_col=NULL){
	## key_col : are those column which are the key variable
	## doesn't not participate in the analysis
	ncol_datas <- names(datas)[!(names(datas) %in% key_col)] 
	for(i in 1:length(ncol_datas)){
		variable <- ncol_datas[i]
		class_var <- class(datas[,variable])
		if(class_var == "logical"){
			## variable which is logical and missing 100%;
			## we are not able to do the missing value imputation
			next;
		}
		nrowIt <- which(is.na(datas[,variable]))
		if(class_var == "factor" | class_var == "character"){
			imputation_value <- names(which.max(table(datas[,variable])))
			}else{
			imputation_value <- mean(datas[,variable],na.rm=TRUE )
		}
		datas[nrowIt,variable]	<- imputation_value
	}
	return(datas)
}

## use of the above function
## summary(myData)
## new_data <- missing_imputation(myData,"id")
## summary(new_data)
