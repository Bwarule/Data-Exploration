## still need convert into function
selectD <- unique(iris$Species)
for(j in 1:length(selectD)){
	selectD_val <- as.character(selectD[j])
	p_value <- cor.test(iris[which(iris$Species %in% selectD_val),c("Sepal.Length")],
	                    iris[which(iris$Species %in% selectD_val),c("Sepal.Width")])$"p.value"
	data_out <- data.frame(Species_type = selectD_val,p_value =p_value )
	if(j==1){
		final_data <- data_out
		}else{
		final_data <- rbind(final_data , data_out)
	}
}
