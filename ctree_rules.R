ctree_model <- ctree(Species ~ ., data = iris)
list_rules <- partykit:::.list.rules.party(ctree_model)
vars <- unique(iris$Species)
outputdata <- as.data.frame(t(data.frame(vars)))
list_rules

for(j in 1:length(list_rules)){
  text_rule <- unlist(list_rules[j])[1]
  attach(iris)
  as.data.frame(table(iris$Species[which(eval(parse(text=text_rule)))]))detach(iris)
 }
 
 ## Get the rule and actual count by each rule
