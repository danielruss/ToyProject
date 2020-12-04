library(tidyverse)

# load the data
data = read_tsv("https://episphere.github.io/logist/test2.txt")

# create a vector of Y values
case.control<- pull(data,"case.control")
# remove it from the tibble...
data$case.control = NULL
model_list = list()



for(SNP in colnames(data)[-1]){
  # Make a list of the data for 1 SNP at a time
  snp<-pull(data,SNP)
  # Run the log reg model for that 1 SNP by status
  model = glm(case.control ~ snp, family = binomial(link = logit))
  model_list[[SNP]] = model
}

#res <- model_list %>% map_df(~c( coef(.x),sqrt(diag(vcov(.x))) ) ) %>% 
#  set_names(c("w0","w1","se0","se1")) %>% 
#  mutate(snpname=names(model_list)) %>% 
#  select(snpname,w0,se0,w1,se1)

makeRow<-function(x){
  val<-as.vector(t(summary(x)$coeff))
  length(val)<-8
  names(val)<-paste0(rep(paste0("w",0:1),each=4),".",c("Est","Std.Error","z.value","p"))
  val
}
res <- model_list %>% map_df(makeRow) %>%
  mutate(snpname=names(model_list))
p.adj<-p.adjust(res$w1.p,method = "fdr")

