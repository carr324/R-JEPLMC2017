#Model function
easy_clus_model=function(train,test,prob_new,spec){

#Cluster vector starts out empty
clusters=c()

#Randomise order presentation
samp=sample(train,replace=F)

#Loop through training stimuli
for(i in 1:length(train)){
	
	#If a cluster exists
	if(length(clusters)>0){
	
	#Cluster activation determines how active the clusters are; same basic exponential gradient as exemplar model
	clust_act=exp(-spec*(abs(samp[i]-clusters)))
	
	
	#If the max cluster activation is less than a threshold
	if(max(clust_act)<prob_new){
		
		#recruit a new cluster
		clusters=c(clusters,samp[i])
			
	}
	
}else{clusters=c(samp[i])}

}

clusters

familiarity_test=numeric(length(test))

#Loop through test stimuli
for(i in 1:length(test)){
	
	
		
	clust_act=exp(-spec*(abs(test[i]-clusters)))
	
	#Familiarity is just the total activation across clusters (like exemplar model)
	familiarity_test[i]=sum(clust_act)
	
	
}

return(list(fam=familiarity_test,clus=clusters))

}


#Parameter settings
prob_new=0.2
spec=.4

#Simulate across all stimuli (order matters here because of the cluster recruitment, so we don't collapse over symmetry)
train_stim_l1=c(seq(-3,-1,length=10),seq(1,3,length=10))
test_stim_l1=c(-3,-2,-1,0,1,2,3)


train_stim_l2=c(seq(-4,-2,length=10),seq(2,4,length=10))
test_stim_l2=c(-4,-3,-2,0,2,3,4)


train_stim_l3=c(seq(-5,-3,length=10),seq(3,5,length=10))
test_stim_l3=c(-5,-4,-3,0,3,4,5)


train_stim_l4=c(seq(-6,-4,length=10),seq(4,6,length=10))
test_stim_l4=c(-6,-5,-4,0,4,5,6)

l1_c=list(fam=c(0,0,0,0,0,0,0))
l2_c=list(fam=c(0,0,0,0,0,0,0))
l3_c=list(fam=c(0,0,0,0,0,0,0))
l4_c=list(fam=c(0,0,0,0,0,0,0))

#Run model with 1000 different orders for each list type
for(sim in 1:1000){

l1_c$fam=l1_c$fam+easy_clus_model(train_stim_l1,test_stim_l1,prob_new,spec)$fam
l2_c$fam=l2_c$fam+easy_clus_model(train_stim_l2,test_stim_l2,prob_new,spec)$fam
l3_c$fam=l3_c$fam+easy_clus_model(train_stim_l3,test_stim_l3,prob_new,spec)$fam
l4_c$fam=l4_c$fam+easy_clus_model(train_stim_l4,test_stim_l4,prob_new,spec)$fam



}
l1_c$fam=l1_c$fam/1000
l2_c$fam=l2_c$fam/1000
l3_c$fam=l3_c$fam/1000
l4_c$fam=l4_c$fam/1000

quartz()
plot(1:4,c(l1_c$fam[4],l2_c$fam[4],l3_c$fam[4],l4_c$fam[4]),type='l',col='red',ylab='Familiarity',ylim=c(0,2.2))
lines(1:4,c((l1_c$fam[3]+l1_c$fam[5])/2,(l2_c$fam[3]+l2_c$fam[5])/2,(l3_c$fam[3]+l3_c$fam[5])/2,(l4_c$fam[3]+l4_c$fam[5])/2),type='l',col='blue')
lines(1:4,c((l1_c$fam[2]+l1_c$fam[6])/2,(l2_c$fam[2]+l2_c$fam[6])/2,(l3_c$fam[2]+l3_c$fam[6])/2,(l4_c$fam[2]+l4_c$fam[6])/2),type='l',col='green')
lines(1:4,c((l1_c$fam[1]+l1_c$fam[7])/2,(l2_c$fam[1]+l2_c$fam[7])/2,(l3_c$fam[1]+l3_c$fam[7])/2,(l4_c$fam[1]+l4_c$fam[7])/2),type='l',col='black')

legend(2.5,2,c('Global Avg','Anti-ideal','Local Avg','Ideal'),col=c('red','blue','green','black'),lty=c(1,1,1,1))

