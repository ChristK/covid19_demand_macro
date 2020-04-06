
	rm( list=ls() )
	
	# install the Bioconductor package manager; uncomment to re-run
	# install.packages("BiocManager")  
	# BiocManager::install("rhdf5")
	library(rhdf5)
	
	
	setwd("D:/OneDrive - Lancaster University/work/covid19/hospital model/")
	data_path = "../results/2020-03-29/" # Jon
	
	# import local authority data, and age labels
	load(file="data/LA.Rdata") # LA, data.frame
	load(file="data/age_labels_5yr.Rdata") # age.labels.5yr, chr vector
	n_age = length(age.labels.5yr)
	
	# Retrieve the posterior data brick
	file_date = "2020-03-29"
	file_name = paste0(data_path,"pred_",file_date,".h5")
	pred_la_names = h5read(file_name, "la_names")
	pred_age_names = h5read(file_name, "age_names")
	
	pred = h5read(file_name, "prediction")
	# The prediction data brick has dimensions [S, N, T, M] 
	#	where N is the number of la/age combinations, 
	#	S is the number of states (SEIR), 
	#	T is the number of days since 2020-02-02, and 
	#	M is the number of posterior simulations.
	dim( pred )
	
	# [1] 2584    4  215  150
	# la-age	SEIR	time	repl
	
	#   group       name       otype dclass                  dim
	#0     /  age_names H5I_DATASET STRING                 2584
	#1     /   dimnames H5I_DATASET STRING                    4
	#2     /   la_names H5I_DATASET STRING                 2584
	#3     / prediction H5I_DATASET  FLOAT 2584 x 4 x 215 x 505
	# note there are 152 LADs and 17 age groups, so 152*17=2584

	date_start_pred = "2020-02-19"
	la_name_seq = c(
		"Cheshire East",
		"Cheshire West and Chester",
		"Halton",
		"Knowsley",
		"Liverpool",
		"St. Helens",
		"Sefton",
		"Warrington",
		"Wirral"
	)
	la_group_name = "cheshire_merseyside"
	la_code_seq = LA$code[match(la_name_seq,LA$name)]
	j = numeric() # maps the selected codes to entries in pred
	for (k in 1:length(la_code_seq)) {
		j = c(j, which(pred_la_names==la_code_seq[k]) )
	}
	
	
	t_max = dim(pred)[3]
	pred_la = rep(LA$code,each=n_age)
	t_seq = as.Date(date_start_pred)+(1:t_max)-1
	la_seq = LA$code[match(la_name_seq,LA$name)]
	la_label = LA$name[match(la_seq,LA$code)]
	n_loc = length(la_seq)
	
	cases = pred[4,j,,]	# R numbers by location by realisation
	repl_max = dim(pred)[4]
	
	# find differences in time series to get the number of new cases by day
		dcases = array(NA, c(t_max-1,n_loc*n_age,repl_max))
		for (k in 1:(n_loc*n_age)) {
			x = cases[k,,] #matrix row=time, col=repl
			y = apply( x, MARGIN=2,diff )
			dcases[,k,] = y
		}
	
	# average across all realisations
		mdcases = apply( dcases, MARGIN=c(1,2), FUN=mean )
		mdcases.lo = apply( dcases, MARGIN=c(1,2), FUN=quantile, prob=0.025 )
		mdcases.hi = apply( dcases, MARGIN=c(1,2), FUN=quantile, prob=0.975 )
		res = data.frame(
			k=rep(1:(17*n_loc),each=(t_max-1)),
			loc=rep(la_seq, each=(t_max-1)*17),
			age=rep( rep(age.labels.5yr, each=(t_max-1)), n_loc),
			date=rep(t_seq[2:t_max],17*n_loc),
			new.inf.m = as.numeric(mdcases),
			new.inf.lo = as.numeric(mdcases.lo),
			new.inf.hi = as.numeric(mdcases.hi)
		)	


	# make LA data
		la_dat = data.frame(
			name=la_name_seq,
			code=la_seq
		)
		
	# quick plot
		i = which(res$loc==res$loc[1] & res$age=="80+")
		plot( res$date[i], res$new.inf.m[i], type="l", log="" )
		lines( res$date[i], res$new.inf.lo[i], lty=2 )
		lines( res$date[i], res$new.inf.hi[i], lty=2 )

	# outfile data files
		write.csv(res,paste0("data/pred_",la_group_name,"_",file_date,".csv"))
		write.csv(la_dat,paste0("data/la_data_",la_group_name,"_",file_date,".csv"))
	

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	