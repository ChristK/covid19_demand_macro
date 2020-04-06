	
# ------------ General functions --------------------------
	
	invlogit = function(x) {
		expx = exp(x)
		expx /(1+expx)
	}
	

	tcol = function(colour,s=0.1) {
		# coverts colour (character) to transparent rgb hex code.
		f = as.numeric(col2rgb(colour))/255
		g = rgb( f[1], f[2], f[3], s )
		return( g )
	}

	add.polygon = function( x, y1, y2, color, s=0.1 ) {
		polygon(
			c(x,rev(x)),
			c(y1,rev(y2)), 
			border=FALSE,
			col=tcol(color, s)
		)
	}

# ------------ data functions --------------------------

import.transm.model.prediction = function(	prop,
											file_name,
											date_start_pred,
											date_start_use,
											loc               ) {	
	# Import prediction, and trim to specified locations (loc)
		dat = read.csv(file_name)
		dat =dat[as.character(dat$loc) %in% as.character(la_dat$code),]
		i = which( is.element(as.character(dat$loc), as.character(loc)))
			# Note, this is where people live, not which hospital they go to.
			# Hospital catchments to be added in future iteration.
		dat = dat[i,]
		t_max = nrow(dat)/17
		date_seq = as.Date(date_start_pred)+(1:t_max)-1	
	
	# convert number of new infections into matrix, with 17 age classes
		nI = matrix(
			(dat$new.inf.m),
			ncol=17,
			byrow=FALSE
		)
		rownames(nI) = as.character(date_seq)
		colnames(nI) = prop$Age
		
	# trim to go from start_date_use
		i = which(date_seq>=as.Date(date_start_use))
		nI = nI[i,]
		date_seq = date_seq[i]
		t_max = length(date_seq)

	# Number of those that will seek care, by day/age
		nSeek = floor( t( t(nI)*p.infected.seek.care ))
	
	return( list( nI=nI, nSeek=nSeek, t_max=t_max, date_seq=date_seq ) )
}


# ------------ simulation functions --------------------------

run.sims = function( prop, nSeek, date_seq, t_max, repl_max ) {

	sim = list()
	pb = txtProgressBar(min=0, max=repl_max, initial=0, style=3)
	for (repl in 1:repl_max) {
	
		# convert to individual-based model structure
		pop = data.frame(
			age=NULL,
			date_inf=NULL
		)
		for (k in 1:t_max) {
			age = rep(colnames(nSeek),times=as.numeric(nSeek[k,]))
			if (length(age)>0) {
				pop = rbind(
					pop,
					data.frame(
						age = age,
						date_inf = date_seq[k]
					)
				)
			}
		}
		
		# When will they seek care?
			pop$date_seek = int_sympt_seek(pop$date_inf)
		# Will they be admitted?
			p = p.infected.hosp[match(pop$age,prop$Age)]
			pop$admit = rbinom(nrow(pop), 1, prob=p)
		# When will they be admitted? (assume same day as seek care)
			pop$date_admit = as.Date( rep(NA,nrow(pop)) )
			pop$date_admit[pop$admit==1] = pop$date_seek[pop$admit==1]
		# Will they require ICU?
			p = p.hosp.ICU[match(pop$age,prop$Age)]
			pop$icu = rbinom(nrow(pop), 1, prob=p)
		# When will they require ICU?
			pop$date_icu = as.Date( rep(NA,nrow(pop)) )
			pop$date_icu[pop$icu==1] = int_admit_icu(pop$date_admit[pop$icu==1])
		# When will they be discharged?
			pop$date_discharge = as.Date( rep(NA,nrow(pop)) )
			pop$date_discharge[pop$icu==0] = int_norm_discharge(pop$date_admit[pop$icu==0])
			pop$date_discharge[pop$icu==1] = int_icu_discharge(pop$date_admit[pop$icu==1])
		# Will they die?
			pop$death = 0
			i = which(pop$admit==1 & pop$icu==0) # admitted, but not ICU
			p = p.death.nonICU[match(pop$age[i],prop$Age)]
			pop$death[i] = rbinom( length(i), 1, prob=p)
			j = which(pop$admit==1 & pop$icu==1) # admitted, but not ICU
			p = p.death.ICU[match(pop$age[j],prop$Age)]
			pop$death[j] = rbinom( length(j), 1, prob=p)
			
		# count bed demand
			demand = data.frame(
				date=c(date_seq[1]-1,date_seq),
				ED=0,
				bed_norm=0,
				bed_icu=0,
				discharge_norm=0,
				discharge_icu=0,
				death_norm=0,
				death_icu=0
			)
			for (k in 2:(nrow(demand))) {
				# seeking care / ED
					i1 = which(pop$date_seek==demand$date[k])
					demand$ED[k] = length(i1)
				# admitted
					i2 = which(pop$date_admit==demand$date[k])
					i3 = which(pop$date_discharge==demand$date[k] & pop$icu==0)
					demand$bed_norm[k] = demand$bed_norm[k-1] + length(i2) - length(i3)
				# icu
					i4 = which(pop$date_icu==demand$date[k])
					i5 = which(pop$date_discharge==demand$date[k] & pop$icu==1)
					demand$bed_icu[k] = demand$bed_icu[k-1] + length(i4) - length(i5)
				# discharge alive
					i6 = which(pop$date_discharge==demand$date[k] & pop$icu==0 & pop$death==0)
					demand$discharge_norm[k] = length(i6)
					i7 = which(pop$date_discharge==demand$date[k] & pop$icu==1 & pop$death==0)
					demand$discharge_icu[k] = length(i7)
				# discharge dead
					i8 = which(pop$date_discharge==demand$date[k] & pop$icu==0 & pop$death==1)
					demand$death_norm[k] = length(i8)
					i9 = which(pop$date_discharge==demand$date[k] & pop$icu==1 & pop$death==1)
					demand$death_icu[k] = length(i9)
			}
			demand = demand[-1,] # remove first 'added' row of zeros
		# save simultion
		sim[[repl]] = demand
		# update progress bar
		setTxtProgressBar(pb, value=repl)
	}
	close(pb)
	return( sim )
}
	
	
extract_demand = function( sim, repl_max, date_seq ){
	# extract ED, bed_norm, and bed_icu
		extract = function(x,i){ x[,i] }
		
		ED = matrix(
			unlist( lapply( sim, extract, i="ED" ) ),
			ncol=repl_max,
			byrow=FALSE
		)
		rownames(ED) = as.character(date_seq)
		
		bed_norm = matrix(
			unlist( lapply( sim, extract, i="bed_norm" ) ),
			ncol=repl_max,
			byrow=FALSE
		)
		rownames(bed_norm) = as.character(date_seq)
		
		bed_icu = matrix(
			unlist( lapply( sim, extract, i="bed_icu" ) ),
			ncol=repl_max,
			byrow=FALSE
		)
		rownames(bed_icu) = as.character(date_seq)
		
		discharge_norm = matrix(
			unlist( lapply( sim, extract, i="discharge_norm" ) ),
			ncol=repl_max,
			byrow=FALSE
		)
		rownames(discharge_norm) = as.character(date_seq)
		
		discharge_icu = matrix(
			unlist( lapply( sim, extract, i="discharge_icu" ) ),
			ncol=repl_max,
			byrow=FALSE
		)
		rownames(discharge_icu) = as.character(date_seq)
		
		death_norm = matrix(
			unlist( lapply( sim, extract, i="death_norm" ) ),
			ncol=repl_max,
			byrow=FALSE
		)
		rownames(death_norm) = as.character(date_seq)
		
		death_icu = matrix(
			unlist( lapply( sim, extract, i="death_icu" ) ),
			ncol=repl_max,
			byrow=FALSE
		)
		rownames(death_icu) = as.character(date_seq)
		
		
	return( 
		list(
			ED=ED,
			bed_norm=bed_norm,
			bed_icu=bed_icu,
			discharge_norm=discharge_norm,
			discharge_icu=discharge_icu,
			death_norm=death_norm,
			death_icu=death_icu
		)
	)
}




get.95pc.by.day = function(x, txt) {
	y = t( apply( x, MARGIN=1, FUN=quantile, prob=c(0.025,0.5,0.975) ) )
	colnames(y) = paste(txt,c("0.025","0.5","0.975"),sep="_")
	return(y)
}




plot.demand = function( demand, repl_max, date_seq, opac ) {
	# opac: line opacity
	#if (opac<0.01) {opac=0.01}
	
	myplot = function(x, colour="red", txt, lg="") {
		colour2 = tcol(colour,s=opac)
		plot( 
			range(date_seq), c(1,max(x)), type="n", 
			log=lg,
			xlab="date", ylab="daily number"
		); grid()
		for (repl in 1:repl_max) {
			lines( date_seq, x[,repl], col=colour2 )
		}
		mtext(side=3,text=txt,adj=0, cex=0.8)
	}
	
	par( mfrow=c(2,7), las=1, mar=c(6,5,2,1) )
	# linear scale
	myplot(demand$ED,       colour="black", txt="ED visits" )
	myplot(demand$bed_norm, colour="blue", txt="non-ICU bed occupancy" )
	myplot(demand$bed_icu,  colour="red",  txt="ICU bed occupancy" )
	myplot(demand$discharge_norm, colour="darkgreen", txt="non_ICU discharges" )
	myplot(demand$discharge_icu, colour="purple", txt="ICU discharges" )
	myplot(demand$death_norm, colour="grey50", txt="non-ICU deaths" )
	myplot(demand$death_icu, colour="grey20", txt="ICU deaths" )
	# log scale
	myplot(demand$ED,       colour="black", txt="ED visits, log-scale", lg="y" )
	myplot(demand$bed_norm, colour="blue", txt="non-ICU bed occupancy, log-scale", lg="y" )
	myplot(demand$bed_icu,  colour="red",  txt="ICU bed occupancy, log-scale", lg="y" )
	myplot(demand$discharge_norm, colour="darkgreen", txt="non-ICU discharges, log-scale", lg="y" )
	myplot(demand$discharge_icu, colour="purple", txt="ICU discharges, log-scale", lg="y" )
	myplot(demand$death_norm, colour="grey50", txt="non-ICU deaths, log-scale", lg="y" )
	myplot(demand$death_icu, colour="grey20", txt="ICU deaths, log-scale", lg="y" )
	
}
		

