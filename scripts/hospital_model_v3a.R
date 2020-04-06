	# Covid-19 UK hospital demand model
	# Jon Read jonathan.read@lancs.ac.uk
	# 2020-03-29

	# Changes since last version
	#  - fixed bug which underestimated ICU demand
	#  - now includes death

  # Ben Barr -  31/3/2020
# changes
# - simplified some of the inputs so it would work with other prediction file and other groups of LAs.
#  - changed prop$Proportion.of.hospitalised.cases.needing.critical.care to 2% version

	rm( list=ls() )


	# Inputs
	# Identify LAD group in csv look up
	#file_date = "2020-03-29" # date prediction made on
	#date_start_pred = "2020-02-19" # first date of prediction
	#pred_file_name = paste0("raw_data/pred_",la_group_name,"_",file_date,".csv")
	la_group_name = "Merseyside"
	la_dat = read.csv("raw_data/merseyside.csv")

	# identify file of predicted new infections
	pred_file_name="raw_data/data_extract/summary_ 2020-03-23 .csv"

	# identify severity probabilities
	# Import info on age=-strat. proportions seeking care and hospitalisation
	prop = read.csv("raw_data/NSHE Oxford LSHTM Imperial age breakdown of COVID severity.csv")

	#recalculate
	prop$Proportion.of.hospitalised.cases.needing.critical.care<- prop$proportion.infected.critical.care.v2 /	prop$proportion.infected.hospitalised.v2
	date_start_pred="2020-02-03" # date of first prediction in time series
	date_start_use="2020-05-14" # date of first prediction in time series

	source("scripts/hospital_model_functions.R")

	# High-level variables / functions
		repl_max = 200 # number of realisations to run
		# Intervals
			int_sympt_seek = function(t) {
				# Interval: symptom onset --> ED visit
				# gamma distribution, with shape=2 and rate=0.34
				t + floor(rgamma(length(t),shape=2,rate=0.34))
			}
			int_admit_icu = function(t) {
				# Interval: admission --> ICU
				# Poisson distribution with mean 1 day
				t + floor(rpois(length(t),1))
			}
			int_norm_discharge = function(t) {
				# Interval: admission --> discharge (non_ICU patient)
				# uniform distribution, from 4 to 8 days
				t + floor(runif(length(t),4,8))
			}
			int_icu_discharge = function(t) {
				# Interval: admission --> discharge (ICU patient)
				# uniform distribution, from 10 to 14 days
				t + floor(runif(length(t),10,14))
			}

			# Note, length of stay independent of final outcome (alive/dead)

		# probability of seeking care if infected
			p.infected.seek.care = prop$proportion.infected.seeking.heathcare.v2
		# probability of being admitted if infected
			p.infected.hosp = prop$proportion.infected.hospitalised.v2
		# probability of requiring ICU if admitted
			p.hosp.ICU = prop$Proportion.of.hospitalised.cases.needing.critical.care
		# probability of death if admitted, but not in ICU
			p.death.nonICU = prop$Proportion.of.non.critical.care.cases.dying
		# probability of death if admitted and in ICU
			p.death.ICU = prop$Proportion.of.critical.cases.dying


	# Run hospital admission and bed demand model for each LAD
		loc_demand = list()
		for (k in 1:nrow(la_dat)) {
			print(paste0( k, "/", nrow(la_dat), " -- ", la_dat$name[k]) )

			# Import infection prediction, and convert to nSeek
			# 	where nSeek is a matrix of the number seeking care,
			#	by day by location.

				tmp = import.transm.model.prediction(
					prop,
					file_name=pred_file_name,
					date_start_pred="2020-02-02", # date of first prediction in time series
					date_start_use="2020-03-30", # date of first prediction in time series
					loc=la_dat$code[k]
				)


				nSeek_loc = tmp$nSeek	# Number of individuals seeking care, by day/age
				t_max = tmp$t_max	# number of days predicted
				date_seq = tmp$date_seq	# date sequence for prediction

			# Hospital capacity model
				sim = run.sims( prop, nSeek_loc, date_seq, t_max, repl_max )
				demand = extract_demand( sim, repl_max, date_seq )

			# Summarize demand and save to file
				loc_demand[[k]] = demand
				demand_summary = cbind(
					get.95pc.by.day(demand$ED, "ED"),
					get.95pc.by.day(demand$bed_norm, "bed_norm"),
					get.95pc.by.day(demand$bed_icu, "bed_icu"),
					get.95pc.by.day(demand$discharge_norm, "discharge_norm"),
					get.95pc.by.day(demand$discharge_icu, "discharge_icu"),
					get.95pc.by.day(demand$death_norm, "death_norm"),
					get.95pc.by.day(demand$death_icu, "death_icu")

				)
				output_file_name = paste0(
					"results/",
					"/demand_summary_",
					"_",
					la_dat$code[k],
					".csv"
				)
				write.csv( demand_summary, output_file_name )
		}



		# aggregate to NHS trust footprint

			footprint = loc_demand[[1]]
			for (k in 2:length(loc_demand)) {
				footprint$ED = footprint$ED + loc_demand[[k]]$ED
				footprint$bed_norm = footprint$bed_norm + loc_demand[[k]]$bed_norm
				footprint$bed_icu = footprint$bed_icu + loc_demand[[k]]$bed_icu
				footprint$discharge_norm = footprint$discharge_norm + loc_demand[[k]]$discharge_norm
				footprint$discharge_icu = footprint$discharge_icu + loc_demand[[k]]$discharge_icu
			}
			footprint_summary = cbind(
					get.95pc.by.day(footprint$ED, "ED"),
					get.95pc.by.day(footprint$bed_norm, "bed_norm"),
					get.95pc.by.day(footprint$bed_icu, "bed_icu"),
					get.95pc.by.day(footprint$discharge_norm, "discharge_norm"),
					get.95pc.by.day(footprint$discharge_icu, "discharge_icu")
				)
			output_file_name2 = paste0(
					"results/",
					la_group_name,
					"_demand_summary_",
					"_combined.csv"
				)
			write.csv(footprint_summary, output_file_name2 )


















