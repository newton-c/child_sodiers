library(Amelia)

set.seed(977634)

dat_total <- ifelse(is.na(dat_total$bdeaths), 0, dat_total$bdeaths)

# dat_total
idvars <- c("country", "hom_rate", "hom_count")
bounds <- matrix(c(7, 0, 100, # duration
			       11, 0, 100000, # hom_count
				   13, 0, 100, # popslums
				   14, 0, 50), # education
			  nrow = 4, ncol = 3, byrow = TRUE) 

a.out <- amelia(as.data.frame(dat_total), m = 5, idvars = idvars, ts = "year", cs = "ccodecow",
	bounds = bounds, empri = .01 * nrow(dat_total))