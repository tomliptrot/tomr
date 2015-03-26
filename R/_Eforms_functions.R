extractFormData <- function(form_ids, dedupe = TRUE){
	library(reshape2)
	library(RODBC) #loads RODBC package (more on that later)
	library(lubridate)
	library(sqldf)
	information  <- odbcConnect("information") #makes connection
	query1 = "select * from (
		select fs.FieldID
		, fs.FieldName
		, fs.FieldValue
		, fs.FormID
		, l.ChristieNo
		, formDataID
		, datemodified
		, dense_rank() over (partition by formDataID order by datemodified desc) [latestForm]

						from [Core].[XMLDataLog] (nolock) fs
									   inner join [Core].[FormData_Log] (nolock) l on fs.formdatalogid = l.formdatalogid
									   -- JOIN xml edat on edat.EformDataLogID = fs.EformDataLogID 
						 where fs.FormID in (33,35) 
									and  l.ChristieNo not in (
														'200804878'
														,'200704154'
														,'201200888'
														,'200509174'
														,'200611352'
														,'200804513'
														,'200901242'
														,'201204163')
										and l.DeleteFlag =  0
		) query
		where latestForm = 1"
		
	# query1 <- paste("select 
		# fs.*,
		# l.ename,
		# edat.ChristieNo,
		# edat.current_record
	 # from XFD.FullShred fs
		# join xfd.eforms l on fs.eformid = l.eformid
		# JOIN XFD.EFormData edat on edat.EformDataLogID = fs.EformDataLogID 
	 # where fs.eformid in (", paste(form_ids, collapse = ','), ") and edat.current_record = 1 and edat.ChristieNo not in (
			 # '200804878'
			# ,'200704154'
			# ,'201200888'
			# ,'200509174'
			# ,'200611352'
			# ,'200804513'
			# ,'200901242'
			# ,'201204163')
		# and edat.EFormDataDeleteFlag is  NULL", collapse = NULL)

	long_data <- sqlQuery(information,query1, na.strings = c(NA, '', 'NULL')) 
	if(nrow(long_data) == 0) stop('no data available for requested form(s)')
	
	names(long_data) <- CleanNames(long_data)#make the column names lower case

	l <- levels(long_data$label)
	l <- tolower(l)
	l <- gsub(' ', '_',l)
	levels(long_data$label) <- l

	reshaped_data <- dcast(long_data, eformid + christieno  + eformdatalogid + eformdataid  ~ label )
	reshaped_data $date_seen <- as.Date(dmy(reshaped_data $date_seen))
	reshaped_data $christieno_site <- paste(reshaped_data $christieno, 
		reshaped_data $ primary_disease_site, 
		reshaped_data$side,
		sep = ' : ')

	de_duped_data <-reshaped_data[ reshaped_data$date_seen == 
		ave(reshaped_data$date_seen, reshaped_data$christieno_site, FUN = min), ]
	
	# de_duped_data <- ddply(reshaped_data, .(christieno_site), function(.x){
		# .x[which.min(.x$date_seen),]
		# })
	
	query2 <- paste("select
			 mpi.*
			-- ef.[CMScore], 
			--ef.cmstr
		FROM  ClinicalOutcomes.COU.VW_Medway_MPIGrouped mpi
		--left outer join cou.[EFormsData02] ef on ef.[casenote Number] = mpi.[casenote Number]
		where mpi.[casenote Number] IN (
		select distinct
			edat.ChristieNo
		 from XFD.FullShred fs
			join xfd.eforms l on fs.eformid = l.eformid
			JOIN XFD.EFormData edat on edat.EformDataLogID = fs.EformDataLogID 
		 where fs.eformid in (", paste(form_ids, collapse = ','), ") and edat.current_record = 1 and edat.ChristieNo not in (
				 '200804878'
				,'200704154'
				,'201200888'
				,'200509174'
				,'200611352'
				,'200804513'
				,'200901242'
				,'201204163') 
				)
		--and ef.CaseSiteRankB = 1  
			")
	
	mpi_data <- sqlQuery(information,query2)
	names(mpi_data) <- CleanNames(mpi_data)
	
	surv_data <- sqldf("select * from 
		de_duped_data dat 
		left outer join mpi_data mpi on mpi.casenote_number = dat.christieno")
	
	odbcCloseAll()
	surv_data
}

getFormIds <- function(form_name_like = '.'){
	#gets form ids and names from the cou db
	#form_name_like can be a rgeular expression to just return form names that are like it
	library(RODBC) #loads RODBC package (more on that later)
	information  <- odbcConnect("information") 
	
	form_id <- sqlQuery(information,'
	SELECT eformid
		  , ename
	  FROM [ClinicalOutcomes].[xfd].[Eforms]', as.is = TRUE)

	 form_id[grep(form_name_like, form_id$ename, ignore.case = TRUE),]
}

get_comorbidity_score <- function(comorb_xml, ids){
	names(comorb_xml) <- ids
	library(stringr)

	# detect where the overall score ahs already been calculated
	i_overall <- which(str_detect(comorb_xml, 'Overall Comorbidity Score is'))

	overall_score <- as.numeric(str_match(comorb_xml[i_overall], 'Overall Comorbidity Score is ([01234])')[,2])
	#save these
	grades_overall <- data.frame(eformdataid = names(comorb_xml[i_overall]),overall_score)
	if(all(str_detect(comorb_xml, 'Overall Comorbidity Score is'))){
		return(grades_overall)
		}

	#for the remaining comorbidities:

	# Identify the important medical comorbidities and grade severity using the index.
	# Overall Comorbidity Score is defined according to the highest ranked single ailment,
	# except in the case where two or more Grade 2 ailments occur in different organ systems.
	# In this situation, the overall comorbidity score should be designated Grade 3.
	if(length(i_overall) > 0){
		split_comorbs <- str_split(comorb_xml[-i_overall], '\n')
		names(split_comorbs) <- names(comorb_xml[-i_overall])
	}
	else
	{
		split_comorbs <- str_split(comorb_xml, '\n')
		names(split_comorbs) <- names(comorb_xml)
	}

	#identify patinets with no comorbs and save these
	i_none <- which(laply(split_comorbs, function(.x) .x[1] == 'None'))
	
	if (length(i_none) > 0){
		grade_0 <- data.frame(ids = names(split_comorbs)[i_none], grade = 0)
	}
	else grade_0  = NULL

	#extract grade of each individual comorbidity
#.x <- split_comorbs[[1]]
	comorbidity_grades <- ldply(split_comorbs, function(.x){
		grade <- as.numeric(str_extract(.x, '[01234]'))
		#grade <- grade[-length(grade)]
		comorb_name <- str_replace(.x, ' grade [01234]', '')
		#comorb_name <- comorb_name[-length(comorb_name)]
		
		data.frame(comorb_name, grade )
		})

	#get data on comorbidity systems from lookup table	
	information  <- odbcConnect("information") 
	ace_lookup <- sqlQuery(information,'select * from LU.ACE_Table')

	#join to grades
	comorbidity_grades <- merge(x = comorbidity_grades, y = ace_lookup, by.x = 'comorb_name', by.y = 'CMName')

	#for each eform,  
	#comorbidity_grades [comorbidity_grades $.id == 12589,]
	
	grade_calculated <- ddply(comorbidity_grades , .(.id), function(.x){
		#get the max grade per system,
		max_grade_by_system <- ddply(.x, .(CMGpName), summarise,  max(grade))[,2]
		
		#if two or more of these are greater than 1 the overall score is 3
		if(sum(max_grade_by_system > 1) > 1) grade = 3
		#else it is the maximum single comorb
		else grade = max(max_grade_by_system)
		grade
		})
	#combine all grades
	names(grade_calculated ) <- names(grades_overall	) 
	if(!is.null(grade_0))	names(grade_0) <-  names(grades_overall	) 
	ace_grades <- rbind(grades_overall	, grade_calculated, grade_0 )

	ace_out <- merge(x = comorb_xml, y = ace_grades, by.x = 0, by.y = 'eformdataid', all = TRUE)
	names(ace_out) <- c('eformdataid', 'comorbidity_string', 'ace_comorbidity_score')
	ace_out
}

combinedStage <- function(cs, ps, remove_abc = FALSE, mets_indicator = NULL){

	#cs is a character vector containe clinical stage info
	#ps is a character vector containe pathological stage info
	#Function retuenbs a combined version of the two

	library(stringr)

	T <- str_extract(cs,'T[Xx0-4isd][abcdismi]*')
	N <- str_extract(cs,'N[Xx0-4is][abcdismi]*')
	M <- str_extract(cs,'M[01X]')

	pT <- str_extract(ps,'T[Xx0-4isd][abcdismi]*')
	pN <- str_extract(ps,'N[Xx0-4is][abcdismi]*')
	pM <- str_extract(ps,'M[01X]')
	
	# Check for conflicts
	clinical_stages <- cbind(T, N, M)
	path_stages <- cbind( pT, pN, pM)
	path_stages <- gsub('p', '', path_stages)
	
	if(remove_abc){
		path_stages <- gsub('[abcd(mi)]', '', path_stages)
		clinical_stages <- gsub('[abcd(mi)]', '', clinical_stages)
	}
	
	# If there is missing stage data for any version repalce with others
	clinical_stages[is.na(clinical_stages)] <- path_stages[is.na(clinical_stages)]
	path_stages[is.na(path_stages)] <- clinical_stages[is.na(path_stages)]
		
	#if (clinical_stages[3] %in% c('M1', 'M1a','M1b'))  return('Stage IV')
	
	final_stage <- vector('character', nrow(clinical_stages))
	
	#If any TNM is missing flag
	#final_stage[which(aaply(is.na(clinical_stages), 1, any))] <- NA # disables to allow for incomplete stages (is missing ms)
	
	#Replace TX or NX with other source 
	
	clinical_stages[,1][which(clinical_stages[,1] == 'TX')]  <- 
		path_stages[,1][which(clinical_stages[,1] == 'TX')] 
	path_stages[,1][which(path_stages[,1] == 'TX')]  <- 
		clinical_stages[,1][which(path_stages[,1] == 'TX')] 

	clinical_stages[,2][which(clinical_stages[,2] == 'NX')]  <- 
		path_stages[,2][which(clinical_stages[,2] == 'NX')] 
	path_stages[,2][which(path_stages[,2] == 'NX')]  <- 
		clinical_stages[,2][which(path_stages[,2] == 'NX')] 
	
	# if(!is.null(mets_indicator)){
		# clinical_stages[,3][is.na(clinical_stages[,3])] <- ifelse(mets_indicator[is.na(clinical_stages[,3])], 
		# 'M1', 'M0')
	# }
	
	# #replace missing with NX or TX
	# clinical_stages[,1][is.na(clinical_stages[,1])] <- 'TX'
	# clinical_stages[,2][is.na(clinical_stages[,2])] <- 'NX'

	final_stage <- aaply(clinical_stages, 1, paste, collapse = ' ')
	
	# If there are dissagreements flag these up
	final_stage[which(aaply(clinical_stages != path_stages, 1, any))] <- 'pathology clinical staging conflict'
	
	#final_stage[which(aaply(is.na(clinical_stages), 1, any))] <- NA
	final_stage
}
