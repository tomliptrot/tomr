# extractFormData <- function(form_ids, dedupe = TRUE){
	# library(reshape2)
	# library(RODBC) #loads RODBC package (more on that later)
	# library(lubridate)
	# library(sqldf)
	# information  <- odbcConnect("information") #makes connection
	# query1 = "select * from (
		# select fs.FieldID
		# , fs.FieldName
		# , fs.FieldValue
		# , fs.FormID
		# , l.ChristieNo
		# , formDataID
		# , datemodified
		# , dense_rank() over (partition by formDataID order by datemodified desc) [latestForm]

						# from [Core].[XMLDataLog] (nolock) fs
									   # inner join [Core].[FormData_Log] (nolock) l on fs.formdatalogid = l.formdatalogid
									   # -- JOIN xml edat on edat.EformDataLogID = fs.EformDataLogID 
						 # where fs.FormID in (33,35) 
									# and  l.ChristieNo not in (
														# '200804878'
														# ,'200704154'
														# ,'201200888'
														# ,'200509174'
														# ,'200611352'
														# ,'200804513'
														# ,'200901242'
														# ,'201204163')
										# and l.DeleteFlag =  0
		# ) query
		# where latestForm = 1"
		
	# # query1 <- paste("select 
		# # fs.*,
		# # l.ename,
		# # edat.ChristieNo,
		# # edat.current_record
	 # # from XFD.FullShred fs
		# # join xfd.eforms l on fs.eformid = l.eformid
		# # JOIN XFD.EFormData edat on edat.EformDataLogID = fs.EformDataLogID 
	 # # where fs.eformid in (", paste(form_ids, collapse = ','), ") and edat.current_record = 1 and edat.ChristieNo not in (
			 # # '200804878'
			# # ,'200704154'
			# # ,'201200888'
			# # ,'200509174'
			# # ,'200611352'
			# # ,'200804513'
			# # ,'200901242'
			# # ,'201204163')
		# # and edat.EFormDataDeleteFlag is  NULL", collapse = NULL)

	# long_data <- sqlQuery(information,query1, na.strings = c(NA, '', 'NULL')) 
	# if(nrow(long_data) == 0) stop('no data available for requested form(s)')
	
	# names(long_data) <- CleanNames(long_data)#make the column names lower case

	# l <- levels(long_data$label)
	# l <- tolower(l)
	# l <- gsub(' ', '_',l)
	# levels(long_data$label) <- l

	# reshaped_data <- dcast(long_data, eformid + christieno  + eformdatalogid + eformdataid  ~ label )
	# reshaped_data $date_seen <- as.Date(dmy(reshaped_data $date_seen))
	# reshaped_data $christieno_site <- paste(reshaped_data $christieno, 
		# reshaped_data $ primary_disease_site, 
		# reshaped_data$side,
		# sep = ' : ')

	# de_duped_data <-reshaped_data[ reshaped_data$date_seen == 
		# ave(reshaped_data$date_seen, reshaped_data$christieno_site, FUN = min), ]
	
	# # de_duped_data <- ddply(reshaped_data, .(christieno_site), function(.x){
		# # .x[which.min(.x$date_seen),]
		# # })
	
	# query2 <- paste("select
			 # mpi.*
			# -- ef.[CMScore], 
			# --ef.cmstr
		# FROM  ClinicalOutcomes.COU.VW_Medway_MPIGrouped mpi
		# --left outer join cou.[EFormsData02] ef on ef.[casenote Number] = mpi.[casenote Number]
		# where mpi.[casenote Number] IN (
		# select distinct
			# edat.ChristieNo
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
				# )
		# --and ef.CaseSiteRankB = 1  
			# ")
	
	# mpi_data <- sqlQuery(information,query2)
	# names(mpi_data) <- CleanNames(mpi_data)
	
	# surv_data <- sqldf("select * from 
		# de_duped_data dat 
		# left outer join mpi_data mpi on mpi.casenote_number = dat.christieno")
	
	# odbcCloseAll()
	# surv_data
# }

extract_form_data <- function(form_ids, dedupe = TRUE, shiny = TRUE, get_survival = FALSE){

	if(length(form_ids) == 0) return('No Form id entered')
	library(reshape2)
	library(RODBC) 
	library(lubridate)
	library(sqldf)
	library(dplyr)
	information  <- odbcConnect("information") #makes connection
	dw3 <- odbcConnect("dw3")
	
	#cat(paste('Request:', Sys.time(), paste0(form_ids, collapse = ', '), '\n'), file = 'log.txt', append = TRUE)
	
	#query to select data excluding test patients and delete flagged records
	query1 <- paste("select * from (
		select fs.FieldID
		, fs.FieldName
		, fs.FieldValue as value
		, fs.FormID
		, f.FormName
		, fs.formdatalogid
		, l.ChristieNo
		, l.formDataID
		, l.datemodified
		, dense_rank() over (partition by l.formDataID order by l.datemodified desc) [latestForm]

						from sdl_eforms.[Core].[XMLDataLog] (nolock) fs
									   inner join sdl_eforms.[Core].[FormData_Log] (nolock) l on fs.formdatalogid = l.formdatalogid
									   join  [SDL_EForms].[Core].[Form] f on f.formid = fs.formid
						 where fs.FormID in (", paste(form_ids, collapse = ','), ")
										and l.DeleteFlag =  0
		) query
		where latestForm = 1", collapse = NULL)

	long_data <- sqlQuery(dw3,query1, na.strings = c(NA, '', 'NULL')) 
	#str(long_data)
	if(nrow(long_data) == 0) stop('no data available for requested form(s)')

	long_data = long_data[!duplicated(long_data),]
	if(shiny) incProgress(amount = 0.1, message = 'Transposing Data')
	names(long_data) <- clean_names(long_data)#make the column names lower case

	l <- levels(long_data$fieldname)
	l <- tolower(l)
	l <- gsub("[[:punct:]]", '',l)
	l <- gsub(' ', '_',l)
	levels(long_data$fieldname) <- l
	
	
	reshaped_data <- dcast(long_data, formid + christieno  +  formdataid + formdatalogid + formname ~ fieldname )
	if(shiny) cat(paste('Returned Data:', Sys.time(), paste0(unique(reshaped_data$formname), collapse = ', '), '\n'), file = 'log.txt', append = TRUE)
	if(dedupe){
		#removes duplicate forms (ie same patient same site)
		reshaped_data $date_seen <- as.Date(dmy(reshaped_data $date_seen))
		reshaped_data $christieno_site <- paste(reshaped_data $christieno, 
			reshaped_data $ primary_disease_site, 
			reshaped_data$side,
			sep = ' : ')
			
		#selects earliest form where duplicates exist
		de_duped_data <- reshaped_data[ reshaped_data$date_seen == 
			ave(reshaped_data$date_seen, reshaped_data$christieno_site, FUN = min), ]
		}
	else de_duped_data <- reshaped_data
	
	# de_duped_data <- ddply(reshaped_data, .(christieno_site), function(.x){
		# .x[which.min(.x$date_seen),]
		# })

	if(get_survival){
		surv_data <- left_join(de_duped_data, mpi_data, by = 'christieno')
		
		if(all(c('path_stage', 'clinical_stage') %in% names(surv_data))){
			if(shiny) incProgress(amount = 0.1, message = 'Combining stage')
			surv_data$combined_stage = combinedStage(ps = surv_data$path_stage, cs = surv_data$clinical_stage)
		}
		if(shiny) cat(paste('Returned MPI Data:', Sys.time(), paste0(unique(reshaped_data$formname), collapse = ', '), '\n'), file = 'log.txt', append = TRUE)	
		return(surv_data)
	}
	
	else return(de_duped_data)
}

getFormIds <- function(form_name_like = '.'){
	#gets form ids and names from the cou db
	#form_name_like can be a rgeular expression to just return form names that are like it
	library(RODBC) #loads RODBC package (more on that later)
	dw3  <- odbcConnect("dw3") 
	
	form_id <- sqlQuery(dw3,'SELECT FormID
	,formname
	,formtypeid
	,header
	,comments
	  FROM [SDL_EForms].[Core].[Form]', as.is = TRUE)

	 form_id[grep(form_name_like, form_id$formname, ignore.case = TRUE),]
}

get_comorbidity_score <- function(comorb_xml, ids){
	names(comorb_xml) <- ids
	library(stringr)

	# detect where the overall score ahs already been calculated
	i_overall <- which(str_detect(comorb_xml, 'Overall Comorbidity Score is'))
    if(length(i_overall) > 0){
        overall_score <- as.numeric(str_match(comorb_xml[i_overall], 'Overall Comorbidity Score is ([01234])')[,2])
        #save these
        grades_overall <- data.frame(eformdataid = names(comorb_xml[i_overall]),overall_score)
        }
        
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

get_general_comorbidity_matrix = function(comorbidity_string){

    cm_string <- gsub('grade [0-5]', '', comorbidity_string )
    cm_string <- gsub(' , ', ',', cm_string)
    cm_string <- gsub(' $', ',', cm_string)

    comorb_types <- table(do.call('c', strsplit(cm_string, ',')))

    comorbs <- t(aaply(names(comorb_types),1,  function(.x){
        grepl(.x, comorbidity_string)
        }))
    colnames(comorbs)	<- paste( tolower(names(comorb_types)),'_comorbidty', sep = '')
    comorbs
    }
    
   
get_specific_comorbidity_matrix = function(comorbidity_string){  
  cm_string = gsub('Overall Comorbidity Score is [01234] = (Mild|None|Severe|Moderate)(\\n)?', '', comorbidity_string)
  cm_string = gsub(' grade [01234]', '', cm_string)
  
  comorb_specific_types = unique(do.call('c',  strsplit(cm_string, '\\n')))
  comorb_specific_types = comorb_specific_types[!is.na(comorb_specific_types)]
  
  comorbs <- t(aaply(comorb_specific_types, 1,  function(.x){
        grepl(.x, comorbidity_string)
        }))
    
  comorb_names =   paste( tolower(comorb_specific_types),'_comorbidty', sep = '')
  comorb_names = clean_names2( comorb_names )
  colnames(comorbs) = comorb_names 
  comorbs
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
	
clean_disease_site <- function(site_string){

	site_string <- gsub('MM, ', '', site_string)

	#removes text after commas in site variable (ie subsite)
	site_string <- gsub('(.*?),.*', '\\1', site_string)
	#removes text inside brackets in site variable (ie subsite)
	site_string <- gsub('\\((.*?)\\)', '', site_string)
	
	site_string[site_string == 'G-O junction'] <- 'oesophagus'
	site_string[site_string == 'G-O junction (GOJ)'] <- 'oesophagus'
	site_string[site_string == 'g-o junction'] <- 'oesophagus'
	site_string[grep('g-o junction' , tolower(site_string ))] <- 'oesophagus'
	
	#check this
	site_string[site_string ==  "lymph nodes only"] <- ''
	
	
	site_string[site_string == 'small cell carcinoma'] <- 'lung'


	site_string[site_string == 'BCC NOS'] <- 'Skin'
	site_string[site_string == 'Melanoma'] <- 'Skin'
	site_string[site_string == 'NHL'] <- 'Non-Hodgkin lymphoma'
	site_string[grep('lymphoma' , site_string )] <- 'lymphoma' 

	site_string[grep('[Ll]eukaemia' ,site_string )] <- 'Leukaemia' # not sure about this either...
	site_string[grep('soft tissue' ,site_string )] <- 'soft tissue'

	site_string[grep('thyroid' ,site_string )] <- 'thyroid'
	
	#check this
	#site_string[site_string == ''] <- 'NA'

	site_string <- tolower(site_string )
	site_string[grep('thyroid' ,site_string )] <- 'thyroid'
	site_string
	}
	

