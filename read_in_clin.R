
##### This Script will read in clinical data and clean it.
# This is the first step in the pipeline

##########
# initialize folders
# ##########
library(dplyr)
library(gsubfn)
library(gsheet)
library(readr)
library(Hmisc)

if('new_clin.RData' %in% dir('data')){

  load('data/new_clin.RData')
} else {
  ##########
  # read in clinical data, sheets 1 and 2
  ##########
  clin_data <- '~/Documents/LFS/Data/clin_data'
  clin1 <- read.csv(paste0(clin_data, '/clin1.csv'), na.strings=c("","NA"), 
                    stringsAsFactors = FALSE) # read the first sheet
  clin2 <- read.csv(paste0(clin_data, '/clin2.csv'), na.strings=c("","NA"),
                    stringsAsFactors = FALSE) # read the second sheet
  
  
  # combine the two data sets
  clin <- rbind(clin1, clin2)
  
  # remove all columns that are completely NA 
  clin <- clin[, colSums(is.na(clin)) < nrow(clin)]
  
  # remove all rows that are completely NA
  clin <- clin[rowSums(is.na(clin)) < ncol(clin),]
  
  #########
  # clean NAs and N/As
  #########
  clin <- as.data.frame(apply(clin, 2, function(x) gsub('N/A', NA, x)))
  
  # make column names lower case and remove any '.' and replace with '_'
  names(clin) <- tolower(names(clin))
  names(clin) <- gsub("\\.\\.", '', names(clin))
  names(clin) <- gsub(".", '_', names(clin), fixed = T)
  
  
  # remove leading and trailing white spaces to all columns
  clin <- as.data.frame(apply(clin, 2, function(x){
    trimws(x, 'both')
  })
  )
  
  ##########
  # check if properly read in
  ##########
  # are there any age indicators 'y,m,w,d' in blood_dna_malkin_lab
  clin <- clin[!grepl('y|m|w|d', clin$blood_dna_malkin_lab),]
  
  
  # # ## HERE go back and clean age of sample collection manually
  # length(which(grepl('/', clin$blood_dna_malkin_lab)))
  # length(which(grepl('/', clin$age_sample_collection)))
  # # 
  # temp <- clin[grepl('/', clin$blood_dna_malkin_lab),]
  
  # clean unknown's from age_sample collection
  clin$age_sample_collection <- gsub('unknown|NA', NA, clin$age_sample_collection)
  clin$age_diagnosis <- gsub('unknown|NA', NA, clin$age_diagnosis)
  
  # need to add data to throws that are represent multiple cancers, to replace the NAs
  
  for(i in 1:nrow(clin)) {
    sub_dat <- clin[i,]
    if(grepl('^P2', sub_dat$cancer_diagnosis_diagnoses)) {
      #get one row previous index 
      j <- i - 1
      clin_j <- clin[j,]
      sub_dat$family_name <- clin_j$family_name
      sub_dat$relationship <- clin_j$relationship
      sub_dat$tm_donor <- clin_j$tm_donor
      sub_dat$p53_germline <- clin_j$p53_germline
      if(is.na(sub_dat$cancer_diagnosis_diagnoses)) {
        sub_dat$cancer_diagnosis_diagnoses <- clin_j$cancer_diagnosis_diagnoses
      }
      if(is.na(sub_dat$age_diagnosis)) {
        sub_dat$age_diagnosis <- clin_j$age_diagnosis
      }
      
      if(!grepl('/', clin_j$age_sample_collection)) {
        sub_dat$age_sample_collection <- clin_j$age_sample_collection
      }
      
      if(!grepl('/', clin_j$blood_dna_malkin_lab)) {
        sub_dat$blood_dna_malkin_lab <- clin_j$blood_dna_malkin_lab

      }
      
      
      
      clin[i,] <- sub_dat
      
      
    } else if(grepl('^P3', sub_dat$cancer_diagnosis_diagnoses)){
      
      #get one row previous index 
      j <- i - 2
      clin_j <- clin[j,]
      sub_dat$family_name <- clin_j$family_name
      sub_dat$relationship <- clin_j$relationship
      sub_dat$tm_donor <- clin_j$tm_donor
      sub_dat$p53_germline <- clin_j$p53_germline
      if(is.na(sub_dat$cancer_diagnosis_diagnoses)) {
        sub_dat$cancer_diagnosis_diagnoses <- clin_j$cancer_diagnosis_diagnoses
      }
      if(is.na(sub_dat$age_diagnosis)) {
        sub_dat$age_diagnosis <- clin_j$age_diagnosis
      }
      
      
      if(!grepl('/', clin_j$age_sample_collection)) {
        sub_dat$age_sample_collection <- clin_j$age_sample_collection
      }
      
      if(!grepl('/', clin_j$blood_dna_malkin_lab)) {
        sub_dat$blood_dna_malkin_lab <- clin_j$blood_dna_malkin_lab
        
      }
      clin[i,] <- sub_dat
      
    } else if(grepl('^P4', sub_dat$cancer_diagnosis_diagnoses)){
      
      #get one row previous index 
      j <- i - 3
      clin_j <- clin[j,]
      sub_dat$family_name <- clin_j$family_name
      sub_dat$relationship <- clin_j$relationship
      sub_dat$tm_donor <- clin_j$tm_donor
      sub_dat$p53_germline <- clin_j$p53_germline
      if(is.na(sub_dat$cancer_diagnosis_diagnoses)) {
        sub_dat$cancer_diagnosis_diagnoses <- clin_j$cancer_diagnosis_diagnoses
      }
      if(is.na(sub_dat$age_diagnosis)) {
        sub_dat$age_diagnosis <- clin_j$age_diagnosis
      }
      
      
      if(!grepl('/', clin_j$age_sample_collection)) {
        sub_dat$age_sample_collection <- clin_j$age_sample_collection
      }
      
      if(!grepl('/', clin_j$blood_dna_malkin_lab)) {
        sub_dat$blood_dna_malkin_lab <- clin_j$blood_dna_malkin_lab
        
      }
      
      clin[i,] <- sub_dat
      
    } else if(grepl('^P5', sub_dat$cancer_diagnosis_diagnoses)){
      
      #get one row previous index 
      j <- i - 4
      clin_j <- clin[j,]
      sub_dat$family_name <- clin_j$family_name
      sub_dat$relationship <- clin_j$relationship
      sub_dat$tm_donor <- clin_j$tm_donor
      sub_dat$p53_germline <- clin_j$p53_germline
      if(is.na(sub_dat$cancer_diagnosis_diagnoses)) {
        sub_dat$cancer_diagnosis_diagnoses <- clin_j$cancer_diagnosis_diagnoses
      }
      if(is.na(sub_dat$age_diagnosis)) {
        sub_dat$age_diagnosis <- clin_j$age_diagnosis
      }
      
      
      if(!grepl('/', clin_j$age_sample_collection)) {
        sub_dat$age_sample_collection <- clin_j$age_sample_collection
      }
      
      if(!grepl('/', clin_j$blood_dna_malkin_lab)) {
        sub_dat$blood_dna_malkin_lab <- clin_j$blood_dna_malkin_lab
        
      }
      clin[i,] <- sub_dat
      
    } else if(grepl('^P6', sub_dat$cancer_diagnosis_diagnoses)){
      
      #get one row previous index 
      j <- i - 2
      clin_j <- clin[j,]
      sub_dat$family_name <- clin_j$family_name
      sub_dat$relationship <- clin_j$relationship
      sub_dat$tm_donor <- clin_j$tm_donor
      sub_dat$p53_germline <-clin_j$p53_germline
      if(is.na(sub_dat$cancer_diagnosis_diagnoses)) {
        sub_dat$cancer_diagnosis_diagnoses <- clin_j$cancer_diagnosis_diagnoses
      }
      if(is.na(sub_dat$age_diagnosis)) {
        sub_dat$age_diagnosis <- clin_j$age_diagnosis
      }
      
      if(!grepl('/', clin_j$age_sample_collection)) {
        sub_dat$age_sample_collection <- clin_j$age_sample_collection
      }
      
      if(!grepl('/', clin_j$blood_dna_malkin_lab)) {
        sub_dat$blood_dna_malkin_lab <- clin_j$blood_dna_malkin_lab
        
      }
      clin[i,] <- sub_dat
      
    }
  
    
  }
  
  result_list <- list()
  for(i in unique(clin$tm_donor)) {
    
    sub_dat <- clin[clin$tm_donor %in% i,]
    sub_dat <- sub_dat[!is.na(sub_dat$tm_donor),]
    
    if(any(grepl('/', sub_dat$blood_dna_malkin_lab)) |
       any(grepl('/', sub_dat$age_sample_collection))) {
      length_ids <- length(unlist(strsplit(as.character(sub_dat$blood_dna_malkin_lab), '/')[1]))
      
      n_row <- nrow(sub_dat)
      
      if(n_row < length_ids) {
        age_split <- trimws(unlist(strsplit(as.character(sub_dat$age_sample_collection), split = "/")), 'both')
        id_split <- trimws(unlist(strsplit(as.character(sub_dat$blood_dna_malkin_lab), split = "/")), 'both')
        sub_dat <- rbind(sub_dat, sub_dat)
        sub_dat$blood_dna_malkin_lab <- as.character(sub_dat$blood_dna_malkin_lab)
        sub_dat$blood_dna_malkin_lab[1] <- id_split[1]
        sub_dat$blood_dna_malkin_lab[2] <- id_split[2]
        sub_dat$age_sample_collection <- as.character(sub_dat$age_sample_collection)
        sub_dat$age_sample_collection[1] <- age_split[1]
        sub_dat$age_sample_collection[2] <- age_split[2]
        
      } else if(n_row == length_ids) {
        sub_dat$blood_dna_malkin_lab <- as.character(sub_dat$blood_dna_malkin_lab)
        
        sub_dat$age_sample_collection <- as.character(sub_dat$age_sample_collection)
        sub_dat$blood_dna_malkin_lab <- trimws(unlist(strsplit(as.character(sub_dat$blood_dna_malkin_lab), '/')[1]), 'both')
        sub_dat$age_sample_collection <- trimws(unlist(strsplit(as.character(sub_dat$age_sample_collection), '/')[1]), 'both')
        
      } else {
        sub_dat$blood_dna_malkin_lab <- as.character(sub_dat$blood_dna_malkin_lab)
        
        sub_dat$age_sample_collection <- as.character(sub_dat$age_sample_collection)
        
        age_split <- trimws(unlist(strsplit(as.character(sub_dat$age_sample_collection), split = "/")), 'both')
        id_split <- trimws(unlist(strsplit(as.character(sub_dat$blood_dna_malkin_lab), split = "/")), 'both')
        
        age_split <- age_split[!is.na(age_split)]
        id_split <- id_split[!is.na(id_split)]
        
        sub_dat[1:length_ids, 'age_sample_collection'] <- age_split
        sub_dat[1:length_ids, 'blood_dna_malkin_lab'] <- id_split
        
      }
      
    }
    
    result_list[[i]] <- sub_dat
    
    print(i)
  }
  clin <- do.call(rbind, result_list)
  
  
  # Clean age of diagnosis column
  clin$age_diagnosis <- as.character(clin$age_diagnosis)
  clin$age_sample_collection <- as.character(clin$age_sample_collection)
  
  # Clean the age variable so that is expressed in years
  cleanAge <-  function(data, column_name) {
    age_vector <- data[, column_name]
    for (i in 1:length(age_vector)) {
      temp_age <- age_vector[i]
      if(!grepl('1|2|3|4|5|6|7|8|9|0', temp_age)) {
        temp_age <- NA
      } else {
        if (grepl('~', temp_age, fixed = TRUE)) {
          temp_age <- strsplit(temp_age, '~', fixed = TRUE)
          temp.2_age <- unlist(temp_age)
          temp.3_age <- temp.2_age[2]
          temp_age <- temp.3_age
        }
        if (grepl('>', temp_age) || grepl('<', temp_age)) {
          temp_age <- gsub('<', '' , temp_age)
          temp_age <- gsub('>', '', temp_age)
        }
        if (grepl('y', temp_age) && grepl('m', temp_age)) {
          temp_age <- gsubfn('([y,m])', list('y' = '.', 'm' = ''), as.character(temp_age))
          temp.2_age <- strsplit(temp_age, '.', fixed = TRUE)
          temp.3_age <- do.call('rbind', temp.2_age)
          temp.4_age <- gsub('.*\\.', paste0(temp.3_age[1], '.') , as.numeric(temp.3_age[2])/12)
          temp_age <- temp.4_age
        } else if (grepl('y', temp_age) && !grepl('m', temp_age)) {
          temp_age  <- gsubfn('y', '.0', as.character(temp_age))
        } else if (!grepl('y', temp_age) && grepl('m', temp_age) && !grepl('w', temp_age)) {
          temp_age <- gsubfn('m', '', as.character(temp_age))
          temp_age <- as.numeric(temp_age)/12
        } else if (grepl('w', temp_age) && grepl('m', temp_age)) {
          temp_age <- gsubfn('([m,w])', list('m' = '.', 'w' = ''), as.character(temp_age))
          temp_age <- ceiling(as.numeric(temp_age))
        } else if (grepl('w', temp_age) && !grepl('m', temp_age)) {
          temp_age <- gsub('w', '', temp_age)
        } else if (grepl('d', temp_age)) {
          temp_age <- gsub('12d', '0.032', temp_age)
        } 
      }
      age_vector[i] <- temp_age
    }
    data[, column_name] <- age_vector
    data[, column_name] <- as.numeric(as.character(data[, column_name]))
    return(data)
  } 
  
  clin <- cleanAge(clin, column_name = 'age_diagnosis')
  clin<- cleanAge(clin, column_name = 'age_sample_collection')
  
  # Convert age of diagnosis and sample collection to months 
  clin$age_diagnosis <- clin$age_diagnosis*12
  clin$age_sample_collection <- clin$age_sample_collection*12
  
  # clean date column and create variable for patient age 
  clin$dob <- ifelse(grepl('known', as.character(clin$dob)), NA,  as.character(clin$dob))
  
  # extract only the last four characters to get year of birth
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  clin$yob <- as.numeric(substrRight(as.character(clin$dob), 4))
  clin$age <- 2016 - clin$yob
  
  # clean p53
  clin$p53_germline <- as.character(clin$p53_germline)
  clin$p53_germline <- ifelse(is.na(clin$p53_germline), NA,
                              ifelse(grepl('WT', clin$p53_germline), 'WT',
                                     ifelse(grepl('Mut', clin$p53_germline), 'MUT', NA)))
  
  # function to clean cancer_diagnoses
  clin$cancer_diagnosis_diagnoses <- as.character(tolower(clin$cancer_diagnosis_diagnoses))
  clin$cancer_diagnosis_diagnoses <- gsub('?', '', clin$cancer_diagnosis_diagnoses, fixed = T)
  clin$cancer_diagnosis_diagnoses <- gsub(".*-","", clin$cancer_diagnosis_diagnoses)
  clin$cancer_diagnosis_diagnoses <- gsub(" ca","", clin$cancer_diagnosis_diagnoses, fixed = T)
  clin$cancer_diagnosis_diagnoses <- gsub("&",",", clin$cancer_diagnosis_diagnoses, fixed = T)
  clin$cancer_diagnosis_diagnoses <- gsub(";",",", clin$cancer_diagnosis_diagnoses, fixed = T)
  clin$cancer_diagnosis_diagnoses <- gsub("/",",", clin$cancer_diagnosis_diagnoses, fixed = T)
  clin$cancer_diagnosis_diagnoses <- gsub(" , ",",", clin$cancer_diagnosis_diagnoses, fixed = T)
  clin$cancer_diagnosis_diagnoses <- gsub("anaplastic","", clin$cancer_diagnosis_diagnoses, fixed = T)
  clin$cancer_diagnosis_diagnoses <- trimws(clin$cancer_diagnosis_diagnoses, which = 'both')
  clin$cancer_diagnosis_diagnoses <- ifelse(grepl('ffected', clin$cancer_diagnosis_diagnoses),
                                            'Unaffected',
                                            ifelse(grepl('adeno', clin$cancer_diagnosis_diagnoses), 
                                                   'adenocarcinoma', 
                                                   ifelse(grepl('os', clin$cancer_diagnosis_diagnoses), 
                                                          'os', 
                                                          ifelse(grepl('unknown|type not', clin$cancer_diagnosis_diagnoses),
                                                                 NA, 
                                                                 ifelse(grepl('hunting',clin$cancer_diagnosis_diagnoses),
                                                                        'colon',
                                                                        ifelse(grepl('paget', clin$cancer_diagnosis_diagnoses),
                                                                               'dcis;idc', 
                                                                               ifelse(clin$cancer_diagnosis_diagnoses %in% c("", ""),
                                                                                      NA, clin$cancer_diagnosis_diagnoses)))))))
  
  ##########
  # clean p53
  ##########
  clin$gender <- as.character(clin$gender)
  clin$gender <- ifelse(clin$gender == 'M', 'Male', 
                        ifelse(clin$gender == 'F', 'Female', 
                               ifelse(clin$gender == 'unknown', NA, clin$gender)))
  
  # replace LFS with family
  clin$family_name <- gsub('LFS', 'Family', clin$family_name)
  
  
  ##########
  # functon to clean family column 
  ##########
  clin$family_name <- ifelse(!grepl('Family', clin$family_name), 
                             NA, clin$family_name)
  clin$family_name<- unlist(lapply(strsplit(clin$family_name, '-'), function(x) x[1]))
  clin$family_name <- tolower(gsub(" ", "_", clin$family_name))
  
  # lauren's changes
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  
  # clean gdna VARS
  clin[,'gdna.exon.intron'] <- NA
  clin[,'gdna.exon.intron'][is.na(clin$gdna) == FALSE] <- 'no clear intron or exon'
  clin[,'gdna.exon.intron'] <- sub(":.*$", "", clin$gdna )
  clin[,'gdna.exon.intron'][grep(pattern = "Deletion",x = clin[,'gdna.exon.intron'])] <- 'no clear intron or exon'
  clin[,'gdna.exon.intron'][grep(pattern = "insertion",x = clin[,'gdna.exon.intron'])] <- 'no clear intron or exon'
  clin[,'gdna.exon.intron'][grep(pattern = "c.",x = clin[,'gdna.exon.intron'])] <- 'no clear intron or exon'
  clin[,'gdna.exon.intron'] <- trim(clin[,'gdna.exon.intron'])
  
  
  # clean base change
  clin[,'gdna.base.change'] <- NA
  clin[,'gdna.base.change'][is.na(clin$gdna) == FALSE] <- 'no bp change'
  clin[,'gdna.base.change'][grep(pattern = '>',x = clin$gdna)] <- unlist(lapply(X = strsplit(x = as.character(clin$gdna),split = '>'),
                                                                                FUN = function(x){if(length(x) == 2){
                                                                                  str1 <- substr(x[1],nchar(x[1]),nchar(x[1]))
                                                                                  str2 <- substr(x[2],1,1)
                                                                                  out.val <- paste0(str1,'>',str2)
                                                                                  return(out.val)
                                                                                }
                                                                                }))
  clin[,'gdna.base.change'] <- trim(clin[,'gdna.base.change'])
  
  # gdna.codon
  clin[,'gdna.codon'] <- NA
  clin[,'gdna.codon'][is.na(clin$gdna) == FALSE] <- 'no clear location'
  clin[,'gdna.codon'][which(substr(clin$gdna,1,4) == 'Exon')] <- substr(x = sub(">.*","",gsub("^[^.]*.", "", clin$gdna[which(substr(clin$gdna,1,4) == 'Exon')])),start = 1,stop = nchar(sub(">.*","",gsub("^[^.]*.", "", clin$gdna[which(substr(clin$gdna,1,4) == 'Exon')])))-1)
  clin[,'gdna.codon'][which(substr(clin$gdna,1,6) == 'Intron')] <- substr(x = sub(">.*","",gsub("^[^.]*.", "", clin$gdna[which(substr(clin$gdna,1,6) == 'Intron')])),start = 1,stop = nchar(sub(">.*","",gsub("^[^.]*.", "", clin$gdna[which(substr(clin$gdna,1,6) == 'Intron')])))-1)
  clin[,'gdna.codon'][grep(pattern = "d",clin[,'gdna.codon'])] <- 'no clear location'
  clin[,'gdna.codon'][grep(pattern = "_",clin[,'gdna.codon'])] <- 'no clear location'
  clin[,'gdna.codon'][grep(pattern = "-",clin[,'gdna.codon'])] <- 'no clear location'
  clin[,'gdna.codon'][grep(pattern = "/+",clin[,'gdna.codon'])] <- 'no clear location'
  clin[,'gdna.codon'][clin[,'gdna.codon'] == ""] <- 'no clear location'
  clin[,'gdna.codon'] <- trim(clin[,'gdna.codon'])
  
  
  # clean protein vars
  clin[,'protein.codon.change'] <- NA
  clin[,'protein.codon.change'][is.na(clin$protein) == FALSE] <- trim(unlist(lapply(strsplit(x = sub(pattern = " / splice$",replacement = "",
                                                                                                     x = sub(pattern = 'p.',replacement = '',
                                                                                                             x = clin$protein[is.na(clin$protein) == FALSE])),split = '[0-9]+'),
                                                                                    function(x){if(length(x) == 1){
                                                                                      return('no_codon_change')
                                                                                    }
                                                                                      if(length(x) == 2){
                                                                                        return(paste0(x[1],'>',x[2]))
                                                                                      }                     
                                                                                      else{
                                                                                        return(NA)
                                                                                      }
                                                                                      
                                                                                    })))
  
  
  clin$protein.codon.change <- unlist(lapply(strsplit(clin$protein.codon.change,split = " "),FUN = function(x){x[1]}))
  
  clin[,'protein.codon.num'] <- NA
  clin[,'protein.codon.num'][is.na(clin$protein) == FALSE] <- as.numeric(gsub("\\D", "", clin$protein[is.na(clin$protein) == FALSE]))
  unique(clin$protein.codon.num)
  
  clin[,'splice.delins.snv'] <- NA
  clin[,'splice.delins.snv'][grep(pattern = '>',clin$gdna.base.change)] <- 'SNV'
  clin[,'splice.delins.snv'][grep(pattern = 'deletion',clin$protein,ignore.case = T)] <- 'Deletion'
  clin[,'splice.delins.snv'][grep(pattern = 'splice',clin$protein,ignore.case = T)] <- 'Splice'
  clin[,'splice.delins.snv'][grep(pattern = 'dup',clin$gdna,ignore.case = T)] <- 'Duplication'
  clin$codon72.npro <- NA
  clin$codon72.npro[clin$codon72 == 'arg/arg'] <- 0
  clin$codon72.npro[clin$codon72 == 'arg/pro'] <- 1
  clin$codon72.npro[clin$codon72 == 'pro/pro'] <- 2
  
  # mdm2
  clin$mdm2.nG <- NA
  clin$mdm2.nG[clin$mdm2 == 'T/T'] <- 0
  clin$mdm2.nG[clin$mdm2 == 'T/G'] <- 1
  clin$mdm2.nG[clin$mdm2 == 'G/G'] <- 2
  
  rm(clin1, clin2, empty_table)
  
  clin$cancer_diagnosis_diagnoses <-  Hmisc::capitalize(clin$cancer_diagnosis_diagnoses)
  
  clin$age_diagnosis <- round(clin$age_diagnosis/12, 2)
  clin$age_sample_collection <- round(clin$age_sample_collection/12, 2)
  
  unique(clin$cancer_diagnosis_diagnoses)
  clin$cancer_diagnosis_diagnoses[grepl('All', clin$cancer_diagnosis_diagnoses)]
  clin$cancer_diagnosis_diagnoses <- gsub(',', ' & ', clin$cancer_diagnosis_diagnoses, fixed = TRUE)
  clin$cancer_diagnosis_diagnoses <- gsub(', ', ' & ', clin$cancer_diagnosis_diagnoses, fixed = TRUE)
  
  # recode cancer 
  clin$cancer_diagnosis_diagnoses <- 
    ifelse(grepl('sarcoma', clin$cancer_diagnosis_diagnoses), 
           'Sarcoma',
           ifelse(grepl('All', clin$cancer_diagnosis_diagnoses), 
                  'ALL', 
                  ifelse(nchar(clin$cancer_diagnosis_diagnoses) < 4, 
                         toupper(clin$cancer_diagnosis_diagnoses), 
                         ifelse(nchar(clin$cancer_diagnosis_diagnoses) >= 4,
                                Hmisc::capitalize(clin$cancer_diagnosis_diagnoses), clin$cancer_diagnosis_diagnoses))))

  
  
  
  
  full_data <- full_data_first[!duplicated(full_data_first$ids), ]
  # 
  full_data1 <- full_data[full_data$ids %in% image_ids,]
  full_data <- full_data1

  full_data$fam_cancer_ratio <- full_data$fam_num_cancer <- full_data$tm_donor_ <-
    full_data$family_name <- full_data$a <- full_data$b <- full_data$F <- full_data$M <-
    full_data$p53_germline <- full_data$sentrix_id <- full_data$gdna.base.change <- full_data$gdna.exon.intron <- NULL
  
  saveRDS(full_data, '~/Desktop/image_methy.rda')
  # read in ids csvs to get data type identifioer
  methyl_ids <- readRDS('data/methyl_ids.rda')
  image_ids <- read_csv('data/image_ids.csv')
  image_ids <- as.character(image_ids$ids)
  dna_ids <- read_csv('data/seq_ids.csv')
  colnames(dna_ids) <- c('ids', 'seq_status')
  # for now remove everything but numbers in id
  dna_ids$ids <- gsub('A|B|_', '',dna_ids$ids)
  
  # dna_ids <- dna_ids %>%
  #   group_by(ids) %>%
  #   summarise(cpanel = sum(seq_mod == 'cpanel'),
  #             wgs = sum(seq_mod == 'wgs'),
  #             wts = sum(seq_mod == 'wts'))
  # 
  # recode cancer to new variable
  
  
  
  clin$blood_dna_malkin_lab <- as.character(clin$blood_dna_malkin_lab)
  clin$blood_dna_malkin_lab <- trimws(clin$blood_dna_malkin_lab, 'both')
  
  # 20, 258
  # add a column to clin that indicates TRUE, if overlapping ids exists. 
  clin$methyl_status <- ifelse(clin$blood_dna_malkin_lab %in% methyl_ids, 'yes', 'no')
  clin$image_status <- ifelse(clin$blood_dna_malkin_lab %in% image_ids, 'yes', 'no')
  
  clin <- left_join(clin, dna_ids, by = c('blood_dna_malkin_lab' = 'ids'))
  
  clin$seq_status[is.na(clin$seq_status)] <- 'None'

  # recode cancer diagnosis for app
  unique(clin$cancer_diagnosis_diagnoses)
  
  save.image('data/new_clin.RData')
  
  
}

cancer_names <- sort(unique(clin$cancer_diagnosis_diagnoses))

clin <- clin[clin$tm_donor != '3277',]
