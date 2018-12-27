# a <- read_html(x) %>% 
#   html_nodes('.mainrow') %>% 
#   length

dateRetrieve <- function(x){
  
  a <- read_html(x) %>% 
    html_nodes('.mainrow') %>% 
    length
  
  if(a > 0){
    out <-  read_html(x) %>% 
    html_table(fill = T) %>% 
    matrix %>% 
    .[5,] %>%
    data.frame %>% 
    select(`Original.PaperDate.PubMedID.DOI`,`Retraction.or.Other.NoticesDate.PubMedID.DOI`) %>%
    apply(X = ., 1., function(x){
      str_sub(x,1,10)
    }) %>% 
    t
    return(out)
    }
  
  else if (a <= 0){
    return(NULL)
  }
  
}

#####

rw_query <- function(x){
  
dates <- dateRetrieve(x)

link <- read_html(x) 

a <- link %>% 
  html_nodes('.mainrow') %>% 
  length
  
if(a > 0){

  out <- list()
  
  for(i in 1:a){
  
  out[[i]] <- list(
 authors = link %>% 
    html_nodes('.mainrow') %>% 
    .[i] %>% 
    html_children() %>% 
    html_nodes('.authorLink') %>% 
    html_text() %>% 
    matrix,
  article = link %>% 
    html_nodes('.mainrow') %>% 
    .[i] %>% 
    html_children() %>% 
    html_nodes('.rTitle') %>% 
    html_text() %>% 
    matrix,
  subject = link %>% 
    html_nodes('.mainrow') %>% 
    .[i] %>% 
    html_children() %>% 
    html_nodes('.rSubject') %>% 
    html_text() %>% 
    matrix,
  journal = link %>% 
    html_nodes('.mainrow') %>% 
    .[i] %>% 
    html_children() %>% 
    html_nodes('.rJournal') %>% 
    html_text() %>% 
    matrix %>% 
    .[1,],
  institution = link %>% 
    html_nodes('.mainrow') %>% 
    .[i] %>% 
    html_children() %>% 
    html_nodes('.rInstitution') %>% 
    html_text() %>% 
    matrix,
  reason = link %>% 
    html_nodes('.mainrow') %>% 
    .[i] %>% 
    html_children() %>% 
    html_nodes('.rReason') %>% 
    html_text() %>% 
    matrix,
  doi = link %>% 
    html_nodes('.mainrow') %>% 
    .[i] %>% 
    html_children() %>% 
    html_nodes('.rNature') %>% 
    html_text() %>% 
    matrix,
 pub_date = dates[i,1],
 retract_date = dates[i,2]
  )
  }
  
return(out)
}

else if (a <= 0){
  return(NULL)
}
  
}

#####

grep(pattern = '.html',
     x = list.files(),
     value = T) %>% 
  .[c(1:5)] %>% 
  matrix %>%
  apply(X = ., MARGIN = 1, FUN = rw_query)

#####

subj <- str_split("%28B%2FT%29+Business+-+Accounting+OR+%28B%2FT%29+Business+-+Economics+OR+%28B%2FT%29+Business+-+General+OR+%28B%2FT%29+Business+-+Management+OR+%28B%2FT%29+Business+-+Manufacturing+OR+%28B%2FT%29+Business+-+Marketing+OR+%28B%2FT%29+Business+-+Public+Relations+OR+%28B%2FT%29+Computer+Science+OR+%28B%2FT%29+Foreign+Aid+OR+%28B%2FT%29+Government+OR+%28B%2FT%29+International+Relations+OR+%28B%2FT%29+Technology+OR+%28B%2FT%29+Transportation+OR+%28B%2FT%29+Urban+Planning+OR+%28BLS%29+Agriculture+OR+%28BLS%29+Anatomy%2FPhysiology+OR+%28BLS%29+Anthropology+OR+%28BLS%29+Archeology+OR+%28BLS%29+Biochemistry+OR+%28BLS%29+Biology+-+Cancer+OR+%28BLS%29+Biology+-+Cellular+OR+%28BLS%29+Biology+-+General+OR+%28BLS%29+Biology+-+Molecular+OR+%28BLS%29+Forensic+Sciences+OR+%28BLS%29+Genetics+OR+%28BLS%29+Microbiology+OR+%28BLS%29+Neuroscience+OR+%28BLS%29+Nutrition+OR+%28BLS%29+Parasitology+OR+%28BLS%29+Plant+Biology%2FBotany+OR+%28BLS%29+Zoology+OR+%28BLS%29+Toxicology+OR+%28ENV%29+Climate+Change+OR+%28ENV%29+Climatology+OR+%28ENV%29+Ecology+OR+%28ENV%29+Environmental+Sciences+OR+%28ENV%29+Food+Science+OR+%28ENV%29+Ground%2FSurface+Water+OR+%28HSC%29+Biostatistics%2FEpidemiology+OR+%28HSC%29+Medicine+-+Alternative+OR+%28HSC%29+Medicine+-+Anesthesia%2FAnaesthesia+OR+%28HSC%29+Medicine+-+Cardiology+OR+%28HSC%29+Medicine+-+Cardiovascular+OR+%28HSC%29+Medicine+-+Dentistry+OR+%28HSC%29+Medicine+-+Dermatology+OR+%28HSC%29+Medicine+-+Diabetes+OR+%28HSC%29+Medicine+-+Drug+Design+OR+%28HSC%29+Medicine+-+Endocrinology+OR+%28HSC%29+Medicine+-+Gastroenterology+OR+%28HSC%29+Medicine+-+General+OR+%28HSC%29+Medicine+-+Geriatric+OR+%28HSC%29+Medicine+-+Immunology+OR+%28HSC%29+Medicine+-+Infectious+Disease+OR+%28HSC%29+Medicine+-+Internal+OR+%28HSC%29+Medicine+-+Neurology+OR+%28HSC%29+Medicine+-+Nursing+OR+%28HSC%29+Medicine+-+Obstetrics%2FGynecology+OR+%28HSC%29+Medicine+-+Oncology+OR+%28HSC%29+Medicine+-+Ophthalmology+OR+%28HSC%29+Medicine+-+Orthopedics+OR+%28HSC%29+Medicine+-+Pathology+OR+%28HSC%29+Medicine+-+Otorhinolaryngology+OR+%28HSC%29+Medicine+-+Pediatrics+OR+%28HSC%29+Medicine+-+Pharmacology+OR+%28HSC%29+Medicine+-+Psychiatry+OR+%28HSC%29+Medicine+-+Pulmonology+OR+%28HSC%29+Medicine+-+Rehabilitation%2FTherapy+OR+%28HSC%29+Medicine+-+Sports+OR+%28HSC%29+Medicine+-+Surgery+OR+%28HSC%29+Medicine+-+Urology+OR+%28HSC%29+Nutrition+OR+%28HSC%29+Occupational+Health+and+Safety+OR+%28HSC%29+Public+Health+and+Safety+OR+%28HSC%29+Radiology%2FImaging+OR+%28HSC%29+Sports+Science+OR+%28HSC%29+Veterinary+Science+OR+%28HUM%29+Architecture+OR+%28HUM%29+Arts+-+Biography+OR+%28HUM%29+Arts+-+Film+Studies+OR+%28HUM%29+Arts+-+General+OR+%28HUM%29+Arts+-+Music+OR+%28HUM%29+Arts+-Literature%2FPoetry+OR+%28HUM%29+Cartography+OR+%28HUM%29+History+-+Africa+OR+%28HUM%29+History+-+Asia+OR+%28HUM%29+History+-+Europe+OR+%28HUM%29+History+-+General+OR+%28HUM%29+History+-+South+America+OR+%28HUM%29+History+-+North+America+OR+%28HUM%29+History+-+United+States+OR+%28HUM%29+Philosophy+OR+%28HUM%29+Religion+OR+%28PHY%29+Astronomy+OR+%28PHY%29+Astrophysics+OR+%28PHY%29+Cosmology+OR+%28PHY%29+Chemistry+OR+%28PHY%29+Crystallography%2FSpectroscopy+OR+%28PHY%29+Energy+OR+%28PHY%29+Engineering+-+Chemical+OR+%28PHY%29+Engineering+-+Electrical+OR+%28PHY%29+Engineering+-+General+OR+%28PHY%29+Engineering+-+Mechanical+OR+%28PHY%29+Engineering+-+Structural+OR+%28PHY%29+Forensic+Sciences+OR+%28PHY%29+Geology+OR+%28PHY%29+Hydrology+OR+%28PHY%29+Materials+Science+OR+%28PHY%29+Mathematics+OR+%28PHY%29+Nanotechnology+OR+%28PHY%29+Physics+OR+%28PHY%29+Statistics+OR+%28PUB%29+Institutional+Journals+OR+%28PUB%29+Society%2FAssociation+Journals+OR+%28SOC%29+Communications+OR+%28SOC%29+Criminology+OR+%28SOC%29+Education+OR+%28SOC%29+Ethics%2FBioethics+OR+%28SOC%29+Forensics+OR+%28SOC%29+Linguistics+OR+%28SOC%29+Law%2FLegal+Issues+OR+%28SOC%29+Military%2FNaval+Studies+OR+%28SOC%29+Philosophy+OR+%28SOC%29+Political+Science+OR+%28SOC%29+Psychology+OR+%28SOC%29+Sexual+And+Marital+Therapy+OR+%28SOC%29+Sociology+OR+%28SOC%29+Sports+and+Recreation",pattern = "OR") %>% 
  unlist

subj[-1] <- substring(subj[-1], 2)

subj[1:(length(subj)-1)] <- substr(subj[1:(length(subj)-1)],1,nchar(subj[1:(length(subj)-1)])-1)
