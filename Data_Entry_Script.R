
library(rstatix)

fn<- function(){
  filelocation<-'PathIn'
  filename<- 'RowBySession_CCABG.csv'
  while(TRUE){
    ID<- suppressWarnings(as.numeric(readline("Enter ID:")))

    if(!is.na(ID)){
      ID<<-ID
      break
    }
    else(
      print('Incorrect entry. Try again.')
    )
  }
  while(TRUE){
    v<- readline("Enter Visit (options: BL, DC, 4m, 6m, 1y, 3y):")
    if(nchar(v)==2){
      v<<-v
      break
    }
    else(
      print('Incorrect entry. Try again.')
    )
  }
  
  if(v == 'BL'){
    visit<-"Baseline"
  }
  if(v == 'DC'){
    visit<-"Discharge"
  }
  if(v == '4m'){
    visit<- "4-month follow-up"
  }
  if(v == '6m'){
    visit<-"6-month follow-up"
  }
  if(v == '1y'){
    visit<-"12-month follow-up"
  }
  if(v == '3y'){
    visit<-"3-year follow-up"
  }
  
  decision<-readline(paste("You are entering data for CCABG", ID, "for their", visit, "visit. Correct? (y/n):"))
  if(decision != 'y'){
    fn()
  }
  
  
  data<-read.csv(paste(filelocation,filename,sep=""))
  
  #make ID formatted for table
  ID2<- paste('CCABG', strrep('0', (4-nchar(ID))),ID, sep="")
  
  
  cantab<-filter(data, data$ID.number == ID2 & data$Assessment.Name == visit)
  cantab<<-cantab
  cantab<-cantab[-c(1:13)]
  
  columns<-read.csv("//rdfs.unisa.edu.au//Group_bbb_research//CAIN//PROJECTS//2018_Cognitive_CABG_Study//DATA//REDCap//CANTAB//RedCAP CANTAB importer tool//DO_NOT_DELETE_OR_MOVE.csv")
  columns<-colnames(columns)
  columns[1]<-"participant_number"
  
  #create matrix
  final<-matrix(ncol = length(columns), nrow = 1)
  colnames(final)<-columns
  final[1]<-ID
  
  option<-c("BL","DC","4m","6m","1y","3y")
  correspond<-c("baseline_visit_1_arm_1", "discharge_visit_2_arm_1", "4month_visit_3_arm_1", "6month_visit_4_arm_1", "1year_visit_5_arm_1", "3years_visit_6_arm_1")
  
  om<-matrix(nrow=2, ncol=length(option))
  om[1,]=option
  om[2,]=correspond
  
  countr<-0
  for(o in om[1,]){
    countr<-countr+1
    if(o == v){
      redcapid<-om [2,countr]
      break
    }
  }
  
  final[2]<-redcapid
  
  for(l in 1:length(cantab)){
    final[l+2]<-cantab[1,l]
  }
  
  z<-strrep('0', (3-nchar(ID)))
  
  pathOUT<-"PathOut"
  write.csv(final, file = paste(pathOUT,"CANTAB_REDCap_import_CCABG",z, ID, "_", visit,".csv", sep=""), row.names = F)
  print("All done. Have a good day!")
}

fn()

