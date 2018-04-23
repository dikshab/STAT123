X919_data <- read_excel("919report_data.xlsx")
summary(X919_data)

#Case #           Date               Time             Involved        
#Min.   :  1.00   Length:161         Length:161         Length:161        
#1st Qu.:  7.00   Class :character   Class :character   Class :character  
#Median : 21.00   Mode  :character   Mode  :character   Mode  :character  
#Mean   : 52.43                                                           
#3rd Qu.:117.00                                                           
#Max.   :188.00                                                           
#Name/Description      Gender             Height         
#Length:161         Length:161         Length:161        
#Class :character   Class :character   Class :character  
#Mode  :character   Mode  :character   Mode  :character  



#Weight (lbs)       Hair Colour        Emergency Services Required
#Length:161         Length:161         Length:161                 
#Class :character   Class :character   Class :character           
#Mode  :character   Mode  :character   Mode  :character           



#Call initiated to: Police (911) Call initiated to: Ambulance  (911)
#Length:161                      Length:161                         
#Class :character                Class :character                   
#Mode  :character                Mode  :character                   



#Call initiated to: Fire (911) Attended By: Police Attended By: Ambulance
#Length:161                    Length:161          Length:161            
#Class :character              Class :character    Class :character      
#Mode  :character              Mode  :character    Mode  :character      



#Attended By:Fire    Vehicle ID        Incident Teyp(S):  Medical
#Length:161         Length:161         Length:161                
#Class :character   Class :character   Class :character          
#Mode  :character   Mode  :character   Mode  :character          



#Incident Teyp(S):  Injury Incident Teyp(S):  Drug/Alcohol
#Length:161                Length:161                     
#Class :character          Class :character               
#Mode  :character          Mode  :character               



#Incident Teyp(S): Violence Incident Teyp(S): Staff Abuse
#Length:161                 Length:161                   
#Class :character           Class :character             
#Mode  :character           Mode  :character             



#Incident Teyp(S):  Refuse to leave Incident Teyp(S): Property Damage
#Length:161                         Length:161                       
#Class :character                   Class :character                 
#Mode  :character                   Mode  :character                 



#Incident Teyp(S): Other (Type) First Aid required   Overdose        
#Length:161                     Length:161         Length:161        
#Class :character               Class :character   Class :character  
#Mode  :character               Mode  :character   Mode  :character  



#Naloxone         Event Location: Drop In Event Location: Lobby
#Length:161         Length:161              Length:161           
#Class :character   Class :character        Class :character     
#Mode  :character   Mode  :character        Mode  :character     



#Event Location: Courtyard Event Location: Kitchen
#Length:161                Length:161             
#Class :character          Class :character       
#Mode  :character          Mode  :character       



#Event Location: Dinning Area Event Location: Chapel
#Length:161                   Length:161            
#Class :character             Class :character      
#Mode  :character             Mode  :character      



#Event Location: Hygiene Event Location: Games room
#Length:161              Length:161                
#Class :character        Class :character          
#Mode  :character        Mode  :character          



#Event Location: Shipping&Reveiving Area
#Length:161                             
#Class :character                       
#Mode  :character                       



#Event Location: Common Housing Area Event Location: In Room
#Length:161                          Length:161             
#Class :character                    Class :character       
#Mode  :character                    Mode  :character       



#Event Location: Perimeter Event Location:Snack Bar Other: (Enter)    
#Length:161                Length:161               Length:161        
#Class :character          Class :character         Class :character  
#Mode  :character          Mode  :character         Mode  :character  

timing_cat <- c("Dawn", "Morning", "Noon", "Afternoon", "Evening", "Night")

# Dawn -> 12:01 am to 4:00 am      0.00069444444444444 to 0.16666666666666667 
# Morning -> 4:01 am to 8:00 am    0.16736111111111111 to 0.33333333333333333
# Noon -> 8:01 am to 12:00 pm      0.33402777777777778 to 0.5
# Afternoon -> 12:01 pm to 4:00 pm 0.50069444444444444 to 0.66666666666666667
# Evening -> 4:01 pm to 8:00 pm    0.66736111111111111 to 0.83333333333333333
# Night -> 8:01 pm to 12:00 am     0.83402777777777778 to 1
 

time_vector <- X919_data$Time
len <- length(time_vector)
char_time <- vector(mode = "character", length = len) # create an empty vector

for(i in 1:len){
  if(time_vector[i] > 0 && time_vector[i] <= 0.16666666666666667){
    char_time[i] = "Dawn"
  } else if(time_vector[i] > 0.16666666666666667 && time_vector[i] <= 0.33333333333333333){
    char_time[i] = "Morning"
  } else if(time_vector[i] > 0.33333333333333333 && time_vector[i] <= 0.5){
    char_time[i] = "Noon"
  } else if(time_vector[i] > 0.5 && time_vector[i] <= 0.66666666666666667){
    char_time[i] = "Afternoon"
  } else if(time_vector[i] > 0.66666666666666667 && time_vector[i] <= 0.83333333333333333){
    char_time[i] = "Evening"
  } else{
    char_time[i] = "Night"
  }
}


char_time
#[1] "Afternoon" "Night"     "Noon"      "Noon"      "Noon"      "Noon"      "Afternoon" "Afternoon" "Afternoon" "Evening"   "Evening"   "Morning"   "Morning"   "Noon"      "Noon"     
#[16] "Night"     "Afternoon" "Dawn"      "Evening"   "Night"     "Night"     "Evening"   "Night"     "Night"     "Morning"   "Afternoon" "Afternoon" "Night"     "Evening"   "Evening"  
#[31] "Evening"   "Evening"   "Night"     "Afternoon" "Afternoon" "Night"     "Noon"      "Afternoon" "Evening"   "Evening"   "Evening"   "Dawn"      "Afternoon" "Evening"   "Evening"  
#[46] "Afternoon" "Afternoon" "Night"     "Noon"      "Noon"      "Afternoon" "Afternoon" "Evening"   "Morning"   "Noon"      "Noon"      "Noon"      "Afternoon" "Night"     "Noon"     
#[61] "Noon"      "Night"     "Dawn"      "Noon"      "Noon"      "Afternoon" "Afternoon" "Afternoon" "Noon"      "Afternoon" "Evening"   "Dawn"      "Noon"      "Night"     "Dawn"     
#[76] "Evening"   "Night"     "Night"     "Noon"      "Noon"      "Noon"      "Noon"      "Night"     "Afternoon" "Noon"      "Afternoon" "Afternoon" "Afternoon" "Night"     "Afternoon"
#[91] "Evening"   "Evening"   "Noon"      "Noon"      "Noon"      "Afternoon" "Afternoon" "Evening"   "Evening"   "Noon"      "Noon"      "Afternoon" "Noon"      "Night"     "Noon"     
#[106] "Afternoon" "Evening"   "Noon"      "Afternoon" "Evening"   "Evening"   "Evening"   "Evening"   "Noon"      "Afternoon" "Dawn"      "Evening"   "Evening"   "Evening"   "Afternoon"
#[121] "Afternoon" "Evening"   "Evening"   "Noon"      "Noon"      "Afternoon" "Afternoon" "Afternoon" "Afternoon" "Night"     "Afternoon" "Afternoon" "Evening"   "Morning"   "Afternoon"
#[136] "Evening"   "Evening"   "Night"     "Noon"      "Afternoon" "Evening"   "Evening"   "Evening"   "Evening"   "Afternoon" "Evening"   "Night"     "Afternoon" "Evening"   "Evening"  
#[151] "Morning"   "Noon"      "Noon"      "Evening"   "Evening"   "Night"     "Morning"   "Noon"      "Noon"      "Evening"   "Evening"  


timing_cat <- c("Dawn", "Morning", "Noon", "Afternoon", "Evening", "Night" )
factor_time <- factor(char_time, ordered = T, levels = timing_cat)
 
summary(factor_time)
#Dawn   Morning      Noon Afternoon   Evening     Night 
#6         7        39        43        44        22 

#Most of the incidents happen between noon and 8pm, so no, there are not more incidents happening at the end of the day



# cleaning up the NAs
summary(X919_data[is.na(X919_data$'Call initiated to: Police (911)')])

ambulance <- X919_data$`Call initiated to: Ambulance  (911)`
overdosed <- X919_data$Overdose

amb_df <- as.data.frame(ambulance)
amb_df[is.na(amb_df)] <- 0 # replacing na with 0
over_df <- as.data.frame(overdosed)
over_df[is.na(over_df)] <- 0
