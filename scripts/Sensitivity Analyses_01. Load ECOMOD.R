##_________________________________
##
## ECOMOD FOR SENSITIVITY ANALYSES
##_________________________________
##

# See other ECOMOD file for full description. This script contains versions
# of ECOMOD adjusted for sensitivity analyses.

# *****************************************************************************

rm(list=ls())

## LOWER LIMITS ----

##    ECOMOD - lower limits, estimated yield ----

solve_BGRI_ECOMOD_l_est <- function(filename,farmdata,default,farm="single",soil,rotlength,rotprob=NULL,crops,tillages,seedrate,delsowing,Nfert,Pfert,Kfert,bgherbdose,
                                    glyphosatedose,numberofsprays,subsidy="yes",blackgrass,cropprice,cropyield,yieldoption="estimate",Nfertprice,
                                    Pfertprice,Kfertprice,seedprice,herbprice,glyphosateprice,machsize,fuelprice,labourwage){  
  
  # THE ORDER OF MODEL INPUTS SUCH AS TILLAGE, SEED RATE N FERTILISER RATES ETC MUST CORRESPOND TO THE ORDER OF CROPS
  # FOR EXAMPLE IF FIRST CROP IS WINTER WHEAT, THEN IN THE NFERT VECTOR, FIRST VALUE MUST CORRESPOND TO WINTER WHEAT N FERTILISER RATE.
  
  # Machine size (machsize) vector MUST be entered in the following order: 1. Tractor szie (kW); 2. Roller size (m);
  # 3. Power harrow size (m); 4. Sprayer tank size (litres); 5. Combine harvester size (kW)
  # For example machsize = c(102,6,4,1400,125)
  
  
  if(farm=="multiple"&is.null(farmdata)){
    stop("RUNNING MODEL FOR MULTIPLE FIELDS CANNOT BE BASED ON SINGLE FIELD/FARM DATA: USE MULTIPLE FIELD/FARM DATA & EQUATE farmdata TO THE DATA")
  }
  
  
  ECOMOD <- function(farm,default,soil,rotlength,rotprob,crops,tillages,seedrate,delsowing,Nfert,Pfert,Kfert,bgherbdose,
                     glyphosatedose,numberofsprays,subsidy,blackgrass,cropprice,cropyield,yieldoption,Nfertprice,
                     Pfertprice,Kfertprice,seedprice,herbprice,glyphosateprice,machsize,fuelprice,labourwage){
    
    # Setting the model to generate default results ******
    
    if(is.null(default)){
      soil <- soil
      rotlength <- rotlength
      crops <- crops
      tillages <- tillages
      seedrate <- seedrate
      delsowing <- delsowing
      Nfert <- Nfert
      Pfert <- Pfert
      Kfert <- Kfert
      bgherbdose <- bgherbdose
      glyphosatedose <- glyphosatedose
      numberofsprays <- numberofsprays
      blackgrass <- blackgrass
      cropprice <- cropprice
      cropyield <- cropyield
      Nfertprice <- Nfertprice
      Pfertprice <- Pfertprice
      Kfertprice <- Kfertprice
      seedprice <- seedprice
      herbprice <- herbprice 
      glyphosateprice <- glyphosateprice
      machsize <- machsize
      fuelprice <- fuelprice
      labourwage <- labourwage
      
    }else if(default=="yes"){ # DEFAULT MODEL INPUT DATA *******************
      soil <- 2.5
      crops <- c("winterwheat","winterwheat","wosr","winterwheat","springbeans","winterwheat")
      tillages <- c("lightcultivation","lightcultivation","deepcultivation","deepcultivation","deepcultivation","deepcultivation")
      seedrate <- c(185,175,3.5,185,334,185)
      delsowing <- c("late","no","late","no","late","late")
      Nfert <- c(180,188,167,190,0,174)
      Pfert <- c(95,95,80,95,70,95)
      Kfert <- c(115,115,70,115,70,115)
      bgherbdose <- c(3.16,7.24,2.54,10.97,10.17,1.7)
      glyphosatedose <- c(2,1.6,4,2.23,4.1,0.98) 
      numberofsprays <- c(3,3,4,4,6,4)
      blackgrass <- c("low","low","low","low","low","low") #**** Needs changing
      cropprice <- c(150,150,335,150,185,150)
      cropyield <- c(9.1,9,3.91,9.8,5.9,10)
      Nfertprice <- 0.78
      Pfertprice <- 0.71
      Kfertprice <- 0.44
      seedprice <- c(0.36,0.36,7.34,0.36,0.38,0.36)
      herbprice <- c(19.5,19.5,19.5,19.5,19.5,19.5) # An average from a farm data
      glyphosateprice <- c(2.43,2.43,2.43,2.43,2.43,2.43) # An average from a farm data
      machsize <- c(102,6,4,1400,125) # Assumed machine sizes
      fuelprice <- 0.625 # ABC Nov 2018, 87th ed
      labourwage <- 10.08 # Nix 2019, 49th ed
      subsidy <- "yes"
    }
    
    #************* Rotation Length Assumption *****************
    lcr <- 6 #rotlength = 6 # Length of vector based on max years of rotation which is 6
    if(length(crops)>lcr){
      warning("Maximum 6-year rotation is assumed (Length of crops vector MUST be 6)")
    }else{
      crops <- crops
    }
    
    if(length(tillages)>6){
      warning("Length of tillage vector MUST be 6")
    }else{
      tillages <- tillages
    }
    
    if(length(machsize)>5){
      warning("Length of machine sizes vector MUST be 5")
      # The machine size must be set in the following order: Tractor size (kW), roller size (m),
      # power harrow size (m), Sprayer size (litres), combine harvester size (kW)
    }else{
      tillages <- tillages
    }
    
    if(length(Nfert)>lcr||length(Pfert)>lcr||length(Kfert)>lcr||length(seedrate)>lcr||length(seedprice)>lcr){
      warning("Maximum 6-year rotation is assumed (Length of crops vector MUST be 6)")
    }else{
      Nfert <- Nfert; Pfert <- Pfert; Kfert <- Kfert; seedrate <- seedrate; seedprice <- seedprice
    }
    
    if(length(bgherbdose)>lcr||length(glyphosatedose)>lcr||length(numberofsprays)>lcr||
       length(cropprice)>lcr||length(cropyield)>lcr||length(seedprice)>lcr||length(herbprice)>lcr){
      warning("Maximum 6-year rotation is assumed (Length of crops vector MUST be 6)")
    }else{
      bgherbdose <- bgherbdose; glyphosatedose <- glyphosatedose; numberofsprays <- numberofsprays;
      cropprice <- cropprice; cropyield <- cropyield; seedprice <- seedprice; herbprice <- herbprice
    }
    
    # Setting soil type
    soi <- c(0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5) # Soil type indices
    if(soil%in%soi==TRUE){
      nsoil <- soil
    }else{
      nsoil <- 2.5 # This sets the soil type to heavy soil (Clay)
      warning("Enter correct soil type: must be between 0.5 and 2.5 at an interval of 0.25. Check that input soil values are formatted as 'numeric' and have 2 dec pl.")
    }
    
    cropBudget <- function(crop,asoil,subsidy,blackgrass,tillage,selfrot,rotpen,yieldoption){ #***********************************************************
      
      # This function estimates gross margin (£/ha) and gross profit (£/ha) for each crop taking into
      # consideration soil type, yield loss due to black-grass infestation,
      # farm operation and machine types, and fuel price.
      
      # Creation of Data files *******
      Input_Output <- c("N Fertiliser","P Fertiliser","K Fertiliser",
                        "N Fertiliser Price","P Fertiliser Price","K Fertiliser Price",
                        "Seed Rate","Seed Price","BG Herbicide Rate","BG Herbicide Price",
                        "Glyphosate Rate", "Glyphosate Price","Number of Sprays",
                        "Sugarbeet Transport","Fertiliser Cost","Seed Cost",
                        "Herbicide Cost","Sundry Cost","Sugarbeet Transport Cost",
                        "Primary Yield","Primary Yield Price",
                        "Secondary Yield","Secondary Yield Price",
                        "Subsidy","Farm Output","Variable Cost",
                        "Gross Margin","Fuel Cost","Labour Cost","Operating Cost",
                        "Gross Profit") # Crop inputs and output parameters
      lcr <- 6 # Equals the number of crops
      cl <- c(rep(0,length(Input_Output)*lcr))
      cd <- matrix(cl, ncol=lcr, byrow = T)
      cols <- c("Input_Output",crops)
      cd1 <- data.frame(Input_Output,cd)
      nfp <- c(rep(Nfertprice,lcr)); pfp <- c(rep(Pfertprice,lcr)); kfp <- c(rep(Kfertprice,lcr))
      
      inp <- rbind(Nfert,Pfert,Kfert,nfp,pfp,kfp,seedrate,seedprice,bgherbdose,herbprice,glyphosatedose,
                   glyphosateprice,numberofsprays) # Putting input data together to create crop data
      
      cd1[seq(1,13),seq(2,7)] <- inp
      cd1[20,seq(2,7)] <- cropyield
      cd1[21,seq(2,7)] <- cropprice
      colnames(cd1) <- cols
      crd <- fi1 <- cd1
      
      # Updated 07/02/2023. Assumed sundry costs are informed by Nix (2019) and ABC (2019).
      # Sundry costs include the average per hectare cost of herbicides NOT targeting BG, from the BGRI data, 
      # as well as cost of other chemicals such as fungicide, insecticides, growth regulators etc.,
      # plus marketing, agronomy costs, in-store chemicals. Data are from 'BGRI_ECOMOD_Chem & Sundry Costs_2023-02-07.xlsx".
      #snwwt <- 146; snswt <- 95; snwba <- 108; snsba <- 81; snwbe <- 101; snsbe <- 89; snwpo <- 1200; snwos <- 110; snsbt <- 409; snset <- 2;
      #snsos <- 67; sndpe <- 156; snwln <- 75; snsln <- 73; snwoa <- 80; snsoa <- 89; 
      snwwt <- 182; snswt <- 100; snwba <- 158; snsba <- 151; snwbe <- 91; snsbe <- 81; snwpo <- 2215; snwos <- 192; snsbt <- 488; snset <- 2;
      snsos <- 140; sndpe <- 155; snwln <- 88; snsln <- 53; snwoa <- 148; snsoa <- 109;    
      
      
      
      # Yield estimates based on soil type and N fertiliser amounts (Response functions obtained from SAFMOD)
      wwt <- round(((11.841-(9.211*(0.9907^(fi1$winterwheat[1])))-(0.0075*(fi1$winterwheat[1])))*(0.743+0.1714*(nsoil))*0.947),2)
      swt <- round((5.885-(2.893*(0.984^(fi1$springwheat[1]))))*(0.73+0.18*(nsoil)),2)
      wba <- round((((12.967-(10.029*(0.993^(fi1$winterbarley[1])))-(0.0147*(fi1$winterbarley[1])))*(0.76+0.16*(nsoil)))*0.89),2)
      sba <- round(((19.98-(18.164*(0.9952^(fi1$springbarley[1])))-(0.0364*(fi1$springbarley[1])))*(0.887+0.075*(nsoil))*1.02),2)
      wbe <- round((((0.95+1.3625*(nsoil))*1.1)*1.05),2)
      sbe <- round((((0.7+1.25*(nsoil))*1.05)*1.2),2)
      wpo <- round((44.507-(29.135*(0.992^fi1$warepotatoes[1])))*1.16,2)
      wos <- round((((3.35+(-0.623*(0.010^(fi1$wosr[1])))-0.000324*(fi1$wosr[1]))*(0.655+0.23*(nsoil)))*0.87),2)
      sbt <- round(((54.543-(0.05*37.82*(0.984^fi1$sugarbeet[1])))*1.30),2)
      #set <- 0
      sos <- round((2.317-(1.139*(0.984^(fi1$sosr[1])))),2)
      wln <- round(0.75+0.45*1.5*(nsoil),2)
      sln <- round(0.75+0.45*(nsoil)*0.95,2)
      dpe <- round((2.48+3.475*(nsoil)-(1.2875*(nsoil)^2)),2)
      woa <- round((((12.967-(10.029*(0.993^(fi1$winteroats[1])))-(0.0147*(fi1$winteroats[1])))*(0.76+0.16*(nsoil)))*0.89),2)
      soa <- round(((19.98-(18.164*(0.9952^(fi1$springoats[1])))-(0.0364*(fi1$springoats[1])))*(0.887+0.075*(nsoil))*1.02),2)
      
      # ========= Yield Penalty | Black-grass ========= 
      # ************ Winter wheat - black-grass infestation assumption ************
      # These yield penalties (abs/low BG density = 0, med=0, high=7.45, vhigh=25.6) are from Varah et al (2019) Nature Sustainability, based on BGRI data.
      # If better data are obtained, we suggest updating these penalties.
      
      # IF ACTUAL YIELD DATA ARE USED YIELD PENALTIES DUE TO BLACK-GRASS ARE NOT TAKEN INTO ACCOUNT
      
      bgf <- function(infestation){
        if(infestation=="low"){
          yieldloss <- 0; wwht1 <- wwt;
          yls <- 0
        }else if(infestation=="medium"){
          yieldloss <- 0; wwht1 <- wwt;
          yls <- 0
        }else if(infestation=="high"){
          yieldloss <- 0; wwht1 <- wwt;
          yls <- 0
        }else if(infestation=="veryhigh"){
          yieldloss <- 13; wwht1 <- round(wwt*(1-(13/100)),2)
          yls <- round((13/100)*wwht1,2)
        }else{
          yieldloss <- 0
          wwht1 <- wwt
          yls <- 0 
        }
        c(wwht1,yls) 
      }
      
      # ========= Yield Penalty | Sowing Date =========
      #************ Delayed Sowing ************
      # Delayed sowing has been modelled to take into account the degree of delay.
      # For example the sowing of winter wheat is optimal in late September and three classes or 
      # options for delayed sowing are defined for October (late), November (later) and December (latest)
      # IF ACTUAL YIELD DATA ARE USED YIELD PENALTIES DUE TO DELAYED SOWING ARE NOT TAKEN INTO ACCOUNT *********
      
      delsow <- function(sowtime){
        if(sowtime=="late"){
          ft1 <- 7/100; ft2 <- 4/100; ft3 <- 3/100; ft4 <- 2/100; ft5 <- 0/100
          ft6 <- 5/100; ft7 <- 10/100; ft8 <- 1/100; ft9 <- 1/100; ft10 <- 0/100; ft11 <- 0/100; ft12 <- 0/100; ft13 <- 0/100
          ds <- c(ft1,ft2,ft3,ft4,ft5,ft6,ft7,ft8,ft9,ft10,ft11,ft12,ft13)
        }else if(sowtime=="later"){
          ft1 <- 12/100; ft2 <- 9/100; ft3 <- 12/100; ft4 <- 5/100; ft5 <- 0/100
          ft6 <- 6/100; ft7 <- 11/100; ft8 <- 5/100; ft9 <- 5/100; ft10 <- 0/100; ft11 <- 0/100; ft12 <- 0/100; ft13 <- 3/100
          ds <- c(ft1,ft2,ft3,ft4,ft5,ft6,ft7,ft8,ft9,ft10,ft11,ft12,ft13)
        }else if(sowtime=="latest"){
          ft1 <- 15/100; ft2 <- 17/100; ft3 <- 18/100; ft4 <- 15/100; ft5 <- 0/100
          ft6 <- 6/100; ft7 <- 11/100; ft8 <- 5/100; ft9 <- 11/100; ft10 <- 0/100; ft11 <- 0/100; ft12 <- 0/100; ft13 <- 6/100
          ds <- c(ft1,ft2,ft3,ft4,ft5,ft6,ft7,ft8,ft9,ft10,ft11,ft12,ft13)
        }else if(sowtime=="no"){
          ft1 <- ft2 <- ft3 <- ft4 <- ft5 <- ft6 <- ft7 <- ft8 <- ft9 <- ft10 <- ft11 <- ft12 <- ft13 <- 0
          ds <- c(ft1,ft2,ft3,ft4,ft5,ft6,ft7,ft8,ft9,ft10,ft11,ft12,ft13)
        }else{
          ft1 <- ft2 <- ft3 <- ft4 <- ft5 <- ft6 <- ft7 <- ft8 <- ft9 <- ft10 <- ft11 <- ft12 <- ft13 <- 0
          ds <- c(ft1,ft2,ft3,ft4,ft5,ft6,ft7,ft8,ft9,ft10,ft11,ft12,ft13)
          warning("Check if delayed sowing option is spelt correctly; option MUST be: no, late, later or latest")
        }
        ds
      }
      
      seccost <- 65 # Secondary yield (straw) price assumed for cereal crops (wheat and barley crops) ********************
      c1 <- 18; c2 <- 19; c3 <- 20; c4 <- 21; c5 <- 22; c6 <- 23; c7 <- 24; c8 <- 25; c9 <- 26;# === row indices
      c10 <- 27; c11 <- 28; c12 <- 29; c13 <- 30; c14 <- 31; 
      
      h1 <- 10; h2 <- 11; h3 <- 12; h4 <- 13; h5 <- 14; h6 <- 15;
      h7 <- 16; h8 <- 17; 
      
      # ********** WW *********
      if(crops[1]=="winterwheat"){
        crd[c1,2] <- snwwt # Sundry cost
        crd[c6,2] <- seccost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2] 
          crd[c5,2] <- crd[c3,2]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- bgf(blackgrass[1])[1]-round(delsow(delsowing[1])[1]*bgf(blackgrass[1])[1],2) # wwt
          crd[c5,2] <- crd[c3,2]*0.5
        }
      }
      if(crops[2]=="winterwheat"){
        crd[c1,3] <- snwwt # Sundry cost
        crd[c6,3] <- seccost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- bgf(blackgrass[2])[1]-round(delsow(delsowing[2])[1]*bgf(blackgrass[2])[1],2) # wwt
          crd[c5,3] <- crd[c3,3]*0.5
        }
      }
      if(crops[3]=="winterwheat"){
        crd[c1,4] <- snwwt # Sundry cost
        crd[c6,4] <- seccost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4] 
          crd[c5,4] <- crd[c3,4]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- bgf(blackgrass[3])[1]-round(delsow(delsowing[3])[1]*bgf(blackgrass[3])[1],2) # wwt
          crd[c5,4] <- crd[c3,4]*0.5
        }
      }
      if(crops[4]=="winterwheat"){
        crd[c1,5] <- snwwt # Sundry cost
        crd[c6,5] <- seccost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- bgf(blackgrass[4])[1]-round(delsow(delsowing[4])[1]*bgf(blackgrass[4])[1],2) # wwt
          crd[c5,5] <- crd[c3,5]*0.5
        }
      } 
      if(crops[5]=="winterwheat"){
        crd[c1,6] <- snwwt # Sundry cost
        crd[c6,6] <- seccost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- bgf(blackgrass[5])[1]-round(delsow(delsowing[5])[1]*bgf(blackgrass[5])[1],2) # wwt
          crd[c5,6] <- crd[c3,6]*0.5
        }
      }
      if(crops[6]=="winterwheat"){
        crd[c1,7] <- snwwt # Sundry cost
        crd[c6,7] <- seccost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- bgf(blackgrass[6])[1]-round(delsow(delsowing[6])[1]*bgf(blackgrass[6])[1],2) # wwt
          crd[c5,7] <- crd[c3,7]*0.5
        }
      }
      
      #********** SPR WHT *********
      if(crops[1]=="springwheat"){
        crd[c1,2] <- snswt # Sundry cost
        crd[c6,2] <- seccost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- swt-round(delsow(delsowing[1])[2]*swt,2)
          crd[c5,2] <- crd[c3,2]*0.5
        }
      }
      if(crops[2]=="springwheat"){
        crd[c1,3] <- snswt # Sundry cost
        crd[c6,3] <- seccost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- swt-round(delsow(delsowing[2])[2]*swt,2)
          crd[c5,3] <- crd[c3,3]*0.5
        }
      }
      if(crops[3]=="springwheat"){
        crd[c1,4] <- snswt # Sundry cost
        crd[c6,4] <- seccost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- swt-round(delsow(delsowing[3])[2]*swt,2)
          crd[c5,4] <- crd[c3,4]*0.5
        }
      }
      if(crops[4]=="springwheat"){
        crd[c1,5] <- snswt # Sundry cost
        crd[c6,5] <- seccost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- swt-round(delsow(delsowing[4])[2]*swt,2)
          crd[c5,5] <- crd[c3,5]*0.5
        }
      } 
      if(crops[5]=="springwheat"){
        crd[c1,6] <- snswt # Sundry cost
        crd[c6,6] <- seccost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- swt-round(delsow(delsowing[5])[2]*swt,2)
          crd[c5,6] <- crd[c3,6]*0.5
        }
      }
      if(crops[6]=="springwheat"){
        crd[c1,7] <- snswt # Sundry cost
        crd[c6,7] <- seccost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- swt-round(delsow(delsowing[6])[2]*swt,2)
          crd[c5,7] <- crd[c3,7]*0.5
        }
      }
      
      #********** WBAR *********
      if(crops[1]=="winterbarley"||crops[1]=="winteroats"){
        crd[c1,2] <- snwba # Sundry cost
        crd[c6,2] <- seccost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- wba-round(delsow(delsowing[1])[3]*wba,2)
          crd[c5,2] <- crd[c3,2]*0.5
        }
      }
      if(crops[2]=="winterbarley"||crops[2]=="winteroats"){
        crd[c1,3] <- snwba # Sundry cost
        crd[c6,3] <- seccost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- wba-round(delsow(delsowing[2])[3]*wba,2)
          crd[c5,3] <- crd[c3,3]*0.5
        }
      }
      if(crops[3]=="winterbarley"||crops[3]=="winteroats"){
        crd[c1,4] <- snwba # Sundry cost
        crd[c6,4] <- seccost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- wba-round(delsow(delsowing[3])[3]*wba,2)
          crd[c5,4] <- crd[c3,4]*0.5
        }
      }
      if(crops[4]=="winterbarley"||crops[4]=="winteroats"){
        crd[c1,5] <- snwba # Sundry cost
        crd[c6,5] <- seccost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- wba-round(delsow(delsowing[4])[3]*wba,2)
          crd[c5,5] <- crd[c3,5]*0.5
        }
      } 
      if(crops[5]=="winterbarley"||crops[5]=="winteroats"){
        crd[c1,6] <- snwba # Sundry cost
        crd[c6,6] <- seccost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- wba-round(delsow(delsowing[5])[3]*wba,2)
          crd[c5,6] <- crd[c3,6]*0.5
        }
      }
      if(crops[6]=="winterbarley"||crops[6]=="winteroats"){
        crd[c1,7] <- snwba # Sundry cost
        crd[c6,7] <- seccost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- wba-round(delsow(delsowing[6])[3]*wba,2)
          crd[c5,7] <- crd[c3,7]*0.5
        }
      }
      #********** SPR BAR *********
      if(crops[1]=="springbarley"||crops[1]=="springoats"){
        crd[c1,2] <- snsba # Sundry cost
        crd[c6,2] <- seccost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- sba-round(delsow(delsowing[1])[4]*sba,2)
          crd[c5,2] <- crd[c3,2]*0.5
        }
      }
      if(crops[2]=="springbarley"||crops[2]=="springoats"){
        crd[c1,3] <- snsba # Sundry cost
        crd[c6,3] <- seccost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- sba-round(delsow(delsowing[2])[4]*sba,2)
          crd[c5,3] <- crd[c3,3]*0.5
        }
      }
      if(crops[3]=="springbarley"||crops[3]=="springoats"){
        crd[c1,4] <- snsba # Sundry cost
        crd[c6,4] <- seccost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- sba-round(delsow(delsowing[3])[4]*sba,2)
          crd[c5,4] <- crd[c3,4]*0.5
        }
      }
      if(crops[4]=="springbarley"||crops[4]=="springoats"){
        crd[c1,5] <- snsba # Sundry cost
        crd[c6,5] <- seccost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- sba-round(delsow(delsowing[4])[4]*sba,2)
          crd[c5,5] <- crd[c3,5]*0.5
        }
      } 
      if(crops[5]=="springbarley"||crops[5]=="springoats"){
        crd[c1,6] <- snsba # Sundry cost
        crd[c6,6] <- seccost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- sba-round(delsow(delsowing[5])[4]*sba,2)
          crd[c5,6] <- crd[c3,6]*0.5
        }
      }
      if(crops[6]=="springbarley"||crops[6]=="springoats"){
        crd[c1,7] <- snsba # Sundry cost
        crd[c6,7] <- seccost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- sba-round(delsow(delsowing[6])[4]*sba,2)
          crd[c5,7] <- crd[c3,7]*0.5
        }
      }
      
      #********** WBEA *********
      if(crops[1]=="winterbeans"){
        crd[c1,2] <- snwbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- wbe-round(delsow(delsowing[1])[5]*wbe,2)
          crd[c5,2] <- crd[c3,2]*0
        }
      }
      if(crops[2]=="winterbeans"){
        crd[c1,3] <- snwbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- wbe-round(delsow(delsowing[2])[5]*wbe,2)
          crd[c5,3] <- crd[c3,3]*0
        }
      }
      if(crops[3]=="winterbeans"){
        crd[c1,4] <- snwbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- wbe-round(delsow(delsowing[3])[5]*wbe,2)
          crd[c5,4] <- crd[c3,4]*0
        }
      }
      if(crops[4]=="winterbeans"){
        crd[c1,5] <- snwbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- wbe-round(delsow(delsowing[4])[5]*wbe,2)
          crd[c5,5] <- crd[c3,5]*0
        }
      } 
      if(crops[5]=="winterbeans"){
        crd[c1,6] <- snwbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- wbe-round(delsow(delsowing[5])[5]*wbe,2)
          crd[c5,6] <- crd[c3,6]*0
        }
      }
      if(crops[6]=="winterbeans"){
        crd[c1,7] <- snwbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- wbe-round(delsow(delsowing[6])[5]*wbe,2)
          crd[c5,7] <- crd[c3,7]*0
        }
      }
      
      #********** SPR BEA *********
      if(crops[1]=="springbeans"){
        crd[c1,2] <- snsbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- sbe-round(delsow(delsowing[1])[6]*sbe,2)
          crd[c5,2] <- crd[c3,2]*0
        }
      }
      if(crops[2]=="springbeans"){
        crd[c1,3] <- snsbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- sbe-round(delsow(delsowing[2])[6]*sbe,2)
          crd[c5,3] <- crd[c3,3]*0
        }
      }
      if(crops[3]=="springbeans"){
        crd[c1,4] <- snsbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- sbe-round(delsow(delsowing[3])[6]*sbe,2)
          crd[c5,4] <- crd[c3,4]*0
        }
      }
      if(crops[4]=="springbeans"){
        crd[c1,5] <- snsbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- sbe-round(delsow(delsowing[4])[6]*sbe,2)
          crd[c5,5] <- crd[c3,5]*0
        }
      } 
      if(crops[5]=="springbeans"){
        crd[c1,6] <- snsbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- sbe-round(delsow(delsowing[5])[6]*sbe,2)
          crd[c5,6] <- crd[c3,6]*0
        }
      }
      if(crops[6]=="springbeans"){
        crd[c1,7] <- snsbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- sbe-round(delsow(delsowing[6])[6]*sbe,2)
          crd[c5,7] <- crd[c3,7]*0
        }
      }
      
      #********** WPOTS *********
      if(crops[1]=="warepotatoes"){
        crd[c1,2] <- snwpo # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- wpo-round(delsow(delsowing[1])[7]*wpo,2)
          crd[c5,2] <- crd[c3,2]*0
        }
      }
      if(crops[2]=="warepotatoes"){
        crd[c1,3] <- snwpo # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- wpo-round(delsow(delsowing[2])[7]*wpo,2)
          crd[c5,3] <- crd[c3,3]*0
        }
      }
      if(crops[3]=="warepotatoes"){
        crd[c1,4] <- snwpo # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- wpo-round(delsow(delsowing[3])[7]*wpo,2)
          crd[c5,4] <- crd[c3,4]*0
        }
      }
      if(crops[4]=="warepotatoes"){
        crd[c1,5] <- snwpo # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- wpo-round(delsow(delsowing[4])[7]*wpo,2)
          crd[c5,5] <- crd[c3,5]*0
        }
      } 
      if(crops[5]=="warepotatoes"){
        crd[c1,6] <- snwpo # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- wpo-round(delsow(delsowing[5])[7]*wpo,2)
          crd[c5,6] <- crd[c3,6]*0
        }
      }
      if(crops[6]=="warepotatoes"){
        crd[c1,7] <- snwpo # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- wpo-round(delsow(delsowing[6])[7]*wpo,2)
          crd[c5,7] <- crd[c3,7]*0
        }
      }
      
      #********** WOSR *********
      if(crops[1]=="wosr"){
        crd[c1,2] <- snwos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- wos-round(delsow(delsowing[1])[8]*wos,2)
          crd[c5,2] <- crd[c3,2]*0
        }
      }
      if(crops[2]=="wosr"){
        crd[c1,3] <- snwos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- wos-round(delsow(delsowing[2])[8]*wos,2)
          crd[c5,3] <- crd[c3,3]*0
        }
      }
      if(crops[3]=="wosr"){
        crd[c1,4] <- snwos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- wos-round(delsow(delsowing[3])[8]*wos,2)
          crd[c5,4] <- crd[c3,4]*0
        }
      }
      if(crops[4]=="wosr"){
        crd[c1,5] <- snwos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- wos-round(delsow(delsowing[4])[8]*wos,2)
          crd[c5,5] <- crd[c3,5]*0
        }
      } 
      if(crops[5]=="wosr"){
        crd[c1,6] <- snwos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- wos-round(delsow(delsowing[5])[8]*wos,2)
          crd[c5,6] <- crd[c3,6]*0
        }
      }
      if(crops[6]=="wosr"){
        crd[c1,7] <- snwos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- wos-round(delsow(delsowing[6])[8]*wos,2)
          crd[c5,7] <- crd[c3,7]*0
        }
      }
      
      #********** SBEET *********
      if(crops[1]=="sugarbeet"){
        crd[h5,2] <- 5 # Cost of transporting sugarbeet @ £5/t
        crd[c1,2] <- snsbt # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- sbt-round(delsow(delsowing[1])[9]*sbt,2)
          crd[c5,2] <- crd[c3,2]*0
        }
      }
      if(crops[2]=="sugarbeet"){
        crd[h5,3] <- 5 # Cost of transporting sugarbeet @ £5/t
        crd[c1,3] <- snsbt # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- sbt-round(delsow(delsowing[2])[9]*sbt,2)
          crd[c5,3] <- crd[c3,3]*0
        }
      }
      if(crops[3]=="sugarbeet"){
        crd[h5,4] <- 5 # Cost of transporting sugarbeet @ £5/t
        crd[c1,4] <- snsbt # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- sbt-round(delsow(delsowing[3])[9]*sbt,2)
          crd[c5,4] <- crd[c3,4]*0
        }
      }
      if(crops[4]=="sugarbeet"){
        crd[h5,5] <- 5 # Cost of transporting sugarbeet @ £5/t
        crd[c1,5] <- snsbt # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- sbt-round(delsow(delsowing[4])[9]*sbt,2)
          crd[c5,5] <- crd[c3,5]*0
        }
      } 
      if(crops[5]=="sugarbeet"){
        crd[h5,6] <- 5 # Cost of transporting sugarbeet @ £5/t
        crd[c1,6] <- snsbt # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- sbt-round(delsow(delsowing[5])[9]*sbt,2)
          crd[c5,6] <- crd[c3,6]*0
        }
      }
      if(crops[6]=="sugarbeet"){
        crd[h5,7] <- 5 # Cost of transporting sugarbeet @ £5/t
        crd[c1,7] <- snsbt # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- sbt-round(delsow(delsowing[6])[9]*sbt,2)
          crd[c5,7] <- crd[c3,7]*0
        }
      }
      
      #********** SETA *********
      if(crops[1]=="setaside"){
        crd[c1,2] <- snset # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- 0
          crd[c5,2] <- 0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- 0
          crd[c5,2] <- 0
        }
      }
      if(crops[2]=="setaside"){
        crd[c1,3] <- snset # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- 0
          crd[c5,3] <- 0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- 0
          crd[c5,3] <- 0
        }
      }
      if(crops[3]=="setaside"){
        crd[c1,4] <- snset # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- 0
          crd[c5,4] <- 0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- 0
          crd[c5,4] <- 0
        }
      }
      if(crops[4]=="setaside"){
        crd[c1,5] <- snset # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- 0
          crd[c5,5] <- 0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- 0
          crd[c5,5] <- 0
        }
      } 
      if(crops[5]=="setaside"){
        crd[c1,6] <- snset # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- 0
          crd[c5,6] <- 0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- 0
          crd[c5,6] <- 0
        }
      }
      if(crops[6]=="setaside"){
        crd[c1,7] <- snset # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- 0
          crd[c5,7] <- 0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- 0
          crd[c5,7] <- 0
        }
      }
      
      #********** SOSR *********
      if(crops[1]=="sosr"){
        crd[c1,2] <- snsos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- sos-round(delsow(delsowing[1])[10]*sos,2)
          crd[c5,2] <- crd[c3,2]*0
        }
      }
      if(crops[2]=="sosr"){
        crd[c1,3] <- snsos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- sos-round(delsow(delsowing[2])[10]*sos,2)
          crd[c5,3] <- crd[c3,3]*0
        }
      }
      if(crops[3]=="sosr"){
        crd[c1,4] <- snsos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- sos-round(delsow(delsowing[3])[10]*sos,2)
          crd[c5,4] <- crd[c3,4]*0
        }
      }
      if(crops[4]=="sosr"){
        crd[c1,5] <- snsos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- sos-round(delsow(delsowing[4])[10]*sos,2)
          crd[c5,5] <- crd[c3,5]*0
        }
      } 
      if(crops[5]=="sosr"){
        crd[c1,6] <- snsos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- sos-round(delsow(delsowing[5])[10]*sos,2)
          crd[c5,6] <- crd[c3,6]*0
        }
      }
      if(crops[6]=="sosr"){
        crd[c1,7] <- snsos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- sos-round(delsow(delsowing[6])[10]*sos,2)
          crd[c5,7] <- crd[c3,7]*0
        }
      }
      
      #********** WLIN *********
      if(crops[1]=="winterlinseed"){
        crd[c1,2] <- snwln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- wln-round(delsow(delsowing[1])[11]*wln,2)
          crd[c5,2] <- crd[c3,2]*0
        }
      }
      if(crops[2]=="winterlinseed"){
        crd[c1,3] <- snwln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- wln-round(delsow(delsowing[2])[11]*wln,2)
          crd[c5,3] <- crd[c3,3]*0
        }
      }
      if(crops[3]=="winterlinseed"){
        crd[c1,4] <- snwln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- wln-round(delsow(delsowing[3])[11]*wln,2)
          crd[c5,4] <- crd[c3,4]*0
        }
      }
      if(crops[4]=="winterlinseed"){
        crd[c1,5] <- snwln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- wln-round(delsow(delsowing[4])[11]*wln,2)
          crd[c5,5] <- crd[c3,5]*0
        }
      } 
      if(crops[5]=="winterlinseed"){
        crd[c1,6] <- snwln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- wln-round(delsow(delsowing[5])[11]*wln,2)
          crd[c5,6] <- crd[c3,6]*0
        }
      }
      if(crops[6]=="winterlinseed"){
        crd[c1,7] <- snwln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- wln-round(delsow(delsowing[6])[11]*wln,2)
          crd[c5,7] <- crd[c3,7]*0
        }
      }
      
      #********** SLIN *********
      if(crops[1]=="springlinseed"){
        crd[c1,2] <- snsln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- sln-round(delsow(delsowing[1])[12]*sln,2)
          crd[c5,2] <- crd[c3,2]*0
        }
      }
      if(crops[2]=="springlinseed"){
        crd[c1,3] <- snsln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- sln-round(delsow(delsowing[2])[12]*sln,2)
          crd[c5,3] <- crd[c3,3]*0
        }
      }
      if(crops[3]=="springlinseed"){
        crd[c1,4] <- snsln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- sln-round(delsow(delsowing[3])[12]*sln,2)
          crd[c5,4] <- crd[c3,4]*0
        }
      }
      if(crops[4]=="springlinseed"){
        crd[c1,5] <- snsln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- sln-round(delsow(delsowing[4])[12]*sln,2)
          crd[c5,5] <- crd[c3,5]*0
        }
      } 
      if(crops[5]=="springlinseed"){
        crd[c1,6] <- snsln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- sln-round(delsow(delsowing[5])[12]*sln,2)
          crd[c5,6] <- crd[c3,6]*0
        }
      }
      if(crops[6]=="springlinseed"){
        crd[c1,7] <- snsln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- sln-round(delsow(delsowing[6])[12]*sln,2)
          crd[c5,7] <- crd[c3,7]*0
        }
      }
      
      #********** PEAS *********
      if(crops[1]=="driedpeas"){
        crd[c1,2] <- sndpe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- dpe-round(delsow(delsowing[1])[13]*dpe,2)
          crd[c5,2] <- crd[c3,2]*0
        }
      }
      if(crops[2]=="driedpeas"){
        crd[c1,3] <- sndpe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- dpe-round(delsow(delsowing[2])[13]*dpe,2)
          crd[c5,3] <- crd[c3,3]*0
        }
      }
      if(crops[3]=="driedpeas"){
        crd[c1,4] <- sndpe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- dpe-round(delsow(delsowing[3])[13]*dpe,2)
          crd[c5,4] <- crd[c3,4]*0
        }
      }
      if(crops[4]=="driedpeas"){
        crd[c1,5] <- sndpe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- dpe-round(delsow(delsowing[4])[13]*dpe,2)
          crd[c5,5] <- crd[c3,5]*0
        }
      } 
      if(crops[5]=="driedpeas"){
        crd[c1,6] <- sndpe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- dpe-round(delsow(delsowing[5])[13]*dpe,2)
          crd[c5,6] <- crd[c3,6]*0
        }
      }
      if(crops[6]=="driedpeas"){
        crd[c1,7] <- sndpe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- dpe-round(delsow(delsowing[6])[13]*dpe,2)
          crd[c5,7] <- crd[c3,7]*0
        }
      }
      
      #**************************************************************************************************************
      # ========= Subsidy amount =========
      crd[c7,seq(2,7)] <- 223 # SUBSIDY AMOUNT ASSUMED @ 223/ha (https://www.gov.uk/guidance/bps-2019)
      cropData <- crd
      
      Operation=c("Spread P/K Fertiliser WWHT","Plough WWHT","Sowing WWHT","Roll WWHT",
                  "Apply N Fertiliser WWHT","Spray WWHT","Combine Harvesting WWHT","Bale WWHT",
                  
                  "Spread P/K Fertiliser SWHT","Plough SWHT","Sowing SWHT","Roll SWHT",
                  "Apply N Fertiliser SWHT","Spray SWHT","Combine Harvesting SWHT","Bale SWHT",
                  
                  "Spread P/K Fertiliser WBAR","Plough WBAR","Sowing WBAR","Roll WBAR",
                  "Apply N Fertiliser WBAR","Spray WBAR","Combine Harvesting WBAR","Bale WBAR",
                  
                  "Spread P/K Fertiliser SBAR","Plough SBAR","Sowing SBAR","Roll SBAR",
                  "Spray SBAR","Apply N Fertiliser SBAR","Combine Harvesting SBAR","Bale SBAR",
                  
                  "Spread P/K Fertiliser WBEA","Plough WBEA","Sowing WBEA","Roll WBEA","Spray WBEA",
                  "Combine Harvesting WBEA",
                  
                  "Spread P/K Fertiliser SBEA","Plough SBEA","Sowing SBEA","Spray SBEA",
                  "Combine Harvesting SBEA",
                  
                  "Plough WPOT","Harrow WPOT","Sowing WPOT","Ridge WPOT","Spray WPOT","Spread P/K Fertiliser WPOT",
                  "Apply N Fertiliser WPOT","Hoeing WPOT","Harvest WPOT",
                  
                  "Spread P/K Fertiliser WOSR","Plough WOSR","Sowing WOSR",
                  "Spray WOSR","Apply N Fertiliser WOSR","Combine Harvesting WOSR",
                  
                  "Plough SBEE","Harrow SBEE","Sowing SBEE","Spray SBEE","Spread P/K Fertiliser SBEE",
                  "Apply N Fertiliser SBEE","Hoeing SBEE","Harvest SBEE",
                  
                  "Plough SETA","Spray SETA",
                  
                  "Spread P/K Fertiliser SOSR","Plough SOSR","Sowing SOSR",
                  "Spray SOSR","Apply N Fertiliser SOSR","Combine Harvesting SOSR",
                  
                  "Spread P/K Fertiliser WLIN","Plough WLIN","Sowing WLIN",
                  "Spray WLIN","Apply N Fertiliser WLIN","Combine Harvesting WLIN",
                  
                  "Spread P/K Fertiliser SLIN","Plough SLIN","Sowing SLIN",
                  "Spray SLIN","Apply N Fertiliser SLIN","Combine Harvesting SLIN",
                  
                  "Spread P/K Fertiliser DPEA","Plough DPEA","Sowing DPEA","Roll DPEA",
                  "Spray DPEA","Combine Harvesting DPEA")
      
      lgt <- length(Operation) # Total number of operation for all crops              
      Workrates <- data.frame(Operations=Operation,   
                              
                              Tractor=c(rep(0,lgt)), Labour=c(rep(0,lgt)), 
                              Power_harrow=c(rep(0,lgt)), Sprayer=c(rep(0,lgt)),
                              Combine_harvester=c(rep(0,lgt)), Baler=c(rep(0,lgt)), 
                              SBEE_harvester=c(rep(0,lgt)),WPOT_harvester=c(rep(0,lgt)),Fuel_cost=c(rep(0,lgt)), 
                              Labour_cost=c(rep(0,lgt)), Operation_cost=c(rep(0,lgt))
      )
      
      Machines <- data.frame(
        Machine=c("Tractor","Rolls","Power harrow","Sprayer","Combine harvetser",
                  "Baler","Sugarbeet harvester"),
        
        Size_Unit=c("kW","m","m","litres","kW","na","na"),
        
        Size=c(machsize,0,0),
        
        Machine_price=c(rep(0,7)),
        
        Depreciation_rate=c(22,14,14,18,18,11,18),
        
        Repair_cost_rate=c(12,5,5,6.8,5.8,5.8,5), Replacement_year=c(5,10,8,7,7,7,7),
        
        Annual_hours=c(2500,rep(300,6))
      )
      
      modData <- list(cropData, Workrates, Machines) # Data files
      
      #************************************************************************************************
      #************************************************************************************************
      
      
      fi1 <- modData[[1]] # Crop Data
      
      # Yield penalties with respect to continuous cropping (self rotation) and sub-optimal rotation
      sf <- selfrot/100 # Self rotation penalty
      rt <- rotpen/100 # Rotational penalty
      
      #***********
      cy1 <- fi1[c3,2]; cy2 <- fi1[c3,3]; cy3 <- fi1[c3,4]; cy4 <- fi1[c3,5]; cy5 <- fi1[c3,6]; cy6 <- fi1[c3,7]
      if(crops[1]=="winterwheat"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0.5
      }else if(crops[1]=="springwheat"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0.5
      }else if(crops[1]=="winterbarley"||crops[1]=="winteroats"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0.5
      }else if(crops[1]=="springbarley"||crops[1]=="springoats"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0.5
      }else if(crops[1]=="winterbeans"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0
      }else if(crops[1]=="springbeans"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0
      }else if(crops[1]=="warepotatoes"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0
      }else if(crops[1]=="wosr"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0
      }else if(crops[1]=="sugarbeet"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0
      }else if(crops[1]=="setaside"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0
      }else if(crops[1]=="sosr"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0
      }else if(crops[1]=="winterlinseed"||crops[1]=="springlinseed"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0
      }else if(crops[1]=="driedpeas"||crops[1]=="peas"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0
      }
      
      #***********
      if(crops[2]=="winterwheat"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0.5
      }else if(crops[2]=="springwheat"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0.5
      }else if(crops[2]=="winterbarley"||crops[2]=="winteroats"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0.5
      }else if(crops[2]=="springbarley"||crops[2]=="springoats"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0.5
      }else if(crops[2]=="winterbeans"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0
      }else if(crops[2]=="springbeans"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0
      }else if(crops[2]=="warepotatoes"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0
      }else if(crops[2]=="wosr"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0
      }else if(crops[2]=="sugarbeet"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0
      }else if(crops[2]=="setaside"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0
      }else if(crops[2]=="sosr"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0
      }else if(crops[2]=="winterlinseed"||crops[2]=="springlinseed"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0
      }else if(crops[2]=="driedpeas"||crops[2]=="peas"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0
      }
      
      #***********
      if(crops[3]=="winterwheat"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0.5
      }else if(crops[3]=="springwheat"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0.5
      }else if(crops[3]=="winterbarley"||crops[3]=="winteroats"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0.5
      }else if(crops[3]=="springbarley"||crops[3]=="springoats"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0.5
      }else if(crops[3]=="winterbeans"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0
      }else if(crops[3]=="springbeans"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0
      }else if(crops[3]=="warepotatoes"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0
      }else if(crops[3]=="wosr"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0
      }else if(crops[3]=="sugarbeet"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0
      }else if(crops[3]=="setaside"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0
      }else if(crops[3]=="sosr"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0
      }else if(crops[3]=="winterlinseed"||crops[3]=="springlinseed"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0
      }else if(crops[3]=="driedpeas"||crops[3]=="peas"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0
      }
      
      #***********
      if(crops[4]=="winterwheat"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0.5
      }else if(crops[4]=="springwheat"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0.5
      }else if(crops[4]=="winterbarley"||crops[4]=="winteroats"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0.5
      }else if(crops[4]=="springbarley"||crops[4]=="springoats"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0.5
      }else if(crops[4]=="winterbeans"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0
      }else if(crops[4]=="springbeans"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0
      }else if(crops[4]=="warepotatoes"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0
      }else if(crops[4]=="wosr"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0
      }else if(crops[4]=="sugarbeet"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0
      }else if(crops[4]=="setaside"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0
      }else if(crops[4]=="sosr"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0
      }else if(crops[4]=="winterlinseed"||crops[4]=="springlinseed"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0
      }else if(crops[4]=="driedpeas"||crops[4]=="peas"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0
      }
      
      #***********
      if(crops[5]=="winterwheat"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0.5
      }else if(crops[5]=="springwheat"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0.5
      }else if(crops[5]=="winterbarley"||crops[5]=="winteroats"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0.5
      }else if(crops[5]=="springbarley"||crops[5]=="springoats"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0.5
      }else if(crops[5]=="winterbeans"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0
      }else if(crops[5]=="springbeans"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0
      }else if(crops[5]=="warepotatoes"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0
      }else if(crops[5]=="wosr"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0
      }else if(crops[5]=="sugarbeet"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0
      }else if(crops[5]=="setaside"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0
      }else if(crops[5]=="sosr"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0
      }else if(crops[5]=="winterlinseed"||crops[5]=="springlinseed"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0
      }else if(crops[5]=="driedpeas"||crops[5]=="peas"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0
      }
      
      #***********
      if(crops[6]=="winterwheat"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0.5
      }else if(crops[6]=="springwheat"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0.5
      }else if(crops[6]=="winterbarley"||crops[6]=="winteroats"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0.5
      }else if(crops[6]=="springbarley"||crops[6]=="springoats"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0.5
      }else if(crops[6]=="winterbeans"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0
      }else if(crops[6]=="springbeans"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0
      }else if(crops[6]=="warepotatoes"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0
      }else if(crops[6]=="wosr"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0
      }else if(crops[6]=="sugarbeet"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0
      }else if(crops[6]=="setaside"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0
      }else if(crops[6]=="sosr"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0
      }else if(crops[6]=="winterlinseed"||crops[6]=="springlinseed"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0
      }else if(crops[6]=="driedpeas"||crops[6]=="peas"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0
      }
      
      
      op <- fi1
      
      #*************** Farm Costs/Revenue ****************************
      # Farm output estimates
      le <- 7
      if(subsidy=="no"){
        farmpayment <- 0
      }else{
        farmpayment <- op[c7,seq(2,le)]
      }
      
      # Farm output estimates
      # Farm output = (Crop yield * Crop price) + farmpayment
      outp <- ((op[c3,seq(2,le)]*op[c4,seq(2,le)])+(op[c5,seq(2,le)]*op[c6,seq(2,le)]))+farmpayment 
      op[c8,seq(2,le)] <- outp 
      
      kk1 <- op
      
      #*************** Farm Costs ****************************
      vc <- kk1 # Updated data with farmOutput(soil, subsidy, blackgrass)
      
      #h1 <- 10; h2 <- 11; h3 <- 12; h4 <- 13; h5 <- 14; h6 <- 15;
      #h7 <- 16; h8 <- 17; 
      
      # Fertiliser (NPK) cost = (Fertiliser rate (kg/ha)* Fertiliser price(£/kg))
      vc[h6,seq(2,le)] <- fertcost <- (vc[1,seq(2,le)]*vc[4,seq(2,le)])+
        (vc[2,seq(2,le)]*vc[5,seq(2,le)])+(vc[3,seq(2,le)]*vc[6,seq(2,le)])
      
      # Seed cost (Seed rate (kg/ha) * Seed price (£/kg))
      vc[h7,seq(2,le)] <- seedcost <- vc[7,seq(2,le)]*vc[8,seq(2,le)]
      
      # Herbicide cost is assumed to be cost of herbicides targeting black-grass 
      # (cost of other herbicides and other chemical costs are incorporated in Sundry cost)
      # Herbicide cost = Total Herbicide rate (kg or l/ha) * Herbicide price (£/l)
      vc[h8,seq(2,le)] <- chemcost <- (vc[9,seq(2,le)]*vc[h1,seq(2,le)])+(vc[h2,seq(2,le)]*vc[h3,seq(2,le)])
      
      # Sundry Cost (£/ha) Informed by data from Nix (2019) Farm Management Pocketbook
      sundry <- vc[c1,seq(2,le)] 
      
      # Sugar beet transport cost from Nix (2019) based on £5.11/t
      vc[c2,seq(2,le)] <- sbeettransport <- vc[h5,seq(2,le)]*vc[c3,seq(2,le)] 
      
      # Variable cost (£/ha)
      vc[c9,seq(2,le)] <- (fertcost+seedcost+chemcost+sundry+sbeettransport)
      
      # Gross margin (£/ha)
      # Gross margin = Output - Variable cost
      vc[c10,seq(2,le)] <- vc[c8,seq(2,le)]-vc[c9,seq(2,le)]
      kk2 <- vc
      
      # operatingCost <- function(nsoil, subsidy, blackgrass){
      
      # Estimates workrate of farm operations and fuel and labour cost
      # wro <- variableCost(nsoil, subsidy, blackgrass)
      
      #========= Machine Sizes ===========
      cr <- kk2 #variableCost(nsoil, subsidy, blackgrass) #wro #Files()[[2]]
      ma <- modData[[3]][,seq(-1,-2)] # Machine data
      wr <- modData[[2]] # Workrate data 
      tractor <- ma[1,1] # Tractor Size or power (kW) 
      sprayer <- ma[4,1] # Sprayer Size (size of tank in litres) 1400 litres
      tsize <- ma[4,1] # Tank size for estimate fertiliser application workrate
      combsize <- ma[5,1] # Size of combine harvester
      # The size of the combine harvester measured in tonnes/hour 
      # was derived on pro rata basis based on information from
      # Agricultural Notebook. A combine harvester with a power of 90kW can harvest 10t/h 
      # Thus a combine harvester of 125kW can harvest (10/90)*125
      extfactor <- round(10/90,2)
      combine <- round(combsize*extfactor) #14 # Represents combine harvester size
      rollwidth <- ma[2,1] # Roll width
      sphoe <- 19 # Assumed speed for hoeing (19km/h)
      rowsp <- 0.6 # Assumed row space (0.6m)
      tpspeed <- 4 # Speed for rolling (km/hr)
      
      # ========= Tillage Types ==========
      # Tillage Assumptions
      # To be able to take into consideration soil type in tillage work rate,
      # the ploughing work rate is taken as a reference value due to the availability of suitable formulae.
      # The workability of ploughing is taken as 1.
      
      if(tillage=="ploughing"){
        wkb <- 1 # ploughing workability used as a reference value
      }else if(tillage=="noninversion"){
        wkb <- 1*0.5 # 50% reduction in plough rate is assumed - i.e. twice as fast as ploughing
      }else if(tillage=="subsoiling"){
        wkb <- 1/0.75 # 75% slower than ploughing (previously this was 1*0.65, as a 35% reduction in plough work rate was assumed. After talking to contractors, this was amended: they said subsoiling is slower than ploughing, and you'd do 75% of what you'd get done if ploughing in the same time).
      }else if(tillage=="lightcultivation"){
        wkb <- 1*0.6 # 40% reduction in plough work rate is assumed
      }else if(tillage=="directdrilling"){
        wkb <- 0 # no cultivation
      }else if (tillage=="minimumtillage"){
        wkb <- 1*0.3 # 70% reduction in plough work rate is assumed
      }else if(tillage=="deepcultivation"){
        wkb <- (1*0.6)*1.4 # Assumed to be 40% more than light cultivation
      }else if(tillage[1]=="inversion"){
        wkb <- 1 
      }else{
        wkb <- 1 
      }
      
      nspr <- numberofsprays # Number of Sprays ***************************************************************************
      
      if("winterwheat"%in%crops==TRUE){
        # Work rates for winter wheat operations =======
        wr[1,2] <- wr[1,3] <- round(((0.06+0.00025*({cr$winterwheat[2]}+{cr$winterwheat[3]}))+
                                       (64.48+0.094*({cr$winterwheat[2]}+{cr$winterwheat[3]}))/ tsize),2) # Spread P/K fert
        wr[2,2] <- wr[2,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        wr[3,2] <- wr[3,3] <- round((0.06+0.00069*{cr$winterwheat[7]})+
                                      (58.82+41.5*{nsoil}+0.00626*{cr$winterwheat[7]})/tractor,2) # Sow
        wr[4,2] <- wr[4,3] <- round((1.5/(tpspeed*rollwidth/10))*0.85,2) # Roll
        wr[5,2] <- wr[5,3] <- 
          round(((0.06+0.00025*({cr$winterwheat[1]}))+(64.68+0.094*({cr$winterwheat[1]}))/ tsize),2) # N fert
        
        if(crops[1]=="winterwheat"){
          wr[6,2] <- wr[6,3] <- wr[6,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="winterwheat"){
          wr[6,2] <- wr[6,3] <- wr[6,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="winterwheat"){
          wr[6,2] <- wr[6,3] <- wr[6,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="winterwheat"){
          wr[6,2] <- wr[6,3] <- wr[6,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="winterwheat"){
          wr[6,2] <- wr[6,3] <- wr[6,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="winterwheat"){
          wr[6,2] <- wr[6,3] <- wr[6,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[6,2] <- wr[6,3] <- wr[6,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        
        cbwwht <- round((1.00*({cr$winterwheat[c3]}+20)/4)/combine,2) # Combine harvester
        wr[7,2] <- 2* cbwwht; wr[7,3] <- 3* cbwwht; wr[7,6] <- cbwwht
        bawwht <- round((({cr$winterwheat[c5]}+13)/4)/combine*0.5,2) # Bale
        wr[8,2] <- 2* bawwht; wr[8,3] <- 2* bawwht; wr[8,7] <- bawwht
      }else{
        wr[seq(1,8),seq(2,12)] <- 0
      }
      
      if("springwheat"%in%crops==TRUE){
        # Work rates for spring wheat operations =======
        wr[9,2] <- wr[9,3] <- round(((0.06+0.00025*({cr$springwheat[3]}+{cr$springwheat[5]}))+
                                       (64.48+0.094*({cr$springwheat[3]}+{cr$springwheat[5]}))/ tsize),2) # Spread P/K fert
        wr[10,2] <- wr[10,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        wr[11,2] <- wr[11,3] <- round(((0.06+0.00069*{cr$springwheat[7]})+
                                         (58.82+41.5*{nsoil}+0.00626*{cr$springwheat[7]})/tractor),2) #Sow
        wr[12,2] <- wr[12,3] <- round((1.5/(tpspeed*rollwidth/10))*0.85,2) # Roll
        wr[13,2] <- wr[13,3] <- round(((0.06+0.00025*({cr$springwheat[1]}))+(64.68+0.094*({cr$springwheat[1]}))/ tsize),2) # N fert
        
        if(crops[1]=="springwheat"){
          wr[14,2] <- wr[14,3] <- wr[14,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="springwheat"){
          wr[14,2] <- wr[14,3] <- wr[14,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="springwheat"){
          wr[14,2] <- wr[14,3] <- wr[14,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="springwheat"){
          wr[14,2] <- wr[14,3] <- wr[14,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="springwheat"){
          wr[14,2] <- wr[14,3] <- wr[14,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="springwheat"){
          wr[14,2] <- wr[14,3] <- wr[14,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[14,2] <- wr[14,3] <- wr[14,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        cbswht <- round(2*(1.00*({cr$springwheat[c3]}+20)/4)/combine,2) # Combine
        wr[15,2] <- 2* cbswht; wr[15,3] <- 3* cbswht; wr[15,6] <- cbswht
        baswht <- round((({cr$springwheat[c5]}+13)/4)/combine*0.5,2) # Bale
        wr[16,2] <- 2* baswht;  wr[16,3] <- 2* baswht; wr[16,7] <- baswht
        # 
      }else{
        wr[seq(9,16),seq(2,12)] <-0
      }
      
      if("winterbarley"%in%crops==TRUE||"winteroats"%in%crops==TRUE){
        # Work rates for winter barley operations =====
        wr[17,2] <- wr[17,3] <- round(((0.06+0.00025*({cr$winterbarley[3]}+{cr$winterbarley[5]}))+
                                         (64.48+0.094*({cr$winterbarley[3]}+{cr$winterbarley[5]}))/ tsize),2) # Spread P/K fert
        wr[18,2] <- wr[18,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        wr[19,2] <- wr[19,3] <- round(((0.06+0.00069*{cr$winterbarley[7]})+
                                         (58.82+41.5*{nsoil}+0.00626*{cr$winterbarley[7]})/tractor),2) # Sow
        wr[20,2] <- wr[20,3] <- round((1.5/(tpspeed*rollwidth/10))*0.85,2) # Roll
        wr[21,2] <- wr[21,3] <- 
          round(((0.06+0.00025*({cr$winterbarley[1]}))+(64.68+0.094*({cr$winterbarley[1]}))/ tsize),2) # N fert
        
        if(crops[1]=="winterbarley"||crops[1]=="winteroats"){
          wr[22,2] <- wr[22,3] <- wr[22,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="winterbarley"||crops[2]=="winteroats"){
          wr[22,2] <- wr[22,3] <- wr[22,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="winterbarley"||crops[3]=="winteroats"){
          wr[22,2] <- wr[22,3] <- wr[22,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="winterbarley"||crops[4]=="winteroats"){
          wr[22,2] <- wr[22,3] <- wr[22,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="winterbarley"||crops[5]=="winteroats"){
          wr[22,2] <- wr[22,3] <- wr[22,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="winterbarley"||crops[6]=="winteroats"){
          wr[22,2] <- wr[22,3] <- wr[22,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[22,2] <- wr[22,3] <- wr[22,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        cbwbar <- round((1.15*({cr$winterbarley[c3]}+24)/4)/combine,2) # Combine
        wr[23,2] <- 2* cbwbar; wr[23,3] <- 3* cbwbar; wr[23,6] <- cbwbar
        bawbar <- round((({cr$winterbarley[c5]}+13)/4)/combine*0.5,2) # Bale
        wr[24,2] <- 3* bawbar; wr[24,3] <- 3* bawbar; wr[24,7] <- bawbar
      }else{
        wr[seq(17,24),seq(2,12)] <- 0
      }
      
      if("springbarley"%in%crops==TRUE||"springoats"%in%crops==TRUE){
        # Work rates for spring barley operations =====
        wr[25,2] <- wr[25,3] <- round(((0.06+0.00025*({cr$springbarley[2]}+{cr$springbarley[3]}))+
                                         (64.48+0.094*({cr$springbarley[2]}+{cr$springbarley[3]}))/ tsize),2) # Spread P/K fert
        wr[26,2] <- wr[26,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        wr[27,2] <- wr[27,3] <- round(((0.06+0.00069*{cr$springbarley[7]})+
                                         (58.82+41.5*{nsoil}+0.00626*{cr$springbarley[7]})/tractor),2) # Sow
        wr[28,2] <- wr[28,3] <- round((1.5/(tpspeed*rollwidth/10))*0.85,2) # Roll
        
        #wr[29,2] <- wr[29,3] <- wr[29,5] <- round(0.11+204.2/sprayer,2) # Spraying
        if(crops[1]=="springbarley"||crops[1]=="springoats"){
          wr[29,2] <- wr[29,3] <- wr[29,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="springbarley"||crops[2]=="springoats"){
          wr[29,2] <- wr[29,3] <- wr[29,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="springbarley"||crops[3]=="springoats"){
          wr[29,2] <- wr[29,3] <- wr[29,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="springbarley"||crops[4]=="springoats"){
          wr[29,2] <- wr[29,3] <- wr[29,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="springbarley"||crops[5]=="springoats"){
          wr[29,2] <- wr[29,3] <- wr[29,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="springbarley"||crops[6]=="springoats"){
          wr[29,2] <- wr[29,3] <- wr[29,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[29,2] <- wr[29,3] <- wr[29,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        
        wr[30,2] <- wr[30,3] <- 
          round(((0.06+0.00025*({cr$springbarley[1]}))+(64.68+0.094*({cr$springbarley[1]}))/ tsize),2) # N fert
        cbsbar <- round(((1.15*{cr$springbarley[c3]}+24)/4)/combine,2) # Combine
        wr[31,2] <- 2* cbsbar; wr[31,3] <- 3* cbsbar; wr[31,6] <- cbsbar
        basbar <- round((({cr$springbarley[c5]}+13)/4)/combine*0.5,2) # Bale
        wr[32,2] <- 3* basbar; wr[32,3] <- 3* basbar; wr[32,7] <- basbar
      }else{
        wr[seq(25,32),seq(2,12)] <- 0
      }
      
      if("winterbeans"%in%crops==TRUE){
        # Work rates for winter beans operations =====
        wr[33,2] <- wr[33,3] <- 
          round(((0.06+0.00025*({cr$winterbeans[3]}+{cr$winterbeans[5]}))+
                   (64.48+0.094*({cr$winterbeans[3]}+{cr$winterbeans[5]}))/ tsize),2) # Spread P/K fert
        wr[34,2] <- wr[34,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        wr[35,2] <- wr[35,3] <- round((3*(0.114+0.00033*{cr$winterbeans[7]})+
                                         (54*{nsoil}+21.6)/tractor),2) # Sow
        wr[36,2] <- wr[36,3] <- round((1.5/(tpspeed*rollwidth/10))*0.85,2) # Roll
        
        #wr[37,2] <- wr[37,3] <- wr[37,5] <- round(0.11+204.2/sprayer,2) #Spray
        if(crops[1]=="winterbeans"){
          wr[37,2] <- wr[37,3] <- wr[37,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="winterbeans"){
          wr[37,2] <- wr[37,3] <- wr[37,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="winterbeans"){
          wr[37,2] <- wr[37,3] <- wr[37,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="winterbeans"){
          wr[37,2] <- wr[37,3] <- wr[37,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="winterbeans"){
          wr[37,2] <- wr[37,3] <- wr[37,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="winterbeans"){
          wr[37,2] <- wr[37,3] <- wr[37,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[37,2] <- wr[37,3] <- wr[37,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        cbwbea <- round((4.05*({cr$winterbeans[c3]}+24)/4)/combine,2) #Combine
        wr[38,2] <- 2* cbwbea; wr[38,3] <- 3* cbwbea; wr[38,6] <- cbwbea
      }else{
        wr[seq(33,38),seq(2,12)] <- 0
      }
      
      if("springbeans"%in%crops==TRUE){
        # Work rates for spring beans operations =====
        wr[39,2] <- wr[39,3] <- 
          round(((0.06+0.00025*({cr$springbeans[2]}+{cr$springbeans[3]}))+
                   (64.48+0.094*({cr$springbeans[2]}+{cr$springbeans[3]}))/ tsize),2) # Spread P/K fert
        wr[40,2] <- wr[40,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        wr[41,2] <- wr[41,3] <- round(((0.06+0.00069*{cr$springbeans[7]})+
                                         (92.42+0.00626*{cr$springbeans[7]}+41.5*{nsoil})/tractor),2) # Sow
        
        if(crops[1]=="springbeans"){
          wr[42,2] <- wr[42,3] <- wr[42,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="springbeans"){
          wr[42,2] <- wr[42,3] <- wr[42,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="springbeans"){
          wr[42,2] <- wr[42,3] <- wr[42,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="springbeans"){
          wr[42,2] <- wr[42,3] <- wr[42,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="springbeans"){
          wr[42,2] <- wr[42,3] <- wr[42,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="springbeans"){
          wr[42,2] <- wr[42,3] <- wr[42,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[42,2] <- wr[42,3] <- wr[42,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        cbsbea <- round((4.05*({cr$springbeans[c3]}+24)/4)/combine,2) # Harvest
        wr[43,2] <- 2* cbsbea; wr[43,3] <- 3* cbsbea; wr[43,6] <- cbsbea
      }else{
        wr[seq(39,43),seq(2,12)] <-0
      }
      
      if("warepotatoes"%in%crops==TRUE){
        # Work rates for ware potatoes operations =====
        wr[44,2] <- wr[44,3] <- 
          round(((1.80*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage 
        wr[45,2] <- wr[45,3] <- 
          wr[45,4] <- round(((25*{nsoil}+33)/tractor),2) # Harrowing
        wr[46,2] <- wr[46,3] <- 
          round(((278/tractor+0.04+0.55*{cr$warepotatoes[7]})/2000),2)*3 # Sow potatoes
        wr[47,2] <- wr[47,3] <- round(((40*{nsoil}+33)/tractor),2) # Ridging
        
        if(crops[1]=="warepotatoes"){
          wr[48,2] <- wr[48,3] <- wr[48,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="warepotatoes"){
          wr[48,2] <- wr[48,3] <- wr[48,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="warepotatoes"){
          wr[48,2] <- wr[48,3] <- wr[48,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="warepotatoes"){
          wr[48,2] <- wr[48,3] <- wr[48,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="warepotatoes"){
          wr[48,2] <- wr[48,3] <- wr[48,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="warepotatoes"){
          wr[48,2] <- wr[48,3] <- wr[48,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[48,2] <- wr[48,3] <- wr[48,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        wr[49,2] <- wr[49,3] <- round(((0.06+0.00025*({cr$warepotatoes[2]}+{cr$warepotatoes[3]}))+
                                         (64.48+0.094*({cr$warepotatoes[2]}+{cr$warepotatoes[3]}))/tsize),2) # Spread P/K fert
        wr[50,2] <- wr[50,3] <- 
          round(((0.06+0.00025*({cr$warepotatoes[1]}))+(64.68+0.094*({cr$warepotatoes[1]}))/ tsize),2) # N fert
        wr[51,2] <- wr[51,3] <-  round(1/(sphoe*rowsp/10*0.8),2) # Hoeing
        hpot <- round(((403/600)+2/(3*(1.25+0.51*{nsoil})*({39.43}/37.728)))*2.51,2) # Harvest pot
        wr[52,2] <- hpot*4; wr[52,3] <- hpot*4; wr[52,9] <- hpot 
      }else{
        wr[seq(44,52),seq(2,12)] <-0
      }
      
      if("wosr"%in%crops==TRUE){
        # Work rates for wosr operations =====
        wr[53,2] <- wr[53,3] <- round(((0.06+0.00025*({cr$wosr[2]}+{cr$wosr[3]}))+
                                         (64.48+0.094*({cr$wosr[2]}+{cr$wosr[3]}))/ tsize),2) # Spread P/K fert
        wr[54,2] <- wr[54,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        wr[55,2] <- wr[55,3] <- round(((0.387+0.00069*cr$wosr[7])+(99.42+0.00626*cr$wosr[7])/tractor),2)# Sow
        
        if(crops[1]=="wosr"){
          wr[56,2] <- wr[56,3] <- wr[25,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="wosr"){
          wr[56,2] <- wr[56,3] <- wr[25,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="wosr"){
          wr[56,2] <- wr[56,3] <- wr[25,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="wosr"){
          wr[56,2] <- wr[56,3] <- wr[25,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="wosr"){
          wr[56,2] <- wr[56,3] <- wr[25,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="wosr"){
          wr[56,2] <- wr[56,3] <- wr[25,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[56,2] <- wr[56,3] <- wr[25,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        wr[57,2] <- wr[57,3] <- 
          round(((0.06+0.00025*({cr$wosr[1]}))+(64.68+0.094*({cr$wosr[1]}))/ tsize),2) # N fert
        cbwosr <- round(((4.05*({cr$wosr[c3]}+24)/4)/combine),2) # Combine
        wr[58,2] <- 2* cbwosr; wr[58,3] <- 3* cbwosr; wr[58,6] <- cbwosr
      }else{
        wr[seq(53,58),seq(2,12)] <-0
      }
      
      if("sugarbeet"%in%crops==TRUE){
        # Work rates for sugarbeet operations =====
        wr[59,2] <- wr[59,3] <- round((1.80*(50*{nsoil}+20))/tractor,2) # Ploughing 
        wr[60,2] <- wr[60,3]  <- wr[29,4] <- round(((25*{nsoil}+33)/tractor)*wkb,2) # Harrowing
        wr[61,2] <- wr[61,3] <- round((0.39+157/tractor), 2) # Planting
        
        if(crops[1]=="sugarbeet"){
          wr[62,2] <- wr[62,3] <- wr[62,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="sugarbeet"){
          wr[62,2] <- wr[62,3] <- wr[62,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="sugarbeet"){
          wr[62,2] <- wr[62,3] <- wr[62,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="sugarbeet"){
          wr[62,2] <- wr[62,3] <- wr[62,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="sugarbeet"){
          wr[62,2] <- wr[62,3] <- wr[62,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="sugarbeet"){
          wr[62,2] <- wr[62,3] <- wr[62,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[62,2] <- wr[62,3] <- wr[62,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        wr[63,2] <- wr[63,3] <- round(((0.06+0.00025*({cr$sugarbeet[2]}+{cr$sugarbeet[3]}))+
                                         (64.48+0.094*({cr$sugarbeet[2]}+{cr$sugarbeet[3]}))/ tsize),2) # Spread P/K fert
        wr[64,2] <- wr[64,3] <- 
          round(((0.06+0.00025*({cr$sugarbeet[1]}))+(64.68+0.094*({cr$sugarbeet[1]}))/ tsize),2) # N fert
        wr[65,2] <- wr[65,3] <-  round(1/(sphoe*rowsp/10*0.8),2) # Hoeing
        hvsbee <- round(((403/600)+2/(3*(1.25+0.51*{nsoil})*({cr$sugarbeet[c3]}/37.728))),2) # Harvest
        wr[66,2] <- 3* hvsbee; wr[66,3] <- 3* hvsbee; wr[66,8] <- hvsbee
      }else{
        wr[seq(59,66),seq(2,12)] <-0
      }
      
      if("setaside"%in%crops==TRUE){#||"fallow"%in%crops==TRUE
        # Work rates for setaside operations =====
        wr[67,2] <- wr[67,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        #wr[68,2] <- wr[68,3] <- wr[68,5] <- round(0.11+204.2/sprayer,2)
        if(crops[1]=="setaside"){
          wr[68,2] <- wr[68,3] <- wr[68,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="setaside"){
          wr[68,2] <- wr[68,3] <- wr[68,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="setaside"){
          wr[68,2] <- wr[68,3] <- wr[68,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="setaside"){
          wr[68,2] <- wr[68,3] <- wr[68,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="setaside"){
          wr[68,2] <- wr[68,3] <- wr[68,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="setaside"){
          wr[68,2] <- wr[68,3] <- wr[68,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[68,2] <- wr[68,3] <- wr[68,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        
      }else{
        wr[seq(67,68),seq(2,12)] <-0
      }
      
      if("sosr"%in%crops==TRUE){
        # Work rates for sosr operations =====
        wr[69,2] <- wr[69,3] <- round(((0.06+0.00025*({cr$sosr[2]}+{cr$sosr[3]}))+
                                         (64.48+0.094*({cr$sosr[2]}+{cr$sosr[3]}))/ tsize),2) # Spread P/K fert
        wr[70,2] <- wr[70,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        wr[71,2] <- wr[71,3] <- round(((0.06+0.00069*cr$sosr[7])+(92.42+0.00626*(cr$sosr[7])+41.5*(nsoil))/tractor),2) # Sow
        
        if(crops[1]=="sosr"){
          wr[72,2] <- wr[72,3] <- wr[72,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="sosr"){
          wr[72,2] <- wr[72,3] <- wr[72,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="sosr"){
          wr[72,2] <- wr[72,3] <- wr[72,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="sosr"){
          wr[72,2] <- wr[72,3] <- wr[72,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="sosr"){
          wr[72,2] <- wr[72,3] <- wr[72,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="sosr"){
          wr[72,2] <- wr[72,3] <- wr[72,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[72,2] <- wr[72,3] <- wr[72,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        wr[73,2] <- wr[73,3] <- 
          round(((0.06+0.00025*({cr$sosr[1]}))+(64.68+0.094*({cr$sosr[1]}))/ tsize),2) # N fert
        cbsosr <- round(((4.05*({cr$sosr[c3]}+24)/4)/combine),2) # Harvest
        wr[74,2] <- 2* cbsosr; wr[74,3] <- 3* cbsosr; wr[74,6] <- cbsosr
      }else{
        wr[seq(69,74),seq(2,12)] <-0
      }
      
      if("winterlinseed"%in%crops==TRUE){
        # Work rates for winterlinseed operations =====
        wr[75,2] <- wr[75,3] <- round(((0.06+0.00025*({cr$winterlinseed[2]}+{cr$winterlinseed[3]}))+
                                         (64.48+0.094*({cr$winterlinseed[2]}+{cr$winterlinseed[3]}))/ tsize),2) # Spread P/K fert
        wr[76,2] <- wr[76,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        
        wr[77,2] <- wr[77,3] <- round(((0.06+0.00069*cr$winterlinseed[7])+
                                         (92.42+0.00626*(cr$winterlinseed[7])+41.5*(nsoil))/tractor),2) # Planting
        
        if(crops[1]=="winterlinseed"){
          wr[78,2] <- wr[78,3] <- wr[78,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="winterlinseed"){
          wr[78,2] <- wr[78,3] <- wr[78,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="winterlinseed"){
          wr[78,2] <- wr[78,3] <- wr[78,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="winterlinseed"){
          wr[78,2] <- wr[78,3] <- wr[78,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="winterlinseed"){
          wr[78,2] <- wr[78,3] <- wr[78,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="winterlinseed"){
          wr[78,2] <- wr[78,3] <- wr[78,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[78,2] <- wr[78,3] <- wr[78,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        wr[79,2] <- wr[79,3] <- 
          round(((0.06+0.00025*({cr$winterlinseed[1]}))+(64.68+0.094*({cr$winterlinseed[1]}))/ tsize),2) # N fert
        cbwlin <- round(((4.05*({cr$winterlinseed[c3]}+24)/4)/combine),2) # Harvest
        wr[80,2] <- 2* cbwlin; wr[80,3] <- 3* cbwlin; wr[80,6] <- cbwlin
      }else{
        wr[seq(75,80),seq(2,12)] <-0
      }
      
      if("springlinseed"%in%crops==TRUE){
        # Work rates for springlinseed operations =====
        wr[81,2] <- wr[81,3] <- round(((0.06+0.00025*({cr$springlinseed[2]}+{cr$springlinseed[3]}))+
                                         (64.48+0.094*({cr$springlinseed[2]}+{cr$springlinseed[3]}))/ tsize),2) # Spread P/K fert
        wr[82,2] <- wr[82,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        
        wr[83,2] <- wr[83,3] <- round(((0.06+0.00069*cr$springlinseed[7])+
                                         (92.42+0.00626*(cr$springlinseed[7])+41.5*(nsoil))/tractor),2) # Planting
        
        if(crops[1]=="springlinseed"){
          wr[84,2] <- wr[84,3] <- wr[84,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="springlinseed"){
          wr[84,2] <- wr[84,3] <- wr[84,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="springlinseed"){
          wr[84,2] <- wr[84,3] <- wr[84,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="springlinseed"){
          wr[84,2] <- wr[84,3] <- wr[84,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="springlinseed"){
          wr[84,2] <- wr[84,3] <- wr[84,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="springlinseed"){
          wr[84,2] <- wr[84,3] <- wr[84,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[84,2] <- wr[84,3] <- wr[84,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        wr[85,2] <- wr[85,3] <- 
          round(((0.06+0.00025*({cr$springlinseed[1]}))+(64.68+0.094*({cr$springlinseed[1]}))/ tsize),2) # N fert
        cbslin <- round(((4.05*({cr$springlinseed[c3]}+24)/4)/combine),2) # Harvest
        wr[86,2] <- 2* cbslin; wr[86,3] <- 3* cbslin; wr[86,6] <- cbslin
      }else{
        wr[seq(81,86),seq(2,12)] <-0
      }
      
      if("driedpeas"%in%crops==TRUE||"peas"%in%crops==TRUE){
        # Work rates for peas operations =====
        wr[87,2] <- wr[87,3] <- 
          round(((0.06+0.00025*({cr$driedpeas[3]}+{cr$driedpeas[5]}))+
                   (64.48+0.094*({cr$driedpeas[3]}+{cr$driedpeas[5]}))/ tsize),2) # P/K Fert
        wr[88,2] <- wr[88,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        
        wr[89,2] <- wr[89,3] <- round((0.06+0.00069*{cr$driedpeas[7]})+
                                        (92.42+0.00626*{cr$driedpeas[7]}+41.5*{nsoil})/tractor,2) # Planting
        wr[90,2] <- wr[90,3] <- round((1.5/(tpspeed*rollwidth/10))*0.85,2) # Roll
        
        if(crops[1]=="driedpeas"||crops[1]=="peas"){
          wr[91,2] <- wr[91,3] <- wr[91,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="driedpeas"||crops[2]=="peas"){
          wr[91,2] <- wr[91,3] <- wr[91,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="driedpeas"||crops[3]=="peas"){
          wr[91,2] <- wr[91,3] <- wr[91,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="driedpeas"||crops[4]=="peas"){
          wr[91,2] <- wr[91,3] <- wr[91,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="driedpeas"||crops[5]=="peas"){
          wr[91,2] <- wr[91,3] <- wr[91,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="driedpeas"||crops[6]=="peas"){
          wr[91,2] <- wr[91,3] <- wr[91,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[91,2] <- wr[91,3] <- wr[91,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        cbdpea <- round((4.05*({cr$driedpeas[c3]}+24)/4)/combine,2) # Harvest
        wr[92,2] <- 2* cbdpea; wr[92,3] <- 3* cbdpea; wr[92,6] <- cbdpea
      }else{
        wr[seq(87,92),seq(2,12)] <- 0
      }
      
      kk3 <- wr
      
      
      # ******* Estimates fuel and labour costs ***************
      
      fuelPrice <- fuelprice #farm[2]
      labourWage <- labourwage #farm[3]
      TP <- round(tractor*1.341,0) # maximum PTO horsepower 
      CP <- round(combsize*1.341,0)
      
      # For diesel tractor, fuel consumption is estimated from the formula below
      # Obtained from: http://www.extension.iastate.edu/agdm/crops/html/a3-29.html (16/06/2015)
      # 0.044 * maximum PTO horsepower for diesel engines
      
      fuel.cons_gal_hr <- round(0.044 * TP,2) # Gallons per hour Tractor
      fuel.cons_gal_hr_comb <- round(0.044 * CP,2) # Combine
      # Convert gallons per hour to litres per hour
      # 1 gallon per hour = 4.546 litres per hour
      
      Fuel_Cons <- round(fuel.cons_gal_hr * 4.546,0) # Fuel consumption (litres/hour)
      Fuel_Cons_comb <- round(fuel.cons_gal_hr_comb * 4.546,0)
      
      # Fuel and labour cost (£/hour)
      fuelCost <-  (fuelPrice * Fuel_Cons)*1.1 # 10% represents oil and lubricants
      fuelCost_comb <- (fuelPrice * Fuel_Cons_comb)*1.1
      
      #Fuel and labour costs @ £/ha (Multiply £/hour by workrates (hr/ha))
      kk3[,10] <- round((kk3[,2]*fuelCost)+(kk3[,6]*fuelCost_comb)+
                          (kk3[,7]*fuelCost_comb)) # Fuel costs
      kk3[,11] <- round(kk3[,3]*labourWage) # Labour costs
      kk3[,12] <- round(kk3[,10]+kk3[,11])
      ops <- kk3
      
      # Fuel cost
      fu <- ops[,10]
      
      fuelcost <- c(sum(fu[seq(1,8)]),sum(fu[seq(9,16)]),sum(fu[seq(17,24)]),sum(fu[seq(25,32)]),
                    sum(fu[seq(33,38)]),sum(fu[seq(39,43)]),sum(fu[seq(44,52)]),sum(fu[seq(53,58)]),sum(fu[seq(59,66)]),
                    sum(fu[seq(67,68)]),sum(fu[seq(69,74)]),sum(fu[seq(75,80)]),sum(fu[seq(81,86)]),sum(fu[seq(87,92)]))
      # Labour cost
      la <- ops[,11]
      
      labourcost <- c(sum(la[seq(1,8)]),sum(la[seq(9,16)]),sum(la[seq(17,24)]),sum(la[seq(25,32)]),
                      sum(la[seq(33,38)]),sum(la[seq(39,43)]),sum(la[seq(44,52)]),sum(la[seq(53,58)]),sum(la[seq(59,66)]),
                      sum(la[seq(67,68)]),sum(la[seq(69,74)]),sum(la[seq(75,80)]),sum(la[seq(81,86)]),sum(la[seq(87,92)]))
      # Operating Cost
      op1 <- ops[,12]
      
      opcost <- c(sum(op1[seq(1,8)]),sum(op1[seq(9,16)]),sum(op1[seq(17,24)]),sum(op1[seq(25,32)]),
                  sum(op1[seq(33,38)]),sum(op1[seq(39,43)]),sum(op1[seq(44,52)]),sum(op1[seq(53,58)]),sum(op1[seq(59,66)]),
                  sum(op1[seq(67,68)]),sum(op1[seq(69,74)]),sum(op1[seq(75,80)]),sum(op1[seq(81,86)]),sum(op1[seq(87,92)]))
      
      crops <- crops
      
      if(crops[1]=="winterwheat"){
        fc1 <- fuelcost[1]; lc1 <- labourcost[1]; opc1 <- opcost[1]
      }else if(crops[1]=="springwheat"){
        fc1 <- fuelcost[2]; lc1 <- labourcost[2]; opc1 <- opcost[2]
      }else if(crops[1]=="winterbarley"||crops[1]=="winteroats"){
        fc1 <- fuelcost[3]; lc1 <- labourcost[3]; opc1 <- opcost[3]
      }else if(crops[1]=="springbarley"||crops[1]=="springoats"){
        fc1 <- fuelcost[4]; lc1 <- labourcost[4]; opc1 <- opcost[4]
      }else if(crops[1]=="winterbeans"){
        fc1 <- fuelcost[5]; lc1 <- labourcost[5]; opc1 <- opcost[5]
      }else if(crops[1]=="springbeans"){
        fc1 <- fuelcost[6]; lc1 <- labourcost[6]; opc1 <- opcost[6]
      }else if(crops[1]=="warepotatoes"){
        fc1 <- fuelcost[7]; lc1 <- labourcost[7]; opc1 <- opcost[7]
      }else if(crops[1]=="wosr"){
        fc1 <- fuelcost[8]; lc1 <- labourcost[8]; opc1 <- opcost[8]
      }else if(crops[1]=="sugarbeet"){
        fc1 <- fuelcost[9]; lc1 <- labourcost[9]; opc1 <- opcost[9]
      }else if(crops[1]=="setaside"||crops[1]=="fallow"){
        fc1 <- fuelcost[10]; lc1 <- labourcost[10]; opc1 <- opcost[10]
      }else if(crops[1]=="sosr"){
        fc1 <- fuelcost[11]; lc1 <- labourcost[11]; opc1 <- opcost[11]
      }else if(crops[1]=="winterlinseed"){
        fc1 <- fuelcost[12]; lc1 <- labourcost[12]; opc1 <- opcost[12]
      }else if(crops[1]=="springlinseed"){
        fc1 <- fuelcost[13]; lc1 <- labourcost[13]; opc1 <- opcost[13]
      }else if(crops[1]=="driedpeas"){
        fc1 <- fuelcost[14]; lc1 <- labourcost[14]; opc1 <- opcost[14]
      }
      
      if(crops[2]=="winterwheat"){
        fc2 <- fuelcost[1]; lc2 <- labourcost[1]; opc2 <- opcost[1]
      }else if(crops[2]=="springwheat"){
        fc2 <- fuelcost[2]; lc2 <- labourcost[2]; opc2 <- opcost[2]
      }else if(crops[2]=="winterbarley"||crops[2]=="winteroats"){
        fc2 <- fuelcost[3]; lc2 <- labourcost[3]; opc2 <- opcost[3]
      }else if(crops[2]=="springbarley"||crops[2]=="springoats"){
        fc2 <- fuelcost[4]; lc2 <- labourcost[4]; opc2 <- opcost[4]
      }else if(crops[2]=="winterbeans"){
        fc2 <- fuelcost[5]; lc2 <- labourcost[5]; opc2 <- opcost[5]
      }else if(crops[2]=="springbeans"){
        fc2 <- fuelcost[6]; lc2 <- labourcost[6]; opc2 <- opcost[6]
      }else if(crops[2]=="warepotatoes"){
        fc2 <- fuelcost[7]; lc2 <- labourcost[7]; opc2 <- opcost[7]
      }else if(crops[2]=="wosr"){
        fc2 <- fuelcost[8]; lc2 <- labourcost[8]; opc2 <- opcost[8]
      }else if(crops[2]=="sugarbeet"){
        fc2 <- fuelcost[9]; lc2 <- labourcost[9]; opc2 <- opcost[9]
      }else if(crops[2]=="setaside"||crops[2]=="fallow"){
        fc2 <- fuelcost[10]; lc2 <- labourcost[10]; opc2 <- opcost[10]
      }else if(crops[2]=="sosr"){
        fc2 <- fuelcost[11]; lc2 <- labourcost[11]; opc2 <- opcost[11]
      }else if(crops[2]=="winterlinseed"){
        fc2 <- fuelcost[12]; lc2 <- labourcost[12]; opc2 <- opcost[12]
      }else if(crops[2]=="springlinseed"){
        fc2 <- fuelcost[13]; lc2 <- labourcost[13]; opc2 <- opcost[13]
      }else if(crops[2]=="driedpeas"){
        fc2 <- fuelcost[14]; lc2 <- labourcost[14]; opc2 <- opcost[14]
      }
      
      if(crops[3]=="winterwheat"){
        fc3 <- fuelcost[1]; lc3 <- labourcost[1]; opc3 <- opcost[1]
      }else if(crops[3]=="springwheat"){
        fc3 <- fuelcost[2]; lc3 <- labourcost[2]; opc3 <- opcost[2]
      }else if(crops[3]=="winterbarley"||crops[3]=="winteroats"){
        fc3 <- fuelcost[3]; lc3 <- labourcost[3]; opc3 <- opcost[3]
      }else if(crops[3]=="springbarley"||crops[3]=="springoats"){
        fc3 <- fuelcost[4]; lc3 <- labourcost[4]; opc3 <- opcost[4]
      }else if(crops[3]=="winterbeans"){
        fc3 <- fuelcost[5]; lc3 <- labourcost[5]; opc3 <- opcost[5]
      }else if(crops[3]=="springbeans"){
        fc3 <- fuelcost[6]; lc3 <- labourcost[6]; opc3 <- opcost[6]
      }else if(crops[3]=="warepotatoes"){
        fc3 <- fuelcost[7]; lc3 <- labourcost[7]; opc3 <- opcost[7]
      }else if(crops[3]=="wosr"){
        fc3 <- fuelcost[8]; lc3 <- labourcost[8]; opc3 <- opcost[8]
      }else if(crops[3]=="sugarbeet"){
        fc3 <- fuelcost[9]; lc3 <- labourcost[9]; opc3 <- opcost[9]
      }else if(crops[3]=="setaside"||crops[3]=="fallow"){
        fc3 <- fuelcost[10]; lc3 <- labourcost[10]; opc3 <- opcost[10]
      }else if(crops[3]=="sosr"){
        fc3 <- fuelcost[11]; lc3 <- labourcost[11]; opc3 <- opcost[11]
      }else if(crops[3]=="winterlinseed"){
        fc3 <- fuelcost[12]; lc3 <- labourcost[12]; opc3 <- opcost[12]
      }else if(crops[3]=="springlinseed"){
        fc3 <- fuelcost[13]; lc3 <- labourcost[13]; opc3 <- opcost[13]
      }else if(crops[3]=="driedpeas"){
        fc3 <- fuelcost[14]; lc3 <- labourcost[14]; opc3 <- opcost[14]
      }
      
      if(crops[4]=="winterwheat"){
        fc4 <- fuelcost[1]; lc4 <- labourcost[1]; opc4 <- opcost[1]
      }else if(crops[4]=="springwheat"){
        fc4 <- fuelcost[2]; lc4 <- labourcost[2]; opc4 <- opcost[2]
      }else if(crops[4]=="winterbarley"||crops[4]=="winteroats"){
        fc4 <- fuelcost[3]; lc4 <- labourcost[3]; opc4 <- opcost[3]
      }else if(crops[4]=="springbarley"||crops[4]=="springoats"){
        fc4 <- fuelcost[4]; lc4 <- labourcost[4]; opc4 <- opcost[4]
      }else if(crops[4]=="winterbeans"){
        fc4 <- fuelcost[5]; lc4 <- labourcost[5]; opc4 <- opcost[5]
      }else if(crops[4]=="springbeans"){
        fc4 <- fuelcost[6]; lc4 <- labourcost[6]; opc4 <- opcost[6]
      }else if(crops[4]=="warepotatoes"){
        fc4 <- fuelcost[7]; lc4 <- labourcost[7]; opc4 <- opcost[7]
      }else if(crops[4]=="wosr"){
        fc4 <- fuelcost[8]; lc4 <- labourcost[8]; opc4 <- opcost[8]
      }else if(crops[4]=="sugarbeet"){
        fc4 <- fuelcost[9]; lc4 <- labourcost[9]; opc4 <- opcost[9]
      }else if(crops[4]=="setaside"||crops[4]=="fallow"){
        fc4 <- fuelcost[10]; lc4 <- labourcost[10]; opc4 <- opcost[10]
      }else if(crops[4]=="sosr"){
        fc4 <- fuelcost[11]; lc4 <- labourcost[11]; opc4 <- opcost[11]
      }else if(crops[4]=="winterlinseed"){
        fc4 <- fuelcost[12]; lc4 <- labourcost[12]; opc4 <- opcost[12]
      }else if(crops[4]=="springlinseed"){
        fc4 <- fuelcost[13]; lc4 <- labourcost[13]; opc4 <- opcost[13]
      }else if(crops[4]=="driedpeas"){
        fc4 <- fuelcost[14]; lc4 <- labourcost[14]; opc4 <- opcost[14]
      }
      
      if(crops[5]=="winterwheat"){
        fc5 <- fuelcost[1]; lc5 <- labourcost[1]; opc5 <- opcost[1]
      }else if(crops[5]=="springwheat"){
        fc5 <- fuelcost[2]; lc5 <- labourcost[2]; opc5 <- opcost[2]
      }else if(crops[5]=="winterbarley"||crops[5]=="winteroats"){
        fc5 <- fuelcost[3]; lc5 <- labourcost[3]; opc5 <- opcost[3]
      }else if(crops[5]=="springbarley"||crops[5]=="springoats"){
        fc5 <- fuelcost[4]; lc5 <- labourcost[4]; opc5 <- opcost[4]
      }else if(crops[5]=="winterbeans"){
        fc5<- fuelcost[5]; lc5 <- labourcost[5]; opc5 <- opcost[5]
      }else if(crops[5]=="springbeans"){
        fc5 <- fuelcost[6]; lc5 <- labourcost[6]; opc5 <- opcost[6]
      }else if(crops[5]=="warepotatoes"){
        fc5 <- fuelcost[7]; lc5 <- labourcost[7]; opc5 <- opcost[7]
      }else if(crops[5]=="wosr"){
        fc5 <- fuelcost[8]; lc5 <- labourcost[8]; opc5 <- opcost[8]
      }else if(crops[5]=="sugarbeet"){
        fc5 <- fuelcost[9]; lc5 <- labourcost[9]; opc5 <- opcost[9]
      }else if(crops[5]=="setaside"||crops[5]=="fallow"){
        fc5 <- fuelcost[10]; lc5 <- labourcost[10]; opc5 <- opcost[10]
      }else if(crops[5]=="sosr"){
        fc5 <- fuelcost[11]; lc5 <- labourcost[11]; opc5 <- opcost[11]
      }else if(crops[5]=="winterlinseed"){
        fc5 <- fuelcost[12]; lc5 <- labourcost[12]; opc5 <- opcost[12]
      }else if(crops[5]=="springlinseed"){
        fc5 <- fuelcost[13]; lc5 <- labourcost[13]; opc5 <- opcost[13]
      }else if(crops[5]=="driedpeas"){
        fc5 <- fuelcost[14]; lc5 <- labourcost[14]; opc5 <- opcost[14]
      }
      
      if(crops[6]=="winterwheat"){
        fc6 <- fuelcost[1]; lc6 <- labourcost[1]; opc6 <- opcost[1]
      }else if(crops[6]=="springwheat"){
        fc6 <- fuelcost[2]; lc6 <- labourcost[2]; opc6 <- opcost[2]
      }else if(crops[6]=="winterbarley"||crops[6]=="winteroats"){
        fc6 <- fuelcost[3]; lc6 <- labourcost[3]; opc6 <- opcost[3]
      }else if(crops[6]=="springbarley"||crops[6]=="springoats"){
        fc6 <- fuelcost[4]; lc6 <- labourcost[4]; opc6 <- opcost[4]
      }else if(crops[6]=="winterbeans"){
        fc6 <- fuelcost[5]; lc6 <- labourcost[5]; opc6 <- opcost[5]
      }else if(crops[6]=="springbeans"){
        fc6 <- fuelcost[6]; lc6 <- labourcost[6]; opc6 <- opcost[6]
      }else if(crops[6]=="warepotatoes"){
        fc6 <- fuelcost[7]; lc6 <- labourcost[7]; opc6 <- opcost[7]
      }else if(crops[6]=="wosr"){
        fc6 <- fuelcost[8]; lc6 <- labourcost[8]; opc6 <- opcost[8]
      }else if(crops[6]=="sugarbeet"){
        fc6 <- fuelcost[9]; lc6 <- labourcost[9]; opc6 <- opcost[9]
      }else if(crops[6]=="setaside"||crops[6]=="fallow"){
        fc6 <- fuelcost[10]; lc6 <- labourcost[10]; opc6 <- opcost[10]
      }else if(crops[6]=="sosr"){
        fc6 <- fuelcost[11]; lc6 <- labourcost[11]; opc6 <- opcost[11]
      }else if(crops[6]=="winterlinseed"){
        fc6 <- fuelcost[12]; lc6 <- labourcost[12]; opc6 <- opcost[12]
      }else if(crops[6]=="springlinseed"){
        fc6 <- fuelcost[13]; lc6 <- labourcost[13]; opc6 <- opcost[13]
      }else if(crops[6]=="driedpeas"){
        fc6 <- fuelcost[14]; lc6 <- labourcost[14]; opc6 <- opcost[14]
      }
      
      cr[c11,seq(2,le)]  <- c(fc1,fc2,fc3,fc4,fc5,fc6)
      cr[c12,seq(2,le)]  <- c(lc1,lc2,lc3,lc4,lc5,lc6)
      cr[c13,seq(2,le)]  <- c(opc1,opc2,opc3,opc4,opc5,opc6)
      
      cr[c14,seq(2,le)] <- grossprofit <- cr[c10,seq(2,le)]-cr[c13,seq(2,le)] # Gross profit estimates
      kk4 <- cr[seq(1,31),]
    }
    
    
    # Model output components =====
    #**************************************************************** 
    rotl <- c(2,3,4,5,6)
    if(rotlength%in%rotl==TRUE){
      nrotl <- rotlength
    }else{
      nrotl <- 6
      warning("Maximum 6-year rotation is assumed (Rotation length MUST be between 2 and 6 years)")
    }
    
    
    if(is.null(rotprob)){
      rotp <- 1/nrotl # Rotational probability (6 year rotation)
    }else{
      rotp <- rotprob
    }
    
    di <- 2
    Component=c("Basic Rotation","Cultivation","Farm Output","Fertiliser Cost","Seed Cost","Herbicide Cost",
                "Sundry Cost","Variable Cost","Gross Margin",
                "Fuel Cost","Labour Cost","Operating Cost","Gross Profit","Rotation Profit")
    
    Unit=c("na","na",rep("?/ha",length(Component)-2))
    
    # Row Indices ===
    c1 <- 18; c2 <- 19; c3 <- 20; c4 <- 21; c5 <- 22; c6 <- 23; c7 <- 24; c8 <- 25; c9 <- 26;# === row indices
    c10 <- 27; c11 <- 28; c12 <- 29; c13 <- 30; c14 <- 31; 
    
    h1 <- 10; h2 <- 11; h3 <- 12; h4 <- 13; h5 <- 14; h6 <- 15;
    h7 <- 16; h8 <- 17; 
    
    # Year 1  *************************************************************
    
    crop1 <- crops[1]; crop2 <- crops[2]; crop3 <- crops[3]; crop4 <- crops[4]; crop5 <- crops[5]; crop6 <- crops[6]
    tillage1 <- tillages[1]; tillage2 <- tillages[2]; tillage3 <- tillages[3]; tillage4 <- tillages[4]; tillage5 <- tillages[5];
    tillage6 <- tillages[6]
    
    if(crop1=="winterwheat"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("WWHT",tillages[1],ncb)
      #cp1 <- "WWHT"
      cy1 <- cb[c3]
    }else if(crop1=="springwheat"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("SWHT",tillages[1],ncb) 
      #cp1 <- "SWHT"
      cy1 <- cb[c3]
    }else if (crop1=="winterbarley"){
      cb <- cropBudget(crop=crops, nsoil, subsidy, blackgrass, tillage1, selfrot=0, rotpen=0, yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("WBAR",tillages[1],ncb) 
      #cp1 <- "WBAR"
      cy1 <- cb[c3]
    }else if(crop1=="springbarley"){
      cb <- cropBudget(crop=crops, nsoil, subsidy, blackgrass, tillage1, selfrot=0, rotpen=0, yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("SBAR",tillages[1],ncb) 
      #cp1 <- "SBAR"
      cy1 <- cb[c3]
    }else if(crop1=="winterbeans"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("WBEA",tillages[1],ncb) 
      cp1 <- "WBEA"
      cy1 <- cb[c3]
    }else if(crop1=="springbeans"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("SBEA",tillages[1],ncb) 
      #cp1 <- "SBEA"
      cy1 <- cb[c3]
    }else if(crop1=="warepotatoes"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("WPOT",tillages[1],ncb) 
      #cp1 <- "WPOT"
      cy1 <- cb[c3]
    }else if(crop1=="wosr"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("WOSR",tillages[1],ncb) 
      #cp1 <- "WOSR"
      cy1 <- cb[c3]
    }else if(crop1=="sugarbeet"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,h8)],cb[c1]+cb[c2],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("SBEE",tillages[1],ncb) 
      #cp1 <- "SBEE"
      cy1 <- cb[c3]
    }else if(crop1=="setaside"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("SETA",tillages[1],ncb) 
      #cp1 <- "SETA"
      cy1 <- cb[c3]
    }else if(crop1=="none"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("SETA",tillages[1],ncb) 
      #cp1 <- "SETA"
      cy1 <- cb[c3]
    }else if(crop1=="fallow"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("FALLOW",tillages[1],ncb)
      #cp1 <- "SETA"
      cy1 <- cb[c3]
    }else if(crop1=="sosr"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("SOSR",tillages[1],ncb)
      cy1 <- cb[c3]
    }else if(crop1=="winterlinseed"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("WLIN",tillages[1],ncb)
      cy1 <- cb[c3]
    }else if(crop1=="springlinseed"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("SLIN",tillages[1],ncb)
      cy1 <- cb[c3]
    }else if(crop1=="driedpeas"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("DPEA",tillages[1],ncb)
      cy1 <- cb[c3]
    }
    
    # Year 2  *************************************************************
    # ========= Yield Penalty | Crop Rotations =========
    if(crop2=="winterwheat"){ 
      
      if(crop1=="winterwheat"){
        selfrot <- 10 # 10% Yield loss
      }else{
        selfrot = 0
      }
      if(crop1=="winterbarley"||crop1=="springbarley"||crop1=="springwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("WWHT",tillage2,ncb2) 
      cy2 <- cb2[c3]
    }else if(crop2=="springwheat"){
      if(crop1=="springwheat"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      if(crop1=="winterbarley"||crop1=="springbarley"||crop1=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("SWHT",tillage2,ncb2) 
      cy2 <- cb2[c3]
    }else if (crop2=="winterbarley"){
      if(crop1=="winterbarley"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      if(crop1=="springwheat"||crop1=="springbarley"||crop1=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("WBAR",tillage2,ncb2) 
      cy2 <- cb2[c3]
    }else if(crop2=="springbarley"){
      if(crop1=="springbarley"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      if(crop1=="springwheat"||crop1=="winterbarley"||crop1=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("SBAR",tillage2,ncb2) 
      cy2 <- cb2[c3]
    }else if(crop2=="winterbeans"){
      selfrot <- 0
      if(crop1=="wosr"||crop1=="sugarbeet"||crop1=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("WBEA",tillage2,ncb2) 
      cy2 <- cb2[c3]
    }else if(crop2=="springbeans"){
      selfrot <- 0
      if(crop1=="wosr"||crop1=="sugarbeet"||crop1=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("SBEA",tillage2,ncb2) 
      cy2 <- cb2[c3]
    }else if(crop2=="warepotatoes"){
      if(crop1=="warepoatoes"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Potatoes is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop1=="sugarbeet"){ 
        rotpen <- 5
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("WPOT",tillage2,ncb2)  
      cy2 <- cb2[c3]
    }else if(crop2=="wosr"){
      if(crop1=="wosr"||crop1=="sosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop1=="winterbeans"||crop1=="springbeans"||crop1=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("WOSR",tillage2,ncb2)  
      cy2 <- cb2[c3]
    }else if(crop2=="sugarbeet"){
      if(crop1=="sugarbeat"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Sugarbeet is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop1=="winterbeans"||crop1=="springbeans"||crop1=="wosr"||crop1=="warepotatoes"||crop1=="sosr"){ 
        rotpen <- 100
        warning("Oilseed rape, Beans or Potatoes--Sugarbeet sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,h8)],cb2[c1]+cb2[c2],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("SBEE",tillage2,ncb2)  
      cy2 <- cb2[c3]
    }else if(crop2=="setaside"){ 
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot=0,rotpen=0,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("SETA",tillage2,ncb2)
      cy2 <- cb2[c3]
    }else if(crop2=="none"){
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot=0,rotpen=0,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("SETA",tillage2,ncb2)
      cy2 <- cb2[c3]
    }else if(crop2=="fallow"){
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot=0,rotpen=0,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("FALLOW",tillage2,ncb2) 
      cy2 <- cb2[c3]
    }else if(crop2=="sosr"){
      if(crop1=="sosr"||crop1=="wosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot = 0
      }
      if(crop1=="winterbeans"||crop1=="springbeans"||crop1=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("SOSR",tillage2,ncb2) 
      cy2 <- cb2[c3]
    }else if(crop2=="winterlinseed"){
      if(crop1=="winterlinseed"||crop1=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop1=="winterbeans"||crop1=="springbeans"||crop1=="sugarbeet"||crop1=="driedpeas"||crop1=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Linseed sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("WLIN",tillage2,ncb2) 
      cy2 <- cb2[c3]
    }else if(crop2=="springlinseed"){
      if(crop1=="winterlinseed"||crop1=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop1=="winterbeans"||crop1=="springbeans"||crop1=="sugarbeet"||crop1=="driedpeas"||crop1=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Linseed sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("SLIN",tillage2,ncb2)
      cy2 <- cb2[c3]
    }else if(crop2=="driedpeas"){
      if(crop1=="driedpeas"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Peas is penalised")
      }else{
        selfrot <- 0
      }
      if(crop1=="wosr"||crop1=="sosr"||crop1=="setaside"){ 
        rotpen <- 100
        warning("OSR and  Rotational Set-aside--Peas sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("DPEA",tillage2,ncb2)
      cy2 <- cb2[c3]
    }
    
    
    # Year 3  *************************************************************
    
    if(crop3=="winterwheat"){
      if(crop2=="winterwheat"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      if(crop2=="winterbarley"||crop2=="springbarley"||crop2=="springwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("WWHT",tillage3,ncb3) 
      cy3 <- cb3[c3]
    }else if(crop3=="springwheat"){
      if(crop2=="springwheat"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      
      if(crop2=="winterbarley"||crop2=="springbarley"||crop2=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("SWHT",tillage3,ncb3) 
      cy3 <- cb3[c3]
    }else if (crop3=="winterbarley"){
      if(crop2=="winterbarley"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      if(crop2=="springwheat"||crop2=="springbarley"||crop2=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("WBAR",tillage3,ncb3) 
      cy3 <- cb3[c3]
    }else if(crop3=="springbarley"){
      if(crop2=="springbarley"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      if(crop2=="springwheat"||crop2=="winterbarley"||crop2=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("SBAR",tillage3,ncb3) 
      cy3 <- cb3[c3]
    }else if(crop3=="winterbeans"){
      selfrot <- 0
      if(crop2=="wosr"||crop2=="sugarbeet"||crop2=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("WBEA",tillage3,ncb3) 
      cy3 <- cb3[c3]
    }else if(crop3=="springbeans"){
      selfrot <- 0
      if(crop2=="wosr"||crop2=="sugarbeet"||crop2=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("SBEA",tillage3,ncb3) 
      cy3 <- cb3[c3]
    }else if(crop3=="warepotatoes"){
      if(crop2=="warepoatoes"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Potatoes is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop2=="sugarbeet"){ 
        rotpen <- 5
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("WPOT",tillage3,ncb3)  
      cy3 <- cb3[c3]
    }else if(crop3=="wosr"){
      if(crop2=="wosr"||crop2=="sosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot <- 0
      }
      
      if(crop2=="winterbeans"||crop2=="springbeans"||crop2=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("WOSR",tillage3,ncb3)  
      cy3 <- cb3[c3]
    }else if(crop3=="sugarbeet"){
      if(crop2=="sugarbeat"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Sugarbeet is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop2=="winterbeans"||crop2=="springbeans"||crop2=="wosr"||crop2=="warepotatoes"||crop2=="sosr"){ 
        rotpen <- 100
        warning("Sugarbeet--Oilseed rape, Beans or Potatoes sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,h8)],cb3[c1]+cb3[c2],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("SBEE",tillage3,ncb3)  
      cy3 <- cb3[c3]
    }else if(crop3=="setaside"||crop3=="none"){ 
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot=0,rotpen=0,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("SETA",tillage3,ncb3) 
      cy3 <- cb3[c3]
    }else if(crop3=="none"){
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot=0,rotpen=0,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("SETA",tillage3,ncb3) 
      cy3 <- cb3[c3]
    }else if(crop3=="fallow"){
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot=0,rotpen=0,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("FALLOW",tillage3,ncb3)
      cy3 <- cb3[c3]
    }else if(crop3=="sosr"){
      if(crop2=="wosr"||crop2=="sosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot = 0
      }
      if(crop2=="winterbeans"||crop2=="springbeans"||crop2=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("SOSR",tillage3,ncb3)
      cy3 <- cb3[c3]
    }else if(crop3=="winterlinseed"){
      if(crop2=="winterlinseed"||crop2=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop2=="winterbeans"||crop2=="springbeans"||crop2=="sugarbeet"||crop2=="driedpeas"||crop2=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("WLIN",tillage3,ncb3)
      cy3 <- cb3[c3]
    }else if(crop3=="springlinseed"){
      if(crop2=="winterlinseed"||crop2=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop2=="winterbeans"||crop2=="springbeans"||crop2=="sugarbeet"||crop2=="driedpeas"||crop2=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("SLIN",tillage3,ncb3) 
      cy3 <- cb3[c3]
    }else if(crop3=="driedpeas"){
      if(crop2=="driedpeas"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Peas is penalised")
      }else{
        selfrot <- 0
      }
      if(crop2=="wosr"||crop2=="sosr"||crop2=="setaside"){ 
        rotpen <- 100
        warning("OSR and Rotational Set-aside--Peas sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("DPEA",tillage3,ncb3) 
      cy3 <- cb3[c3]
    } 
    
    
    # Year 4  *************************************************************
    
    if(crop4=="winterwheat"){
      
      if(crop3=="winterwheat"){
        selfrot <- 15 # 11% Yield loss
      }else{
        selfrot = 0
      }
      if(crop3=="winterbarley"||crop3=="springbarley"||crop3=="springwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("WWHT",tillage4,ncb4) 
      cy4 <- cb4[c3]
    }else if(crop4=="springwheat"){
      if(crop3=="springwheat"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      
      if(crop3=="winterbarley"||crop3=="springbarley"||crop3=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("SWHT",tillage4,ncb4) 
      cy4 <- cb4[c3]
    }else if (crop4=="winterbarley"){
      if(crop3=="winterbarley"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      if(crop3=="springwheat"||crop3=="springbarley"||crop3=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("WBAR",tillage4,ncb4) 
      cy4 <- cb4[c3]
    }else if(crop4=="springbarley"){
      if(crop3=="springbarley"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      if(crop3=="springwheat"||crop3=="winterbarley"||crop3=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("SBAR",tillage4,ncb4) 
      cy4 <- cb4[c3]
    }else if(crop4=="winterbeans"){
      selfrot <- 0
      if(crop3=="wosr"||crop3=="sugarbeet"||crop3=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("WBEA",tillage4,ncb4) 
      cy4 <- cb4[c3]
    }else if(crop4=="springbeans"){
      selfrot <- 0
      if(crop3=="wosr"||crop3=="sugarbeet"||crop3=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("SBEA",tillage4,ncb4) 
      cy4 <- cb4[c3]
    }else if(crop4=="warepotatoes"){
      if(crop3=="warepoatoes"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Potatoes is penalised")
      }else{
        selfrot = 0
      }
      if(crop3=="sugarbeet"){ 
        rotpen <- 5
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("WPOT",tillage4,ncb4)  
      cy4 <- cb4[c3]
    }else if(crop4=="wosr"){
      if(crop3=="wosr"||crop3=="sosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot = 0
      }
      if(crop3=="winterbeans"||crop3=="springbeans"||crop3=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("WOSR",tillage4,ncb4)  
      cy4 <- cb4[c3]
    }else if(crop4=="sugarbeet"){
      if(crop3=="sugarbeat"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Sugarbeet is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop3=="winterbeans"||crop3=="springbeans"||crop3=="wosr"||crop3=="warepotatoes"||crop3=="sosr"){ 
        rotpen <- 100
        warning("Sugarbeet--Oilseed rape, Beans or Potatoes sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,h8)],cb4[c1]+cb4[c2],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("SBEE",tillage4,ncb4)  
      cy4 <- cb4[c3]
    }else if(crop4=="setaside"){ 
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot=0,rotpen=0,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("SETA",tillage4,ncb4)
      cy4 <- cb4[c3]
    }else if(crop4=="none"){
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot=0,rotpen=0,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("SETA",tillage4,ncb4)
      cy4 <- cb4[c3]
    }else if(crop4=="fallow"){
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot=0,rotpen=0,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("FALLOW",tillage4,ncb4)
      cy4 <- cb4[c3]
    }else if(crop4=="sosr"){
      if(crop3=="wosr"||crop3=="sosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot = 0
      }
      if(crop3=="winterbeans"||crop3=="springbeans"||crop3=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("SOSR",tillage4,ncb4) 
      cy4 <- cb4[c3]
    }else if(crop4=="winterlinseed"){
      if(crop3=="winterlinseed"||crop3=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop3=="winterbeans"||crop3=="springbeans"||crop3=="sugarbeet"||crop3=="driedpeas"||crop3=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Linseed sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("WLIN",tillage4,ncb4)
      cy4 <- cb4[c3]
    }else if(crop4=="springlinseed"){
      if(crop3=="winterlinseed"||crop3=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop3=="winterbeans"||crop3=="springbeans"||crop3=="sugarbeet"||crop3=="driedpeas"||crop3=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Linseed sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("SLIN",tillage4,ncb4)
      cy4 <- cb4[c3]
    }else if(crop4=="driedpeas"){
      if(crop3=="driedpeas"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Peas is penalised")
      }else{
        selfrot <- 0
      }
      if(crop3=="wosr"||crop3=="sosr"||crop3=="setaside"){ 
        rotpen <- 100
        warning("OSR and Rotational Set-aside--Peas sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("DPEA",tillage4,ncb4)
      cy4 <- cb4[c3]
    }
    
    # Year 5  *************************************************************
    
    if(crop5=="winterwheat"){
      
      if(crop4=="winterwheat"){
        selfrot <- 15 # 15% Yield loss
      }else{
        selfrot = 0
      }
      
      if(crop4=="winterbarley"||crop4=="springbarley"||crop4=="springwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("WWHT",tillage5,ncb5) 
      cy5 <- cb5[c3]
    }else if(crop5=="springwheat"){
      if(crop4=="springwheat"){
        selfrot <- 11 # 
      }else{
        selfrot = 0
      }
      
      if(crop4=="winterbarley"||crop4=="springbarley"||crop4=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("SWHT",tillage5,ncb5) 
      cy5 <- cb5[c3]
    }else if (crop5=="winterbarley"){
      if(crop4=="winterbarley"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      
      if(crop4=="springwheat"||crop4=="springbarley"||crop4=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("WBAR",tillage5,ncb5) 
      cy5 <- cb5[c3]
    }else if(crop5=="springbarley"){
      if(crop4=="springbarley"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      
      if(crop4=="springwheat"||crop4=="winterbarley"||crop4=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("SBAR",tillage5,ncb5) 
      cy5 <- cb5[c3]
    }else if(crop5=="winterbeans"){
      selfrot <- 0
      if(crop4=="wosr"||crop4=="sugarbeet"||crop4=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("WBEA",tillage5,ncb5) 
      cy5 <- cb5[c3]
    }else if(crop5=="springbeans"){
      selfrot <- 0
      if(crop4=="wosr"||crop4=="sugarbeet"||crop4=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("SBEA",tillage5,ncb5) 
      cy5 <- cb5[c3]
    }else if(crop5=="warepotatoes"){
      if(crop4=="warepoatoes"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Potatoes is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop4=="sugarbeet"){ 
        rotpen <- 5
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("WPOT",tillage5,ncb5)  
      cy5 <- cb5[c3]
    }else if(crop5=="wosr"){
      if(crop4=="wosr"||crop4=="sosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop4=="winterbeans"||crop4=="springbeans"||crop4=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("WOSR",tillage5,ncb5)  
      cy5 <- cb5[c3]
    }else if(crop5=="sugarbeet"){
      if(crop4=="sugarbeat"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Sugarbeet is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop4=="winterbeans"||crop4=="springbeans"||crop4=="wosr"||crop4=="warepotatoes"||crop4=="sosr"){ 
        rotpen <- 100
        warning("Oilseed rape, Beans or Potatoes--Sugarbeet sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,h8)],cb5[c1]+cb5[c2],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("SBEE",tillage5,ncb5)  
      cy5 <- cb5[c3]
    }else if(crop5=="setaside"){ 
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot=0,rotpen=0,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("SETA",tillage5,ncb5) 
      cy5 <- cb5[c3]
    }else if(crop5=="none"){
      ccb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot=0,rotpen=0,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("SETA",tillage5,ncb5)
      cy5 <- cb5[c3]
    }else if(crop5=="fallow"){
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot=0,rotpen=0,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("FALLOW",tillage5,ncb5)
      cy5 <- cb5[c3]
    }else if(crop5=="sosr"){
      if(crop4=="wosr"||crop4=="sosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot = 0
      }
      if(crop4=="winterbeans"||crop4=="springbeans"||crop4=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("SOSR",tillage5,ncb5)
      cy5 <- cb5[c3]
    }else if(crop5=="winterlinseed"){
      if(crop4=="winterlinseed"||crop4=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop4=="winterbeans"||crop4=="springbeans"||crop4=="sugarbeet"||crop4=="driedpeas"||crop4=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Linseed sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("WLIN",tillage5,ncb5)
      cy5 <- cb5[c3]
    }else if(crop5=="springlinseed"){
      if(crop4=="winterlinseed"||crop4=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop4=="winterbeans"||crop4=="springbeans"||crop4=="sugarbeet"||crop4=="driedpeas"||crop4=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Linseed sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("SLIN",tillage5,ncb5) 
      cy5 <- cb5[c3]
    }else if(crop5=="driedpeas"){
      if(crop4=="driedpeas"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Peas is penalised")
      }else{
        selfrot <- 0
      }
      if(crop4=="wosr"||crop4=="sosr"||crop4=="setaside"){ 
        rotpen <- 100
        warning("OSR and Rotational Set-aside--Peas sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("DPEA",tillage5,ncb5)
      cy5 <- cb5[c3]
    }
    
    
    # Year 6  *************************************************************
    
    if(crop6=="winterwheat"){
      
      if(crop5=="winterwheat"){
        selfrot <- 17 # 17% Yield loss
      }else{
        selfrot = 0
      }
      if(crop5=="winterbarley"||crop5=="springbarley"||crop5=="springwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("WWHT",tillage6,ncb6) 
      cy6 <- cb6[c3]
    }else if(crop6=="springwheat"){
      if(crop5=="springwheat"){
        selfrot <- 12 # 12% Yield loss
      }else{
        selfrot = 0
      }
      
      if(crop5=="winterbarley"||crop5=="springbarley"||crop5=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("SWHT",tillage6,ncb6) 
      cy6 <- cb6[c3]
    }else if (crop6=="winterbarley"){
      if(crop5=="winterbarley"){
        selfrot <- 12 # 12% Yield loss
      }else{
        selfrot = 0
      }
      
      if(crop5=="springwheat"||crop5=="springbarley"||crop5=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("WBAR",tillage6,ncb6) 
      cy6 <- cb6[c3]
    }else if(crop6=="springbarley"){
      if(crop5=="springbarley"){
        selfrot <- 12 # 12% Yield loss
      }else{
        selfrot = 0
      }
      
      if(crop5=="springwheat"||crop5=="winterbarley"||crop5=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("SBAR",tillage6,ncb6) 
      cy6 <- cb6[c3]
    }else if(crop6=="winterbeans"){
      selfrot <- 0
      if(crop5=="wosr"||crop5=="sugarbeet"||crop5=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("WBEA",tillage6,ncb6) 
      cy6 <- cb6[c3]
    }else if(crop6=="springbeans"){
      selfrot <- 0
      if(crop5=="wosr"||crop5=="sugarbeet"||crop5=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("SBEA",tillage6,ncb6) 
      cy6 <- cb6[c3]
    }else if(crop6=="warepotatoes"){
      if(crop5=="warepoatoes"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Potatoes is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop5=="sugarbeet"){ 
        rotpen <- 5
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("WPOT",tillage6,ncb6)  
      cy6 <- cb6[c3]
    }else if(crop6=="wosr"){
      if(crop5=="wosr"||crop5=="sosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot = 0
      }
      if(crop5=="winterbeans"||crop5=="springbeans"||crop5=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("WOSR",tillage6,ncb6)  
      cy6 <- cb6[c3]
    }else if(crop6=="sugarbeet"){
      if(crop5=="sugarbeet"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Sugarbeet is penalised")
      }else{
        selfrot = 0
      }
      if(crop5=="winterbeans"||crop5=="springbeans"||crop5=="wosr"||crop5=="warepotatoes"||crop5=="sosr"){ 
        rotpen <- 100
        warning("Oilseed rape, Beans or Potatoes--Sugarbeet sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,h8)],cb6[c1]+cb6[c2],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("SBEE",tillage6,ncb6)  
      cy6 <- cb6[c3]
    }else if(crop6=="setaside"){ 
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot=0,rotpen=0,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("SETA",tillage6,ncb6)
      cy6 <- cb6[c3]
    }else if(crop6=="none"){
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot=0,rotpen=0,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("SETA",tillage6,ncb6) # c(rep(0,14))
      cy6 <- cb6[c3]
    }else if(crop6=="fallow"){
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot=0,rotpen=0,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("FALLOW",tillage6,ncb6) 
      cy6 <- cb6[c3]
    }else if(crop6=="sosr"){
      if(crop5=="wosr"||crop5=="sosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot = 0
      }
      if(crop5=="winterbeans"||crop5=="springbeans"||crop5=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("SOSR",tillage6,ncb6)
      cy6 <- cb6[c3]
    }else if(crop6=="winterlinseed"){
      if(crop5=="winterlinseed"||crop5=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop5=="winterbeans"||crop5=="springbeans"||crop5=="sugarbeet"||crop5=="driedpeas"||crop5=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Linseed sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("WLIN",tillage6,ncb6) 
      cy6 <- cb6[c3]
    }else if(crop6=="springlinseed"){
      if(crop5=="winterlinseed"||crop5=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop5=="winterbeans"||crop5=="springbeans"||crop5=="sugarbeet"||crop5=="driedpeas"||crop5=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Linseed sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("SLIN",tillage6,ncb6) 
      cy6 <- cb6[c3]
    }else if(crop6=="driedpeas"){
      if(crop5=="driedpeas"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Peas is penalised")
      }else{
        selfrot <- 0
      }
      if(crop5=="wosr"||crop5=="sosr"||crop5=="setaside"){ 
        rotpen <- 100
        warning("OSR and  Rotational Set-aside--Peas sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("DPEA",tillage6,ncb6)
      cy6 <- cb6[c3]
    }
    
    # Rotation lenth: Maximum 6 year rotation is assumed
    
    if(farm=="single"){
      
      d1 <- data.frame(Component,Unit,Year1,Year2,Year3,Year4,Year5,Year6)
      nrotl <- rotlength
      if(nrotl==2){
        d1[,seq(-5,-8)]
      }else if(nrotl==3){
        d1[,seq(-6,-8)]
      }else if(nrotl==4){
        d1[,seq(-7,-8)]
      }else if(nrotl==5){
        d1[,-8]
      }else if(nrotl==6){
        d1
      }
      
    }else if(farm=="multiple"){
      d2 <- c(cy1,ncb,cy2,ncb2,cy3,ncb3,cy4,ncb4,cy5,ncb5,cy6,ncb6)
      #d2
      if(nrotl==2){
        d3 <- d2[seq(-27,-78)]
        d4 <- c(d3, sum(d3[13],d3[26]))
      }else if(nrotl==3){
        d3 <- d2[seq(-40,-78)]
        d4 <- c(d3, sum(d3[13],d3[26],d3[39]))
      }else if(nrotl==4){
        d3 <- d2[seq(-53,-78)]
        d4 <- c(d3, sum(d3[13],d3[26],d3[39],d3[52]))
      }else if(nrotl==5){
        d3 <- d2[seq(-66,-78)]
        d4 <- c(d3, sum(d3[13],d3[26],d3[39],d3[52],d3[65]))
      }else if(nrotl==6){
        d3 <- d2
        d4 <- c(d3, sum(d3[13],d3[26],d3[39],d3[52],d3[65],d3[78]))
        
      }
    }
  }
  #************************
  if(farm=="single"){
    
    ecm <- ECOMOD(farm="single",default,soil,rotlength,rotprob,crops,tillages,seedrate,delsowing,Nfert,Pfert,Kfert,bgherbdose,
                  glyphosatedose,numberofsprays,subsidy,blackgrass,cropprice,cropyield,yieldoption,Nfertprice,
                  Pfertprice,Kfertprice,seedprice,herbprice,glyphosateprice,machsize,fuelprice,labourwage)
    
  }else if(farm=="multiple"&is.null(default)){
    fd <- farmdata
    
    est <- c("yield","output","fertcost","seedcost","herbcost","sundry","varcost","gmargin","fuelcost",
             "labcost","opcost","grossprof","rotgrossprof")
    
    cp1 <- "crop.1" #crops[1]
    cp2 <- "crop.2" #crops[2]
    cp3 <- "crop.3" #crops[3]
    cp4 <- "crop.4" #crops[4]
    cp5 <- "crop.5" #crops[5]
    cp6 <- "crop.6" #crops[6]
    
    v1 <- ".1"
    v2 <- ".2"
    v3 <- ".3"
    v4 <- ".4"
    v5 <- ".5"
    v6 <- ".6"    
    
    p1 <- paste(est,v1,sep="")
    p2 <- paste(est,v2,sep="")
    p3 <- paste(est,v3,sep="")
    p4 <- paste(est,v4,sep="")
    p5 <- paste(est,v5,sep="")
    p6 <- paste(est,v6,sep="")
    
    hd <- c(p1,p2,p3,p4,p5,p6)
    
    nrotl <- rotlength
    if(nrotl==2){
      hd1 <- hd[seq(-27,-78)]
    }else if(nrotl==3){
      hd1 <- hd[seq(-40,-78)]
    }else if(nrotl==4){
      hd1 <- hd[seq(-53,-78)]
    }else if(nrotl==5){
      hd1 <- hd[seq(-66,-78)]
    }else if(nrotl==6){
      hd1 <- hd
    }
    
    cp <- fd[,seq(5,10)]
    ti <- fd[,seq(11,16)]
    ds <- fd[,seq(23,28)]
    sb <- as.character(fd[,65])
    bg <- fd[,seq(66,71)]
    yo <- as.character(fd[,84])
    
    
    for(k in 1:ncol(cp)){
      if(class(cp[,k])=="factor"){cp[,k] <- as.character(cp[,k])}
    }
    
    for(k in 1:ncol(ti)){
      if(class(ti[,k])=="factor"){ti[,k] <- as.character(ti[,k])}
    }
    
    for(k in 1:ncol(ds)){
      if(class(ds[,k])=="factor"){ds[,k] <- as.character(ds[,k])}
    }
    
    
    for(k in 1:ncol(bg)){
      if(class(bg[,k])=="factor"){bg[,k] <- as.character(bg[,k])}
    }
    
    
    
    
    fms <- c()
    
    index <- c(seq(1, length(farmdata[,1])))
    
    for(i in index){
      fms <- c(fms,ECOMOD(farm="multiple",default,soil=fd[i,3],rotlength,rotprob,crops=c(cp[,1][i],cp[,2][i],cp[,3][i],cp[,4][i],cp[,5][i],cp[,6][i]),
                          tillage=c(ti[,1][i],ti[,2][i],ti[,3][i],ti[,4][i],ti[,5][i],ti[,6][i]),seedrate=as.numeric(fd[i,seq(17,22)]),
                          delsowing=c(ds[,1][i],ds[,2][i],ds[,3][i],ds[,4][i],ds[,5][i],ds[,6][i]),
                          Nfert=as.numeric(fd[i,seq(29,34)]),Pfert=as.numeric(fd[i,seq(35,40)]),Kfert=as.numeric(fd[i,seq(41,46)]),
                          bgherbdose=as.numeric(fd[i,seq(47,52)]), 
                          glyphosatedose=as.numeric(fd[i,seq(53,58)]),numberofsprays=as.numeric(fd[i,seq(59,64)]),subsidy=sb[i],
                          blackgrass=c(bg[,1][i],bg[,2][i],bg[,3][i],bg[,4][i],bg[,5][i],bg[,6][i]), 
                          cropprice=as.numeric(fd[i,seq(72,77)]),
                          cropyield=as.numeric(fd[i,seq(78,83)]),yieldoption=yo[i], #yieldoption, 
                          Nfertprice=as.numeric(fd[i,85]),Pfertprice=as.numeric(fd[i,86]),
                          Kfertprice=as.numeric(fd[i,87]),
                          seedprice=as.numeric(fd[i,seq(88,93)]),herbprice=as.numeric(fd[i,seq(94,99)]),glyphosateprice=as.numeric(fd[i,seq(100,105)]),
                          machsize=as.numeric(fd[i,seq(106,110)]),fuelprice=as.numeric(fd[i,111]),
                          labourwage=as.numeric(fd[i,112])))
      # subsidy==fd[i,70]
      # yieldoption==fd[i,89] or yo[i]
      #print(i)
      fms1 <- fms
      cols <- c(hd1,"totalrotgrossprof")
      ros <- as.character(index)
      
      mat <- suppressWarnings(matrix(fms1,ncol=length(cols),nrow=length(index),byrow=T,dimnames=list(ros,cols)))
      Field_no <- index
      field_name <- fd[,2]
      ecm <- data.frame(Field_no,field_name,fd[,seq(5,10)],mat)
      
      # NAME OF THE FILE CAN BE CHANGED TO WHICHEVER NAME DESIRED *********
      # e.g. "Farms_Fields_Economic_Outcomes.csv" as in Supplementary Information
      write.table(ecm,file=filename,row.names=FALSE,sep=",")
      
    }
  }else if(farm=="multiple"&default=="yes"){
    stop("RUNNING MODEL FOR MULTIPLE FIELDS CANNOT BE BASED ON DEFAULT VALUES: SET default to NULL")
  }
  
  ka <- ecm
} 



##    ECOMOD - lower limits, actual yield ----

solve_BGRI_ECOMOD_l_act <- function(filename,farmdata,default,farm="single",soil,rotlength,rotprob=NULL,crops,tillages,seedrate,delsowing,Nfert,Pfert,Kfert,bgherbdose,
                                    glyphosatedose,numberofsprays,subsidy="yes",blackgrass,cropprice,cropyield,yieldoption="actual",Nfertprice,
                                    Pfertprice,Kfertprice,seedprice,herbprice,glyphosateprice,machsize,fuelprice,labourwage){  
  
  # THE ORDER OF MODEL INPUTS SUCH AS TILLAGE, SEED RATE N FERTILISER RATES ETC MUST CORRESPOND TO THE ORDER OF CROPS
  # FOR EXAMPLE IF FIRST CROP IS WINTER WHEAT, THEN IN THE NFERT VECTOR, FIRST VALUE MUST CORRESPOND TO WINTER WHEAT N FERTILISER RATE.
  
  # Machine size (machsize) vector MUST be entered in the following order: 1. Tractor szie (kW); 2. Roller size (m);
  # 3. Power harrow size (m); 4. Sprayer tank size (litres); 5. Combine harvester size (kW)
  # For example machsize = c(102,6,4,1400,125)
  
  
  if(farm=="multiple"&is.null(farmdata)){
    stop("RUNNING MODEL FOR MULTIPLE FIELDS CANNOT BE BASED ON SINGLE FIELD/FARM DATA: USE MULTIPLE FIELD/FARM DATA & EQUATE farmdata TO THE DATA")
  }
  
  
  ECOMOD <- function(farm,default,soil,rotlength,rotprob,crops,tillages,seedrate,delsowing,Nfert,Pfert,Kfert,bgherbdose,
                     glyphosatedose,numberofsprays,subsidy,blackgrass,cropprice,cropyield,yieldoption,Nfertprice,
                     Pfertprice,Kfertprice,seedprice,herbprice,glyphosateprice,machsize,fuelprice,labourwage){
    
    # Setting the model to generate default results ******
    
    if(is.null(default)){
      soil <- soil
      rotlength <- rotlength
      crops <- crops
      tillages <- tillages
      seedrate <- seedrate
      delsowing <- delsowing
      Nfert <- Nfert
      Pfert <- Pfert
      Kfert <- Kfert
      bgherbdose <- bgherbdose
      glyphosatedose <- glyphosatedose
      numberofsprays <- numberofsprays
      blackgrass <- blackgrass
      cropprice <- cropprice
      cropyield <- cropyield
      Nfertprice <- Nfertprice
      Pfertprice <- Pfertprice
      Kfertprice <- Kfertprice
      seedprice <- seedprice
      herbprice <- herbprice 
      glyphosateprice <- glyphosateprice
      machsize <- machsize
      fuelprice <- fuelprice
      labourwage <- labourwage
      
    }else if(default=="yes"){ # DEFAULT MODEL INPUT DATA *******************
      soil <- 2.5
      crops <- c("winterwheat","winterwheat","wosr","winterwheat","springbeans","winterwheat")
      tillages <- c("lightcultivation","lightcultivation","deepcultivation","deepcultivation","deepcultivation","deepcultivation")
      seedrate <- c(185,175,3.5,185,334,185)
      delsowing <- c("late","no","late","no","late","late")
      Nfert <- c(180,188,167,190,0,174)
      Pfert <- c(95,95,80,95,70,95)
      Kfert <- c(115,115,70,115,70,115)
      bgherbdose <- c(3.16,7.24,2.54,10.97,10.17,1.7)
      glyphosatedose <- c(2,1.6,4,2.23,4.1,0.98) 
      numberofsprays <- c(3,3,4,4,6,4)
      blackgrass <- c("low","low","low","low","low","low") #**** Needs changing
      cropprice <- c(150,150,335,150,185,150)
      cropyield <- c(9.1,9,3.91,9.8,5.9,10)
      Nfertprice <- 0.78
      Pfertprice <- 0.71
      Kfertprice <- 0.44
      seedprice <- c(0.36,0.36,7.34,0.36,0.38,0.36)
      herbprice <- c(19.5,19.5,19.5,19.5,19.5,19.5) # An average from a farm data
      glyphosateprice <- c(2.43,2.43,2.43,2.43,2.43,2.43) # An average from a farm data
      machsize <- c(102,6,4,1400,125) # Assumed machine sizes
      fuelprice <- 0.625 # ABC Nov 2018, 87th ed
      labourwage <- 10.08 # Nix 2019, 49th ed
      subsidy <- "yes"
    }
    
    #************* Rotation Length Assumption *****************
    lcr <- 6 #rotlength = 6 # Length of vector based on max years of rotation which is 6
    if(length(crops)>lcr){
      warning("Maximum 6-year rotation is assumed (Length of crops vector MUST be 6)")
    }else{
      crops <- crops
    }
    
    if(length(tillages)>6){
      warning("Length of tillage vector MUST be 6")
    }else{
      tillages <- tillages
    }
    
    if(length(machsize)>5){
      warning("Length of machine sizes vector MUST be 5")
      # The machine size must be set in the following order: Tractor size (kW), roller size (m),
      # power harrow size (m), Sprayer size (litres), combine harvester size (kW)
    }else{
      tillages <- tillages
    }
    
    if(length(Nfert)>lcr||length(Pfert)>lcr||length(Kfert)>lcr||length(seedrate)>lcr||length(seedprice)>lcr){
      warning("Maximum 6-year rotation is assumed (Length of crops vector MUST be 6)")
    }else{
      Nfert <- Nfert; Pfert <- Pfert; Kfert <- Kfert; seedrate <- seedrate; seedprice <- seedprice
    }
    
    if(length(bgherbdose)>lcr||length(glyphosatedose)>lcr||length(numberofsprays)>lcr||
       length(cropprice)>lcr||length(cropyield)>lcr||length(seedprice)>lcr||length(herbprice)>lcr){
      warning("Maximum 6-year rotation is assumed (Length of crops vector MUST be 6)")
    }else{
      bgherbdose <- bgherbdose; glyphosatedose <- glyphosatedose; numberofsprays <- numberofsprays;
      cropprice <- cropprice; cropyield <- cropyield; seedprice <- seedprice; herbprice <- herbprice
    }
    
    # Setting soil type
    soi <- c(0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5) # Soil type indices
    if(soil%in%soi==TRUE){
      nsoil <- soil
    }else{
      nsoil <- 2.5 # This sets the soil type to heavy soil (Clay)
      warning("Enter correct soil type: must be between 0.5 and 2.5 at an interval of 0.25. Check that input soil values are formatted as 'numeric' and have 2 dec pl.")
    }
    
    cropBudget <- function(crop,asoil,subsidy,blackgrass,tillage,selfrot,rotpen,yieldoption){ #***********************************************************
      
      # This function estimates gross margin (£/ha) and gross profit (£/ha) for each crop taking into
      # consideration soil type, yield loss due to black-grass infestation,
      # farm operation and machine types, and fuel price.
      
      # Creation of Data files *******
      Input_Output <- c("N Fertiliser","P Fertiliser","K Fertiliser",
                        "N Fertiliser Price","P Fertiliser Price","K Fertiliser Price",
                        "Seed Rate","Seed Price","BG Herbicide Rate","BG Herbicide Price",
                        "Glyphosate Rate", "Glyphosate Price","Number of Sprays",
                        "Sugarbeet Transport","Fertiliser Cost","Seed Cost",
                        "Herbicide Cost","Sundry Cost","Sugarbeet Transport Cost",
                        "Primary Yield","Primary Yield Price",
                        "Secondary Yield","Secondary Yield Price",
                        "Subsidy","Farm Output","Variable Cost",
                        "Gross Margin","Fuel Cost","Labour Cost","Operating Cost",
                        "Gross Profit") # Crop inputs and output parameters
      lcr <- 6 # Equals the number of crops
      cl <- c(rep(0,length(Input_Output)*lcr))
      cd <- matrix(cl, ncol=lcr, byrow = T)
      cols <- c("Input_Output",crops)
      cd1 <- data.frame(Input_Output,cd)
      nfp <- c(rep(Nfertprice,lcr)); pfp <- c(rep(Pfertprice,lcr)); kfp <- c(rep(Kfertprice,lcr))
      
      inp <- rbind(Nfert,Pfert,Kfert,nfp,pfp,kfp,seedrate,seedprice,bgherbdose,herbprice,glyphosatedose,
                   glyphosateprice,numberofsprays) # Putting input data together to create crop data
      
      cd1[seq(1,13),seq(2,7)] <- inp
      cd1[20,seq(2,7)] <- cropyield
      cd1[21,seq(2,7)] <- cropprice
      colnames(cd1) <- cols
      crd <- fi1 <- cd1
      
      # Updated 07/02/2023. Assumed sundry costs are informed by Nix (2019) and ABC (2019).
      # Sundry costs include the average per hectare cost of herbicides NOT targeting BG, from the BGRI data, 
      # as well as cost of other chemicals such as fungicide, insecticides, growth regulators etc.,
      # plus marketing, agronomy costs, in-store chemicals. Data are from 'BGRI_ECOMOD_Chem & Sundry Costs_2023-02-07.xlsx".
      #snwwt <- 146; snswt <- 95; snwba <- 108; snsba <- 81; snwbe <- 101; snsbe <- 89; snwpo <- 1200; snwos <- 110; snsbt <- 409; snset <- 2;
      #snsos <- 67; sndpe <- 156; snwln <- 75; snsln <- 73; snwoa <- 80; snsoa <- 89; 
      snwwt <- 182; snswt <- 100; snwba <- 158; snsba <- 151; snwbe <- 91; snsbe <- 81; snwpo <- 2215; snwos <- 192; snsbt <- 488; snset <- 2;
      snsos <- 140; sndpe <- 155; snwln <- 88; snsln <- 53; snwoa <- 148; snsoa <- 109;  
      
      
      
      # Yield estimates based on soil type and N fertiliser amounts (Response functions obtained from SAFMOD)
      wwt <- round(((11.841-(9.211*(0.9907^(fi1$winterwheat[1])))-(0.0075*(fi1$winterwheat[1])))*(0.743+0.1714*(nsoil))*0.947),2)
      swt <- round((5.885-(2.893*(0.984^(fi1$springwheat[1]))))*(0.73+0.18*(nsoil)),2)
      wba <- round((((12.967-(10.029*(0.993^(fi1$winterbarley[1])))-(0.0147*(fi1$winterbarley[1])))*(0.76+0.16*(nsoil)))*0.89),2)
      sba <- round(((19.98-(18.164*(0.9952^(fi1$springbarley[1])))-(0.0364*(fi1$springbarley[1])))*(0.887+0.075*(nsoil))*1.02),2)
      wbe <- round((((0.95+1.3625*(nsoil))*1.1)*1.05),2)
      sbe <- round((((0.7+1.25*(nsoil))*1.05)*1.2),2)
      wpo <- round((44.507-(29.135*(0.992^fi1$warepotatoes[1])))*1.16,2)
      wos <- round((((3.35+(-0.623*(0.010^(fi1$wosr[1])))-0.000324*(fi1$wosr[1]))*(0.655+0.23*(nsoil)))*0.87),2)
      sbt <- round(((54.543-(0.05*37.82*(0.984^fi1$sugarbeet[1])))*1.30),2)
      #set <- 0
      sos <- round((2.317-(1.139*(0.984^(fi1$sosr[1])))),2)
      wln <- round(0.75+0.45*1.5*(nsoil),2)
      sln <- round(0.75+0.45*(nsoil)*0.95,2)
      dpe <- round((2.48+3.475*(nsoil)-(1.2875*(nsoil)^2)),2)
      woa <- round((((12.967-(10.029*(0.993^(fi1$winteroats[1])))-(0.0147*(fi1$winteroats[1])))*(0.76+0.16*(nsoil)))*0.89),2)
      soa <- round(((19.98-(18.164*(0.9952^(fi1$springoats[1])))-(0.0364*(fi1$springoats[1])))*(0.887+0.075*(nsoil))*1.02),2)
      
      # ========= Yield Penalty | Black-grass ========= 
      # ************ Winter wheat - black-grass infestation assumption ************
      # These yield penalties (abs/low BG density = 0, med=0, high=7.45, vhigh=25.6) are from Varah et al (2019) Nature Sustainability, based on BGRI data.
      # If better data are obtained, we suggest updating these penalties.
      
      # IF ACTUAL YIELD DATA ARE USED YIELD PENALTIES DUE TO BLACK-GRASS ARE NOT TAKEN INTO ACCOUNT
      
      bgf <- function(infestation){
        if(infestation=="low"){
          yieldloss <- 0; wwht1 <- wwt;
          yls <- 0
        }else if(infestation=="medium"){
          yieldloss <- 0; wwht1 <- wwt;
          yls <- 0
        }else if(infestation=="high"){
          yieldloss <- 0; wwht1 <- wwt;
          yls <- 0
        }else if(infestation=="veryhigh"){
          yieldloss <- 13; wwht1 <- round(wwt*(1-(13/100)),2)
          yls <- round((13/100)*wwht1,2)
        }else{
          yieldloss <- 0
          wwht1 <- wwt
          yls <- 0 
        }
        c(wwht1,yls) 
      }
      
      # ========= Yield Penalty | Sowing Date =========
      #************ Delayed Sowing ************
      # Delayed sowing has been modelled to take into account the degree of delay.
      # For example the sowing of winter wheat is optimal in late September and three classes or 
      # options for delayed sowing are defined for October (late), November (later) and December (latest)
      # IF ACTUAL YIELD DATA ARE USED YIELD PENALTIES DUE TO DELAYED SOWING ARE NOT TAKEN INTO ACCOUNT *********
      
      delsow <- function(sowtime){
        if(sowtime=="late"){
          ft1 <- 7/100; ft2 <- 4/100; ft3 <- 3/100; ft4 <- 2/100; ft5 <- 0/100
          ft6 <- 5/100; ft7 <- 10/100; ft8 <- 1/100; ft9 <- 1/100; ft10 <- 0/100; ft11 <- 0/100; ft12 <- 0/100; ft13 <- 0/100
          ds <- c(ft1,ft2,ft3,ft4,ft5,ft6,ft7,ft8,ft9,ft10,ft11,ft12,ft13)
        }else if(sowtime=="later"){
          ft1 <- 12/100; ft2 <- 9/100; ft3 <- 12/100; ft4 <- 5/100; ft5 <- 0/100
          ft6 <- 6/100; ft7 <- 11/100; ft8 <- 5/100; ft9 <- 5/100; ft10 <- 0/100; ft11 <- 0/100; ft12 <- 0/100; ft13 <- 3/100
          ds <- c(ft1,ft2,ft3,ft4,ft5,ft6,ft7,ft8,ft9,ft10,ft11,ft12,ft13)
        }else if(sowtime=="latest"){
          ft1 <- 15/100; ft2 <- 17/100; ft3 <- 18/100; ft4 <- 15/100; ft5 <- 0/100
          ft6 <- 6/100; ft7 <- 11/100; ft8 <- 5/100; ft9 <- 11/100; ft10 <- 0/100; ft11 <- 0/100; ft12 <- 0/100; ft13 <- 6/100
          ds <- c(ft1,ft2,ft3,ft4,ft5,ft6,ft7,ft8,ft9,ft10,ft11,ft12,ft13)
        }else if(sowtime=="no"){
          ft1 <- ft2 <- ft3 <- ft4 <- ft5 <- ft6 <- ft7 <- ft8 <- ft9 <- ft10 <- ft11 <- ft12 <- ft13 <- 0
          ds <- c(ft1,ft2,ft3,ft4,ft5,ft6,ft7,ft8,ft9,ft10,ft11,ft12,ft13)
        }else{
          ft1 <- ft2 <- ft3 <- ft4 <- ft5 <- ft6 <- ft7 <- ft8 <- ft9 <- ft10 <- ft11 <- ft12 <- ft13 <- 0
          ds <- c(ft1,ft2,ft3,ft4,ft5,ft6,ft7,ft8,ft9,ft10,ft11,ft12,ft13)
          warning("Check if delayed sowing option is spelt correctly; option MUST be: no, late, later or latest")
        }
        ds
      }
      
      seccost <- 65 # Secondary yield (straw) price assumed for cereal crops (wheat and barley crops) ********************
      c1 <- 18; c2 <- 19; c3 <- 20; c4 <- 21; c5 <- 22; c6 <- 23; c7 <- 24; c8 <- 25; c9 <- 26;# === row indices
      c10 <- 27; c11 <- 28; c12 <- 29; c13 <- 30; c14 <- 31; 
      
      h1 <- 10; h2 <- 11; h3 <- 12; h4 <- 13; h5 <- 14; h6 <- 15;
      h7 <- 16; h8 <- 17; 
      
      # ********** WW *********
      if(crops[1]=="winterwheat"){
        crd[c1,2] <- snwwt # Sundry cost
        crd[c6,2] <- seccost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2] 
          crd[c5,2] <- crd[c3,2]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- bgf(blackgrass[1])[1]-round(delsow(delsowing[1])[1]*bgf(blackgrass[1])[1],2) # wwt
          crd[c5,2] <- crd[c3,2]*0.5
        }
      }
      if(crops[2]=="winterwheat"){
        crd[c1,3] <- snwwt # Sundry cost
        crd[c6,3] <- seccost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- bgf(blackgrass[2])[1]-round(delsow(delsowing[2])[1]*bgf(blackgrass[2])[1],2) # wwt
          crd[c5,3] <- crd[c3,3]*0.5
        }
      }
      if(crops[3]=="winterwheat"){
        crd[c1,4] <- snwwt # Sundry cost
        crd[c6,4] <- seccost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4] 
          crd[c5,4] <- crd[c3,4]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- bgf(blackgrass[3])[1]-round(delsow(delsowing[3])[1]*bgf(blackgrass[3])[1],2) # wwt
          crd[c5,4] <- crd[c3,4]*0.5
        }
      }
      if(crops[4]=="winterwheat"){
        crd[c1,5] <- snwwt # Sundry cost
        crd[c6,5] <- seccost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- bgf(blackgrass[4])[1]-round(delsow(delsowing[4])[1]*bgf(blackgrass[4])[1],2) # wwt
          crd[c5,5] <- crd[c3,5]*0.5
        }
      } 
      if(crops[5]=="winterwheat"){
        crd[c1,6] <- snwwt # Sundry cost
        crd[c6,6] <- seccost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- bgf(blackgrass[5])[1]-round(delsow(delsowing[5])[1]*bgf(blackgrass[5])[1],2) # wwt
          crd[c5,6] <- crd[c3,6]*0.5
        }
      }
      if(crops[6]=="winterwheat"){
        crd[c1,7] <- snwwt # Sundry cost
        crd[c6,7] <- seccost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- bgf(blackgrass[6])[1]-round(delsow(delsowing[6])[1]*bgf(blackgrass[6])[1],2) # wwt
          crd[c5,7] <- crd[c3,7]*0.5
        }
      }
      
      #********** SPR WHT *********
      if(crops[1]=="springwheat"){
        crd[c1,2] <- snswt # Sundry cost
        crd[c6,2] <- seccost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- swt-round(delsow(delsowing[1])[2]*swt,2)
          crd[c5,2] <- crd[c3,2]*0.5
        }
      }
      if(crops[2]=="springwheat"){
        crd[c1,3] <- snswt # Sundry cost
        crd[c6,3] <- seccost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- swt-round(delsow(delsowing[2])[2]*swt,2)
          crd[c5,3] <- crd[c3,3]*0.5
        }
      }
      if(crops[3]=="springwheat"){
        crd[c1,4] <- snswt # Sundry cost
        crd[c6,4] <- seccost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- swt-round(delsow(delsowing[3])[2]*swt,2)
          crd[c5,4] <- crd[c3,4]*0.5
        }
      }
      if(crops[4]=="springwheat"){
        crd[c1,5] <- snswt # Sundry cost
        crd[c6,5] <- seccost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- swt-round(delsow(delsowing[4])[2]*swt,2)
          crd[c5,5] <- crd[c3,5]*0.5
        }
      } 
      if(crops[5]=="springwheat"){
        crd[c1,6] <- snswt # Sundry cost
        crd[c6,6] <- seccost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- swt-round(delsow(delsowing[5])[2]*swt,2)
          crd[c5,6] <- crd[c3,6]*0.5
        }
      }
      if(crops[6]=="springwheat"){
        crd[c1,7] <- snswt # Sundry cost
        crd[c6,7] <- seccost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- swt-round(delsow(delsowing[6])[2]*swt,2)
          crd[c5,7] <- crd[c3,7]*0.5
        }
      }
      
      #********** WBAR *********
      if(crops[1]=="winterbarley"||crops[1]=="winteroats"){
        crd[c1,2] <- snwba # Sundry cost
        crd[c6,2] <- seccost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- wba-round(delsow(delsowing[1])[3]*wba,2)
          crd[c5,2] <- crd[c3,2]*0.5
        }
      }
      if(crops[2]=="winterbarley"||crops[2]=="winteroats"){
        crd[c1,3] <- snwba # Sundry cost
        crd[c6,3] <- seccost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- wba-round(delsow(delsowing[2])[3]*wba,2)
          crd[c5,3] <- crd[c3,3]*0.5
        }
      }
      if(crops[3]=="winterbarley"||crops[3]=="winteroats"){
        crd[c1,4] <- snwba # Sundry cost
        crd[c6,4] <- seccost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- wba-round(delsow(delsowing[3])[3]*wba,2)
          crd[c5,4] <- crd[c3,4]*0.5
        }
      }
      if(crops[4]=="winterbarley"||crops[4]=="winteroats"){
        crd[c1,5] <- snwba # Sundry cost
        crd[c6,5] <- seccost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- wba-round(delsow(delsowing[4])[3]*wba,2)
          crd[c5,5] <- crd[c3,5]*0.5
        }
      } 
      if(crops[5]=="winterbarley"||crops[5]=="winteroats"){
        crd[c1,6] <- snwba # Sundry cost
        crd[c6,6] <- seccost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- wba-round(delsow(delsowing[5])[3]*wba,2)
          crd[c5,6] <- crd[c3,6]*0.5
        }
      }
      if(crops[6]=="winterbarley"||crops[6]=="winteroats"){
        crd[c1,7] <- snwba # Sundry cost
        crd[c6,7] <- seccost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- wba-round(delsow(delsowing[6])[3]*wba,2)
          crd[c5,7] <- crd[c3,7]*0.5
        }
      }
      #********** SPR BAR *********
      if(crops[1]=="springbarley"||crops[1]=="springoats"){
        crd[c1,2] <- snsba # Sundry cost
        crd[c6,2] <- seccost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- sba-round(delsow(delsowing[1])[4]*sba,2)
          crd[c5,2] <- crd[c3,2]*0.5
        }
      }
      if(crops[2]=="springbarley"||crops[2]=="springoats"){
        crd[c1,3] <- snsba # Sundry cost
        crd[c6,3] <- seccost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- sba-round(delsow(delsowing[2])[4]*sba,2)
          crd[c5,3] <- crd[c3,3]*0.5
        }
      }
      if(crops[3]=="springbarley"||crops[3]=="springoats"){
        crd[c1,4] <- snsba # Sundry cost
        crd[c6,4] <- seccost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- sba-round(delsow(delsowing[3])[4]*sba,2)
          crd[c5,4] <- crd[c3,4]*0.5
        }
      }
      if(crops[4]=="springbarley"||crops[4]=="springoats"){
        crd[c1,5] <- snsba # Sundry cost
        crd[c6,5] <- seccost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- sba-round(delsow(delsowing[4])[4]*sba,2)
          crd[c5,5] <- crd[c3,5]*0.5
        }
      } 
      if(crops[5]=="springbarley"||crops[5]=="springoats"){
        crd[c1,6] <- snsba # Sundry cost
        crd[c6,6] <- seccost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- sba-round(delsow(delsowing[5])[4]*sba,2)
          crd[c5,6] <- crd[c3,6]*0.5
        }
      }
      if(crops[6]=="springbarley"||crops[6]=="springoats"){
        crd[c1,7] <- snsba # Sundry cost
        crd[c6,7] <- seccost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- sba-round(delsow(delsowing[6])[4]*sba,2)
          crd[c5,7] <- crd[c3,7]*0.5
        }
      }
      
      #********** WBEA *********
      if(crops[1]=="winterbeans"){
        crd[c1,2] <- snwbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- wbe-round(delsow(delsowing[1])[5]*wbe,2)
          crd[c5,2] <- crd[c3,2]*0
        }
      }
      if(crops[2]=="winterbeans"){
        crd[c1,3] <- snwbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- wbe-round(delsow(delsowing[2])[5]*wbe,2)
          crd[c5,3] <- crd[c3,3]*0
        }
      }
      if(crops[3]=="winterbeans"){
        crd[c1,4] <- snwbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- wbe-round(delsow(delsowing[3])[5]*wbe,2)
          crd[c5,4] <- crd[c3,4]*0
        }
      }
      if(crops[4]=="winterbeans"){
        crd[c1,5] <- snwbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- wbe-round(delsow(delsowing[4])[5]*wbe,2)
          crd[c5,5] <- crd[c3,5]*0
        }
      } 
      if(crops[5]=="winterbeans"){
        crd[c1,6] <- snwbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- wbe-round(delsow(delsowing[5])[5]*wbe,2)
          crd[c5,6] <- crd[c3,6]*0
        }
      }
      if(crops[6]=="winterbeans"){
        crd[c1,7] <- snwbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- wbe-round(delsow(delsowing[6])[5]*wbe,2)
          crd[c5,7] <- crd[c3,7]*0
        }
      }
      
      #********** SPR BEA *********
      if(crops[1]=="springbeans"){
        crd[c1,2] <- snsbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- sbe-round(delsow(delsowing[1])[6]*sbe,2)
          crd[c5,2] <- crd[c3,2]*0
        }
      }
      if(crops[2]=="springbeans"){
        crd[c1,3] <- snsbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- sbe-round(delsow(delsowing[2])[6]*sbe,2)
          crd[c5,3] <- crd[c3,3]*0
        }
      }
      if(crops[3]=="springbeans"){
        crd[c1,4] <- snsbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- sbe-round(delsow(delsowing[3])[6]*sbe,2)
          crd[c5,4] <- crd[c3,4]*0
        }
      }
      if(crops[4]=="springbeans"){
        crd[c1,5] <- snsbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- sbe-round(delsow(delsowing[4])[6]*sbe,2)
          crd[c5,5] <- crd[c3,5]*0
        }
      } 
      if(crops[5]=="springbeans"){
        crd[c1,6] <- snsbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- sbe-round(delsow(delsowing[5])[6]*sbe,2)
          crd[c5,6] <- crd[c3,6]*0
        }
      }
      if(crops[6]=="springbeans"){
        crd[c1,7] <- snsbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- sbe-round(delsow(delsowing[6])[6]*sbe,2)
          crd[c5,7] <- crd[c3,7]*0
        }
      }
      
      #********** WPOTS *********
      if(crops[1]=="warepotatoes"){
        crd[c1,2] <- snwpo # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- wpo-round(delsow(delsowing[1])[7]*wpo,2)
          crd[c5,2] <- crd[c3,2]*0
        }
      }
      if(crops[2]=="warepotatoes"){
        crd[c1,3] <- snwpo # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- wpo-round(delsow(delsowing[2])[7]*wpo,2)
          crd[c5,3] <- crd[c3,3]*0
        }
      }
      if(crops[3]=="warepotatoes"){
        crd[c1,4] <- snwpo # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- wpo-round(delsow(delsowing[3])[7]*wpo,2)
          crd[c5,4] <- crd[c3,4]*0
        }
      }
      if(crops[4]=="warepotatoes"){
        crd[c1,5] <- snwpo # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- wpo-round(delsow(delsowing[4])[7]*wpo,2)
          crd[c5,5] <- crd[c3,5]*0
        }
      } 
      if(crops[5]=="warepotatoes"){
        crd[c1,6] <- snwpo # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- wpo-round(delsow(delsowing[5])[7]*wpo,2)
          crd[c5,6] <- crd[c3,6]*0
        }
      }
      if(crops[6]=="warepotatoes"){
        crd[c1,7] <- snwpo # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- wpo-round(delsow(delsowing[6])[7]*wpo,2)
          crd[c5,7] <- crd[c3,7]*0
        }
      }
      
      #********** WOSR *********
      if(crops[1]=="wosr"){
        crd[c1,2] <- snwos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- wos-round(delsow(delsowing[1])[8]*wos,2)
          crd[c5,2] <- crd[c3,2]*0
        }
      }
      if(crops[2]=="wosr"){
        crd[c1,3] <- snwos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- wos-round(delsow(delsowing[2])[8]*wos,2)
          crd[c5,3] <- crd[c3,3]*0
        }
      }
      if(crops[3]=="wosr"){
        crd[c1,4] <- snwos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- wos-round(delsow(delsowing[3])[8]*wos,2)
          crd[c5,4] <- crd[c3,4]*0
        }
      }
      if(crops[4]=="wosr"){
        crd[c1,5] <- snwos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- wos-round(delsow(delsowing[4])[8]*wos,2)
          crd[c5,5] <- crd[c3,5]*0
        }
      } 
      if(crops[5]=="wosr"){
        crd[c1,6] <- snwos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- wos-round(delsow(delsowing[5])[8]*wos,2)
          crd[c5,6] <- crd[c3,6]*0
        }
      }
      if(crops[6]=="wosr"){
        crd[c1,7] <- snwos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- wos-round(delsow(delsowing[6])[8]*wos,2)
          crd[c5,7] <- crd[c3,7]*0
        }
      }
      
      #********** SBEET *********
      if(crops[1]=="sugarbeet"){
        crd[h5,2] <- 5 # Cost of transporting sugarbeet @ £5/t
        crd[c1,2] <- snsbt # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- sbt-round(delsow(delsowing[1])[9]*sbt,2)
          crd[c5,2] <- crd[c3,2]*0
        }
      }
      if(crops[2]=="sugarbeet"){
        crd[h5,3] <- 5 # Cost of transporting sugarbeet @ £5/t
        crd[c1,3] <- snsbt # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- sbt-round(delsow(delsowing[2])[9]*sbt,2)
          crd[c5,3] <- crd[c3,3]*0
        }
      }
      if(crops[3]=="sugarbeet"){
        crd[h5,4] <- 5 # Cost of transporting sugarbeet @ £5/t
        crd[c1,4] <- snsbt # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- sbt-round(delsow(delsowing[3])[9]*sbt,2)
          crd[c5,4] <- crd[c3,4]*0
        }
      }
      if(crops[4]=="sugarbeet"){
        crd[h5,5] <- 5 # Cost of transporting sugarbeet @ £5/t
        crd[c1,5] <- snsbt # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- sbt-round(delsow(delsowing[4])[9]*sbt,2)
          crd[c5,5] <- crd[c3,5]*0
        }
      } 
      if(crops[5]=="sugarbeet"){
        crd[h5,6] <- 5 # Cost of transporting sugarbeet @ £5/t
        crd[c1,6] <- snsbt # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- sbt-round(delsow(delsowing[5])[9]*sbt,2)
          crd[c5,6] <- crd[c3,6]*0
        }
      }
      if(crops[6]=="sugarbeet"){
        crd[h5,7] <- 5 # Cost of transporting sugarbeet @ £5/t
        crd[c1,7] <- snsbt # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- sbt-round(delsow(delsowing[6])[9]*sbt,2)
          crd[c5,7] <- crd[c3,7]*0
        }
      }
      
      #********** SETA *********
      if(crops[1]=="setaside"){
        crd[c1,2] <- snset # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- 0
          crd[c5,2] <- 0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- 0
          crd[c5,2] <- 0
        }
      }
      if(crops[2]=="setaside"){
        crd[c1,3] <- snset # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- 0
          crd[c5,3] <- 0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- 0
          crd[c5,3] <- 0
        }
      }
      if(crops[3]=="setaside"){
        crd[c1,4] <- snset # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- 0
          crd[c5,4] <- 0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- 0
          crd[c5,4] <- 0
        }
      }
      if(crops[4]=="setaside"){
        crd[c1,5] <- snset # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- 0
          crd[c5,5] <- 0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- 0
          crd[c5,5] <- 0
        }
      } 
      if(crops[5]=="setaside"){
        crd[c1,6] <- snset # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- 0
          crd[c5,6] <- 0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- 0
          crd[c5,6] <- 0
        }
      }
      if(crops[6]=="setaside"){
        crd[c1,7] <- snset # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- 0
          crd[c5,7] <- 0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- 0
          crd[c5,7] <- 0
        }
      }
      
      #********** SOSR *********
      if(crops[1]=="sosr"){
        crd[c1,2] <- snsos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- sos-round(delsow(delsowing[1])[10]*sos,2)
          crd[c5,2] <- crd[c3,2]*0
        }
      }
      if(crops[2]=="sosr"){
        crd[c1,3] <- snsos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- sos-round(delsow(delsowing[2])[10]*sos,2)
          crd[c5,3] <- crd[c3,3]*0
        }
      }
      if(crops[3]=="sosr"){
        crd[c1,4] <- snsos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- sos-round(delsow(delsowing[3])[10]*sos,2)
          crd[c5,4] <- crd[c3,4]*0
        }
      }
      if(crops[4]=="sosr"){
        crd[c1,5] <- snsos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- sos-round(delsow(delsowing[4])[10]*sos,2)
          crd[c5,5] <- crd[c3,5]*0
        }
      } 
      if(crops[5]=="sosr"){
        crd[c1,6] <- snsos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- sos-round(delsow(delsowing[5])[10]*sos,2)
          crd[c5,6] <- crd[c3,6]*0
        }
      }
      if(crops[6]=="sosr"){
        crd[c1,7] <- snsos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- sos-round(delsow(delsowing[6])[10]*sos,2)
          crd[c5,7] <- crd[c3,7]*0
        }
      }
      
      #********** WLIN *********
      if(crops[1]=="winterlinseed"){
        crd[c1,2] <- snwln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- wln-round(delsow(delsowing[1])[11]*wln,2)
          crd[c5,2] <- crd[c3,2]*0
        }
      }
      if(crops[2]=="winterlinseed"){
        crd[c1,3] <- snwln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- wln-round(delsow(delsowing[2])[11]*wln,2)
          crd[c5,3] <- crd[c3,3]*0
        }
      }
      if(crops[3]=="winterlinseed"){
        crd[c1,4] <- snwln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- wln-round(delsow(delsowing[3])[11]*wln,2)
          crd[c5,4] <- crd[c3,4]*0
        }
      }
      if(crops[4]=="winterlinseed"){
        crd[c1,5] <- snwln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- wln-round(delsow(delsowing[4])[11]*wln,2)
          crd[c5,5] <- crd[c3,5]*0
        }
      } 
      if(crops[5]=="winterlinseed"){
        crd[c1,6] <- snwln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- wln-round(delsow(delsowing[5])[11]*wln,2)
          crd[c5,6] <- crd[c3,6]*0
        }
      }
      if(crops[6]=="winterlinseed"){
        crd[c1,7] <- snwln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- wln-round(delsow(delsowing[6])[11]*wln,2)
          crd[c5,7] <- crd[c3,7]*0
        }
      }
      
      #********** SLIN *********
      if(crops[1]=="springlinseed"){
        crd[c1,2] <- snsln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- sln-round(delsow(delsowing[1])[12]*sln,2)
          crd[c5,2] <- crd[c3,2]*0
        }
      }
      if(crops[2]=="springlinseed"){
        crd[c1,3] <- snsln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- sln-round(delsow(delsowing[2])[12]*sln,2)
          crd[c5,3] <- crd[c3,3]*0
        }
      }
      if(crops[3]=="springlinseed"){
        crd[c1,4] <- snsln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- sln-round(delsow(delsowing[3])[12]*sln,2)
          crd[c5,4] <- crd[c3,4]*0
        }
      }
      if(crops[4]=="springlinseed"){
        crd[c1,5] <- snsln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- sln-round(delsow(delsowing[4])[12]*sln,2)
          crd[c5,5] <- crd[c3,5]*0
        }
      } 
      if(crops[5]=="springlinseed"){
        crd[c1,6] <- snsln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- sln-round(delsow(delsowing[5])[12]*sln,2)
          crd[c5,6] <- crd[c3,6]*0
        }
      }
      if(crops[6]=="springlinseed"){
        crd[c1,7] <- snsln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- sln-round(delsow(delsowing[6])[12]*sln,2)
          crd[c5,7] <- crd[c3,7]*0
        }
      }
      
      #********** PEAS *********
      if(crops[1]=="driedpeas"){
        crd[c1,2] <- sndpe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- dpe-round(delsow(delsowing[1])[13]*dpe,2)
          crd[c5,2] <- crd[c3,2]*0
        }
      }
      if(crops[2]=="driedpeas"){
        crd[c1,3] <- sndpe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- dpe-round(delsow(delsowing[2])[13]*dpe,2)
          crd[c5,3] <- crd[c3,3]*0
        }
      }
      if(crops[3]=="driedpeas"){
        crd[c1,4] <- sndpe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- dpe-round(delsow(delsowing[3])[13]*dpe,2)
          crd[c5,4] <- crd[c3,4]*0
        }
      }
      if(crops[4]=="driedpeas"){
        crd[c1,5] <- sndpe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- dpe-round(delsow(delsowing[4])[13]*dpe,2)
          crd[c5,5] <- crd[c3,5]*0
        }
      } 
      if(crops[5]=="driedpeas"){
        crd[c1,6] <- sndpe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- dpe-round(delsow(delsowing[5])[13]*dpe,2)
          crd[c5,6] <- crd[c3,6]*0
        }
      }
      if(crops[6]=="driedpeas"){
        crd[c1,7] <- sndpe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- dpe-round(delsow(delsowing[6])[13]*dpe,2)
          crd[c5,7] <- crd[c3,7]*0
        }
      }
      
      #**************************************************************************************************************
      # ========= Subsidy amount =========
      crd[c7,seq(2,7)] <- 223 # SUBSIDY AMOUNT ASSUMED @ 223/ha (https://www.gov.uk/guidance/bps-2019)
      cropData <- crd
      
      Operation=c("Spread P/K Fertiliser WWHT","Plough WWHT","Sowing WWHT","Roll WWHT",
                  "Apply N Fertiliser WWHT","Spray WWHT","Combine Harvesting WWHT","Bale WWHT",
                  
                  "Spread P/K Fertiliser SWHT","Plough SWHT","Sowing SWHT","Roll SWHT",
                  "Apply N Fertiliser SWHT","Spray SWHT","Combine Harvesting SWHT","Bale SWHT",
                  
                  "Spread P/K Fertiliser WBAR","Plough WBAR","Sowing WBAR","Roll WBAR",
                  "Apply N Fertiliser WBAR","Spray WBAR","Combine Harvesting WBAR","Bale WBAR",
                  
                  "Spread P/K Fertiliser SBAR","Plough SBAR","Sowing SBAR","Roll SBAR",
                  "Spray SBAR","Apply N Fertiliser SBAR","Combine Harvesting SBAR","Bale SBAR",
                  
                  "Spread P/K Fertiliser WBEA","Plough WBEA","Sowing WBEA","Roll WBEA","Spray WBEA",
                  "Combine Harvesting WBEA",
                  
                  "Spread P/K Fertiliser SBEA","Plough SBEA","Sowing SBEA","Spray SBEA",
                  "Combine Harvesting SBEA",
                  
                  "Plough WPOT","Harrow WPOT","Sowing WPOT","Ridge WPOT","Spray WPOT","Spread P/K Fertiliser WPOT",
                  "Apply N Fertiliser WPOT","Hoeing WPOT","Harvest WPOT",
                  
                  "Spread P/K Fertiliser WOSR","Plough WOSR","Sowing WOSR",
                  "Spray WOSR","Apply N Fertiliser WOSR","Combine Harvesting WOSR",
                  
                  "Plough SBEE","Harrow SBEE","Sowing SBEE","Spray SBEE","Spread P/K Fertiliser SBEE",
                  "Apply N Fertiliser SBEE","Hoeing SBEE","Harvest SBEE",
                  
                  "Plough SETA","Spray SETA",
                  
                  "Spread P/K Fertiliser SOSR","Plough SOSR","Sowing SOSR",
                  "Spray SOSR","Apply N Fertiliser SOSR","Combine Harvesting SOSR",
                  
                  "Spread P/K Fertiliser WLIN","Plough WLIN","Sowing WLIN",
                  "Spray WLIN","Apply N Fertiliser WLIN","Combine Harvesting WLIN",
                  
                  "Spread P/K Fertiliser SLIN","Plough SLIN","Sowing SLIN",
                  "Spray SLIN","Apply N Fertiliser SLIN","Combine Harvesting SLIN",
                  
                  "Spread P/K Fertiliser DPEA","Plough DPEA","Sowing DPEA","Roll DPEA",
                  "Spray DPEA","Combine Harvesting DPEA")
      
      lgt <- length(Operation) # Total number of operation for all crops              
      Workrates <- data.frame(Operations=Operation,   
                              
                              Tractor=c(rep(0,lgt)), Labour=c(rep(0,lgt)), 
                              Power_harrow=c(rep(0,lgt)), Sprayer=c(rep(0,lgt)),
                              Combine_harvester=c(rep(0,lgt)), Baler=c(rep(0,lgt)), 
                              SBEE_harvester=c(rep(0,lgt)),WPOT_harvester=c(rep(0,lgt)),Fuel_cost=c(rep(0,lgt)), 
                              Labour_cost=c(rep(0,lgt)), Operation_cost=c(rep(0,lgt))
      )
      
      Machines <- data.frame(
        Machine=c("Tractor","Rolls","Power harrow","Sprayer","Combine harvetser",
                  "Baler","Sugarbeet harvester"),
        
        Size_Unit=c("kW","m","m","litres","kW","na","na"),
        
        Size=c(machsize,0,0),
        
        Machine_price=c(rep(0,7)),
        
        Depreciation_rate=c(22,14,14,18,18,11,18),
        
        Repair_cost_rate=c(12,5,5,6.8,5.8,5.8,5), Replacement_year=c(5,10,8,7,7,7,7),
        
        Annual_hours=c(2500,rep(300,6))
      )
      
      modData <- list(cropData, Workrates, Machines) # Data files
      
      #************************************************************************************************
      #************************************************************************************************
      
      
      fi1 <- modData[[1]] # Crop Data
      
      # Yield penalties with respect to continuous cropping (self rotation) and sub-optimal rotation
      sf <- selfrot/100 # Self rotation penalty
      rt <- rotpen/100 # Rotational penalty
      
      #***********
      cy1 <- fi1[c3,2]; cy2 <- fi1[c3,3]; cy3 <- fi1[c3,4]; cy4 <- fi1[c3,5]; cy5 <- fi1[c3,6]; cy6 <- fi1[c3,7]
      if(crops[1]=="winterwheat"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0.5
      }else if(crops[1]=="springwheat"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0.5
      }else if(crops[1]=="winterbarley"||crops[1]=="winteroats"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0.5
      }else if(crops[1]=="springbarley"||crops[1]=="springoats"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0.5
      }else if(crops[1]=="winterbeans"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0
      }else if(crops[1]=="springbeans"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0
      }else if(crops[1]=="warepotatoes"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0
      }else if(crops[1]=="wosr"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0
      }else if(crops[1]=="sugarbeet"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0
      }else if(crops[1]=="setaside"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0
      }else if(crops[1]=="sosr"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0
      }else if(crops[1]=="winterlinseed"||crops[1]=="springlinseed"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0
      }else if(crops[1]=="driedpeas"||crops[1]=="peas"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0
      }
      
      #***********
      if(crops[2]=="winterwheat"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0.5
      }else if(crops[2]=="springwheat"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0.5
      }else if(crops[2]=="winterbarley"||crops[2]=="winteroats"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0.5
      }else if(crops[2]=="springbarley"||crops[2]=="springoats"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0.5
      }else if(crops[2]=="winterbeans"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0
      }else if(crops[2]=="springbeans"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0
      }else if(crops[2]=="warepotatoes"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0
      }else if(crops[2]=="wosr"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0
      }else if(crops[2]=="sugarbeet"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0
      }else if(crops[2]=="setaside"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0
      }else if(crops[2]=="sosr"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0
      }else if(crops[2]=="winterlinseed"||crops[2]=="springlinseed"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0
      }else if(crops[2]=="driedpeas"||crops[2]=="peas"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0
      }
      
      #***********
      if(crops[3]=="winterwheat"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0.5
      }else if(crops[3]=="springwheat"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0.5
      }else if(crops[3]=="winterbarley"||crops[3]=="winteroats"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0.5
      }else if(crops[3]=="springbarley"||crops[3]=="springoats"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0.5
      }else if(crops[3]=="winterbeans"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0
      }else if(crops[3]=="springbeans"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0
      }else if(crops[3]=="warepotatoes"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0
      }else if(crops[3]=="wosr"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0
      }else if(crops[3]=="sugarbeet"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0
      }else if(crops[3]=="setaside"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0
      }else if(crops[3]=="sosr"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0
      }else if(crops[3]=="winterlinseed"||crops[3]=="springlinseed"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0
      }else if(crops[3]=="driedpeas"||crops[3]=="peas"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0
      }
      
      #***********
      if(crops[4]=="winterwheat"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0.5
      }else if(crops[4]=="springwheat"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0.5
      }else if(crops[4]=="winterbarley"||crops[4]=="winteroats"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0.5
      }else if(crops[4]=="springbarley"||crops[4]=="springoats"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0.5
      }else if(crops[4]=="winterbeans"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0
      }else if(crops[4]=="springbeans"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0
      }else if(crops[4]=="warepotatoes"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0
      }else if(crops[4]=="wosr"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0
      }else if(crops[4]=="sugarbeet"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0
      }else if(crops[4]=="setaside"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0
      }else if(crops[4]=="sosr"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0
      }else if(crops[4]=="winterlinseed"||crops[4]=="springlinseed"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0
      }else if(crops[4]=="driedpeas"||crops[4]=="peas"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0
      }
      
      #***********
      if(crops[5]=="winterwheat"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0.5
      }else if(crops[5]=="springwheat"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0.5
      }else if(crops[5]=="winterbarley"||crops[5]=="winteroats"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0.5
      }else if(crops[5]=="springbarley"||crops[5]=="springoats"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0.5
      }else if(crops[5]=="winterbeans"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0
      }else if(crops[5]=="springbeans"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0
      }else if(crops[5]=="warepotatoes"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0
      }else if(crops[5]=="wosr"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0
      }else if(crops[5]=="sugarbeet"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0
      }else if(crops[5]=="setaside"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0
      }else if(crops[5]=="sosr"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0
      }else if(crops[5]=="winterlinseed"||crops[5]=="springlinseed"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0
      }else if(crops[5]=="driedpeas"||crops[5]=="peas"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0
      }
      
      #***********
      if(crops[6]=="winterwheat"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0.5
      }else if(crops[6]=="springwheat"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0.5
      }else if(crops[6]=="winterbarley"||crops[6]=="winteroats"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0.5
      }else if(crops[6]=="springbarley"||crops[6]=="springoats"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0.5
      }else if(crops[6]=="winterbeans"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0
      }else if(crops[6]=="springbeans"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0
      }else if(crops[6]=="warepotatoes"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0
      }else if(crops[6]=="wosr"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0
      }else if(crops[6]=="sugarbeet"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0
      }else if(crops[6]=="setaside"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0
      }else if(crops[6]=="sosr"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0
      }else if(crops[6]=="winterlinseed"||crops[6]=="springlinseed"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0
      }else if(crops[6]=="driedpeas"||crops[6]=="peas"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0
      }
      
      
      op <- fi1
      
      #*************** Farm Costs/Revenue ****************************
      # Farm output estimates
      le <- 7
      if(subsidy=="no"){
        farmpayment <- 0
      }else{
        farmpayment <- op[c7,seq(2,le)]
      }
      
      # Farm output estimates
      # Farm output = (Crop yield * Crop price) + farmpayment
      outp <- ((op[c3,seq(2,le)]*op[c4,seq(2,le)])+(op[c5,seq(2,le)]*op[c6,seq(2,le)]))+farmpayment 
      op[c8,seq(2,le)] <- outp 
      
      kk1 <- op
      
      #*************** Farm Costs ****************************
      vc <- kk1 # Updated data with farmOutput(soil, subsidy, blackgrass)
      
      #h1 <- 10; h2 <- 11; h3 <- 12; h4 <- 13; h5 <- 14; h6 <- 15;
      #h7 <- 16; h8 <- 17; 
      
      # Fertiliser (NPK) cost = (Fertiliser rate (kg/ha)* Fertiliser price(£/kg))
      vc[h6,seq(2,le)] <- fertcost <- (vc[1,seq(2,le)]*vc[4,seq(2,le)])+
        (vc[2,seq(2,le)]*vc[5,seq(2,le)])+(vc[3,seq(2,le)]*vc[6,seq(2,le)])
      
      # Seed cost (Seed rate (kg/ha) * Seed price (£/kg))
      vc[h7,seq(2,le)] <- seedcost <- vc[7,seq(2,le)]*vc[8,seq(2,le)]
      
      # Herbicide cost is assumed to be cost of herbicides targeting black-grass 
      # (cost of other herbicides and other chemical costs are incorporated in Sundry cost)
      # Herbicide cost = Total Herbicide rate (kg or l/ha) * Herbicide price (£/l)
      vc[h8,seq(2,le)] <- chemcost <- (vc[9,seq(2,le)]*vc[h1,seq(2,le)])+(vc[h2,seq(2,le)]*vc[h3,seq(2,le)])
      
      # Sundry Cost (£/ha) Informed by data from Nix (2019) Farm Management Pocketbook
      sundry <- vc[c1,seq(2,le)] 
      
      # Sugar beet transport cost from Nix (2019) based on £5.11/t
      vc[c2,seq(2,le)] <- sbeettransport <- vc[h5,seq(2,le)]*vc[c3,seq(2,le)] 
      
      # Variable cost (£/ha)
      vc[c9,seq(2,le)] <- (fertcost+seedcost+chemcost+sundry+sbeettransport)
      
      # Gross margin (£/ha)
      # Gross margin = Output - Variable cost
      vc[c10,seq(2,le)] <- vc[c8,seq(2,le)]-vc[c9,seq(2,le)]
      kk2 <- vc
      
      # operatingCost <- function(nsoil, subsidy, blackgrass){
      
      # Estimates workrate of farm operations and fuel and labour cost
      # wro <- variableCost(nsoil, subsidy, blackgrass)
      
      #========= Machine Sizes ===========
      cr <- kk2 #variableCost(nsoil, subsidy, blackgrass) #wro #Files()[[2]]
      ma <- modData[[3]][,seq(-1,-2)] # Machine data
      wr <- modData[[2]] # Workrate data 
      tractor <- ma[1,1] # Tractor Size or power (kW) 
      sprayer <- ma[4,1] # Sprayer Size (size of tank in litres) 1400 litres
      tsize <- ma[4,1] # Tank size for estimate fertiliser application workrate
      combsize <- ma[5,1] # Size of combine harvester
      # The size of the combine harvester measured in tonnes/hour 
      # was derived on pro rata basis based on information from
      # Agricultural Notebook. A combine harvester with a power of 90kW can harvest 10t/h 
      # Thus a combine harvester of 125kW can harvest (10/90)*125
      extfactor <- round(10/90,2)
      combine <- round(combsize*extfactor) #14 # Represents combine harvester size
      rollwidth <- ma[2,1] # Roll width
      sphoe <- 19 # Assumed speed for hoeing (19km/h)
      rowsp <- 0.6 # Assumed row space (0.6m)
      tpspeed <- 4 # Speed for rolling (km/hr)
      
      # ========= Tillage Types ==========
      # Tillage Assumptions
      # To be able to take into consideration soil type in tillage work rate,
      # the ploughing work rate is taken as a reference value due to the availability of suitable formulae.
      # The workability of ploughing is taken as 1.
      
      if(tillage=="ploughing"){
        wkb <- 1 # ploughing workability used as a reference value
      }else if(tillage=="noninversion"){
        wkb <- 1*0.5 # 50% reduction in plough rate is assumed - i.e. twice as fast as ploughing
      }else if(tillage=="subsoiling"){
        wkb <- 1/0.75 # 75% slower than ploughing (previously this was 1*0.65, as a 35% reduction in plough work rate was assumed. After talking to contractors, this was amended: they said subsoiling is slower than ploughing, and you'd do 75% of what you'd get done if ploughing in the same time).
      }else if(tillage=="lightcultivation"){
        wkb <- 1*0.6 # 40% reduction in plough work rate is assumed
      }else if(tillage=="directdrilling"){
        wkb <- 0 # no cultivation
      }else if (tillage=="minimumtillage"){
        wkb <- 1*0.3 # 70% reduction in plough work rate is assumed
      }else if(tillage=="deepcultivation"){
        wkb <- (1*0.6)*1.4 # Assumed to be 40% more than light cultivation
      }else if(tillage[1]=="inversion"){
        wkb <- 1 
      }else{
        wkb <- 1 
      }
      
      nspr <- numberofsprays # Number of Sprays ***************************************************************************
      
      if("winterwheat"%in%crops==TRUE){
        # Work rates for winter wheat operations =======
        wr[1,2] <- wr[1,3] <- round(((0.06+0.00025*({cr$winterwheat[2]}+{cr$winterwheat[3]}))+
                                       (64.48+0.094*({cr$winterwheat[2]}+{cr$winterwheat[3]}))/ tsize),2) # Spread P/K fert
        wr[2,2] <- wr[2,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        wr[3,2] <- wr[3,3] <- round((0.06+0.00069*{cr$winterwheat[7]})+
                                      (58.82+41.5*{nsoil}+0.00626*{cr$winterwheat[7]})/tractor,2) # Sow
        wr[4,2] <- wr[4,3] <- round((1.5/(tpspeed*rollwidth/10))*0.85,2) # Roll
        wr[5,2] <- wr[5,3] <- 
          round(((0.06+0.00025*({cr$winterwheat[1]}))+(64.68+0.094*({cr$winterwheat[1]}))/ tsize),2) # N fert
        
        if(crops[1]=="winterwheat"){
          wr[6,2] <- wr[6,3] <- wr[6,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="winterwheat"){
          wr[6,2] <- wr[6,3] <- wr[6,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="winterwheat"){
          wr[6,2] <- wr[6,3] <- wr[6,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="winterwheat"){
          wr[6,2] <- wr[6,3] <- wr[6,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="winterwheat"){
          wr[6,2] <- wr[6,3] <- wr[6,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="winterwheat"){
          wr[6,2] <- wr[6,3] <- wr[6,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[6,2] <- wr[6,3] <- wr[6,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        
        cbwwht <- round((1.00*({cr$winterwheat[c3]}+20)/4)/combine,2) # Combine harvester
        wr[7,2] <- 2* cbwwht; wr[7,3] <- 3* cbwwht; wr[7,6] <- cbwwht
        bawwht <- round((({cr$winterwheat[c5]}+13)/4)/combine*0.5,2) # Bale
        wr[8,2] <- 2* bawwht; wr[8,3] <- 2* bawwht; wr[8,7] <- bawwht
      }else{
        wr[seq(1,8),seq(2,12)] <- 0
      }
      
      if("springwheat"%in%crops==TRUE){
        # Work rates for spring wheat operations =======
        wr[9,2] <- wr[9,3] <- round(((0.06+0.00025*({cr$springwheat[3]}+{cr$springwheat[5]}))+
                                       (64.48+0.094*({cr$springwheat[3]}+{cr$springwheat[5]}))/ tsize),2) # Spread P/K fert
        wr[10,2] <- wr[10,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        wr[11,2] <- wr[11,3] <- round(((0.06+0.00069*{cr$springwheat[7]})+
                                         (58.82+41.5*{nsoil}+0.00626*{cr$springwheat[7]})/tractor),2) #Sow
        wr[12,2] <- wr[12,3] <- round((1.5/(tpspeed*rollwidth/10))*0.85,2) # Roll
        wr[13,2] <- wr[13,3] <- round(((0.06+0.00025*({cr$springwheat[1]}))+(64.68+0.094*({cr$springwheat[1]}))/ tsize),2) # N fert
        
        if(crops[1]=="springwheat"){
          wr[14,2] <- wr[14,3] <- wr[14,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="springwheat"){
          wr[14,2] <- wr[14,3] <- wr[14,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="springwheat"){
          wr[14,2] <- wr[14,3] <- wr[14,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="springwheat"){
          wr[14,2] <- wr[14,3] <- wr[14,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="springwheat"){
          wr[14,2] <- wr[14,3] <- wr[14,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="springwheat"){
          wr[14,2] <- wr[14,3] <- wr[14,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[14,2] <- wr[14,3] <- wr[14,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        cbswht <- round(2*(1.00*({cr$springwheat[c3]}+20)/4)/combine,2) # Combine
        wr[15,2] <- 2* cbswht; wr[15,3] <- 3* cbswht; wr[15,6] <- cbswht
        baswht <- round((({cr$springwheat[c5]}+13)/4)/combine*0.5,2) # Bale
        wr[16,2] <- 2* baswht;  wr[16,3] <- 2* baswht; wr[16,7] <- baswht
        # 
      }else{
        wr[seq(9,16),seq(2,12)] <-0
      }
      
      if("winterbarley"%in%crops==TRUE||"winteroats"%in%crops==TRUE){
        # Work rates for winter barley operations =====
        wr[17,2] <- wr[17,3] <- round(((0.06+0.00025*({cr$winterbarley[3]}+{cr$winterbarley[5]}))+
                                         (64.48+0.094*({cr$winterbarley[3]}+{cr$winterbarley[5]}))/ tsize),2) # Spread P/K fert
        wr[18,2] <- wr[18,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        wr[19,2] <- wr[19,3] <- round(((0.06+0.00069*{cr$winterbarley[7]})+
                                         (58.82+41.5*{nsoil}+0.00626*{cr$winterbarley[7]})/tractor),2) # Sow
        wr[20,2] <- wr[20,3] <- round((1.5/(tpspeed*rollwidth/10))*0.85,2) # Roll
        wr[21,2] <- wr[21,3] <- 
          round(((0.06+0.00025*({cr$winterbarley[1]}))+(64.68+0.094*({cr$winterbarley[1]}))/ tsize),2) # N fert
        
        if(crops[1]=="winterbarley"||crops[1]=="winteroats"){
          wr[22,2] <- wr[22,3] <- wr[22,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="winterbarley"||crops[2]=="winteroats"){
          wr[22,2] <- wr[22,3] <- wr[22,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="winterbarley"||crops[3]=="winteroats"){
          wr[22,2] <- wr[22,3] <- wr[22,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="winterbarley"||crops[4]=="winteroats"){
          wr[22,2] <- wr[22,3] <- wr[22,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="winterbarley"||crops[5]=="winteroats"){
          wr[22,2] <- wr[22,3] <- wr[22,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="winterbarley"||crops[6]=="winteroats"){
          wr[22,2] <- wr[22,3] <- wr[22,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[22,2] <- wr[22,3] <- wr[22,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        cbwbar <- round((1.15*({cr$winterbarley[c3]}+24)/4)/combine,2) # Combine
        wr[23,2] <- 2* cbwbar; wr[23,3] <- 3* cbwbar; wr[23,6] <- cbwbar
        bawbar <- round((({cr$winterbarley[c5]}+13)/4)/combine*0.5,2) # Bale
        wr[24,2] <- 3* bawbar; wr[24,3] <- 3* bawbar; wr[24,7] <- bawbar
      }else{
        wr[seq(17,24),seq(2,12)] <- 0
      }
      
      if("springbarley"%in%crops==TRUE||"springoats"%in%crops==TRUE){
        # Work rates for spring barley operations =====
        wr[25,2] <- wr[25,3] <- round(((0.06+0.00025*({cr$springbarley[2]}+{cr$springbarley[3]}))+
                                         (64.48+0.094*({cr$springbarley[2]}+{cr$springbarley[3]}))/ tsize),2) # Spread P/K fert
        wr[26,2] <- wr[26,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        wr[27,2] <- wr[27,3] <- round(((0.06+0.00069*{cr$springbarley[7]})+
                                         (58.82+41.5*{nsoil}+0.00626*{cr$springbarley[7]})/tractor),2) # Sow
        wr[28,2] <- wr[28,3] <- round((1.5/(tpspeed*rollwidth/10))*0.85,2) # Roll
        
        #wr[29,2] <- wr[29,3] <- wr[29,5] <- round(0.11+204.2/sprayer,2) # Spraying
        if(crops[1]=="springbarley"||crops[1]=="springoats"){
          wr[29,2] <- wr[29,3] <- wr[29,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="springbarley"||crops[2]=="springoats"){
          wr[29,2] <- wr[29,3] <- wr[29,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="springbarley"||crops[3]=="springoats"){
          wr[29,2] <- wr[29,3] <- wr[29,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="springbarley"||crops[4]=="springoats"){
          wr[29,2] <- wr[29,3] <- wr[29,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="springbarley"||crops[5]=="springoats"){
          wr[29,2] <- wr[29,3] <- wr[29,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="springbarley"||crops[6]=="springoats"){
          wr[29,2] <- wr[29,3] <- wr[29,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[29,2] <- wr[29,3] <- wr[29,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        
        wr[30,2] <- wr[30,3] <- 
          round(((0.06+0.00025*({cr$springbarley[1]}))+(64.68+0.094*({cr$springbarley[1]}))/ tsize),2) # N fert
        cbsbar <- round(((1.15*{cr$springbarley[c3]}+24)/4)/combine,2) # Combine
        wr[31,2] <- 2* cbsbar; wr[31,3] <- 3* cbsbar; wr[31,6] <- cbsbar
        basbar <- round((({cr$springbarley[c5]}+13)/4)/combine*0.5,2) # Bale
        wr[32,2] <- 3* basbar; wr[32,3] <- 3* basbar; wr[32,7] <- basbar
      }else{
        wr[seq(25,32),seq(2,12)] <- 0
      }
      
      if("winterbeans"%in%crops==TRUE){
        # Work rates for winter beans operations =====
        wr[33,2] <- wr[33,3] <- 
          round(((0.06+0.00025*({cr$winterbeans[3]}+{cr$winterbeans[5]}))+
                   (64.48+0.094*({cr$winterbeans[3]}+{cr$winterbeans[5]}))/ tsize),2) # Spread P/K fert
        wr[34,2] <- wr[34,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        wr[35,2] <- wr[35,3] <- round((3*(0.114+0.00033*{cr$winterbeans[7]})+
                                         (54*{nsoil}+21.6)/tractor),2) # Sow
        wr[36,2] <- wr[36,3] <- round((1.5/(tpspeed*rollwidth/10))*0.85,2) # Roll
        
        #wr[37,2] <- wr[37,3] <- wr[37,5] <- round(0.11+204.2/sprayer,2) #Spray
        if(crops[1]=="winterbeans"){
          wr[37,2] <- wr[37,3] <- wr[37,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="winterbeans"){
          wr[37,2] <- wr[37,3] <- wr[37,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="winterbeans"){
          wr[37,2] <- wr[37,3] <- wr[37,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="winterbeans"){
          wr[37,2] <- wr[37,3] <- wr[37,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="winterbeans"){
          wr[37,2] <- wr[37,3] <- wr[37,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="winterbeans"){
          wr[37,2] <- wr[37,3] <- wr[37,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[37,2] <- wr[37,3] <- wr[37,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        cbwbea <- round((4.05*({cr$winterbeans[c3]}+24)/4)/combine,2) #Combine
        wr[38,2] <- 2* cbwbea; wr[38,3] <- 3* cbwbea; wr[38,6] <- cbwbea
      }else{
        wr[seq(33,38),seq(2,12)] <- 0
      }
      
      if("springbeans"%in%crops==TRUE){
        # Work rates for spring beans operations =====
        wr[39,2] <- wr[39,3] <- 
          round(((0.06+0.00025*({cr$springbeans[2]}+{cr$springbeans[3]}))+
                   (64.48+0.094*({cr$springbeans[2]}+{cr$springbeans[3]}))/ tsize),2) # Spread P/K fert
        wr[40,2] <- wr[40,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        wr[41,2] <- wr[41,3] <- round(((0.06+0.00069*{cr$springbeans[7]})+
                                         (92.42+0.00626*{cr$springbeans[7]}+41.5*{nsoil})/tractor),2) # Sow
        
        if(crops[1]=="springbeans"){
          wr[42,2] <- wr[42,3] <- wr[42,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="springbeans"){
          wr[42,2] <- wr[42,3] <- wr[42,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="springbeans"){
          wr[42,2] <- wr[42,3] <- wr[42,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="springbeans"){
          wr[42,2] <- wr[42,3] <- wr[42,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="springbeans"){
          wr[42,2] <- wr[42,3] <- wr[42,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="springbeans"){
          wr[42,2] <- wr[42,3] <- wr[42,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[42,2] <- wr[42,3] <- wr[42,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        cbsbea <- round((4.05*({cr$springbeans[c3]}+24)/4)/combine,2) # Harvest
        wr[43,2] <- 2* cbsbea; wr[43,3] <- 3* cbsbea; wr[43,6] <- cbsbea
      }else{
        wr[seq(39,43),seq(2,12)] <-0
      }
      
      if("warepotatoes"%in%crops==TRUE){
        # Work rates for ware potatoes operations =====
        wr[44,2] <- wr[44,3] <- 
          round(((1.80*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage 
        wr[45,2] <- wr[45,3] <- 
          wr[45,4] <- round(((25*{nsoil}+33)/tractor),2) # Harrowing
        wr[46,2] <- wr[46,3] <- 
          round(((278/tractor+0.04+0.55*{cr$warepotatoes[7]})/2000),2)*3 # Sow potatoes
        wr[47,2] <- wr[47,3] <- round(((40*{nsoil}+33)/tractor),2) # Ridging
        
        if(crops[1]=="warepotatoes"){
          wr[48,2] <- wr[48,3] <- wr[48,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="warepotatoes"){
          wr[48,2] <- wr[48,3] <- wr[48,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="warepotatoes"){
          wr[48,2] <- wr[48,3] <- wr[48,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="warepotatoes"){
          wr[48,2] <- wr[48,3] <- wr[48,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="warepotatoes"){
          wr[48,2] <- wr[48,3] <- wr[48,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="warepotatoes"){
          wr[48,2] <- wr[48,3] <- wr[48,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[48,2] <- wr[48,3] <- wr[48,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        wr[49,2] <- wr[49,3] <- round(((0.06+0.00025*({cr$warepotatoes[2]}+{cr$warepotatoes[3]}))+
                                         (64.48+0.094*({cr$warepotatoes[2]}+{cr$warepotatoes[3]}))/tsize),2) # Spread P/K fert
        wr[50,2] <- wr[50,3] <- 
          round(((0.06+0.00025*({cr$warepotatoes[1]}))+(64.68+0.094*({cr$warepotatoes[1]}))/ tsize),2) # N fert
        wr[51,2] <- wr[51,3] <-  round(1/(sphoe*rowsp/10*0.8),2) # Hoeing
        hpot <- round(((403/600)+2/(3*(1.25+0.51*{nsoil})*({39.43}/37.728)))*2.51,2) # Harvest pot
        wr[52,2] <- hpot*4; wr[52,3] <- hpot*4; wr[52,9] <- hpot 
      }else{
        wr[seq(44,52),seq(2,12)] <-0
      }
      
      if("wosr"%in%crops==TRUE){
        # Work rates for wosr operations =====
        wr[53,2] <- wr[53,3] <- round(((0.06+0.00025*({cr$wosr[2]}+{cr$wosr[3]}))+
                                         (64.48+0.094*({cr$wosr[2]}+{cr$wosr[3]}))/ tsize),2) # Spread P/K fert
        wr[54,2] <- wr[54,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        wr[55,2] <- wr[55,3] <- round(((0.387+0.00069*cr$wosr[7])+(99.42+0.00626*cr$wosr[7])/tractor),2)# Sow
        
        if(crops[1]=="wosr"){
          wr[56,2] <- wr[56,3] <- wr[25,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="wosr"){
          wr[56,2] <- wr[56,3] <- wr[25,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="wosr"){
          wr[56,2] <- wr[56,3] <- wr[25,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="wosr"){
          wr[56,2] <- wr[56,3] <- wr[25,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="wosr"){
          wr[56,2] <- wr[56,3] <- wr[25,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="wosr"){
          wr[56,2] <- wr[56,3] <- wr[25,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[56,2] <- wr[56,3] <- wr[25,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        wr[57,2] <- wr[57,3] <- 
          round(((0.06+0.00025*({cr$wosr[1]}))+(64.68+0.094*({cr$wosr[1]}))/ tsize),2) # N fert
        cbwosr <- round(((4.05*({cr$wosr[c3]}+24)/4)/combine),2) # Combine
        wr[58,2] <- 2* cbwosr; wr[58,3] <- 3* cbwosr; wr[58,6] <- cbwosr
      }else{
        wr[seq(53,58),seq(2,12)] <-0
      }
      
      if("sugarbeet"%in%crops==TRUE){
        # Work rates for sugarbeet operations =====
        wr[59,2] <- wr[59,3] <- round((1.80*(50*{nsoil}+20))/tractor,2) # Ploughing 
        wr[60,2] <- wr[60,3]  <- wr[29,4] <- round(((25*{nsoil}+33)/tractor)*wkb,2) # Harrowing
        wr[61,2] <- wr[61,3] <- round((0.39+157/tractor), 2) # Planting
        
        if(crops[1]=="sugarbeet"){
          wr[62,2] <- wr[62,3] <- wr[62,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="sugarbeet"){
          wr[62,2] <- wr[62,3] <- wr[62,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="sugarbeet"){
          wr[62,2] <- wr[62,3] <- wr[62,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="sugarbeet"){
          wr[62,2] <- wr[62,3] <- wr[62,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="sugarbeet"){
          wr[62,2] <- wr[62,3] <- wr[62,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="sugarbeet"){
          wr[62,2] <- wr[62,3] <- wr[62,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[62,2] <- wr[62,3] <- wr[62,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        wr[63,2] <- wr[63,3] <- round(((0.06+0.00025*({cr$sugarbeet[2]}+{cr$sugarbeet[3]}))+
                                         (64.48+0.094*({cr$sugarbeet[2]}+{cr$sugarbeet[3]}))/ tsize),2) # Spread P/K fert
        wr[64,2] <- wr[64,3] <- 
          round(((0.06+0.00025*({cr$sugarbeet[1]}))+(64.68+0.094*({cr$sugarbeet[1]}))/ tsize),2) # N fert
        wr[65,2] <- wr[65,3] <-  round(1/(sphoe*rowsp/10*0.8),2) # Hoeing
        hvsbee <- round(((403/600)+2/(3*(1.25+0.51*{nsoil})*({cr$sugarbeet[c3]}/37.728))),2) # Harvest
        wr[66,2] <- 3* hvsbee; wr[66,3] <- 3* hvsbee; wr[66,8] <- hvsbee
      }else{
        wr[seq(59,66),seq(2,12)] <-0
      }
      
      if("setaside"%in%crops==TRUE){#||"fallow"%in%crops==TRUE
        # Work rates for setaside operations =====
        wr[67,2] <- wr[67,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        #wr[68,2] <- wr[68,3] <- wr[68,5] <- round(0.11+204.2/sprayer,2)
        if(crops[1]=="setaside"){
          wr[68,2] <- wr[68,3] <- wr[68,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="setaside"){
          wr[68,2] <- wr[68,3] <- wr[68,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="setaside"){
          wr[68,2] <- wr[68,3] <- wr[68,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="setaside"){
          wr[68,2] <- wr[68,3] <- wr[68,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="setaside"){
          wr[68,2] <- wr[68,3] <- wr[68,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="setaside"){
          wr[68,2] <- wr[68,3] <- wr[68,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[68,2] <- wr[68,3] <- wr[68,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        
      }else{
        wr[seq(67,68),seq(2,12)] <-0
      }
      
      if("sosr"%in%crops==TRUE){
        # Work rates for sosr operations =====
        wr[69,2] <- wr[69,3] <- round(((0.06+0.00025*({cr$sosr[2]}+{cr$sosr[3]}))+
                                         (64.48+0.094*({cr$sosr[2]}+{cr$sosr[3]}))/ tsize),2) # Spread P/K fert
        wr[70,2] <- wr[70,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        wr[71,2] <- wr[71,3] <- round(((0.06+0.00069*cr$sosr[7])+(92.42+0.00626*(cr$sosr[7])+41.5*(nsoil))/tractor),2) # Sow
        
        if(crops[1]=="sosr"){
          wr[72,2] <- wr[72,3] <- wr[72,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="sosr"){
          wr[72,2] <- wr[72,3] <- wr[72,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="sosr"){
          wr[72,2] <- wr[72,3] <- wr[72,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="sosr"){
          wr[72,2] <- wr[72,3] <- wr[72,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="sosr"){
          wr[72,2] <- wr[72,3] <- wr[72,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="sosr"){
          wr[72,2] <- wr[72,3] <- wr[72,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[72,2] <- wr[72,3] <- wr[72,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        wr[73,2] <- wr[73,3] <- 
          round(((0.06+0.00025*({cr$sosr[1]}))+(64.68+0.094*({cr$sosr[1]}))/ tsize),2) # N fert
        cbsosr <- round(((4.05*({cr$sosr[c3]}+24)/4)/combine),2) # Harvest
        wr[74,2] <- 2* cbsosr; wr[74,3] <- 3* cbsosr; wr[74,6] <- cbsosr
      }else{
        wr[seq(69,74),seq(2,12)] <-0
      }
      
      if("winterlinseed"%in%crops==TRUE){
        # Work rates for winterlinseed operations =====
        wr[75,2] <- wr[75,3] <- round(((0.06+0.00025*({cr$winterlinseed[2]}+{cr$winterlinseed[3]}))+
                                         (64.48+0.094*({cr$winterlinseed[2]}+{cr$winterlinseed[3]}))/ tsize),2) # Spread P/K fert
        wr[76,2] <- wr[76,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        
        wr[77,2] <- wr[77,3] <- round(((0.06+0.00069*cr$winterlinseed[7])+
                                         (92.42+0.00626*(cr$winterlinseed[7])+41.5*(nsoil))/tractor),2) # Planting
        
        if(crops[1]=="winterlinseed"){
          wr[78,2] <- wr[78,3] <- wr[78,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="winterlinseed"){
          wr[78,2] <- wr[78,3] <- wr[78,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="winterlinseed"){
          wr[78,2] <- wr[78,3] <- wr[78,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="winterlinseed"){
          wr[78,2] <- wr[78,3] <- wr[78,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="winterlinseed"){
          wr[78,2] <- wr[78,3] <- wr[78,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="winterlinseed"){
          wr[78,2] <- wr[78,3] <- wr[78,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[78,2] <- wr[78,3] <- wr[78,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        wr[79,2] <- wr[79,3] <- 
          round(((0.06+0.00025*({cr$winterlinseed[1]}))+(64.68+0.094*({cr$winterlinseed[1]}))/ tsize),2) # N fert
        cbwlin <- round(((4.05*({cr$winterlinseed[c3]}+24)/4)/combine),2) # Harvest
        wr[80,2] <- 2* cbwlin; wr[80,3] <- 3* cbwlin; wr[80,6] <- cbwlin
      }else{
        wr[seq(75,80),seq(2,12)] <-0
      }
      
      if("springlinseed"%in%crops==TRUE){
        # Work rates for springlinseed operations =====
        wr[81,2] <- wr[81,3] <- round(((0.06+0.00025*({cr$springlinseed[2]}+{cr$springlinseed[3]}))+
                                         (64.48+0.094*({cr$springlinseed[2]}+{cr$springlinseed[3]}))/ tsize),2) # Spread P/K fert
        wr[82,2] <- wr[82,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        
        wr[83,2] <- wr[83,3] <- round(((0.06+0.00069*cr$springlinseed[7])+
                                         (92.42+0.00626*(cr$springlinseed[7])+41.5*(nsoil))/tractor),2) # Planting
        
        if(crops[1]=="springlinseed"){
          wr[84,2] <- wr[84,3] <- wr[84,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="springlinseed"){
          wr[84,2] <- wr[84,3] <- wr[84,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="springlinseed"){
          wr[84,2] <- wr[84,3] <- wr[84,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="springlinseed"){
          wr[84,2] <- wr[84,3] <- wr[84,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="springlinseed"){
          wr[84,2] <- wr[84,3] <- wr[84,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="springlinseed"){
          wr[84,2] <- wr[84,3] <- wr[84,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[84,2] <- wr[84,3] <- wr[84,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        wr[85,2] <- wr[85,3] <- 
          round(((0.06+0.00025*({cr$springlinseed[1]}))+(64.68+0.094*({cr$springlinseed[1]}))/ tsize),2) # N fert
        cbslin <- round(((4.05*({cr$springlinseed[c3]}+24)/4)/combine),2) # Harvest
        wr[86,2] <- 2* cbslin; wr[86,3] <- 3* cbslin; wr[86,6] <- cbslin
      }else{
        wr[seq(81,86),seq(2,12)] <-0
      }
      
      if("driedpeas"%in%crops==TRUE||"peas"%in%crops==TRUE){
        # Work rates for peas operations =====
        wr[87,2] <- wr[87,3] <- 
          round(((0.06+0.00025*({cr$driedpeas[3]}+{cr$driedpeas[5]}))+
                   (64.48+0.094*({cr$driedpeas[3]}+{cr$driedpeas[5]}))/ tsize),2) # P/K Fert
        wr[88,2] <- wr[88,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        
        wr[89,2] <- wr[89,3] <- round((0.06+0.00069*{cr$driedpeas[7]})+
                                        (92.42+0.00626*{cr$driedpeas[7]}+41.5*{nsoil})/tractor,2) # Planting
        wr[90,2] <- wr[90,3] <- round((1.5/(tpspeed*rollwidth/10))*0.85,2) # Roll
        
        if(crops[1]=="driedpeas"||crops[1]=="peas"){
          wr[91,2] <- wr[91,3] <- wr[91,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="driedpeas"||crops[2]=="peas"){
          wr[91,2] <- wr[91,3] <- wr[91,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="driedpeas"||crops[3]=="peas"){
          wr[91,2] <- wr[91,3] <- wr[91,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="driedpeas"||crops[4]=="peas"){
          wr[91,2] <- wr[91,3] <- wr[91,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="driedpeas"||crops[5]=="peas"){
          wr[91,2] <- wr[91,3] <- wr[91,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="driedpeas"||crops[6]=="peas"){
          wr[91,2] <- wr[91,3] <- wr[91,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[91,2] <- wr[91,3] <- wr[91,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        cbdpea <- round((4.05*({cr$driedpeas[c3]}+24)/4)/combine,2) # Harvest
        wr[92,2] <- 2* cbdpea; wr[92,3] <- 3* cbdpea; wr[92,6] <- cbdpea
      }else{
        wr[seq(87,92),seq(2,12)] <- 0
      }
      
      kk3 <- wr
      
      
      # ******* Estimates fuel and labour costs ***************
      
      fuelPrice <- fuelprice #farm[2]
      labourWage <- labourwage #farm[3]
      TP <- round(tractor*1.341,0) # maximum PTO horsepower 
      CP <- round(combsize*1.341,0)
      
      # For diesel tractor, fuel consumption is estimated from the formula below
      # Obtained from: http://www.extension.iastate.edu/agdm/crops/html/a3-29.html (16/06/2015)
      # 0.044 * maximum PTO horsepower for diesel engines
      
      fuel.cons_gal_hr <- round(0.044 * TP,2) # Gallons per hour Tractor
      fuel.cons_gal_hr_comb <- round(0.044 * CP,2) # Combine
      # Convert gallons per hour to litres per hour
      # 1 gallon per hour = 4.546 litres per hour
      
      Fuel_Cons <- round(fuel.cons_gal_hr * 4.546,0) # Fuel consumption (litres/hour)
      Fuel_Cons_comb <- round(fuel.cons_gal_hr_comb * 4.546,0)
      
      # Fuel and labour cost (£/hour)
      fuelCost <-  (fuelPrice * Fuel_Cons)*1.1 # 10% represents oil and lubricants
      fuelCost_comb <- (fuelPrice * Fuel_Cons_comb)*1.1
      
      #Fuel and labour costs @ £/ha (Multiply £/hour by workrates (hr/ha))
      kk3[,10] <- round((kk3[,2]*fuelCost)+(kk3[,6]*fuelCost_comb)+
                          (kk3[,7]*fuelCost_comb)) # Fuel costs
      kk3[,11] <- round(kk3[,3]*labourWage) # Labour costs
      kk3[,12] <- round(kk3[,10]+kk3[,11])
      ops <- kk3
      
      # Fuel cost
      fu <- ops[,10]
      
      fuelcost <- c(sum(fu[seq(1,8)]),sum(fu[seq(9,16)]),sum(fu[seq(17,24)]),sum(fu[seq(25,32)]),
                    sum(fu[seq(33,38)]),sum(fu[seq(39,43)]),sum(fu[seq(44,52)]),sum(fu[seq(53,58)]),sum(fu[seq(59,66)]),
                    sum(fu[seq(67,68)]),sum(fu[seq(69,74)]),sum(fu[seq(75,80)]),sum(fu[seq(81,86)]),sum(fu[seq(87,92)]))
      # Labour cost
      la <- ops[,11]
      
      labourcost <- c(sum(la[seq(1,8)]),sum(la[seq(9,16)]),sum(la[seq(17,24)]),sum(la[seq(25,32)]),
                      sum(la[seq(33,38)]),sum(la[seq(39,43)]),sum(la[seq(44,52)]),sum(la[seq(53,58)]),sum(la[seq(59,66)]),
                      sum(la[seq(67,68)]),sum(la[seq(69,74)]),sum(la[seq(75,80)]),sum(la[seq(81,86)]),sum(la[seq(87,92)]))
      # Operating Cost
      op1 <- ops[,12]
      
      opcost <- c(sum(op1[seq(1,8)]),sum(op1[seq(9,16)]),sum(op1[seq(17,24)]),sum(op1[seq(25,32)]),
                  sum(op1[seq(33,38)]),sum(op1[seq(39,43)]),sum(op1[seq(44,52)]),sum(op1[seq(53,58)]),sum(op1[seq(59,66)]),
                  sum(op1[seq(67,68)]),sum(op1[seq(69,74)]),sum(op1[seq(75,80)]),sum(op1[seq(81,86)]),sum(op1[seq(87,92)]))
      
      crops <- crops
      
      if(crops[1]=="winterwheat"){
        fc1 <- fuelcost[1]; lc1 <- labourcost[1]; opc1 <- opcost[1]
      }else if(crops[1]=="springwheat"){
        fc1 <- fuelcost[2]; lc1 <- labourcost[2]; opc1 <- opcost[2]
      }else if(crops[1]=="winterbarley"||crops[1]=="winteroats"){
        fc1 <- fuelcost[3]; lc1 <- labourcost[3]; opc1 <- opcost[3]
      }else if(crops[1]=="springbarley"||crops[1]=="springoats"){
        fc1 <- fuelcost[4]; lc1 <- labourcost[4]; opc1 <- opcost[4]
      }else if(crops[1]=="winterbeans"){
        fc1 <- fuelcost[5]; lc1 <- labourcost[5]; opc1 <- opcost[5]
      }else if(crops[1]=="springbeans"){
        fc1 <- fuelcost[6]; lc1 <- labourcost[6]; opc1 <- opcost[6]
      }else if(crops[1]=="warepotatoes"){
        fc1 <- fuelcost[7]; lc1 <- labourcost[7]; opc1 <- opcost[7]
      }else if(crops[1]=="wosr"){
        fc1 <- fuelcost[8]; lc1 <- labourcost[8]; opc1 <- opcost[8]
      }else if(crops[1]=="sugarbeet"){
        fc1 <- fuelcost[9]; lc1 <- labourcost[9]; opc1 <- opcost[9]
      }else if(crops[1]=="setaside"||crops[1]=="fallow"){
        fc1 <- fuelcost[10]; lc1 <- labourcost[10]; opc1 <- opcost[10]
      }else if(crops[1]=="sosr"){
        fc1 <- fuelcost[11]; lc1 <- labourcost[11]; opc1 <- opcost[11]
      }else if(crops[1]=="winterlinseed"){
        fc1 <- fuelcost[12]; lc1 <- labourcost[12]; opc1 <- opcost[12]
      }else if(crops[1]=="springlinseed"){
        fc1 <- fuelcost[13]; lc1 <- labourcost[13]; opc1 <- opcost[13]
      }else if(crops[1]=="driedpeas"){
        fc1 <- fuelcost[14]; lc1 <- labourcost[14]; opc1 <- opcost[14]
      }
      
      if(crops[2]=="winterwheat"){
        fc2 <- fuelcost[1]; lc2 <- labourcost[1]; opc2 <- opcost[1]
      }else if(crops[2]=="springwheat"){
        fc2 <- fuelcost[2]; lc2 <- labourcost[2]; opc2 <- opcost[2]
      }else if(crops[2]=="winterbarley"||crops[2]=="winteroats"){
        fc2 <- fuelcost[3]; lc2 <- labourcost[3]; opc2 <- opcost[3]
      }else if(crops[2]=="springbarley"||crops[2]=="springoats"){
        fc2 <- fuelcost[4]; lc2 <- labourcost[4]; opc2 <- opcost[4]
      }else if(crops[2]=="winterbeans"){
        fc2 <- fuelcost[5]; lc2 <- labourcost[5]; opc2 <- opcost[5]
      }else if(crops[2]=="springbeans"){
        fc2 <- fuelcost[6]; lc2 <- labourcost[6]; opc2 <- opcost[6]
      }else if(crops[2]=="warepotatoes"){
        fc2 <- fuelcost[7]; lc2 <- labourcost[7]; opc2 <- opcost[7]
      }else if(crops[2]=="wosr"){
        fc2 <- fuelcost[8]; lc2 <- labourcost[8]; opc2 <- opcost[8]
      }else if(crops[2]=="sugarbeet"){
        fc2 <- fuelcost[9]; lc2 <- labourcost[9]; opc2 <- opcost[9]
      }else if(crops[2]=="setaside"||crops[2]=="fallow"){
        fc2 <- fuelcost[10]; lc2 <- labourcost[10]; opc2 <- opcost[10]
      }else if(crops[2]=="sosr"){
        fc2 <- fuelcost[11]; lc2 <- labourcost[11]; opc2 <- opcost[11]
      }else if(crops[2]=="winterlinseed"){
        fc2 <- fuelcost[12]; lc2 <- labourcost[12]; opc2 <- opcost[12]
      }else if(crops[2]=="springlinseed"){
        fc2 <- fuelcost[13]; lc2 <- labourcost[13]; opc2 <- opcost[13]
      }else if(crops[2]=="driedpeas"){
        fc2 <- fuelcost[14]; lc2 <- labourcost[14]; opc2 <- opcost[14]
      }
      
      if(crops[3]=="winterwheat"){
        fc3 <- fuelcost[1]; lc3 <- labourcost[1]; opc3 <- opcost[1]
      }else if(crops[3]=="springwheat"){
        fc3 <- fuelcost[2]; lc3 <- labourcost[2]; opc3 <- opcost[2]
      }else if(crops[3]=="winterbarley"||crops[3]=="winteroats"){
        fc3 <- fuelcost[3]; lc3 <- labourcost[3]; opc3 <- opcost[3]
      }else if(crops[3]=="springbarley"||crops[3]=="springoats"){
        fc3 <- fuelcost[4]; lc3 <- labourcost[4]; opc3 <- opcost[4]
      }else if(crops[3]=="winterbeans"){
        fc3 <- fuelcost[5]; lc3 <- labourcost[5]; opc3 <- opcost[5]
      }else if(crops[3]=="springbeans"){
        fc3 <- fuelcost[6]; lc3 <- labourcost[6]; opc3 <- opcost[6]
      }else if(crops[3]=="warepotatoes"){
        fc3 <- fuelcost[7]; lc3 <- labourcost[7]; opc3 <- opcost[7]
      }else if(crops[3]=="wosr"){
        fc3 <- fuelcost[8]; lc3 <- labourcost[8]; opc3 <- opcost[8]
      }else if(crops[3]=="sugarbeet"){
        fc3 <- fuelcost[9]; lc3 <- labourcost[9]; opc3 <- opcost[9]
      }else if(crops[3]=="setaside"||crops[3]=="fallow"){
        fc3 <- fuelcost[10]; lc3 <- labourcost[10]; opc3 <- opcost[10]
      }else if(crops[3]=="sosr"){
        fc3 <- fuelcost[11]; lc3 <- labourcost[11]; opc3 <- opcost[11]
      }else if(crops[3]=="winterlinseed"){
        fc3 <- fuelcost[12]; lc3 <- labourcost[12]; opc3 <- opcost[12]
      }else if(crops[3]=="springlinseed"){
        fc3 <- fuelcost[13]; lc3 <- labourcost[13]; opc3 <- opcost[13]
      }else if(crops[3]=="driedpeas"){
        fc3 <- fuelcost[14]; lc3 <- labourcost[14]; opc3 <- opcost[14]
      }
      
      if(crops[4]=="winterwheat"){
        fc4 <- fuelcost[1]; lc4 <- labourcost[1]; opc4 <- opcost[1]
      }else if(crops[4]=="springwheat"){
        fc4 <- fuelcost[2]; lc4 <- labourcost[2]; opc4 <- opcost[2]
      }else if(crops[4]=="winterbarley"||crops[4]=="winteroats"){
        fc4 <- fuelcost[3]; lc4 <- labourcost[3]; opc4 <- opcost[3]
      }else if(crops[4]=="springbarley"||crops[4]=="springoats"){
        fc4 <- fuelcost[4]; lc4 <- labourcost[4]; opc4 <- opcost[4]
      }else if(crops[4]=="winterbeans"){
        fc4 <- fuelcost[5]; lc4 <- labourcost[5]; opc4 <- opcost[5]
      }else if(crops[4]=="springbeans"){
        fc4 <- fuelcost[6]; lc4 <- labourcost[6]; opc4 <- opcost[6]
      }else if(crops[4]=="warepotatoes"){
        fc4 <- fuelcost[7]; lc4 <- labourcost[7]; opc4 <- opcost[7]
      }else if(crops[4]=="wosr"){
        fc4 <- fuelcost[8]; lc4 <- labourcost[8]; opc4 <- opcost[8]
      }else if(crops[4]=="sugarbeet"){
        fc4 <- fuelcost[9]; lc4 <- labourcost[9]; opc4 <- opcost[9]
      }else if(crops[4]=="setaside"||crops[4]=="fallow"){
        fc4 <- fuelcost[10]; lc4 <- labourcost[10]; opc4 <- opcost[10]
      }else if(crops[4]=="sosr"){
        fc4 <- fuelcost[11]; lc4 <- labourcost[11]; opc4 <- opcost[11]
      }else if(crops[4]=="winterlinseed"){
        fc4 <- fuelcost[12]; lc4 <- labourcost[12]; opc4 <- opcost[12]
      }else if(crops[4]=="springlinseed"){
        fc4 <- fuelcost[13]; lc4 <- labourcost[13]; opc4 <- opcost[13]
      }else if(crops[4]=="driedpeas"){
        fc4 <- fuelcost[14]; lc4 <- labourcost[14]; opc4 <- opcost[14]
      }
      
      if(crops[5]=="winterwheat"){
        fc5 <- fuelcost[1]; lc5 <- labourcost[1]; opc5 <- opcost[1]
      }else if(crops[5]=="springwheat"){
        fc5 <- fuelcost[2]; lc5 <- labourcost[2]; opc5 <- opcost[2]
      }else if(crops[5]=="winterbarley"||crops[5]=="winteroats"){
        fc5 <- fuelcost[3]; lc5 <- labourcost[3]; opc5 <- opcost[3]
      }else if(crops[5]=="springbarley"||crops[5]=="springoats"){
        fc5 <- fuelcost[4]; lc5 <- labourcost[4]; opc5 <- opcost[4]
      }else if(crops[5]=="winterbeans"){
        fc5<- fuelcost[5]; lc5 <- labourcost[5]; opc5 <- opcost[5]
      }else if(crops[5]=="springbeans"){
        fc5 <- fuelcost[6]; lc5 <- labourcost[6]; opc5 <- opcost[6]
      }else if(crops[5]=="warepotatoes"){
        fc5 <- fuelcost[7]; lc5 <- labourcost[7]; opc5 <- opcost[7]
      }else if(crops[5]=="wosr"){
        fc5 <- fuelcost[8]; lc5 <- labourcost[8]; opc5 <- opcost[8]
      }else if(crops[5]=="sugarbeet"){
        fc5 <- fuelcost[9]; lc5 <- labourcost[9]; opc5 <- opcost[9]
      }else if(crops[5]=="setaside"||crops[5]=="fallow"){
        fc5 <- fuelcost[10]; lc5 <- labourcost[10]; opc5 <- opcost[10]
      }else if(crops[5]=="sosr"){
        fc5 <- fuelcost[11]; lc5 <- labourcost[11]; opc5 <- opcost[11]
      }else if(crops[5]=="winterlinseed"){
        fc5 <- fuelcost[12]; lc5 <- labourcost[12]; opc5 <- opcost[12]
      }else if(crops[5]=="springlinseed"){
        fc5 <- fuelcost[13]; lc5 <- labourcost[13]; opc5 <- opcost[13]
      }else if(crops[5]=="driedpeas"){
        fc5 <- fuelcost[14]; lc5 <- labourcost[14]; opc5 <- opcost[14]
      }
      
      if(crops[6]=="winterwheat"){
        fc6 <- fuelcost[1]; lc6 <- labourcost[1]; opc6 <- opcost[1]
      }else if(crops[6]=="springwheat"){
        fc6 <- fuelcost[2]; lc6 <- labourcost[2]; opc6 <- opcost[2]
      }else if(crops[6]=="winterbarley"||crops[6]=="winteroats"){
        fc6 <- fuelcost[3]; lc6 <- labourcost[3]; opc6 <- opcost[3]
      }else if(crops[6]=="springbarley"||crops[6]=="springoats"){
        fc6 <- fuelcost[4]; lc6 <- labourcost[4]; opc6 <- opcost[4]
      }else if(crops[6]=="winterbeans"){
        fc6 <- fuelcost[5]; lc6 <- labourcost[5]; opc6 <- opcost[5]
      }else if(crops[6]=="springbeans"){
        fc6 <- fuelcost[6]; lc6 <- labourcost[6]; opc6 <- opcost[6]
      }else if(crops[6]=="warepotatoes"){
        fc6 <- fuelcost[7]; lc6 <- labourcost[7]; opc6 <- opcost[7]
      }else if(crops[6]=="wosr"){
        fc6 <- fuelcost[8]; lc6 <- labourcost[8]; opc6 <- opcost[8]
      }else if(crops[6]=="sugarbeet"){
        fc6 <- fuelcost[9]; lc6 <- labourcost[9]; opc6 <- opcost[9]
      }else if(crops[6]=="setaside"||crops[6]=="fallow"){
        fc6 <- fuelcost[10]; lc6 <- labourcost[10]; opc6 <- opcost[10]
      }else if(crops[6]=="sosr"){
        fc6 <- fuelcost[11]; lc6 <- labourcost[11]; opc6 <- opcost[11]
      }else if(crops[6]=="winterlinseed"){
        fc6 <- fuelcost[12]; lc6 <- labourcost[12]; opc6 <- opcost[12]
      }else if(crops[6]=="springlinseed"){
        fc6 <- fuelcost[13]; lc6 <- labourcost[13]; opc6 <- opcost[13]
      }else if(crops[6]=="driedpeas"){
        fc6 <- fuelcost[14]; lc6 <- labourcost[14]; opc6 <- opcost[14]
      }
      
      cr[c11,seq(2,le)]  <- c(fc1,fc2,fc3,fc4,fc5,fc6)
      cr[c12,seq(2,le)]  <- c(lc1,lc2,lc3,lc4,lc5,lc6)
      cr[c13,seq(2,le)]  <- c(opc1,opc2,opc3,opc4,opc5,opc6)
      
      cr[c14,seq(2,le)] <- grossprofit <- cr[c10,seq(2,le)]-cr[c13,seq(2,le)] # Gross profit estimates
      kk4 <- cr[seq(1,31),]
    }
    
    
    # Model output components =====
    #**************************************************************** 
    rotl <- c(2,3,4,5,6)
    if(rotlength%in%rotl==TRUE){
      nrotl <- rotlength
    }else{
      nrotl <- 6
      warning("Maximum 6-year rotation is assumed (Rotation length MUST be between 2 and 6 years)")
    }
    
    
    if(is.null(rotprob)){
      rotp <- 1/nrotl # Rotational probability (6 year rotation)
    }else{
      rotp <- rotprob
    }
    
    di <- 2
    Component=c("Basic Rotation","Cultivation","Farm Output","Fertiliser Cost","Seed Cost","Herbicide Cost",
                "Sundry Cost","Variable Cost","Gross Margin",
                "Fuel Cost","Labour Cost","Operating Cost","Gross Profit","Rotation Profit")
    
    Unit=c("na","na",rep("?/ha",length(Component)-2))
    
    # Row Indices ===
    c1 <- 18; c2 <- 19; c3 <- 20; c4 <- 21; c5 <- 22; c6 <- 23; c7 <- 24; c8 <- 25; c9 <- 26;# === row indices
    c10 <- 27; c11 <- 28; c12 <- 29; c13 <- 30; c14 <- 31; 
    
    h1 <- 10; h2 <- 11; h3 <- 12; h4 <- 13; h5 <- 14; h6 <- 15;
    h7 <- 16; h8 <- 17; 
    
    # Year 1  *************************************************************
    
    crop1 <- crops[1]; crop2 <- crops[2]; crop3 <- crops[3]; crop4 <- crops[4]; crop5 <- crops[5]; crop6 <- crops[6]
    tillage1 <- tillages[1]; tillage2 <- tillages[2]; tillage3 <- tillages[3]; tillage4 <- tillages[4]; tillage5 <- tillages[5];
    tillage6 <- tillages[6]
    
    if(crop1=="winterwheat"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("WWHT",tillages[1],ncb)
      #cp1 <- "WWHT"
      cy1 <- cb[c3]
    }else if(crop1=="springwheat"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("SWHT",tillages[1],ncb) 
      #cp1 <- "SWHT"
      cy1 <- cb[c3]
    }else if (crop1=="winterbarley"){
      cb <- cropBudget(crop=crops, nsoil, subsidy, blackgrass, tillage1, selfrot=0, rotpen=0, yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("WBAR",tillages[1],ncb) 
      #cp1 <- "WBAR"
      cy1 <- cb[c3]
    }else if(crop1=="springbarley"){
      cb <- cropBudget(crop=crops, nsoil, subsidy, blackgrass, tillage1, selfrot=0, rotpen=0, yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("SBAR",tillages[1],ncb) 
      #cp1 <- "SBAR"
      cy1 <- cb[c3]
    }else if(crop1=="winterbeans"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("WBEA",tillages[1],ncb) 
      cp1 <- "WBEA"
      cy1 <- cb[c3]
    }else if(crop1=="springbeans"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("SBEA",tillages[1],ncb) 
      #cp1 <- "SBEA"
      cy1 <- cb[c3]
    }else if(crop1=="warepotatoes"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("WPOT",tillages[1],ncb) 
      #cp1 <- "WPOT"
      cy1 <- cb[c3]
    }else if(crop1=="wosr"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("WOSR",tillages[1],ncb) 
      #cp1 <- "WOSR"
      cy1 <- cb[c3]
    }else if(crop1=="sugarbeet"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,h8)],cb[c1]+cb[c2],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("SBEE",tillages[1],ncb) 
      #cp1 <- "SBEE"
      cy1 <- cb[c3]
    }else if(crop1=="setaside"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("SETA",tillages[1],ncb) 
      #cp1 <- "SETA"
      cy1 <- cb[c3]
    }else if(crop1=="none"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("SETA",tillages[1],ncb) 
      #cp1 <- "SETA"
      cy1 <- cb[c3]
    }else if(crop1=="fallow"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("FALLOW",tillages[1],ncb)
      #cp1 <- "SETA"
      cy1 <- cb[c3]
    }else if(crop1=="sosr"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("SOSR",tillages[1],ncb)
      cy1 <- cb[c3]
    }else if(crop1=="winterlinseed"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("WLIN",tillages[1],ncb)
      cy1 <- cb[c3]
    }else if(crop1=="springlinseed"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("SLIN",tillages[1],ncb)
      cy1 <- cb[c3]
    }else if(crop1=="driedpeas"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("DPEA",tillages[1],ncb)
      cy1 <- cb[c3]
    }
    
    # Year 2  *************************************************************
    # ========= Yield Penalty | Crop Rotations =========
    if(crop2=="winterwheat"){ 
      
      if(crop1=="winterwheat"){
        selfrot <- 10 # 10% Yield loss
      }else{
        selfrot = 0
      }
      if(crop1=="winterbarley"||crop1=="springbarley"||crop1=="springwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("WWHT",tillage2,ncb2) 
      cy2 <- cb2[c3]
    }else if(crop2=="springwheat"){
      if(crop1=="springwheat"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      if(crop1=="winterbarley"||crop1=="springbarley"||crop1=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("SWHT",tillage2,ncb2) 
      cy2 <- cb2[c3]
    }else if (crop2=="winterbarley"){
      if(crop1=="winterbarley"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      if(crop1=="springwheat"||crop1=="springbarley"||crop1=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("WBAR",tillage2,ncb2) 
      cy2 <- cb2[c3]
    }else if(crop2=="springbarley"){
      if(crop1=="springbarley"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      if(crop1=="springwheat"||crop1=="winterbarley"||crop1=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("SBAR",tillage2,ncb2) 
      cy2 <- cb2[c3]
    }else if(crop2=="winterbeans"){
      selfrot <- 0
      if(crop1=="wosr"||crop1=="sugarbeet"||crop1=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("WBEA",tillage2,ncb2) 
      cy2 <- cb2[c3]
    }else if(crop2=="springbeans"){
      selfrot <- 0
      if(crop1=="wosr"||crop1=="sugarbeet"||crop1=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("SBEA",tillage2,ncb2) 
      cy2 <- cb2[c3]
    }else if(crop2=="warepotatoes"){
      if(crop1=="warepoatoes"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Potatoes is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop1=="sugarbeet"){ 
        rotpen <- 5
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("WPOT",tillage2,ncb2)  
      cy2 <- cb2[c3]
    }else if(crop2=="wosr"){
      if(crop1=="wosr"||crop1=="sosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop1=="winterbeans"||crop1=="springbeans"||crop1=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("WOSR",tillage2,ncb2)  
      cy2 <- cb2[c3]
    }else if(crop2=="sugarbeet"){
      if(crop1=="sugarbeat"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Sugarbeet is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop1=="winterbeans"||crop1=="springbeans"||crop1=="wosr"||crop1=="warepotatoes"||crop1=="sosr"){ 
        rotpen <- 100
        warning("Oilseed rape, Beans or Potatoes--Sugarbeet sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,h8)],cb2[c1]+cb2[c2],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("SBEE",tillage2,ncb2)  
      cy2 <- cb2[c3]
    }else if(crop2=="setaside"){ 
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot=0,rotpen=0,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("SETA",tillage2,ncb2)
      cy2 <- cb2[c3]
    }else if(crop2=="none"){
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot=0,rotpen=0,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("SETA",tillage2,ncb2)
      cy2 <- cb2[c3]
    }else if(crop2=="fallow"){
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot=0,rotpen=0,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("FALLOW",tillage2,ncb2) 
      cy2 <- cb2[c3]
    }else if(crop2=="sosr"){
      if(crop1=="sosr"||crop1=="wosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot = 0
      }
      if(crop1=="winterbeans"||crop1=="springbeans"||crop1=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("SOSR",tillage2,ncb2) 
      cy2 <- cb2[c3]
    }else if(crop2=="winterlinseed"){
      if(crop1=="winterlinseed"||crop1=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop1=="winterbeans"||crop1=="springbeans"||crop1=="sugarbeet"||crop1=="driedpeas"||crop1=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Linseed sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("WLIN",tillage2,ncb2) 
      cy2 <- cb2[c3]
    }else if(crop2=="springlinseed"){
      if(crop1=="winterlinseed"||crop1=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop1=="winterbeans"||crop1=="springbeans"||crop1=="sugarbeet"||crop1=="driedpeas"||crop1=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Linseed sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("SLIN",tillage2,ncb2)
      cy2 <- cb2[c3]
    }else if(crop2=="driedpeas"){
      if(crop1=="driedpeas"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Peas is penalised")
      }else{
        selfrot <- 0
      }
      if(crop1=="wosr"||crop1=="sosr"||crop1=="setaside"){ 
        rotpen <- 100
        warning("OSR and  Rotational Set-aside--Peas sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("DPEA",tillage2,ncb2)
      cy2 <- cb2[c3]
    }
    
    
    # Year 3  *************************************************************
    
    if(crop3=="winterwheat"){
      if(crop2=="winterwheat"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      if(crop2=="winterbarley"||crop2=="springbarley"||crop2=="springwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("WWHT",tillage3,ncb3) 
      cy3 <- cb3[c3]
    }else if(crop3=="springwheat"){
      if(crop2=="springwheat"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      
      if(crop2=="winterbarley"||crop2=="springbarley"||crop2=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("SWHT",tillage3,ncb3) 
      cy3 <- cb3[c3]
    }else if (crop3=="winterbarley"){
      if(crop2=="winterbarley"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      if(crop2=="springwheat"||crop2=="springbarley"||crop2=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("WBAR",tillage3,ncb3) 
      cy3 <- cb3[c3]
    }else if(crop3=="springbarley"){
      if(crop2=="springbarley"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      if(crop2=="springwheat"||crop2=="winterbarley"||crop2=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("SBAR",tillage3,ncb3) 
      cy3 <- cb3[c3]
    }else if(crop3=="winterbeans"){
      selfrot <- 0
      if(crop2=="wosr"||crop2=="sugarbeet"||crop2=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("WBEA",tillage3,ncb3) 
      cy3 <- cb3[c3]
    }else if(crop3=="springbeans"){
      selfrot <- 0
      if(crop2=="wosr"||crop2=="sugarbeet"||crop2=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("SBEA",tillage3,ncb3) 
      cy3 <- cb3[c3]
    }else if(crop3=="warepotatoes"){
      if(crop2=="warepoatoes"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Potatoes is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop2=="sugarbeet"){ 
        rotpen <- 5
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("WPOT",tillage3,ncb3)  
      cy3 <- cb3[c3]
    }else if(crop3=="wosr"){
      if(crop2=="wosr"||crop2=="sosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot <- 0
      }
      
      if(crop2=="winterbeans"||crop2=="springbeans"||crop2=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("WOSR",tillage3,ncb3)  
      cy3 <- cb3[c3]
    }else if(crop3=="sugarbeet"){
      if(crop2=="sugarbeat"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Sugarbeet is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop2=="winterbeans"||crop2=="springbeans"||crop2=="wosr"||crop2=="warepotatoes"||crop2=="sosr"){ 
        rotpen <- 100
        warning("Sugarbeet--Oilseed rape, Beans or Potatoes sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,h8)],cb3[c1]+cb3[c2],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("SBEE",tillage3,ncb3)  
      cy3 <- cb3[c3]
    }else if(crop3=="setaside"||crop3=="none"){ 
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot=0,rotpen=0,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("SETA",tillage3,ncb3) 
      cy3 <- cb3[c3]
    }else if(crop3=="none"){
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot=0,rotpen=0,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("SETA",tillage3,ncb3) 
      cy3 <- cb3[c3]
    }else if(crop3=="fallow"){
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot=0,rotpen=0,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("FALLOW",tillage3,ncb3)
      cy3 <- cb3[c3]
    }else if(crop3=="sosr"){
      if(crop2=="wosr"||crop2=="sosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot = 0
      }
      if(crop2=="winterbeans"||crop2=="springbeans"||crop2=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("SOSR",tillage3,ncb3)
      cy3 <- cb3[c3]
    }else if(crop3=="winterlinseed"){
      if(crop2=="winterlinseed"||crop2=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop2=="winterbeans"||crop2=="springbeans"||crop2=="sugarbeet"||crop2=="driedpeas"||crop2=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("WLIN",tillage3,ncb3)
      cy3 <- cb3[c3]
    }else if(crop3=="springlinseed"){
      if(crop2=="winterlinseed"||crop2=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop2=="winterbeans"||crop2=="springbeans"||crop2=="sugarbeet"||crop2=="driedpeas"||crop2=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("SLIN",tillage3,ncb3) 
      cy3 <- cb3[c3]
    }else if(crop3=="driedpeas"){
      if(crop2=="driedpeas"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Peas is penalised")
      }else{
        selfrot <- 0
      }
      if(crop2=="wosr"||crop2=="sosr"||crop2=="setaside"){ 
        rotpen <- 100
        warning("OSR and Rotational Set-aside--Peas sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("DPEA",tillage3,ncb3) 
      cy3 <- cb3[c3]
    } 
    
    
    # Year 4  *************************************************************
    
    if(crop4=="winterwheat"){
      
      if(crop3=="winterwheat"){
        selfrot <- 15 # 11% Yield loss
      }else{
        selfrot = 0
      }
      if(crop3=="winterbarley"||crop3=="springbarley"||crop3=="springwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("WWHT",tillage4,ncb4) 
      cy4 <- cb4[c3]
    }else if(crop4=="springwheat"){
      if(crop3=="springwheat"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      
      if(crop3=="winterbarley"||crop3=="springbarley"||crop3=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("SWHT",tillage4,ncb4) 
      cy4 <- cb4[c3]
    }else if (crop4=="winterbarley"){
      if(crop3=="winterbarley"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      if(crop3=="springwheat"||crop3=="springbarley"||crop3=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("WBAR",tillage4,ncb4) 
      cy4 <- cb4[c3]
    }else if(crop4=="springbarley"){
      if(crop3=="springbarley"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      if(crop3=="springwheat"||crop3=="winterbarley"||crop3=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("SBAR",tillage4,ncb4) 
      cy4 <- cb4[c3]
    }else if(crop4=="winterbeans"){
      selfrot <- 0
      if(crop3=="wosr"||crop3=="sugarbeet"||crop3=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("WBEA",tillage4,ncb4) 
      cy4 <- cb4[c3]
    }else if(crop4=="springbeans"){
      selfrot <- 0
      if(crop3=="wosr"||crop3=="sugarbeet"||crop3=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("SBEA",tillage4,ncb4) 
      cy4 <- cb4[c3]
    }else if(crop4=="warepotatoes"){
      if(crop3=="warepoatoes"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Potatoes is penalised")
      }else{
        selfrot = 0
      }
      if(crop3=="sugarbeet"){ 
        rotpen <- 5
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("WPOT",tillage4,ncb4)  
      cy4 <- cb4[c3]
    }else if(crop4=="wosr"){
      if(crop3=="wosr"||crop3=="sosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot = 0
      }
      if(crop3=="winterbeans"||crop3=="springbeans"||crop3=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("WOSR",tillage4,ncb4)  
      cy4 <- cb4[c3]
    }else if(crop4=="sugarbeet"){
      if(crop3=="sugarbeat"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Sugarbeet is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop3=="winterbeans"||crop3=="springbeans"||crop3=="wosr"||crop3=="warepotatoes"||crop3=="sosr"){ 
        rotpen <- 100
        warning("Sugarbeet--Oilseed rape, Beans or Potatoes sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,h8)],cb4[c1]+cb4[c2],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("SBEE",tillage4,ncb4)  
      cy4 <- cb4[c3]
    }else if(crop4=="setaside"){ 
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot=0,rotpen=0,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("SETA",tillage4,ncb4)
      cy4 <- cb4[c3]
    }else if(crop4=="none"){
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot=0,rotpen=0,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("SETA",tillage4,ncb4)
      cy4 <- cb4[c3]
    }else if(crop4=="fallow"){
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot=0,rotpen=0,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("FALLOW",tillage4,ncb4)
      cy4 <- cb4[c3]
    }else if(crop4=="sosr"){
      if(crop3=="wosr"||crop3=="sosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot = 0
      }
      if(crop3=="winterbeans"||crop3=="springbeans"||crop3=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("SOSR",tillage4,ncb4) 
      cy4 <- cb4[c3]
    }else if(crop4=="winterlinseed"){
      if(crop3=="winterlinseed"||crop3=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop3=="winterbeans"||crop3=="springbeans"||crop3=="sugarbeet"||crop3=="driedpeas"||crop3=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Linseed sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("WLIN",tillage4,ncb4)
      cy4 <- cb4[c3]
    }else if(crop4=="springlinseed"){
      if(crop3=="winterlinseed"||crop3=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop3=="winterbeans"||crop3=="springbeans"||crop3=="sugarbeet"||crop3=="driedpeas"||crop3=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Linseed sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("SLIN",tillage4,ncb4)
      cy4 <- cb4[c3]
    }else if(crop4=="driedpeas"){
      if(crop3=="driedpeas"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Peas is penalised")
      }else{
        selfrot <- 0
      }
      if(crop3=="wosr"||crop3=="sosr"||crop3=="setaside"){ 
        rotpen <- 100
        warning("OSR and Rotational Set-aside--Peas sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("DPEA",tillage4,ncb4)
      cy4 <- cb4[c3]
    }
    
    # Year 5  *************************************************************
    
    if(crop5=="winterwheat"){
      
      if(crop4=="winterwheat"){
        selfrot <- 15 # 15% Yield loss
      }else{
        selfrot = 0
      }
      
      if(crop4=="winterbarley"||crop4=="springbarley"||crop4=="springwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("WWHT",tillage5,ncb5) 
      cy5 <- cb5[c3]
    }else if(crop5=="springwheat"){
      if(crop4=="springwheat"){
        selfrot <- 11 # 
      }else{
        selfrot = 0
      }
      
      if(crop4=="winterbarley"||crop4=="springbarley"||crop4=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("SWHT",tillage5,ncb5) 
      cy5 <- cb5[c3]
    }else if (crop5=="winterbarley"){
      if(crop4=="winterbarley"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      
      if(crop4=="springwheat"||crop4=="springbarley"||crop4=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("WBAR",tillage5,ncb5) 
      cy5 <- cb5[c3]
    }else if(crop5=="springbarley"){
      if(crop4=="springbarley"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      
      if(crop4=="springwheat"||crop4=="winterbarley"||crop4=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("SBAR",tillage5,ncb5) 
      cy5 <- cb5[c3]
    }else if(crop5=="winterbeans"){
      selfrot <- 0
      if(crop4=="wosr"||crop4=="sugarbeet"||crop4=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("WBEA",tillage5,ncb5) 
      cy5 <- cb5[c3]
    }else if(crop5=="springbeans"){
      selfrot <- 0
      if(crop4=="wosr"||crop4=="sugarbeet"||crop4=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("SBEA",tillage5,ncb5) 
      cy5 <- cb5[c3]
    }else if(crop5=="warepotatoes"){
      if(crop4=="warepoatoes"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Potatoes is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop4=="sugarbeet"){ 
        rotpen <- 5
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("WPOT",tillage5,ncb5)  
      cy5 <- cb5[c3]
    }else if(crop5=="wosr"){
      if(crop4=="wosr"||crop4=="sosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop4=="winterbeans"||crop4=="springbeans"||crop4=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("WOSR",tillage5,ncb5)  
      cy5 <- cb5[c3]
    }else if(crop5=="sugarbeet"){
      if(crop4=="sugarbeat"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Sugarbeet is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop4=="winterbeans"||crop4=="springbeans"||crop4=="wosr"||crop4=="warepotatoes"||crop4=="sosr"){ 
        rotpen <- 100
        warning("Oilseed rape, Beans or Potatoes--Sugarbeet sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,h8)],cb5[c1]+cb5[c2],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("SBEE",tillage5,ncb5)  
      cy5 <- cb5[c3]
    }else if(crop5=="setaside"){ 
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot=0,rotpen=0,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("SETA",tillage5,ncb5) 
      cy5 <- cb5[c3]
    }else if(crop5=="none"){
      ccb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot=0,rotpen=0,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("SETA",tillage5,ncb5)
      cy5 <- cb5[c3]
    }else if(crop5=="fallow"){
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot=0,rotpen=0,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("FALLOW",tillage5,ncb5)
      cy5 <- cb5[c3]
    }else if(crop5=="sosr"){
      if(crop4=="wosr"||crop4=="sosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot = 0
      }
      if(crop4=="winterbeans"||crop4=="springbeans"||crop4=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("SOSR",tillage5,ncb5)
      cy5 <- cb5[c3]
    }else if(crop5=="winterlinseed"){
      if(crop4=="winterlinseed"||crop4=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop4=="winterbeans"||crop4=="springbeans"||crop4=="sugarbeet"||crop4=="driedpeas"||crop4=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Linseed sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("WLIN",tillage5,ncb5)
      cy5 <- cb5[c3]
    }else if(crop5=="springlinseed"){
      if(crop4=="winterlinseed"||crop4=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop4=="winterbeans"||crop4=="springbeans"||crop4=="sugarbeet"||crop4=="driedpeas"||crop4=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Linseed sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("SLIN",tillage5,ncb5) 
      cy5 <- cb5[c3]
    }else if(crop5=="driedpeas"){
      if(crop4=="driedpeas"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Peas is penalised")
      }else{
        selfrot <- 0
      }
      if(crop4=="wosr"||crop4=="sosr"||crop4=="setaside"){ 
        rotpen <- 100
        warning("OSR and Rotational Set-aside--Peas sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("DPEA",tillage5,ncb5)
      cy5 <- cb5[c3]
    }
    
    
    # Year 6  *************************************************************
    
    if(crop6=="winterwheat"){
      
      if(crop5=="winterwheat"){
        selfrot <- 17 # 17% Yield loss
      }else{
        selfrot = 0
      }
      if(crop5=="winterbarley"||crop5=="springbarley"||crop5=="springwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("WWHT",tillage6,ncb6) 
      cy6 <- cb6[c3]
    }else if(crop6=="springwheat"){
      if(crop5=="springwheat"){
        selfrot <- 12 # 12% Yield loss
      }else{
        selfrot = 0
      }
      
      if(crop5=="winterbarley"||crop5=="springbarley"||crop5=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("SWHT",tillage6,ncb6) 
      cy6 <- cb6[c3]
    }else if (crop6=="winterbarley"){
      if(crop5=="winterbarley"){
        selfrot <- 12 # 12% Yield loss
      }else{
        selfrot = 0
      }
      
      if(crop5=="springwheat"||crop5=="springbarley"||crop5=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("WBAR",tillage6,ncb6) 
      cy6 <- cb6[c3]
    }else if(crop6=="springbarley"){
      if(crop5=="springbarley"){
        selfrot <- 12 # 12% Yield loss
      }else{
        selfrot = 0
      }
      
      if(crop5=="springwheat"||crop5=="winterbarley"||crop5=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("SBAR",tillage6,ncb6) 
      cy6 <- cb6[c3]
    }else if(crop6=="winterbeans"){
      selfrot <- 0
      if(crop5=="wosr"||crop5=="sugarbeet"||crop5=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("WBEA",tillage6,ncb6) 
      cy6 <- cb6[c3]
    }else if(crop6=="springbeans"){
      selfrot <- 0
      if(crop5=="wosr"||crop5=="sugarbeet"||crop5=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("SBEA",tillage6,ncb6) 
      cy6 <- cb6[c3]
    }else if(crop6=="warepotatoes"){
      if(crop5=="warepoatoes"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Potatoes is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop5=="sugarbeet"){ 
        rotpen <- 5
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("WPOT",tillage6,ncb6)  
      cy6 <- cb6[c3]
    }else if(crop6=="wosr"){
      if(crop5=="wosr"||crop5=="sosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot = 0
      }
      if(crop5=="winterbeans"||crop5=="springbeans"||crop5=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("WOSR",tillage6,ncb6)  
      cy6 <- cb6[c3]
    }else if(crop6=="sugarbeet"){
      if(crop5=="sugarbeet"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Sugarbeet is penalised")
      }else{
        selfrot = 0
      }
      if(crop5=="winterbeans"||crop5=="springbeans"||crop5=="wosr"||crop5=="warepotatoes"||crop5=="sosr"){ 
        rotpen <- 100
        warning("Oilseed rape, Beans or Potatoes--Sugarbeet sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,h8)],cb6[c1]+cb6[c2],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("SBEE",tillage6,ncb6)  
      cy6 <- cb6[c3]
    }else if(crop6=="setaside"){ 
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot=0,rotpen=0,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("SETA",tillage6,ncb6)
      cy6 <- cb6[c3]
    }else if(crop6=="none"){
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot=0,rotpen=0,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("SETA",tillage6,ncb6) # c(rep(0,14))
      cy6 <- cb6[c3]
    }else if(crop6=="fallow"){
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot=0,rotpen=0,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("FALLOW",tillage6,ncb6) 
      cy6 <- cb6[c3]
    }else if(crop6=="sosr"){
      if(crop5=="wosr"||crop5=="sosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot = 0
      }
      if(crop5=="winterbeans"||crop5=="springbeans"||crop5=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("SOSR",tillage6,ncb6)
      cy6 <- cb6[c3]
    }else if(crop6=="winterlinseed"){
      if(crop5=="winterlinseed"||crop5=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop5=="winterbeans"||crop5=="springbeans"||crop5=="sugarbeet"||crop5=="driedpeas"||crop5=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Linseed sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("WLIN",tillage6,ncb6) 
      cy6 <- cb6[c3]
    }else if(crop6=="springlinseed"){
      if(crop5=="winterlinseed"||crop5=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop5=="winterbeans"||crop5=="springbeans"||crop5=="sugarbeet"||crop5=="driedpeas"||crop5=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Linseed sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("SLIN",tillage6,ncb6) 
      cy6 <- cb6[c3]
    }else if(crop6=="driedpeas"){
      if(crop5=="driedpeas"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Peas is penalised")
      }else{
        selfrot <- 0
      }
      if(crop5=="wosr"||crop5=="sosr"||crop5=="setaside"){ 
        rotpen <- 100
        warning("OSR and  Rotational Set-aside--Peas sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("DPEA",tillage6,ncb6)
      cy6 <- cb6[c3]
    }
    
    # Rotation lenth: Maximum 6 year rotation is assumed
    
    if(farm=="single"){
      
      d1 <- data.frame(Component,Unit,Year1,Year2,Year3,Year4,Year5,Year6)
      nrotl <- rotlength
      if(nrotl==2){
        d1[,seq(-5,-8)]
      }else if(nrotl==3){
        d1[,seq(-6,-8)]
      }else if(nrotl==4){
        d1[,seq(-7,-8)]
      }else if(nrotl==5){
        d1[,-8]
      }else if(nrotl==6){
        d1
      }
      
    }else if(farm=="multiple"){
      d2 <- c(cy1,ncb,cy2,ncb2,cy3,ncb3,cy4,ncb4,cy5,ncb5,cy6,ncb6)
      #d2
      if(nrotl==2){
        d3 <- d2[seq(-27,-78)]
        d4 <- c(d3, sum(d3[13],d3[26]))
      }else if(nrotl==3){
        d3 <- d2[seq(-40,-78)]
        d4 <- c(d3, sum(d3[13],d3[26],d3[39]))
      }else if(nrotl==4){
        d3 <- d2[seq(-53,-78)]
        d4 <- c(d3, sum(d3[13],d3[26],d3[39],d3[52]))
      }else if(nrotl==5){
        d3 <- d2[seq(-66,-78)]
        d4 <- c(d3, sum(d3[13],d3[26],d3[39],d3[52],d3[65]))
      }else if(nrotl==6){
        d3 <- d2
        d4 <- c(d3, sum(d3[13],d3[26],d3[39],d3[52],d3[65],d3[78]))
        
      }
    }
  }
  #************************
  if(farm=="single"){
    
    ecm <- ECOMOD(farm="single",default,soil,rotlength,rotprob,crops,tillages,seedrate,delsowing,Nfert,Pfert,Kfert,bgherbdose,
                  glyphosatedose,numberofsprays,subsidy,blackgrass,cropprice,cropyield,yieldoption,Nfertprice,
                  Pfertprice,Kfertprice,seedprice,herbprice,glyphosateprice,machsize,fuelprice,labourwage)
    
  }else if(farm=="multiple"&is.null(default)){
    fd <- farmdata
    
    est <- c("yield","output","fertcost","seedcost","herbcost","sundry","varcost","gmargin","fuelcost",
             "labcost","opcost","grossprof","rotgrossprof")
    
    cp1 <- "crop.1" #crops[1]
    cp2 <- "crop.2" #crops[2]
    cp3 <- "crop.3" #crops[3]
    cp4 <- "crop.4" #crops[4]
    cp5 <- "crop.5" #crops[5]
    cp6 <- "crop.6" #crops[6]
    
    v1 <- ".1"
    v2 <- ".2"
    v3 <- ".3"
    v4 <- ".4"
    v5 <- ".5"
    v6 <- ".6"    
    
    p1 <- paste(est,v1,sep="")
    p2 <- paste(est,v2,sep="")
    p3 <- paste(est,v3,sep="")
    p4 <- paste(est,v4,sep="")
    p5 <- paste(est,v5,sep="")
    p6 <- paste(est,v6,sep="")
    
    hd <- c(p1,p2,p3,p4,p5,p6)
    
    nrotl <- rotlength
    if(nrotl==2){
      hd1 <- hd[seq(-27,-78)]
    }else if(nrotl==3){
      hd1 <- hd[seq(-40,-78)]
    }else if(nrotl==4){
      hd1 <- hd[seq(-53,-78)]
    }else if(nrotl==5){
      hd1 <- hd[seq(-66,-78)]
    }else if(nrotl==6){
      hd1 <- hd
    }
    
    cp <- fd[,seq(5,10)]
    ti <- fd[,seq(11,16)]
    ds <- fd[,seq(23,28)]
    sb <- as.character(fd[,65])
    bg <- fd[,seq(66,71)]
    yo <- as.character(fd[,84])
    
    
    for(k in 1:ncol(cp)){
      if(class(cp[,k])=="factor"){cp[,k] <- as.character(cp[,k])}
    }
    
    for(k in 1:ncol(ti)){
      if(class(ti[,k])=="factor"){ti[,k] <- as.character(ti[,k])}
    }
    
    for(k in 1:ncol(ds)){
      if(class(ds[,k])=="factor"){ds[,k] <- as.character(ds[,k])}
    }
    
    
    for(k in 1:ncol(bg)){
      if(class(bg[,k])=="factor"){bg[,k] <- as.character(bg[,k])}
    }
    
    
    
    
    fms <- c()
    
    index <- c(seq(1, length(farmdata[,1])))
    
    for(i in index){
      fms <- c(fms,ECOMOD(farm="multiple",default,soil=fd[i,3],rotlength,rotprob,crops=c(cp[,1][i],cp[,2][i],cp[,3][i],cp[,4][i],cp[,5][i],cp[,6][i]),
                          tillage=c(ti[,1][i],ti[,2][i],ti[,3][i],ti[,4][i],ti[,5][i],ti[,6][i]),seedrate=as.numeric(fd[i,seq(17,22)]),
                          delsowing=c(ds[,1][i],ds[,2][i],ds[,3][i],ds[,4][i],ds[,5][i],ds[,6][i]),
                          Nfert=as.numeric(fd[i,seq(29,34)]),Pfert=as.numeric(fd[i,seq(35,40)]),Kfert=as.numeric(fd[i,seq(41,46)]),
                          bgherbdose=as.numeric(fd[i,seq(47,52)]), 
                          glyphosatedose=as.numeric(fd[i,seq(53,58)]),numberofsprays=as.numeric(fd[i,seq(59,64)]),subsidy=sb[i],
                          blackgrass=c(bg[,1][i],bg[,2][i],bg[,3][i],bg[,4][i],bg[,5][i],bg[,6][i]), 
                          cropprice=as.numeric(fd[i,seq(72,77)]),
                          cropyield=as.numeric(fd[i,seq(78,83)]),yieldoption=yo[i], #yieldoption, 
                          Nfertprice=as.numeric(fd[i,85]),Pfertprice=as.numeric(fd[i,86]),
                          Kfertprice=as.numeric(fd[i,87]),
                          seedprice=as.numeric(fd[i,seq(88,93)]),herbprice=as.numeric(fd[i,seq(94,99)]),glyphosateprice=as.numeric(fd[i,seq(100,105)]),
                          machsize=as.numeric(fd[i,seq(106,110)]),fuelprice=as.numeric(fd[i,111]),
                          labourwage=as.numeric(fd[i,112])))
      # subsidy==fd[i,70]
      # yieldoption==fd[i,89] or yo[i]
      #print(i)
      fms1 <- fms
      cols <- c(hd1,"totalrotgrossprof")
      ros <- as.character(index)
      
      mat <- suppressWarnings(matrix(fms1,ncol=length(cols),nrow=length(index),byrow=T,dimnames=list(ros,cols)))
      Field_no <- index
      field_name <- fd[,2]
      ecm <- data.frame(Field_no,field_name,fd[,seq(5,10)],mat)
      
      # NAME OF THE FILE CAN BE CHANGED TO WHICHEVER NAME DESIRED *********
      # e.g. "Farms_Fields_Economic_Outcomes.csv" as in Supplementary Information
      write.table(ecm,file=filename,row.names=FALSE,sep=",")
      
    }
  }else if(farm=="multiple"&default=="yes"){
    stop("RUNNING MODEL FOR MULTIPLE FIELDS CANNOT BE BASED ON DEFAULT VALUES: SET default to NULL")
  }
  
  ka <- ecm
} 


## UPPER LIMITS ----

##    ECOMOD - upper limits, estimated yield ----

solve_BGRI_ECOMOD_u_est <- function(filename,farmdata,default,farm="single",soil,rotlength,rotprob=NULL,crops,tillages,seedrate,delsowing,Nfert,Pfert,Kfert,bgherbdose,
                                glyphosatedose,numberofsprays,subsidy="yes",blackgrass,cropprice,cropyield,yieldoption="estimate",Nfertprice,
                                Pfertprice,Kfertprice,seedprice,herbprice,glyphosateprice,machsize,fuelprice,labourwage){  
  
  # THE ORDER OF MODEL INPUTS SUCH AS TILLAGE, SEED RATE N FERTILISER RATES ETC MUST CORRESPOND TO THE ORDER OF CROPS
  # FOR EXAMPLE IF FIRST CROP IS WINTER WHEAT, THEN IN THE NFERT VECTOR, FIRST VALUE MUST CORRESPOND TO WINTER WHEAT N FERTILISER RATE.
  
  # Machine size (machsize) vector MUST be entered in the following order: 1. Tractor szie (kW); 2. Roller size (m);
  # 3. Power harrow size (m); 4. Sprayer tank size (litres); 5. Combine harvester size (kW)
  # For example machsize = c(102,6,4,1400,125)
  
  
  if(farm=="multiple"&is.null(farmdata)){
    stop("RUNNING MODEL FOR MULTIPLE FIELDS CANNOT BE BASED ON SINGLE FIELD/FARM DATA: USE MULTIPLE FIELD/FARM DATA & EQUATE farmdata TO THE DATA")
  }
  
  
  ECOMOD <- function(farm,default,soil,rotlength,rotprob,crops,tillages,seedrate,delsowing,Nfert,Pfert,Kfert,bgherbdose,
                     glyphosatedose,numberofsprays,subsidy,blackgrass,cropprice,cropyield,yieldoption,Nfertprice,
                     Pfertprice,Kfertprice,seedprice,herbprice,glyphosateprice,machsize,fuelprice,labourwage){
    
    # Setting the model to generate default results ******
    
    if(is.null(default)){
      soil <- soil
      rotlength <- rotlength
      crops <- crops
      tillages <- tillages
      seedrate <- seedrate
      delsowing <- delsowing
      Nfert <- Nfert
      Pfert <- Pfert
      Kfert <- Kfert
      bgherbdose <- bgherbdose
      glyphosatedose <- glyphosatedose
      numberofsprays <- numberofsprays
      blackgrass <- blackgrass
      cropprice <- cropprice
      cropyield <- cropyield
      Nfertprice <- Nfertprice
      Pfertprice <- Pfertprice
      Kfertprice <- Kfertprice
      seedprice <- seedprice
      herbprice <- herbprice 
      glyphosateprice <- glyphosateprice
      machsize <- machsize
      fuelprice <- fuelprice
      labourwage <- labourwage
      
    }else if(default=="yes"){ # DEFAULT MODEL INPUT DATA *******************
      soil <- 2.5
      crops <- c("winterwheat","winterwheat","wosr","winterwheat","springbeans","winterwheat")
      tillages <- c("lightcultivation","lightcultivation","deepcultivation","deepcultivation","deepcultivation","deepcultivation")
      seedrate <- c(185,175,3.5,185,334,185)
      delsowing <- c("late","no","late","no","late","late")
      Nfert <- c(180,188,167,190,0,174)
      Pfert <- c(95,95,80,95,70,95)
      Kfert <- c(115,115,70,115,70,115)
      bgherbdose <- c(3.16,7.24,2.54,10.97,10.17,1.7)
      glyphosatedose <- c(2,1.6,4,2.23,4.1,0.98) 
      numberofsprays <- c(3,3,4,4,6,4)
      blackgrass <- c("low","low","low","low","low","low") #**** Needs changing
      cropprice <- c(150,150,335,150,185,150)
      cropyield <- c(9.1,9,3.91,9.8,5.9,10)
      Nfertprice <- 0.78
      Pfertprice <- 0.71
      Kfertprice <- 0.44
      seedprice <- c(0.36,0.36,7.34,0.36,0.38,0.36)
      herbprice <- c(19.5,19.5,19.5,19.5,19.5,19.5) # An average from a farm data
      glyphosateprice <- c(2.43,2.43,2.43,2.43,2.43,2.43) # An average from a farm data
      machsize <- c(102,6,4,1400,125) # Assumed machine sizes
      fuelprice <- 0.625 # ABC Nov 2018, 87th ed
      labourwage <- 10.08 # Nix 2019, 49th ed
      subsidy <- "yes"
    }
    
    #************* Rotation Length Assumption *****************
    lcr <- 6 #rotlength = 6 # Length of vector based on max years of rotation which is 6
    if(length(crops)>lcr){
      warning("Maximum 6-year rotation is assumed (Length of crops vector MUST be 6)")
    }else{
      crops <- crops
    }
    
    if(length(tillages)>6){
      warning("Length of tillage vector MUST be 6")
    }else{
      tillages <- tillages
    }
    
    if(length(machsize)>5){
      warning("Length of machine sizes vector MUST be 5")
      # The machine size must be set in the following order: Tractor size (kW), roller size (m),
      # power harrow size (m), Sprayer size (litres), combine harvester size (kW)
    }else{
      tillages <- tillages
    }
    
    if(length(Nfert)>lcr||length(Pfert)>lcr||length(Kfert)>lcr||length(seedrate)>lcr||length(seedprice)>lcr){
      warning("Maximum 6-year rotation is assumed (Length of crops vector MUST be 6)")
    }else{
      Nfert <- Nfert; Pfert <- Pfert; Kfert <- Kfert; seedrate <- seedrate; seedprice <- seedprice
    }
    
    if(length(bgherbdose)>lcr||length(glyphosatedose)>lcr||length(numberofsprays)>lcr||
       length(cropprice)>lcr||length(cropyield)>lcr||length(seedprice)>lcr||length(herbprice)>lcr){
      warning("Maximum 6-year rotation is assumed (Length of crops vector MUST be 6)")
    }else{
      bgherbdose <- bgherbdose; glyphosatedose <- glyphosatedose; numberofsprays <- numberofsprays;
      cropprice <- cropprice; cropyield <- cropyield; seedprice <- seedprice; herbprice <- herbprice
    }
    
    # Setting soil type
    soi <- c(0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5) # Soil type indices
    if(soil%in%soi==TRUE){
      nsoil <- soil
    }else{
      nsoil <- 2.5 # This sets the soil type to heavy soil (Clay)
      warning("Enter correct soil type: must be between 0.5 and 2.5 at an interval of 0.25. Check that input soil values are formatted as 'numeric' and have 2 dec pl.")
    }
    
    cropBudget <- function(crop,asoil,subsidy,blackgrass,tillage,selfrot,rotpen,yieldoption){ #***********************************************************
      
      # This function estimates gross margin (£/ha) and gross profit (£/ha) for each crop taking into
      # consideration soil type, yield loss due to black-grass infestation,
      # farm operation and machine types, and fuel price.
      
      # Creation of Data files *******
      Input_Output <- c("N Fertiliser","P Fertiliser","K Fertiliser",
                        "N Fertiliser Price","P Fertiliser Price","K Fertiliser Price",
                        "Seed Rate","Seed Price","BG Herbicide Rate","BG Herbicide Price",
                        "Glyphosate Rate", "Glyphosate Price","Number of Sprays",
                        "Sugarbeet Transport","Fertiliser Cost","Seed Cost",
                        "Herbicide Cost","Sundry Cost","Sugarbeet Transport Cost",
                        "Primary Yield","Primary Yield Price",
                        "Secondary Yield","Secondary Yield Price",
                        "Subsidy","Farm Output","Variable Cost",
                        "Gross Margin","Fuel Cost","Labour Cost","Operating Cost",
                        "Gross Profit") # Crop inputs and output parameters
      lcr <- 6 # Equals the number of crops
      cl <- c(rep(0,length(Input_Output)*lcr))
      cd <- matrix(cl, ncol=lcr, byrow = T)
      cols <- c("Input_Output",crops)
      cd1 <- data.frame(Input_Output,cd)
      nfp <- c(rep(Nfertprice,lcr)); pfp <- c(rep(Pfertprice,lcr)); kfp <- c(rep(Kfertprice,lcr))
      
      inp <- rbind(Nfert,Pfert,Kfert,nfp,pfp,kfp,seedrate,seedprice,bgherbdose,herbprice,glyphosatedose,
                   glyphosateprice,numberofsprays) # Putting input data together to create crop data
      
      cd1[seq(1,13),seq(2,7)] <- inp
      cd1[20,seq(2,7)] <- cropyield
      cd1[21,seq(2,7)] <- cropprice
      colnames(cd1) <- cols
      crd <- fi1 <- cd1
      
      # Updated 07/02/2023. Assumed sundry costs are informed by Nix (2019) and ABC (2019).
      # Sundry costs include the average per hectare cost of herbicides NOT targeting BG, from the BGRI data, 
      # as well as cost of other chemicals such as fungicide, insecticides, growth regulators etc.,
      # plus marketing, agronomy costs, in-store chemicals. Data are from 'BGRI_ECOMOD_Chem & Sundry Costs_2023-02-07.xlsx".
      #snwwt <- 146; snswt <- 95; snwba <- 108; snsba <- 81; snwbe <- 101; snsbe <- 89; snwpo <- 1200; snwos <- 110; snsbt <- 409; snset <- 2;
      #snsos <- 67; sndpe <- 156; snwln <- 75; snsln <- 73; snwoa <- 80; snsoa <- 89; 
      snwwt <- 182; snswt <- 100; snwba <- 158; snsba <- 151; snwbe <- 91; snsbe <- 81; snwpo <- 2215; snwos <- 192; snsbt <- 488; snset <- 2;
      snsos <- 140; sndpe <- 155; snwln <- 88; snsln <- 53; snwoa <- 148; snsoa <- 109;  
      
      
      
      # Yield estimates based on soil type and N fertiliser amounts (Response functions obtained from SAFMOD)
      wwt <- round(((11.841-(9.211*(0.9907^(fi1$winterwheat[1])))-(0.0075*(fi1$winterwheat[1])))*(0.743+0.1714*(nsoil))*0.947),2)
      swt <- round((5.885-(2.893*(0.984^(fi1$springwheat[1]))))*(0.73+0.18*(nsoil)),2)
      wba <- round((((12.967-(10.029*(0.993^(fi1$winterbarley[1])))-(0.0147*(fi1$winterbarley[1])))*(0.76+0.16*(nsoil)))*0.89),2)
      sba <- round(((19.98-(18.164*(0.9952^(fi1$springbarley[1])))-(0.0364*(fi1$springbarley[1])))*(0.887+0.075*(nsoil))*1.02),2)
      wbe <- round((((0.95+1.3625*(nsoil))*1.1)*1.05),2)
      sbe <- round((((0.7+1.25*(nsoil))*1.05)*1.2),2)
      wpo <- round((44.507-(29.135*(0.992^fi1$warepotatoes[1])))*1.16,2)
      wos <- round((((3.35+(-0.623*(0.010^(fi1$wosr[1])))-0.000324*(fi1$wosr[1]))*(0.655+0.23*(nsoil)))*0.87),2)
      sbt <- round(((54.543-(0.05*37.82*(0.984^fi1$sugarbeet[1])))*1.30),2)
      #set <- 0
      sos <- round((2.317-(1.139*(0.984^(fi1$sosr[1])))),2)
      wln <- round(0.75+0.45*1.5*(nsoil),2)
      sln <- round(0.75+0.45*(nsoil)*0.95,2)
      dpe <- round((2.48+3.475*(nsoil)-(1.2875*(nsoil)^2)),2)
      woa <- round((((12.967-(10.029*(0.993^(fi1$winteroats[1])))-(0.0147*(fi1$winteroats[1])))*(0.76+0.16*(nsoil)))*0.89),2)
      soa <- round(((19.98-(18.164*(0.9952^(fi1$springoats[1])))-(0.0364*(fi1$springoats[1])))*(0.887+0.075*(nsoil))*1.02),2)
      
      # ========= Yield Penalty | Black-grass ========= 
      # ************ Winter wheat - black-grass infestation assumption ************
      # These yield penalties (abs/low BG density = 0, med=0, high=7.45, vhigh=25.6) are from Varah et al (2019) Nature Sustainability, based on BGRI data.
      # If better data are obtained, we suggest updating these penalties.
      
      # IF ACTUAL YIELD DATA ARE USED YIELD PENALTIES DUE TO BLACK-GRASS ARE NOT TAKEN INTO ACCOUNT
      
      bgf <- function(infestation){
        if(infestation=="low"){
          yieldloss <- 0; wwht1 <- wwt;
          yls <- 0
        }else if(infestation=="medium"){
          yieldloss <- 5.14; wwht1 <- round(wwt*(1-(5.14/100)),2)
          yls <- round((5.14/100)*wwht1,2)
        }else if(infestation=="high"){
          yieldloss <- 45; wwht1 <- round(wwt*(1-(45/100)),2)
          yls <- round((45/100)*wwht1,2)
        }else if(infestation=="veryhigh"){
          yieldloss <- 70; wwht1 <- round(wwt*(1-(70/100)),2)
          yls <- round((70/100)*wwht1,2)
        }else{
          yieldloss <- 0
          wwht1 <- wwt
          yls <- 0 
        }
        c(wwht1,yls) 
      }
      
      # ========= Yield Penalty | Sowing Date =========
      #************ Delayed Sowing ************
      # Delayed sowing has been modelled to take into account the degree of delay.
      # For example the sowing of winter wheat is optimal in late September and three classes or 
      # options for delayed sowing are defined for October (late), November (later) and December (latest)
      # IF ACTUAL YIELD DATA ARE USED YIELD PENALTIES DUE TO DELAYED SOWING ARE NOT TAKEN INTO ACCOUNT *********
      
      delsow <- function(sowtime){
        if(sowtime=="late"){
          ft1 <- 7/100; ft2 <- 4/100; ft3 <- 3/100; ft4 <- 2/100; ft5 <- 0/100
          ft6 <- 5/100; ft7 <- 10/100; ft8 <- 1/100; ft9 <- 1/100; ft10 <- 0/100; ft11 <- 0/100; ft12 <- 0/100; ft13 <- 0/100
          ds <- c(ft1,ft2,ft3,ft4,ft5,ft6,ft7,ft8,ft9,ft10,ft11,ft12,ft13)
        }else if(sowtime=="later"){
          ft1 <- 12/100; ft2 <- 9/100; ft3 <- 12/100; ft4 <- 5/100; ft5 <- 0/100
          ft6 <- 6/100; ft7 <- 11/100; ft8 <- 5/100; ft9 <- 5/100; ft10 <- 0/100; ft11 <- 0/100; ft12 <- 0/100; ft13 <- 3/100
          ds <- c(ft1,ft2,ft3,ft4,ft5,ft6,ft7,ft8,ft9,ft10,ft11,ft12,ft13)
        }else if(sowtime=="latest"){
          ft1 <- 15/100; ft2 <- 17/100; ft3 <- 18/100; ft4 <- 15/100; ft5 <- 0/100
          ft6 <- 6/100; ft7 <- 11/100; ft8 <- 5/100; ft9 <- 11/100; ft10 <- 0/100; ft11 <- 0/100; ft12 <- 0/100; ft13 <- 6/100
          ds <- c(ft1,ft2,ft3,ft4,ft5,ft6,ft7,ft8,ft9,ft10,ft11,ft12,ft13)
        }else if(sowtime=="no"){
          ft1 <- ft2 <- ft3 <- ft4 <- ft5 <- ft6 <- ft7 <- ft8 <- ft9 <- ft10 <- ft11 <- ft12 <- ft13 <- 0
          ds <- c(ft1,ft2,ft3,ft4,ft5,ft6,ft7,ft8,ft9,ft10,ft11,ft12,ft13)
        }else{
          ft1 <- ft2 <- ft3 <- ft4 <- ft5 <- ft6 <- ft7 <- ft8 <- ft9 <- ft10 <- ft11 <- ft12 <- ft13 <- 0
          ds <- c(ft1,ft2,ft3,ft4,ft5,ft6,ft7,ft8,ft9,ft10,ft11,ft12,ft13)
          warning("Check if delayed sowing option is spelt correctly; option MUST be: no, late, later or latest")
        }
        ds
      }
      
      seccost <- 65 # Secondary yield (straw) price assumed for cereal crops (wheat and barley crops) ********************
      c1 <- 18; c2 <- 19; c3 <- 20; c4 <- 21; c5 <- 22; c6 <- 23; c7 <- 24; c8 <- 25; c9 <- 26;# === row indices
      c10 <- 27; c11 <- 28; c12 <- 29; c13 <- 30; c14 <- 31; 
      
      h1 <- 10; h2 <- 11; h3 <- 12; h4 <- 13; h5 <- 14; h6 <- 15;
      h7 <- 16; h8 <- 17; 
      
      # ********** WW *********
      if(crops[1]=="winterwheat"){
        crd[c1,2] <- snwwt # Sundry cost
        crd[c6,2] <- seccost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2] 
          crd[c5,2] <- crd[c3,2]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- bgf(blackgrass[1])[1]-round(delsow(delsowing[1])[1]*bgf(blackgrass[1])[1],2) # wwt
          crd[c5,2] <- crd[c3,2]*0.5
        }
      }
      if(crops[2]=="winterwheat"){
        crd[c1,3] <- snwwt # Sundry cost
        crd[c6,3] <- seccost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- bgf(blackgrass[2])[1]-round(delsow(delsowing[2])[1]*bgf(blackgrass[2])[1],2) # wwt
          crd[c5,3] <- crd[c3,3]*0.5
        }
      }
      if(crops[3]=="winterwheat"){
        crd[c1,4] <- snwwt # Sundry cost
        crd[c6,4] <- seccost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4] 
          crd[c5,4] <- crd[c3,4]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- bgf(blackgrass[3])[1]-round(delsow(delsowing[3])[1]*bgf(blackgrass[3])[1],2) # wwt
          crd[c5,4] <- crd[c3,4]*0.5
        }
      }
      if(crops[4]=="winterwheat"){
        crd[c1,5] <- snwwt # Sundry cost
        crd[c6,5] <- seccost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- bgf(blackgrass[4])[1]-round(delsow(delsowing[4])[1]*bgf(blackgrass[4])[1],2) # wwt
          crd[c5,5] <- crd[c3,5]*0.5
        }
      } 
      if(crops[5]=="winterwheat"){
        crd[c1,6] <- snwwt # Sundry cost
        crd[c6,6] <- seccost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- bgf(blackgrass[5])[1]-round(delsow(delsowing[5])[1]*bgf(blackgrass[5])[1],2) # wwt
          crd[c5,6] <- crd[c3,6]*0.5
        }
      }
      if(crops[6]=="winterwheat"){
        crd[c1,7] <- snwwt # Sundry cost
        crd[c6,7] <- seccost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- bgf(blackgrass[6])[1]-round(delsow(delsowing[6])[1]*bgf(blackgrass[6])[1],2) # wwt
          crd[c5,7] <- crd[c3,7]*0.5
        }
      }
      
      #********** SPR WHT *********
      if(crops[1]=="springwheat"){
        crd[c1,2] <- snswt # Sundry cost
        crd[c6,2] <- seccost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- swt-round(delsow(delsowing[1])[2]*swt,2)
          crd[c5,2] <- crd[c3,2]*0.5
        }
      }
      if(crops[2]=="springwheat"){
        crd[c1,3] <- snswt # Sundry cost
        crd[c6,3] <- seccost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- swt-round(delsow(delsowing[2])[2]*swt,2)
          crd[c5,3] <- crd[c3,3]*0.5
        }
      }
      if(crops[3]=="springwheat"){
        crd[c1,4] <- snswt # Sundry cost
        crd[c6,4] <- seccost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- swt-round(delsow(delsowing[3])[2]*swt,2)
          crd[c5,4] <- crd[c3,4]*0.5
        }
      }
      if(crops[4]=="springwheat"){
        crd[c1,5] <- snswt # Sundry cost
        crd[c6,5] <- seccost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- swt-round(delsow(delsowing[4])[2]*swt,2)
          crd[c5,5] <- crd[c3,5]*0.5
        }
      } 
      if(crops[5]=="springwheat"){
        crd[c1,6] <- snswt # Sundry cost
        crd[c6,6] <- seccost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- swt-round(delsow(delsowing[5])[2]*swt,2)
          crd[c5,6] <- crd[c3,6]*0.5
        }
      }
      if(crops[6]=="springwheat"){
        crd[c1,7] <- snswt # Sundry cost
        crd[c6,7] <- seccost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- swt-round(delsow(delsowing[6])[2]*swt,2)
          crd[c5,7] <- crd[c3,7]*0.5
        }
      }
      
      #********** WBAR *********
      if(crops[1]=="winterbarley"||crops[1]=="winteroats"){
        crd[c1,2] <- snwba # Sundry cost
        crd[c6,2] <- seccost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- wba-round(delsow(delsowing[1])[3]*wba,2)
          crd[c5,2] <- crd[c3,2]*0.5
        }
      }
      if(crops[2]=="winterbarley"||crops[2]=="winteroats"){
        crd[c1,3] <- snwba # Sundry cost
        crd[c6,3] <- seccost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- wba-round(delsow(delsowing[2])[3]*wba,2)
          crd[c5,3] <- crd[c3,3]*0.5
        }
      }
      if(crops[3]=="winterbarley"||crops[3]=="winteroats"){
        crd[c1,4] <- snwba # Sundry cost
        crd[c6,4] <- seccost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- wba-round(delsow(delsowing[3])[3]*wba,2)
          crd[c5,4] <- crd[c3,4]*0.5
        }
      }
      if(crops[4]=="winterbarley"||crops[4]=="winteroats"){
        crd[c1,5] <- snwba # Sundry cost
        crd[c6,5] <- seccost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- wba-round(delsow(delsowing[4])[3]*wba,2)
          crd[c5,5] <- crd[c3,5]*0.5
        }
      } 
      if(crops[5]=="winterbarley"||crops[5]=="winteroats"){
        crd[c1,6] <- snwba # Sundry cost
        crd[c6,6] <- seccost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- wba-round(delsow(delsowing[5])[3]*wba,2)
          crd[c5,6] <- crd[c3,6]*0.5
        }
      }
      if(crops[6]=="winterbarley"||crops[6]=="winteroats"){
        crd[c1,7] <- snwba # Sundry cost
        crd[c6,7] <- seccost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- wba-round(delsow(delsowing[6])[3]*wba,2)
          crd[c5,7] <- crd[c3,7]*0.5
        }
      }
      #********** SPR BAR *********
      if(crops[1]=="springbarley"||crops[1]=="springoats"){
        crd[c1,2] <- snsba # Sundry cost
        crd[c6,2] <- seccost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- sba-round(delsow(delsowing[1])[4]*sba,2)
          crd[c5,2] <- crd[c3,2]*0.5
        }
      }
      if(crops[2]=="springbarley"||crops[2]=="springoats"){
        crd[c1,3] <- snsba # Sundry cost
        crd[c6,3] <- seccost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- sba-round(delsow(delsowing[2])[4]*sba,2)
          crd[c5,3] <- crd[c3,3]*0.5
        }
      }
      if(crops[3]=="springbarley"||crops[3]=="springoats"){
        crd[c1,4] <- snsba # Sundry cost
        crd[c6,4] <- seccost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- sba-round(delsow(delsowing[3])[4]*sba,2)
          crd[c5,4] <- crd[c3,4]*0.5
        }
      }
      if(crops[4]=="springbarley"||crops[4]=="springoats"){
        crd[c1,5] <- snsba # Sundry cost
        crd[c6,5] <- seccost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- sba-round(delsow(delsowing[4])[4]*sba,2)
          crd[c5,5] <- crd[c3,5]*0.5
        }
      } 
      if(crops[5]=="springbarley"||crops[5]=="springoats"){
        crd[c1,6] <- snsba # Sundry cost
        crd[c6,6] <- seccost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- sba-round(delsow(delsowing[5])[4]*sba,2)
          crd[c5,6] <- crd[c3,6]*0.5
        }
      }
      if(crops[6]=="springbarley"||crops[6]=="springoats"){
        crd[c1,7] <- snsba # Sundry cost
        crd[c6,7] <- seccost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- sba-round(delsow(delsowing[6])[4]*sba,2)
          crd[c5,7] <- crd[c3,7]*0.5
        }
      }
      
      #********** WBEA *********
      if(crops[1]=="winterbeans"){
        crd[c1,2] <- snwbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- wbe-round(delsow(delsowing[1])[5]*wbe,2)
          crd[c5,2] <- crd[c3,2]*0
        }
      }
      if(crops[2]=="winterbeans"){
        crd[c1,3] <- snwbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- wbe-round(delsow(delsowing[2])[5]*wbe,2)
          crd[c5,3] <- crd[c3,3]*0
        }
      }
      if(crops[3]=="winterbeans"){
        crd[c1,4] <- snwbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- wbe-round(delsow(delsowing[3])[5]*wbe,2)
          crd[c5,4] <- crd[c3,4]*0
        }
      }
      if(crops[4]=="winterbeans"){
        crd[c1,5] <- snwbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- wbe-round(delsow(delsowing[4])[5]*wbe,2)
          crd[c5,5] <- crd[c3,5]*0
        }
      } 
      if(crops[5]=="winterbeans"){
        crd[c1,6] <- snwbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- wbe-round(delsow(delsowing[5])[5]*wbe,2)
          crd[c5,6] <- crd[c3,6]*0
        }
      }
      if(crops[6]=="winterbeans"){
        crd[c1,7] <- snwbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- wbe-round(delsow(delsowing[6])[5]*wbe,2)
          crd[c5,7] <- crd[c3,7]*0
        }
      }
      
      #********** SPR BEA *********
      if(crops[1]=="springbeans"){
        crd[c1,2] <- snsbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- sbe-round(delsow(delsowing[1])[6]*sbe,2)
          crd[c5,2] <- crd[c3,2]*0
        }
      }
      if(crops[2]=="springbeans"){
        crd[c1,3] <- snsbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- sbe-round(delsow(delsowing[2])[6]*sbe,2)
          crd[c5,3] <- crd[c3,3]*0
        }
      }
      if(crops[3]=="springbeans"){
        crd[c1,4] <- snsbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- sbe-round(delsow(delsowing[3])[6]*sbe,2)
          crd[c5,4] <- crd[c3,4]*0
        }
      }
      if(crops[4]=="springbeans"){
        crd[c1,5] <- snsbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- sbe-round(delsow(delsowing[4])[6]*sbe,2)
          crd[c5,5] <- crd[c3,5]*0
        }
      } 
      if(crops[5]=="springbeans"){
        crd[c1,6] <- snsbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- sbe-round(delsow(delsowing[5])[6]*sbe,2)
          crd[c5,6] <- crd[c3,6]*0
        }
      }
      if(crops[6]=="springbeans"){
        crd[c1,7] <- snsbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- sbe-round(delsow(delsowing[6])[6]*sbe,2)
          crd[c5,7] <- crd[c3,7]*0
        }
      }
      
      #********** WPOTS *********
      if(crops[1]=="warepotatoes"){
        crd[c1,2] <- snwpo # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- wpo-round(delsow(delsowing[1])[7]*wpo,2)
          crd[c5,2] <- crd[c3,2]*0
        }
      }
      if(crops[2]=="warepotatoes"){
        crd[c1,3] <- snwpo # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- wpo-round(delsow(delsowing[2])[7]*wpo,2)
          crd[c5,3] <- crd[c3,3]*0
        }
      }
      if(crops[3]=="warepotatoes"){
        crd[c1,4] <- snwpo # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- wpo-round(delsow(delsowing[3])[7]*wpo,2)
          crd[c5,4] <- crd[c3,4]*0
        }
      }
      if(crops[4]=="warepotatoes"){
        crd[c1,5] <- snwpo # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- wpo-round(delsow(delsowing[4])[7]*wpo,2)
          crd[c5,5] <- crd[c3,5]*0
        }
      } 
      if(crops[5]=="warepotatoes"){
        crd[c1,6] <- snwpo # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- wpo-round(delsow(delsowing[5])[7]*wpo,2)
          crd[c5,6] <- crd[c3,6]*0
        }
      }
      if(crops[6]=="warepotatoes"){
        crd[c1,7] <- snwpo # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- wpo-round(delsow(delsowing[6])[7]*wpo,2)
          crd[c5,7] <- crd[c3,7]*0
        }
      }
      
      #********** WOSR *********
      if(crops[1]=="wosr"){
        crd[c1,2] <- snwos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- wos-round(delsow(delsowing[1])[8]*wos,2)
          crd[c5,2] <- crd[c3,2]*0
        }
      }
      if(crops[2]=="wosr"){
        crd[c1,3] <- snwos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- wos-round(delsow(delsowing[2])[8]*wos,2)
          crd[c5,3] <- crd[c3,3]*0
        }
      }
      if(crops[3]=="wosr"){
        crd[c1,4] <- snwos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- wos-round(delsow(delsowing[3])[8]*wos,2)
          crd[c5,4] <- crd[c3,4]*0
        }
      }
      if(crops[4]=="wosr"){
        crd[c1,5] <- snwos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- wos-round(delsow(delsowing[4])[8]*wos,2)
          crd[c5,5] <- crd[c3,5]*0
        }
      } 
      if(crops[5]=="wosr"){
        crd[c1,6] <- snwos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- wos-round(delsow(delsowing[5])[8]*wos,2)
          crd[c5,6] <- crd[c3,6]*0
        }
      }
      if(crops[6]=="wosr"){
        crd[c1,7] <- snwos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- wos-round(delsow(delsowing[6])[8]*wos,2)
          crd[c5,7] <- crd[c3,7]*0
        }
      }
      
      #********** SBEET *********
      if(crops[1]=="sugarbeet"){
        crd[h5,2] <- 5 # Cost of transporting sugarbeet @ £5/t
        crd[c1,2] <- snsbt # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- sbt-round(delsow(delsowing[1])[9]*sbt,2)
          crd[c5,2] <- crd[c3,2]*0
        }
      }
      if(crops[2]=="sugarbeet"){
        crd[h5,3] <- 5 # Cost of transporting sugarbeet @ £5/t
        crd[c1,3] <- snsbt # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- sbt-round(delsow(delsowing[2])[9]*sbt,2)
          crd[c5,3] <- crd[c3,3]*0
        }
      }
      if(crops[3]=="sugarbeet"){
        crd[h5,4] <- 5 # Cost of transporting sugarbeet @ £5/t
        crd[c1,4] <- snsbt # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- sbt-round(delsow(delsowing[3])[9]*sbt,2)
          crd[c5,4] <- crd[c3,4]*0
        }
      }
      if(crops[4]=="sugarbeet"){
        crd[h5,5] <- 5 # Cost of transporting sugarbeet @ £5/t
        crd[c1,5] <- snsbt # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- sbt-round(delsow(delsowing[4])[9]*sbt,2)
          crd[c5,5] <- crd[c3,5]*0
        }
      } 
      if(crops[5]=="sugarbeet"){
        crd[h5,6] <- 5 # Cost of transporting sugarbeet @ £5/t
        crd[c1,6] <- snsbt # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- sbt-round(delsow(delsowing[5])[9]*sbt,2)
          crd[c5,6] <- crd[c3,6]*0
        }
      }
      if(crops[6]=="sugarbeet"){
        crd[h5,7] <- 5 # Cost of transporting sugarbeet @ £5/t
        crd[c1,7] <- snsbt # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- sbt-round(delsow(delsowing[6])[9]*sbt,2)
          crd[c5,7] <- crd[c3,7]*0
        }
      }
      
      #********** SETA *********
      if(crops[1]=="setaside"){
        crd[c1,2] <- snset # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- 0
          crd[c5,2] <- 0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- 0
          crd[c5,2] <- 0
        }
      }
      if(crops[2]=="setaside"){
        crd[c1,3] <- snset # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- 0
          crd[c5,3] <- 0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- 0
          crd[c5,3] <- 0
        }
      }
      if(crops[3]=="setaside"){
        crd[c1,4] <- snset # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- 0
          crd[c5,4] <- 0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- 0
          crd[c5,4] <- 0
        }
      }
      if(crops[4]=="setaside"){
        crd[c1,5] <- snset # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- 0
          crd[c5,5] <- 0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- 0
          crd[c5,5] <- 0
        }
      } 
      if(crops[5]=="setaside"){
        crd[c1,6] <- snset # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- 0
          crd[c5,6] <- 0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- 0
          crd[c5,6] <- 0
        }
      }
      if(crops[6]=="setaside"){
        crd[c1,7] <- snset # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- 0
          crd[c5,7] <- 0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- 0
          crd[c5,7] <- 0
        }
      }
      
      #********** SOSR *********
      if(crops[1]=="sosr"){
        crd[c1,2] <- snsos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- sos-round(delsow(delsowing[1])[10]*sos,2)
          crd[c5,2] <- crd[c3,2]*0
        }
      }
      if(crops[2]=="sosr"){
        crd[c1,3] <- snsos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- sos-round(delsow(delsowing[2])[10]*sos,2)
          crd[c5,3] <- crd[c3,3]*0
        }
      }
      if(crops[3]=="sosr"){
        crd[c1,4] <- snsos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- sos-round(delsow(delsowing[3])[10]*sos,2)
          crd[c5,4] <- crd[c3,4]*0
        }
      }
      if(crops[4]=="sosr"){
        crd[c1,5] <- snsos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- sos-round(delsow(delsowing[4])[10]*sos,2)
          crd[c5,5] <- crd[c3,5]*0
        }
      } 
      if(crops[5]=="sosr"){
        crd[c1,6] <- snsos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- sos-round(delsow(delsowing[5])[10]*sos,2)
          crd[c5,6] <- crd[c3,6]*0
        }
      }
      if(crops[6]=="sosr"){
        crd[c1,7] <- snsos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- sos-round(delsow(delsowing[6])[10]*sos,2)
          crd[c5,7] <- crd[c3,7]*0
        }
      }
      
      #********** WLIN *********
      if(crops[1]=="winterlinseed"){
        crd[c1,2] <- snwln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- wln-round(delsow(delsowing[1])[11]*wln,2)
          crd[c5,2] <- crd[c3,2]*0
        }
      }
      if(crops[2]=="winterlinseed"){
        crd[c1,3] <- snwln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- wln-round(delsow(delsowing[2])[11]*wln,2)
          crd[c5,3] <- crd[c3,3]*0
        }
      }
      if(crops[3]=="winterlinseed"){
        crd[c1,4] <- snwln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- wln-round(delsow(delsowing[3])[11]*wln,2)
          crd[c5,4] <- crd[c3,4]*0
        }
      }
      if(crops[4]=="winterlinseed"){
        crd[c1,5] <- snwln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- wln-round(delsow(delsowing[4])[11]*wln,2)
          crd[c5,5] <- crd[c3,5]*0
        }
      } 
      if(crops[5]=="winterlinseed"){
        crd[c1,6] <- snwln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- wln-round(delsow(delsowing[5])[11]*wln,2)
          crd[c5,6] <- crd[c3,6]*0
        }
      }
      if(crops[6]=="winterlinseed"){
        crd[c1,7] <- snwln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- wln-round(delsow(delsowing[6])[11]*wln,2)
          crd[c5,7] <- crd[c3,7]*0
        }
      }
      
      #********** SLIN *********
      if(crops[1]=="springlinseed"){
        crd[c1,2] <- snsln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- sln-round(delsow(delsowing[1])[12]*sln,2)
          crd[c5,2] <- crd[c3,2]*0
        }
      }
      if(crops[2]=="springlinseed"){
        crd[c1,3] <- snsln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- sln-round(delsow(delsowing[2])[12]*sln,2)
          crd[c5,3] <- crd[c3,3]*0
        }
      }
      if(crops[3]=="springlinseed"){
        crd[c1,4] <- snsln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- sln-round(delsow(delsowing[3])[12]*sln,2)
          crd[c5,4] <- crd[c3,4]*0
        }
      }
      if(crops[4]=="springlinseed"){
        crd[c1,5] <- snsln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- sln-round(delsow(delsowing[4])[12]*sln,2)
          crd[c5,5] <- crd[c3,5]*0
        }
      } 
      if(crops[5]=="springlinseed"){
        crd[c1,6] <- snsln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- sln-round(delsow(delsowing[5])[12]*sln,2)
          crd[c5,6] <- crd[c3,6]*0
        }
      }
      if(crops[6]=="springlinseed"){
        crd[c1,7] <- snsln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- sln-round(delsow(delsowing[6])[12]*sln,2)
          crd[c5,7] <- crd[c3,7]*0
        }
      }
      
      #********** PEAS *********
      if(crops[1]=="driedpeas"){
        crd[c1,2] <- sndpe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- dpe-round(delsow(delsowing[1])[13]*dpe,2)
          crd[c5,2] <- crd[c3,2]*0
        }
      }
      if(crops[2]=="driedpeas"){
        crd[c1,3] <- sndpe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- dpe-round(delsow(delsowing[2])[13]*dpe,2)
          crd[c5,3] <- crd[c3,3]*0
        }
      }
      if(crops[3]=="driedpeas"){
        crd[c1,4] <- sndpe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- dpe-round(delsow(delsowing[3])[13]*dpe,2)
          crd[c5,4] <- crd[c3,4]*0
        }
      }
      if(crops[4]=="driedpeas"){
        crd[c1,5] <- sndpe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- dpe-round(delsow(delsowing[4])[13]*dpe,2)
          crd[c5,5] <- crd[c3,5]*0
        }
      } 
      if(crops[5]=="driedpeas"){
        crd[c1,6] <- sndpe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- dpe-round(delsow(delsowing[5])[13]*dpe,2)
          crd[c5,6] <- crd[c3,6]*0
        }
      }
      if(crops[6]=="driedpeas"){
        crd[c1,7] <- sndpe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- dpe-round(delsow(delsowing[6])[13]*dpe,2)
          crd[c5,7] <- crd[c3,7]*0
        }
      }
      
      #**************************************************************************************************************
      # ========= Subsidy amount =========
      crd[c7,seq(2,7)] <- 223 # SUBSIDY AMOUNT ASSUMED @ 223/ha (https://www.gov.uk/guidance/bps-2019)
      cropData <- crd
      
      Operation=c("Spread P/K Fertiliser WWHT","Plough WWHT","Sowing WWHT","Roll WWHT",
                  "Apply N Fertiliser WWHT","Spray WWHT","Combine Harvesting WWHT","Bale WWHT",
                  
                  "Spread P/K Fertiliser SWHT","Plough SWHT","Sowing SWHT","Roll SWHT",
                  "Apply N Fertiliser SWHT","Spray SWHT","Combine Harvesting SWHT","Bale SWHT",
                  
                  "Spread P/K Fertiliser WBAR","Plough WBAR","Sowing WBAR","Roll WBAR",
                  "Apply N Fertiliser WBAR","Spray WBAR","Combine Harvesting WBAR","Bale WBAR",
                  
                  "Spread P/K Fertiliser SBAR","Plough SBAR","Sowing SBAR","Roll SBAR",
                  "Spray SBAR","Apply N Fertiliser SBAR","Combine Harvesting SBAR","Bale SBAR",
                  
                  "Spread P/K Fertiliser WBEA","Plough WBEA","Sowing WBEA","Roll WBEA","Spray WBEA",
                  "Combine Harvesting WBEA",
                  
                  "Spread P/K Fertiliser SBEA","Plough SBEA","Sowing SBEA","Spray SBEA",
                  "Combine Harvesting SBEA",
                  
                  "Plough WPOT","Harrow WPOT","Sowing WPOT","Ridge WPOT","Spray WPOT","Spread P/K Fertiliser WPOT",
                  "Apply N Fertiliser WPOT","Hoeing WPOT","Harvest WPOT",
                  
                  "Spread P/K Fertiliser WOSR","Plough WOSR","Sowing WOSR",
                  "Spray WOSR","Apply N Fertiliser WOSR","Combine Harvesting WOSR",
                  
                  "Plough SBEE","Harrow SBEE","Sowing SBEE","Spray SBEE","Spread P/K Fertiliser SBEE",
                  "Apply N Fertiliser SBEE","Hoeing SBEE","Harvest SBEE",
                  
                  "Plough SETA","Spray SETA",
                  
                  "Spread P/K Fertiliser SOSR","Plough SOSR","Sowing SOSR",
                  "Spray SOSR","Apply N Fertiliser SOSR","Combine Harvesting SOSR",
                  
                  "Spread P/K Fertiliser WLIN","Plough WLIN","Sowing WLIN",
                  "Spray WLIN","Apply N Fertiliser WLIN","Combine Harvesting WLIN",
                  
                  "Spread P/K Fertiliser SLIN","Plough SLIN","Sowing SLIN",
                  "Spray SLIN","Apply N Fertiliser SLIN","Combine Harvesting SLIN",
                  
                  "Spread P/K Fertiliser DPEA","Plough DPEA","Sowing DPEA","Roll DPEA",
                  "Spray DPEA","Combine Harvesting DPEA")
      
      lgt <- length(Operation) # Total number of operation for all crops              
      Workrates <- data.frame(Operations=Operation,   
                              
                              Tractor=c(rep(0,lgt)), Labour=c(rep(0,lgt)), 
                              Power_harrow=c(rep(0,lgt)), Sprayer=c(rep(0,lgt)),
                              Combine_harvester=c(rep(0,lgt)), Baler=c(rep(0,lgt)), 
                              SBEE_harvester=c(rep(0,lgt)),WPOT_harvester=c(rep(0,lgt)),Fuel_cost=c(rep(0,lgt)), 
                              Labour_cost=c(rep(0,lgt)), Operation_cost=c(rep(0,lgt))
      )
      
      Machines <- data.frame(
        Machine=c("Tractor","Rolls","Power harrow","Sprayer","Combine harvetser",
                  "Baler","Sugarbeet harvester"),
        
        Size_Unit=c("kW","m","m","litres","kW","na","na"),
        
        Size=c(machsize,0,0),
        
        Machine_price=c(rep(0,7)),
        
        Depreciation_rate=c(22,14,14,18,18,11,18),
        
        Repair_cost_rate=c(12,5,5,6.8,5.8,5.8,5), Replacement_year=c(5,10,8,7,7,7,7),
        
        Annual_hours=c(2500,rep(300,6))
      )
      
      modData <- list(cropData, Workrates, Machines) # Data files
      
      #************************************************************************************************
      #************************************************************************************************
      
      
      fi1 <- modData[[1]] # Crop Data
      
      # Yield penalties with respect to continuous cropping (self rotation) and sub-optimal rotation
      sf <- selfrot/100 # Self rotation penalty
      rt <- rotpen/100 # Rotational penalty
      
      #***********
      cy1 <- fi1[c3,2]; cy2 <- fi1[c3,3]; cy3 <- fi1[c3,4]; cy4 <- fi1[c3,5]; cy5 <- fi1[c3,6]; cy6 <- fi1[c3,7]
      if(crops[1]=="winterwheat"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0.5
      }else if(crops[1]=="springwheat"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0.5
      }else if(crops[1]=="winterbarley"||crops[1]=="winteroats"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0.5
      }else if(crops[1]=="springbarley"||crops[1]=="springoats"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0.5
      }else if(crops[1]=="winterbeans"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0
      }else if(crops[1]=="springbeans"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0
      }else if(crops[1]=="warepotatoes"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0
      }else if(crops[1]=="wosr"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0
      }else if(crops[1]=="sugarbeet"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0
      }else if(crops[1]=="setaside"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0
      }else if(crops[1]=="sosr"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0
      }else if(crops[1]=="winterlinseed"||crops[1]=="springlinseed"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0
      }else if(crops[1]=="driedpeas"||crops[1]=="peas"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0
      }
      
      #***********
      if(crops[2]=="winterwheat"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0.5
      }else if(crops[2]=="springwheat"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0.5
      }else if(crops[2]=="winterbarley"||crops[2]=="winteroats"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0.5
      }else if(crops[2]=="springbarley"||crops[2]=="springoats"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0.5
      }else if(crops[2]=="winterbeans"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0
      }else if(crops[2]=="springbeans"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0
      }else if(crops[2]=="warepotatoes"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0
      }else if(crops[2]=="wosr"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0
      }else if(crops[2]=="sugarbeet"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0
      }else if(crops[2]=="setaside"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0
      }else if(crops[2]=="sosr"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0
      }else if(crops[2]=="winterlinseed"||crops[2]=="springlinseed"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0
      }else if(crops[2]=="driedpeas"||crops[2]=="peas"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0
      }
      
      #***********
      if(crops[3]=="winterwheat"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0.5
      }else if(crops[3]=="springwheat"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0.5
      }else if(crops[3]=="winterbarley"||crops[3]=="winteroats"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0.5
      }else if(crops[3]=="springbarley"||crops[3]=="springoats"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0.5
      }else if(crops[3]=="winterbeans"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0
      }else if(crops[3]=="springbeans"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0
      }else if(crops[3]=="warepotatoes"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0
      }else if(crops[3]=="wosr"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0
      }else if(crops[3]=="sugarbeet"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0
      }else if(crops[3]=="setaside"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0
      }else if(crops[3]=="sosr"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0
      }else if(crops[3]=="winterlinseed"||crops[3]=="springlinseed"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0
      }else if(crops[3]=="driedpeas"||crops[3]=="peas"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0
      }
      
      #***********
      if(crops[4]=="winterwheat"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0.5
      }else if(crops[4]=="springwheat"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0.5
      }else if(crops[4]=="winterbarley"||crops[4]=="winteroats"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0.5
      }else if(crops[4]=="springbarley"||crops[4]=="springoats"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0.5
      }else if(crops[4]=="winterbeans"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0
      }else if(crops[4]=="springbeans"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0
      }else if(crops[4]=="warepotatoes"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0
      }else if(crops[4]=="wosr"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0
      }else if(crops[4]=="sugarbeet"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0
      }else if(crops[4]=="setaside"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0
      }else if(crops[4]=="sosr"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0
      }else if(crops[4]=="winterlinseed"||crops[4]=="springlinseed"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0
      }else if(crops[4]=="driedpeas"||crops[4]=="peas"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0
      }
      
      #***********
      if(crops[5]=="winterwheat"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0.5
      }else if(crops[5]=="springwheat"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0.5
      }else if(crops[5]=="winterbarley"||crops[5]=="winteroats"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0.5
      }else if(crops[5]=="springbarley"||crops[5]=="springoats"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0.5
      }else if(crops[5]=="winterbeans"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0
      }else if(crops[5]=="springbeans"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0
      }else if(crops[5]=="warepotatoes"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0
      }else if(crops[5]=="wosr"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0
      }else if(crops[5]=="sugarbeet"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0
      }else if(crops[5]=="setaside"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0
      }else if(crops[5]=="sosr"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0
      }else if(crops[5]=="winterlinseed"||crops[5]=="springlinseed"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0
      }else if(crops[5]=="driedpeas"||crops[5]=="peas"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0
      }
      
      #***********
      if(crops[6]=="winterwheat"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0.5
      }else if(crops[6]=="springwheat"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0.5
      }else if(crops[6]=="winterbarley"||crops[6]=="winteroats"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0.5
      }else if(crops[6]=="springbarley"||crops[6]=="springoats"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0.5
      }else if(crops[6]=="winterbeans"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0
      }else if(crops[6]=="springbeans"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0
      }else if(crops[6]=="warepotatoes"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0
      }else if(crops[6]=="wosr"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0
      }else if(crops[6]=="sugarbeet"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0
      }else if(crops[6]=="setaside"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0
      }else if(crops[6]=="sosr"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0
      }else if(crops[6]=="winterlinseed"||crops[6]=="springlinseed"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0
      }else if(crops[6]=="driedpeas"||crops[6]=="peas"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0
      }
      
      
      op <- fi1
      
      #*************** Farm Costs/Revenue ****************************
      # Farm output estimates
      le <- 7
      if(subsidy=="no"){
        farmpayment <- 0
      }else{
        farmpayment <- op[c7,seq(2,le)]
      }
      
      # Farm output estimates
      # Farm output = (Crop yield * Crop price) + farmpayment
      outp <- ((op[c3,seq(2,le)]*op[c4,seq(2,le)])+(op[c5,seq(2,le)]*op[c6,seq(2,le)]))+farmpayment 
      op[c8,seq(2,le)] <- outp 
      
      kk1 <- op
      
      #*************** Farm Costs ****************************
      vc <- kk1 # Updated data with farmOutput(soil, subsidy, blackgrass)
      
      #h1 <- 10; h2 <- 11; h3 <- 12; h4 <- 13; h5 <- 14; h6 <- 15;
      #h7 <- 16; h8 <- 17; 
      
      # Fertiliser (NPK) cost = (Fertiliser rate (kg/ha)* Fertiliser price(£/kg))
      vc[h6,seq(2,le)] <- fertcost <- (vc[1,seq(2,le)]*vc[4,seq(2,le)])+
        (vc[2,seq(2,le)]*vc[5,seq(2,le)])+(vc[3,seq(2,le)]*vc[6,seq(2,le)])
      
      # Seed cost (Seed rate (kg/ha) * Seed price (£/kg))
      vc[h7,seq(2,le)] <- seedcost <- vc[7,seq(2,le)]*vc[8,seq(2,le)]
      
      # Herbicide cost is assumed to be cost of herbicides targeting black-grass 
      # (cost of other herbicides and other chemical costs are incorporated in Sundry cost)
      # Herbicide cost = Total Herbicide rate (kg or l/ha) * Herbicide price (£/l)
      vc[h8,seq(2,le)] <- chemcost <- (vc[9,seq(2,le)]*vc[h1,seq(2,le)])+(vc[h2,seq(2,le)]*vc[h3,seq(2,le)])
      
      # Sundry Cost (£/ha) Informed by data from Nix (2019) Farm Management Pocketbook
      sundry <- vc[c1,seq(2,le)] 
      
      # Sugar beet transport cost from Nix (2019) based on £5.11/t
      vc[c2,seq(2,le)] <- sbeettransport <- vc[h5,seq(2,le)]*vc[c3,seq(2,le)] 
      
      # Variable cost (£/ha)
      vc[c9,seq(2,le)] <- (fertcost+seedcost+chemcost+sundry+sbeettransport)
      
      # Gross margin (£/ha)
      # Gross margin = Output - Variable cost
      vc[c10,seq(2,le)] <- vc[c8,seq(2,le)]-vc[c9,seq(2,le)]
      kk2 <- vc
      
      # operatingCost <- function(nsoil, subsidy, blackgrass){
      
      # Estimates workrate of farm operations and fuel and labour cost
      # wro <- variableCost(nsoil, subsidy, blackgrass)
      
      #========= Machine Sizes ===========
      cr <- kk2 #variableCost(nsoil, subsidy, blackgrass) #wro #Files()[[2]]
      ma <- modData[[3]][,seq(-1,-2)] # Machine data
      wr <- modData[[2]] # Workrate data 
      tractor <- ma[1,1] # Tractor Size or power (kW) 
      sprayer <- ma[4,1] # Sprayer Size (size of tank in litres) 1400 litres
      tsize <- ma[4,1] # Tank size for estimate fertiliser application workrate
      combsize <- ma[5,1] # Size of combine harvester
      # The size of the combine harvester measured in tonnes/hour 
      # was derived on pro rata basis based on information from
      # Agricultural Notebook. A combine harvester with a power of 90kW can harvest 10t/h 
      # Thus a combine harvester of 125kW can harvest (10/90)*125
      extfactor <- round(10/90,2)
      combine <- round(combsize*extfactor) #14 # Represents combine harvester size
      rollwidth <- ma[2,1] # Roll width
      sphoe <- 19 # Assumed speed for hoeing (19km/h)
      rowsp <- 0.6 # Assumed row space (0.6m)
      tpspeed <- 4 # Speed for rolling (km/hr)
      
      # ========= Tillage Types ==========
      # Tillage Assumptions
      # To be able to take into consideration soil type in tillage work rate,
      # the ploughing work rate is taken as a reference value due to the availability of suitable formulae.
      # The workability of ploughing is taken as 1.
      
      if(tillage=="ploughing"){
        wkb <- 1 # ploughing workability used as a reference value
      }else if(tillage=="noninversion"){
        wkb <- 1*0.5 # 50% reduction in plough rate is assumed - i.e. twice as fast as ploughing
      }else if(tillage=="subsoiling"){
        wkb <- 1/0.75 # 75% slower than ploughing (previously this was 1*0.65, as a 35% reduction in plough work rate was assumed. After talking to contractors, this was amended: they said subsoiling is slower than ploughing, and you'd do 75% of what you'd get done if ploughing in the same time).
      }else if(tillage=="lightcultivation"){
        wkb <- 1*0.6 # 40% reduction in plough work rate is assumed
      }else if(tillage=="directdrilling"){
        wkb <- 0 # no cultivation
      }else if (tillage=="minimumtillage"){
        wkb <- 1*0.3 # 70% reduction in plough work rate is assumed
      }else if(tillage=="deepcultivation"){
        wkb <- (1*0.6)*1.4 # Assumed to be 40% more than light cultivation
      }else if(tillage[1]=="inversion"){
        wkb <- 1 
      }else{
        wkb <- 1 
      }
      
      nspr <- numberofsprays # Number of Sprays ***************************************************************************
      
      if("winterwheat"%in%crops==TRUE){
        # Work rates for winter wheat operations =======
        wr[1,2] <- wr[1,3] <- round(((0.06+0.00025*({cr$winterwheat[2]}+{cr$winterwheat[3]}))+
                                       (64.48+0.094*({cr$winterwheat[2]}+{cr$winterwheat[3]}))/ tsize),2) # Spread P/K fert
        wr[2,2] <- wr[2,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        wr[3,2] <- wr[3,3] <- round((0.06+0.00069*{cr$winterwheat[7]})+
                                      (58.82+41.5*{nsoil}+0.00626*{cr$winterwheat[7]})/tractor,2) # Sow
        wr[4,2] <- wr[4,3] <- round((1.5/(tpspeed*rollwidth/10))*0.85,2) # Roll
        wr[5,2] <- wr[5,3] <- 
          round(((0.06+0.00025*({cr$winterwheat[1]}))+(64.68+0.094*({cr$winterwheat[1]}))/ tsize),2) # N fert
        
        if(crops[1]=="winterwheat"){
          wr[6,2] <- wr[6,3] <- wr[6,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="winterwheat"){
          wr[6,2] <- wr[6,3] <- wr[6,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="winterwheat"){
          wr[6,2] <- wr[6,3] <- wr[6,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="winterwheat"){
          wr[6,2] <- wr[6,3] <- wr[6,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="winterwheat"){
          wr[6,2] <- wr[6,3] <- wr[6,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="winterwheat"){
          wr[6,2] <- wr[6,3] <- wr[6,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[6,2] <- wr[6,3] <- wr[6,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        
        cbwwht <- round((1.00*({cr$winterwheat[c3]}+20)/4)/combine,2) # Combine harvester
        wr[7,2] <- 2* cbwwht; wr[7,3] <- 3* cbwwht; wr[7,6] <- cbwwht
        bawwht <- round((({cr$winterwheat[c5]}+13)/4)/combine*0.5,2) # Bale
        wr[8,2] <- 2* bawwht; wr[8,3] <- 2* bawwht; wr[8,7] <- bawwht
      }else{
        wr[seq(1,8),seq(2,12)] <- 0
      }
      
      if("springwheat"%in%crops==TRUE){
        # Work rates for spring wheat operations =======
        wr[9,2] <- wr[9,3] <- round(((0.06+0.00025*({cr$springwheat[3]}+{cr$springwheat[5]}))+
                                       (64.48+0.094*({cr$springwheat[3]}+{cr$springwheat[5]}))/ tsize),2) # Spread P/K fert
        wr[10,2] <- wr[10,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        wr[11,2] <- wr[11,3] <- round(((0.06+0.00069*{cr$springwheat[7]})+
                                         (58.82+41.5*{nsoil}+0.00626*{cr$springwheat[7]})/tractor),2) #Sow
        wr[12,2] <- wr[12,3] <- round((1.5/(tpspeed*rollwidth/10))*0.85,2) # Roll
        wr[13,2] <- wr[13,3] <- round(((0.06+0.00025*({cr$springwheat[1]}))+(64.68+0.094*({cr$springwheat[1]}))/ tsize),2) # N fert
        
        if(crops[1]=="springwheat"){
          wr[14,2] <- wr[14,3] <- wr[14,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="springwheat"){
          wr[14,2] <- wr[14,3] <- wr[14,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="springwheat"){
          wr[14,2] <- wr[14,3] <- wr[14,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="springwheat"){
          wr[14,2] <- wr[14,3] <- wr[14,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="springwheat"){
          wr[14,2] <- wr[14,3] <- wr[14,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="springwheat"){
          wr[14,2] <- wr[14,3] <- wr[14,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[14,2] <- wr[14,3] <- wr[14,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        cbswht <- round(2*(1.00*({cr$springwheat[c3]}+20)/4)/combine,2) # Combine
        wr[15,2] <- 2* cbswht; wr[15,3] <- 3* cbswht; wr[15,6] <- cbswht
        baswht <- round((({cr$springwheat[c5]}+13)/4)/combine*0.5,2) # Bale
        wr[16,2] <- 2* baswht;  wr[16,3] <- 2* baswht; wr[16,7] <- baswht
        # 
      }else{
        wr[seq(9,16),seq(2,12)] <-0
      }
      
      if("winterbarley"%in%crops==TRUE||"winteroats"%in%crops==TRUE){
        # Work rates for winter barley operations =====
        wr[17,2] <- wr[17,3] <- round(((0.06+0.00025*({cr$winterbarley[3]}+{cr$winterbarley[5]}))+
                                         (64.48+0.094*({cr$winterbarley[3]}+{cr$winterbarley[5]}))/ tsize),2) # Spread P/K fert
        wr[18,2] <- wr[18,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        wr[19,2] <- wr[19,3] <- round(((0.06+0.00069*{cr$winterbarley[7]})+
                                         (58.82+41.5*{nsoil}+0.00626*{cr$winterbarley[7]})/tractor),2) # Sow
        wr[20,2] <- wr[20,3] <- round((1.5/(tpspeed*rollwidth/10))*0.85,2) # Roll
        wr[21,2] <- wr[21,3] <- 
          round(((0.06+0.00025*({cr$winterbarley[1]}))+(64.68+0.094*({cr$winterbarley[1]}))/ tsize),2) # N fert
        
        if(crops[1]=="winterbarley"||crops[1]=="winteroats"){
          wr[22,2] <- wr[22,3] <- wr[22,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="winterbarley"||crops[2]=="winteroats"){
          wr[22,2] <- wr[22,3] <- wr[22,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="winterbarley"||crops[3]=="winteroats"){
          wr[22,2] <- wr[22,3] <- wr[22,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="winterbarley"||crops[4]=="winteroats"){
          wr[22,2] <- wr[22,3] <- wr[22,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="winterbarley"||crops[5]=="winteroats"){
          wr[22,2] <- wr[22,3] <- wr[22,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="winterbarley"||crops[6]=="winteroats"){
          wr[22,2] <- wr[22,3] <- wr[22,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[22,2] <- wr[22,3] <- wr[22,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        cbwbar <- round((1.15*({cr$winterbarley[c3]}+24)/4)/combine,2) # Combine
        wr[23,2] <- 2* cbwbar; wr[23,3] <- 3* cbwbar; wr[23,6] <- cbwbar
        bawbar <- round((({cr$winterbarley[c5]}+13)/4)/combine*0.5,2) # Bale
        wr[24,2] <- 3* bawbar; wr[24,3] <- 3* bawbar; wr[24,7] <- bawbar
      }else{
        wr[seq(17,24),seq(2,12)] <- 0
      }
      
      if("springbarley"%in%crops==TRUE||"springoats"%in%crops==TRUE){
        # Work rates for spring barley operations =====
        wr[25,2] <- wr[25,3] <- round(((0.06+0.00025*({cr$springbarley[2]}+{cr$springbarley[3]}))+
                                         (64.48+0.094*({cr$springbarley[2]}+{cr$springbarley[3]}))/ tsize),2) # Spread P/K fert
        wr[26,2] <- wr[26,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        wr[27,2] <- wr[27,3] <- round(((0.06+0.00069*{cr$springbarley[7]})+
                                         (58.82+41.5*{nsoil}+0.00626*{cr$springbarley[7]})/tractor),2) # Sow
        wr[28,2] <- wr[28,3] <- round((1.5/(tpspeed*rollwidth/10))*0.85,2) # Roll
        
        #wr[29,2] <- wr[29,3] <- wr[29,5] <- round(0.11+204.2/sprayer,2) # Spraying
        if(crops[1]=="springbarley"||crops[1]=="springoats"){
          wr[29,2] <- wr[29,3] <- wr[29,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="springbarley"||crops[2]=="springoats"){
          wr[29,2] <- wr[29,3] <- wr[29,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="springbarley"||crops[3]=="springoats"){
          wr[29,2] <- wr[29,3] <- wr[29,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="springbarley"||crops[4]=="springoats"){
          wr[29,2] <- wr[29,3] <- wr[29,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="springbarley"||crops[5]=="springoats"){
          wr[29,2] <- wr[29,3] <- wr[29,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="springbarley"||crops[6]=="springoats"){
          wr[29,2] <- wr[29,3] <- wr[29,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[29,2] <- wr[29,3] <- wr[29,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        
        wr[30,2] <- wr[30,3] <- 
          round(((0.06+0.00025*({cr$springbarley[1]}))+(64.68+0.094*({cr$springbarley[1]}))/ tsize),2) # N fert
        cbsbar <- round(((1.15*{cr$springbarley[c3]}+24)/4)/combine,2) # Combine
        wr[31,2] <- 2* cbsbar; wr[31,3] <- 3* cbsbar; wr[31,6] <- cbsbar
        basbar <- round((({cr$springbarley[c5]}+13)/4)/combine*0.5,2) # Bale
        wr[32,2] <- 3* basbar; wr[32,3] <- 3* basbar; wr[32,7] <- basbar
      }else{
        wr[seq(25,32),seq(2,12)] <- 0
      }
      
      if("winterbeans"%in%crops==TRUE){
        # Work rates for winter beans operations =====
        wr[33,2] <- wr[33,3] <- 
          round(((0.06+0.00025*({cr$winterbeans[3]}+{cr$winterbeans[5]}))+
                   (64.48+0.094*({cr$winterbeans[3]}+{cr$winterbeans[5]}))/ tsize),2) # Spread P/K fert
        wr[34,2] <- wr[34,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        wr[35,2] <- wr[35,3] <- round((3*(0.114+0.00033*{cr$winterbeans[7]})+
                                         (54*{nsoil}+21.6)/tractor),2) # Sow
        wr[36,2] <- wr[36,3] <- round((1.5/(tpspeed*rollwidth/10))*0.85,2) # Roll
        
        #wr[37,2] <- wr[37,3] <- wr[37,5] <- round(0.11+204.2/sprayer,2) #Spray
        if(crops[1]=="winterbeans"){
          wr[37,2] <- wr[37,3] <- wr[37,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="winterbeans"){
          wr[37,2] <- wr[37,3] <- wr[37,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="winterbeans"){
          wr[37,2] <- wr[37,3] <- wr[37,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="winterbeans"){
          wr[37,2] <- wr[37,3] <- wr[37,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="winterbeans"){
          wr[37,2] <- wr[37,3] <- wr[37,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="winterbeans"){
          wr[37,2] <- wr[37,3] <- wr[37,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[37,2] <- wr[37,3] <- wr[37,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        cbwbea <- round((4.05*({cr$winterbeans[c3]}+24)/4)/combine,2) #Combine
        wr[38,2] <- 2* cbwbea; wr[38,3] <- 3* cbwbea; wr[38,6] <- cbwbea
      }else{
        wr[seq(33,38),seq(2,12)] <- 0
      }
      
      if("springbeans"%in%crops==TRUE){
        # Work rates for spring beans operations =====
        wr[39,2] <- wr[39,3] <- 
          round(((0.06+0.00025*({cr$springbeans[2]}+{cr$springbeans[3]}))+
                   (64.48+0.094*({cr$springbeans[2]}+{cr$springbeans[3]}))/ tsize),2) # Spread P/K fert
        wr[40,2] <- wr[40,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        wr[41,2] <- wr[41,3] <- round(((0.06+0.00069*{cr$springbeans[7]})+
                                         (92.42+0.00626*{cr$springbeans[7]}+41.5*{nsoil})/tractor),2) # Sow
        
        if(crops[1]=="springbeans"){
          wr[42,2] <- wr[42,3] <- wr[42,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="springbeans"){
          wr[42,2] <- wr[42,3] <- wr[42,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="springbeans"){
          wr[42,2] <- wr[42,3] <- wr[42,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="springbeans"){
          wr[42,2] <- wr[42,3] <- wr[42,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="springbeans"){
          wr[42,2] <- wr[42,3] <- wr[42,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="springbeans"){
          wr[42,2] <- wr[42,3] <- wr[42,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[42,2] <- wr[42,3] <- wr[42,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        cbsbea <- round((4.05*({cr$springbeans[c3]}+24)/4)/combine,2) # Harvest
        wr[43,2] <- 2* cbsbea; wr[43,3] <- 3* cbsbea; wr[43,6] <- cbsbea
      }else{
        wr[seq(39,43),seq(2,12)] <-0
      }
      
      if("warepotatoes"%in%crops==TRUE){
        # Work rates for ware potatoes operations =====
        wr[44,2] <- wr[44,3] <- 
          round(((1.80*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage 
        wr[45,2] <- wr[45,3] <- 
          wr[45,4] <- round(((25*{nsoil}+33)/tractor),2) # Harrowing
        wr[46,2] <- wr[46,3] <- 
          round(((278/tractor+0.04+0.55*{cr$warepotatoes[7]})/2000),2)*3 # Sow potatoes
        wr[47,2] <- wr[47,3] <- round(((40*{nsoil}+33)/tractor),2) # Ridging
        
        if(crops[1]=="warepotatoes"){
          wr[48,2] <- wr[48,3] <- wr[48,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="warepotatoes"){
          wr[48,2] <- wr[48,3] <- wr[48,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="warepotatoes"){
          wr[48,2] <- wr[48,3] <- wr[48,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="warepotatoes"){
          wr[48,2] <- wr[48,3] <- wr[48,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="warepotatoes"){
          wr[48,2] <- wr[48,3] <- wr[48,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="warepotatoes"){
          wr[48,2] <- wr[48,3] <- wr[48,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[48,2] <- wr[48,3] <- wr[48,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        wr[49,2] <- wr[49,3] <- round(((0.06+0.00025*({cr$warepotatoes[2]}+{cr$warepotatoes[3]}))+
                                         (64.48+0.094*({cr$warepotatoes[2]}+{cr$warepotatoes[3]}))/tsize),2) # Spread P/K fert
        wr[50,2] <- wr[50,3] <- 
          round(((0.06+0.00025*({cr$warepotatoes[1]}))+(64.68+0.094*({cr$warepotatoes[1]}))/ tsize),2) # N fert
        wr[51,2] <- wr[51,3] <-  round(1/(sphoe*rowsp/10*0.8),2) # Hoeing
        hpot <- round(((403/600)+2/(3*(1.25+0.51*{nsoil})*({39.43}/37.728)))*2.51,2) # Harvest pot
        wr[52,2] <- hpot*4; wr[52,3] <- hpot*4; wr[52,9] <- hpot 
      }else{
        wr[seq(44,52),seq(2,12)] <-0
      }
      
      if("wosr"%in%crops==TRUE){
        # Work rates for wosr operations =====
        wr[53,2] <- wr[53,3] <- round(((0.06+0.00025*({cr$wosr[2]}+{cr$wosr[3]}))+
                                         (64.48+0.094*({cr$wosr[2]}+{cr$wosr[3]}))/ tsize),2) # Spread P/K fert
        wr[54,2] <- wr[54,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        wr[55,2] <- wr[55,3] <- round(((0.387+0.00069*cr$wosr[7])+(99.42+0.00626*cr$wosr[7])/tractor),2)# Sow
        
        if(crops[1]=="wosr"){
          wr[56,2] <- wr[56,3] <- wr[25,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="wosr"){
          wr[56,2] <- wr[56,3] <- wr[25,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="wosr"){
          wr[56,2] <- wr[56,3] <- wr[25,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="wosr"){
          wr[56,2] <- wr[56,3] <- wr[25,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="wosr"){
          wr[56,2] <- wr[56,3] <- wr[25,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="wosr"){
          wr[56,2] <- wr[56,3] <- wr[25,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[56,2] <- wr[56,3] <- wr[25,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        wr[57,2] <- wr[57,3] <- 
          round(((0.06+0.00025*({cr$wosr[1]}))+(64.68+0.094*({cr$wosr[1]}))/ tsize),2) # N fert
        cbwosr <- round(((4.05*({cr$wosr[c3]}+24)/4)/combine),2) # Combine
        wr[58,2] <- 2* cbwosr; wr[58,3] <- 3* cbwosr; wr[58,6] <- cbwosr
      }else{
        wr[seq(53,58),seq(2,12)] <-0
      }
      
      if("sugarbeet"%in%crops==TRUE){
        # Work rates for sugarbeet operations =====
        wr[59,2] <- wr[59,3] <- round((1.80*(50*{nsoil}+20))/tractor,2) # Ploughing 
        wr[60,2] <- wr[60,3]  <- wr[29,4] <- round(((25*{nsoil}+33)/tractor)*wkb,2) # Harrowing
        wr[61,2] <- wr[61,3] <- round((0.39+157/tractor), 2) # Planting
        
        if(crops[1]=="sugarbeet"){
          wr[62,2] <- wr[62,3] <- wr[62,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="sugarbeet"){
          wr[62,2] <- wr[62,3] <- wr[62,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="sugarbeet"){
          wr[62,2] <- wr[62,3] <- wr[62,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="sugarbeet"){
          wr[62,2] <- wr[62,3] <- wr[62,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="sugarbeet"){
          wr[62,2] <- wr[62,3] <- wr[62,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="sugarbeet"){
          wr[62,2] <- wr[62,3] <- wr[62,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[62,2] <- wr[62,3] <- wr[62,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        wr[63,2] <- wr[63,3] <- round(((0.06+0.00025*({cr$sugarbeet[2]}+{cr$sugarbeet[3]}))+
                                         (64.48+0.094*({cr$sugarbeet[2]}+{cr$sugarbeet[3]}))/ tsize),2) # Spread P/K fert
        wr[64,2] <- wr[64,3] <- 
          round(((0.06+0.00025*({cr$sugarbeet[1]}))+(64.68+0.094*({cr$sugarbeet[1]}))/ tsize),2) # N fert
        wr[65,2] <- wr[65,3] <-  round(1/(sphoe*rowsp/10*0.8),2) # Hoeing
        hvsbee <- round(((403/600)+2/(3*(1.25+0.51*{nsoil})*({cr$sugarbeet[c3]}/37.728))),2) # Harvest
        wr[66,2] <- 3* hvsbee; wr[66,3] <- 3* hvsbee; wr[66,8] <- hvsbee
      }else{
        wr[seq(59,66),seq(2,12)] <-0
      }
      
      if("setaside"%in%crops==TRUE){#||"fallow"%in%crops==TRUE
        # Work rates for setaside operations =====
        wr[67,2] <- wr[67,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        #wr[68,2] <- wr[68,3] <- wr[68,5] <- round(0.11+204.2/sprayer,2)
        if(crops[1]=="setaside"){
          wr[68,2] <- wr[68,3] <- wr[68,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="setaside"){
          wr[68,2] <- wr[68,3] <- wr[68,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="setaside"){
          wr[68,2] <- wr[68,3] <- wr[68,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="setaside"){
          wr[68,2] <- wr[68,3] <- wr[68,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="setaside"){
          wr[68,2] <- wr[68,3] <- wr[68,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="setaside"){
          wr[68,2] <- wr[68,3] <- wr[68,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[68,2] <- wr[68,3] <- wr[68,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        
      }else{
        wr[seq(67,68),seq(2,12)] <-0
      }
      
      if("sosr"%in%crops==TRUE){
        # Work rates for sosr operations =====
        wr[69,2] <- wr[69,3] <- round(((0.06+0.00025*({cr$sosr[2]}+{cr$sosr[3]}))+
                                         (64.48+0.094*({cr$sosr[2]}+{cr$sosr[3]}))/ tsize),2) # Spread P/K fert
        wr[70,2] <- wr[70,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        wr[71,2] <- wr[71,3] <- round(((0.06+0.00069*cr$sosr[7])+(92.42+0.00626*(cr$sosr[7])+41.5*(nsoil))/tractor),2) # Sow
        
        if(crops[1]=="sosr"){
          wr[72,2] <- wr[72,3] <- wr[72,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="sosr"){
          wr[72,2] <- wr[72,3] <- wr[72,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="sosr"){
          wr[72,2] <- wr[72,3] <- wr[72,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="sosr"){
          wr[72,2] <- wr[72,3] <- wr[72,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="sosr"){
          wr[72,2] <- wr[72,3] <- wr[72,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="sosr"){
          wr[72,2] <- wr[72,3] <- wr[72,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[72,2] <- wr[72,3] <- wr[72,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        wr[73,2] <- wr[73,3] <- 
          round(((0.06+0.00025*({cr$sosr[1]}))+(64.68+0.094*({cr$sosr[1]}))/ tsize),2) # N fert
        cbsosr <- round(((4.05*({cr$sosr[c3]}+24)/4)/combine),2) # Harvest
        wr[74,2] <- 2* cbsosr; wr[74,3] <- 3* cbsosr; wr[74,6] <- cbsosr
      }else{
        wr[seq(69,74),seq(2,12)] <-0
      }
      
      if("winterlinseed"%in%crops==TRUE){
        # Work rates for winterlinseed operations =====
        wr[75,2] <- wr[75,3] <- round(((0.06+0.00025*({cr$winterlinseed[2]}+{cr$winterlinseed[3]}))+
                                         (64.48+0.094*({cr$winterlinseed[2]}+{cr$winterlinseed[3]}))/ tsize),2) # Spread P/K fert
        wr[76,2] <- wr[76,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        
        wr[77,2] <- wr[77,3] <- round(((0.06+0.00069*cr$winterlinseed[7])+
                                         (92.42+0.00626*(cr$winterlinseed[7])+41.5*(nsoil))/tractor),2) # Planting
        
        if(crops[1]=="winterlinseed"){
          wr[78,2] <- wr[78,3] <- wr[78,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="winterlinseed"){
          wr[78,2] <- wr[78,3] <- wr[78,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="winterlinseed"){
          wr[78,2] <- wr[78,3] <- wr[78,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="winterlinseed"){
          wr[78,2] <- wr[78,3] <- wr[78,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="winterlinseed"){
          wr[78,2] <- wr[78,3] <- wr[78,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="winterlinseed"){
          wr[78,2] <- wr[78,3] <- wr[78,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[78,2] <- wr[78,3] <- wr[78,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        wr[79,2] <- wr[79,3] <- 
          round(((0.06+0.00025*({cr$winterlinseed[1]}))+(64.68+0.094*({cr$winterlinseed[1]}))/ tsize),2) # N fert
        cbwlin <- round(((4.05*({cr$winterlinseed[c3]}+24)/4)/combine),2) # Harvest
        wr[80,2] <- 2* cbwlin; wr[80,3] <- 3* cbwlin; wr[80,6] <- cbwlin
      }else{
        wr[seq(75,80),seq(2,12)] <-0
      }
      
      if("springlinseed"%in%crops==TRUE){
        # Work rates for springlinseed operations =====
        wr[81,2] <- wr[81,3] <- round(((0.06+0.00025*({cr$springlinseed[2]}+{cr$springlinseed[3]}))+
                                         (64.48+0.094*({cr$springlinseed[2]}+{cr$springlinseed[3]}))/ tsize),2) # Spread P/K fert
        wr[82,2] <- wr[82,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        
        wr[83,2] <- wr[83,3] <- round(((0.06+0.00069*cr$springlinseed[7])+
                                         (92.42+0.00626*(cr$springlinseed[7])+41.5*(nsoil))/tractor),2) # Planting
        
        if(crops[1]=="springlinseed"){
          wr[84,2] <- wr[84,3] <- wr[84,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="springlinseed"){
          wr[84,2] <- wr[84,3] <- wr[84,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="springlinseed"){
          wr[84,2] <- wr[84,3] <- wr[84,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="springlinseed"){
          wr[84,2] <- wr[84,3] <- wr[84,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="springlinseed"){
          wr[84,2] <- wr[84,3] <- wr[84,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="springlinseed"){
          wr[84,2] <- wr[84,3] <- wr[84,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[84,2] <- wr[84,3] <- wr[84,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        wr[85,2] <- wr[85,3] <- 
          round(((0.06+0.00025*({cr$springlinseed[1]}))+(64.68+0.094*({cr$springlinseed[1]}))/ tsize),2) # N fert
        cbslin <- round(((4.05*({cr$springlinseed[c3]}+24)/4)/combine),2) # Harvest
        wr[86,2] <- 2* cbslin; wr[86,3] <- 3* cbslin; wr[86,6] <- cbslin
      }else{
        wr[seq(81,86),seq(2,12)] <-0
      }
      
      if("driedpeas"%in%crops==TRUE||"peas"%in%crops==TRUE){
        # Work rates for peas operations =====
        wr[87,2] <- wr[87,3] <- 
          round(((0.06+0.00025*({cr$driedpeas[3]}+{cr$driedpeas[5]}))+
                   (64.48+0.094*({cr$driedpeas[3]}+{cr$driedpeas[5]}))/ tsize),2) # P/K Fert
        wr[88,2] <- wr[88,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        
        wr[89,2] <- wr[89,3] <- round((0.06+0.00069*{cr$driedpeas[7]})+
                                        (92.42+0.00626*{cr$driedpeas[7]}+41.5*{nsoil})/tractor,2) # Planting
        wr[90,2] <- wr[90,3] <- round((1.5/(tpspeed*rollwidth/10))*0.85,2) # Roll
        
        if(crops[1]=="driedpeas"||crops[1]=="peas"){
          wr[91,2] <- wr[91,3] <- wr[91,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="driedpeas"||crops[2]=="peas"){
          wr[91,2] <- wr[91,3] <- wr[91,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="driedpeas"||crops[3]=="peas"){
          wr[91,2] <- wr[91,3] <- wr[91,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="driedpeas"||crops[4]=="peas"){
          wr[91,2] <- wr[91,3] <- wr[91,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="driedpeas"||crops[5]=="peas"){
          wr[91,2] <- wr[91,3] <- wr[91,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="driedpeas"||crops[6]=="peas"){
          wr[91,2] <- wr[91,3] <- wr[91,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[91,2] <- wr[91,3] <- wr[91,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        cbdpea <- round((4.05*({cr$driedpeas[c3]}+24)/4)/combine,2) # Harvest
        wr[92,2] <- 2* cbdpea; wr[92,3] <- 3* cbdpea; wr[92,6] <- cbdpea
      }else{
        wr[seq(87,92),seq(2,12)] <- 0
      }
      
      kk3 <- wr
      
      
      # ******* Estimates fuel and labour costs ***************
      
      fuelPrice <- fuelprice #farm[2]
      labourWage <- labourwage #farm[3]
      TP <- round(tractor*1.341,0) # maximum PTO horsepower 
      CP <- round(combsize*1.341,0)
      
      # For diesel tractor, fuel consumption is estimated from the formula below
      # Obtained from: http://www.extension.iastate.edu/agdm/crops/html/a3-29.html (16/06/2015)
      # 0.044 * maximum PTO horsepower for diesel engines
      
      fuel.cons_gal_hr <- round(0.044 * TP,2) # Gallons per hour Tractor
      fuel.cons_gal_hr_comb <- round(0.044 * CP,2) # Combine
      # Convert gallons per hour to litres per hour
      # 1 gallon per hour = 4.546 litres per hour
      
      Fuel_Cons <- round(fuel.cons_gal_hr * 4.546,0) # Fuel consumption (litres/hour)
      Fuel_Cons_comb <- round(fuel.cons_gal_hr_comb * 4.546,0)
      
      # Fuel and labour cost (£/hour)
      fuelCost <-  (fuelPrice * Fuel_Cons)*1.1 # 10% represents oil and lubricants
      fuelCost_comb <- (fuelPrice * Fuel_Cons_comb)*1.1
      
      #Fuel and labour costs @ £/ha (Multiply £/hour by workrates (hr/ha))
      kk3[,10] <- round((kk3[,2]*fuelCost)+(kk3[,6]*fuelCost_comb)+
                          (kk3[,7]*fuelCost_comb)) # Fuel costs
      kk3[,11] <- round(kk3[,3]*labourWage) # Labour costs
      kk3[,12] <- round(kk3[,10]+kk3[,11])
      ops <- kk3
      
      # Fuel cost
      fu <- ops[,10]
      
      fuelcost <- c(sum(fu[seq(1,8)]),sum(fu[seq(9,16)]),sum(fu[seq(17,24)]),sum(fu[seq(25,32)]),
                    sum(fu[seq(33,38)]),sum(fu[seq(39,43)]),sum(fu[seq(44,52)]),sum(fu[seq(53,58)]),sum(fu[seq(59,66)]),
                    sum(fu[seq(67,68)]),sum(fu[seq(69,74)]),sum(fu[seq(75,80)]),sum(fu[seq(81,86)]),sum(fu[seq(87,92)]))
      # Labour cost
      la <- ops[,11]
      
      labourcost <- c(sum(la[seq(1,8)]),sum(la[seq(9,16)]),sum(la[seq(17,24)]),sum(la[seq(25,32)]),
                      sum(la[seq(33,38)]),sum(la[seq(39,43)]),sum(la[seq(44,52)]),sum(la[seq(53,58)]),sum(la[seq(59,66)]),
                      sum(la[seq(67,68)]),sum(la[seq(69,74)]),sum(la[seq(75,80)]),sum(la[seq(81,86)]),sum(la[seq(87,92)]))
      # Operating Cost
      op1 <- ops[,12]
      
      opcost <- c(sum(op1[seq(1,8)]),sum(op1[seq(9,16)]),sum(op1[seq(17,24)]),sum(op1[seq(25,32)]),
                  sum(op1[seq(33,38)]),sum(op1[seq(39,43)]),sum(op1[seq(44,52)]),sum(op1[seq(53,58)]),sum(op1[seq(59,66)]),
                  sum(op1[seq(67,68)]),sum(op1[seq(69,74)]),sum(op1[seq(75,80)]),sum(op1[seq(81,86)]),sum(op1[seq(87,92)]))
      
      crops <- crops
      
      if(crops[1]=="winterwheat"){
        fc1 <- fuelcost[1]; lc1 <- labourcost[1]; opc1 <- opcost[1]
      }else if(crops[1]=="springwheat"){
        fc1 <- fuelcost[2]; lc1 <- labourcost[2]; opc1 <- opcost[2]
      }else if(crops[1]=="winterbarley"||crops[1]=="winteroats"){
        fc1 <- fuelcost[3]; lc1 <- labourcost[3]; opc1 <- opcost[3]
      }else if(crops[1]=="springbarley"||crops[1]=="springoats"){
        fc1 <- fuelcost[4]; lc1 <- labourcost[4]; opc1 <- opcost[4]
      }else if(crops[1]=="winterbeans"){
        fc1 <- fuelcost[5]; lc1 <- labourcost[5]; opc1 <- opcost[5]
      }else if(crops[1]=="springbeans"){
        fc1 <- fuelcost[6]; lc1 <- labourcost[6]; opc1 <- opcost[6]
      }else if(crops[1]=="warepotatoes"){
        fc1 <- fuelcost[7]; lc1 <- labourcost[7]; opc1 <- opcost[7]
      }else if(crops[1]=="wosr"){
        fc1 <- fuelcost[8]; lc1 <- labourcost[8]; opc1 <- opcost[8]
      }else if(crops[1]=="sugarbeet"){
        fc1 <- fuelcost[9]; lc1 <- labourcost[9]; opc1 <- opcost[9]
      }else if(crops[1]=="setaside"||crops[1]=="fallow"){
        fc1 <- fuelcost[10]; lc1 <- labourcost[10]; opc1 <- opcost[10]
      }else if(crops[1]=="sosr"){
        fc1 <- fuelcost[11]; lc1 <- labourcost[11]; opc1 <- opcost[11]
      }else if(crops[1]=="winterlinseed"){
        fc1 <- fuelcost[12]; lc1 <- labourcost[12]; opc1 <- opcost[12]
      }else if(crops[1]=="springlinseed"){
        fc1 <- fuelcost[13]; lc1 <- labourcost[13]; opc1 <- opcost[13]
      }else if(crops[1]=="driedpeas"){
        fc1 <- fuelcost[14]; lc1 <- labourcost[14]; opc1 <- opcost[14]
      }
      
      if(crops[2]=="winterwheat"){
        fc2 <- fuelcost[1]; lc2 <- labourcost[1]; opc2 <- opcost[1]
      }else if(crops[2]=="springwheat"){
        fc2 <- fuelcost[2]; lc2 <- labourcost[2]; opc2 <- opcost[2]
      }else if(crops[2]=="winterbarley"||crops[2]=="winteroats"){
        fc2 <- fuelcost[3]; lc2 <- labourcost[3]; opc2 <- opcost[3]
      }else if(crops[2]=="springbarley"||crops[2]=="springoats"){
        fc2 <- fuelcost[4]; lc2 <- labourcost[4]; opc2 <- opcost[4]
      }else if(crops[2]=="winterbeans"){
        fc2 <- fuelcost[5]; lc2 <- labourcost[5]; opc2 <- opcost[5]
      }else if(crops[2]=="springbeans"){
        fc2 <- fuelcost[6]; lc2 <- labourcost[6]; opc2 <- opcost[6]
      }else if(crops[2]=="warepotatoes"){
        fc2 <- fuelcost[7]; lc2 <- labourcost[7]; opc2 <- opcost[7]
      }else if(crops[2]=="wosr"){
        fc2 <- fuelcost[8]; lc2 <- labourcost[8]; opc2 <- opcost[8]
      }else if(crops[2]=="sugarbeet"){
        fc2 <- fuelcost[9]; lc2 <- labourcost[9]; opc2 <- opcost[9]
      }else if(crops[2]=="setaside"||crops[2]=="fallow"){
        fc2 <- fuelcost[10]; lc2 <- labourcost[10]; opc2 <- opcost[10]
      }else if(crops[2]=="sosr"){
        fc2 <- fuelcost[11]; lc2 <- labourcost[11]; opc2 <- opcost[11]
      }else if(crops[2]=="winterlinseed"){
        fc2 <- fuelcost[12]; lc2 <- labourcost[12]; opc2 <- opcost[12]
      }else if(crops[2]=="springlinseed"){
        fc2 <- fuelcost[13]; lc2 <- labourcost[13]; opc2 <- opcost[13]
      }else if(crops[2]=="driedpeas"){
        fc2 <- fuelcost[14]; lc2 <- labourcost[14]; opc2 <- opcost[14]
      }
      
      if(crops[3]=="winterwheat"){
        fc3 <- fuelcost[1]; lc3 <- labourcost[1]; opc3 <- opcost[1]
      }else if(crops[3]=="springwheat"){
        fc3 <- fuelcost[2]; lc3 <- labourcost[2]; opc3 <- opcost[2]
      }else if(crops[3]=="winterbarley"||crops[3]=="winteroats"){
        fc3 <- fuelcost[3]; lc3 <- labourcost[3]; opc3 <- opcost[3]
      }else if(crops[3]=="springbarley"||crops[3]=="springoats"){
        fc3 <- fuelcost[4]; lc3 <- labourcost[4]; opc3 <- opcost[4]
      }else if(crops[3]=="winterbeans"){
        fc3 <- fuelcost[5]; lc3 <- labourcost[5]; opc3 <- opcost[5]
      }else if(crops[3]=="springbeans"){
        fc3 <- fuelcost[6]; lc3 <- labourcost[6]; opc3 <- opcost[6]
      }else if(crops[3]=="warepotatoes"){
        fc3 <- fuelcost[7]; lc3 <- labourcost[7]; opc3 <- opcost[7]
      }else if(crops[3]=="wosr"){
        fc3 <- fuelcost[8]; lc3 <- labourcost[8]; opc3 <- opcost[8]
      }else if(crops[3]=="sugarbeet"){
        fc3 <- fuelcost[9]; lc3 <- labourcost[9]; opc3 <- opcost[9]
      }else if(crops[3]=="setaside"||crops[3]=="fallow"){
        fc3 <- fuelcost[10]; lc3 <- labourcost[10]; opc3 <- opcost[10]
      }else if(crops[3]=="sosr"){
        fc3 <- fuelcost[11]; lc3 <- labourcost[11]; opc3 <- opcost[11]
      }else if(crops[3]=="winterlinseed"){
        fc3 <- fuelcost[12]; lc3 <- labourcost[12]; opc3 <- opcost[12]
      }else if(crops[3]=="springlinseed"){
        fc3 <- fuelcost[13]; lc3 <- labourcost[13]; opc3 <- opcost[13]
      }else if(crops[3]=="driedpeas"){
        fc3 <- fuelcost[14]; lc3 <- labourcost[14]; opc3 <- opcost[14]
      }
      
      if(crops[4]=="winterwheat"){
        fc4 <- fuelcost[1]; lc4 <- labourcost[1]; opc4 <- opcost[1]
      }else if(crops[4]=="springwheat"){
        fc4 <- fuelcost[2]; lc4 <- labourcost[2]; opc4 <- opcost[2]
      }else if(crops[4]=="winterbarley"||crops[4]=="winteroats"){
        fc4 <- fuelcost[3]; lc4 <- labourcost[3]; opc4 <- opcost[3]
      }else if(crops[4]=="springbarley"||crops[4]=="springoats"){
        fc4 <- fuelcost[4]; lc4 <- labourcost[4]; opc4 <- opcost[4]
      }else if(crops[4]=="winterbeans"){
        fc4 <- fuelcost[5]; lc4 <- labourcost[5]; opc4 <- opcost[5]
      }else if(crops[4]=="springbeans"){
        fc4 <- fuelcost[6]; lc4 <- labourcost[6]; opc4 <- opcost[6]
      }else if(crops[4]=="warepotatoes"){
        fc4 <- fuelcost[7]; lc4 <- labourcost[7]; opc4 <- opcost[7]
      }else if(crops[4]=="wosr"){
        fc4 <- fuelcost[8]; lc4 <- labourcost[8]; opc4 <- opcost[8]
      }else if(crops[4]=="sugarbeet"){
        fc4 <- fuelcost[9]; lc4 <- labourcost[9]; opc4 <- opcost[9]
      }else if(crops[4]=="setaside"||crops[4]=="fallow"){
        fc4 <- fuelcost[10]; lc4 <- labourcost[10]; opc4 <- opcost[10]
      }else if(crops[4]=="sosr"){
        fc4 <- fuelcost[11]; lc4 <- labourcost[11]; opc4 <- opcost[11]
      }else if(crops[4]=="winterlinseed"){
        fc4 <- fuelcost[12]; lc4 <- labourcost[12]; opc4 <- opcost[12]
      }else if(crops[4]=="springlinseed"){
        fc4 <- fuelcost[13]; lc4 <- labourcost[13]; opc4 <- opcost[13]
      }else if(crops[4]=="driedpeas"){
        fc4 <- fuelcost[14]; lc4 <- labourcost[14]; opc4 <- opcost[14]
      }
      
      if(crops[5]=="winterwheat"){
        fc5 <- fuelcost[1]; lc5 <- labourcost[1]; opc5 <- opcost[1]
      }else if(crops[5]=="springwheat"){
        fc5 <- fuelcost[2]; lc5 <- labourcost[2]; opc5 <- opcost[2]
      }else if(crops[5]=="winterbarley"||crops[5]=="winteroats"){
        fc5 <- fuelcost[3]; lc5 <- labourcost[3]; opc5 <- opcost[3]
      }else if(crops[5]=="springbarley"||crops[5]=="springoats"){
        fc5 <- fuelcost[4]; lc5 <- labourcost[4]; opc5 <- opcost[4]
      }else if(crops[5]=="winterbeans"){
        fc5<- fuelcost[5]; lc5 <- labourcost[5]; opc5 <- opcost[5]
      }else if(crops[5]=="springbeans"){
        fc5 <- fuelcost[6]; lc5 <- labourcost[6]; opc5 <- opcost[6]
      }else if(crops[5]=="warepotatoes"){
        fc5 <- fuelcost[7]; lc5 <- labourcost[7]; opc5 <- opcost[7]
      }else if(crops[5]=="wosr"){
        fc5 <- fuelcost[8]; lc5 <- labourcost[8]; opc5 <- opcost[8]
      }else if(crops[5]=="sugarbeet"){
        fc5 <- fuelcost[9]; lc5 <- labourcost[9]; opc5 <- opcost[9]
      }else if(crops[5]=="setaside"||crops[5]=="fallow"){
        fc5 <- fuelcost[10]; lc5 <- labourcost[10]; opc5 <- opcost[10]
      }else if(crops[5]=="sosr"){
        fc5 <- fuelcost[11]; lc5 <- labourcost[11]; opc5 <- opcost[11]
      }else if(crops[5]=="winterlinseed"){
        fc5 <- fuelcost[12]; lc5 <- labourcost[12]; opc5 <- opcost[12]
      }else if(crops[5]=="springlinseed"){
        fc5 <- fuelcost[13]; lc5 <- labourcost[13]; opc5 <- opcost[13]
      }else if(crops[5]=="driedpeas"){
        fc5 <- fuelcost[14]; lc5 <- labourcost[14]; opc5 <- opcost[14]
      }
      
      if(crops[6]=="winterwheat"){
        fc6 <- fuelcost[1]; lc6 <- labourcost[1]; opc6 <- opcost[1]
      }else if(crops[6]=="springwheat"){
        fc6 <- fuelcost[2]; lc6 <- labourcost[2]; opc6 <- opcost[2]
      }else if(crops[6]=="winterbarley"||crops[6]=="winteroats"){
        fc6 <- fuelcost[3]; lc6 <- labourcost[3]; opc6 <- opcost[3]
      }else if(crops[6]=="springbarley"||crops[6]=="springoats"){
        fc6 <- fuelcost[4]; lc6 <- labourcost[4]; opc6 <- opcost[4]
      }else if(crops[6]=="winterbeans"){
        fc6 <- fuelcost[5]; lc6 <- labourcost[5]; opc6 <- opcost[5]
      }else if(crops[6]=="springbeans"){
        fc6 <- fuelcost[6]; lc6 <- labourcost[6]; opc6 <- opcost[6]
      }else if(crops[6]=="warepotatoes"){
        fc6 <- fuelcost[7]; lc6 <- labourcost[7]; opc6 <- opcost[7]
      }else if(crops[6]=="wosr"){
        fc6 <- fuelcost[8]; lc6 <- labourcost[8]; opc6 <- opcost[8]
      }else if(crops[6]=="sugarbeet"){
        fc6 <- fuelcost[9]; lc6 <- labourcost[9]; opc6 <- opcost[9]
      }else if(crops[6]=="setaside"||crops[6]=="fallow"){
        fc6 <- fuelcost[10]; lc6 <- labourcost[10]; opc6 <- opcost[10]
      }else if(crops[6]=="sosr"){
        fc6 <- fuelcost[11]; lc6 <- labourcost[11]; opc6 <- opcost[11]
      }else if(crops[6]=="winterlinseed"){
        fc6 <- fuelcost[12]; lc6 <- labourcost[12]; opc6 <- opcost[12]
      }else if(crops[6]=="springlinseed"){
        fc6 <- fuelcost[13]; lc6 <- labourcost[13]; opc6 <- opcost[13]
      }else if(crops[6]=="driedpeas"){
        fc6 <- fuelcost[14]; lc6 <- labourcost[14]; opc6 <- opcost[14]
      }
      
      cr[c11,seq(2,le)]  <- c(fc1,fc2,fc3,fc4,fc5,fc6)
      cr[c12,seq(2,le)]  <- c(lc1,lc2,lc3,lc4,lc5,lc6)
      cr[c13,seq(2,le)]  <- c(opc1,opc2,opc3,opc4,opc5,opc6)
      
      cr[c14,seq(2,le)] <- grossprofit <- cr[c10,seq(2,le)]-cr[c13,seq(2,le)] # Gross profit estimates
      kk4 <- cr[seq(1,31),]
    }
    
    
    # Model output components =====
    #**************************************************************** 
    rotl <- c(2,3,4,5,6)
    if(rotlength%in%rotl==TRUE){
      nrotl <- rotlength
    }else{
      nrotl <- 6
      warning("Maximum 6-year rotation is assumed (Rotation length MUST be between 2 and 6 years)")
    }
    
    
    if(is.null(rotprob)){
      rotp <- 1/nrotl # Rotational probability (6 year rotation)
    }else{
      rotp <- rotprob
    }
    
    di <- 2
    Component=c("Basic Rotation","Cultivation","Farm Output","Fertiliser Cost","Seed Cost","Herbicide Cost",
                "Sundry Cost","Variable Cost","Gross Margin",
                "Fuel Cost","Labour Cost","Operating Cost","Gross Profit","Rotation Profit")
    
    Unit=c("na","na",rep("?/ha",length(Component)-2))
    
    # Row Indices ===
    c1 <- 18; c2 <- 19; c3 <- 20; c4 <- 21; c5 <- 22; c6 <- 23; c7 <- 24; c8 <- 25; c9 <- 26;# === row indices
    c10 <- 27; c11 <- 28; c12 <- 29; c13 <- 30; c14 <- 31; 
    
    h1 <- 10; h2 <- 11; h3 <- 12; h4 <- 13; h5 <- 14; h6 <- 15;
    h7 <- 16; h8 <- 17; 
    
    # Year 1  *************************************************************
    
    crop1 <- crops[1]; crop2 <- crops[2]; crop3 <- crops[3]; crop4 <- crops[4]; crop5 <- crops[5]; crop6 <- crops[6]
    tillage1 <- tillages[1]; tillage2 <- tillages[2]; tillage3 <- tillages[3]; tillage4 <- tillages[4]; tillage5 <- tillages[5];
    tillage6 <- tillages[6]
    
    if(crop1=="winterwheat"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("WWHT",tillages[1],ncb)
      #cp1 <- "WWHT"
      cy1 <- cb[c3]
    }else if(crop1=="springwheat"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("SWHT",tillages[1],ncb) 
      #cp1 <- "SWHT"
      cy1 <- cb[c3]
    }else if (crop1=="winterbarley"){
      cb <- cropBudget(crop=crops, nsoil, subsidy, blackgrass, tillage1, selfrot=0, rotpen=0, yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("WBAR",tillages[1],ncb) 
      #cp1 <- "WBAR"
      cy1 <- cb[c3]
    }else if(crop1=="springbarley"){
      cb <- cropBudget(crop=crops, nsoil, subsidy, blackgrass, tillage1, selfrot=0, rotpen=0, yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("SBAR",tillages[1],ncb) 
      #cp1 <- "SBAR"
      cy1 <- cb[c3]
    }else if(crop1=="winterbeans"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("WBEA",tillages[1],ncb) 
      cp1 <- "WBEA"
      cy1 <- cb[c3]
    }else if(crop1=="springbeans"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("SBEA",tillages[1],ncb) 
      #cp1 <- "SBEA"
      cy1 <- cb[c3]
    }else if(crop1=="warepotatoes"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("WPOT",tillages[1],ncb) 
      #cp1 <- "WPOT"
      cy1 <- cb[c3]
    }else if(crop1=="wosr"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("WOSR",tillages[1],ncb) 
      #cp1 <- "WOSR"
      cy1 <- cb[c3]
    }else if(crop1=="sugarbeet"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,h8)],cb[c1]+cb[c2],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("SBEE",tillages[1],ncb) 
      #cp1 <- "SBEE"
      cy1 <- cb[c3]
    }else if(crop1=="setaside"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("SETA",tillages[1],ncb) 
      #cp1 <- "SETA"
      cy1 <- cb[c3]
    }else if(crop1=="none"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("SETA",tillages[1],ncb) 
      #cp1 <- "SETA"
      cy1 <- cb[c3]
    }else if(crop1=="fallow"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("FALLOW",tillages[1],ncb)
      #cp1 <- "SETA"
      cy1 <- cb[c3]
    }else if(crop1=="sosr"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("SOSR",tillages[1],ncb)
      cy1 <- cb[c3]
    }else if(crop1=="winterlinseed"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("WLIN",tillages[1],ncb)
      cy1 <- cb[c3]
    }else if(crop1=="springlinseed"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("SLIN",tillages[1],ncb)
      cy1 <- cb[c3]
    }else if(crop1=="driedpeas"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("DPEA",tillages[1],ncb)
      cy1 <- cb[c3]
    }
    
    # Year 2  *************************************************************
    # ========= Yield Penalty | Crop Rotations =========
    if(crop2=="winterwheat"){ 
      
      if(crop1=="winterwheat"){
        selfrot <- 10 # 10% Yield loss
      }else{
        selfrot = 0
      }
      if(crop1=="winterbarley"||crop1=="springbarley"||crop1=="springwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("WWHT",tillage2,ncb2) 
      cy2 <- cb2[c3]
    }else if(crop2=="springwheat"){
      if(crop1=="springwheat"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      if(crop1=="winterbarley"||crop1=="springbarley"||crop1=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("SWHT",tillage2,ncb2) 
      cy2 <- cb2[c3]
    }else if (crop2=="winterbarley"){
      if(crop1=="winterbarley"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      if(crop1=="springwheat"||crop1=="springbarley"||crop1=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("WBAR",tillage2,ncb2) 
      cy2 <- cb2[c3]
    }else if(crop2=="springbarley"){
      if(crop1=="springbarley"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      if(crop1=="springwheat"||crop1=="winterbarley"||crop1=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("SBAR",tillage2,ncb2) 
      cy2 <- cb2[c3]
    }else if(crop2=="winterbeans"){
      selfrot <- 0
      if(crop1=="wosr"||crop1=="sugarbeet"||crop1=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("WBEA",tillage2,ncb2) 
      cy2 <- cb2[c3]
    }else if(crop2=="springbeans"){
      selfrot <- 0
      if(crop1=="wosr"||crop1=="sugarbeet"||crop1=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("SBEA",tillage2,ncb2) 
      cy2 <- cb2[c3]
    }else if(crop2=="warepotatoes"){
      if(crop1=="warepoatoes"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Potatoes is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop1=="sugarbeet"){ 
        rotpen <- 5
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("WPOT",tillage2,ncb2)  
      cy2 <- cb2[c3]
    }else if(crop2=="wosr"){
      if(crop1=="wosr"||crop1=="sosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop1=="winterbeans"||crop1=="springbeans"||crop1=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("WOSR",tillage2,ncb2)  
      cy2 <- cb2[c3]
    }else if(crop2=="sugarbeet"){
      if(crop1=="sugarbeat"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Sugarbeet is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop1=="winterbeans"||crop1=="springbeans"||crop1=="wosr"||crop1=="warepotatoes"||crop1=="sosr"){ 
        rotpen <- 100
        warning("Oilseed rape, Beans or Potatoes--Sugarbeet sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,h8)],cb2[c1]+cb2[c2],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("SBEE",tillage2,ncb2)  
      cy2 <- cb2[c3]
    }else if(crop2=="setaside"){ 
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot=0,rotpen=0,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("SETA",tillage2,ncb2)
      cy2 <- cb2[c3]
    }else if(crop2=="none"){
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot=0,rotpen=0,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("SETA",tillage2,ncb2)
      cy2 <- cb2[c3]
    }else if(crop2=="fallow"){
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot=0,rotpen=0,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("FALLOW",tillage2,ncb2) 
      cy2 <- cb2[c3]
    }else if(crop2=="sosr"){
      if(crop1=="sosr"||crop1=="wosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot = 0
      }
      if(crop1=="winterbeans"||crop1=="springbeans"||crop1=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("SOSR",tillage2,ncb2) 
      cy2 <- cb2[c3]
    }else if(crop2=="winterlinseed"){
      if(crop1=="winterlinseed"||crop1=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop1=="winterbeans"||crop1=="springbeans"||crop1=="sugarbeet"||crop1=="driedpeas"||crop1=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Linseed sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("WLIN",tillage2,ncb2) 
      cy2 <- cb2[c3]
    }else if(crop2=="springlinseed"){
      if(crop1=="winterlinseed"||crop1=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop1=="winterbeans"||crop1=="springbeans"||crop1=="sugarbeet"||crop1=="driedpeas"||crop1=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Linseed sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("SLIN",tillage2,ncb2)
      cy2 <- cb2[c3]
    }else if(crop2=="driedpeas"){
      if(crop1=="driedpeas"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Peas is penalised")
      }else{
        selfrot <- 0
      }
      if(crop1=="wosr"||crop1=="sosr"||crop1=="setaside"){ 
        rotpen <- 100
        warning("OSR and  Rotational Set-aside--Peas sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("DPEA",tillage2,ncb2)
      cy2 <- cb2[c3]
    }
    
    
    # Year 3  *************************************************************
    
    if(crop3=="winterwheat"){
      if(crop2=="winterwheat"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      if(crop2=="winterbarley"||crop2=="springbarley"||crop2=="springwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("WWHT",tillage3,ncb3) 
      cy3 <- cb3[c3]
    }else if(crop3=="springwheat"){
      if(crop2=="springwheat"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      
      if(crop2=="winterbarley"||crop2=="springbarley"||crop2=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("SWHT",tillage3,ncb3) 
      cy3 <- cb3[c3]
    }else if (crop3=="winterbarley"){
      if(crop2=="winterbarley"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      if(crop2=="springwheat"||crop2=="springbarley"||crop2=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("WBAR",tillage3,ncb3) 
      cy3 <- cb3[c3]
    }else if(crop3=="springbarley"){
      if(crop2=="springbarley"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      if(crop2=="springwheat"||crop2=="winterbarley"||crop2=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("SBAR",tillage3,ncb3) 
      cy3 <- cb3[c3]
    }else if(crop3=="winterbeans"){
      selfrot <- 0
      if(crop2=="wosr"||crop2=="sugarbeet"||crop2=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("WBEA",tillage3,ncb3) 
      cy3 <- cb3[c3]
    }else if(crop3=="springbeans"){
      selfrot <- 0
      if(crop2=="wosr"||crop2=="sugarbeet"||crop2=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("SBEA",tillage3,ncb3) 
      cy3 <- cb3[c3]
    }else if(crop3=="warepotatoes"){
      if(crop2=="warepoatoes"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Potatoes is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop2=="sugarbeet"){ 
        rotpen <- 5
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("WPOT",tillage3,ncb3)  
      cy3 <- cb3[c3]
    }else if(crop3=="wosr"){
      if(crop2=="wosr"||crop2=="sosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot <- 0
      }
      
      if(crop2=="winterbeans"||crop2=="springbeans"||crop2=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("WOSR",tillage3,ncb3)  
      cy3 <- cb3[c3]
    }else if(crop3=="sugarbeet"){
      if(crop2=="sugarbeat"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Sugarbeet is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop2=="winterbeans"||crop2=="springbeans"||crop2=="wosr"||crop2=="warepotatoes"||crop2=="sosr"){ 
        rotpen <- 100
        warning("Sugarbeet--Oilseed rape, Beans or Potatoes sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,h8)],cb3[c1]+cb3[c2],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("SBEE",tillage3,ncb3)  
      cy3 <- cb3[c3]
    }else if(crop3=="setaside"||crop3=="none"){ 
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot=0,rotpen=0,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("SETA",tillage3,ncb3) 
      cy3 <- cb3[c3]
    }else if(crop3=="none"){
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot=0,rotpen=0,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("SETA",tillage3,ncb3) 
      cy3 <- cb3[c3]
    }else if(crop3=="fallow"){
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot=0,rotpen=0,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("FALLOW",tillage3,ncb3)
      cy3 <- cb3[c3]
    }else if(crop3=="sosr"){
      if(crop2=="wosr"||crop2=="sosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot = 0
      }
      if(crop2=="winterbeans"||crop2=="springbeans"||crop2=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("SOSR",tillage3,ncb3)
      cy3 <- cb3[c3]
    }else if(crop3=="winterlinseed"){
      if(crop2=="winterlinseed"||crop2=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop2=="winterbeans"||crop2=="springbeans"||crop2=="sugarbeet"||crop2=="driedpeas"||crop2=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("WLIN",tillage3,ncb3)
      cy3 <- cb3[c3]
    }else if(crop3=="springlinseed"){
      if(crop2=="winterlinseed"||crop2=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop2=="winterbeans"||crop2=="springbeans"||crop2=="sugarbeet"||crop2=="driedpeas"||crop2=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("SLIN",tillage3,ncb3) 
      cy3 <- cb3[c3]
    }else if(crop3=="driedpeas"){
      if(crop2=="driedpeas"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Peas is penalised")
      }else{
        selfrot <- 0
      }
      if(crop2=="wosr"||crop2=="sosr"||crop2=="setaside"){ 
        rotpen <- 100
        warning("OSR and Rotational Set-aside--Peas sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("DPEA",tillage3,ncb3) 
      cy3 <- cb3[c3]
    } 
    
    
    # Year 4  *************************************************************
    
    if(crop4=="winterwheat"){
      
      if(crop3=="winterwheat"){
        selfrot <- 15 # 11% Yield loss
      }else{
        selfrot = 0
      }
      if(crop3=="winterbarley"||crop3=="springbarley"||crop3=="springwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("WWHT",tillage4,ncb4) 
      cy4 <- cb4[c3]
    }else if(crop4=="springwheat"){
      if(crop3=="springwheat"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      
      if(crop3=="winterbarley"||crop3=="springbarley"||crop3=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("SWHT",tillage4,ncb4) 
      cy4 <- cb4[c3]
    }else if (crop4=="winterbarley"){
      if(crop3=="winterbarley"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      if(crop3=="springwheat"||crop3=="springbarley"||crop3=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("WBAR",tillage4,ncb4) 
      cy4 <- cb4[c3]
    }else if(crop4=="springbarley"){
      if(crop3=="springbarley"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      if(crop3=="springwheat"||crop3=="winterbarley"||crop3=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("SBAR",tillage4,ncb4) 
      cy4 <- cb4[c3]
    }else if(crop4=="winterbeans"){
      selfrot <- 0
      if(crop3=="wosr"||crop3=="sugarbeet"||crop3=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("WBEA",tillage4,ncb4) 
      cy4 <- cb4[c3]
    }else if(crop4=="springbeans"){
      selfrot <- 0
      if(crop3=="wosr"||crop3=="sugarbeet"||crop3=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("SBEA",tillage4,ncb4) 
      cy4 <- cb4[c3]
    }else if(crop4=="warepotatoes"){
      if(crop3=="warepoatoes"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Potatoes is penalised")
      }else{
        selfrot = 0
      }
      if(crop3=="sugarbeet"){ 
        rotpen <- 5
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("WPOT",tillage4,ncb4)  
      cy4 <- cb4[c3]
    }else if(crop4=="wosr"){
      if(crop3=="wosr"||crop3=="sosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot = 0
      }
      if(crop3=="winterbeans"||crop3=="springbeans"||crop3=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("WOSR",tillage4,ncb4)  
      cy4 <- cb4[c3]
    }else if(crop4=="sugarbeet"){
      if(crop3=="sugarbeat"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Sugarbeet is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop3=="winterbeans"||crop3=="springbeans"||crop3=="wosr"||crop3=="warepotatoes"||crop3=="sosr"){ 
        rotpen <- 100
        warning("Sugarbeet--Oilseed rape, Beans or Potatoes sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,h8)],cb4[c1]+cb4[c2],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("SBEE",tillage4,ncb4)  
      cy4 <- cb4[c3]
    }else if(crop4=="setaside"){ 
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot=0,rotpen=0,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("SETA",tillage4,ncb4)
      cy4 <- cb4[c3]
    }else if(crop4=="none"){
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot=0,rotpen=0,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("SETA",tillage4,ncb4)
      cy4 <- cb4[c3]
    }else if(crop4=="fallow"){
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot=0,rotpen=0,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("FALLOW",tillage4,ncb4)
      cy4 <- cb4[c3]
    }else if(crop4=="sosr"){
      if(crop3=="wosr"||crop3=="sosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot = 0
      }
      if(crop3=="winterbeans"||crop3=="springbeans"||crop3=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("SOSR",tillage4,ncb4) 
      cy4 <- cb4[c3]
    }else if(crop4=="winterlinseed"){
      if(crop3=="winterlinseed"||crop3=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop3=="winterbeans"||crop3=="springbeans"||crop3=="sugarbeet"||crop3=="driedpeas"||crop3=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Linseed sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("WLIN",tillage4,ncb4)
      cy4 <- cb4[c3]
    }else if(crop4=="springlinseed"){
      if(crop3=="winterlinseed"||crop3=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop3=="winterbeans"||crop3=="springbeans"||crop3=="sugarbeet"||crop3=="driedpeas"||crop3=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Linseed sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("SLIN",tillage4,ncb4)
      cy4 <- cb4[c3]
    }else if(crop4=="driedpeas"){
      if(crop3=="driedpeas"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Peas is penalised")
      }else{
        selfrot <- 0
      }
      if(crop3=="wosr"||crop3=="sosr"||crop3=="setaside"){ 
        rotpen <- 100
        warning("OSR and Rotational Set-aside--Peas sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("DPEA",tillage4,ncb4)
      cy4 <- cb4[c3]
    }
    
    # Year 5  *************************************************************
    
    if(crop5=="winterwheat"){
      
      if(crop4=="winterwheat"){
        selfrot <- 15 # 15% Yield loss
      }else{
        selfrot = 0
      }
      
      if(crop4=="winterbarley"||crop4=="springbarley"||crop4=="springwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("WWHT",tillage5,ncb5) 
      cy5 <- cb5[c3]
    }else if(crop5=="springwheat"){
      if(crop4=="springwheat"){
        selfrot <- 11 # 
      }else{
        selfrot = 0
      }
      
      if(crop4=="winterbarley"||crop4=="springbarley"||crop4=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("SWHT",tillage5,ncb5) 
      cy5 <- cb5[c3]
    }else if (crop5=="winterbarley"){
      if(crop4=="winterbarley"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      
      if(crop4=="springwheat"||crop4=="springbarley"||crop4=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("WBAR",tillage5,ncb5) 
      cy5 <- cb5[c3]
    }else if(crop5=="springbarley"){
      if(crop4=="springbarley"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      
      if(crop4=="springwheat"||crop4=="winterbarley"||crop4=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("SBAR",tillage5,ncb5) 
      cy5 <- cb5[c3]
    }else if(crop5=="winterbeans"){
      selfrot <- 0
      if(crop4=="wosr"||crop4=="sugarbeet"||crop4=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("WBEA",tillage5,ncb5) 
      cy5 <- cb5[c3]
    }else if(crop5=="springbeans"){
      selfrot <- 0
      if(crop4=="wosr"||crop4=="sugarbeet"||crop4=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("SBEA",tillage5,ncb5) 
      cy5 <- cb5[c3]
    }else if(crop5=="warepotatoes"){
      if(crop4=="warepoatoes"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Potatoes is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop4=="sugarbeet"){ 
        rotpen <- 5
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("WPOT",tillage5,ncb5)  
      cy5 <- cb5[c3]
    }else if(crop5=="wosr"){
      if(crop4=="wosr"||crop4=="sosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop4=="winterbeans"||crop4=="springbeans"||crop4=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("WOSR",tillage5,ncb5)  
      cy5 <- cb5[c3]
    }else if(crop5=="sugarbeet"){
      if(crop4=="sugarbeat"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Sugarbeet is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop4=="winterbeans"||crop4=="springbeans"||crop4=="wosr"||crop4=="warepotatoes"||crop4=="sosr"){ 
        rotpen <- 100
        warning("Oilseed rape, Beans or Potatoes--Sugarbeet sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,h8)],cb5[c1]+cb5[c2],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("SBEE",tillage5,ncb5)  
      cy5 <- cb5[c3]
    }else if(crop5=="setaside"){ 
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot=0,rotpen=0,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("SETA",tillage5,ncb5) 
      cy5 <- cb5[c3]
    }else if(crop5=="none"){
      ccb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot=0,rotpen=0,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("SETA",tillage5,ncb5)
      cy5 <- cb5[c3]
    }else if(crop5=="fallow"){
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot=0,rotpen=0,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("FALLOW",tillage5,ncb5)
      cy5 <- cb5[c3]
    }else if(crop5=="sosr"){
      if(crop4=="wosr"||crop4=="sosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot = 0
      }
      if(crop4=="winterbeans"||crop4=="springbeans"||crop4=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("SOSR",tillage5,ncb5)
      cy5 <- cb5[c3]
    }else if(crop5=="winterlinseed"){
      if(crop4=="winterlinseed"||crop4=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop4=="winterbeans"||crop4=="springbeans"||crop4=="sugarbeet"||crop4=="driedpeas"||crop4=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Linseed sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("WLIN",tillage5,ncb5)
      cy5 <- cb5[c3]
    }else if(crop5=="springlinseed"){
      if(crop4=="winterlinseed"||crop4=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop4=="winterbeans"||crop4=="springbeans"||crop4=="sugarbeet"||crop4=="driedpeas"||crop4=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Linseed sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("SLIN",tillage5,ncb5) 
      cy5 <- cb5[c3]
    }else if(crop5=="driedpeas"){
      if(crop4=="driedpeas"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Peas is penalised")
      }else{
        selfrot <- 0
      }
      if(crop4=="wosr"||crop4=="sosr"||crop4=="setaside"){ 
        rotpen <- 100
        warning("OSR and Rotational Set-aside--Peas sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("DPEA",tillage5,ncb5)
      cy5 <- cb5[c3]
    }
    
    
    # Year 6  *************************************************************
    
    if(crop6=="winterwheat"){
      
      if(crop5=="winterwheat"){
        selfrot <- 17 # 17% Yield loss
      }else{
        selfrot = 0
      }
      if(crop5=="winterbarley"||crop5=="springbarley"||crop5=="springwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("WWHT",tillage6,ncb6) 
      cy6 <- cb6[c3]
    }else if(crop6=="springwheat"){
      if(crop5=="springwheat"){
        selfrot <- 12 # 12% Yield loss
      }else{
        selfrot = 0
      }
      
      if(crop5=="winterbarley"||crop5=="springbarley"||crop5=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("SWHT",tillage6,ncb6) 
      cy6 <- cb6[c3]
    }else if (crop6=="winterbarley"){
      if(crop5=="winterbarley"){
        selfrot <- 12 # 12% Yield loss
      }else{
        selfrot = 0
      }
      
      if(crop5=="springwheat"||crop5=="springbarley"||crop5=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("WBAR",tillage6,ncb6) 
      cy6 <- cb6[c3]
    }else if(crop6=="springbarley"){
      if(crop5=="springbarley"){
        selfrot <- 12 # 12% Yield loss
      }else{
        selfrot = 0
      }
      
      if(crop5=="springwheat"||crop5=="winterbarley"||crop5=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("SBAR",tillage6,ncb6) 
      cy6 <- cb6[c3]
    }else if(crop6=="winterbeans"){
      selfrot <- 0
      if(crop5=="wosr"||crop5=="sugarbeet"||crop5=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("WBEA",tillage6,ncb6) 
      cy6 <- cb6[c3]
    }else if(crop6=="springbeans"){
      selfrot <- 0
      if(crop5=="wosr"||crop5=="sugarbeet"||crop5=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("SBEA",tillage6,ncb6) 
      cy6 <- cb6[c3]
    }else if(crop6=="warepotatoes"){
      if(crop5=="warepoatoes"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Potatoes is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop5=="sugarbeet"){ 
        rotpen <- 5
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("WPOT",tillage6,ncb6)  
      cy6 <- cb6[c3]
    }else if(crop6=="wosr"){
      if(crop5=="wosr"||crop5=="sosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot = 0
      }
      if(crop5=="winterbeans"||crop5=="springbeans"||crop5=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("WOSR",tillage6,ncb6)  
      cy6 <- cb6[c3]
    }else if(crop6=="sugarbeet"){
      if(crop5=="sugarbeet"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Sugarbeet is penalised")
      }else{
        selfrot = 0
      }
      if(crop5=="winterbeans"||crop5=="springbeans"||crop5=="wosr"||crop5=="warepotatoes"||crop5=="sosr"){ 
        rotpen <- 100
        warning("Oilseed rape, Beans or Potatoes--Sugarbeet sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,h8)],cb6[c1]+cb6[c2],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("SBEE",tillage6,ncb6)  
      cy6 <- cb6[c3]
    }else if(crop6=="setaside"){ 
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot=0,rotpen=0,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("SETA",tillage6,ncb6)
      cy6 <- cb6[c3]
    }else if(crop6=="none"){
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot=0,rotpen=0,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("SETA",tillage6,ncb6) # c(rep(0,14))
      cy6 <- cb6[c3]
    }else if(crop6=="fallow"){
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot=0,rotpen=0,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("FALLOW",tillage6,ncb6) 
      cy6 <- cb6[c3]
    }else if(crop6=="sosr"){
      if(crop5=="wosr"||crop5=="sosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot = 0
      }
      if(crop5=="winterbeans"||crop5=="springbeans"||crop5=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("SOSR",tillage6,ncb6)
      cy6 <- cb6[c3]
    }else if(crop6=="winterlinseed"){
      if(crop5=="winterlinseed"||crop5=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop5=="winterbeans"||crop5=="springbeans"||crop5=="sugarbeet"||crop5=="driedpeas"||crop5=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Linseed sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("WLIN",tillage6,ncb6) 
      cy6 <- cb6[c3]
    }else if(crop6=="springlinseed"){
      if(crop5=="winterlinseed"||crop5=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop5=="winterbeans"||crop5=="springbeans"||crop5=="sugarbeet"||crop5=="driedpeas"||crop5=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Linseed sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("SLIN",tillage6,ncb6) 
      cy6 <- cb6[c3]
    }else if(crop6=="driedpeas"){
      if(crop5=="driedpeas"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Peas is penalised")
      }else{
        selfrot <- 0
      }
      if(crop5=="wosr"||crop5=="sosr"||crop5=="setaside"){ 
        rotpen <- 100
        warning("OSR and  Rotational Set-aside--Peas sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("DPEA",tillage6,ncb6)
      cy6 <- cb6[c3]
    }
    
    # Rotation lenth: Maximum 6 year rotation is assumed
    
    if(farm=="single"){
      
      d1 <- data.frame(Component,Unit,Year1,Year2,Year3,Year4,Year5,Year6)
      nrotl <- rotlength
      if(nrotl==2){
        d1[,seq(-5,-8)]
      }else if(nrotl==3){
        d1[,seq(-6,-8)]
      }else if(nrotl==4){
        d1[,seq(-7,-8)]
      }else if(nrotl==5){
        d1[,-8]
      }else if(nrotl==6){
        d1
      }
      
    }else if(farm=="multiple"){
      d2 <- c(cy1,ncb,cy2,ncb2,cy3,ncb3,cy4,ncb4,cy5,ncb5,cy6,ncb6)
      #d2
      if(nrotl==2){
        d3 <- d2[seq(-27,-78)]
        d4 <- c(d3, sum(d3[13],d3[26]))
      }else if(nrotl==3){
        d3 <- d2[seq(-40,-78)]
        d4 <- c(d3, sum(d3[13],d3[26],d3[39]))
      }else if(nrotl==4){
        d3 <- d2[seq(-53,-78)]
        d4 <- c(d3, sum(d3[13],d3[26],d3[39],d3[52]))
      }else if(nrotl==5){
        d3 <- d2[seq(-66,-78)]
        d4 <- c(d3, sum(d3[13],d3[26],d3[39],d3[52],d3[65]))
      }else if(nrotl==6){
        d3 <- d2
        d4 <- c(d3, sum(d3[13],d3[26],d3[39],d3[52],d3[65],d3[78]))
        
      }
    }
  }
  #************************
  if(farm=="single"){
    
    ecm <- ECOMOD(farm="single",default,soil,rotlength,rotprob,crops,tillages,seedrate,delsowing,Nfert,Pfert,Kfert,bgherbdose,
                  glyphosatedose,numberofsprays,subsidy,blackgrass,cropprice,cropyield,yieldoption,Nfertprice,
                  Pfertprice,Kfertprice,seedprice,herbprice,glyphosateprice,machsize,fuelprice,labourwage)
    
  }else if(farm=="multiple"&is.null(default)){
    fd <- farmdata
    
    est <- c("yield","output","fertcost","seedcost","herbcost","sundry","varcost","gmargin","fuelcost",
             "labcost","opcost","grossprof","rotgrossprof")
    
    cp1 <- "crop.1" #crops[1]
    cp2 <- "crop.2" #crops[2]
    cp3 <- "crop.3" #crops[3]
    cp4 <- "crop.4" #crops[4]
    cp5 <- "crop.5" #crops[5]
    cp6 <- "crop.6" #crops[6]
    
    v1 <- ".1"
    v2 <- ".2"
    v3 <- ".3"
    v4 <- ".4"
    v5 <- ".5"
    v6 <- ".6"    
    
    p1 <- paste(est,v1,sep="")
    p2 <- paste(est,v2,sep="")
    p3 <- paste(est,v3,sep="")
    p4 <- paste(est,v4,sep="")
    p5 <- paste(est,v5,sep="")
    p6 <- paste(est,v6,sep="")
    
    hd <- c(p1,p2,p3,p4,p5,p6)
    
    nrotl <- rotlength
    if(nrotl==2){
      hd1 <- hd[seq(-27,-78)]
    }else if(nrotl==3){
      hd1 <- hd[seq(-40,-78)]
    }else if(nrotl==4){
      hd1 <- hd[seq(-53,-78)]
    }else if(nrotl==5){
      hd1 <- hd[seq(-66,-78)]
    }else if(nrotl==6){
      hd1 <- hd
    }
    
    cp <- fd[,seq(5,10)]
    ti <- fd[,seq(11,16)]
    ds <- fd[,seq(23,28)]
    sb <- as.character(fd[,65])
    bg <- fd[,seq(66,71)]
    yo <- as.character(fd[,84])
    
    
    for(k in 1:ncol(cp)){
      if(class(cp[,k])=="factor"){cp[,k] <- as.character(cp[,k])}
    }
    
    for(k in 1:ncol(ti)){
      if(class(ti[,k])=="factor"){ti[,k] <- as.character(ti[,k])}
    }
    
    for(k in 1:ncol(ds)){
      if(class(ds[,k])=="factor"){ds[,k] <- as.character(ds[,k])}
    }
    
    
    for(k in 1:ncol(bg)){
      if(class(bg[,k])=="factor"){bg[,k] <- as.character(bg[,k])}
    }
    
    
    
    
    fms <- c()
    
    index <- c(seq(1, length(farmdata[,1])))
    
    for(i in index){
      fms <- c(fms,ECOMOD(farm="multiple",default,soil=fd[i,3],rotlength,rotprob,crops=c(cp[,1][i],cp[,2][i],cp[,3][i],cp[,4][i],cp[,5][i],cp[,6][i]),
                          tillage=c(ti[,1][i],ti[,2][i],ti[,3][i],ti[,4][i],ti[,5][i],ti[,6][i]),seedrate=as.numeric(fd[i,seq(17,22)]),
                          delsowing=c(ds[,1][i],ds[,2][i],ds[,3][i],ds[,4][i],ds[,5][i],ds[,6][i]),
                          Nfert=as.numeric(fd[i,seq(29,34)]),Pfert=as.numeric(fd[i,seq(35,40)]),Kfert=as.numeric(fd[i,seq(41,46)]),
                          bgherbdose=as.numeric(fd[i,seq(47,52)]), 
                          glyphosatedose=as.numeric(fd[i,seq(53,58)]),numberofsprays=as.numeric(fd[i,seq(59,64)]),subsidy=sb[i],
                          blackgrass=c(bg[,1][i],bg[,2][i],bg[,3][i],bg[,4][i],bg[,5][i],bg[,6][i]), 
                          cropprice=as.numeric(fd[i,seq(72,77)]),
                          cropyield=as.numeric(fd[i,seq(78,83)]),yieldoption=yo[i], #yieldoption, 
                          Nfertprice=as.numeric(fd[i,85]),Pfertprice=as.numeric(fd[i,86]),
                          Kfertprice=as.numeric(fd[i,87]),
                          seedprice=as.numeric(fd[i,seq(88,93)]),herbprice=as.numeric(fd[i,seq(94,99)]),glyphosateprice=as.numeric(fd[i,seq(100,105)]),
                          machsize=as.numeric(fd[i,seq(106,110)]),fuelprice=as.numeric(fd[i,111]),
                          labourwage=as.numeric(fd[i,112])))
      # subsidy==fd[i,70]
      # yieldoption==fd[i,89] or yo[i]
      #print(i)
      fms1 <- fms
      cols <- c(hd1,"totalrotgrossprof")
      ros <- as.character(index)
      
      mat <- suppressWarnings(matrix(fms1,ncol=length(cols),nrow=length(index),byrow=T,dimnames=list(ros,cols)))
      Field_no <- index
      field_name <- fd[,2]
      ecm <- data.frame(Field_no,field_name,fd[,seq(5,10)],mat)
      
      # NAME OF THE FILE CAN BE CHANGED TO WHICHEVER NAME DESIRED *********
      # e.g. "Farms_Fields_Economic_Outcomes.csv" as in Supplementary Information
      write.table(ecm,file=filename,row.names=FALSE,sep=",")
      
    }
  }else if(farm=="multiple"&default=="yes"){
    stop("RUNNING MODEL FOR MULTIPLE FIELDS CANNOT BE BASED ON DEFAULT VALUES: SET default to NULL")
  }
  
  ka <- ecm
} 


##    ECOMOD - upper limits, actual yield ----

solve_BGRI_ECOMOD_u_act <- function(filename,farmdata,default,farm="single",soil,rotlength,rotprob=NULL,crops,tillages,seedrate,delsowing,Nfert,Pfert,Kfert,bgherbdose,
                                    glyphosatedose,numberofsprays,subsidy="yes",blackgrass,cropprice,cropyield,yieldoption="actual",Nfertprice,
                                    Pfertprice,Kfertprice,seedprice,herbprice,glyphosateprice,machsize,fuelprice,labourwage){  
  
  # THE ORDER OF MODEL INPUTS SUCH AS TILLAGE, SEED RATE N FERTILISER RATES ETC MUST CORRESPOND TO THE ORDER OF CROPS
  # FOR EXAMPLE IF FIRST CROP IS WINTER WHEAT, THEN IN THE NFERT VECTOR, FIRST VALUE MUST CORRESPOND TO WINTER WHEAT N FERTILISER RATE.
  
  # Machine size (machsize) vector MUST be entered in the following order: 1. Tractor szie (kW); 2. Roller size (m);
  # 3. Power harrow size (m); 4. Sprayer tank size (litres); 5. Combine harvester size (kW)
  # For example machsize = c(102,6,4,1400,125)
  
  
  if(farm=="multiple"&is.null(farmdata)){
    stop("RUNNING MODEL FOR MULTIPLE FIELDS CANNOT BE BASED ON SINGLE FIELD/FARM DATA: USE MULTIPLE FIELD/FARM DATA & EQUATE farmdata TO THE DATA")
  }
  
  
  ECOMOD <- function(farm,default,soil,rotlength,rotprob,crops,tillages,seedrate,delsowing,Nfert,Pfert,Kfert,bgherbdose,
                     glyphosatedose,numberofsprays,subsidy,blackgrass,cropprice,cropyield,yieldoption,Nfertprice,
                     Pfertprice,Kfertprice,seedprice,herbprice,glyphosateprice,machsize,fuelprice,labourwage){
    
    # Setting the model to generate default results ******
    
    if(is.null(default)){
      soil <- soil
      rotlength <- rotlength
      crops <- crops
      tillages <- tillages
      seedrate <- seedrate
      delsowing <- delsowing
      Nfert <- Nfert
      Pfert <- Pfert
      Kfert <- Kfert
      bgherbdose <- bgherbdose
      glyphosatedose <- glyphosatedose
      numberofsprays <- numberofsprays
      blackgrass <- blackgrass
      cropprice <- cropprice
      cropyield <- cropyield
      Nfertprice <- Nfertprice
      Pfertprice <- Pfertprice
      Kfertprice <- Kfertprice
      seedprice <- seedprice
      herbprice <- herbprice 
      glyphosateprice <- glyphosateprice
      machsize <- machsize
      fuelprice <- fuelprice
      labourwage <- labourwage
      
    }else if(default=="yes"){ # DEFAULT MODEL INPUT DATA *******************
      soil <- 2.5
      crops <- c("winterwheat","winterwheat","wosr","winterwheat","springbeans","winterwheat")
      tillages <- c("lightcultivation","lightcultivation","deepcultivation","deepcultivation","deepcultivation","deepcultivation")
      seedrate <- c(185,175,3.5,185,334,185)
      delsowing <- c("late","no","late","no","late","late")
      Nfert <- c(180,188,167,190,0,174)
      Pfert <- c(95,95,80,95,70,95)
      Kfert <- c(115,115,70,115,70,115)
      bgherbdose <- c(3.16,7.24,2.54,10.97,10.17,1.7)
      glyphosatedose <- c(2,1.6,4,2.23,4.1,0.98) 
      numberofsprays <- c(3,3,4,4,6,4)
      blackgrass <- c("low","low","low","low","low","low") #**** Needs changing
      cropprice <- c(150,150,335,150,185,150)
      cropyield <- c(9.1,9,3.91,9.8,5.9,10)
      Nfertprice <- 0.78
      Pfertprice <- 0.71
      Kfertprice <- 0.44
      seedprice <- c(0.36,0.36,7.34,0.36,0.38,0.36)
      herbprice <- c(19.5,19.5,19.5,19.5,19.5,19.5) # An average from a farm data
      glyphosateprice <- c(2.43,2.43,2.43,2.43,2.43,2.43) # An average from a farm data
      machsize <- c(102,6,4,1400,125) # Assumed machine sizes
      fuelprice <- 0.625 # ABC Nov 2018, 87th ed
      labourwage <- 10.08 # Nix 2019, 49th ed
      subsidy <- "yes"
    }
    
    #************* Rotation Length Assumption *****************
    lcr <- 6 #rotlength = 6 # Length of vector based on max years of rotation which is 6
    if(length(crops)>lcr){
      warning("Maximum 6-year rotation is assumed (Length of crops vector MUST be 6)")
    }else{
      crops <- crops
    }
    
    if(length(tillages)>6){
      warning("Length of tillage vector MUST be 6")
    }else{
      tillages <- tillages
    }
    
    if(length(machsize)>5){
      warning("Length of machine sizes vector MUST be 5")
      # The machine size must be set in the following order: Tractor size (kW), roller size (m),
      # power harrow size (m), Sprayer size (litres), combine harvester size (kW)
    }else{
      tillages <- tillages
    }
    
    if(length(Nfert)>lcr||length(Pfert)>lcr||length(Kfert)>lcr||length(seedrate)>lcr||length(seedprice)>lcr){
      warning("Maximum 6-year rotation is assumed (Length of crops vector MUST be 6)")
    }else{
      Nfert <- Nfert; Pfert <- Pfert; Kfert <- Kfert; seedrate <- seedrate; seedprice <- seedprice
    }
    
    if(length(bgherbdose)>lcr||length(glyphosatedose)>lcr||length(numberofsprays)>lcr||
       length(cropprice)>lcr||length(cropyield)>lcr||length(seedprice)>lcr||length(herbprice)>lcr){
      warning("Maximum 6-year rotation is assumed (Length of crops vector MUST be 6)")
    }else{
      bgherbdose <- bgherbdose; glyphosatedose <- glyphosatedose; numberofsprays <- numberofsprays;
      cropprice <- cropprice; cropyield <- cropyield; seedprice <- seedprice; herbprice <- herbprice
    }
    
    # Setting soil type
    soi <- c(0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5) # Soil type indices
    if(soil%in%soi==TRUE){
      nsoil <- soil
    }else{
      nsoil <- 2.5 # This sets the soil type to heavy soil (Clay)
      warning("Enter correct soil type: must be between 0.5 and 2.5 at an interval of 0.25. Check that input soil values are formatted as 'numeric' and have 2 dec pl.")
    }
    
    cropBudget <- function(crop,asoil,subsidy,blackgrass,tillage,selfrot,rotpen,yieldoption){ #***********************************************************
      
      # This function estimates gross margin (£/ha) and gross profit (£/ha) for each crop taking into
      # consideration soil type, yield loss due to black-grass infestation,
      # farm operation and machine types, and fuel price.
      
      # Creation of Data files *******
      Input_Output <- c("N Fertiliser","P Fertiliser","K Fertiliser",
                        "N Fertiliser Price","P Fertiliser Price","K Fertiliser Price",
                        "Seed Rate","Seed Price","BG Herbicide Rate","BG Herbicide Price",
                        "Glyphosate Rate", "Glyphosate Price","Number of Sprays",
                        "Sugarbeet Transport","Fertiliser Cost","Seed Cost",
                        "Herbicide Cost","Sundry Cost","Sugarbeet Transport Cost",
                        "Primary Yield","Primary Yield Price",
                        "Secondary Yield","Secondary Yield Price",
                        "Subsidy","Farm Output","Variable Cost",
                        "Gross Margin","Fuel Cost","Labour Cost","Operating Cost",
                        "Gross Profit") # Crop inputs and output parameters
      lcr <- 6 # Equals the number of crops
      cl <- c(rep(0,length(Input_Output)*lcr))
      cd <- matrix(cl, ncol=lcr, byrow = T)
      cols <- c("Input_Output",crops)
      cd1 <- data.frame(Input_Output,cd)
      nfp <- c(rep(Nfertprice,lcr)); pfp <- c(rep(Pfertprice,lcr)); kfp <- c(rep(Kfertprice,lcr))
      
      inp <- rbind(Nfert,Pfert,Kfert,nfp,pfp,kfp,seedrate,seedprice,bgherbdose,herbprice,glyphosatedose,
                   glyphosateprice,numberofsprays) # Putting input data together to create crop data
      
      cd1[seq(1,13),seq(2,7)] <- inp
      cd1[20,seq(2,7)] <- cropyield
      cd1[21,seq(2,7)] <- cropprice
      colnames(cd1) <- cols
      crd <- fi1 <- cd1
      
      # Updated 07/02/2023. Assumed sundry costs are informed by Nix (2019) and ABC (2019).
      # Sundry costs include the average per hectare cost of herbicides NOT targeting BG, from the BGRI data, 
      # as well as cost of other chemicals such as fungicide, insecticides, growth regulators etc.,
      # plus marketing, agronomy costs, in-store chemicals. Data are from 'BGRI_ECOMOD_Chem & Sundry Costs_2023-02-07.xlsx".
      #snwwt <- 146; snswt <- 95; snwba <- 108; snsba <- 81; snwbe <- 101; snsbe <- 89; snwpo <- 1200; snwos <- 110; snsbt <- 409; snset <- 2;
      #snsos <- 67; sndpe <- 156; snwln <- 75; snsln <- 73; snwoa <- 80; snsoa <- 89; 
      snwwt <- 182; snswt <- 100; snwba <- 158; snsba <- 151; snwbe <- 91; snsbe <- 81; snwpo <- 2215; snwos <- 192; snsbt <- 488; snset <- 2;
      snsos <- 140; sndpe <- 155; snwln <- 88; snsln <- 53; snwoa <- 148; snsoa <- 109;   
      
      
      # Yield estimates based on soil type and N fertiliser amounts (Response functions obtained from SAFMOD)
      wwt <- round(((11.841-(9.211*(0.9907^(fi1$winterwheat[1])))-(0.0075*(fi1$winterwheat[1])))*(0.743+0.1714*(nsoil))*0.947),2)
      swt <- round((5.885-(2.893*(0.984^(fi1$springwheat[1]))))*(0.73+0.18*(nsoil)),2)
      wba <- round((((12.967-(10.029*(0.993^(fi1$winterbarley[1])))-(0.0147*(fi1$winterbarley[1])))*(0.76+0.16*(nsoil)))*0.89),2)
      sba <- round(((19.98-(18.164*(0.9952^(fi1$springbarley[1])))-(0.0364*(fi1$springbarley[1])))*(0.887+0.075*(nsoil))*1.02),2)
      wbe <- round((((0.95+1.3625*(nsoil))*1.1)*1.05),2)
      sbe <- round((((0.7+1.25*(nsoil))*1.05)*1.2),2)
      wpo <- round((44.507-(29.135*(0.992^fi1$warepotatoes[1])))*1.16,2)
      wos <- round((((3.35+(-0.623*(0.010^(fi1$wosr[1])))-0.000324*(fi1$wosr[1]))*(0.655+0.23*(nsoil)))*0.87),2)
      sbt <- round(((54.543-(0.05*37.82*(0.984^fi1$sugarbeet[1])))*1.30),2)
      #set <- 0
      sos <- round((2.317-(1.139*(0.984^(fi1$sosr[1])))),2)
      wln <- round(0.75+0.45*1.5*(nsoil),2)
      sln <- round(0.75+0.45*(nsoil)*0.95,2)
      dpe <- round((2.48+3.475*(nsoil)-(1.2875*(nsoil)^2)),2)
      woa <- round((((12.967-(10.029*(0.993^(fi1$winteroats[1])))-(0.0147*(fi1$winteroats[1])))*(0.76+0.16*(nsoil)))*0.89),2)
      soa <- round(((19.98-(18.164*(0.9952^(fi1$springoats[1])))-(0.0364*(fi1$springoats[1])))*(0.887+0.075*(nsoil))*1.02),2)
      
      # ========= Yield Penalty | Black-grass ========= 
      # ************ Winter wheat - black-grass infestation assumption ************
      # These yield penalties (abs/low BG density = 0, med=0, high=7.45, vhigh=25.6) are from Varah et al (2019) Nature Sustainability, based on BGRI data.
      # If better data are obtained, we suggest updating these penalties.
      
      # IF ACTUAL YIELD DATA ARE USED YIELD PENALTIES DUE TO BLACK-GRASS ARE NOT TAKEN INTO ACCOUNT
      
      bgf <- function(infestation){
        if(infestation=="low"){
          yieldloss <- 0; wwht1 <- wwt;
          yls <- 0
        }else if(infestation=="medium"){
          yieldloss <- 5.14; wwht1 <- round(wwt*(1-(5.14/100)),2)
          yls <- round((5.14/100)*wwht1,2)
        }else if(infestation=="high"){
          yieldloss <- 45; wwht1 <- round(wwt*(1-(45/100)),2)
          yls <- round((45/100)*wwht1,2)
        }else if(infestation=="veryhigh"){
          yieldloss <- 70; wwht1 <- round(wwt*(1-(70/100)),2)
          yls <- round((70/100)*wwht1,2)
        }else{
          yieldloss <- 0
          wwht1 <- wwt
          yls <- 0 
        }
        c(wwht1,yls) 
      }
      
      # ========= Yield Penalty | Sowing Date =========
      #************ Delayed Sowing ************
      # Delayed sowing has been modelled to take into account the degree of delay.
      # For example the sowing of winter wheat is optimal in late September and three classes or 
      # options for delayed sowing are defined for October (late), November (later) and December (latest)
      # IF ACTUAL YIELD DATA ARE USED YIELD PENALTIES DUE TO DELAYED SOWING ARE NOT TAKEN INTO ACCOUNT *********
      
      delsow <- function(sowtime){
        if(sowtime=="late"){
          ft1 <- 7/100; ft2 <- 4/100; ft3 <- 3/100; ft4 <- 2/100; ft5 <- 0/100
          ft6 <- 5/100; ft7 <- 10/100; ft8 <- 1/100; ft9 <- 1/100; ft10 <- 0/100; ft11 <- 0/100; ft12 <- 0/100; ft13 <- 0/100
          ds <- c(ft1,ft2,ft3,ft4,ft5,ft6,ft7,ft8,ft9,ft10,ft11,ft12,ft13)
        }else if(sowtime=="later"){
          ft1 <- 12/100; ft2 <- 9/100; ft3 <- 12/100; ft4 <- 5/100; ft5 <- 0/100
          ft6 <- 6/100; ft7 <- 11/100; ft8 <- 5/100; ft9 <- 5/100; ft10 <- 0/100; ft11 <- 0/100; ft12 <- 0/100; ft13 <- 3/100
          ds <- c(ft1,ft2,ft3,ft4,ft5,ft6,ft7,ft8,ft9,ft10,ft11,ft12,ft13)
        }else if(sowtime=="latest"){
          ft1 <- 15/100; ft2 <- 17/100; ft3 <- 18/100; ft4 <- 15/100; ft5 <- 0/100
          ft6 <- 6/100; ft7 <- 11/100; ft8 <- 5/100; ft9 <- 11/100; ft10 <- 0/100; ft11 <- 0/100; ft12 <- 0/100; ft13 <- 6/100
          ds <- c(ft1,ft2,ft3,ft4,ft5,ft6,ft7,ft8,ft9,ft10,ft11,ft12,ft13)
        }else if(sowtime=="no"){
          ft1 <- ft2 <- ft3 <- ft4 <- ft5 <- ft6 <- ft7 <- ft8 <- ft9 <- ft10 <- ft11 <- ft12 <- ft13 <- 0
          ds <- c(ft1,ft2,ft3,ft4,ft5,ft6,ft7,ft8,ft9,ft10,ft11,ft12,ft13)
        }else{
          ft1 <- ft2 <- ft3 <- ft4 <- ft5 <- ft6 <- ft7 <- ft8 <- ft9 <- ft10 <- ft11 <- ft12 <- ft13 <- 0
          ds <- c(ft1,ft2,ft3,ft4,ft5,ft6,ft7,ft8,ft9,ft10,ft11,ft12,ft13)
          warning("Check if delayed sowing option is spelt correctly; option MUST be: no, late, later or latest")
        }
        ds
      }
      
      seccost <- 65 # Secondary yield (straw) price assumed for cereal crops (wheat and barley crops) ********************
      c1 <- 18; c2 <- 19; c3 <- 20; c4 <- 21; c5 <- 22; c6 <- 23; c7 <- 24; c8 <- 25; c9 <- 26;# === row indices
      c10 <- 27; c11 <- 28; c12 <- 29; c13 <- 30; c14 <- 31; 
      
      h1 <- 10; h2 <- 11; h3 <- 12; h4 <- 13; h5 <- 14; h6 <- 15;
      h7 <- 16; h8 <- 17; 
      
      # ********** WW *********
      if(crops[1]=="winterwheat"){
        crd[c1,2] <- snwwt # Sundry cost
        crd[c6,2] <- seccost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2] 
          crd[c5,2] <- crd[c3,2]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- bgf(blackgrass[1])[1]-round(delsow(delsowing[1])[1]*bgf(blackgrass[1])[1],2) # wwt
          crd[c5,2] <- crd[c3,2]*0.5
        }
      }
      if(crops[2]=="winterwheat"){
        crd[c1,3] <- snwwt # Sundry cost
        crd[c6,3] <- seccost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- bgf(blackgrass[2])[1]-round(delsow(delsowing[2])[1]*bgf(blackgrass[2])[1],2) # wwt
          crd[c5,3] <- crd[c3,3]*0.5
        }
      }
      if(crops[3]=="winterwheat"){
        crd[c1,4] <- snwwt # Sundry cost
        crd[c6,4] <- seccost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4] 
          crd[c5,4] <- crd[c3,4]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- bgf(blackgrass[3])[1]-round(delsow(delsowing[3])[1]*bgf(blackgrass[3])[1],2) # wwt
          crd[c5,4] <- crd[c3,4]*0.5
        }
      }
      if(crops[4]=="winterwheat"){
        crd[c1,5] <- snwwt # Sundry cost
        crd[c6,5] <- seccost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- bgf(blackgrass[4])[1]-round(delsow(delsowing[4])[1]*bgf(blackgrass[4])[1],2) # wwt
          crd[c5,5] <- crd[c3,5]*0.5
        }
      } 
      if(crops[5]=="winterwheat"){
        crd[c1,6] <- snwwt # Sundry cost
        crd[c6,6] <- seccost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- bgf(blackgrass[5])[1]-round(delsow(delsowing[5])[1]*bgf(blackgrass[5])[1],2) # wwt
          crd[c5,6] <- crd[c3,6]*0.5
        }
      }
      if(crops[6]=="winterwheat"){
        crd[c1,7] <- snwwt # Sundry cost
        crd[c6,7] <- seccost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- bgf(blackgrass[6])[1]-round(delsow(delsowing[6])[1]*bgf(blackgrass[6])[1],2) # wwt
          crd[c5,7] <- crd[c3,7]*0.5
        }
      }
      
      #********** SPR WHT *********
      if(crops[1]=="springwheat"){
        crd[c1,2] <- snswt # Sundry cost
        crd[c6,2] <- seccost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- swt-round(delsow(delsowing[1])[2]*swt,2)
          crd[c5,2] <- crd[c3,2]*0.5
        }
      }
      if(crops[2]=="springwheat"){
        crd[c1,3] <- snswt # Sundry cost
        crd[c6,3] <- seccost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- swt-round(delsow(delsowing[2])[2]*swt,2)
          crd[c5,3] <- crd[c3,3]*0.5
        }
      }
      if(crops[3]=="springwheat"){
        crd[c1,4] <- snswt # Sundry cost
        crd[c6,4] <- seccost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- swt-round(delsow(delsowing[3])[2]*swt,2)
          crd[c5,4] <- crd[c3,4]*0.5
        }
      }
      if(crops[4]=="springwheat"){
        crd[c1,5] <- snswt # Sundry cost
        crd[c6,5] <- seccost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- swt-round(delsow(delsowing[4])[2]*swt,2)
          crd[c5,5] <- crd[c3,5]*0.5
        }
      } 
      if(crops[5]=="springwheat"){
        crd[c1,6] <- snswt # Sundry cost
        crd[c6,6] <- seccost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- swt-round(delsow(delsowing[5])[2]*swt,2)
          crd[c5,6] <- crd[c3,6]*0.5
        }
      }
      if(crops[6]=="springwheat"){
        crd[c1,7] <- snswt # Sundry cost
        crd[c6,7] <- seccost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- swt-round(delsow(delsowing[6])[2]*swt,2)
          crd[c5,7] <- crd[c3,7]*0.5
        }
      }
      
      #********** WBAR *********
      if(crops[1]=="winterbarley"||crops[1]=="winteroats"){
        crd[c1,2] <- snwba # Sundry cost
        crd[c6,2] <- seccost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- wba-round(delsow(delsowing[1])[3]*wba,2)
          crd[c5,2] <- crd[c3,2]*0.5
        }
      }
      if(crops[2]=="winterbarley"||crops[2]=="winteroats"){
        crd[c1,3] <- snwba # Sundry cost
        crd[c6,3] <- seccost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- wba-round(delsow(delsowing[2])[3]*wba,2)
          crd[c5,3] <- crd[c3,3]*0.5
        }
      }
      if(crops[3]=="winterbarley"||crops[3]=="winteroats"){
        crd[c1,4] <- snwba # Sundry cost
        crd[c6,4] <- seccost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- wba-round(delsow(delsowing[3])[3]*wba,2)
          crd[c5,4] <- crd[c3,4]*0.5
        }
      }
      if(crops[4]=="winterbarley"||crops[4]=="winteroats"){
        crd[c1,5] <- snwba # Sundry cost
        crd[c6,5] <- seccost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- wba-round(delsow(delsowing[4])[3]*wba,2)
          crd[c5,5] <- crd[c3,5]*0.5
        }
      } 
      if(crops[5]=="winterbarley"||crops[5]=="winteroats"){
        crd[c1,6] <- snwba # Sundry cost
        crd[c6,6] <- seccost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- wba-round(delsow(delsowing[5])[3]*wba,2)
          crd[c5,6] <- crd[c3,6]*0.5
        }
      }
      if(crops[6]=="winterbarley"||crops[6]=="winteroats"){
        crd[c1,7] <- snwba # Sundry cost
        crd[c6,7] <- seccost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- wba-round(delsow(delsowing[6])[3]*wba,2)
          crd[c5,7] <- crd[c3,7]*0.5
        }
      }
      #********** SPR BAR *********
      if(crops[1]=="springbarley"||crops[1]=="springoats"){
        crd[c1,2] <- snsba # Sundry cost
        crd[c6,2] <- seccost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- sba-round(delsow(delsowing[1])[4]*sba,2)
          crd[c5,2] <- crd[c3,2]*0.5
        }
      }
      if(crops[2]=="springbarley"||crops[2]=="springoats"){
        crd[c1,3] <- snsba # Sundry cost
        crd[c6,3] <- seccost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- sba-round(delsow(delsowing[2])[4]*sba,2)
          crd[c5,3] <- crd[c3,3]*0.5
        }
      }
      if(crops[3]=="springbarley"||crops[3]=="springoats"){
        crd[c1,4] <- snsba # Sundry cost
        crd[c6,4] <- seccost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- sba-round(delsow(delsowing[3])[4]*sba,2)
          crd[c5,4] <- crd[c3,4]*0.5
        }
      }
      if(crops[4]=="springbarley"||crops[4]=="springoats"){
        crd[c1,5] <- snsba # Sundry cost
        crd[c6,5] <- seccost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- sba-round(delsow(delsowing[4])[4]*sba,2)
          crd[c5,5] <- crd[c3,5]*0.5
        }
      } 
      if(crops[5]=="springbarley"||crops[5]=="springoats"){
        crd[c1,6] <- snsba # Sundry cost
        crd[c6,6] <- seccost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- sba-round(delsow(delsowing[5])[4]*sba,2)
          crd[c5,6] <- crd[c3,6]*0.5
        }
      }
      if(crops[6]=="springbarley"||crops[6]=="springoats"){
        crd[c1,7] <- snsba # Sundry cost
        crd[c6,7] <- seccost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0.5
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- sba-round(delsow(delsowing[6])[4]*sba,2)
          crd[c5,7] <- crd[c3,7]*0.5
        }
      }
      
      #********** WBEA *********
      if(crops[1]=="winterbeans"){
        crd[c1,2] <- snwbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- wbe-round(delsow(delsowing[1])[5]*wbe,2)
          crd[c5,2] <- crd[c3,2]*0
        }
      }
      if(crops[2]=="winterbeans"){
        crd[c1,3] <- snwbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- wbe-round(delsow(delsowing[2])[5]*wbe,2)
          crd[c5,3] <- crd[c3,3]*0
        }
      }
      if(crops[3]=="winterbeans"){
        crd[c1,4] <- snwbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- wbe-round(delsow(delsowing[3])[5]*wbe,2)
          crd[c5,4] <- crd[c3,4]*0
        }
      }
      if(crops[4]=="winterbeans"){
        crd[c1,5] <- snwbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- wbe-round(delsow(delsowing[4])[5]*wbe,2)
          crd[c5,5] <- crd[c3,5]*0
        }
      } 
      if(crops[5]=="winterbeans"){
        crd[c1,6] <- snwbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- wbe-round(delsow(delsowing[5])[5]*wbe,2)
          crd[c5,6] <- crd[c3,6]*0
        }
      }
      if(crops[6]=="winterbeans"){
        crd[c1,7] <- snwbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- wbe-round(delsow(delsowing[6])[5]*wbe,2)
          crd[c5,7] <- crd[c3,7]*0
        }
      }
      
      #********** SPR BEA *********
      if(crops[1]=="springbeans"){
        crd[c1,2] <- snsbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- sbe-round(delsow(delsowing[1])[6]*sbe,2)
          crd[c5,2] <- crd[c3,2]*0
        }
      }
      if(crops[2]=="springbeans"){
        crd[c1,3] <- snsbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- sbe-round(delsow(delsowing[2])[6]*sbe,2)
          crd[c5,3] <- crd[c3,3]*0
        }
      }
      if(crops[3]=="springbeans"){
        crd[c1,4] <- snsbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- sbe-round(delsow(delsowing[3])[6]*sbe,2)
          crd[c5,4] <- crd[c3,4]*0
        }
      }
      if(crops[4]=="springbeans"){
        crd[c1,5] <- snsbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- sbe-round(delsow(delsowing[4])[6]*sbe,2)
          crd[c5,5] <- crd[c3,5]*0
        }
      } 
      if(crops[5]=="springbeans"){
        crd[c1,6] <- snsbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- sbe-round(delsow(delsowing[5])[6]*sbe,2)
          crd[c5,6] <- crd[c3,6]*0
        }
      }
      if(crops[6]=="springbeans"){
        crd[c1,7] <- snsbe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- sbe-round(delsow(delsowing[6])[6]*sbe,2)
          crd[c5,7] <- crd[c3,7]*0
        }
      }
      
      #********** WPOTS *********
      if(crops[1]=="warepotatoes"){
        crd[c1,2] <- snwpo # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- wpo-round(delsow(delsowing[1])[7]*wpo,2)
          crd[c5,2] <- crd[c3,2]*0
        }
      }
      if(crops[2]=="warepotatoes"){
        crd[c1,3] <- snwpo # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- wpo-round(delsow(delsowing[2])[7]*wpo,2)
          crd[c5,3] <- crd[c3,3]*0
        }
      }
      if(crops[3]=="warepotatoes"){
        crd[c1,4] <- snwpo # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- wpo-round(delsow(delsowing[3])[7]*wpo,2)
          crd[c5,4] <- crd[c3,4]*0
        }
      }
      if(crops[4]=="warepotatoes"){
        crd[c1,5] <- snwpo # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- wpo-round(delsow(delsowing[4])[7]*wpo,2)
          crd[c5,5] <- crd[c3,5]*0
        }
      } 
      if(crops[5]=="warepotatoes"){
        crd[c1,6] <- snwpo # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- wpo-round(delsow(delsowing[5])[7]*wpo,2)
          crd[c5,6] <- crd[c3,6]*0
        }
      }
      if(crops[6]=="warepotatoes"){
        crd[c1,7] <- snwpo # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- wpo-round(delsow(delsowing[6])[7]*wpo,2)
          crd[c5,7] <- crd[c3,7]*0
        }
      }
      
      #********** WOSR *********
      if(crops[1]=="wosr"){
        crd[c1,2] <- snwos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- wos-round(delsow(delsowing[1])[8]*wos,2)
          crd[c5,2] <- crd[c3,2]*0
        }
      }
      if(crops[2]=="wosr"){
        crd[c1,3] <- snwos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- wos-round(delsow(delsowing[2])[8]*wos,2)
          crd[c5,3] <- crd[c3,3]*0
        }
      }
      if(crops[3]=="wosr"){
        crd[c1,4] <- snwos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- wos-round(delsow(delsowing[3])[8]*wos,2)
          crd[c5,4] <- crd[c3,4]*0
        }
      }
      if(crops[4]=="wosr"){
        crd[c1,5] <- snwos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- wos-round(delsow(delsowing[4])[8]*wos,2)
          crd[c5,5] <- crd[c3,5]*0
        }
      } 
      if(crops[5]=="wosr"){
        crd[c1,6] <- snwos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- wos-round(delsow(delsowing[5])[8]*wos,2)
          crd[c5,6] <- crd[c3,6]*0
        }
      }
      if(crops[6]=="wosr"){
        crd[c1,7] <- snwos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- wos-round(delsow(delsowing[6])[8]*wos,2)
          crd[c5,7] <- crd[c3,7]*0
        }
      }
      
      #********** SBEET *********
      if(crops[1]=="sugarbeet"){
        crd[h5,2] <- 5 # Cost of transporting sugarbeet @ £5/t
        crd[c1,2] <- snsbt # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- sbt-round(delsow(delsowing[1])[9]*sbt,2)
          crd[c5,2] <- crd[c3,2]*0
        }
      }
      if(crops[2]=="sugarbeet"){
        crd[h5,3] <- 5 # Cost of transporting sugarbeet @ £5/t
        crd[c1,3] <- snsbt # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- sbt-round(delsow(delsowing[2])[9]*sbt,2)
          crd[c5,3] <- crd[c3,3]*0
        }
      }
      if(crops[3]=="sugarbeet"){
        crd[h5,4] <- 5 # Cost of transporting sugarbeet @ £5/t
        crd[c1,4] <- snsbt # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- sbt-round(delsow(delsowing[3])[9]*sbt,2)
          crd[c5,4] <- crd[c3,4]*0
        }
      }
      if(crops[4]=="sugarbeet"){
        crd[h5,5] <- 5 # Cost of transporting sugarbeet @ £5/t
        crd[c1,5] <- snsbt # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- sbt-round(delsow(delsowing[4])[9]*sbt,2)
          crd[c5,5] <- crd[c3,5]*0
        }
      } 
      if(crops[5]=="sugarbeet"){
        crd[h5,6] <- 5 # Cost of transporting sugarbeet @ £5/t
        crd[c1,6] <- snsbt # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- sbt-round(delsow(delsowing[5])[9]*sbt,2)
          crd[c5,6] <- crd[c3,6]*0
        }
      }
      if(crops[6]=="sugarbeet"){
        crd[h5,7] <- 5 # Cost of transporting sugarbeet @ £5/t
        crd[c1,7] <- snsbt # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- sbt-round(delsow(delsowing[6])[9]*sbt,2)
          crd[c5,7] <- crd[c3,7]*0
        }
      }
      
      #********** SETA *********
      if(crops[1]=="setaside"){
        crd[c1,2] <- snset # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- 0
          crd[c5,2] <- 0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- 0
          crd[c5,2] <- 0
        }
      }
      if(crops[2]=="setaside"){
        crd[c1,3] <- snset # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- 0
          crd[c5,3] <- 0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- 0
          crd[c5,3] <- 0
        }
      }
      if(crops[3]=="setaside"){
        crd[c1,4] <- snset # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- 0
          crd[c5,4] <- 0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- 0
          crd[c5,4] <- 0
        }
      }
      if(crops[4]=="setaside"){
        crd[c1,5] <- snset # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- 0
          crd[c5,5] <- 0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- 0
          crd[c5,5] <- 0
        }
      } 
      if(crops[5]=="setaside"){
        crd[c1,6] <- snset # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- 0
          crd[c5,6] <- 0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- 0
          crd[c5,6] <- 0
        }
      }
      if(crops[6]=="setaside"){
        crd[c1,7] <- snset # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- 0
          crd[c5,7] <- 0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- 0
          crd[c5,7] <- 0
        }
      }
      
      #********** SOSR *********
      if(crops[1]=="sosr"){
        crd[c1,2] <- snsos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- sos-round(delsow(delsowing[1])[10]*sos,2)
          crd[c5,2] <- crd[c3,2]*0
        }
      }
      if(crops[2]=="sosr"){
        crd[c1,3] <- snsos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- sos-round(delsow(delsowing[2])[10]*sos,2)
          crd[c5,3] <- crd[c3,3]*0
        }
      }
      if(crops[3]=="sosr"){
        crd[c1,4] <- snsos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- sos-round(delsow(delsowing[3])[10]*sos,2)
          crd[c5,4] <- crd[c3,4]*0
        }
      }
      if(crops[4]=="sosr"){
        crd[c1,5] <- snsos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- sos-round(delsow(delsowing[4])[10]*sos,2)
          crd[c5,5] <- crd[c3,5]*0
        }
      } 
      if(crops[5]=="sosr"){
        crd[c1,6] <- snsos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- sos-round(delsow(delsowing[5])[10]*sos,2)
          crd[c5,6] <- crd[c3,6]*0
        }
      }
      if(crops[6]=="sosr"){
        crd[c1,7] <- snsos # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- sos-round(delsow(delsowing[6])[10]*sos,2)
          crd[c5,7] <- crd[c3,7]*0
        }
      }
      
      #********** WLIN *********
      if(crops[1]=="winterlinseed"){
        crd[c1,2] <- snwln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- wln-round(delsow(delsowing[1])[11]*wln,2)
          crd[c5,2] <- crd[c3,2]*0
        }
      }
      if(crops[2]=="winterlinseed"){
        crd[c1,3] <- snwln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- wln-round(delsow(delsowing[2])[11]*wln,2)
          crd[c5,3] <- crd[c3,3]*0
        }
      }
      if(crops[3]=="winterlinseed"){
        crd[c1,4] <- snwln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- wln-round(delsow(delsowing[3])[11]*wln,2)
          crd[c5,4] <- crd[c3,4]*0
        }
      }
      if(crops[4]=="winterlinseed"){
        crd[c1,5] <- snwln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- wln-round(delsow(delsowing[4])[11]*wln,2)
          crd[c5,5] <- crd[c3,5]*0
        }
      } 
      if(crops[5]=="winterlinseed"){
        crd[c1,6] <- snwln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- wln-round(delsow(delsowing[5])[11]*wln,2)
          crd[c5,6] <- crd[c3,6]*0
        }
      }
      if(crops[6]=="winterlinseed"){
        crd[c1,7] <- snwln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- wln-round(delsow(delsowing[6])[11]*wln,2)
          crd[c5,7] <- crd[c3,7]*0
        }
      }
      
      #********** SLIN *********
      if(crops[1]=="springlinseed"){
        crd[c1,2] <- snsln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- sln-round(delsow(delsowing[1])[12]*sln,2)
          crd[c5,2] <- crd[c3,2]*0
        }
      }
      if(crops[2]=="springlinseed"){
        crd[c1,3] <- snsln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- sln-round(delsow(delsowing[2])[12]*sln,2)
          crd[c5,3] <- crd[c3,3]*0
        }
      }
      if(crops[3]=="springlinseed"){
        crd[c1,4] <- snsln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- sln-round(delsow(delsowing[3])[12]*sln,2)
          crd[c5,4] <- crd[c3,4]*0
        }
      }
      if(crops[4]=="springlinseed"){
        crd[c1,5] <- snsln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- sln-round(delsow(delsowing[4])[12]*sln,2)
          crd[c5,5] <- crd[c3,5]*0
        }
      } 
      if(crops[5]=="springlinseed"){
        crd[c1,6] <- snsln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- sln-round(delsow(delsowing[5])[12]*sln,2)
          crd[c5,6] <- crd[c3,6]*0
        }
      }
      if(crops[6]=="springlinseed"){
        crd[c1,7] <- snsln # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- sln-round(delsow(delsowing[6])[12]*sln,2)
          crd[c5,7] <- crd[c3,7]*0
        }
      }
      
      #********** PEAS *********
      if(crops[1]=="driedpeas"){
        crd[c1,2] <- sndpe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,2] <- crd[c3,2]
          crd[c5,2] <- crd[c3,2]*0
        }else if(yieldoption=="estimate"){
          crd[c3,2] <- dpe-round(delsow(delsowing[1])[13]*dpe,2)
          crd[c5,2] <- crd[c3,2]*0
        }
      }
      if(crops[2]=="driedpeas"){
        crd[c1,3] <- sndpe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,3] <- crd[c3,3]
          crd[c5,3] <- crd[c3,3]*0
        }else if(yieldoption=="estimate"){
          crd[c3,3] <- dpe-round(delsow(delsowing[2])[13]*dpe,2)
          crd[c5,3] <- crd[c3,3]*0
        }
      }
      if(crops[3]=="driedpeas"){
        crd[c1,4] <- sndpe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,4] <- crd[c3,4]
          crd[c5,4] <- crd[c3,4]*0
        }else if(yieldoption=="estimate"){
          crd[c3,4] <- dpe-round(delsow(delsowing[3])[13]*dpe,2)
          crd[c5,4] <- crd[c3,4]*0
        }
      }
      if(crops[4]=="driedpeas"){
        crd[c1,5] <- sndpe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,5] <- crd[c3,5]
          crd[c5,5] <- crd[c3,5]*0
        }else if(yieldoption=="estimate"){
          crd[c3,5] <- dpe-round(delsow(delsowing[4])[13]*dpe,2)
          crd[c5,5] <- crd[c3,5]*0
        }
      } 
      if(crops[5]=="driedpeas"){
        crd[c1,6] <- sndpe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,6] <- crd[c3,6]
          crd[c5,6] <- crd[c3,6]*0
        }else if(yieldoption=="estimate"){
          crd[c3,6] <- dpe-round(delsow(delsowing[5])[13]*dpe,2)
          crd[c5,6] <- crd[c3,6]*0
        }
      }
      if(crops[6]=="driedpeas"){
        crd[c1,7] <- sndpe # Sundry cost
        if(yieldoption=="actual"){
          crd[c3,7] <- crd[c3,7]
          crd[c5,7] <- crd[c3,7]*0
        }else if(yieldoption=="estimate"){
          crd[c3,7] <- dpe-round(delsow(delsowing[6])[13]*dpe,2)
          crd[c5,7] <- crd[c3,7]*0
        }
      }
      
      #**************************************************************************************************************
      # ========= Subsidy amount =========
      crd[c7,seq(2,7)] <- 223 # SUBSIDY AMOUNT ASSUMED @ 223/ha (https://www.gov.uk/guidance/bps-2019)
      cropData <- crd
      
      Operation=c("Spread P/K Fertiliser WWHT","Plough WWHT","Sowing WWHT","Roll WWHT",
                  "Apply N Fertiliser WWHT","Spray WWHT","Combine Harvesting WWHT","Bale WWHT",
                  
                  "Spread P/K Fertiliser SWHT","Plough SWHT","Sowing SWHT","Roll SWHT",
                  "Apply N Fertiliser SWHT","Spray SWHT","Combine Harvesting SWHT","Bale SWHT",
                  
                  "Spread P/K Fertiliser WBAR","Plough WBAR","Sowing WBAR","Roll WBAR",
                  "Apply N Fertiliser WBAR","Spray WBAR","Combine Harvesting WBAR","Bale WBAR",
                  
                  "Spread P/K Fertiliser SBAR","Plough SBAR","Sowing SBAR","Roll SBAR",
                  "Spray SBAR","Apply N Fertiliser SBAR","Combine Harvesting SBAR","Bale SBAR",
                  
                  "Spread P/K Fertiliser WBEA","Plough WBEA","Sowing WBEA","Roll WBEA","Spray WBEA",
                  "Combine Harvesting WBEA",
                  
                  "Spread P/K Fertiliser SBEA","Plough SBEA","Sowing SBEA","Spray SBEA",
                  "Combine Harvesting SBEA",
                  
                  "Plough WPOT","Harrow WPOT","Sowing WPOT","Ridge WPOT","Spray WPOT","Spread P/K Fertiliser WPOT",
                  "Apply N Fertiliser WPOT","Hoeing WPOT","Harvest WPOT",
                  
                  "Spread P/K Fertiliser WOSR","Plough WOSR","Sowing WOSR",
                  "Spray WOSR","Apply N Fertiliser WOSR","Combine Harvesting WOSR",
                  
                  "Plough SBEE","Harrow SBEE","Sowing SBEE","Spray SBEE","Spread P/K Fertiliser SBEE",
                  "Apply N Fertiliser SBEE","Hoeing SBEE","Harvest SBEE",
                  
                  "Plough SETA","Spray SETA",
                  
                  "Spread P/K Fertiliser SOSR","Plough SOSR","Sowing SOSR",
                  "Spray SOSR","Apply N Fertiliser SOSR","Combine Harvesting SOSR",
                  
                  "Spread P/K Fertiliser WLIN","Plough WLIN","Sowing WLIN",
                  "Spray WLIN","Apply N Fertiliser WLIN","Combine Harvesting WLIN",
                  
                  "Spread P/K Fertiliser SLIN","Plough SLIN","Sowing SLIN",
                  "Spray SLIN","Apply N Fertiliser SLIN","Combine Harvesting SLIN",
                  
                  "Spread P/K Fertiliser DPEA","Plough DPEA","Sowing DPEA","Roll DPEA",
                  "Spray DPEA","Combine Harvesting DPEA")
      
      lgt <- length(Operation) # Total number of operation for all crops              
      Workrates <- data.frame(Operations=Operation,   
                              
                              Tractor=c(rep(0,lgt)), Labour=c(rep(0,lgt)), 
                              Power_harrow=c(rep(0,lgt)), Sprayer=c(rep(0,lgt)),
                              Combine_harvester=c(rep(0,lgt)), Baler=c(rep(0,lgt)), 
                              SBEE_harvester=c(rep(0,lgt)),WPOT_harvester=c(rep(0,lgt)),Fuel_cost=c(rep(0,lgt)), 
                              Labour_cost=c(rep(0,lgt)), Operation_cost=c(rep(0,lgt))
      )
      
      Machines <- data.frame(
        Machine=c("Tractor","Rolls","Power harrow","Sprayer","Combine harvetser",
                  "Baler","Sugarbeet harvester"),
        
        Size_Unit=c("kW","m","m","litres","kW","na","na"),
        
        Size=c(machsize,0,0),
        
        Machine_price=c(rep(0,7)),
        
        Depreciation_rate=c(22,14,14,18,18,11,18),
        
        Repair_cost_rate=c(12,5,5,6.8,5.8,5.8,5), Replacement_year=c(5,10,8,7,7,7,7),
        
        Annual_hours=c(2500,rep(300,6))
      )
      
      modData <- list(cropData, Workrates, Machines) # Data files
      
      #************************************************************************************************
      #************************************************************************************************
      
      
      fi1 <- modData[[1]] # Crop Data
      
      # Yield penalties with respect to continuous cropping (self rotation) and sub-optimal rotation
      sf <- selfrot/100 # Self rotation penalty
      rt <- rotpen/100 # Rotational penalty
      
      #***********
      cy1 <- fi1[c3,2]; cy2 <- fi1[c3,3]; cy3 <- fi1[c3,4]; cy4 <- fi1[c3,5]; cy5 <- fi1[c3,6]; cy6 <- fi1[c3,7]
      if(crops[1]=="winterwheat"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0.5
      }else if(crops[1]=="springwheat"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0.5
      }else if(crops[1]=="winterbarley"||crops[1]=="winteroats"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0.5
      }else if(crops[1]=="springbarley"||crops[1]=="springoats"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0.5
      }else if(crops[1]=="winterbeans"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0
      }else if(crops[1]=="springbeans"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0
      }else if(crops[1]=="warepotatoes"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0
      }else if(crops[1]=="wosr"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0
      }else if(crops[1]=="sugarbeet"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0
      }else if(crops[1]=="setaside"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0
      }else if(crops[1]=="sosr"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0
      }else if(crops[1]=="winterlinseed"||crops[1]=="springlinseed"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0
      }else if(crops[1]=="driedpeas"||crops[1]=="peas"){
        fi1[c3,2] <- fi1[c3,2]; fi1[c5,2] <- fi1[c3,2]*0
      }
      
      #***********
      if(crops[2]=="winterwheat"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0.5
      }else if(crops[2]=="springwheat"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0.5
      }else if(crops[2]=="winterbarley"||crops[2]=="winteroats"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0.5
      }else if(crops[2]=="springbarley"||crops[2]=="springoats"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0.5
      }else if(crops[2]=="winterbeans"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0
      }else if(crops[2]=="springbeans"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0
      }else if(crops[2]=="warepotatoes"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0
      }else if(crops[2]=="wosr"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0
      }else if(crops[2]=="sugarbeet"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0
      }else if(crops[2]=="setaside"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0
      }else if(crops[2]=="sosr"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0
      }else if(crops[2]=="winterlinseed"||crops[2]=="springlinseed"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0
      }else if(crops[2]=="driedpeas"||crops[2]=="peas"){
        fi1[c3,3] <- cy2-(cy2*sf)-(cy2*rt); fi1[c5,3] <- fi1[c3,3]*0
      }
      
      #***********
      if(crops[3]=="winterwheat"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0.5
      }else if(crops[3]=="springwheat"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0.5
      }else if(crops[3]=="winterbarley"||crops[3]=="winteroats"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0.5
      }else if(crops[3]=="springbarley"||crops[3]=="springoats"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0.5
      }else if(crops[3]=="winterbeans"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0
      }else if(crops[3]=="springbeans"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0
      }else if(crops[3]=="warepotatoes"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0
      }else if(crops[3]=="wosr"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0
      }else if(crops[3]=="sugarbeet"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0
      }else if(crops[3]=="setaside"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0
      }else if(crops[3]=="sosr"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0
      }else if(crops[3]=="winterlinseed"||crops[3]=="springlinseed"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0
      }else if(crops[3]=="driedpeas"||crops[3]=="peas"){
        fi1[c3,4] <- cy3-(cy3*sf)-(cy3*rt); fi1[c5,4] <- fi1[c3,4]*0
      }
      
      #***********
      if(crops[4]=="winterwheat"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0.5
      }else if(crops[4]=="springwheat"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0.5
      }else if(crops[4]=="winterbarley"||crops[4]=="winteroats"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0.5
      }else if(crops[4]=="springbarley"||crops[4]=="springoats"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0.5
      }else if(crops[4]=="winterbeans"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0
      }else if(crops[4]=="springbeans"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0
      }else if(crops[4]=="warepotatoes"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0
      }else if(crops[4]=="wosr"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0
      }else if(crops[4]=="sugarbeet"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0
      }else if(crops[4]=="setaside"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0
      }else if(crops[4]=="sosr"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0
      }else if(crops[4]=="winterlinseed"||crops[4]=="springlinseed"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0
      }else if(crops[4]=="driedpeas"||crops[4]=="peas"){
        fi1[c3,5] <- cy4-(cy4*sf)-(cy4*rt); fi1[c5,5] <- fi1[c3,5]*0
      }
      
      #***********
      if(crops[5]=="winterwheat"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0.5
      }else if(crops[5]=="springwheat"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0.5
      }else if(crops[5]=="winterbarley"||crops[5]=="winteroats"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0.5
      }else if(crops[5]=="springbarley"||crops[5]=="springoats"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0.5
      }else if(crops[5]=="winterbeans"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0
      }else if(crops[5]=="springbeans"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0
      }else if(crops[5]=="warepotatoes"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0
      }else if(crops[5]=="wosr"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0
      }else if(crops[5]=="sugarbeet"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0
      }else if(crops[5]=="setaside"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0
      }else if(crops[5]=="sosr"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0
      }else if(crops[5]=="winterlinseed"||crops[5]=="springlinseed"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0
      }else if(crops[5]=="driedpeas"||crops[5]=="peas"){
        fi1[c3,6] <- cy5-(cy5*sf)-(cy5*rt); fi1[c5,6] <- fi1[c3,6]*0
      }
      
      #***********
      if(crops[6]=="winterwheat"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0.5
      }else if(crops[6]=="springwheat"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0.5
      }else if(crops[6]=="winterbarley"||crops[6]=="winteroats"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0.5
      }else if(crops[6]=="springbarley"||crops[6]=="springoats"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0.5
      }else if(crops[6]=="winterbeans"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0
      }else if(crops[6]=="springbeans"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0
      }else if(crops[6]=="warepotatoes"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0
      }else if(crops[6]=="wosr"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0
      }else if(crops[6]=="sugarbeet"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0
      }else if(crops[6]=="setaside"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0
      }else if(crops[6]=="sosr"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0
      }else if(crops[6]=="winterlinseed"||crops[6]=="springlinseed"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0
      }else if(crops[6]=="driedpeas"||crops[6]=="peas"){
        fi1[c3,7] <- cy6-(cy6*sf)-(cy6*rt); fi1[c5,7] <- fi1[c3,7]*0
      }
      
      
      op <- fi1
      
      #*************** Farm Costs/Revenue ****************************
      # Farm output estimates
      le <- 7
      if(subsidy=="no"){
        farmpayment <- 0
      }else{
        farmpayment <- op[c7,seq(2,le)]
      }
      
      # Farm output estimates
      # Farm output = (Crop yield * Crop price) + farmpayment
      outp <- ((op[c3,seq(2,le)]*op[c4,seq(2,le)])+(op[c5,seq(2,le)]*op[c6,seq(2,le)]))+farmpayment 
      op[c8,seq(2,le)] <- outp 
      
      kk1 <- op
      
      #*************** Farm Costs ****************************
      vc <- kk1 # Updated data with farmOutput(soil, subsidy, blackgrass)
      
      #h1 <- 10; h2 <- 11; h3 <- 12; h4 <- 13; h5 <- 14; h6 <- 15;
      #h7 <- 16; h8 <- 17; 
      
      # Fertiliser (NPK) cost = (Fertiliser rate (kg/ha)* Fertiliser price(£/kg))
      vc[h6,seq(2,le)] <- fertcost <- (vc[1,seq(2,le)]*vc[4,seq(2,le)])+
        (vc[2,seq(2,le)]*vc[5,seq(2,le)])+(vc[3,seq(2,le)]*vc[6,seq(2,le)])
      
      # Seed cost (Seed rate (kg/ha) * Seed price (£/kg))
      vc[h7,seq(2,le)] <- seedcost <- vc[7,seq(2,le)]*vc[8,seq(2,le)]
      
      # Herbicide cost is assumed to be cost of herbicides targeting black-grass 
      # (cost of other herbicides and other chemical costs are incorporated in Sundry cost)
      # Herbicide cost = Total Herbicide rate (kg or l/ha) * Herbicide price (£/l)
      vc[h8,seq(2,le)] <- chemcost <- (vc[9,seq(2,le)]*vc[h1,seq(2,le)])+(vc[h2,seq(2,le)]*vc[h3,seq(2,le)])
      
      # Sundry Cost (£/ha) Informed by data from Nix (2019) Farm Management Pocketbook
      sundry <- vc[c1,seq(2,le)] 
      
      # Sugar beet transport cost from Nix (2019) based on £5.11/t
      vc[c2,seq(2,le)] <- sbeettransport <- vc[h5,seq(2,le)]*vc[c3,seq(2,le)] 
      
      # Variable cost (£/ha)
      vc[c9,seq(2,le)] <- (fertcost+seedcost+chemcost+sundry+sbeettransport)
      
      # Gross margin (£/ha)
      # Gross margin = Output - Variable cost
      vc[c10,seq(2,le)] <- vc[c8,seq(2,le)]-vc[c9,seq(2,le)]
      kk2 <- vc
      
      # operatingCost <- function(nsoil, subsidy, blackgrass){
      
      # Estimates workrate of farm operations and fuel and labour cost
      # wro <- variableCost(nsoil, subsidy, blackgrass)
      
      #========= Machine Sizes ===========
      cr <- kk2 #variableCost(nsoil, subsidy, blackgrass) #wro #Files()[[2]]
      ma <- modData[[3]][,seq(-1,-2)] # Machine data
      wr <- modData[[2]] # Workrate data 
      tractor <- ma[1,1] # Tractor Size or power (kW) 
      sprayer <- ma[4,1] # Sprayer Size (size of tank in litres) 1400 litres
      tsize <- ma[4,1] # Tank size for estimate fertiliser application workrate
      combsize <- ma[5,1] # Size of combine harvester
      # The size of the combine harvester measured in tonnes/hour 
      # was derived on pro rata basis based on information from
      # Agricultural Notebook. A combine harvester with a power of 90kW can harvest 10t/h 
      # Thus a combine harvester of 125kW can harvest (10/90)*125
      extfactor <- round(10/90,2)
      combine <- round(combsize*extfactor) #14 # Represents combine harvester size
      rollwidth <- ma[2,1] # Roll width
      sphoe <- 19 # Assumed speed for hoeing (19km/h)
      rowsp <- 0.6 # Assumed row space (0.6m)
      tpspeed <- 4 # Speed for rolling (km/hr)
      
      # ========= Tillage Types ==========
      # Tillage Assumptions
      # To be able to take into consideration soil type in tillage work rate,
      # the ploughing work rate is taken as a reference value due to the availability of suitable formulae.
      # The workability of ploughing is taken as 1.
      
      if(tillage=="ploughing"){
        wkb <- 1 # ploughing workability used as a reference value
      }else if(tillage=="noninversion"){
        wkb <- 1*0.5 # 50% reduction in plough rate is assumed - i.e. twice as fast as ploughing
      }else if(tillage=="subsoiling"){
        wkb <- 1/0.75 # 75% slower than ploughing (previously this was 1*0.65, as a 35% reduction in plough work rate was assumed. After talking to contractors, this was amended: they said subsoiling is slower than ploughing, and you'd do 75% of what you'd get done if ploughing in the same time).
      }else if(tillage=="lightcultivation"){
        wkb <- 1*0.6 # 40% reduction in plough work rate is assumed
      }else if(tillage=="directdrilling"){
        wkb <- 0 # no cultivation
      }else if (tillage=="minimumtillage"){
        wkb <- 1*0.3 # 70% reduction in plough work rate is assumed
      }else if(tillage=="deepcultivation"){
        wkb <- (1*0.6)*1.4 # Assumed to be 40% more than light cultivation
      }else if(tillage[1]=="inversion"){
        wkb <- 1 
      }else{
        wkb <- 1 
      }
      
      nspr <- numberofsprays # Number of Sprays ***************************************************************************
      
      if("winterwheat"%in%crops==TRUE){
        # Work rates for winter wheat operations =======
        wr[1,2] <- wr[1,3] <- round(((0.06+0.00025*({cr$winterwheat[2]}+{cr$winterwheat[3]}))+
                                       (64.48+0.094*({cr$winterwheat[2]}+{cr$winterwheat[3]}))/ tsize),2) # Spread P/K fert
        wr[2,2] <- wr[2,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        wr[3,2] <- wr[3,3] <- round((0.06+0.00069*{cr$winterwheat[7]})+
                                      (58.82+41.5*{nsoil}+0.00626*{cr$winterwheat[7]})/tractor,2) # Sow
        wr[4,2] <- wr[4,3] <- round((1.5/(tpspeed*rollwidth/10))*0.85,2) # Roll
        wr[5,2] <- wr[5,3] <- 
          round(((0.06+0.00025*({cr$winterwheat[1]}))+(64.68+0.094*({cr$winterwheat[1]}))/ tsize),2) # N fert
        
        if(crops[1]=="winterwheat"){
          wr[6,2] <- wr[6,3] <- wr[6,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="winterwheat"){
          wr[6,2] <- wr[6,3] <- wr[6,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="winterwheat"){
          wr[6,2] <- wr[6,3] <- wr[6,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="winterwheat"){
          wr[6,2] <- wr[6,3] <- wr[6,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="winterwheat"){
          wr[6,2] <- wr[6,3] <- wr[6,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="winterwheat"){
          wr[6,2] <- wr[6,3] <- wr[6,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[6,2] <- wr[6,3] <- wr[6,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        
        cbwwht <- round((1.00*({cr$winterwheat[c3]}+20)/4)/combine,2) # Combine harvester
        wr[7,2] <- 2* cbwwht; wr[7,3] <- 3* cbwwht; wr[7,6] <- cbwwht
        bawwht <- round((({cr$winterwheat[c5]}+13)/4)/combine*0.5,2) # Bale
        wr[8,2] <- 2* bawwht; wr[8,3] <- 2* bawwht; wr[8,7] <- bawwht
      }else{
        wr[seq(1,8),seq(2,12)] <- 0
      }
      
      if("springwheat"%in%crops==TRUE){
        # Work rates for spring wheat operations =======
        wr[9,2] <- wr[9,3] <- round(((0.06+0.00025*({cr$springwheat[3]}+{cr$springwheat[5]}))+
                                       (64.48+0.094*({cr$springwheat[3]}+{cr$springwheat[5]}))/ tsize),2) # Spread P/K fert
        wr[10,2] <- wr[10,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        wr[11,2] <- wr[11,3] <- round(((0.06+0.00069*{cr$springwheat[7]})+
                                         (58.82+41.5*{nsoil}+0.00626*{cr$springwheat[7]})/tractor),2) #Sow
        wr[12,2] <- wr[12,3] <- round((1.5/(tpspeed*rollwidth/10))*0.85,2) # Roll
        wr[13,2] <- wr[13,3] <- round(((0.06+0.00025*({cr$springwheat[1]}))+(64.68+0.094*({cr$springwheat[1]}))/ tsize),2) # N fert
        
        if(crops[1]=="springwheat"){
          wr[14,2] <- wr[14,3] <- wr[14,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="springwheat"){
          wr[14,2] <- wr[14,3] <- wr[14,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="springwheat"){
          wr[14,2] <- wr[14,3] <- wr[14,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="springwheat"){
          wr[14,2] <- wr[14,3] <- wr[14,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="springwheat"){
          wr[14,2] <- wr[14,3] <- wr[14,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="springwheat"){
          wr[14,2] <- wr[14,3] <- wr[14,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[14,2] <- wr[14,3] <- wr[14,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        cbswht <- round(2*(1.00*({cr$springwheat[c3]}+20)/4)/combine,2) # Combine
        wr[15,2] <- 2* cbswht; wr[15,3] <- 3* cbswht; wr[15,6] <- cbswht
        baswht <- round((({cr$springwheat[c5]}+13)/4)/combine*0.5,2) # Bale
        wr[16,2] <- 2* baswht;  wr[16,3] <- 2* baswht; wr[16,7] <- baswht
        # 
      }else{
        wr[seq(9,16),seq(2,12)] <-0
      }
      
      if("winterbarley"%in%crops==TRUE||"winteroats"%in%crops==TRUE){
        # Work rates for winter barley operations =====
        wr[17,2] <- wr[17,3] <- round(((0.06+0.00025*({cr$winterbarley[3]}+{cr$winterbarley[5]}))+
                                         (64.48+0.094*({cr$winterbarley[3]}+{cr$winterbarley[5]}))/ tsize),2) # Spread P/K fert
        wr[18,2] <- wr[18,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        wr[19,2] <- wr[19,3] <- round(((0.06+0.00069*{cr$winterbarley[7]})+
                                         (58.82+41.5*{nsoil}+0.00626*{cr$winterbarley[7]})/tractor),2) # Sow
        wr[20,2] <- wr[20,3] <- round((1.5/(tpspeed*rollwidth/10))*0.85,2) # Roll
        wr[21,2] <- wr[21,3] <- 
          round(((0.06+0.00025*({cr$winterbarley[1]}))+(64.68+0.094*({cr$winterbarley[1]}))/ tsize),2) # N fert
        
        if(crops[1]=="winterbarley"||crops[1]=="winteroats"){
          wr[22,2] <- wr[22,3] <- wr[22,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="winterbarley"||crops[2]=="winteroats"){
          wr[22,2] <- wr[22,3] <- wr[22,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="winterbarley"||crops[3]=="winteroats"){
          wr[22,2] <- wr[22,3] <- wr[22,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="winterbarley"||crops[4]=="winteroats"){
          wr[22,2] <- wr[22,3] <- wr[22,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="winterbarley"||crops[5]=="winteroats"){
          wr[22,2] <- wr[22,3] <- wr[22,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="winterbarley"||crops[6]=="winteroats"){
          wr[22,2] <- wr[22,3] <- wr[22,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[22,2] <- wr[22,3] <- wr[22,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        cbwbar <- round((1.15*({cr$winterbarley[c3]}+24)/4)/combine,2) # Combine
        wr[23,2] <- 2* cbwbar; wr[23,3] <- 3* cbwbar; wr[23,6] <- cbwbar
        bawbar <- round((({cr$winterbarley[c5]}+13)/4)/combine*0.5,2) # Bale
        wr[24,2] <- 3* bawbar; wr[24,3] <- 3* bawbar; wr[24,7] <- bawbar
      }else{
        wr[seq(17,24),seq(2,12)] <- 0
      }
      
      if("springbarley"%in%crops==TRUE||"springoats"%in%crops==TRUE){
        # Work rates for spring barley operations =====
        wr[25,2] <- wr[25,3] <- round(((0.06+0.00025*({cr$springbarley[2]}+{cr$springbarley[3]}))+
                                         (64.48+0.094*({cr$springbarley[2]}+{cr$springbarley[3]}))/ tsize),2) # Spread P/K fert
        wr[26,2] <- wr[26,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        wr[27,2] <- wr[27,3] <- round(((0.06+0.00069*{cr$springbarley[7]})+
                                         (58.82+41.5*{nsoil}+0.00626*{cr$springbarley[7]})/tractor),2) # Sow
        wr[28,2] <- wr[28,3] <- round((1.5/(tpspeed*rollwidth/10))*0.85,2) # Roll
        
        #wr[29,2] <- wr[29,3] <- wr[29,5] <- round(0.11+204.2/sprayer,2) # Spraying
        if(crops[1]=="springbarley"||crops[1]=="springoats"){
          wr[29,2] <- wr[29,3] <- wr[29,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="springbarley"||crops[2]=="springoats"){
          wr[29,2] <- wr[29,3] <- wr[29,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="springbarley"||crops[3]=="springoats"){
          wr[29,2] <- wr[29,3] <- wr[29,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="springbarley"||crops[4]=="springoats"){
          wr[29,2] <- wr[29,3] <- wr[29,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="springbarley"||crops[5]=="springoats"){
          wr[29,2] <- wr[29,3] <- wr[29,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="springbarley"||crops[6]=="springoats"){
          wr[29,2] <- wr[29,3] <- wr[29,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[29,2] <- wr[29,3] <- wr[29,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        
        wr[30,2] <- wr[30,3] <- 
          round(((0.06+0.00025*({cr$springbarley[1]}))+(64.68+0.094*({cr$springbarley[1]}))/ tsize),2) # N fert
        cbsbar <- round(((1.15*{cr$springbarley[c3]}+24)/4)/combine,2) # Combine
        wr[31,2] <- 2* cbsbar; wr[31,3] <- 3* cbsbar; wr[31,6] <- cbsbar
        basbar <- round((({cr$springbarley[c5]}+13)/4)/combine*0.5,2) # Bale
        wr[32,2] <- 3* basbar; wr[32,3] <- 3* basbar; wr[32,7] <- basbar
      }else{
        wr[seq(25,32),seq(2,12)] <- 0
      }
      
      if("winterbeans"%in%crops==TRUE){
        # Work rates for winter beans operations =====
        wr[33,2] <- wr[33,3] <- 
          round(((0.06+0.00025*({cr$winterbeans[3]}+{cr$winterbeans[5]}))+
                   (64.48+0.094*({cr$winterbeans[3]}+{cr$winterbeans[5]}))/ tsize),2) # Spread P/K fert
        wr[34,2] <- wr[34,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        wr[35,2] <- wr[35,3] <- round((3*(0.114+0.00033*{cr$winterbeans[7]})+
                                         (54*{nsoil}+21.6)/tractor),2) # Sow
        wr[36,2] <- wr[36,3] <- round((1.5/(tpspeed*rollwidth/10))*0.85,2) # Roll
        
        #wr[37,2] <- wr[37,3] <- wr[37,5] <- round(0.11+204.2/sprayer,2) #Spray
        if(crops[1]=="winterbeans"){
          wr[37,2] <- wr[37,3] <- wr[37,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="winterbeans"){
          wr[37,2] <- wr[37,3] <- wr[37,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="winterbeans"){
          wr[37,2] <- wr[37,3] <- wr[37,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="winterbeans"){
          wr[37,2] <- wr[37,3] <- wr[37,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="winterbeans"){
          wr[37,2] <- wr[37,3] <- wr[37,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="winterbeans"){
          wr[37,2] <- wr[37,3] <- wr[37,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[37,2] <- wr[37,3] <- wr[37,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        cbwbea <- round((4.05*({cr$winterbeans[c3]}+24)/4)/combine,2) #Combine
        wr[38,2] <- 2* cbwbea; wr[38,3] <- 3* cbwbea; wr[38,6] <- cbwbea
      }else{
        wr[seq(33,38),seq(2,12)] <- 0
      }
      
      if("springbeans"%in%crops==TRUE){
        # Work rates for spring beans operations =====
        wr[39,2] <- wr[39,3] <- 
          round(((0.06+0.00025*({cr$springbeans[2]}+{cr$springbeans[3]}))+
                   (64.48+0.094*({cr$springbeans[2]}+{cr$springbeans[3]}))/ tsize),2) # Spread P/K fert
        wr[40,2] <- wr[40,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        wr[41,2] <- wr[41,3] <- round(((0.06+0.00069*{cr$springbeans[7]})+
                                         (92.42+0.00626*{cr$springbeans[7]}+41.5*{nsoil})/tractor),2) # Sow
        
        if(crops[1]=="springbeans"){
          wr[42,2] <- wr[42,3] <- wr[42,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="springbeans"){
          wr[42,2] <- wr[42,3] <- wr[42,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="springbeans"){
          wr[42,2] <- wr[42,3] <- wr[42,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="springbeans"){
          wr[42,2] <- wr[42,3] <- wr[42,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="springbeans"){
          wr[42,2] <- wr[42,3] <- wr[42,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="springbeans"){
          wr[42,2] <- wr[42,3] <- wr[42,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[42,2] <- wr[42,3] <- wr[42,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        cbsbea <- round((4.05*({cr$springbeans[c3]}+24)/4)/combine,2) # Harvest
        wr[43,2] <- 2* cbsbea; wr[43,3] <- 3* cbsbea; wr[43,6] <- cbsbea
      }else{
        wr[seq(39,43),seq(2,12)] <-0
      }
      
      if("warepotatoes"%in%crops==TRUE){
        # Work rates for ware potatoes operations =====
        wr[44,2] <- wr[44,3] <- 
          round(((1.80*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage 
        wr[45,2] <- wr[45,3] <- 
          wr[45,4] <- round(((25*{nsoil}+33)/tractor),2) # Harrowing
        wr[46,2] <- wr[46,3] <- 
          round(((278/tractor+0.04+0.55*{cr$warepotatoes[7]})/2000),2)*3 # Sow potatoes
        wr[47,2] <- wr[47,3] <- round(((40*{nsoil}+33)/tractor),2) # Ridging
        
        if(crops[1]=="warepotatoes"){
          wr[48,2] <- wr[48,3] <- wr[48,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="warepotatoes"){
          wr[48,2] <- wr[48,3] <- wr[48,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="warepotatoes"){
          wr[48,2] <- wr[48,3] <- wr[48,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="warepotatoes"){
          wr[48,2] <- wr[48,3] <- wr[48,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="warepotatoes"){
          wr[48,2] <- wr[48,3] <- wr[48,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="warepotatoes"){
          wr[48,2] <- wr[48,3] <- wr[48,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[48,2] <- wr[48,3] <- wr[48,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        wr[49,2] <- wr[49,3] <- round(((0.06+0.00025*({cr$warepotatoes[2]}+{cr$warepotatoes[3]}))+
                                         (64.48+0.094*({cr$warepotatoes[2]}+{cr$warepotatoes[3]}))/tsize),2) # Spread P/K fert
        wr[50,2] <- wr[50,3] <- 
          round(((0.06+0.00025*({cr$warepotatoes[1]}))+(64.68+0.094*({cr$warepotatoes[1]}))/ tsize),2) # N fert
        wr[51,2] <- wr[51,3] <-  round(1/(sphoe*rowsp/10*0.8),2) # Hoeing
        hpot <- round(((403/600)+2/(3*(1.25+0.51*{nsoil})*({39.43}/37.728)))*2.51,2) # Harvest pot
        wr[52,2] <- hpot*4; wr[52,3] <- hpot*4; wr[52,9] <- hpot 
      }else{
        wr[seq(44,52),seq(2,12)] <-0
      }
      
      if("wosr"%in%crops==TRUE){
        # Work rates for wosr operations =====
        wr[53,2] <- wr[53,3] <- round(((0.06+0.00025*({cr$wosr[2]}+{cr$wosr[3]}))+
                                         (64.48+0.094*({cr$wosr[2]}+{cr$wosr[3]}))/ tsize),2) # Spread P/K fert
        wr[54,2] <- wr[54,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        wr[55,2] <- wr[55,3] <- round(((0.387+0.00069*cr$wosr[7])+(99.42+0.00626*cr$wosr[7])/tractor),2)# Sow
        
        if(crops[1]=="wosr"){
          wr[56,2] <- wr[56,3] <- wr[25,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="wosr"){
          wr[56,2] <- wr[56,3] <- wr[25,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="wosr"){
          wr[56,2] <- wr[56,3] <- wr[25,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="wosr"){
          wr[56,2] <- wr[56,3] <- wr[25,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="wosr"){
          wr[56,2] <- wr[56,3] <- wr[25,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="wosr"){
          wr[56,2] <- wr[56,3] <- wr[25,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[56,2] <- wr[56,3] <- wr[25,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        wr[57,2] <- wr[57,3] <- 
          round(((0.06+0.00025*({cr$wosr[1]}))+(64.68+0.094*({cr$wosr[1]}))/ tsize),2) # N fert
        cbwosr <- round(((4.05*({cr$wosr[c3]}+24)/4)/combine),2) # Combine
        wr[58,2] <- 2* cbwosr; wr[58,3] <- 3* cbwosr; wr[58,6] <- cbwosr
      }else{
        wr[seq(53,58),seq(2,12)] <-0
      }
      
      if("sugarbeet"%in%crops==TRUE){
        # Work rates for sugarbeet operations =====
        wr[59,2] <- wr[59,3] <- round((1.80*(50*{nsoil}+20))/tractor,2) # Ploughing 
        wr[60,2] <- wr[60,3]  <- wr[29,4] <- round(((25*{nsoil}+33)/tractor)*wkb,2) # Harrowing
        wr[61,2] <- wr[61,3] <- round((0.39+157/tractor), 2) # Planting
        
        if(crops[1]=="sugarbeet"){
          wr[62,2] <- wr[62,3] <- wr[62,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="sugarbeet"){
          wr[62,2] <- wr[62,3] <- wr[62,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="sugarbeet"){
          wr[62,2] <- wr[62,3] <- wr[62,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="sugarbeet"){
          wr[62,2] <- wr[62,3] <- wr[62,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="sugarbeet"){
          wr[62,2] <- wr[62,3] <- wr[62,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="sugarbeet"){
          wr[62,2] <- wr[62,3] <- wr[62,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[62,2] <- wr[62,3] <- wr[62,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        wr[63,2] <- wr[63,3] <- round(((0.06+0.00025*({cr$sugarbeet[2]}+{cr$sugarbeet[3]}))+
                                         (64.48+0.094*({cr$sugarbeet[2]}+{cr$sugarbeet[3]}))/ tsize),2) # Spread P/K fert
        wr[64,2] <- wr[64,3] <- 
          round(((0.06+0.00025*({cr$sugarbeet[1]}))+(64.68+0.094*({cr$sugarbeet[1]}))/ tsize),2) # N fert
        wr[65,2] <- wr[65,3] <-  round(1/(sphoe*rowsp/10*0.8),2) # Hoeing
        hvsbee <- round(((403/600)+2/(3*(1.25+0.51*{nsoil})*({cr$sugarbeet[c3]}/37.728))),2) # Harvest
        wr[66,2] <- 3* hvsbee; wr[66,3] <- 3* hvsbee; wr[66,8] <- hvsbee
      }else{
        wr[seq(59,66),seq(2,12)] <-0
      }
      
      if("setaside"%in%crops==TRUE){#||"fallow"%in%crops==TRUE
        # Work rates for setaside operations =====
        wr[67,2] <- wr[67,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        #wr[68,2] <- wr[68,3] <- wr[68,5] <- round(0.11+204.2/sprayer,2)
        if(crops[1]=="setaside"){
          wr[68,2] <- wr[68,3] <- wr[68,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="setaside"){
          wr[68,2] <- wr[68,3] <- wr[68,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="setaside"){
          wr[68,2] <- wr[68,3] <- wr[68,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="setaside"){
          wr[68,2] <- wr[68,3] <- wr[68,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="setaside"){
          wr[68,2] <- wr[68,3] <- wr[68,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="setaside"){
          wr[68,2] <- wr[68,3] <- wr[68,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[68,2] <- wr[68,3] <- wr[68,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        
      }else{
        wr[seq(67,68),seq(2,12)] <-0
      }
      
      if("sosr"%in%crops==TRUE){
        # Work rates for sosr operations =====
        wr[69,2] <- wr[69,3] <- round(((0.06+0.00025*({cr$sosr[2]}+{cr$sosr[3]}))+
                                         (64.48+0.094*({cr$sosr[2]}+{cr$sosr[3]}))/ tsize),2) # Spread P/K fert
        wr[70,2] <- wr[70,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        wr[71,2] <- wr[71,3] <- round(((0.06+0.00069*cr$sosr[7])+(92.42+0.00626*(cr$sosr[7])+41.5*(nsoil))/tractor),2) # Sow
        
        if(crops[1]=="sosr"){
          wr[72,2] <- wr[72,3] <- wr[72,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="sosr"){
          wr[72,2] <- wr[72,3] <- wr[72,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="sosr"){
          wr[72,2] <- wr[72,3] <- wr[72,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="sosr"){
          wr[72,2] <- wr[72,3] <- wr[72,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="sosr"){
          wr[72,2] <- wr[72,3] <- wr[72,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="sosr"){
          wr[72,2] <- wr[72,3] <- wr[72,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[72,2] <- wr[72,3] <- wr[72,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        wr[73,2] <- wr[73,3] <- 
          round(((0.06+0.00025*({cr$sosr[1]}))+(64.68+0.094*({cr$sosr[1]}))/ tsize),2) # N fert
        cbsosr <- round(((4.05*({cr$sosr[c3]}+24)/4)/combine),2) # Harvest
        wr[74,2] <- 2* cbsosr; wr[74,3] <- 3* cbsosr; wr[74,6] <- cbsosr
      }else{
        wr[seq(69,74),seq(2,12)] <-0
      }
      
      if("winterlinseed"%in%crops==TRUE){
        # Work rates for winterlinseed operations =====
        wr[75,2] <- wr[75,3] <- round(((0.06+0.00025*({cr$winterlinseed[2]}+{cr$winterlinseed[3]}))+
                                         (64.48+0.094*({cr$winterlinseed[2]}+{cr$winterlinseed[3]}))/ tsize),2) # Spread P/K fert
        wr[76,2] <- wr[76,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        
        wr[77,2] <- wr[77,3] <- round(((0.06+0.00069*cr$winterlinseed[7])+
                                         (92.42+0.00626*(cr$winterlinseed[7])+41.5*(nsoil))/tractor),2) # Planting
        
        if(crops[1]=="winterlinseed"){
          wr[78,2] <- wr[78,3] <- wr[78,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="winterlinseed"){
          wr[78,2] <- wr[78,3] <- wr[78,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="winterlinseed"){
          wr[78,2] <- wr[78,3] <- wr[78,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="winterlinseed"){
          wr[78,2] <- wr[78,3] <- wr[78,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="winterlinseed"){
          wr[78,2] <- wr[78,3] <- wr[78,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="winterlinseed"){
          wr[78,2] <- wr[78,3] <- wr[78,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[78,2] <- wr[78,3] <- wr[78,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        wr[79,2] <- wr[79,3] <- 
          round(((0.06+0.00025*({cr$winterlinseed[1]}))+(64.68+0.094*({cr$winterlinseed[1]}))/ tsize),2) # N fert
        cbwlin <- round(((4.05*({cr$winterlinseed[c3]}+24)/4)/combine),2) # Harvest
        wr[80,2] <- 2* cbwlin; wr[80,3] <- 3* cbwlin; wr[80,6] <- cbwlin
      }else{
        wr[seq(75,80),seq(2,12)] <-0
      }
      
      if("springlinseed"%in%crops==TRUE){
        # Work rates for springlinseed operations =====
        wr[81,2] <- wr[81,3] <- round(((0.06+0.00025*({cr$springlinseed[2]}+{cr$springlinseed[3]}))+
                                         (64.48+0.094*({cr$springlinseed[2]}+{cr$springlinseed[3]}))/ tsize),2) # Spread P/K fert
        wr[82,2] <- wr[82,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        
        wr[83,2] <- wr[83,3] <- round(((0.06+0.00069*cr$springlinseed[7])+
                                         (92.42+0.00626*(cr$springlinseed[7])+41.5*(nsoil))/tractor),2) # Planting
        
        if(crops[1]=="springlinseed"){
          wr[84,2] <- wr[84,3] <- wr[84,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="springlinseed"){
          wr[84,2] <- wr[84,3] <- wr[84,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="springlinseed"){
          wr[84,2] <- wr[84,3] <- wr[84,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="springlinseed"){
          wr[84,2] <- wr[84,3] <- wr[84,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="springlinseed"){
          wr[84,2] <- wr[84,3] <- wr[84,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="springlinseed"){
          wr[84,2] <- wr[84,3] <- wr[84,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[84,2] <- wr[84,3] <- wr[84,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        wr[85,2] <- wr[85,3] <- 
          round(((0.06+0.00025*({cr$springlinseed[1]}))+(64.68+0.094*({cr$springlinseed[1]}))/ tsize),2) # N fert
        cbslin <- round(((4.05*({cr$springlinseed[c3]}+24)/4)/combine),2) # Harvest
        wr[86,2] <- 2* cbslin; wr[86,3] <- 3* cbslin; wr[86,6] <- cbslin
      }else{
        wr[seq(81,86),seq(2,12)] <-0
      }
      
      if("driedpeas"%in%crops==TRUE||"peas"%in%crops==TRUE){
        # Work rates for peas operations =====
        wr[87,2] <- wr[87,3] <- 
          round(((0.06+0.00025*({cr$driedpeas[3]}+{cr$driedpeas[5]}))+
                   (64.48+0.094*({cr$driedpeas[3]}+{cr$driedpeas[5]}))/ tsize),2) # P/K Fert
        wr[88,2] <- wr[88,3] <- round(((1.44*(50*{nsoil}+20))/tractor)*wkb,2) # Tillage
        
        wr[89,2] <- wr[89,3] <- round((0.06+0.00069*{cr$driedpeas[7]})+
                                        (92.42+0.00626*{cr$driedpeas[7]}+41.5*{nsoil})/tractor,2) # Planting
        wr[90,2] <- wr[90,3] <- round((1.5/(tpspeed*rollwidth/10))*0.85,2) # Roll
        
        if(crops[1]=="driedpeas"||crops[1]=="peas"){
          wr[91,2] <- wr[91,3] <- wr[91,5] <- round(0.11+204.2/sprayer,2)*nspr[1] # Spraying
        }else if(crops[2]=="driedpeas"||crops[2]=="peas"){
          wr[91,2] <- wr[91,3] <- wr[91,5] <- round(0.11+204.2/sprayer,2)*nspr[2] # Spraying
        }else if(crops[3]=="driedpeas"||crops[3]=="peas"){
          wr[91,2] <- wr[91,3] <- wr[91,5] <- round(0.11+204.2/sprayer,2)*nspr[3] # Spraying
        }else if(crops[4]=="driedpeas"||crops[4]=="peas"){
          wr[91,2] <- wr[91,3] <- wr[91,5] <- round(0.11+204.2/sprayer,2)*nspr[4] # Spraying
        }else if(crops[5]=="driedpeas"||crops[5]=="peas"){
          wr[91,2] <- wr[91,3] <- wr[91,5] <- round(0.11+204.2/sprayer,2)*nspr[5] # Spraying
        }else if(crops[6]=="driedpeas"||crops[6]=="peas"){
          wr[91,2] <- wr[91,3] <- wr[91,5] <- round(0.11+204.2/sprayer,2)*nspr[6] # Spraying
        }else{
          wr[91,2] <- wr[91,3] <- wr[91,5] <- round(0.11+204.2/sprayer,2)*0 # Spraying
        }
        cbdpea <- round((4.05*({cr$driedpeas[c3]}+24)/4)/combine,2) # Harvest
        wr[92,2] <- 2* cbdpea; wr[92,3] <- 3* cbdpea; wr[92,6] <- cbdpea
      }else{
        wr[seq(87,92),seq(2,12)] <- 0
      }
      
      kk3 <- wr
      
      
      # ******* Estimates fuel and labour costs ***************
      
      fuelPrice <- fuelprice #farm[2]
      labourWage <- labourwage #farm[3]
      TP <- round(tractor*1.341,0) # maximum PTO horsepower 
      CP <- round(combsize*1.341,0)
      
      # For diesel tractor, fuel consumption is estimated from the formula below
      # Obtained from: http://www.extension.iastate.edu/agdm/crops/html/a3-29.html (16/06/2015)
      # 0.044 * maximum PTO horsepower for diesel engines
      
      fuel.cons_gal_hr <- round(0.044 * TP,2) # Gallons per hour Tractor
      fuel.cons_gal_hr_comb <- round(0.044 * CP,2) # Combine
      # Convert gallons per hour to litres per hour
      # 1 gallon per hour = 4.546 litres per hour
      
      Fuel_Cons <- round(fuel.cons_gal_hr * 4.546,0) # Fuel consumption (litres/hour)
      Fuel_Cons_comb <- round(fuel.cons_gal_hr_comb * 4.546,0)
      
      # Fuel and labour cost (£/hour)
      fuelCost <-  (fuelPrice * Fuel_Cons)*1.1 # 10% represents oil and lubricants
      fuelCost_comb <- (fuelPrice * Fuel_Cons_comb)*1.1
      
      #Fuel and labour costs @ £/ha (Multiply £/hour by workrates (hr/ha))
      kk3[,10] <- round((kk3[,2]*fuelCost)+(kk3[,6]*fuelCost_comb)+
                          (kk3[,7]*fuelCost_comb)) # Fuel costs
      kk3[,11] <- round(kk3[,3]*labourWage) # Labour costs
      kk3[,12] <- round(kk3[,10]+kk3[,11])
      ops <- kk3
      
      # Fuel cost
      fu <- ops[,10]
      
      fuelcost <- c(sum(fu[seq(1,8)]),sum(fu[seq(9,16)]),sum(fu[seq(17,24)]),sum(fu[seq(25,32)]),
                    sum(fu[seq(33,38)]),sum(fu[seq(39,43)]),sum(fu[seq(44,52)]),sum(fu[seq(53,58)]),sum(fu[seq(59,66)]),
                    sum(fu[seq(67,68)]),sum(fu[seq(69,74)]),sum(fu[seq(75,80)]),sum(fu[seq(81,86)]),sum(fu[seq(87,92)]))
      # Labour cost
      la <- ops[,11]
      
      labourcost <- c(sum(la[seq(1,8)]),sum(la[seq(9,16)]),sum(la[seq(17,24)]),sum(la[seq(25,32)]),
                      sum(la[seq(33,38)]),sum(la[seq(39,43)]),sum(la[seq(44,52)]),sum(la[seq(53,58)]),sum(la[seq(59,66)]),
                      sum(la[seq(67,68)]),sum(la[seq(69,74)]),sum(la[seq(75,80)]),sum(la[seq(81,86)]),sum(la[seq(87,92)]))
      # Operating Cost
      op1 <- ops[,12]
      
      opcost <- c(sum(op1[seq(1,8)]),sum(op1[seq(9,16)]),sum(op1[seq(17,24)]),sum(op1[seq(25,32)]),
                  sum(op1[seq(33,38)]),sum(op1[seq(39,43)]),sum(op1[seq(44,52)]),sum(op1[seq(53,58)]),sum(op1[seq(59,66)]),
                  sum(op1[seq(67,68)]),sum(op1[seq(69,74)]),sum(op1[seq(75,80)]),sum(op1[seq(81,86)]),sum(op1[seq(87,92)]))
      
      crops <- crops
      
      if(crops[1]=="winterwheat"){
        fc1 <- fuelcost[1]; lc1 <- labourcost[1]; opc1 <- opcost[1]
      }else if(crops[1]=="springwheat"){
        fc1 <- fuelcost[2]; lc1 <- labourcost[2]; opc1 <- opcost[2]
      }else if(crops[1]=="winterbarley"||crops[1]=="winteroats"){
        fc1 <- fuelcost[3]; lc1 <- labourcost[3]; opc1 <- opcost[3]
      }else if(crops[1]=="springbarley"||crops[1]=="springoats"){
        fc1 <- fuelcost[4]; lc1 <- labourcost[4]; opc1 <- opcost[4]
      }else if(crops[1]=="winterbeans"){
        fc1 <- fuelcost[5]; lc1 <- labourcost[5]; opc1 <- opcost[5]
      }else if(crops[1]=="springbeans"){
        fc1 <- fuelcost[6]; lc1 <- labourcost[6]; opc1 <- opcost[6]
      }else if(crops[1]=="warepotatoes"){
        fc1 <- fuelcost[7]; lc1 <- labourcost[7]; opc1 <- opcost[7]
      }else if(crops[1]=="wosr"){
        fc1 <- fuelcost[8]; lc1 <- labourcost[8]; opc1 <- opcost[8]
      }else if(crops[1]=="sugarbeet"){
        fc1 <- fuelcost[9]; lc1 <- labourcost[9]; opc1 <- opcost[9]
      }else if(crops[1]=="setaside"||crops[1]=="fallow"){
        fc1 <- fuelcost[10]; lc1 <- labourcost[10]; opc1 <- opcost[10]
      }else if(crops[1]=="sosr"){
        fc1 <- fuelcost[11]; lc1 <- labourcost[11]; opc1 <- opcost[11]
      }else if(crops[1]=="winterlinseed"){
        fc1 <- fuelcost[12]; lc1 <- labourcost[12]; opc1 <- opcost[12]
      }else if(crops[1]=="springlinseed"){
        fc1 <- fuelcost[13]; lc1 <- labourcost[13]; opc1 <- opcost[13]
      }else if(crops[1]=="driedpeas"){
        fc1 <- fuelcost[14]; lc1 <- labourcost[14]; opc1 <- opcost[14]
      }
      
      if(crops[2]=="winterwheat"){
        fc2 <- fuelcost[1]; lc2 <- labourcost[1]; opc2 <- opcost[1]
      }else if(crops[2]=="springwheat"){
        fc2 <- fuelcost[2]; lc2 <- labourcost[2]; opc2 <- opcost[2]
      }else if(crops[2]=="winterbarley"||crops[2]=="winteroats"){
        fc2 <- fuelcost[3]; lc2 <- labourcost[3]; opc2 <- opcost[3]
      }else if(crops[2]=="springbarley"||crops[2]=="springoats"){
        fc2 <- fuelcost[4]; lc2 <- labourcost[4]; opc2 <- opcost[4]
      }else if(crops[2]=="winterbeans"){
        fc2 <- fuelcost[5]; lc2 <- labourcost[5]; opc2 <- opcost[5]
      }else if(crops[2]=="springbeans"){
        fc2 <- fuelcost[6]; lc2 <- labourcost[6]; opc2 <- opcost[6]
      }else if(crops[2]=="warepotatoes"){
        fc2 <- fuelcost[7]; lc2 <- labourcost[7]; opc2 <- opcost[7]
      }else if(crops[2]=="wosr"){
        fc2 <- fuelcost[8]; lc2 <- labourcost[8]; opc2 <- opcost[8]
      }else if(crops[2]=="sugarbeet"){
        fc2 <- fuelcost[9]; lc2 <- labourcost[9]; opc2 <- opcost[9]
      }else if(crops[2]=="setaside"||crops[2]=="fallow"){
        fc2 <- fuelcost[10]; lc2 <- labourcost[10]; opc2 <- opcost[10]
      }else if(crops[2]=="sosr"){
        fc2 <- fuelcost[11]; lc2 <- labourcost[11]; opc2 <- opcost[11]
      }else if(crops[2]=="winterlinseed"){
        fc2 <- fuelcost[12]; lc2 <- labourcost[12]; opc2 <- opcost[12]
      }else if(crops[2]=="springlinseed"){
        fc2 <- fuelcost[13]; lc2 <- labourcost[13]; opc2 <- opcost[13]
      }else if(crops[2]=="driedpeas"){
        fc2 <- fuelcost[14]; lc2 <- labourcost[14]; opc2 <- opcost[14]
      }
      
      if(crops[3]=="winterwheat"){
        fc3 <- fuelcost[1]; lc3 <- labourcost[1]; opc3 <- opcost[1]
      }else if(crops[3]=="springwheat"){
        fc3 <- fuelcost[2]; lc3 <- labourcost[2]; opc3 <- opcost[2]
      }else if(crops[3]=="winterbarley"||crops[3]=="winteroats"){
        fc3 <- fuelcost[3]; lc3 <- labourcost[3]; opc3 <- opcost[3]
      }else if(crops[3]=="springbarley"||crops[3]=="springoats"){
        fc3 <- fuelcost[4]; lc3 <- labourcost[4]; opc3 <- opcost[4]
      }else if(crops[3]=="winterbeans"){
        fc3 <- fuelcost[5]; lc3 <- labourcost[5]; opc3 <- opcost[5]
      }else if(crops[3]=="springbeans"){
        fc3 <- fuelcost[6]; lc3 <- labourcost[6]; opc3 <- opcost[6]
      }else if(crops[3]=="warepotatoes"){
        fc3 <- fuelcost[7]; lc3 <- labourcost[7]; opc3 <- opcost[7]
      }else if(crops[3]=="wosr"){
        fc3 <- fuelcost[8]; lc3 <- labourcost[8]; opc3 <- opcost[8]
      }else if(crops[3]=="sugarbeet"){
        fc3 <- fuelcost[9]; lc3 <- labourcost[9]; opc3 <- opcost[9]
      }else if(crops[3]=="setaside"||crops[3]=="fallow"){
        fc3 <- fuelcost[10]; lc3 <- labourcost[10]; opc3 <- opcost[10]
      }else if(crops[3]=="sosr"){
        fc3 <- fuelcost[11]; lc3 <- labourcost[11]; opc3 <- opcost[11]
      }else if(crops[3]=="winterlinseed"){
        fc3 <- fuelcost[12]; lc3 <- labourcost[12]; opc3 <- opcost[12]
      }else if(crops[3]=="springlinseed"){
        fc3 <- fuelcost[13]; lc3 <- labourcost[13]; opc3 <- opcost[13]
      }else if(crops[3]=="driedpeas"){
        fc3 <- fuelcost[14]; lc3 <- labourcost[14]; opc3 <- opcost[14]
      }
      
      if(crops[4]=="winterwheat"){
        fc4 <- fuelcost[1]; lc4 <- labourcost[1]; opc4 <- opcost[1]
      }else if(crops[4]=="springwheat"){
        fc4 <- fuelcost[2]; lc4 <- labourcost[2]; opc4 <- opcost[2]
      }else if(crops[4]=="winterbarley"||crops[4]=="winteroats"){
        fc4 <- fuelcost[3]; lc4 <- labourcost[3]; opc4 <- opcost[3]
      }else if(crops[4]=="springbarley"||crops[4]=="springoats"){
        fc4 <- fuelcost[4]; lc4 <- labourcost[4]; opc4 <- opcost[4]
      }else if(crops[4]=="winterbeans"){
        fc4 <- fuelcost[5]; lc4 <- labourcost[5]; opc4 <- opcost[5]
      }else if(crops[4]=="springbeans"){
        fc4 <- fuelcost[6]; lc4 <- labourcost[6]; opc4 <- opcost[6]
      }else if(crops[4]=="warepotatoes"){
        fc4 <- fuelcost[7]; lc4 <- labourcost[7]; opc4 <- opcost[7]
      }else if(crops[4]=="wosr"){
        fc4 <- fuelcost[8]; lc4 <- labourcost[8]; opc4 <- opcost[8]
      }else if(crops[4]=="sugarbeet"){
        fc4 <- fuelcost[9]; lc4 <- labourcost[9]; opc4 <- opcost[9]
      }else if(crops[4]=="setaside"||crops[4]=="fallow"){
        fc4 <- fuelcost[10]; lc4 <- labourcost[10]; opc4 <- opcost[10]
      }else if(crops[4]=="sosr"){
        fc4 <- fuelcost[11]; lc4 <- labourcost[11]; opc4 <- opcost[11]
      }else if(crops[4]=="winterlinseed"){
        fc4 <- fuelcost[12]; lc4 <- labourcost[12]; opc4 <- opcost[12]
      }else if(crops[4]=="springlinseed"){
        fc4 <- fuelcost[13]; lc4 <- labourcost[13]; opc4 <- opcost[13]
      }else if(crops[4]=="driedpeas"){
        fc4 <- fuelcost[14]; lc4 <- labourcost[14]; opc4 <- opcost[14]
      }
      
      if(crops[5]=="winterwheat"){
        fc5 <- fuelcost[1]; lc5 <- labourcost[1]; opc5 <- opcost[1]
      }else if(crops[5]=="springwheat"){
        fc5 <- fuelcost[2]; lc5 <- labourcost[2]; opc5 <- opcost[2]
      }else if(crops[5]=="winterbarley"||crops[5]=="winteroats"){
        fc5 <- fuelcost[3]; lc5 <- labourcost[3]; opc5 <- opcost[3]
      }else if(crops[5]=="springbarley"||crops[5]=="springoats"){
        fc5 <- fuelcost[4]; lc5 <- labourcost[4]; opc5 <- opcost[4]
      }else if(crops[5]=="winterbeans"){
        fc5<- fuelcost[5]; lc5 <- labourcost[5]; opc5 <- opcost[5]
      }else if(crops[5]=="springbeans"){
        fc5 <- fuelcost[6]; lc5 <- labourcost[6]; opc5 <- opcost[6]
      }else if(crops[5]=="warepotatoes"){
        fc5 <- fuelcost[7]; lc5 <- labourcost[7]; opc5 <- opcost[7]
      }else if(crops[5]=="wosr"){
        fc5 <- fuelcost[8]; lc5 <- labourcost[8]; opc5 <- opcost[8]
      }else if(crops[5]=="sugarbeet"){
        fc5 <- fuelcost[9]; lc5 <- labourcost[9]; opc5 <- opcost[9]
      }else if(crops[5]=="setaside"||crops[5]=="fallow"){
        fc5 <- fuelcost[10]; lc5 <- labourcost[10]; opc5 <- opcost[10]
      }else if(crops[5]=="sosr"){
        fc5 <- fuelcost[11]; lc5 <- labourcost[11]; opc5 <- opcost[11]
      }else if(crops[5]=="winterlinseed"){
        fc5 <- fuelcost[12]; lc5 <- labourcost[12]; opc5 <- opcost[12]
      }else if(crops[5]=="springlinseed"){
        fc5 <- fuelcost[13]; lc5 <- labourcost[13]; opc5 <- opcost[13]
      }else if(crops[5]=="driedpeas"){
        fc5 <- fuelcost[14]; lc5 <- labourcost[14]; opc5 <- opcost[14]
      }
      
      if(crops[6]=="winterwheat"){
        fc6 <- fuelcost[1]; lc6 <- labourcost[1]; opc6 <- opcost[1]
      }else if(crops[6]=="springwheat"){
        fc6 <- fuelcost[2]; lc6 <- labourcost[2]; opc6 <- opcost[2]
      }else if(crops[6]=="winterbarley"||crops[6]=="winteroats"){
        fc6 <- fuelcost[3]; lc6 <- labourcost[3]; opc6 <- opcost[3]
      }else if(crops[6]=="springbarley"||crops[6]=="springoats"){
        fc6 <- fuelcost[4]; lc6 <- labourcost[4]; opc6 <- opcost[4]
      }else if(crops[6]=="winterbeans"){
        fc6 <- fuelcost[5]; lc6 <- labourcost[5]; opc6 <- opcost[5]
      }else if(crops[6]=="springbeans"){
        fc6 <- fuelcost[6]; lc6 <- labourcost[6]; opc6 <- opcost[6]
      }else if(crops[6]=="warepotatoes"){
        fc6 <- fuelcost[7]; lc6 <- labourcost[7]; opc6 <- opcost[7]
      }else if(crops[6]=="wosr"){
        fc6 <- fuelcost[8]; lc6 <- labourcost[8]; opc6 <- opcost[8]
      }else if(crops[6]=="sugarbeet"){
        fc6 <- fuelcost[9]; lc6 <- labourcost[9]; opc6 <- opcost[9]
      }else if(crops[6]=="setaside"||crops[6]=="fallow"){
        fc6 <- fuelcost[10]; lc6 <- labourcost[10]; opc6 <- opcost[10]
      }else if(crops[6]=="sosr"){
        fc6 <- fuelcost[11]; lc6 <- labourcost[11]; opc6 <- opcost[11]
      }else if(crops[6]=="winterlinseed"){
        fc6 <- fuelcost[12]; lc6 <- labourcost[12]; opc6 <- opcost[12]
      }else if(crops[6]=="springlinseed"){
        fc6 <- fuelcost[13]; lc6 <- labourcost[13]; opc6 <- opcost[13]
      }else if(crops[6]=="driedpeas"){
        fc6 <- fuelcost[14]; lc6 <- labourcost[14]; opc6 <- opcost[14]
      }
      
      cr[c11,seq(2,le)]  <- c(fc1,fc2,fc3,fc4,fc5,fc6)
      cr[c12,seq(2,le)]  <- c(lc1,lc2,lc3,lc4,lc5,lc6)
      cr[c13,seq(2,le)]  <- c(opc1,opc2,opc3,opc4,opc5,opc6)
      
      cr[c14,seq(2,le)] <- grossprofit <- cr[c10,seq(2,le)]-cr[c13,seq(2,le)] # Gross profit estimates
      kk4 <- cr[seq(1,31),]
    }
    
    
    # Model output components =====
    #**************************************************************** 
    rotl <- c(2,3,4,5,6)
    if(rotlength%in%rotl==TRUE){
      nrotl <- rotlength
    }else{
      nrotl <- 6
      warning("Maximum 6-year rotation is assumed (Rotation length MUST be between 2 and 6 years)")
    }
    
    
    if(is.null(rotprob)){
      rotp <- 1/nrotl # Rotational probability (6 year rotation)
    }else{
      rotp <- rotprob
    }
    
    di <- 2
    Component=c("Basic Rotation","Cultivation","Farm Output","Fertiliser Cost","Seed Cost","Herbicide Cost",
                "Sundry Cost","Variable Cost","Gross Margin",
                "Fuel Cost","Labour Cost","Operating Cost","Gross Profit","Rotation Profit")
    
    Unit=c("na","na",rep("?/ha",length(Component)-2))
    
    # Row Indices ===
    c1 <- 18; c2 <- 19; c3 <- 20; c4 <- 21; c5 <- 22; c6 <- 23; c7 <- 24; c8 <- 25; c9 <- 26;# === row indices
    c10 <- 27; c11 <- 28; c12 <- 29; c13 <- 30; c14 <- 31; 
    
    h1 <- 10; h2 <- 11; h3 <- 12; h4 <- 13; h5 <- 14; h6 <- 15;
    h7 <- 16; h8 <- 17; 
    
    # Year 1  *************************************************************
    
    crop1 <- crops[1]; crop2 <- crops[2]; crop3 <- crops[3]; crop4 <- crops[4]; crop5 <- crops[5]; crop6 <- crops[6]
    tillage1 <- tillages[1]; tillage2 <- tillages[2]; tillage3 <- tillages[3]; tillage4 <- tillages[4]; tillage5 <- tillages[5];
    tillage6 <- tillages[6]
    
    if(crop1=="winterwheat"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("WWHT",tillages[1],ncb)
      #cp1 <- "WWHT"
      cy1 <- cb[c3]
    }else if(crop1=="springwheat"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("SWHT",tillages[1],ncb) 
      #cp1 <- "SWHT"
      cy1 <- cb[c3]
    }else if (crop1=="winterbarley"){
      cb <- cropBudget(crop=crops, nsoil, subsidy, blackgrass, tillage1, selfrot=0, rotpen=0, yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("WBAR",tillages[1],ncb) 
      #cp1 <- "WBAR"
      cy1 <- cb[c3]
    }else if(crop1=="springbarley"){
      cb <- cropBudget(crop=crops, nsoil, subsidy, blackgrass, tillage1, selfrot=0, rotpen=0, yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("SBAR",tillages[1],ncb) 
      #cp1 <- "SBAR"
      cy1 <- cb[c3]
    }else if(crop1=="winterbeans"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("WBEA",tillages[1],ncb) 
      cp1 <- "WBEA"
      cy1 <- cb[c3]
    }else if(crop1=="springbeans"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("SBEA",tillages[1],ncb) 
      #cp1 <- "SBEA"
      cy1 <- cb[c3]
    }else if(crop1=="warepotatoes"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("WPOT",tillages[1],ncb) 
      #cp1 <- "WPOT"
      cy1 <- cb[c3]
    }else if(crop1=="wosr"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("WOSR",tillages[1],ncb) 
      #cp1 <- "WOSR"
      cy1 <- cb[c3]
    }else if(crop1=="sugarbeet"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,h8)],cb[c1]+cb[c2],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("SBEE",tillages[1],ncb) 
      #cp1 <- "SBEE"
      cy1 <- cb[c3]
    }else if(crop1=="setaside"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("SETA",tillages[1],ncb) 
      #cp1 <- "SETA"
      cy1 <- cb[c3]
    }else if(crop1=="none"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("SETA",tillages[1],ncb) 
      #cp1 <- "SETA"
      cy1 <- cb[c3]
    }else if(crop1=="fallow"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("FALLOW",tillages[1],ncb)
      #cp1 <- "SETA"
      cy1 <- cb[c3]
    }else if(crop1=="sosr"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("SOSR",tillages[1],ncb)
      cy1 <- cb[c3]
    }else if(crop1=="winterlinseed"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("WLIN",tillages[1],ncb)
      cy1 <- cb[c3]
    }else if(crop1=="springlinseed"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("SLIN",tillages[1],ncb)
      cy1 <- cb[c3]
    }else if(crop1=="driedpeas"){
      cb <- cropBudget(crop=crops,nsoil, subsidy, blackgrass,tillage1,selfrot=0,rotpen=0,yieldoption)[[2]]
      ncb <- round(c(cb[c8],cb[seq(h6,c1)],cb[seq(c9,c14)],cb[c14]*rotp),di)
      Year1 <- c("DPEA",tillages[1],ncb)
      cy1 <- cb[c3]
    }
    
    # Year 2  *************************************************************
    # ========= Yield Penalty | Crop Rotations =========
    if(crop2=="winterwheat"){ 
      
      if(crop1=="winterwheat"){
        selfrot <- 10 # 10% Yield loss
      }else{
        selfrot = 0
      }
      if(crop1=="winterbarley"||crop1=="springbarley"||crop1=="springwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("WWHT",tillage2,ncb2) 
      cy2 <- cb2[c3]
    }else if(crop2=="springwheat"){
      if(crop1=="springwheat"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      if(crop1=="winterbarley"||crop1=="springbarley"||crop1=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("SWHT",tillage2,ncb2) 
      cy2 <- cb2[c3]
    }else if (crop2=="winterbarley"){
      if(crop1=="winterbarley"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      if(crop1=="springwheat"||crop1=="springbarley"||crop1=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("WBAR",tillage2,ncb2) 
      cy2 <- cb2[c3]
    }else if(crop2=="springbarley"){
      if(crop1=="springbarley"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      if(crop1=="springwheat"||crop1=="winterbarley"||crop1=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("SBAR",tillage2,ncb2) 
      cy2 <- cb2[c3]
    }else if(crop2=="winterbeans"){
      selfrot <- 0
      if(crop1=="wosr"||crop1=="sugarbeet"||crop1=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("WBEA",tillage2,ncb2) 
      cy2 <- cb2[c3]
    }else if(crop2=="springbeans"){
      selfrot <- 0
      if(crop1=="wosr"||crop1=="sugarbeet"||crop1=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("SBEA",tillage2,ncb2) 
      cy2 <- cb2[c3]
    }else if(crop2=="warepotatoes"){
      if(crop1=="warepoatoes"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Potatoes is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop1=="sugarbeet"){ 
        rotpen <- 5
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("WPOT",tillage2,ncb2)  
      cy2 <- cb2[c3]
    }else if(crop2=="wosr"){
      if(crop1=="wosr"||crop1=="sosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop1=="winterbeans"||crop1=="springbeans"||crop1=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("WOSR",tillage2,ncb2)  
      cy2 <- cb2[c3]
    }else if(crop2=="sugarbeet"){
      if(crop1=="sugarbeat"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Sugarbeet is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop1=="winterbeans"||crop1=="springbeans"||crop1=="wosr"||crop1=="warepotatoes"||crop1=="sosr"){ 
        rotpen <- 100
        warning("Oilseed rape, Beans or Potatoes--Sugarbeet sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,h8)],cb2[c1]+cb2[c2],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("SBEE",tillage2,ncb2)  
      cy2 <- cb2[c3]
    }else if(crop2=="setaside"){ 
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot=0,rotpen=0,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("SETA",tillage2,ncb2)
      cy2 <- cb2[c3]
    }else if(crop2=="none"){
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot=0,rotpen=0,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("SETA",tillage2,ncb2)
      cy2 <- cb2[c3]
    }else if(crop2=="fallow"){
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot=0,rotpen=0,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("FALLOW",tillage2,ncb2) 
      cy2 <- cb2[c3]
    }else if(crop2=="sosr"){
      if(crop1=="sosr"||crop1=="wosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot = 0
      }
      if(crop1=="winterbeans"||crop1=="springbeans"||crop1=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("SOSR",tillage2,ncb2) 
      cy2 <- cb2[c3]
    }else if(crop2=="winterlinseed"){
      if(crop1=="winterlinseed"||crop1=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop1=="winterbeans"||crop1=="springbeans"||crop1=="sugarbeet"||crop1=="driedpeas"||crop1=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Linseed sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("WLIN",tillage2,ncb2) 
      cy2 <- cb2[c3]
    }else if(crop2=="springlinseed"){
      if(crop1=="winterlinseed"||crop1=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop1=="winterbeans"||crop1=="springbeans"||crop1=="sugarbeet"||crop1=="driedpeas"||crop1=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Linseed sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("SLIN",tillage2,ncb2)
      cy2 <- cb2[c3]
    }else if(crop2=="driedpeas"){
      if(crop1=="driedpeas"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Peas is penalised")
      }else{
        selfrot <- 0
      }
      if(crop1=="wosr"||crop1=="sosr"||crop1=="setaside"){ 
        rotpen <- 100
        warning("OSR and  Rotational Set-aside--Peas sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb2 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage2,selfrot,rotpen,yieldoption)[[3]]
      ncb2 <- round(c(cb2[c8],cb2[seq(h6,c1)],cb2[seq(c9,c14)],cb2[c14]*rotp),di)
      Year2 <- c("DPEA",tillage2,ncb2)
      cy2 <- cb2[c3]
    }
    
    
    # Year 3  *************************************************************
    
    if(crop3=="winterwheat"){
      if(crop2=="winterwheat"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      if(crop2=="winterbarley"||crop2=="springbarley"||crop2=="springwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("WWHT",tillage3,ncb3) 
      cy3 <- cb3[c3]
    }else if(crop3=="springwheat"){
      if(crop2=="springwheat"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      
      if(crop2=="winterbarley"||crop2=="springbarley"||crop2=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("SWHT",tillage3,ncb3) 
      cy3 <- cb3[c3]
    }else if (crop3=="winterbarley"){
      if(crop2=="winterbarley"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      if(crop2=="springwheat"||crop2=="springbarley"||crop2=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("WBAR",tillage3,ncb3) 
      cy3 <- cb3[c3]
    }else if(crop3=="springbarley"){
      if(crop2=="springbarley"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      if(crop2=="springwheat"||crop2=="winterbarley"||crop2=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("SBAR",tillage3,ncb3) 
      cy3 <- cb3[c3]
    }else if(crop3=="winterbeans"){
      selfrot <- 0
      if(crop2=="wosr"||crop2=="sugarbeet"||crop2=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("WBEA",tillage3,ncb3) 
      cy3 <- cb3[c3]
    }else if(crop3=="springbeans"){
      selfrot <- 0
      if(crop2=="wosr"||crop2=="sugarbeet"||crop2=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("SBEA",tillage3,ncb3) 
      cy3 <- cb3[c3]
    }else if(crop3=="warepotatoes"){
      if(crop2=="warepoatoes"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Potatoes is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop2=="sugarbeet"){ 
        rotpen <- 5
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("WPOT",tillage3,ncb3)  
      cy3 <- cb3[c3]
    }else if(crop3=="wosr"){
      if(crop2=="wosr"||crop2=="sosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot <- 0
      }
      
      if(crop2=="winterbeans"||crop2=="springbeans"||crop2=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("WOSR",tillage3,ncb3)  
      cy3 <- cb3[c3]
    }else if(crop3=="sugarbeet"){
      if(crop2=="sugarbeat"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Sugarbeet is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop2=="winterbeans"||crop2=="springbeans"||crop2=="wosr"||crop2=="warepotatoes"||crop2=="sosr"){ 
        rotpen <- 100
        warning("Sugarbeet--Oilseed rape, Beans or Potatoes sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,h8)],cb3[c1]+cb3[c2],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("SBEE",tillage3,ncb3)  
      cy3 <- cb3[c3]
    }else if(crop3=="setaside"||crop3=="none"){ 
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot=0,rotpen=0,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("SETA",tillage3,ncb3) 
      cy3 <- cb3[c3]
    }else if(crop3=="none"){
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot=0,rotpen=0,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("SETA",tillage3,ncb3) 
      cy3 <- cb3[c3]
    }else if(crop3=="fallow"){
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot=0,rotpen=0,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("FALLOW",tillage3,ncb3)
      cy3 <- cb3[c3]
    }else if(crop3=="sosr"){
      if(crop2=="wosr"||crop2=="sosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot = 0
      }
      if(crop2=="winterbeans"||crop2=="springbeans"||crop2=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("SOSR",tillage3,ncb3)
      cy3 <- cb3[c3]
    }else if(crop3=="winterlinseed"){
      if(crop2=="winterlinseed"||crop2=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop2=="winterbeans"||crop2=="springbeans"||crop2=="sugarbeet"||crop2=="driedpeas"||crop2=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("WLIN",tillage3,ncb3)
      cy3 <- cb3[c3]
    }else if(crop3=="springlinseed"){
      if(crop2=="winterlinseed"||crop2=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop2=="winterbeans"||crop2=="springbeans"||crop2=="sugarbeet"||crop2=="driedpeas"||crop2=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("SLIN",tillage3,ncb3) 
      cy3 <- cb3[c3]
    }else if(crop3=="driedpeas"){
      if(crop2=="driedpeas"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Peas is penalised")
      }else{
        selfrot <- 0
      }
      if(crop2=="wosr"||crop2=="sosr"||crop2=="setaside"){ 
        rotpen <- 100
        warning("OSR and Rotational Set-aside--Peas sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb3 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage3,selfrot,rotpen,yieldoption)[[4]]
      ncb3 <- round(c(cb3[c8],cb3[seq(h6,c1)],cb3[seq(c9,c14)],cb3[c14]*rotp),di)
      Year3 <- c("DPEA",tillage3,ncb3) 
      cy3 <- cb3[c3]
    } 
    
    
    # Year 4  *************************************************************
    
    if(crop4=="winterwheat"){
      
      if(crop3=="winterwheat"){
        selfrot <- 15 # 11% Yield loss
      }else{
        selfrot = 0
      }
      if(crop3=="winterbarley"||crop3=="springbarley"||crop3=="springwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("WWHT",tillage4,ncb4) 
      cy4 <- cb4[c3]
    }else if(crop4=="springwheat"){
      if(crop3=="springwheat"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      
      if(crop3=="winterbarley"||crop3=="springbarley"||crop3=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("SWHT",tillage4,ncb4) 
      cy4 <- cb4[c3]
    }else if (crop4=="winterbarley"){
      if(crop3=="winterbarley"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      if(crop3=="springwheat"||crop3=="springbarley"||crop3=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("WBAR",tillage4,ncb4) 
      cy4 <- cb4[c3]
    }else if(crop4=="springbarley"){
      if(crop3=="springbarley"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      if(crop3=="springwheat"||crop3=="winterbarley"||crop3=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("SBAR",tillage4,ncb4) 
      cy4 <- cb4[c3]
    }else if(crop4=="winterbeans"){
      selfrot <- 0
      if(crop3=="wosr"||crop3=="sugarbeet"||crop3=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("WBEA",tillage4,ncb4) 
      cy4 <- cb4[c3]
    }else if(crop4=="springbeans"){
      selfrot <- 0
      if(crop3=="wosr"||crop3=="sugarbeet"||crop3=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("SBEA",tillage4,ncb4) 
      cy4 <- cb4[c3]
    }else if(crop4=="warepotatoes"){
      if(crop3=="warepoatoes"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Potatoes is penalised")
      }else{
        selfrot = 0
      }
      if(crop3=="sugarbeet"){ 
        rotpen <- 5
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("WPOT",tillage4,ncb4)  
      cy4 <- cb4[c3]
    }else if(crop4=="wosr"){
      if(crop3=="wosr"||crop3=="sosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot = 0
      }
      if(crop3=="winterbeans"||crop3=="springbeans"||crop3=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("WOSR",tillage4,ncb4)  
      cy4 <- cb4[c3]
    }else if(crop4=="sugarbeet"){
      if(crop3=="sugarbeat"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Sugarbeet is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop3=="winterbeans"||crop3=="springbeans"||crop3=="wosr"||crop3=="warepotatoes"||crop3=="sosr"){ 
        rotpen <- 100
        warning("Sugarbeet--Oilseed rape, Beans or Potatoes sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,h8)],cb4[c1]+cb4[c2],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("SBEE",tillage4,ncb4)  
      cy4 <- cb4[c3]
    }else if(crop4=="setaside"){ 
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot=0,rotpen=0,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("SETA",tillage4,ncb4)
      cy4 <- cb4[c3]
    }else if(crop4=="none"){
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot=0,rotpen=0,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("SETA",tillage4,ncb4)
      cy4 <- cb4[c3]
    }else if(crop4=="fallow"){
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot=0,rotpen=0,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("FALLOW",tillage4,ncb4)
      cy4 <- cb4[c3]
    }else if(crop4=="sosr"){
      if(crop3=="wosr"||crop3=="sosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot = 0
      }
      if(crop3=="winterbeans"||crop3=="springbeans"||crop3=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("SOSR",tillage4,ncb4) 
      cy4 <- cb4[c3]
    }else if(crop4=="winterlinseed"){
      if(crop3=="winterlinseed"||crop3=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop3=="winterbeans"||crop3=="springbeans"||crop3=="sugarbeet"||crop3=="driedpeas"||crop3=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Linseed sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("WLIN",tillage4,ncb4)
      cy4 <- cb4[c3]
    }else if(crop4=="springlinseed"){
      if(crop3=="winterlinseed"||crop3=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop3=="winterbeans"||crop3=="springbeans"||crop3=="sugarbeet"||crop3=="driedpeas"||crop3=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Linseed sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("SLIN",tillage4,ncb4)
      cy4 <- cb4[c3]
    }else if(crop4=="driedpeas"){
      if(crop3=="driedpeas"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Peas is penalised")
      }else{
        selfrot <- 0
      }
      if(crop3=="wosr"||crop3=="sosr"||crop3=="setaside"){ 
        rotpen <- 100
        warning("OSR and Rotational Set-aside--Peas sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb4 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage4,selfrot,rotpen,yieldoption)[[5]]
      ncb4 <- round(c(cb4[c8],cb4[seq(h6,c1)],cb4[seq(c9,c14)],cb4[c14]*rotp),di)
      Year4 <- c("DPEA",tillage4,ncb4)
      cy4 <- cb4[c3]
    }
    
    # Year 5  *************************************************************
    
    if(crop5=="winterwheat"){
      
      if(crop4=="winterwheat"){
        selfrot <- 15 # 15% Yield loss
      }else{
        selfrot = 0
      }
      
      if(crop4=="winterbarley"||crop4=="springbarley"||crop4=="springwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("WWHT",tillage5,ncb5) 
      cy5 <- cb5[c3]
    }else if(crop5=="springwheat"){
      if(crop4=="springwheat"){
        selfrot <- 11 # 
      }else{
        selfrot = 0
      }
      
      if(crop4=="winterbarley"||crop4=="springbarley"||crop4=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("SWHT",tillage5,ncb5) 
      cy5 <- cb5[c3]
    }else if (crop5=="winterbarley"){
      if(crop4=="winterbarley"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      
      if(crop4=="springwheat"||crop4=="springbarley"||crop4=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("WBAR",tillage5,ncb5) 
      cy5 <- cb5[c3]
    }else if(crop5=="springbarley"){
      if(crop4=="springbarley"){
        selfrot <- 11 # 11% Yield loss
      }else{
        selfrot = 0
      }
      
      if(crop4=="springwheat"||crop4=="winterbarley"||crop4=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("SBAR",tillage5,ncb5) 
      cy5 <- cb5[c3]
    }else if(crop5=="winterbeans"){
      selfrot <- 0
      if(crop4=="wosr"||crop4=="sugarbeet"||crop4=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("WBEA",tillage5,ncb5) 
      cy5 <- cb5[c3]
    }else if(crop5=="springbeans"){
      selfrot <- 0
      if(crop4=="wosr"||crop4=="sugarbeet"||crop4=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("SBEA",tillage5,ncb5) 
      cy5 <- cb5[c3]
    }else if(crop5=="warepotatoes"){
      if(crop4=="warepoatoes"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Potatoes is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop4=="sugarbeet"){ 
        rotpen <- 5
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("WPOT",tillage5,ncb5)  
      cy5 <- cb5[c3]
    }else if(crop5=="wosr"){
      if(crop4=="wosr"||crop4=="sosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop4=="winterbeans"||crop4=="springbeans"||crop4=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("WOSR",tillage5,ncb5)  
      cy5 <- cb5[c3]
    }else if(crop5=="sugarbeet"){
      if(crop4=="sugarbeat"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Sugarbeet is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop4=="winterbeans"||crop4=="springbeans"||crop4=="wosr"||crop4=="warepotatoes"||crop4=="sosr"){ 
        rotpen <- 100
        warning("Oilseed rape, Beans or Potatoes--Sugarbeet sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,h8)],cb5[c1]+cb5[c2],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("SBEE",tillage5,ncb5)  
      cy5 <- cb5[c3]
    }else if(crop5=="setaside"){ 
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot=0,rotpen=0,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("SETA",tillage5,ncb5) 
      cy5 <- cb5[c3]
    }else if(crop5=="none"){
      ccb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot=0,rotpen=0,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("SETA",tillage5,ncb5)
      cy5 <- cb5[c3]
    }else if(crop5=="fallow"){
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot=0,rotpen=0,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("FALLOW",tillage5,ncb5)
      cy5 <- cb5[c3]
    }else if(crop5=="sosr"){
      if(crop4=="wosr"||crop4=="sosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot = 0
      }
      if(crop4=="winterbeans"||crop4=="springbeans"||crop4=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("SOSR",tillage5,ncb5)
      cy5 <- cb5[c3]
    }else if(crop5=="winterlinseed"){
      if(crop4=="winterlinseed"||crop4=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop4=="winterbeans"||crop4=="springbeans"||crop4=="sugarbeet"||crop4=="driedpeas"||crop4=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Linseed sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("WLIN",tillage5,ncb5)
      cy5 <- cb5[c3]
    }else if(crop5=="springlinseed"){
      if(crop4=="winterlinseed"||crop4=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop4=="winterbeans"||crop4=="springbeans"||crop4=="sugarbeet"||crop4=="driedpeas"||crop4=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Linseed sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("SLIN",tillage5,ncb5) 
      cy5 <- cb5[c3]
    }else if(crop5=="driedpeas"){
      if(crop4=="driedpeas"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Peas is penalised")
      }else{
        selfrot <- 0
      }
      if(crop4=="wosr"||crop4=="sosr"||crop4=="setaside"){ 
        rotpen <- 100
        warning("OSR and Rotational Set-aside--Peas sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb5 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage5,selfrot,rotpen,yieldoption)[[6]]
      ncb5 <- round(c(cb5[c8],cb5[seq(h6,c1)],cb5[seq(c9,c14)],cb5[c14]*rotp),di)
      Year5 <- c("DPEA",tillage5,ncb5)
      cy5 <- cb5[c3]
    }
    
    
    # Year 6  *************************************************************
    
    if(crop6=="winterwheat"){
      
      if(crop5=="winterwheat"){
        selfrot <- 17 # 17% Yield loss
      }else{
        selfrot = 0
      }
      if(crop5=="winterbarley"||crop5=="springbarley"||crop5=="springwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("WWHT",tillage6,ncb6) 
      cy6 <- cb6[c3]
    }else if(crop6=="springwheat"){
      if(crop5=="springwheat"){
        selfrot <- 12 # 12% Yield loss
      }else{
        selfrot = 0
      }
      
      if(crop5=="winterbarley"||crop5=="springbarley"||crop5=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("SWHT",tillage6,ncb6) 
      cy6 <- cb6[c3]
    }else if (crop6=="winterbarley"){
      if(crop5=="winterbarley"){
        selfrot <- 12 # 12% Yield loss
      }else{
        selfrot = 0
      }
      
      if(crop5=="springwheat"||crop5=="springbarley"||crop5=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("WBAR",tillage6,ncb6) 
      cy6 <- cb6[c3]
    }else if(crop6=="springbarley"){
      if(crop5=="springbarley"){
        selfrot <- 12 # 12% Yield loss
      }else{
        selfrot = 0
      }
      
      if(crop5=="springwheat"||crop5=="winterbarley"||crop5=="winterwheat"){
        rotpen <- 11
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("SBAR",tillage6,ncb6) 
      cy6 <- cb6[c3]
    }else if(crop6=="winterbeans"){
      selfrot <- 0
      if(crop5=="wosr"||crop5=="sugarbeet"||crop5=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("WBEA",tillage6,ncb6) 
      cy6 <- cb6[c3]
    }else if(crop6=="springbeans"){
      selfrot <- 0
      if(crop5=="wosr"||crop5=="sugarbeet"||crop5=="sosr"){
        rotpen <- 100
        warning("Oilseed rape or Sugarbeet--Beans sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("SBEA",tillage6,ncb6) 
      cy6 <- cb6[c3]
    }else if(crop6=="warepotatoes"){
      if(crop5=="warepoatoes"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Potatoes is penalised")
      }else{
        selfrot = 0
      }
      
      if(crop5=="sugarbeet"){ 
        rotpen <- 5
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("WPOT",tillage6,ncb6)  
      cy6 <- cb6[c3]
    }else if(crop6=="wosr"){
      if(crop5=="wosr"||crop5=="sosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot = 0
      }
      if(crop5=="winterbeans"||crop5=="springbeans"||crop5=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("WOSR",tillage6,ncb6)  
      cy6 <- cb6[c3]
    }else if(crop6=="sugarbeet"){
      if(crop5=="sugarbeet"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Sugarbeet is penalised")
      }else{
        selfrot = 0
      }
      if(crop5=="winterbeans"||crop5=="springbeans"||crop5=="wosr"||crop5=="warepotatoes"||crop5=="sosr"){ 
        rotpen <- 100
        warning("Oilseed rape, Beans or Potatoes--Sugarbeet sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,h8)],cb6[c1]+cb6[c2],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("SBEE",tillage6,ncb6)  
      cy6 <- cb6[c3]
    }else if(crop6=="setaside"){ 
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot=0,rotpen=0,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("SETA",tillage6,ncb6)
      cy6 <- cb6[c3]
    }else if(crop6=="none"){
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot=0,rotpen=0,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("SETA",tillage6,ncb6) # c(rep(0,14))
      cy6 <- cb6[c3]
    }else if(crop6=="fallow"){
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot=0,rotpen=0,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("FALLOW",tillage6,ncb6) 
      cy6 <- cb6[c3]
    }else if(crop6=="sosr"){
      if(crop5=="wosr"||crop5=="sosr"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Oilseed rape is penalised")
      }else{
        selfrot = 0
      }
      if(crop5=="winterbeans"||crop5=="springbeans"||crop5=="sugarbeet"){ 
        rotpen <- 100
        warning("Sugarbeet or Beans--Oilseed rape sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("SOSR",tillage6,ncb6)
      cy6 <- cb6[c3]
    }else if(crop6=="winterlinseed"){
      if(crop5=="winterlinseed"||crop5=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop5=="winterbeans"||crop5=="springbeans"||crop5=="sugarbeet"||crop5=="driedpeas"||crop5=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Linseed sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("WLIN",tillage6,ncb6) 
      cy6 <- cb6[c3]
    }else if(crop6=="springlinseed"){
      if(crop5=="winterlinseed"||crop5=="springlinseed"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Linseed is penalised")
      }else{
        selfrot <- 0
      }
      if(crop5=="winterbeans"||crop5=="springbeans"||crop5=="sugarbeet"||crop5=="driedpeas"||crop5=="warepotatoes"){ 
        rotpen <- 100
        warning("Sugarbeet, Potatoes or Beans/Peas--Linseed sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("SLIN",tillage6,ncb6) 
      cy6 <- cb6[c3]
    }else if(crop6=="driedpeas"){
      if(crop5=="driedpeas"){
        selfrot <- 100 # Not Allowed
        warning("Self Rotation of Peas is penalised")
      }else{
        selfrot <- 0
      }
      if(crop5=="wosr"||crop5=="sosr"||crop5=="setaside"){ 
        rotpen <- 100
        warning("OSR and  Rotational Set-aside--Peas sequences are penalised")
      }else{
        rotpen <- 0
      }
      if(yieldoption=="actual"){
        selfrot <- 0
        rotpen <- 0
      }else if(yieldoption=="estimate"){
        selfrot <- selfrot
        rotpen <- rotpen
      }
      cb6 <- cropBudget(crop=crops,soil, subsidy, blackgrass,tillage=tillage6,selfrot,rotpen,yieldoption)[[7]]
      ncb6 <- round(c(cb6[c8],cb6[seq(h6,c1)],cb6[seq(c9,c14)],cb6[c14]*rotp),di)
      Year6 <- c("DPEA",tillage6,ncb6)
      cy6 <- cb6[c3]
    }
    
    # Rotation lenth: Maximum 6 year rotation is assumed
    
    if(farm=="single"){
      
      d1 <- data.frame(Component,Unit,Year1,Year2,Year3,Year4,Year5,Year6)
      nrotl <- rotlength
      if(nrotl==2){
        d1[,seq(-5,-8)]
      }else if(nrotl==3){
        d1[,seq(-6,-8)]
      }else if(nrotl==4){
        d1[,seq(-7,-8)]
      }else if(nrotl==5){
        d1[,-8]
      }else if(nrotl==6){
        d1
      }
      
    }else if(farm=="multiple"){
      d2 <- c(cy1,ncb,cy2,ncb2,cy3,ncb3,cy4,ncb4,cy5,ncb5,cy6,ncb6)
      #d2
      if(nrotl==2){
        d3 <- d2[seq(-27,-78)]
        d4 <- c(d3, sum(d3[13],d3[26]))
      }else if(nrotl==3){
        d3 <- d2[seq(-40,-78)]
        d4 <- c(d3, sum(d3[13],d3[26],d3[39]))
      }else if(nrotl==4){
        d3 <- d2[seq(-53,-78)]
        d4 <- c(d3, sum(d3[13],d3[26],d3[39],d3[52]))
      }else if(nrotl==5){
        d3 <- d2[seq(-66,-78)]
        d4 <- c(d3, sum(d3[13],d3[26],d3[39],d3[52],d3[65]))
      }else if(nrotl==6){
        d3 <- d2
        d4 <- c(d3, sum(d3[13],d3[26],d3[39],d3[52],d3[65],d3[78]))
        
      }
    }
  }
  #************************
  if(farm=="single"){
    
    ecm <- ECOMOD(farm="single",default,soil,rotlength,rotprob,crops,tillages,seedrate,delsowing,Nfert,Pfert,Kfert,bgherbdose,
                  glyphosatedose,numberofsprays,subsidy,blackgrass,cropprice,cropyield,yieldoption,Nfertprice,
                  Pfertprice,Kfertprice,seedprice,herbprice,glyphosateprice,machsize,fuelprice,labourwage)
    
  }else if(farm=="multiple"&is.null(default)){
    fd <- farmdata
    
    est <- c("yield","output","fertcost","seedcost","herbcost","sundry","varcost","gmargin","fuelcost",
             "labcost","opcost","grossprof","rotgrossprof")
    
    cp1 <- "crop.1" #crops[1]
    cp2 <- "crop.2" #crops[2]
    cp3 <- "crop.3" #crops[3]
    cp4 <- "crop.4" #crops[4]
    cp5 <- "crop.5" #crops[5]
    cp6 <- "crop.6" #crops[6]
    
    v1 <- ".1"
    v2 <- ".2"
    v3 <- ".3"
    v4 <- ".4"
    v5 <- ".5"
    v6 <- ".6"    
    
    p1 <- paste(est,v1,sep="")
    p2 <- paste(est,v2,sep="")
    p3 <- paste(est,v3,sep="")
    p4 <- paste(est,v4,sep="")
    p5 <- paste(est,v5,sep="")
    p6 <- paste(est,v6,sep="")
    
    hd <- c(p1,p2,p3,p4,p5,p6)
    
    nrotl <- rotlength
    if(nrotl==2){
      hd1 <- hd[seq(-27,-78)]
    }else if(nrotl==3){
      hd1 <- hd[seq(-40,-78)]
    }else if(nrotl==4){
      hd1 <- hd[seq(-53,-78)]
    }else if(nrotl==5){
      hd1 <- hd[seq(-66,-78)]
    }else if(nrotl==6){
      hd1 <- hd
    }
    
    cp <- fd[,seq(5,10)]
    ti <- fd[,seq(11,16)]
    ds <- fd[,seq(23,28)]
    sb <- as.character(fd[,65])
    bg <- fd[,seq(66,71)]
    yo <- as.character(fd[,84])
    
    
    for(k in 1:ncol(cp)){
      if(class(cp[,k])=="factor"){cp[,k] <- as.character(cp[,k])}
    }
    
    for(k in 1:ncol(ti)){
      if(class(ti[,k])=="factor"){ti[,k] <- as.character(ti[,k])}
    }
    
    for(k in 1:ncol(ds)){
      if(class(ds[,k])=="factor"){ds[,k] <- as.character(ds[,k])}
    }
    
    
    for(k in 1:ncol(bg)){
      if(class(bg[,k])=="factor"){bg[,k] <- as.character(bg[,k])}
    }
    
    
    
    
    fms <- c()
    
    index <- c(seq(1, length(farmdata[,1])))
    
    for(i in index){
      fms <- c(fms,ECOMOD(farm="multiple",default,soil=fd[i,3],rotlength,rotprob,crops=c(cp[,1][i],cp[,2][i],cp[,3][i],cp[,4][i],cp[,5][i],cp[,6][i]),
                          tillage=c(ti[,1][i],ti[,2][i],ti[,3][i],ti[,4][i],ti[,5][i],ti[,6][i]),seedrate=as.numeric(fd[i,seq(17,22)]),
                          delsowing=c(ds[,1][i],ds[,2][i],ds[,3][i],ds[,4][i],ds[,5][i],ds[,6][i]),
                          Nfert=as.numeric(fd[i,seq(29,34)]),Pfert=as.numeric(fd[i,seq(35,40)]),Kfert=as.numeric(fd[i,seq(41,46)]),
                          bgherbdose=as.numeric(fd[i,seq(47,52)]), 
                          glyphosatedose=as.numeric(fd[i,seq(53,58)]),numberofsprays=as.numeric(fd[i,seq(59,64)]),subsidy=sb[i],
                          blackgrass=c(bg[,1][i],bg[,2][i],bg[,3][i],bg[,4][i],bg[,5][i],bg[,6][i]), 
                          cropprice=as.numeric(fd[i,seq(72,77)]),
                          cropyield=as.numeric(fd[i,seq(78,83)]),yieldoption=yo[i], #yieldoption, 
                          Nfertprice=as.numeric(fd[i,85]),Pfertprice=as.numeric(fd[i,86]),
                          Kfertprice=as.numeric(fd[i,87]),
                          seedprice=as.numeric(fd[i,seq(88,93)]),herbprice=as.numeric(fd[i,seq(94,99)]),glyphosateprice=as.numeric(fd[i,seq(100,105)]),
                          machsize=as.numeric(fd[i,seq(106,110)]),fuelprice=as.numeric(fd[i,111]),
                          labourwage=as.numeric(fd[i,112])))
      # subsidy==fd[i,70]
      # yieldoption==fd[i,89] or yo[i]
      #print(i)
      fms1 <- fms
      cols <- c(hd1,"totalrotgrossprof")
      ros <- as.character(index)
      
      mat <- suppressWarnings(matrix(fms1,ncol=length(cols),nrow=length(index),byrow=T,dimnames=list(ros,cols)))
      Field_no <- index
      field_name <- fd[,2]
      ecm <- data.frame(Field_no,field_name,fd[,seq(5,10)],mat)
      
      # NAME OF THE FILE CAN BE CHANGED TO WHICHEVER NAME DESIRED *********
      # e.g. "Farms_Fields_Economic_Outcomes.csv" as in Supplementary Information
      write.table(ecm,file=filename,row.names=FALSE,sep=",")
      
    }
  }else if(farm=="multiple"&default=="yes"){
    stop("RUNNING MODEL FOR MULTIPLE FIELDS CANNOT BE BASED ON DEFAULT VALUES: SET default to NULL")
  }
  
  ka <- ecm
} 

