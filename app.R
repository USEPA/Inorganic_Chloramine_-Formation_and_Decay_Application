#Required R packages for application to run
library(deSolve)
library(ggplot2)
library(reshape2)
library(scales)
library(shiny)
library(shinyBS)
library(shinythemes)

#SERVER FUNCTION SECTION BEGINS HERE########################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################

#SERVER FUNCTION GENERAL SECTION############################################################################################
############################################################################################################################

#Set parameter names for inputed conditions to pass to simulation function
sim_Names <- c("TOTCl_mgL", "TOTCl_gas_lbs", "TOTCl_liquid_lbs", "TOTCl_liquid_percent",
               "TOTNH_mgL", "TOTNH_gas_lbs", "TOTNH_liquid_lbs", "TOTNH_liquid_percent", "ratio", "Q",
               "Mono_mgL", "Di_mgL", "FreeNH_mgL",
               "Mono_mgL_boost", "Di_mgL_boost", "TOTNH_mgL_boost", "TOTCl_mgL_boost",
               "pH", "Alk", "T_C", "TOC", "fast", "slow", "time_input_m",
               "time_input_h", "time_input_d", "Method", "ChlorineAdd", "AmmoniaAdd", "time_scale")

#Define colorblind friendly palette
cb_palette <- c("#000000", "#CC79A7", "#E69F00", "#56B4E9", "#009E73",
                "#0072B2", "#D55E00", "#999999")

#Define basic theme used for all plots
mytheme <-  theme(
  panel.background = element_rect(fill = "white", colour = NA),
  panel.grid.major = element_line(colour = "grey70", size = 0.2),
  panel.grid.minor = element_line(colour = "grey85", size = 0.5),
  legend.background = element_blank(),
  legend.key = element_blank(),
  legend.title = element_blank(),
  legend.text = element_text(face = "bold", size = rel(1.25)),
  legend.position = "top",
  legend.direction = "horizontal",
  strip.background = element_blank(),
  strip.text = element_text(face = "bold", size = rel (1.5)),
  axis.ticks = element_line(colour = "black", size = 1),
  axis.line = element_line(colour = "black", size = 1, lineend = "square"),
  axis.text.x = element_text(colour = "black", size = 12),
  axis.text.y = element_text(colour = "black", size = 12),
  axis.title.x = element_text(size = 14),
  axis.title.y = element_text(size = 14))

#Define function that copies initial conditions from one simulation to another
update_IC <- function(session, from, to, input) {

  #Update initial conditions for slider inputs
  updateSliderInput(session, paste0(to, "_", "TOTCl_mgL"), value = input[[paste0(from, "_", "TOTCl_mgL")]])
  updateSliderInput(session, paste0(to, "_", "TOTNH_mgL"), value = input[[paste0(from, "_", "TOTNH_mgL")]])
  updateSliderInput(session, paste0(to, "_", "TOTCl_liquid_percent"), value = input[[paste0(from, "_", "TOTCl_liquid_percent")]])
  updateSliderInput(session, paste0(to, "_", "TOTNH_liquid_percent"), value = input[[paste0(from, "_", "TOTNH_liquid_percent")]])
  updateSliderInput(session, paste0(to, "_", "ratio"), value = input[[paste0(from, "_", "ratio")]])
  updateSliderInput(session, paste0(to, "_", "Mono_mgL"), value = input[[paste0(from, "_", "Mono_mgL")]])
  updateSliderInput(session, paste0(to, "_", "Di_mgL"), value = input[[paste0(from, "_", "Di_mgL")]])
  updateSliderInput(session, paste0(to, "_", "FreeNH_mgL"), value = input[[paste0(from, "_", "FreeNH_mgL")]])
  updateSliderInput(session, paste0(to, "_", "Mono_mgL_boost"), value = input[[paste0(from, "_", "Mono_mgL_boost")]])
  updateSliderInput(session, paste0(to, "_", "Di_mgL_boost"), value = input[[paste0(from, "_", "Di_mgL_boost")]])
  updateSliderInput(session, paste0(to, "_", "TOTNH_mgL_boost"), value = input[[paste0(from, "_", "TOTNH_mgL_boost")]])
  updateSliderInput(session, paste0(to, "_", "TOTCl_mgL_boost"), value = input[[paste0(from, "_", "TOTCl_mgL_boost")]])
  updateSliderInput(session, paste0(to, "_", "pH"), value = input[[paste0(from, "_", "pH")]])
  updateSliderInput(session, paste0(to, "_", "Alk"), value = input[[paste0(from, "_", "Alk")]])
  updateSliderInput(session, paste0(to, "_", "T_C"), value = input[[paste0(from, "_", "T_C")]])
  updateSliderInput(session, paste0(to, "_", "TOC"), value = input[[paste0(from, "_", "TOC")]])
  updateSliderInput(session, paste0(to, "_", "fast"), value = input[[paste0(from, "_", "fast")]])
  updateSliderInput(session, paste0(to, "_", "slow"), value = input[[paste0(from, "_", "slow")]])
  updateSliderInput(session, paste0(to, "_", "time_input_m"), value = input[[paste0(from, "_", "time_input_m")]])
  updateSliderInput(session, paste0(to, "_", "time_input_h"), value = input[[paste0(from, "_", "time_input_h")]])
  updateSliderInput(session, paste0(to, "_", "time_input_d"), value = input[[paste0(from, "_", "time_input_d")]])

  #Update initial conditions for numeric inputs
  updateNumericInput(session, paste0(to, "_", "TOTCl_gas_lbs"), value = input[[paste0(from, "_", "TOTCl_gas_lbs")]])
  updateNumericInput(session, paste0(to, "_", "TOTNH_gas_lbs"), value = input[[paste0(from, "_", "TOTNH_gas_lbs")]])
  updateNumericInput(session, paste0(to, "_", "TOTCl_liquid_lbs"), value = input[[paste0(from, "_", "TOTCl_liquid_lbs")]])
  updateNumericInput(session, paste0(to, "_", "TOTNH_liquid_lbs"), value = input[[paste0(from, "_", "TOTNH_liquid_lbs")]])
  updateNumericInput(session, paste0(to, "_", "Q"), value = input[[paste0(from, "_", "Q")]])

  #Update initial conditions for select inputs
  updateSelectInput(session, paste0(to, "_", "Method"), selected = input[[paste0(from, "_", "Method")]])
  updateSelectInput(session, paste0(to, "_", "ChlorineAdd"), selected = input[[paste0(from, "_", "ChlorineAdd")]])
  updateSelectInput(session, paste0(to, "_", "AmmoniaAdd"), selected = input[[paste0(from, "_", "AmmoniaAdd")]])
  updateSelectInput(session, paste0(to, "_", "time_scale"), selected = input[[paste0(from, "_", "time_scale")]])
}

#Define function to extract initial conditions for reporting back out as a summary table
initial_conditions <- function(TOTCl_mgL, TOTCl_gas_lbs, TOTCl_liquid_lbs, TOTCl_liquid_percent,
                               TOTNH_mgL, TOTNH_gas_lbs, TOTNH_liquid_lbs, TOTNH_liquid_percent, ratio, Q,
                               Mono_mgL, Di_mgL, FreeNH_mgL,
                               Mono_mgL_boost, Di_mgL_boost, TOTNH_mgL_boost, TOTCl_mgL_boost,
                               pH, Alk, T_C, TOC, fast, slow, time_input_m,
                               time_input_h, time_input_d, Method, ChlorineAdd, AmmoniaAdd, time_scale) {

  #Get initial conditions based on various possible input scenarios

  #Simultaneous addition
  if (Method == "simadd") {
    Mono_ic <- 0.00
    Di_ic <- 0.00
    if (ChlorineAdd == "conc") {
      TOTCl_ic <- TOTCl_mgL
    }
    if (ChlorineAdd == "gas") {
      TOTCl_ic <- TOTCl_gas_lbs/Q/8.34
    }
    if (ChlorineAdd == "liquid") {
      TOTCl_ic <- TOTCl_liquid_lbs * (TOTCl_liquid_percent/100)/Q/8.34
    }
    if (AmmoniaAdd == "conc") {
      TOTNH_ic <- TOTNH_mgL
    }
    if (AmmoniaAdd == "ratio") {
      TOTNH_ic <- TOTCl_ic/ratio
    }
    if (AmmoniaAdd == "gas") {
      TOTNH_ic <- TOTNH_gas_lbs/Q/8.34 * 14/17
    }
    if (AmmoniaAdd == "liquid") {
      TOTNH_ic <- TOTNH_liquid_lbs * (TOTNH_liquid_percent/100)/Q/8.34 * 14/17
    }
  }

  #Preformed chloramines
  if (Method == "preform") {
    TOTCl_ic <- 0.00
    TOTNH_ic <- FreeNH_mgL
    Mono_ic <- Mono_mgL
    Di_ic <- Di_mgL
  }

  #Booster chlorination
  if (Method == "boost") {
    TOTCl_ic <- TOTCl_mgL_boost
    TOTNH_ic <- TOTNH_mgL_boost
    Mono_ic <- Mono_mgL_boost
    Di_ic <- Di_mgL_boost
  }

  #Initial chlorine to nitrogen ratio calculations common to all scenarios
  ratio_ic_N <- (TOTCl_ic + Mono_ic + Di_ic)/(TOTNH_ic + Mono_ic*14/71 + Di_ic*14/(71*2))
  ratio_ic_NH <- round(ratio_ic_N*14/17, digits = 2)
  ratio_ic_N <- round(ratio_ic_N, digits = 2)

  #Round concentration values
  TOTCl_ic <- round(TOTCl_ic, digits = 2)
  TOTNH_ic <- round(TOTNH_ic, digits = 2)
  Mono_ic <- round(Mono_ic, digits = 2)
  Di_ic <- round(Di_ic, digits = 2)

  #Combine intial condition names and values in a table
  Value <- c(TOTCl_ic, TOTNH_ic, Mono_ic, Di_ic, ratio_ic_N, ratio_ic_NH, pH, Alk, T_C, TOC, fast, slow)
  Parameter <- c("Free Chlorine (mg Cl2/L) =",
                 "Free Ammonia (mg N/L) =",
                 "Monochloramine (mg Cl2/L) =",
                 "Dichloramine (mg Cl2/L) =",
                 "Chlorine:Nitrogen Ratio (X:1) =",
                 "Chlorine:Ammonia Ratio (X:1) =",
                 "pH =",
                 "Total Alkalinity (mg CaCO3/L) =",
                 "Temperature (Celsius) =",
                 "Total Organic Carbon (mg C/L) =",
                 "TOC Fast Site Fraction =",
                 "TOC Slow Site Fraction =")
  ic <- cbind(Parameter, Value)
}

#Define function to simulate chloramine formation and decay
simulate_chloramine <- function(TOTCl_mgL, TOTCl_gas_lbs, TOTCl_liquid_lbs, TOTCl_liquid_percent,
                                TOTNH_mgL, TOTNH_gas_lbs, TOTNH_liquid_lbs, TOTNH_liquid_percent, ratio, Q,
                                Mono_mgL, Di_mgL, FreeNH_mgL,
                                Mono_mgL_boost, Di_mgL_boost, TOTNH_mgL_boost, TOTCl_mgL_boost,
                                pH, Alk, T_C, TOC, fast, slow, time_input_m,
                                time_input_h, time_input_d, Method, ChlorineAdd, AmmoniaAdd, time_scale) {

  #Set initial concentrations based on various possible input scenarios

  #Simultaneous addition
  if (Method == "simadd") {
    Mono_ini <- 0
    Di_ini <- 0
    if (ChlorineAdd == "conc") {
      TOTCl_ini <- TOTCl_mgL/71000
    }
    if (ChlorineAdd == "gas") {
      TOTCl_ini <- TOTCl_gas_lbs/Q/8.34/71000
    }
    if (ChlorineAdd == "liquid") {
      TOTCl_ini <- TOTCl_liquid_lbs * (TOTCl_liquid_percent/100)/Q/8.34/71000
    }
    if (AmmoniaAdd == "conc") {
      TOTNH_ini <- TOTNH_mgL/14000
    }
    if (AmmoniaAdd == "ratio") {
      TOTNH_ini <- TOTCl_ini/(ratio*14/71)
    }
    if (AmmoniaAdd == "gas") {
      TOTNH_ini <- TOTNH_gas_lbs/Q/8.34/17000
    }
    if (AmmoniaAdd == "liquid") {
      TOTNH_ini <- TOTNH_liquid_lbs * (TOTNH_liquid_percent/100)/Q/8.34/17000
    }
  }

  #Preformed chloramines
  if (Method == "preform") {
    TOTCl_ini <- 0
    TOTNH_ini <- FreeNH_mgL/14000
    Mono_ini <- Mono_mgL/71000
    Di_ini <- Di_mgL/71000/2
  }

  #Booster chlorination
  if (Method == "boost") {
    TOTCl_ini <- TOTCl_mgL_boost/71000
    TOTNH_ini <- TOTNH_mgL_boost/14000
    Mono_ini <- Mono_mgL_boost/71000
    Di_ini <- Di_mgL_boost/71000/2
  }

  #Convert temperature from Celsius to Kelvin
  T_K <- T_C + 273.15

  #Convert TOC into fast and slow fractions (moles C/L)
  DOC1_ini <- TOC*fast/12000
  DOC2_ini <- TOC*slow/12000

  #Convert time into seconds and set time steps (creates more data points at short times)

  #Minutes to seconds conversion
  if (time_scale == "minutes") {
    time_sim_s <- time_input_m * 60
    time <- seq(from = 0, to = time_sim_s, by = 1)
  }

  #Hours to seconds conversion
  if (time_scale == "hours") {
    time_sim_s <- time_input_h * 60 * 60
    time_fast <- seq(from = 0, to = 1799, by = 1)
    time_mid <- seq(from = 1800, to = time_sim_s, by = 30)
    time <- c(time_fast, time_mid, time_sim_s)
  }

  #Days to seconds conversion
  if (time_scale == "days") {
    time_sim_s <- time_input_d * 60 * 60 * 24
    time_fast <- seq(from = 0, to = 1799, by = 1)
    time_mid <- seq(from = 1800, to = 86370, by = 30)
    time_slow <- seq(from = 86400, to = time_sim_s, by = 3600)
    time <- c(time_fast, time_mid, time_slow, time_sim_s)
  }

  #Calculate equilibrium constants for chloramine system adjusted for temperature
  KHOCl <- 10^(-(1.18e-4 * T_K^2 - 7.86e-2 * T_K + 20.5))  #10^-7.6
  KNH4 <- 10^(-(1.03e-4 * T_K^2 - 9.21e-2 * T_K + 27.6))   #10^-9.25
  KH2CO3 <- 10^(-(1.48e-4 * T_K^2 - 9.39e-2 * T_K + 21.2)) #10^-6.35
  KHCO3 <- 10^(-(1.19e-4 * T_K^2 - 7.99e-2 * T_K + 23.6))  #10^-10.33
  KW <- 10^(-(1.5e-4 * T_K^2 - 1.23e-1 * T_K + 37.3))      #10^-14

  #Calculate water species concentrations (moles/L)
  H <- 10^-pH
  OH <- KW/H

  #Calculate alpha values

  #Free chlorine system
  alpha0TOTCl <- 1/(1 + KHOCl/H)
  alpha1TOTCl <- 1/(1 + H/KHOCl)

  #Free ammonia system
  alpha0TOTNH <- 1/(1 + KNH4/H)
  alpha1TOTNH <- 1/(1 + H/KNH4)

  #Carbonate system
  alpha0TOTCO <- 1/(1 + KH2CO3/H + KH2CO3*KHCO3/H^2)
  alpha1TOTCO <- 1/(1 + H/KH2CO3 + KHCO3/H)
  alpha2TOTCO <- 1/(1 + H/KHCO3 + H^2/(KH2CO3*KHCO3))

  #Calculate total carbonate concentration (moles/L) based on user-entered pH and alkalinity
  TOTCO <- (Alk/50000 + H - OH)/(alpha1TOTCO + 2 * alpha2TOTCO)

  #Calculate carbonate species concentrations (moles/L)
  H2CO3 <- alpha0TOTCO*TOTCO
  HCO3 <- alpha1TOTCO*TOTCO
  CO3 <- alpha2TOTCO*TOTCO

  #Calculated rate constants (moles/L and seconds) adjusted for temperature
  k1 <- 6.6e8 * exp(-1510/T_K)                #4.2e6
  k2 <- 1.38e8 * exp(-8800/T_K)               #2.1e-5
  k3 <- 3.0e5 * exp(-2010/T_K)                #2.8e2
  k4 <- 6.5e-7
  k5H <- 1.05e7 * exp(-2169/T_K)              #6.9e3
  k5HCO3 <- 4.2e31 * exp(-22144/T_K)          #2.2e-1
  k5H2CO3 <- 8.19e6 * exp(-4026/T_K)          #1.1e1
  k5 <- k5H*H + k5HCO3*HCO3 + k5H2CO3*H2CO3
  k6 <- 6.0e4
  k7 <- 1.1e2
  k8 <- 2.8e4
  k9 <- 8.3e3
  k10 <- 1.5e-2
  k11p <- 3.28e9*OH + 6.0e6*CO3
  k11OCl <- 9e4
  k12 <- 5.56e10
  k13 <- 1.39e9
  k14 <- 2.31e2
  kDOC1 <- 5.4
  kDOC2 <- 180

  #Define function for chloramine system equations
  chloramine <- function(t, y, parms) {
    with(as.list(y), {

      dTOTNH <- (-k1*alpha0TOTCl*TOTCl*alpha1TOTNH*TOTNH + k2*NH2Cl + k5*NH2Cl^2 - k6*NHCl2*alpha1TOTNH*TOTNH*H + kDOC1*NH2Cl*DOC1)
      dTOTCl <- (-k1*alpha0TOTCl*TOTCl*alpha1TOTNH*TOTNH + k2*NH2Cl - k3*alpha0TOTCl*TOTCl*NH2Cl + k4*NHCl2 + k8*I*NHCl2 -
                   (k11p + k11OCl*alpha1TOTCl*TOTCl)*alpha0TOTCl*TOTCl*NHCl2 + 2*k12*NHCl2*NCl3*OH + k13*NH2Cl*NCl3*OH -
                   2*k14*NHCl2*alpha1TOTCl*TOTCl - kDOC2*alpha0TOTCl*TOTCl*DOC2)
      dNH2Cl <- (k1*alpha0TOTCl*TOTCl*alpha1TOTNH*TOTNH - k2*NH2Cl - k3*alpha0TOTCl*TOTCl*NH2Cl + k4*NHCl2 - 2*k5*NH2Cl^2 +
                   2*k6*NHCl2*alpha1TOTNH*TOTNH*H - k9*I*NH2Cl - k10*NH2Cl*NHCl2 - k13*NH2Cl*NCl3*OH - kDOC1*NH2Cl*DOC1)
      dNHCl2 <- (k3*alpha0TOTCl*TOTCl*NH2Cl - k4*NHCl2 + k5*NH2Cl^2 - k6*NHCl2*alpha1TOTNH*TOTNH*H - k7*NHCl2*OH - k8*I*NHCl2 -
                   k10*NH2Cl*NHCl2 - (k11p + k11OCl*alpha1TOTCl*TOTCl)*alpha0TOTCl*TOTCl*NHCl2 - k12*NHCl2*NCl3*OH -
                   k14*NHCl2*alpha1TOTCl*TOTCl)
      dNCl3 <- ((k11p + k11OCl*alpha1TOTCl*TOTCl)*alpha0TOTCl*TOTCl*NHCl2 - k12*NHCl2*NCl3*OH - k13*NH2Cl*NCl3*OH)
      dI <- (k7*NHCl2*OH - k8*I*NHCl2 - k9*I*NH2Cl)
      dDOC1 <- (-kDOC1*NH2Cl*DOC1)
      dDOC2 <- (-kDOC2*alpha0TOTCl*TOTCl*DOC2)
      list(c(dTOTNH, dTOTCl, dNH2Cl, dNHCl2, dNCl3, dI, dDOC1, dDOC2))
    })
  }

  #Set initial condition concentrations for simulation
  yini <- c(TOTNH = TOTNH_ini, TOTCl = TOTCl_ini, NH2Cl = Mono_ini, NHCl2 = Di_ini, NCl3 = 0,
            I = 0, DOC1 = DOC1_ini, DOC2 = DOC2_ini)

  #Solve chloramine ODE system
  sim <- ode(func = chloramine, parms = NULL, y = yini, times = time, atol = 1e-12, rtol = 1e-12)

  #Extract concentrations (moles/L) and convert to typical units (e.g., mg Cl2/L or mg N/L)
  sim_data <- as.data.frame(sim)
  sim_data$Total_Chlorine <- (sim_data$NH2Cl + sim_data$NHCl2*2 + sim_data$NCl3*3 + sim_data$TOTCl)*71000
  sim_data$Monochloramine <- sim_data$NH2Cl*71000
  sim_data$Dichloramine <- sim_data$NHCl2*71000*2
  sim_data$Trichloramine <- sim_data$NCl3*71000*3
  sim_data$Free_Chlorine <- sim_data$TOTCl*71000
  sim_data$Free_Ammonia <- sim_data$TOTNH*14000
  sim_data$Total_Ammonia_N <- (sim_data$TOTNH + sim_data$NH2Cl + sim_data$NHCl2 + sim_data$NCl3)*14000
  sim_data$Total_Ammonia_NH3 <- (sim_data$TOTNH + sim_data$NH2Cl + sim_data$NHCl2 + sim_data$NCl3)*17000
  sim_data$Cl2N <- sim_data$Total_Chlorine/sim_data$Total_Ammonia_N
  sim_data$Cl2NH3 <- sim_data$Total_Chlorine/sim_data$Total_Ammonia_NH3
  sim <- melt(sim_data, id.vars="time", variable.name="chemical", value.name="concentration")
}

#Define function to plot simulation results in various formats based on user selected tab
plot_sim <- function(sim, chem, type, time_scale) {

  #Extract concentrations to plot based on user selection
  sim_simple <- sim[sim$chemical %in% c("Total_Chlorine", "Monochloramine", "Dichloramine",
                                        "Trichloramine", "Free_Chlorine", "Free_Ammonia"),]
  sim_simple$chemical <- factor(sim_simple$chemical)
  levels(sim_simple$chemical) <- c("Total Chlorine", "Monochloramine", "Dichloramine",
                                   "Trichloramine", "Free Chlorine", "Free Ammonia")

  #Setup time units for plots based on user selection

  #Minutes for time scale
  if (time_scale == "minutes") {
    sim_simple$time <- sim_simple$time/60
    sim$time <- sim$time/60
    timelabel <- "Time(minutes)"
  }

  #Hours for time scale
  if (time_scale == "hours") {
    sim_simple$time <- sim_simple$time/60/60
    sim$time <- sim$time/60/60
    timelabel <- "Time(hours)"
  }

  #Days for time scale
  if (time_scale == "days") {
    sim_simple$time <- sim_simple$time/60/60/24
    sim$time <- sim$time/60/60/24
    timelabel <- "Time(days)"
  }

  #Select plot type based on user selected tab

  #Define and select plot of individual chemical concentrations versus time
  if (type == "ind") {
    plot <- ggplot(subset(sim_simple, chemical %in% chem), aes(x=time, y=concentration, colour=chemical)) +
      geom_line(size = 1) +
      xlab(timelabel) +
      aes(ymin=0) +
      ylab(expression(Chlorine~(mg~Cl[2]~L^-1)~and~Free~Ammonia~(mg~N~L^-1)~Concentrations)) +
      facet_wrap(~ chemical, ncol = 1, scales = "free_y") +
      annotate("segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf, size = 1) +
      scale_colour_manual(guide = FALSE, values = cb_palette) +
      mytheme
  }

  #Define and select chemical concentration composite plot versus time
  if (type == "comp") {
    plot <- ggplot(subset(sim_simple, chemical %in% chem), aes(x=time, y=concentration, colour=chemical)) +
      geom_line(size = 1) +
      xlab(timelabel) +
      aes(ymin=0) +
      ylab(expression(Chlorine~(mg~Cl[2]~L^-1)~and~Free~Ammonia~(mg~N~L^-1)~Concentrations)) +
      guides(colour = guide_legend(ncol = 2)) +
      scale_colour_manual (values = cb_palette) +
      mytheme
  }

  #Define and select chlorine to nitrogen mass ratio plot versus time
  if (type == "Cl2N") {
    plot <- ggplot(subset(sim, chemical %in% c("Cl2N", "Cl2NH3")), aes(x=time, y=concentration, colour=chemical)) +
      geom_line(size = 1) +
      xlab(timelabel) +
      aes(ymin=0) +
      ylab(expression(Chlorine~to~Nitrogen~(Cl[2]:N)~or~Chlorine~to~Ammonia~(Cl[2]:NH[3])~Mass~Ratio)) +
      scale_colour_manual(labels = c("Chlorine:Nitrogen", "Chlorine:Ammonia"), values = cb_palette) +
      guides(colour = guide_legend(nrow = 2)) +
      mytheme
  }

  return(plot)
}

#Define function to allow user to download simulation data
export_data <- function(sim) {

  #Extract concentrations to export in typical units and create data frame
  Time_minutes <- sim[sim$chemical == "Total_Chlorine", "time"]/60
  Time_hours <- sim[sim$chemical == "Total_Chlorine", "time"]/60/60
  Time_days <- sim[sim$chemical == "Total_Chlorine", "time"]/60/60/24
  Total_Chlorine_mg_Cl2_L <- sim[sim$chemical == "Total_Chlorine", "concentration"]
  Monochloramine_mg_Cl2_L <- sim[sim$chemical == "Monochloramine", "concentration"]
  Dichloramine_mg_Cl2_L <- sim[sim$chemical == "Dichloramine", "concentration"]
  Trichloramine_mg_Cl2_L <- sim[sim$chemical == "Trichloramine", "concentration"]
  Free_Chlorine_mg_Cl2_L <- sim[sim$chemical == "Free_Chlorine", "concentration"]
  Free_Ammonia_mg_N_L <- sim[sim$chemical == "Free_Ammonia", "concentration"]
  data <- data.frame(Time_minutes,
                     Time_hours,
                     Time_days,
                     Total_Chlorine_mg_Cl2_L,
                     Monochloramine_mg_Cl2_L,
                     Dichloramine_mg_Cl2_L,
                     Trichloramine_mg_Cl2_L,
                     Free_Chlorine_mg_Cl2_L,
                     Free_Ammonia_mg_N_L)
}

#SERVER FUNCTION DEFINITION SECTION#########################################################################################
############################################################################################################################

#Define server logic required to run simulations and produce output
server <- function(input, output, session) {

  # Set option for sticky sessions
  options("Set-Cookie" = paste0("JSESSIONID=", session$token))

  #Copy Simulation A's inputs to Simulation B's inputs
  observe({
    #Take a dependency on input$AtoBIC
    if(input$A_to_B_IC == 0) return(NULL)

    isolate(update_IC(session, "A", "B", input))
  })

  #Copy Simulation B's inputs to Simulation A's inputs
  observe({
    #Take a dependency on input$BtoAIC
    if(input$B_to_A_IC == 0) return(NULL)

    isolate(update_IC(session, "B", "A", input))
  })

  #Define function to get inputted initial conditions and states based on prefix provided
  sim_Params <- function(prefix) {
    params <- lapply(sim_Names, function(p) {
      input[[paste0(prefix, "_", p)]]
    })
  }

  #Run chloramine system simulation based on provided initial conditions
  simA <- reactive({
    #Take a dependency on input$simupdate
    if(input$simupdateA == 0) return(NULL)

    #Isolate simulation run only on input$simupdate button selection
    isolate(do.call(simulate_chloramine, sim_Params("A")))
  })

  simB <- reactive({
    #Take a dependency on input$simupdate
    if(input$simupdateB == 0) return(NULL)

    #Isolate simulation run only on input$simupdate button selection
    isolate(do.call(simulate_chloramine, sim_Params("B")))
  })

  #Get initial conditions summary table
  output$A_ic <- DT::renderDT({
    #Take a dependency on input$simupdate
    if(input$simupdateA == 0) return(NULL)

    #Isolate simulation run only on input$simupdate button selection
    isolate(DT::datatable(
      do.call(initial_conditions, sim_Params("A")),
      options = list(
        dom = "t",
        pageLength = 15,
        columnDefs = list(
          list(width = "90%", targets = 0),
          list(width = "10%", targets = 1),
          list(className = "alignRight", targets = c(0,1)),
          list(orderable = FALSE, targets = c(0,1)),
          list(searchable = FALSE, targets = c(0,1))
          )
        )
      ))
    },
    server = FALSE)

  output$B_ic <- DT::renderDT({
    #Take a dependency on input$simupdate
    if(input$simupdateB == 0) return(NULL)

    #Isolate simulation run only on input$simupdate button selection
    isolate(DT::datatable(
      do.call(initial_conditions, sim_Params("B")),
      options = list(
        dom = "t",
        pageLength = 15,
        columnDefs = list(
          list(width = "90%", targets = 0),
          list(width = "10%", targets = 1),
          list(className = "alignRight", targets = c(0,1)),
          list(orderable = FALSE, targets = c(0,1)),
          list(searchable = FALSE, targets = c(0,1))
          )
        )
      ))
    },
    server = FALSE)

  #Produce desired reactive plots
  output$A_ind <- renderPlot({
    #Take a dependency on input$plotupdate and input$simupdate
    if(input$simupdateA == 0) return(NULL)
    input$plotupdateA

    #Isolate plot to update only on input$plotupdate and input$simupdate selection
    isolate(plot_sim(simA(), input$A_chemicals, "ind", input$A_time_scale))
  })

  output$A_comp <- renderPlot({
    #Take a dependency on input$plotupdate and input$simupdate
    if(input$simupdateA == 0) return(NULL)
    input$plotupdateA

    #Isolate plot to update only on input$plotupdate and input$simupdate selection
    isolate(plot_sim(simA(), input$A_chemicals, "comp", input$A_time_scale))
  })

  output$A_Cl2N <- renderPlot({
    #Take a dependency on input$plotupdate and input$simupdate
    if(input$simupdateA == 0) return(NULL)
    input$plotupdateA

    #Isolate plot to update only on input$plotupdate and input$simupdate selection
    isolate(plot_sim(simA(), input$A_chemicals, "Cl2N", input$A_time_scale))
  })

  output$B_ind <- renderPlot({
    #Take a dependency on input$plotupdate and input$simupdate
    if(input$simupdateB == 0) return(NULL)
    input$plotupdateB

    #Isolate plot to update only on input$plotupdate and input$simupdate selection
    isolate(plot_sim(simB(), input$B_chemicals, "ind", input$B_time_scale))
  })

  output$B_comp <- renderPlot({
    #Take a dependency on input$plotupdate and input$simupdate
    if(input$simupdateB == 0) return(NULL)
    input$plotupdateB

    #Isolate plot to update only on input$plotupdate and input$simupdate selection
    isolate(plot_sim(simB(), input$B_chemicals, "comp", input$B_time_scale))
  })

  output$B_Cl2N <- renderPlot({
    #Take a dependency on input$plotupdate and input$simupdate
    if(input$simupdateB == 0) return(NULL)
    input$plotupdateB

    #Isolate plot to update only on input$plotupdate and input$simupdate selection
    isolate(plot_sim(simB(), input$B_chemicals, "Cl2N", input$B_time_scale))
  })

  #Expression that gets data to be downloaded at user's request
  output$A_downloadData <- downloadHandler(
    filename = function() {
      paste('A', '_', substr(as.character(Sys.time()),1,10),
            '_', substr(as.character(Sys.time()),12,16), '.csv', sep='')
    },
    content = function(file) {write.csv(export_data(simA()), file, row.names=TRUE)
    }
  )

  output$B_downloadData <- downloadHandler(
    filename = function() {
      paste('B', '_', substr(as.character(Sys.time()),1,10),
            '_', substr(as.character(Sys.time()),12,16), '.csv', sep='')
    },
    content = function(file) {
      write.csv(export_data(simB()), file, row.names=TRUE)
    }
  )

  #Detects when tabs have been pressed
  output$activeATab <- reactive({
    return(input$Atabs)
  })
  output$activeBTab <- reactive({
    return(input$Btabs)
  })

  outputOptions(output, 'activeATab', suspendWhenHidden=FALSE)
  outputOptions(output, 'activeBTab', suspendWhenHidden=FALSE)
}

#UI OBJECT SECTION BEGINS HERE##############################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################

#UI OBJECT GENERAL SECTION##################################################################################################
############################################################################################################################

#Define function to take inputs for initial conditions for simulations
render_data_inputs <- function(prefix, prefix2) {
  wellPanel(
    #Section title
    h4(paste0("Simulation ", prefix, " Inputs")),

    #Call to display initial simulation notification
    conditionalPanel(condition = paste0("input.simupdate", prefix, "== 0"),
                     tags$div("Note: An initial simulation has not been run; therefore, no plots have been generated", id = "initialsim")
    ),
    br(),
    h4("Initial Conditions"),

    fluidRow(
      column(6,
             #Create input and tooltip for method of chemical addition
             selectInput(paste0(prefix, "_", "Method"),
                         label = h4("Chemical Addition Scenarios"),
                         choices = c("Simultaneous Addition" = "simadd",
                                     "Preformed Chloramines" = "preform",
                                     "Booster Chlorination" = "boost"),
                         selected = "simadd",
                         selectize = TRUE),
             bsTooltip(id = paste0(prefix, "_", "Method"),
                       paste0("Select whether to simulate (1) Simultaneous Addition:  free chlorine and free ammonia are present ",
                              "simultaneously from either free chlorine addition to a water containing free ammonia or free ammonia ",
                              "addition to a water containing free chlorine and you wish to simulate chloramine formation and subsequent decay ",
                              "(e.g., drinking water treatment plant chloramine formation); ",
                              "(2) Preformed Chloramines: known concentrations of chloramines and free ammonia already exist and you wish ",
                              "to simulate chloramine decay (e.g., drinking water distribution system samples)",
                              "; or (3) Booster Chlorination: known concentrations of chloramines and free ammonia already exist and you wish ",
                              "to simulate adding free chlorine to recombine the free ammonia into chloramines and the subsequent decay ",
                              "(e.g., distribution system free chlorine addition)"
                              ),
                       "right",
                       options = list(container = "body")),

             #Only show panel if simultaneous addition is selected
             conditionalPanel(
               condition = paste0("input.", prefix, "_", "Method == 'simadd'"),

               #Create input and tooltip for free chlorine addition method
               selectInput(paste0(prefix, "_", "ChlorineAdd"),
                           label = h5("Free Chlorine Addition Method"),
                           choices = c("Known Concentration" = "conc",
                                       "Gas Feed" = "gas",
                                       "Liquid Feed" = "liquid"),
                           selected = "conc",
                           selectize = TRUE),
               bsTooltip(id = paste0(prefix, "_", "ChlorineAdd"),
                         "Select method of addition that sets free chlorine concentration at point of free ammonia addition",
                         "right",
                         options = list(container = "body")),

               #Only show panel if free chlorine concentration is selected
               conditionalPanel(
                 condition = paste0("input.", prefix, "_", "ChlorineAdd == 'conc'"),

                 #Create input and tooltip for free chlorine concentration addition
                 sliderInput(paste0(prefix, "_", "TOTCl_mgL"),
                             label = p(HTML("Free Chlorine Concentration (mg Cl<sub>2</sub>/L)"),
                                       style = "font-size: 12px"),
                             min = 0.00,
                             max = 15.00,
                             value = 4.00,
                             step = 0.05
                             ),
                 bsTooltip(id = paste0(prefix, "_", "TOTCl_mgL"),
                           "Set slider to known initial free chlorine concentration at point of free ammonia addition",
                           "right",
                           options = list(container = "body"))
                 ),

               #Only show panel if chlorine gas is selected
               conditionalPanel(
                 condition = paste0("input.", prefix, "_", "ChlorineAdd == 'gas'"),

                 #Create input and tooltip for free chlorine gas addition
                 numericInput(paste0(prefix, "_", "TOTCl_gas_lbs"),
                              label = p(HTML("Gas Dose (pounds/day)"),
                                        style = "font-size: 12px"),
                              min = 0,
                              max = 1e10,
                              value = 100,
                              step = 1),
                 bsTooltip(id = paste0(prefix, "_", "TOTCl_gas_lbs"),
                           "Enter known pounds/day addition of chlorine gas (Assumed 100% available chlorine)",
                           "right",
                           options = list(container = "body"))
                 ),

               #Only show panel if chlorine liquid is selected
               conditionalPanel(
                 condition = paste0("input.", prefix, "_", "ChlorineAdd == 'liquid'"),

                 #Create input and tooltip for free chlorine liquid addition
                 numericInput(paste0(prefix, "_", "TOTCl_liquid_lbs"),
                              label = p(HTML("Liquid Dose (pounds/day)"),
                                        style = "font-size: 12px"),
                              min = 0,
                              max = 1e10,
                              value = 100,
                              step = 1),
                 bsTooltip(id = paste0(prefix, "_", "TOTCl_liquid_lbs"),
                           "Enter known pounds/day addition of free chlorine solution",
                           "right",
                           options = list(container = "body")),

                 #Create input and tooltip for free chlorine liquid addition percent
                 sliderInput(paste0(prefix, "_", "TOTCl_liquid_percent"),
                             label = p(HTML("Liquid Strength (% available Cl<sub>2</sub>)"),
                                       style = "font-size: 12px"),
                             min = 0,
                             max = 100,
                             value = 50,
                             step = 1),
                 bsTooltip(id = paste0(prefix, "_", "TOTCl_liquid_percent"),
                           "Set slider to known strength of free chlorine solution (set to 100% if already taken into account in liquid dose)",
                           "right",
                           options = list(container = "body"))
                 ),

               #Create input and tooltip for free ammonia addition method
               selectInput(paste0(prefix, "_", "AmmoniaAdd"),
                           label = h5("Free Ammonia Addition Method"),
                           choices = c("Known Concentration" = "conc",
                                       "Chlorine to Nitrogen Ratio" = "ratio",
                                       "Gas Feed" = "gas",
                                       "Liquid Feed" = "liquid"),
                           selected = "conc",
                           selectize = TRUE),
               bsTooltip(id = paste0(prefix, "_", "AmmoniaAdd"),
                         "Select method of addition that sets free ammonia concentration at point of application to free chlorine",
                         "right",
                         options = list(container = "body")),

               #Only show panel if free ammonia concentration is selected
               conditionalPanel(
                 condition = paste0("input.", prefix, "_", "AmmoniaAdd == 'conc'"),

                 #Create input and tooltip for free ammonia concentration addition
                 sliderInput(paste0(prefix, "_", "TOTNH_mgL"),
                             label = p("Free Ammonia Concentration (mg N/L)",
                                       style = "font-size: 12px"),
                             min = 0.00,
                             max = 5.00,
                             value = 1.00,
                             step = 0.05),
                 bsTooltip(id = paste0(prefix, "_", "TOTNH_mgL"),
                           "Use slider to set free ammonia concentration at point of application to free chlorine",
                           "right",
                           options = list(container = "body"))
                 ),

               #Only show panel if chlorine to nitrogen mass ratio is selected
               conditionalPanel(
                 condition = paste0("input.", prefix, "_", "AmmoniaAdd == 'ratio'"),

                 #Create input and tooltip for chlorine to nitrogen mass ratio addition
                 sliderInput(paste0(prefix, "_", "ratio"),
                             label = p(HTML("Mass Ratio (Cl<sub>2</sub>:N)"),
                                       style = "font-size: 12px"),
                             min = 0.05,
                             max = 15.00,
                             value = 4.00,
                             step = 0.05),
                 bsTooltip(id = paste0(prefix, "_", "ratio"),
                           "Use slider to set target chlorine to nitrogen mass ratio which will be used to calculated the free ammonia concentration at point of application of free chlorine",
                           "right",
                           options = list(container = "body"))
                 ),

               #Only show panel if free ammonia gas feed is selected
               conditionalPanel(
                 condition = paste0("input.", prefix, "_", "AmmoniaAdd == 'gas'"),

                 #Create input and tooltip for free ammonia gas addition
                 numericInput(paste0(prefix, "_", "TOTNH_gas_lbs"),
                              label = p(HTML("Gas Dose (pounds/day)"),
                                        style = "font-size: 12px"),
                              min = 0,
                              max = 1e10,
                              value = 15,
                              step = 1),
                 bsTooltip(id = paste0(prefix, "_", "TOTNH_gas_lbs"),
                           "Enter known pounds/day addition of ammonia gas (Assumed 100% available ammonia)",
                           "right",
                           options = list(container = "body"))
                 ),

               #Only show panel if free ammonia liquid feed is selected
               conditionalPanel(
                 condition = paste0("input.", prefix, "_", "AmmoniaAdd == 'liquid'"),

                 #Create input and tooltip for free ammonia liquid addition
                 numericInput(paste0(prefix, "_", "TOTNH_liquid_lbs"),
                              label = p(HTML("Liquid Dose (pounds/day)"),
                                        style = "font-size: 12px"),
                              min = 0,
                              max = 1e10,
                              value = 15,
                              step = 1),
                 bsTooltip(id = paste0(prefix, "_", "TOTNH_liquid_lbs"),
                           "Enter known pounds/day addition of free ammonia solution",
                           "right",
                           options = list(container = "body")),

                 #Create input and tooltip for free ammonia liquid addition percent
                 sliderInput(paste0(prefix, "_", "TOTNH_liquid_percent"),
                             label = p(HTML("Liquid Strength (% available NH<sub>3</sub>)"),
                                       style = "font-size: 12px"),
                             min = 0,
                             max = 100,
                             value = 50,
                             step = 1),
                 bsTooltip(id = paste0(prefix, "_", "TOTNH_liquid_percent"),
                           "Set slider to known strength of free ammonia solution (set to 100% if already taken into account in liquid dose)",
                           "right",
                           options = list(container = "body"))
                 ),

               #Only show panel if free ammonia or free chlorine addition liquid or gas feed is selected
               conditionalPanel(
                 condition = paste0("input.", prefix, "_", "ChlorineAdd == 'gas' ||
                                      input.", prefix, "_", "ChlorineAdd == 'liquid' ||
                                      input.", prefix, "_", "AmmoniaAdd == 'gas' ||
                                      input.", prefix, "_", "AmmoniaAdd == 'liquid'"),

                 #Create input and tooltip for plant production
                 h5("Plant Information"),
                 numericInput(paste0(prefix, "_", "Q"),
                              label = p("Plant Production (MGD)",
                                        style = "font-size: 12px"),
                              min = 0.1,
                              max = 1e10,
                              value = 2.0,
                              step = 0.1),
                 bsTooltip(id = paste0(prefix, "_", "Q"),
                           "Enter known plant production in million gallons per day (only required when entering gas or liquid chemical additions)",
                           "right",
                           options = list(container = "body"))
                 )
               ),

             #Only show panel if preformed chloramines is selected
             conditionalPanel(
               condition = paste0("input.", prefix, "_", "Method == 'preform'"),

               #Section title
               h5("Known Chemical Concentrations"),

               #Create input and tooltip for known monochloramine concentration
               sliderInput(paste0(prefix, "_", "Mono_mgL"),
                           label = p(HTML("Monochloramine Concentration (mg Cl<sub>2</sub>/L)"),
                                     style = "font-size: 12px"),
                           min = 0.00,
                           max = 10.00,
                           value = 4.00,
                           step = 0.05),
               bsTooltip(id = paste0(prefix, "_", "Mono_mgL"),
                         "Set slider to known monochloramine concentration",
                         "right",
                         options = list(container = "body")),
               br(),

               #Create input and tooltip for known dichloramine concentration
               sliderInput(paste0(prefix, "_", "Di_mgL"),
                           label = p(HTML("Dichloramine Concentration (mg Cl<sub>2</sub>/L)"),
                                     style = "font-size: 12px"),
                           min = 0.00,
                           max = 10.00,
                           value = 0.00,
                           step = 0.05),
               bsTooltip(id = paste0(prefix, "_", "Di_mgL"),
                         "Set slider to known dichloramine concentration (this can be estimated by measuring total chlorine and subtracting monochloramine)",
                         "right",
                         options = list(container = "body")),
               br(),

               #Create input and tooltip for known free ammonia concentration
               sliderInput(paste0(prefix, "_", "FreeNH_mgL"),
                           label = p("Free Ammonia Concentration (mg N/L)",
                                     style = "font-size: 12px"),
                           min = 0.00,
                           max = 5.00,
                           value = 0.10,
                           step = 0.05),
               bsTooltip(id = paste0(prefix, "_", "FreeNH_mgL"),
                         "Set slider to known free ammonia concentration",
                         "right",
                         options = list(container = "body"))
               ),

             #Only show panel if booster chlorination is selected
             conditionalPanel(
               condition = paste0("input.", prefix, "_", "Method == 'boost'"),

               #Section title
               h5("Known Chemical Concentrations"),

               #Create input and tooltip for known monochloramine concentration
               sliderInput(paste0(prefix, "_", "Mono_mgL_boost"),
                           label = p(HTML("Monochloramine Concentration (mg Cl<sub>2</sub>/L)"),
                                     style = "font-size: 12px"),
                           min = 0.00,
                           max = 10.00,
                           value = 2.00,
                           step = 0.05),
               bsTooltip(id = paste0(prefix, "_", "Mono_mgL_boost"),
                         "Set slider to known monochloramine concentration",
                         "right",
                         options = list(container = "body")),
               br(),

               #Create input and tooltip for known dichloramine concentration
               sliderInput(paste0(prefix, "_", "Di_mgL_boost"),
                           label = p(HTML("Dichloramine Concentration (mg Cl<sub>2</sub>/L)"),
                                     style = "font-size: 12px"),
                           min = 0.00,
                           max = 10.00,
                           value = 0.00,
                           step = 0.05),
               bsTooltip(id = paste0(prefix, "_", "Di_mgL_boost"),
                         "Set slider to known dichloramine concentration (this can be estimated by measuring total chlorine and subtracting monochloramine)",
                         "right",
                         options = list(container = "body")),
               br(),

               #Create input and tooltip for known free ammonia concentration
               sliderInput(paste0(prefix, "_", "TOTNH_mgL_boost"),
                           label = p("Free Ammonia Concentration (mg N/L)",
                                     style = "font-size: 12px"),
                           min = 0.00,
                           max = 5.00,
                           value = 0.50,
                           step = 0.05),
               bsTooltip(id = paste0(prefix, "_", "TOTNH_mgL_boost"),
                         "Set slider to known free ammonia concentration",
                         "right",
                         options = list(container = "body")),

               #Section title
               h5("Booster Free Chlorine Addition"),

               #Create input and tooltip for known free chlorine concentration
               sliderInput(paste0(prefix, "_", "TOTCl_mgL_boost"),
                           label = p(HTML("Added Free Chlorine Concentration (mg Cl<sub>2</sub>/L)"),
                                     style = "font-size: 12px"),
                           min = 0.00,
                           max = 10.00,
                           value = 2.00,
                           step = 0.05),
               bsTooltip(id = paste0(prefix, "_", "TOTCl_mgL_boost"),
                         "Set slider to known added free chlorine concentration to recombine free ammonia into chloramines",
                         "right",
                         options = list(container = "body"))
               )
             ),
      column(6,
             #Section title
             h4("Water Quality"),

             #Create input and tooltip for known pH
             sliderInput(paste0(prefix, "_", "pH"),
                         label = p("pH",
                                   style = "font-size: 12px"),
                         min = 6.00,
                         max = 9.00,
                         value = 8.00,
                         step = 0.05),
             br(),
             bsTooltip(id = paste0(prefix, "_", "pH"),
                       "Set slider to known pH (held constant during simulations)",
                       "left",
                       options = list(container = "body")),

             #Create input and tooltip for known alkalinity
             sliderInput(paste0(prefix, "_", "Alk"),
                         label = p(HTML("Total Alkalinity (mg/L as CaCO<sub>3</sub>)"),
                                   style = "font-size: 12px"),
                         min = 0,
                         max = 500,
                         value = 150,
                         step = 5),
             br(),
             bsTooltip(id = paste0(prefix, "_", "Alk"),
                       "Set slider to known alkalinity as mg/L of calcium carbonate (held constant during simulations)",
                       "left",
                       options = list(container = "body")),

             #Create input and tooltip for known temperature
             sliderInput(paste0(prefix, "_", "T_C"),
                         label = p(HTML("Water Temperature (&deg;C)"),
                                   style = "font-size: 12px"),
                         min = 5.0,
                         max = 35.0,
                         value = 25.0,
                         step = 0.5),
             br(),
             bsTooltip(id = paste0(prefix, "_", "T_C"),
                       "Set slider to known water temperature in degrees Celsius (held constant during simulations)",
                       "left",
                       options = list(container = "body")),

             #Section title
             h4("Total Organic Carbon (TOC)"),

             #Create input and tooltip for known total organic carbon concentration
             sliderInput(paste0(prefix, "_", "TOC"),
                         label = p("TOC Concentration (mg C/L)",
                                   style = "font-size: 12px"),
                         min = 0.0,
                         max = 10.0,
                         value = 0.0,
                         step = 0.1),
             br(),
             bsTooltip(id = paste0(prefix, "_", "TOC"),
                       "Set slider to known total organic carbon concentration (set to zero to ignore organic reactions)",
                       "left",
                       options = list(container = "body")),

             #Create input and tooltip for known organic carbon fast reaction site fraction
             sliderInput(paste0(prefix, "_", "fast"),
                         label = p("TOC Fast Reactive Fraction",
                                   style = "font-size: 12px"),
                         min = 0.000,
                         max = 0.100,
                         value = 0.020,
                         step = 0.001),
             br(),
             bsTooltip(id = paste0(prefix, "_", "fast"),
                       "Set slider to known/assumed fraction of total organic carbon associated with the fast organic reaction",
                       "left",
                       options = list(container = "body")),

             #Create input and tooltip for known organic carbon slow reaction site fraction
             sliderInput(paste0(prefix, "_", "slow"),
                         label = p("TOC Slow Reactive Fraction",
                                   style = "font-size: 12px"),
                         min = 0.00,
                         max = 0.90,
                         value = 0.65,
                         step = 0.01),
             br(),
             bsTooltip(id = paste0(prefix, "_", "slow"),
                       "Set slider to known/assumed fraction of total organic carbon associated with the slow organic reaction",
                       "left",
                       options = list(container = "body")),

             #Create input and tooltip for time scale selection
             selectInput(paste0(prefix, "_", "time_scale"),
                         label = h4("Simulation Time Unit Selection"),
                         choices = c("Minutes" = "minutes",
                                     "Hours" = "hours",
                                     "Days" = "days"),
                         selected = "days",
                         selectize = TRUE),
             bsTooltip(id = paste0(prefix, "_", "time_scale"),
                       "Select desired time-scale for simulation (this also impacts x-axis on generated plots)",
                       "left",
                       options = list(container = "body")),

             #Only show panel if minutes are selected for time
             conditionalPanel(
               condition = paste0("input.", prefix, "_", "time_scale == 'minutes'"),

               #Create input and tooltip for time scale selection in minutes
               sliderInput(paste0(prefix, "_", "time_input_m"),
                           label = p("Simulation Time (minutes)",
                                     style = "font-size: 12px"),
                           min = 1,
                           max = 120,
                           value = 60,
                           step = 1),
               bsTooltip(id = paste0(prefix, "_", "time_input_m"),
                         "Select simulation length in minutes",
                         "left",
                         options = list(container = "body"))
               ),

             #Only show panel if hours are selected for time
             conditionalPanel(
               condition = paste0("input.", prefix, "_", "time_scale == 'hours'"),

               #Create input and tooltip for time scale selection in hours
               sliderInput(paste0(prefix, "_", "time_input_h"),
                           label = p("Simulation Time (hours)",
                                     style = "font-size: 12px"),
                           min = 2,
                           max = 48,
                           value = 24,
                           step = 1),
               bsTooltip(id = paste0(prefix, "_", "time_input_h"),
                         "Select simulation length in hours",
                         "left",
                         options = list(container = "body"))
               ),

             #Only show panel if days are selected for time
             conditionalPanel(
               condition = paste0("input.", prefix, "_", "time_scale == 'days'"),

               #Create input and tooltip for time scale selection in days
               sliderInput(paste0(prefix, "_", "time_input_d"),
                           label = p("Simulation Time (days)",
                                     style = "font-size: 12px"),
                           min = 2,
                           max = 60,
                           value = 7,
                           step = 1),
               bsTooltip(id = paste0(prefix, "_", "time_input_d"),
                         "Select simulation length in days",
                         "left",
                         options = list(container = "body"))
               ),

             #Call to display notification if simulations are on different time scales
             conditionalPanel(
               condition = "input.A_time_scale != input.B_time_scale",
               tags$div("Note: Simulation A and B have different time units currently selected", id = "timescale")
               )
             )
      ),

    #Create button and tooltip to copy initial conditions to the other simulation
    actionButton(paste0(prefix, "_to_", prefix2, "_IC"),
                 paste0("Copy Simulation ", prefix, "'s Inputs to Simulation ", prefix2, "'s Inputs"),
                 icon("copy")
    ),
    bsTooltip(id = paste0(prefix, "_to_", prefix2, "_IC"),
              "Press button to copy current simulation inputs to other simulation",
              "top",
              options = list(container = "body")
    ),
    br(),
    br(),

    #Create button and tooltip to update simulation
    actionButton(paste0("simupdate", prefix),
                 paste0("Update Simulation ", prefix, " and Plots (Press after Finished Changing Simulation Inputs)"),
                 icon("refresh")
    ),
    bsTooltip(id = paste0("simupdate", prefix),
              "Press button to update simulation and associated output plots using current input settings",
              "top",
              options = list(container = "body")
    ),
    br(),
    br(),

    #Create button and tooltip to download simulation data
    downloadButton(paste0(prefix, "_", "downloadData"),
                   paste0("Simulation ", prefix, " ", "Chemical Concentration Data Download (.csv file)")),
    bsTooltip(id = paste0(prefix, "_", "downloadData"),
              "Press button to download simulation data to a comma seperated variable (.csv) file for use in another program (e.g., Excel)",
              "bottom",
              options = list(container = "body"))
    )
}

#Define function to take inputs for plot generation
render_plot_inputs <- function(prefix) {
  wellPanel(
    fluidRow(
      column(6,
             #Section title
             h4(paste0("Simulation ", prefix, " Plot Preferences")),
             br(),

             #Create button and tooltip to update plots
             actionButton(paste0("plotupdate", prefix),
                          paste0("Update Plots for Simulation ", prefix),
                          icon("refresh")),
             bsTooltip(id = paste0("plotupdate", prefix),
                       "Press button to update output plots with current plot settings (does not rerun simulation)",
                       "bottom",
                       options = list(container = "body")),
             br(),
             br(),

             #Create input and tooltip to select which chemicals to display on plots
             checkboxGroupInput(paste0(prefix, "_", "chemicals"),
                                label = h4("Chemicals to Plot"),
                                choices = c("Total Chlorine",
                                            "Monochloramine",
                                            "Dichloramine",
                                            "Trichloramine",
                                            "Free Chlorine",
                                            "Free Ammonia"),
                                selected = c("Total Chlorine",
                                             "Monochloramine",
                                             "Dichloramine",
                                             "Trichloramine",
                                             "Free Chlorine",
                                             "Free Ammonia"),
                                inline = FALSE
                                ),
             bsTooltip(id = paste0(prefix, "_", "chemicals"),
                       "Use check boxes to select which chemicals to show in the output plots",
                       "right",
                       options = list(container = "body"))
             ),

      column(6,
             #Section title
             h4(paste0("Simulation ", prefix, " Initial Conditions")),

             #Call function to create summary table of initial conditions used in simulations
             DT::DTOutput(paste0(prefix, "_ic"))
             )
      )
    )
}

#Define function to generate plots
render_plot_outputs <- function(prefix) {
  wellPanel(
    #Section title
    h4(paste0("Simulation ", prefix, " Plots")),
    br(),

    #Create three panels
    tabsetPanel(
      id = paste0(prefix, "tabs"),
      type = "tabs",

      #Create panel for individual chemical plots
      tabPanel(
        title = h6("1. Individual Chemicals"),
        value = "ind",
        plotOutput(paste0(prefix, "_ind"), height = "1000px")
        ),

      #Create panel for composite chemical plot
      tabPanel(
        title = h6("2. Composite Chemicals"),
        value = "comp",
        plotOutput(paste0(prefix, "_comp"), height = "600px")),

      #Create panel for chlorine to nitrogen ratio plot
      tabPanel(
        title = h6("3. Chlorine to Nitrogen Ratios"),
        value = "Cl2N",
        plotOutput(paste0(prefix, "_Cl2N"), height = "600px"))
      ),

    #Create tooltip for panel selection
    bsTooltip(id = paste0(prefix, "tabs"),
              paste0("Select desired plot to show: ",
                     "(1) individual chemicals plotted in separate panels, ",
                     "(2) a composite view of chemicals on a single graph, or ",
                     "(3) chlorine to nitrogen mass ratios"),
              "top",
              options = list(container = "body"))
    )
}

#UI OBJECT DEFINITION SECTION#############################################################################################
############################################################################################################################

#Define UI layout
ui <- shinyUI(fluidPage(theme = shinytheme("flatly"),

  #Define header
  tags$head(tags$style(type = "text/css",

  #Define progress bar class
    "#loadmessage {
       position: fixed;
       width: 50%;
       top: 25%;
       right: 25%;
       text-align: center;
       font-weight: bold;
       font-size: 300%;
       color: black;
       padding: 10px;
       word-wrap: break-word;
       line-height: 40px;
       border-style: solid;
       border-width: large;
       border-color: black;
       border-radius: 15px;
       background-color: #f5f5f5;
       opacity: 1;
       z-index: 105;}",

  #Define different simulation timescale warning class
    "#timescale {
       width: 90%;
       top: 0%;
       left: 5%;
       text-align: center;
       font-weight: bold;
       font-size: 12px;
       color: black;
       padding: 7.5px;
       word-wrap: break-word;
       line-height: 15px;
       border-style: solid;
       border-width: large;
       border-color: black;
       border-radius: 15px;
       background-color: Yellow;
       opacity: 1;
       z-index: 105;}",

  #Define initial simulation not run warning style
    "#initialsim {
       width: 90%;
       top: 0%;
       left: 5%;
       text-align: center;
       font-weight: bold;
       font-size: 12px;
       color: black;
       padding: 7.5px;
       word-wrap: break-word;
       line-height: 15px;
       border-style: solid;
       border-width: large;
       border-color: black;
       border-radius: 15px;
       background-color: Yellow;
       opacity: 1;
       z-index: 105;}",

  #Define style to bold free chlorine, monochloramine and free ammonia in chemicals to plot checkbox group
    "input[value = 'Total Chlorine'] + span,
      input[value = 'Monochloramine'] + span,
      input[value = 'Free Ammonia'] + span {
       font-weight: 900;}",

  #Define style for buttons
  ".btn {
       width: 100%;
       word-wrap: break-word;
       white-space: normal;}",

  #Define style for initial conditions table
    ".datatables .alignRight {
       color: #0072B2;
       text-align: right;
       font-weight: bold;
       font-size: 12px;}"
      )
  ),

####Added from EPA template####################################################################################################################################
  tags$body(class = "html wide-template"),
  tags$head(tags$link(rel = "stylesheet",
                      type = "text/css", href = "style.css")),

  # Header
  HTML("<header class='masthead clearfix' role='banner'>
       <img alt='' class='site-logo' src='https://www.epa.gov/sites/all/themes/epa/logo.png'>
       <div class='site-name-and-slogan'>
       <h1 class='site-name'><a href='https://www.epa.gov/' rel='home' title='Go to the home page'><span>US EPA</span></a></h1>
       <div class='site-slogan'>
       United States Environmental Protection Agency
       </div>
       </div>
       <div class='region-header'>
       <div class='block-epa-core-gsa-epa-search' id='block-epa-core-gsa-epa-search'>"),

  # Search Form
  #$form(action='https://search.epa.gov/epasearch/epasearch', class='epa-search', method='get',
  #      tags$label(class='element-hidden'),
  #      tags$input(autocomplete='off', class='form-text ui-autocomplete-input', id='search-box', name='querytext', placeholder='Search EPA.gov', value=''),
  #  tags$span( class='ui-helper-hidden-accessible', role='status'),
  #  tags$button(class='epa-search-button', id='search-button', title='Search', type='submit'),
  #  tags$input(name='areaname', type='hidden', value=''),
  #  tags$input(name='areacontacts', type='hidden', value=''),
  #  tags$input(name='areasearchurl', type='hidden', value=''),
  #  tags$input(name='typeofsearch', type='hidden', value='epa'),
  #  tags$input(name='result_template', type='hidden', value='2col.ftl')
  #),

  HTML("</div>
       </div>
       </header>
       <nav class='nav main-nav clearfix' role='navigation'>
       <div class='nav__inner'>
       <h2 class='element-invisible'>Main menu</h2>
       <ul class='menu' role='menu'>
       <li class='expanded active-trail menu-item' role='presentation'>
       <a class='active-trail menu-link' href='https://www.epa.gov/environmental-topics' role='menuitem' title='View links to the most popular pages for each of EPA&#8217s top environmental topics.'>Environmental Topics</a></li>
       <li class='menu-item' role='presentation'>
       <a class='menu-link' href='https://www.epa.gov/laws-regulations' role='menuitem' title='View links to regulatory information by topic and sector, and to top pages about environmental laws, regulations, policies, compliance, and enforcement.'>Laws &amp; Regulations</a></li>
       <li class='expanded menu-item' role='presentation'>
       <a class='menu-link' href='https://www.epa.gov/aboutepa' role='menuitem' title='Learn more about our mission, organization, and locations.'>About EPA</a></li>
       </ul>
       </div>
       </nav>
       <div class='mobile-nav' id='mobile-nav'>
       <div class='mobile-bar clearfix'>
       <label class='menu-button' for='mobile-nav-toggle'>Menu</label>
       </div><input checked id='mobile-nav-toggle' type='checkbox'>
       <div class='mobile-links element-hidden' id='mobile-links' style='height:2404px;'>
       <ul class='mobile-menu'>
       <li class='expanded menu-item'><a class='menu-link' href='https://www.epa.gov/environmental-topics' tabindex='-1' title='View links to the most popular pages for each of EPA&#8217s top environmental topics.'>Environmental Topics</a></li>
       <li class='menu-item'><a class='menu-link' href='https://www.epa.gov/laws-regulations' tabindex='-1' title='View links to regulatory information by topic and sector, and to top pages about environmental laws, regulations, policies, compliance, and enforcement.'>Laws &amp; Regulations</a></li>
       <li class='expanded menu-item'><a class='menu-link' href='https://www.epa.gov/aboutepa' tabindex='-1' title='Learn more about our mission, organization, and locations.'>About EPA</a></li>
       </ul>
       </div>
       </div>
       <section class='main-content clearfix' id='main-content' lang='en' role='main' tabindex='-1'>
       <div class='region-preface clearfix'>
       <div class='block-views-revision-hublinks-block' id='block-views-revision-hublinks-block'>
       <div class='view view-revision-hublinks view-id-revision_hublinks'>
       <span class='related-info'><strong>Related Topics:</strong></span>
       <ul class='menu pipeline'>
       <li class='menu-item'><a href='https://www.epa.gov/environmental-topics'>Environmental Topics</a></li>
       </ul>
       </div>
       </div>
       <div class='block block-pane block-pane-epa-web-area-connect' id='block-pane-epa-web-area-connect'>
       <ul class='menu utility-menu'>
       <li class='menu-item'><a class='menu-link' href='https://www.epa.gov/water-research/forms/contact-us-about-water-research'>Contact Us</a></li>
       </ul>
       </div>
       </div>
       <div class='main-column clearfix'><!--googleon:all-->
       <h1  class='page-title'>Batch (Plug Flow) Reactor Simulation of Drinking Water Chloramine Formation and Decay</h1>
       <div class='panel-pane pane-node-content'>
       <div class='pane-content'>
       <div class='node node-page clearfix view-mode-full'>"),
####Added from EPA template####################################################################################################################################

  #Call to display progress bar
  conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                   tags$div("Update in progress...", id = "loadmessage")
                   ),

  #Application title block
  h4("Version 0.52, Last Updated February 16, 2016"),

  h4("Created by David G. Wahman (wahman.david@epa.gov), United States Environmental Protection Agency"),

  p("Chloramine kinetic model implementation from Jafvert & Valentine",
    a(target = "_blank", href="http://pubs.acs.org/doi/abs/10.1021/es00027a022", "(Environ. Sci. Technol., 1992, 26 (3), pp 577-586)"),
    "and Vikesland et al.",
    a(target = "_blank", href="http://www.sciencedirect.com/science/article/pii/S0043135400004061", "(Water Res., 2001, 35 (7), pp 1766-1776).")),
  p("Natural organic matter reaction implementation from Duirk et al.",
    a(target = "_blank", href="http://www.sciencedirect.com/science/article/pii/S0043135405003027", "(Water Res., 2005, 39 (14), pp 3418-3431),"),
    " using their average fast and slow organic reaction rate constants."),
  p("The provided application provides two side-by-side simulations (A and B) and associated graphs to allow comparison of input choices on chloramine formation and decay."),

  p("To open a manuscript describing the application in a new window, click on the following link: ",

    a(target = "_blank", href = "manual.pdf", "Application Documentation")

  ),

p("The application was developed by the United States Environmental Protection Agency (EPA). No warranty expressed or implied is made regarding the accuracy
    or utility of the system, nor shall the act of distribution constitute any such warranty. Any reference to specific commercial products, processes, or services by service mark,
  trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by
  EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity
  by EPA or the United States Government. This application has been reviewed in accordance with EPA policy
  and has been approved for external and free use. The views expressed in this application do not necessarily represent the views
  or policies of the Agency. Although a reasonable effort has been made to assure that the results obtained are correct,
  this application is experimental. Therefore, the author and the EPA are not responsible and assume no liability whatsoever
  for any results or any use made of the results obtained from this application, nor for any damages or litigation that result
  from the use of the application for any purpose."),

  hr(),

  #Layout for initial conditions, plot inputs, and plots
  fluidRow(
    column(6,
           render_data_inputs("A", "B"),
           hr(),
           render_plot_inputs("A"),
           render_plot_outputs("A")
    ),
    column(6,
           render_data_inputs("B", "A"),
           hr(),
           render_plot_inputs("B"),
           render_plot_outputs("B")
           )
    ),

####Additional required contact section########################################################################################################################
hr(),
p( a(href="https://www.epa.gov/water-research/forms/contact-us-about-water-research", "Contact Us"),
   " to ask a question, provide feedback, or report a problem."),

####Added from EPA template####################################################################################################################################
# Footer
HTML("</div>
     </div>
     </div>
     </div>
     </section>
     <footer class='main-footer clearfix' role='contentinfo'>
     <div class='main-footer__inner'>
     <div class='region-footer'>
     <div class='block-pane-epa-global-footer' id='block-pane-epa-global-footer'>
     <div class='row cols-3'>
     <div class='col size-1of3'>
     <div class='col__title'>
     Discover.
     </div>
     <ul class='menu'>
     <li><a href='https://www.epa.gov/accessibility'>Accessibility</a></li>
     <li><a href='https://www.epa.gov/aboutepa/administrator-gina-mccarthy'>EPA Administrator</a></li>
     <li><a href='https://www.epa.gov/planandbudget'>Budget &amp; Performance</a></li>
     <li><a href='https://www.epa.gov/contracts'>Contracting</a></li>
     <li><a href='https://www.epa.gov/home/grants-and-other-funding-opportunities'>Grants</a></li>
     <li><a href='https://19january2017snapshot.epa.gov'>January 19, 2017 Web Snapshot</a></li>
     <li><a href='https://www.epa.gov/ocr/whistleblower-protections-epa-and-how-they-relate-non-disclosure-agreements-signed-epa-employees'>No FEAR Act Data</a></li>
     <li><a href='https://www.epa.gov/privacy'>Privacy</a></li>
     </ul>
     </div>
     <div class='col size-1of3'>
     <div class='col__title'>
     Connect.
     </div>
     <ul class='menu'>
     <li><a href='https://www.data.gov/'>Data.gov</a></li>
     <li><a href='https://www.epa.gov/office-inspector-general/about-epas-office-inspector-general'>Inspector General</a></li>
     <li><a href='https://www.epa.gov/careers'>Jobs</a></li>
     <li><a href='https://www.epa.gov/newsroom'>Newsroom</a></li>
     <li><a href='https://www.epa.gov/open'>Open Government</a></li>
     <li><a href='https://www.regulations.gov/'>Regulations.gov</a></li>
     <li><a href='https://www.epa.gov/newsroom/email-subscriptions'>Subscribe</a></li>
     <li><a href='https://www.usa.gov/'>USA.gov</a></li>
     <li><a href='https://www.whitehouse.gov/'>White House</a></li>
     </ul>
     </div>
     <div class='col size-1of3'>
     <div class='col__title'>
     Ask.
     </div>
     <ul class='menu'>
     <li><a href='https://www.epa.gov/home/forms/contact-epa'>Contact Us</a></li>
     <li><a href='https://www.epa.gov/home/epa-hotlines'>Hotlines</a></li>
     <li><a href='https://www.epa.gov/foia'>FOIA Requests</a></li>
     <li><a href='https://www.epa.gov/home/frequent-questions-specific-epa-programstopics'>Frequent Questions</a></li>
     </ul>
     <div class='col__title'>
     Follow.
     </div>
     <ul class='social-menu'>
     <li><a class='menu-link social-facebook' href='https://www.facebook.com/EPA'>Facebook</a></li>
     <li><a class='menu-link social-twitter' href='https://twitter.com/epa'>Twitter</a></li>
     <li><a class='menu-link social-youtube' href='https://www.youtube.com/user/USEPAgov'>YouTube</a></li>
     <li><a class='menu-link social-flickr' href='https://www.flickr.com/photos/usepagov'>Flickr</a></li>
     <li><a class='menu-link social-instagram' href='https://www.instagram.com/epagov'>Instagram</a></li>
     </ul>
     <p class='last-updated'>Last updated on March 20, 2019</p>
     </div>
     </div>
     </div>
     </div>
     </div>
     </footer>")
####Added from EPA template####################################################################################################################################

)
)

#APPLICATION FUNCTION CALL DEFINITION#######################################################################################
############################################################################################################################
shinyApp(ui = ui, server = server)