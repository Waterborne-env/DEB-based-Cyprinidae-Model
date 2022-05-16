; ==========================================================================================================================================
; ==========================================================================================================================================
; ===== March 2022 - Code for the paper Accolla et al., 2022 - Modeling pesticide effects on multiple listed fish species. A case study of chlorothalonil effects on Cyprinidae
; Code simulating the population dynamicsd of four listed species of Cyprinidae (fish).
; Species life cycles are described by DEB equations
; Species are exposed to chlorothalonil and affected by drough events
; ==========================================================================================================================================


; ========================== DEFINITION OF PARAMETERS AND STATE VARIABLES ==================================================================

extensions [csv array]

globals[
  Param-HC
  ;U_E^0                    ; t L^2, initial reserves of the embryos at the start of the simulation
  kappa_X                   ; assimilation efficiency [-]
  L_0                       ; initial structural volume [cm]
  mu_E mu_X                 ; chemical potential of dry reserve and of food [J/mol]
  omega_E d_E               ; weight parameters for biomass: molar wet weight of reserve [g/mol], specific density of reserve [g/L^3]
  v_rate                    ; energy conductance [cm/t]
  kappa_R                   ; fraction reprod buffer fixed into eggs [-]
  k_J                       ; maturity maintenance rate coefficient [1/t]
  h_a s_G                   ; ageing parameters: Weibull ageing acceleration [1/t^2], Gompertz stress coefficient [-]. Ageing module has been implemented but not used
  zoom                      ; zoom factor [-]
  E_H^b E_H^j E_H^p         ; maturity at birth, metamorphosis, puberty [J]
  del_M                     ; shape parameter [-]
  F_m F_m_sc                ; surface-specific searching rate [m^3/s]; same parameter with the shape correction
  T_ref                     ; reference T [K]
  T_A                       ; Arrhenius temperature [K]
  p_m                       ; volume-specific maintenance rate [J/cm^3/t]
  E_G                       ; volume-specific cost of growth [J/cm^3]
  kap                       ; allocation fraction to soma [-]
  J_X_Am_int                ; surface-area-specific max ingestion rate [mol/cm^3/t]
  T_corr T_corr_embryo      ; temperature correction factor for organisms and for embryos [-]

  ; reproductive season
  reprod-start
  reprod-end
  year y-stoch              ; year count, y-stoch is used to check if the yearly time interval is equal to the interval between two years showing drought event
  exp-in exp-end            ; beginning and end of exposure
  day                       ; day count
  ; temperature and resource variables
  temp
  temper  temperK
  temperature-list
  temp-Arr-list T-Arr-list
  resource-list
  resource
  Z                         ; spring-summer length
  ; parameters for environmental stochasticiy (bad recruitment years)
  low-recruitment
 ; lower-limit-low-recruitment
 ; upper-limit-low-recruitment
  bad-year-list             ; list of time intervals between bad recruitment years
  bad-year                  ; time interval [year] to have a bad recruitment year
;  upper-limit-bad-year
;  lower-limit-bad-year


  Tsim                      ; total simulation time [years]
  ;; dead-counts
  dead-starvation
  dead-DD
  dead-bkg
  dead-lifespan
  food-multiplier           ; adjust food availability

  ;---------- GUTS IT
  exposure expo-list        ; read exposure from file
  year-exposure             ; years during which exposure occurs

  ; --- indirect effects - Weibull distribution
  SSD-list lambda-Weibull k-Weibull
  SSD Log-exp


  ; ---------------- GUTS parameters -----------------
  mw                        ; Median of the log-logistic distribution [µg/L]
  Fs                        ; Spread factor of the threshold distribution
  kd                        ; dominant rate contant [t-1]
  beta                      ; Shape parameter for the distribution of thresholds
  Cw                        ; water chemical concentration
  guts_deathcount           ; count dead
  ;guts_n

  ; ------------------------------------------------------------------ ;
  ; --------- TKTD egg parameters -----------------------------------
  kd-egg                   ; dominant rate contant - assumed to be equal to the GUTS-IT one
  stress-egg               ; decrease egg production
  c_T                      ; tolerance gradient of the TKTD model
  c_0                      ; threshold - assumed to be = to NOEC
  ; ------------------------------------------------------------------ ;

 ; ---------------- GUTS hatching parameters -----------------
  mw-h
  Fs-h
  kd-h
  beta-h
  guts_deathcount-h
  Cw-h
  guts_n-h
 ;------------------------------------------------------------------;
  w-turtles                ; sum weight turtles for DD
; ------------------------------------------------------------------------------------------------------------------------------------------

  X                        ; food quantity (density) [J/m^3]
  X-in                     ; initial food density if unlimited foodif food-dynamics = "unlimited"
  d_X                      ; change of food in time
  volume                   ; not really important here, because not spatially explicit [m^3]
  rX                       ; resource growth rate [1/t]
; parameters for defining food abundance
  X-max X-min
  limit ; minimum resource individual can "see"
  theta
]
; ------------------------------------------------------------------------------------------------------------------------------------------

; definition of parameters for the individuals:
; the notation follows the DEBtool-notation as far as possible
; deviation: rates are indicated with "_rate" rather than a dot
; each individual(turtle) in the model has the following parameters
turtles-own[
  ; - - - - - - - - - - - - - - - STATE VARIABLES - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  L           ; structural length [cm]
  dL          ; change of structural length in time
  U_H         ; scaled maturity [t cm^2]
  dU_H        ; change of scaled maturity in time
  U_E         ; scaled reserves [t cm^2]
  dU_E        ; change of scaled reserves in time
  e_scaled    ; scaled reserves per unit of structure [-]
  U_R         ; scaled energy in reproduction buffer (not standard DEB) [t cm^2]
  dU_R        ; change of energy in reproduction buffer (reproduction rate)
  stage       ; staged of th elife cycle
  age
  t_birth t_metam t_matur ; time at birth, metamorphosis and time at maturationto define shape correction
  day-birth year-birth    ; day and year of birth

  ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ; - - - - - - - - - - - - - - - FLUXES (used by several submodels) - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  S_A         ; assimilation flux [cm^2]
  S_C         ; mobilisation flux [cm^2]

  ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ; - - - - - - - - - - - - - - - EMBRYO (we use different state variable to not affect the state variable of the mother) - - - - - - - - --
  e_scaled_embryo ; [-]
  e_ref           ; [-]
  U_E_embryo      ; [t cm^2]
  S_C_embryo      ; [cm^2]
  U_H_embryo      ; [t cm^2]
  L_embryo        ; [cm]
  dU_E_embryo
  dU_H_embryo
  dL_embryo
  ; parameters used to calculate the costs for an egg / initial reserves
  lower-bound                          ; lower boundary for shooting method
  upper-bound                          ; upper boundary for shooting method
  estimation                           ; estimated value for the costs for an egg / initial reserve
  lay-egg?                             ; parameter needed to hand over if an egg can be laid
  sim                                  ; this keeps track of how many times the calc-egg-size loop is run

  egg-number                           ; count number of eggs per reproductive event
  egg-survival

  ; -----------  TKTD on eggs --------------------
  egg-number-stress                    ; number of eggs after exposure
  ; ----------------------------------------------

  cv  ; coefficient of variability among individuals

  ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ; - - - - - - - - - - - - - - - BASIC DEB PARAMETERS  - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  p_am_int p_am p_am_sc ; surface-specific maximum assimilation rate. Different parameters refer to changes due to individual variability and shape correction [J/cm^2/t]
  E_m_int E_m           ; maximum energy density, Different parameters refer to changes due to individual variability  [J/cm^3]
  pX_m_int pX_m pX_m_sc ; surface-specific maximum ingestion rate. Different parameters refer to changes due to individual variability and shape correction [J/cm^2/t]
  J_X_Am J_X_Am_sc      ; surface-area specific max ingestion rate express in mol [ mol/cm^2/t]
  U_H^b_int    U_H^b    ; t L^2, scaled maturity at birth
  U_H^j_int    U_H^j    ; t L^2, scaled maturity at metamorphosis
  U_H^p_int     U_H^p   ; t L^2, scaled maturity at puberty
  ; parameter that is used to randomize the input parameters
  scatter-multiplier
  shape_corr
  U_E^0 ; initial energy (embryo)

 ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 ;; ------------------------------- COMPOUND PARAMETERS
  g_int g              ; energy investment ratio. Different parameters refer to changes due to individual variability [-]
  k_M                  ; somatic maintenance rate coefficient [1/t]
  L_m_int L_m L_m_sc   ; maximum volumetric length. Different parameters refer to changes due to shape correction [cm]
  L_b L_j L_p          ; length at birth, metamorphosis, puberty [cm]
  v_rate_sc            ; energy conductance (velocity) with shape correction [cm/t]
  ; parameters to differentiate in the module for laying eggs. Parameters have not shape correction/individual variability if eggs are layed at the beginning,
  ; otherwise they have the mother shape-correction/individual variability factor
  v_bis
  g_bis
  U_H^b_bis

  ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ; - - - - - - - - - - - - - - - PREY DYNAMICS (only relevant if prey-dynamics are set to logistic) - - - - - - - - - - - - - - - - - - -

  ;J_XAm_rate  ; surface-area-specific maximum ingestion rate [# / (cm^2 t)]
  K_ind        ; (half) saturation coefficient [# / cm^2]
  f            ; scaled functional response [-]

  ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ; - - - - - - - - - - - - - - - AGEING -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  q_acceleration  ; ageing acceleration [-]
  dq_acceleration ; change of ageing acceleration in time
  h_rate          ; hazard rate [-]
  dh_rate         ; change of hazard rate in time
  Hazard

  ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ; - - - - - - - - - - - - - - - BACKGROUND MORTALITY-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  lifespan
  survival-rate
  mortality-rate
  juvenile-mortality-rate
  adult-mortality-rate
  coef-m

  ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ; - - - - - - - - - - - - - - - DENSITY-DEPENDENT MORTALITY-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  DD-survival                  ; survival rate linked to density dependence
  DailySurvival_Egg_Larva_0    ; survival rate without density dependence
  gamma_dd_egg_survival        ; coefficient RIcker function

  ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ; - - - - - - - - - - - - - - - REPRODUCTIVE PERIOD -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  spawning-day                 ; random day for spawning, individual-specific

  ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ; - - - - - - - - - - - - - - - GUTS PARAMETER / VARIABLES -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  guts_it_thresh               ; individual-specific threshold
  dDdt                         ; internal damage variation over time
  D-int                        ; internal damage
  guts_it_thresh-h             ; individual-specific threshold - GUTS on hatching
  D-int-h                      ; internal damage - GUTS on hatching
  dDdt-h                       ; internal damage variation over time - GUTS on hatching
]


; ==========================================================================================================================================
; ========================== SETUP PROCEDURE: SETTING INITIAL CONDITIONS ===================================================================
; ==========================================================================================================================================

to setup
  ca
  clear-all
  reset-ticks

;  random-seed seed
;  random-seed behaviorspace-run-number

  print behaviorspace-run-number
  set year 1
  set T_ref 293.15
  set food-multiplier 10 ^ 7

  ;; initial day is set up depending on the species-specific reproduction period
  if species = "Spikedace"
  [set day 90
   set exp-in 15;
]
  if species = "Humpback Chub"
  [set day 80
   set exp-in 25;
]
  if species = "DR Minnow"
  [set day 1
   set exp-in 20;
]
  if species = "Topeka Shiner"
  [set day 150
   set exp-in 15;
]

  ;; define exposure period and total simulation time
  set exp-end exp-in + 5
  set Tsim exp-end + 3; 50

  ; - - - - - - - - - - - - - - -   GUTS - IT - - - - - - - - - - - - - - -
; read parameters for the GUTS exposure sub-models and the food-reduction sub-model
  ifelse (file-exists? Chemical-Parameters); "Common-param.csv")
      [
        file-open Chemical-Parameters
        let xyz file-read ;
        set kd file-read                    ; dominant rate of the reduced model  [1/d]
        set xyz file-read ;
        set mw file-read                    ; median log-logistic distribution -- also called alpha  [ug/L]
        set xyz file-read
        set Fs file-read                    ; spread factor of the threshols ditribution [-]
        set xyz file-read
        set beta file-read ; shape parameter for the distribution of thresholds [-]
        set xyz file-read
        ; - - - - - - - - - - - - - - -   INDIRECT EFFECTS  - - - - - - - - - - - - - - -
        set lambda-Weibull file-read
        set xyz file-read
        set k-Weibull file-read
        file-close]
  [print "Parameter input file does not exist in current directory!"
    user-message "Chemical input file does not exist in current directory!"
    stop]

  ; - - - - - - - - - - - - - - - TKTD on eggs - - - - - - - - - - - - - - -
ifelse (file-exists? Chemical-Parameters-TKTD)
      [
        file-open Chemical-Parameters-TKTD
        let xyz file-read ;
        set kd-egg file-read
        print kd-egg
        set xyz file-read ;
        set c_T file-read
        set xyz file-read
        set c_0 file-read
        file-close]
  [print "Parameter input file does not exist in current directory!"
    user-message "Chemical input file does not exist in current directory!"
    stop]
  set stress-egg 0 ; if no effect on eggs

  ; - - - - - - - - - - - - - - -   GUTS - IT on hatching - - - - - - - - - - - - - - -
  ; ----------- mortality before hatching (embryo stage) due to chemical exposure. Hatching corresponds to DEB birth (exogenous feeding)
    ifelse (file-exists? Chemical-Parameters-Hatching)
      [
        file-open Chemical-Parameters-Hatching
        let xyz file-read ;
        set kd-h file-read
        set xyz file-read ;
        set mw-h file-read
        set xyz file-read
        set Fs-h file-read
        set xyz file-read
        set beta-h file-read ; shape parameter for the distribution of thresholds [-]
        file-close]
  [print "Parameter input file does not exist in current directory!"
    user-message "Chemical input file does not exist in current directory!"
    stop]

  ; - - - - - - -- - - read modeled hydroxychlorothalonil concentration - - - - - - - - - - - -
  readPestConc
  set year-exposure 1
  ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ;---------------------- Stochastic drought events ----------------------
  setup-bad-year
  set y-stoch year
  set bad-year item (year) bad-year-list
  ;---------------------------------------------------------------

 crt  50
 ask  turtles  [

    set stage "embryo"
    set t_birth 0
    set t_metam 0
    set t_matur 0
    set age 0

    ;------- GUTS --------
    set D-int 0
    set dDdt 0

    ; ---------- GUTS hatching -------
    set D-int-h 0
    set dDdt-h 0
    set cv 0.1
;    set h_rate 0
;    set Hazard 0
;    set q_acceleration 0

;; read species-specific parameters
     ifelse (file-exists? Species-parameters)
      [
        file-open Species-parameters ;
        let jj file-read
        set reprod-start file-read
        set jj file-read
        set reprod-end file-read
        set jj file-read
        set v_rate   file-read
        set jj file-read
        set kap      file-read
        set jj file-read
        set p_m   file-read
        set jj file-read
        set E_G   file-read
        set jj file-read
        set E_h^b file-read
        set jj file-read
        set E_h^j file-read
        set jj file-read
        set E_h^p  file-read
        set jj file-read
        set lifespan file-read;
        set jj file-read
        set del_M file-read
        set jj file-read
        set h_a file-read
        set jj file-read
        set s_G file-read
        set jj file-read
        set zoom file-read
        set jj file-read
        set egg-survival file-read
        set jj file-read
        set juvenile-mortality-rate file-read
        set jj file-read
        set adult-mortality-rate file-read
        set jj file-read
        set gamma_dd_egg_survival file-read;
        set jj file-read
        set k_j  file-read
        set jj file-read
        set kappa_R  file-read
        set jj file-read
        set kappa_X  file-read
        set jj file-read
        set L_0  file-read
        set jj file-read
        set mu_X  file-read
        set jj file-read
        set F_m  file-read
        set jj file-read
        set mu_E  file-read
        set jj file-read
        set d_E  file-read
        set jj file-read
        set omega_E file-read
        set jj file-read
        set T_A  file-read
        file-close
      ]
        [print "Parameter input file does not exist in current directory!"
    user-message "Parameter input file does not exist in current directory!"
      stop]

    set spawning-day (reprod-start + random (reprod-end - reprod-start))
    setup-temperature
    convert-parameters
    calc-embryo-reserve-investment
    individual-variability

    set K_ind J_X_Am * mu_X / F_m

; ----- T FROM FILE --------
    set T_corr item (day - 1) temp-Arr-list

    set DailySurvival_Egg_Larva_0  1    ; set to 1 because egg mortality is as in initial database without DD

; ----- setup individual thresholds for the GUTS model (if chemical concnetration > threshold --> individual dies) ------
    if chemical = "yes" and year = exp-in
    [
      with-local-randomness [
        let p round ((random-float 1) * 980)
        ifelse (p = 980)
        [ set guts_it_thresh item 979 GUTS-IT-thresholds
          if guts_it_thresh = 0
          [print "attention"]
        ]
        [ set guts_it_thresh item p GUTS-IT-thresholds

          if guts_it_thresh = 0
          [print "attention"
            print "p"
            print p
          ]
        ]
      ]
 ; ----- GUTS on hatching ------
       if (effects-on-hatching = "yes")
            [
      with-local-randomness [
        let p round ((random-float 1) * 980)
        ifelse (p = 980)
        [ set guts_it_thresh-h item 979 GUTS-IT-thresholds-h
          if guts_it_thresh-h = 0
          [print "attention"]
        ]
        [ set guts_it_thresh-h item p GUTS-IT-thresholds-h

          if guts_it_thresh-h = 0
          [print "attention"
            print "p"
            print p
          ]
        ]
      ]
      ]
    ]

  ]
  ; set initial food as multiple of maximum ingestion rate
  let pam_gen p_m * zoom / kap
  let JX_gen pam_gen / (kappa_X * mu_X)
  set X-max food-multiplier * (JX_gen * mu_X / F_m)
  set X-min X-max / 4
  set limit X-min / 2
  set theta 1

    if food-dynamics = "sinusoidal"
    [
      setup-resource
      set X item (day - 1) resource-list
      set volume 1
    ]
    ;
    if food-dynamics = "unlimited"
    [
      set X-in 6 * 10 ^ 5
      set X X-in
    ]

  set dead-starvation 0
  set dead-DD 0
  set dead-bkg 0
  set guts_deathcount 0
  set guts_deathcount-h 0
  set dead-lifespan 0
end
; ==========================================================================================================================================
; ========================== END SETUP PROCEDURE ===================================================================
; ==========================================================================================================================================


; ==========================================================================================================================================
; ========================== GO PROCEDURE: RUNNING THE MODEL ===============================================================================
; ==========================================================================================================================================

to go
  if year = Tsim [stop]
  if count turtles = 0 [stop]

  ifelse timestep > 1 ; DEB dynamics for DRM requires smaller timesteps to be accurate
  [
    if ticks mod timestep = 1
    [
      set day day + 1
      if day = 366
      [
        if forcing_variable = "yes"
        [
          if (year - y-stoch = bad-year) ; if the number of simulated year is equal to the interval for having a bad years
          [
            set low-recruitment (random (upper-limit-low-recruitment - lower-limit-low-recruitment) + lower-limit-low-recruitment) / 100 ; low recruitment can be between 11% and 19%
            let t-pool count turtles with [age < 1]
            set low-recruitment floor ( (1 - low-recruitment) * t-pool )
            ask n-of low-recruitment turtles with [age < 1] [die]
            set y-stoch year
            set bad-year item (year) bad-year-list   ; reset interval before having another bad year
          ]
        ]
        set year year + 1
        set day 1
        set dead-bkg 0
        set guts_deathcount 0
        set dead-DD 0
        set guts_deathcount-h 0
        if year >  exp-in and year < exp-end
        [
          if chemical = "yes"
          [
            set year-exposure year-exposure + 1 ; read following year in the field-exposure file
          ]
        ]
      ]
    ]
  ]

  [
    set day day + 1
    if day = 366
    [
      if forcing_variable = "yes"
      [

        if (year - y-stoch = bad-year) ; if the number of simulated year is equal to the interval for having a bad years
        [
          set low-recruitment (random (20 - 11) + 11) / 100 ; low recruitment can be between 11% and 19%
          let t-pool count turtles with [age < 1]
          set low-recruitment floor ( (1 - low-recruitment) * t-pool )
          ask n-of low-recruitment turtles with [age < 1] [die]
          set y-stoch year
          set bad-year item (year) bad-year-list           ; reset interval before having another bad year
        ]
      ]
      set year year + 1
      set day 1
      set dead-bkg 0
      set guts_deathcount 0
      set dead-DD 0
      set guts_deathcount-h 0
      if year >  exp-in and year < exp-end
      [
        if chemical = "yes"
        [
          set year-exposure year-exposure + 1 ; read following year in the field-exposure file
        ]
      ]
    ]
  ]

; ----- T FROM FILE --------
  set T_corr item (day - 1) temp-Arr-list  ; temperature correction for DEB fluxes

  ; ------------------------------ FOOD ---------------------------
  if food-dynamics = "sinusoidal"
  [
    calc-d_X
    set X X + d_X / timestep
    if X < 0
    [write "X PROBLEM"
      show X
      set X 0]
  ]
  if food-dynamics = "unlimited"
  [
    set X X-in
  ]
  if (density-dependence = "yes")
      [
        set w-turtles sum [weight] of (turtles with [U_H >= U_H^p])
  ]
  if chemical = "yes"
  [
            if indirect-effects = "yes" and year >= exp-in and year <= exp-end
            [
              set X X * (1 - (food-reduction))
            ]
  ]

  ask turtles
  [
    set e_scaled v_rate * U_E / L ^ 3
    if U_H >= U_H^p  ; if puberty has been reached, energy is allocated to reproduction and reproduction occurs if the conditions are met
    [
      set stage "adult"
    ]
    if U_H >= U_H^b  and   U_H < U_H^p
    [
      set stage "juvenile"
    ]

    ifelse timestep > 1
    [
      if ticks mod timestep = 1
      [
        if day = day-birth and year > year-birth
        [
          set age age + 1
        ]
      ]
    ]
    [
      if day = day-birth and year > year-birth
      [
        set age age + 1
      ]
    ]

    shape-correction
    calc-dU_E

    ifelse U_H >= U_H^p
    [
      calc-dU_R
      set dU_H 0
      if U_R > 0
      [
        set egg-number 0
          if day = spawning-day
          [
            ifelse timestep > 1
            [
              if ticks mod timestep = 0
              [
                calc-lay-eggs                             ; if there is enough energy to lay eggs, then calculate how many eggs are spawned
                if lay-egg? = 1
                [
                  calc-embryo-reserve-investment         ; calculate how much energy to invest in an embryo
                  lay-eggs                               ; produce offsprings
                ]
              ]
            ]
            [
              calc-lay-eggs                             ; if there is enough energy to lay eggs, then calculate how many eggs are spawned
              if lay-egg? = 1
              [
                calc-embryo-reserve-investment         ; calculate how much energy to invest in an embryo
                lay-eggs
              ]
            ]
          ]
      ]
    ]
    [
      calc-dU_H      ; individuals have not reached puberty
      set dU_R 0
    ]


    ifelse timestep > 1
    [
      if ticks mod timestep = 0
      [
        if (density-dependence = "yes" and stage = "embryo" and year > 1)
        [ DD ]
        if chemical = "yes"
        [
          if year >= exp-in and year <= exp-end
          [
            ifelse (stage = "embryo")
            [
              if (effects-on-hatching = "yes")
              [
                GUTS-IT-h
              ]
            ]
            [
              GUTS-IT
              ifelse effects-on-eggs = "yes"
              [
                TKTD-eggs
              ]
              [set stress-egg 0]
            ]
          ]
        ]
      ]
    ]
    [if (density-dependence = "yes" and stage = "embryo" and year > 1)
      [ DD ]
      if chemical = "yes"
        [
          if year >= exp-in and year <= exp-end
          [
            ifelse (stage = "embryo")
          [
            if (effects-on-hatching = "yes")
            [
              GUTS-IT-h
            ]
          ]
          [
            GUTS-IT
            ifelse effects-on-eggs = "yes"
            [
              TKTD-eggs
            ]
            [set stress-egg 0]
          ]
        ]
      ]
    ]

    calc-dL
;    calc-dq_acceleration
;    calc-dh_rate
    mortality
  ]

  update
  tick
  do-plots
  if count turtles = 0 [stop]
end
; ==========================================================================================================================================
; ========================== END GO PROCEDURE ===================================================================
; ==========================================================================================================================================


; ==========================================================================================================================================
; ========================== SUBMODELS =====================================================================================================
; ==========================================================================================================================================

to setup-temperature

  if (species = "Humpback Chub" ) or (species = "Spikedace" )
  [if not file-exists? "LCR_temperature.txt"
    [
      user-message "No file 'temperature' exists!"
      stop
    ]
    file-open "LCR_temperature.txt"
    set temp csv:from-file "LCR_temperature.txt"
    file-close
  ]

  if (species = "Topeka Shiner" )
  [ if not file-exists? "TS_temperature.txt"
    [
      user-message "No file 'temperature' exists!"
      stop
    ]
    file-open "TS_temperature.txt"
    set temp csv:from-file "TS_temperature.txt"
    file-close
  ]

  if (species = "DR Minnow" )
  [ if not file-exists? "DR_temperature.txt"
    [
      user-message "No file 'temperature' exists!"
      stop
    ]
    file-open "DR_temperature.txt"
    set temp csv:from-file "DR_temperature.txt"
    file-close
  ]


  set temperature-list []
  set temp-Arr-list []
  set T-Arr-list []
  let cnt 0
  while [cnt < length temp]
  [
    set temper item (cnt) temp
    let a array:from-list temper ; through array to have number from a list
    let b array:item a 0 ;
    set temperK  exp ((T_A / T_ref ) - ( T_A / (b + 273.15)))
    set temp-Arr-list lput (temperK) temp-Arr-list
    set cnt cnt + 1
  ]
  set T_corr item (day - 1) temp-Arr-list
end

to setup-resource
  set resource-list []
  let cnt-d 1
  while [cnt-d < 366]
  [
    ; sinusoidal functions adjusted to represent environmental fluctuations of food density
    if (species = "Topeka Shiner"  )
    [  set Z 365 / 4
      ifelse ( cnt-d <= Z )
      [set resource X-max - ((X-max - X-min) / 2) * ( 1 + ( sin ( (180 * (Z - cnt-d) / ( (365 / 2)  - Z)))) ^ theta) ] ;; spring/summer period
      [set resource X-max - ((X-max - X-min) / 2) * ( 1 - ( sin ( (180 * (cnt-d - Z) / ( (365 / 2)  - Z)))) ^ theta) ] ;; autumn/winter period
    ]

    if (species = "Humpback Chub" ) or (species = "Spikedace" )
    [   set Z (365 / 2)
      ifelse ( cnt-d <= Z )
      [set resource X-max - ((X-max - X-min) / 2) * ( 1 - ( sin ( (180  * (Z - cnt-d) / ( (365 )  - Z )) + 30 )) ^ theta) ] ;; spring/summer period +30
      [set resource X-max - ((X-max - X-min) / 2) * ( 1 + ( sin ( (180  * (cnt-d - Z) / ( (365)  - Z )) - 30 )) ^ theta) ] ;; autumn/winter period era 1/2 - sin
    ]

    if (species = "DR Minnow" )
    [   set Z 365 / 4
      let X-min-minnow X-max / 2
      ifelse ( cnt-d <= Z )
      [set resource X-max - ((X-max - X-min-minnow) / 2) * ( 1 - ( sin ( (180 * (Z - cnt-d) / ( (365 / 2)  - Z)) + 100)) ^ theta) ] ;; spring/summer period
      [set resource X-max - ((X-max - X-min-minnow) / 2) * ( 1 + ( sin ( (180 * (cnt-d - Z) / ( (365 / 2)  - Z)) - 100)) ^ theta) ] ;; autumn/winter period
    ]
    set resource-list lput (resource) resource-list
    set cnt-d cnt-d + 1
  ]
end

to setup-bad-year
  ; bad years occur randomly every 1 to 25 years
  set bad-year-list []
  let cnt 0
  while [cnt < Tsim]
  [
    let rnd-year random upper-limit-bad-year + lower-limit-bad-year
    set bad-year-list lput rnd-year bad-year-list
    set cnt cnt + 1
  ]
end

; ---------------- conversion of parameters: from add_my_pet to standard DEB parameters ----------------------------------------------------

to convert-parameters
  set p_am_int p_m * zoom / kap
  set U_H^b_int E_H^b / p_am_int
  set U_H^p_int E_H^p / p_am_int
  set U_H^j_int E_H^j / p_am_int
  set k_M p_m / E_G
  set g_int (E_G * v_rate / p_am_int) / kap

  set J_X_Am_int p_am_int / (kappa_X * mu_X)
  set E_m_int p_am_int / v_rate
 ; set pX_m_int  p_am_int / kappa_X
  set L_m_int kap * p_am_int / p_m
end

; ------------------------ INDIVIDUAL VARIABILITY ------------------------------------------------------------------------------------------
to individual-variability
  ; individuals vary in their DEB paramters on a normal distribution with a mean on the input paramater and a coefficent of variation equal to the cv
  ; set cv to 0 or scatter-multiplier = 1 for no variation
  set scatter-multiplier e ^ (random-normal 0 cv) ;;
  set J_X_Am   J_X_Am_int * scatter-multiplier
  set g g_int / scatter-multiplier
  set U_H^b U_H^b_int / scatter-multiplier ;
  set U_H^j U_H^j_int / scatter-multiplier ;
  set U_H^p U_H^p_int / scatter-multiplier ;
 ; set pX_m  pX_m_int * scatter-multiplier
  set L_m L_m_int * scatter-multiplier
  set p_am p_am_int * scatter-multiplier
  set E_m E_m_int * scatter-multiplier
end

to shape-correction
  if U_H < U_H^b
    [
      set shape_corr 1
      set v_rate_sc v_rate * shape_corr  ;
      set J_X_Am_sc   J_X_Am * shape_corr
      ; set pX_m_sc  pX_m * shape_corr
      set L_m_sc L_m * shape_corr
      set F_m_sc F_m * shape_corr
      set p_am_sc p_am  * shape_corr
  ]
  ;; accelerated growth (like V1 morph) until metamorphosis
  if U_H >= U_H^b and U_H < U_H^j
    [
      set t_birth t_birth + 1
      if  t_birth = 1
      [ set L_b L
        set day-birth day
        set year-birth year
      ]
      set shape_corr L / L_b
      set v_rate_sc v_rate * shape_corr  ;
      set J_X_Am_sc   J_X_Am * shape_corr ;
  ;  set pX_m_sc  pX_m * shape_corr ;
      set L_m_sc L_m * shape_corr
      set F_m_sc F_m * shape_corr
      set p_am_sc p_am  * shape_corr
  ]
  ;; back to isomorph, but amplified growth after metamorphosis
  if U_H >= U_H^j
    [ set t_metam t_metam + 1
      if t_metam = 1
      [set L_j L
      ]
      set shape_corr L_j / L_b
      set v_rate_sc v_rate * shape_corr  ;
      set J_X_Am_sc   J_X_Am * shape_corr
  ;  set pX_m_sc  pX_m * shape_corr
      set L_m_sc L_m * shape_corr
      set F_m_sc F_m * shape_corr
      set p_am_sc p_am  * shape_corr
  ]
end


; ----------------- RESERVE DYNAMICS -------------------------------------------------------------------------------------------------------
; change in reserves: determined by the difference between assimilation (S_A) and mobilization (S_C) fluxes
; when food-dynamics are constant f = the value of f_scaled set in the user interface
; if food is set to  "logistic" f depends on prey density and the half-saturation coefficient (K)
; for embryos f = 0 because they do not feed exogenously

to calc-dU_E
if U_H <= U_H^b
  [
      set f 0
      set S_A 0
  ]

  if U_H > U_H^b
  [
    if food-dynamics = "sinusoidal"
    [
        ifelse X <= limit
      [
        set f 0
        set S_A 0
      ]
      [
        set K_ind J_X_Am_sc * mu_X / F_m
        set f (X - limit) / (K_ind + (X - limit))
        set S_A  f * L ^ 2 * shape_corr
        let eaten-food ((S_A * p_am / kappa_X) * T_corr / volume) / timestep
        if eaten-food > X
        [
          set eaten-food (X )
          set S_A eaten-food * (kappa_X * volume * timestep) / (p_am * T_corr)
        ]

        set X X - eaten-food
        if ( X <= 0)
        [
          print " attention X < 0"
          print X
          set X 0]
      ]
    ]
    if food-dynamics = "unlimited"
    [
      set f 1
      set S_A  f * L ^ 2 * shape_corr
    ]
  ]

  set S_C L ^ 2 * (g * e_scaled / (g + e_scaled)) * (1 + (L * k_M / v_rate_sc))
  set S_C S_C * shape_corr
  set dU_E (S_A - S_C )
  set dU_E dU_E * T_corr
end

; ----------------- MATURITY AND REPRODUCTION  ---------------------------------------------------------------------------------------------
; change in maturity is calculated (for immature individuals only)

to calc-dU_H
  set dU_H ((1 - kap) * S_C - k_J * U_H)
  set dU_H dU_H * T_corr
end

; the following procedure calculates change in reprobuffer if mature
to calc-dU_R
  set dU_R  ((1 - kap) * S_C - k_J * U_H^p)
  set dU_R dU_R * T_corr
end


; ----------------- DYNAMICS OF STRUCTURAL LENGHT-------------------------------------------------------------------------------------------
; the following procedure calculates change in structural length, if growth in negative the individual does not have enough energy to pay somatic maintenance and the starvation submodel is run
; where growth is set to 0 and individuals divirt enough energy from development (for juveniles) or reprodution (for adults) to pay maintenance costs
to calc-dL
  set dL ((1 / 3) * ((v_rate * S_C /( g * L ^ 2 )) - k_M * L)) * T_corr ;
  let condition (v_rate_sc / (g * k_M)) ;
  if (U_H > U_H^b and e_scaled  <  L / (v_rate_sc / (g * k_M))) ;
  [
    if U_H >= U_H^b + 0.01
    [print "starv"]
    set dL 0
    ifelse  (U_H < U_H^p)
    [
      set dU_H ((1 - kap) * e_scaled * L ^ 2 * shape_corr - k_J * U_H - kap * L ^ 2 * shape_corr * (L / (v_rate_sc / (g * k_M)) - e_scaled)) * T_corr
      set dU_R 0

      if (dU_H < 0)
      [
;        print "die starvation"
        set dead-starvation dead-starvation + 1
        die
    ]]
    [set dU_H 0
      if U_R < 0 [
;          print "die starvation"
        set dead-starvation dead-starvation + 1
        die]
      set dU_R ((1 - kap) * e_scaled * L ^ 2 * shape_corr - k_J * U_H^p - kap * L ^ 2 * shape_corr * (L / (v_rate_sc / (g * k_M)) - e_scaled)) * T_corr
    ]
    set dU_E  (S_A - ( e_scaled * L ^ 2 * shape_corr)) * T_corr
  ]


  ;  ---------------  TEST  ---------------------
;  if (dL < 0 and U_H > U_H^b)
;  [
;    print "problem"
;    die
;  ]
 ;  ------------------------------------
end


to-report weight
  report  L ^ 3 + ( U_E * p_am + U_R * p_am ) * omega_E / ( d_E * mu_E) ;; shape-corr not needed here because it is E and E_R
  report weight
end


;---------- CHECK IF POSSIBLE TO LAY EGGS -------------------------------------------------------------------------------------------------
; in the following, individuals determine if they have enough energy in their repro buffer to reproduce by creating an embryo with initial reserves set to the energy
; currently in their repro buffer * kap_R (conversion efficiancy of  reprobuffer to embryo) if the individual has enough energy to produce an offspring which will reach
; maturity and have a reserve density greater than the mothers when it hatches "lay-egg?" is set to one which will trigger the reproduction procedures "calc-embryo-reserve-investment" and "lay-eggs"
to calc-lay-eggs
  set L_embryo  L_0
  set U_E_embryo U_R * kappa_R
  set U_H_embryo  0

  loop [
    set e_scaled_embryo v_rate * (U_E_embryo / L_embryo  ^ 3)
    set S_C_embryo L_embryo  ^ 2 * (g * e_scaled_embryo / (g + e_scaled_embryo)) * (1 + (L_embryo  / (g * (v_rate / ( g * k_M)))))

    set dU_E_embryo  ( -1 * S_C_embryo ) * T_corr
    set dU_H_embryo  ((1 - kap) * S_C_embryo - k_J * U_H_embryo ) * T_corr
    set dL_embryo  ((1 / 3) * (((v_rate /( g * L_embryo  ^ 2 )) * S_C_embryo) - k_M * L_embryo )) * T_corr

    set  U_E_embryo  U_E_embryo +  dU_E_embryo  / timestep2
    set  U_H_embryo  U_H_embryo  +  dU_H_embryo   / timestep2
    set  L_embryo    L_embryo  +  dL_embryo   / timestep2

    if U_H_embryo  > U_H^b and lay-egg? != 1 [
      set lay-egg? 1
      stop]
    if e_scaled_embryo < e_scaled [
      stop]
  ]
end


; ------------------------ INITIAL ENERGY --------------------------------------------------------------------------------------------------
; calculate the initial energy of the first individuals using a bisection method

to calc-embryo-reserve-investment
  set lower-bound 0
  ifelse ticks = 0
  [set upper-bound 100000
    set v_bis v_rate
    set g_bis g_int
    set U_H^b_bis U_H^b_int
  ]
  [set upper-bound U_R * kappa_R
    set v_bis v_rate ;
    set g_bis g
    set U_H^b_bis U_H^b
    ;; parameters are equal to those of the mother when reproduction happens
;  let g_int g ; g is g_int / scatter-multiplier
;  let U_H^b_int U_H^b ; U_H^b is U_H^b_int / scatter-multiplier
  ]
  set sim 0

  loop[
    set sim sim + 1
    set estimation .5 * (lower-bound + upper-bound)
    set L_embryo  L_0

    set U_E_embryo estimation
    set U_H_embryo  0
    set e_scaled_embryo v_bis * (U_E_embryo / L_embryo  ^ 3)

    ifelse ticks = 0[set e_ref 1][set e_ref e_scaled]  ; e_ref now determines which e_scaled_embryo to calculate: 1 for ticks = 0 (in the setup procedure), e_scaled otherwise

    while [U_H_embryo  < U_H^b_bis and e_scaled_embryo > e_ref ]
      [ set e_scaled_embryo v_bis * (U_E_embryo / L_embryo  ^ 3)
        set S_C_embryo L_embryo  ^ 2 * (g_bis * e_scaled_embryo / (g_bis + e_scaled_embryo)) * (1 + (L_embryo  / (g_bis * (v_bis / ( g_bis * k_M)))))

        set dU_E_embryo  ( -1 * S_C_embryo )
        set dU_E_embryo dU_E_embryo * T_corr

        set dU_H_embryo  ((1 - kap) * S_C_embryo - k_J * U_H_embryo  )
        set dU_H_embryo dU_H_embryo * T_corr

        set dL_embryo   ((1 / 3) * (((v_bis /( g_bis * L_embryo  ^ 2 )) * S_C_embryo) - k_M * L_embryo ))
        set dL_embryo dL_embryo * T_corr

        set  U_E_embryo  U_E_embryo +  dU_E_embryo / timestep2
        set  U_H_embryo   U_H_embryo  +  dU_H_embryo / timestep2
        set  L_embryo   L_embryo  +  dL_embryo / timestep2
    ]

    if e_scaled_embryo <  .05 +  e_ref and e_scaled_embryo > -.05 + e_ref and U_H_embryo  >= U_H^b_bis  [

      ifelse ticks = 0 ;
      [set U_E^0 estimation
        set L L_0
        set U_E U_E^0
        set e_scaled v_rate * (U_E / L ^ 3)
        set U_H 0
        set U_R 0
        stop
      ][
        set U_E^0 estimation
        set egg-number U_R * kappa_R / U_E^0 ;
        set egg-number-stress U_R * kappa_R * (1 - stress-egg) / U_E^0
      if year >= exp-in and year <= exp-end
  [
        ]

        set egg-number floor(egg-survival * egg-number-stress)
        stop]]

    ifelse U_H_embryo  > U_H^b_bis
      [
        set upper-bound estimation ]
      [
      set lower-bound estimation ]
    if sim > 200 [user-message ("Embryo submodel did not converge. Timestep may need to be smaller.")
     show sim
      stop
    ]
    ;if the timestep is too big relative to the speed of growth of species this will no converge
  ]

end

;--------- LAY EGGS ------------------------------------------------------------------------------------------------------------------------
; the following procedure is run for mature individuals which have enough energy to reproduce
; they create offsprings and give it the following state variables and DEB parameters
; the initial reserves is set to the value determined by the bisection method in "calc_egg_size"

to lay-eggs
  hatch  egg-number
  [

    individual-variability
    set stage "embryo"
    set K_ind J_X_Am * mu_X / F_m
    set L L_0
    set U_E estimation
    set e_scaled v_rate * (U_E / L ^ 3)
    set U_H 0
    set U_R 0
    ;set dU_R  0
    set dh_rate 0
    set dq_acceleration 0
    set lay-egg? 0
    set age 0
    set h_rate 0
    set Hazard 0
    set q_acceleration 0
    set spawning-day (reprod-start + random (reprod-end - reprod-start))

    set D-int 0
    set dDdt 0
    set D-int-h 0
    set dDdt-h 0
 ; ----- GUTS ------
    if chemical = "yes" and year >= exp-in - lifespan  and year <= exp-end
    [
      with-local-randomness [
        let p round ((random-float 1) * 980)
        ifelse (p = 980)
        [ set guts_it_thresh item 979 GUTS-IT-thresholds
          if guts_it_thresh = 0
          [print "attention"]
        ]
        [ set guts_it_thresh item p GUTS-IT-thresholds

          if guts_it_thresh = 0
          [print "attention"
            print "p"
            print p
          ]
        ]
      ]
      if (effects-on-hatching = "yes")
            [
              ; ----- GUTS on hatching ------
              with-local-randomness [
                let p round ((random-float 1) * 980)
                ifelse (p = 980)
                [ set guts_it_thresh-h item 979 GUTS-IT-thresholds-h
                  if guts_it_thresh-h = 0
                  [print "attention"]
                ]
                [ set guts_it_thresh-h item p GUTS-IT-thresholds-h

                  if guts_it_thresh-h = 0
                  [print "attention"
                    print "p"
                    print p
                  ]
                ]
              ]
      ]
    ]
  ]

  set lay-egg? 2 ;0
  set U_R U_R - U_R * kappa_R;
  if U_R < 0 ; to avoid calculation problems
  [set U_R 0
  print "problem calc U_R"
  ]
end


;--------- CANNIBALISM ON EGGS ------------------------------------------------------------------------------------------------------------------------

to DD
  ; from Schmolke et al 2019
  ; if egg/larva  mortality is density dependent, Ricker function is applied
  let area 1000
  set DD-survival DailySurvival_Egg_Larva_0 * exp ((-1) * gamma_dd_egg_survival *  (w-turtles) / area) ;
  let xr random-float 1.0
  if ((xr) > DD-survival)
  [
    set dead-DD dead-DD + 1
    die  ]
end


; ----------------- RESOURCE DYNAMICS ----------------------------------------------------------------------------------------------------------
to  calc-d_X
  set rX 1.5
  set d_X (rX * (item (day - 1) resource-list - X) )
end


; ------------------------------------------------------------------------------------------------------------------------------------------
; ----------------- AGEING -----------------------------------------------------------------------------------------------------------------
; --------- Not sure if needed, background mortality works better than DEB ageing module -  erase this part? -------------------------------
; the following procedure calculates the change in damage enducing compounds of an individual

;to calc-dq_acceleration
;  set dq_acceleration (q_acceleration * (L ^ 3 / (v_rate_sc / ( g * k_M)) ^ 3) * s_G + h_a) * e_scaled * (( v_rate_sc / L) - ((3 / L)*  dL)) - ((3 / L ) * dL) * q_acceleration ; yes shape correction because ....
;  set dq_acceleration dq_acceleration * T_corr
;end
;
;; the following procedure calculates the change in damage in the individual
;to calc-dh_rate
;  set dh_rate q_acceleration - ((3 / L) * dL) * h_rate
;  set dh_rate dh_rate * T_corr
;end


to mortality
  if age > lifespan
      [
        set dead-lifespan dead-lifespan + 1
        die
  ]
  if U_H > U_H^b and year > 3

  [
;---------- background mortality defined by DEB-ageing module -----------
;    set survival-rate e ^ (- Hazard)
;    set mortality-rate (1 - survival-rate) * coef-m

    ; --------- the following part is to separetely adjust juvenile and adult mortalities.
        if stage = "juvenile"
        [
          ifelse timestep > 1
          [
            let adjustment (1 - juvenile-mortality-rate) ^ (1 / timestep) ; adjust survival rate depending on the timestep. Rates on input file are daily mortality rates
            set mortality-rate 1 - adjustment]
          [ set mortality-rate juvenile-mortality-rate ]
        ]
          if stage = "adult"
          [
            ifelse timestep > 1
            [
              let adjustment (1 - adult-mortality-rate) ^ (1 / timestep ) ; adjust survival rate depending on the timestep. Rates on input file are daily mortality rates
              set mortality-rate 1 - adjustment]
            [set mortality-rate adult-mortality-rate ]
          ]
    ; ---------
    let xx random-float 1

    if xx < mortality-rate [
      set dead-bkg dead-bkg + 1
      die
    ]
  ]
end


to readPestConc
  if (file-exists? ExposureFile = FALSE)
  [user-message "Wraning: exposure file does not exist in current directory!"]
  set expo-list csv:from-file ExposureFile
end

to-report chemical-concentration
  ; values are column then row
  ; (indices start at 0 but "row 0" is the column headings)
  set exposure item year-exposure item day expo-list
  set exposure exposure * MF
  report exposure
end

to-report food-reduction
  let expon (chemical-concentration / lambda-Weibull) ^ k-Weibull
  ifelse chemical-concentration = 0
    [ set SSD 0 ]
  [set SSD (1 - exp (- expon)) ]
  report SSD
end


; ----------------- GUTS-IT  from  ; from Schmolke et al 2019 ------------------------------------------------------------------------------
to-report GUTS-IT-thresholds
  ; Procedure calculates the concentration thresholds for GUTS-IT
  ; A list with 1000 concentrations is calculated, each concentration level corresponds to 0.001 probability of individual threshold
  ; the list may include some concentrations multiple times, reflecting higher probability of the corresponding internal concentration to be an individual's threshold for death
  let tList []
  let prob 0.001
  let conc 1.4  ; 1.4 micro g/L is the NOEC
  let i 0
  set tList lput 1.4 tList
  let thresh 0
  while [ i < 979 ] [
    while [ (thresh + 0.0000001) < prob ] [
      set thresh (GUTS_IT_threshold_function conc mw beta)
      set conc conc + 0.01
      set conc (precision conc 2)
    ]
    set tList lput (precision conc 2) tList
    set prob prob + 0.001
    set i i + 1
  ]
  report tList
end

to-report GUTS-IT-thresholds-h
  ; Procedure calculates the concentration thresholds for GUTS-IT
  ; A list with 1000 concentrations is calculated, each concentration level corresponds to 0.001 probability of individual threshold
  ; the list may include some concentrations multiple times, reflecting higher probability of the corresponding internal concentration to be an individual's threshold for death
  let tList-h []
  let prob-h 0.001
  let conc-h 1.4  ; 1.4 micro g/L is the NOEC
  let i 0
  set tList-h lput 1.4 tList-h
  let thresh-h 0
  while [ i < 979 ] [
    while [ (thresh-h + 0.0000001) < prob-h ] [
      set thresh-h (GUTS_IT_threshold_function conc-h mw-h beta-h)
      set conc-h conc-h + 0.01
      set conc-h (precision conc-h 2)
    ]
    set tList-h lput (precision conc-h 2) tList-h
    set prob-h prob-h + 0.001
    set i i + 1
  ]
  report tList-h
end


to-report GUTS_IT_threshold_function [ c mwf betaf ]
  let term1 c / mwf
  ifelse (term1 < 0.0001)
  [ report 0 ]
  [ report (precision (1 / (1 + (term1 ^ ((-1) * betaf)))) 7) ]
end

to GUTS-IT ; scaled GUTS-IT implemented to act on individual internal concentration thresholds
set Cw chemical-concentration ; water conc  [microg/L]
  set dDdt 0 ; damage variation over time
  repeat guts_n  [
    if (effects-on-survival = "yes")
      [
    		if (D-int >= guts_it_thresh)
        [
          set guts_deathcount guts_deathcount + 1
        	die	
      ]
    ]		
    		set dDdt kd * (Cw - D-int)
    		set D-int D-int + (1 / guts_n) * dDdt
  	]
end


to GUTS-IT-h ; scaled GUTS-IT implemented to act on individual internal concentration thresholds
  set Cw chemical-concentration ; water conc  [ug/L]
  set dDdt-h 0 ; damage variation over time
   set guts_n-h guts_n
  repeat guts_n-h  [
    		if (D-int-h >= guts_it_thresh-h)
    [
      			set guts_deathcount-h guts_deathcount-h + 1
      			die	
    			]		
    		set dDdt-h kd-h * (Cw - D-int-h)
    		set D-int-h D-int-h + (1 / guts_n-h) * dDdt-h
  	]

end

;to-report intConc_GUTS
;  report mean [t_intConc_GUTS] of turtles
;end


to TKTD-eggs
  set stress-egg 1 / c_T * ( D-int - c_0)
  if stress-egg > 1
  [set stress-egg 1]
  if stress-egg < 0
  [set stress-egg 0]
end

; ------------------------------------------------------------------------------------------------------------------------------------------
; ----------------- UPDATE -----------------------------------------------------------------------------------------------------------------
; ------------------------------------------------------------------------------------------------------------------------------------------

to update
; individuals update their state variables based on the calc_state variable proccesses
  ask turtles
  [
    set U_E U_E + dU_E / timestep
    set U_H U_H + dU_H / timestep
    set U_R U_R + dU_R / timestep
    set L L + dL / timestep

    if U_H > U_H^b
    [ set q_acceleration q_acceleration + dq_acceleration / timestep
      set h_rate h_rate + dh_rate / timestep
      set Hazard Hazard + h_rate / timestep
    ]
 ]

end


; ------------------------------------------------------------------------------------------------------------------------------------------
; ----------------- PLOT -------------------------------------------------------------------------------------------------------------------
; ------------------------------------------------------------------------------------------------------------------------------------------

to do-plots

;  set-current-plot "Powers"
;  set-current-plot-pen "Assimilation"
;  set-plot-pen-interval 1
;  ifelse any? turtles with [U_H > U_H^b]
;  [plot mean [S_A] of turtles with [U_H > U_H^b]]
;  [plot 0]
;
;  set-current-plot-pen "Mobilization"
;  set-plot-pen-interval 1
;  ifelse any? turtles with [U_H > U_H^b]
;  [plot mean [S_C] of turtles with [U_H > U_H^b]]
;  [plot 0]

; ----------------- PLOT one agent - verification with add-my-pet --------------------------------------------------------------------------
; ------------------------------------------------------------------------------------------------------------------------------------------

;  set-current-plot "Length"
;  set-plot-pen-interval 1
;  ifelse any? turtles with [U_H > U_H^b]
;  [plot mean [L / del_M] of turtles with [U_H > U_H^b]]
;  [plot 0]


 ; ------------------------------------------------------------------------------------------------------------------------------------------

  set-current-plot "food density"
  set-plot-pen-interval 1 / timestep
  plot X

  set-current-plot "guts on survival"
  set-plot-pen-interval 1 / timestep
  plot guts_deathcount

  set-current-plot "guts on hatching"
  set-plot-pen-interval 1 / timestep
  plot guts_deathcount-h

  set-current-plot "stress level on egg production"
  set-plot-pen-interval 1 / timestep
  plot stress-egg

  set-current-plot "dead drought"
  set-plot-pen-interval 1 / timestep
  plot low-recruitment


  set-current-plot "Internal concentration"
  set-plot-pen-interval 1
  ifelse any? turtles with [U_H > U_H^b]
  [plot mean [D-int] of turtles with [U_H > U_H^b]]
  [plot 0]


  set-current-plot "Population abundance"
  set-current-plot-pen "Juveniles"
  set-plot-pen-interval 1 / timestep
  ifelse any? turtles with [U_H > U_H^b and U_H <= U_H^p] [plot count turtles with [U_H > U_H^b and U_H <= U_H^p]]
  [plot 0]
    set-current-plot-pen "Adults"
  set-plot-pen-interval 1 / timestep
  ifelse any? turtles with [U_H > U_H^p] [plot count turtles with [U_H > U_H^p]]
  [plot 0]
  set-current-plot-pen "Population"
  set-plot-pen-interval 1 / timestep
  ifelse any? turtles with [U_H > U_H^b ] [plot count turtles with [U_H > U_H^b]]
  [plot 0]

  if indirect-effects = "yes" and year >=  exp-in
  [
    set-current-plot "SSD"
    set-plot-pen-interval 1 / timestep
    plot SSD  ]

end
@#$#@#$#@
GRAPHICS-WINDOW
190
21
231
63
-1
-1
1.0
1
10
1
1
1
0
1
1
1
-16
16
-16
16
0
0
1
ticks
30.0

BUTTON
19
30
82
63
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
103
31
166
64
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
17
125
105
170
NIL
dead-lifespan
17
1
11

MONITOR
18
71
101
116
count turtles
count turtles
17
1
11

MONITOR
115
71
172
116
NIL
year
17
1
11

MONITOR
121
125
185
170
dead bkg
dead-bkg
17
1
11

MONITOR
186
125
274
170
dead drought
low-recruitment
17
1
11

MONITOR
20
178
93
223
dead starv
dead-starvation
17
1
11

MONITOR
189
69
246
114
NIL
day
17
1
11

CHOOSER
17
240
155
285
species
species
"Topeka Shiner" "DR Minnow" "Humpback Chub" "Spikedace"
2

CHOOSER
152
290
290
335
density-dependence
density-dependence
"yes" "no"
0

CHOOSER
16
291
154
336
forcing_variable
forcing_variable
"yes" "no"
0

CHOOSER
307
213
445
258
chemical
chemical
"yes" "no"
0

SLIDER
12
347
184
380
timestep
timestep
1
24
1.0
1
1
NIL
HORIZONTAL

SLIDER
196
346
368
379
timestep2
timestep2
1
48
24.0
1
1
NIL
HORIZONTAL

PLOT
496
10
972
324
Population abundance
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"juveniles" 1.0 0 -13840069 true "" ""
"population" 1.0 0 -16777216 true "" ""
"Adults" 1.0 0 -5825686 true "" ""

INPUTBOX
11
399
187
466
CommonParameters
Common-param.txt
1
1
String

INPUTBOX
11
642
187
709
ExposureFile
Hydroxy-TX.csv
1
1
String

CHOOSER
307
169
447
214
indirect-effects
indirect-effects
"yes" "no"
0

INPUTBOX
11
464
249
524
Chemical-Parameters
Chemical-param-GUTS.txt
1
0
String

CHOOSER
307
124
446
169
effects-on-eggs
effects-on-eggs
"yes" "no"
0

CHOOSER
308
79
446
124
effects-on-hatching
effects-on-hatching
"yes" "no"
0

CHOOSER
299
290
437
335
food-dynamics
food-dynamics
"unlimited" "sinusoidal"
1

INPUTBOX
329
398
400
458
T-optimal
13.0
1
0
Number

INPUTBOX
400
398
469
458
guts_n
1440.0
1
0
Number

INPUTBOX
253
398
330
458
MF
11.0
1
0
Number

PLOT
851
332
1177
584
Internal Concentration
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

PLOT
978
11
1349
314
guts on survival
NIL
NIL
0.0
10.0
0.0
5.0
true
false
"" ""
PENS
"guts on survival" 1.0 0 -16777216 true "" ""

INPUTBOX
11
524
226
584
Chemical-Parameters-Hatching
Chemical-param-GUTS-hatching.txt
1
0
String

INPUTBOX
11
584
187
644
Chemical-Parameters-TKTD
Chemical-param-TKTD.txt
1
0
String

INPUTBOX
253
457
397
517
lower-limit-low-recruitment
11.0
1
0
Number

INPUTBOX
397
457
542
517
upper-limit-low-recruitment
20.0
1
0
Number

INPUTBOX
253
516
398
576
upper-limit-bad-year
25.0
1
0
Number

INPUTBOX
397
517
542
577
lower-limit-bad-year
1.0
1
0
Number

PLOT
552
372
844
578
dead drought
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

PLOT
1481
329
1789
581
SSD
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

CHOOSER
308
34
446
79
effects-on-survival
effects-on-survival
"yes" "no"
0

PLOT
1350
10
1739
315
guts on hatching
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

PLOT
1179
330
1484
584
stress level on egg production
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

PLOT
710
586
986
767
food density
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

PLOT
1159
610
1359
760
temperature
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

INPUTBOX
219
641
438
701
Species-parameters
Parameters-HC.txt
1
0
String

@#$#@#$#@
## WHAT IS IT?

Model representing the population dynamics of multiple listed fish species. It estimates the population-level effects of time-variable exposures under realistic habitat conditions. At the individual level, the metabolic processes are based on DEB theory and are driven by temperature. Resource availability can also affect population dynamics when exposed to the stressor. Each species is characterized by different DEB-parameter sets, mortality rates, density-dependent parameters, and environmental conditions (i.e., temperature and food drivers). Population dynamics emerge from interactions among individuals and between individuals and the environment and are affected by stochastic droughts. Chemical effects are based on laboratory data and consider relevant effect endpoints, such as fish mortality, hatching success, egg production, and a decrease in food availability.          

## HOW IT WORKS


## HOW TO USE IT

THe user can choose one of the four species to represent in the interface. However, the name of the input file "Species-parameters" has to be changed manually (e.g., Parameters-HC.txt)
The different effect sub-models can be swithced on and off, whereas the exposure magnification factor (MF) can be entered manually in the interface.

## THINGS TO NOTICE



## THINGS TO TRY

## EXTENDING THE MODEL

TKTD parameters can be changed as well as the exposure scenario by choosing other input files (write a different input file in the interface)

## NETLOGO FEATURES



## RELATED MODELS


## CREDITS AND REFERENCES
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
