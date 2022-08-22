#    http://shiny.rstudio.com/

# Define UI 
ui <- fluidPage(
  title="Aid for planning SPT screening experiments,v0.2",
  # Intro / explanations ----
  fluidRow(
    column(12, 
      h2("Aid for planning SPT screening experiments, v0.2"),
      h4("In this version, the user gives SSMDs and SDs of the compounds."),
    	h4(HTML(paste("We are using definitions of Strictly Standardized Mean Difference",
    	" from the Wikipedia page <a href='https://en.wikipedia.org/wiki/Strictly_standardized_mean_difference#:~:text=In%20statistics%2C%20the%20strictly%20standardized,from%20one%20of%20two%20groups.'>",
    	"Strictly standardized mean difference</a>"
    	))),
      h4(HTML(paste("We assume that data has been centered and scaled so that all of the DMSO",
      "(or the relevant negative control) can be used in making inferences about each",
      "compound. We have <B>SSMD=(Mean<sub>compound</sub>-Mean<sub>DMSO</sub>) / sqrt(",
      "Variance<sub>compound</sub>+Variance<sub>DMSO</sub>)</B>."))),
      
      h4(HTML(paste("Below we are choosing the number of non-control compounds",
        "and the proportion of these, &pi;<sub>0</sub>, whose effect on the metric is",
        "the same as the negative control. By this assumption, 1-&pi;<sub>0</sub> is",
        "the proportion of the non-control compounds whose effect on the metric is",
        "different from that of the negative control. This proportion is evenly divided",
        "among the three different classes of active compounds below."
      ))),
      h4(HTML(paste("The last option in the section 'Screen-wide parameters' is",
      "<b><font color='red'>'How heavy should the tail be?'</font></b> Random variables having the same variances can",
      "have different shapes. This slider controls how heavy the tails are in the t-distribution",
      "from which individual FOV measurements are drawn. That t-distribution will have degrees of freedom equal to 41",
      "minus the value chosen by this slider. Later choices for SDs will build on this choice.",
      "Note that large settings for this begin to cause a downward bias in pi0 which can be mitigated by increasing",
      "the number of FOV measurements."))),
      h4(HTML(paste("The third hit calling method below, <b>FDR cutoff</b>, makes use of",
        "<a href='https://en.wikipedia.org/wiki/Q-value_(statistics)'>Storey's implementation of the q-value</a>",
        "which is a method for controlling the <a href='https://en.wikipedia.org/wiki/False_discovery_rate'>",
        "false discovery rate, ie, the FDR.</a>"))),
      h4(HTML(paste("More info on the calculations behind this app can be found at",
      "<a href='https://eikontx.atlassian.net/wiki/spaces/DescFOVFilesARScreen/pages/1397227589/Aid+for+planning+SPT+screening+experiments'>",
      "the corresponding Confluence page.</a>")))
    )),
    hr(),
  # User inputs screen-wide parameters  ----
    fluidRow(
    column(12, h2("Screen-wide parameters"))),
  fluidRow(
    column(4, sliderInput("nCompounds", "Total number of non-control compounds", 
    	min = 1e4, max = 2e5, value=2e4, step=2e3)),
    column(4, selectInput("metricNow", "Choose a metric:",
    	# c("MPDC" ='MPDC', 'Q3JL'='Q3JL', "CDF_AUC"='CDF_AUC'), selected="MPDC")),
    	# Commenting out for now as we only want metrics which increase when dynamics are increased
    	c("MPDC" ='MPDC', 'Q3JL'='Q3JL'), selected="MPDC")),
    column(4, selectInput("pi0", HTML(paste("Prop'n of non-active compounds, ie, &pi;<sub>0</sub>",
      "(Prop'n of compounds like neg control)")),
      c("0.9" =0.9, '0.99'=0.99, "0.999" =0.999, '0.9999'=0.9999), selected="0.99"))),
  fluidRow(
    column(4, sliderInput("nFOVsPerWell", "Number FOVs (number of data points) per well", 
      min =2, max = 200, value=6, step=1)),
    column(4, 
      # h4(HTML(paste("<font color='red'><b>How heavy should the tail be?</b></font>"))),
      h4(HTML(paste("How heavy should the tail be?"))),
      sliderInput("tailWeight", 
      HTML(paste("Lower is more like a normal distribution")), 
      min = 1, max = 38.9, value=36, step=0.1),
    plotOutput("howHeavyTails"))
    # df for individual FOV measurements will be 41 - tailWeight
    
    ), # end of fluidRow
   hr(),
  fluidRow( # Number of wells of various types per plate ----
    column(12, h2("Number of wells / Number of plates"), 
           h4("We assume that 308 wells are available per plate."),
           h4("Controls"))),
    # This is set above in constants section as nWellsPerPlate 
  fluidRow(
    # column(6, sliderInput("nWellsPerPlatePositive", HTML(paste("Number wells <font size='+2'>",
    column(4, sliderInput("nWellsPerPlatePositive", HTML(paste("Number wells",
      "per plate positive controls")), min = 0, max = 64, value=4, step=1)), 
    column(4, sliderInput("nWellsPerPlateNegative", HTML(paste("Number wells",
      "per plate negative control")), min = 0, max = 64, value=4, step=1)),
    column(4, sliderInput("nWellsPerPlateNoDye", HTML(paste("Number no dye wells",
      "per plate")), min = 0, max = 64, value=2, step=1)),
    ),
  fluidRow( # Number of wells of various types per plate ----
    column(12, h4("Non-controls"))),
  fluidRow(
    column(6, sliderInput("nWellsPlateNonControls", HTML(paste("Number wells",
      "per compound per plate")),
      min = 1, max = 100, value=2, step=1)),
    column(6, sliderInput("nPlatesNonControls", HTML(paste("Number plates on which",
      "any specific non-control appears")), min = 1, max = 100, value=2, step=1))
    ),
  # User inputs neg control & non-actives  ----
   fluidRow(
     column(12, h2("Characteristics for negative control (eg, DMSO) and non-active compounds")),
     column(6, h4("Negative control"),
       h5(HTML(paste("<em>Mean will often be close to zero",
    	   "as we are assuming that the data is centered</em>"))),
    	 sliderInput("mu.DMSO", paste("Mean, MPDC (um^2/sec) "), min =-0.5, max = 0.5, value=0, step=0.01),
    	 sliderInput("sdDMSO", "SD, MPDC", min =0.01, max = 8, value=1, step=0.1)
     ),
     column(6, h4("Non-active compounds"),
       h5(HTML(paste("<em>SSMD for non-actives is assumed to be zero</em>"))),
       sliderInput("sd.NonAct", "SD, MPDC", min =0.01, max = 8, value=1, step=0.1)
     )), # end of fluidRow
  # User inputs actives  ----
  hr(),
  fluidRow(
    column(12, h2("Characteristics for three classes of active compounds"))),
  fluidRow(
    column(4, 
    	h4("First class of active compound"), # First type of active ----
    	selectInput("SSMD.t1", "Choose the true effect size, ie, the SSMD:",
    	  c("Extremely strong=5" =5, 'Very strong=3'=3, 
    	  "Strong=2"=2, 'Fairly strong=1.82'=1.82,
    	  "Moderate=1.46"=1.46, 'Fairly moderate=1.14'=1.14, 'Fairly weak=0.875'=0.875,
    	  "Weak=0.625"=0.625, 'Very weak=0.375'=0.375), selected=1.46),
    	sliderInput("sd.t1", "SD, MPDC",
    	            min =0.01, max = 8, value=1, step=0.1)
    	),
    # user inputs, type 2 ----
    column(4,
      h4("Second class of active compound"),
      selectInput("SSMD.t2", "Choose the true effect size, ie, the SSMD:",
        c("Extremely strong=5" =5, 'Very strong=3'=3, 
        "Strong=2"=2, 'Fairly strong=1.82'=1.82,
        "Moderate=1.46"=1.46, 'Fairly moderate=1.14'=1.14, 'Fairly weak=0.875'=0.875,
        "Weak=0.625"=0.625, 'Very weak=0.375'=0.375), selected=1.46),
      sliderInput("sd.t2", "SD, MPDC",
                  min =0.01, max = 8, value=1, step=0.1)
      ),
    # user inputs, type 3 ----
    column(3,
    h4("Third class of active compound"),
      selectInput("SSMD.t3", "Choose the true effect size, ie, the SSMD::",
      c("Extremely strong=5" =5, 'Very strong=3'=3, 
      "Strong=2"=2, 'Fairly strong=1.82'=1.82,
      "Moderate=1.46"=1.46, 'Fairly moderate=1.14'=1.14, 'Fairly weak=0.875'=0.875,
      "Weak=0.625"=0.625, 'Very weak=0.375'=0.375), selected=1.46),
         sliderInput("sd.t3", "SD(t3), MPDC",
                     min =0.01, max = 8, value=1, step=0.1)
  )
 ), # End of fluid row
 hr(), # User inputs analysis criteria ----
 fluidRow(column(12, h2("Specify analysis parameters"),
   h5(HTML(paste("<B>We are performing three distinct assessments.</B>",
   "<ul><li>'The industry standard': Mean for the negative control +/-",
   "<b>M</b> * ( the SD of the negative control). Below, you specify <b>M</b>.</li>",
   "<li>You specify an SSMD cut-off.</li>",
   "<li>You specify a FDR (False Discovery Rate) cut-off.</li></ul>",
   "In this iteration of this tool",
   "we are assuming that only increases in dynamics are of interest.")))),
   # "As well, you can combine these criteria...")
   column(4, h4("'Industry standard'"),
     sliderInput("sdIndStand", paste("Multiple of the SD of the neg control (How many SDs",
     "away from the mean of the neg",
     "control to be a hit?)"), min =0.5, max = 5, value=1, step=0.1)),
   column(4, h4("SSMD cutoff"),
     sliderInput("SSMDCutoff", "Min SSMD to be a hit", 
     min =0.25, max = 8, value=0.5, step=0.1)),
   column(4, h4("FDR (q-value) cutoff"),
     sliderInput("qvalCutoff", "Max FDR to be a hit", 
     min =0.01, max = 0.9, value=0.4, step=0.01))),
 fluidRow(column(12,
  actionButton("goNow", "Click to start crunching to generate plots")
 )),
 hr(),
 
 h2("Results from hit calling"), # Tabular outputs ----
 fluidRow(column(12, 
   h4("Number of plates needed"),
   tableOutput('NumberOfPlates'))),
 fluidRow(column(6, 
   h4("Counts of hit declarations by compound class and analysis method"),
   tableOutput('tableTestResDataFrame')),
   column(6,
   h4("Precision, recall, and F-stat table"),
   tableOutput('FscoreTab'))),
 h2("Outputs showing SSMD by compound class"), # Graphical outputs ----
   fluidRow(column(6, h4("SSMDs by compound class"), plotOutput("SSMDbyClass")),
     column(6, h4("Observed counts per SSMD subtype by compound class"), 
     plotOutput("tableSSMDbyClass"))),
 hr(),
 h2("Other graphical outputs"),
 fluidRow(column(6, plotOutput("sampleMeans")),
  column(6, plotOutput("histPVals")))
  # fluidRow(column(6, plotOutput("summaryCalcs")
) # End UI----
