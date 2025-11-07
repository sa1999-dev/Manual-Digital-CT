#Download following Packages
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)
library(deSolve)
library(tidyr)
library(purrr)
library(scales)
library(DiagrammeR)
library(rsconnect)

ui <- dashboardPage(
  dashboardHeader(title = "Epidemic & Economic CT Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Epidemic Models", tabName = "epidemic"),
      menuItem("Parameters", tabName = "parameters"),
      menuItem("Reach Analysis", tabName = "reach"),
      menuItem("Demographics", tabName = "demographics"),
      menuItem("Cost Analysis", tabName = "costs"),
      menuItem("Summary Metrics", tabName = "summary")
    )),
  dashboardBody(
    tags$head(
      tags$style(HTML("
               .value-box-icon {font-size: 30px !important;}
               .small-box h3 {font-size: 28px !important;}
               "))
    ),
    tabItems(
      # Epidemics Model
      tabItem(tabName = "epidemic",
              fluidRow(
                column(12,
                       h2("SEIQR Epidemic Models: Manual vs Electronic Tracing"),
                       p("Compare the effectiveness of manual (Qm) and electronic (Qe) contact tracing strategies.")
                )
              ),
              
              fluidRow(
                column(6,
                       box(title = "Manual Tracing (Qm) Controls", status = "primary", 
                           solidHeader = TRUE, width = 12,
                           sliderInput("pi_m", 
                                       label = HTML("&pi;<sub>m</sub> (Manual tracing coverage: share I → Qm)"),
                                       min = 0, max = 1, value = 0.4, step = 0.05)
                       )
                ),
                column(6,
                       box(title = "Electronic Tracing (Qe) Controls", status = "success", 
                           solidHeader = TRUE, width = 12,
                           sliderInput("pi_e", 
                                       label = HTML("&pi;<sub>e</sub> (Electronic tracing coverage: share I → Qe)"),
                                       min = 0, max = 1, value = 0.4, step = 0.05)
                       )
                )
              ),
              
              fluidRow(
                box(title = "Interactive Comparison of I(t): Manual vs Electronic Tracing", 
                    status = "info", solidHeader = TRUE, width = 12,
                    plotOutput("epidemic_comparison_plot", height = "500px"))
              ),
              
              fluidRow(
                column(6,
                       box(title = "Manual Tracing (Qm) Active Compartments", 
                           status = "primary", solidHeader = TRUE, width = 12,
                           plotOutput("qm_compartments_plot", height = "400px")
                       )
                ),
                column(6,
                       box(title = "Electronic Tracing (Qe) Active Compartments", 
                           status = "success", solidHeader = TRUE, width = 12,
                           plotOutput("qe_compartments_plot", height = "400px")
                       )
                )
              ),
              
              fluidRow(
                column(6,
                       box(title = "Qm Model: AUC(I) vs π", status = "primary", 
                           solidHeader = TRUE, width = 12,
                           plotOutput("qm_sweep_auc", height = "350px")
                       )
                ),
                column(6,
                       box(title = "Qe Model: AUC(I) vs π", status = "success", 
                           solidHeader = TRUE, width = 12,
                           plotOutput("qe_sweep_auc", height = "350px")
                       )
                )
              ),
              
              fluidRow(
                column(6,
                       box(title = "Qm Model: Peak Day vs π", status = "primary", 
                           solidHeader = TRUE, width = 12,
                           plotOutput("qm_sweep_peak", height = "350px")
                       )
                ),
                column(6,
                       box(title = "Qe Model: Peak Day vs π", status = "success", 
                           solidHeader = TRUE, width = 12,
                           plotOutput("qe_sweep_peak", height = "350px")
                       )
                )
              )
      ),
      
      # Parameters
      tabItem(tabName = "parameters",
              fluidRow(
                box(title = "Manual Contact Tracing Parameters", status = "primary", 
                    solidHeader = TRUE, width = 6, collapsible = TRUE,
                    numericInput("response_rate_manual", "Response Rate", 
                                 value = 0.61, min = 0.1, max = 1, step = 0.01),
                    numericInput("contacts_per_user_manual", "Number of Contacts per Index Case", 
                                 value = 5, min = 1, max = 20),
                    numericInput("staff_hourly_cost", "Staff Hourly Cost ($)", 
                                 value = 25, min = 15, max = 50),
                    numericInput("manual_staff_hours_per_day", "Staff Hours per Day", 
                                 value = 20, min = 1, max = 100, step = 1),
                    numericInput("time_per_index", "Time per Index Case (minutes)", 
                                 value = 79.23, min = 30, max = 200, step = 0.1),
                    numericInput("time_per_contact", "Time per Contact (minutes)", 
                                 value = 49.92, min = 20, max = 120, step = 0.1),
                    numericInput("non_responder_followup_time", "Non-Responder Follow-up Time (minutes)", 
                                 value = 21.50, min = 10, max = 60, step = 0.5),
                    numericInput("fixed_costs_manual", "Fixed Costs ($)", 
                                 value = 10000, min = 0, max = 100000, step = 1000)
                ),
                
                box(title = "Digital Contact Tracing Parameters", status = "success", 
                    solidHeader = TRUE, width = 6, collapsible = TRUE,
                    h4("Capacity & Response"),
                    numericInput("max_sent_per_day", "Max People Reached per Day", 
                                 value = 100, min = 10, max = 500, step = 10),
                    numericInput("response_rate_digital", "Index Case Response Rate", 
                                 value = 0.07, min = 0.001, max = 0.5, step = 0.001),
                    numericInput("recruitment_success", "Recruitment Success (contacts joining)", 
                                 value = 0.11, min = 0.01, max = 0.5, step = 0.01),
                    numericInput("contacts_per_user_digital", "Contacts per Index Case", 
                                 value = 4.3, min = 1, max = 10, step = 0.1),
                    
                    hr(),
                    h4("Time Parameters"),
                    numericInput("survey_review_time", "Survey Review Time per Successful Contact (minutes)", 
                                 value = 15, min = 5, max = 60, step = 1),
                    numericInput("unsuccessful_contact_time", "Time per Unsuccessful Contact (minutes)", 
                                 value = 3, min = 1, max = 30, step = 1),
                    
                    hr(),
                    h4("Cost Parameters"),
                    numericInput("maintenance_hours_per_day", "Maintenance Hours per Day", 
                                 value = 1, min = 0, max = 8, step = 0.5),
                    numericInput("maintenance_staff_hourly", "Maintenance Staff Hourly Rate ($)", 
                                 value = 50, min = 25, max = 100, step = 5),
                    numericInput("digital_staff_hourly", "Staff Hourly Cost ($)", 
                                 value = 25, min = 15, max = 50, step = 1),
                    numericInput("fixed_costs_digital", "Fixed Costs ($)", 
                                 value = 50000, min = 0, max = 500000, step = 1000)
                )
              ),
              
              fluidRow(
                box(title = "Analysis Settings", status = "warning", solidHeader = TRUE, 
                    width = 12, collapsible = TRUE,
                    fluidRow(
                      column(6,
                             numericInput("analysis_period_days", "Analysis Period (days)", 
                                          value = 30, min = 1, max = 365)),
                      column(6,
                             numericInput("total_population", "Total Population (for reference)", 
                                          value = 100000, min = 1000, max = 10000000, step = 1000))
                    ),
                    br(),
                    actionButton("calculate", "Calculate Results", 
                                 class = "btn-primary btn-lg", width = "200px")
                )
              )
      ),
      
      # Reach Analysis
      tabItem(tabName = "reach",
              fluidRow(
                valueBoxOutput("manual_reach_box", width = 4),
                valueBoxOutput("digital_reach_box", width = 4),
                valueBoxOutput("reach_advantage_box", width = 4)
              ),
              
              fluidRow(
                box(title = "People Reached in Fixed Time", status = "primary", 
                    solidHeader = TRUE, width = 12,
                    plotlyOutput("reach_comparison_plot", height = "400px")
                )
              )
      ),
      
      # Demographics
      tabItem(tabName = "demographics",
              fluidRow(
                box(title = "Digital Contact Tracing: Invitation vs Completion Rates by Demographics", 
                    status = "info", solidHeader = TRUE, width = 12,
                    p("Based on historical data from Durham County digital contact tracing implementation.")
                )
              ),
              
              fluidRow(
                box(title = "Age Group Analysis", status = "primary", 
                    solidHeader = TRUE, width = 4,
                    plotlyOutput("age_demographics_plot", height = "500px")
                ),
                
                box(title = "Race/Ethnicity Analysis", status = "success", 
                    solidHeader = TRUE, width = 4,
                    plotlyOutput("race_demographics_plot", height = "500px")
                ),
                
                box(title = "Language Analysis", status = "warning", 
                    solidHeader = TRUE, width = 4,
                    plotlyOutput("language_demographics_plot", height = "500px")
                )
              )
      ),
      
      #Cost Analysis
      tabItem(tabName = "costs",
              fluidRow(
                valueBoxOutput("manual_cost_box", width = 4),
                valueBoxOutput("digital_cost_box", width = 4),
                valueBoxOutput("cost_advantage_box", width = 4)
              ),
              
              fluidRow(
                box(title = "Cost Comparison", status = "danger", 
                    solidHeader = TRUE, width = 6,
                    plotlyOutput("total_cost_plot", height = "350px")
                ),
                
                box(title = "Cost per Person Reached", status = "warning", 
                    solidHeader = TRUE, width = 6,
                    plotlyOutput("cost_per_person_plot", height = "350px")
                )
              ),
              
              fluidRow(
                box(title = "Cost Breakdown by Component", status = "primary", 
                    solidHeader = TRUE, width = 12,
                    plotlyOutput("cost_breakdown_plot", height = "400px")
                )
              )),
      
      # Summary Metrics Tab
      tabItem(tabName = "summary",
              fluidRow(
                column(6,
                       box(title = "People per Hour", status = "info", 
                           solidHeader = TRUE, width = 12,
                           h4(textOutput("people_per_hour_text"))
                       )
                ),
                column(6,
                       box(title = "Cost Efficiency", status = "success", 
                           solidHeader = TRUE, width = 12,
                           h4(textOutput("cost_efficiency_text"))
                       )
                )
              ),
              
              fluidRow(
                box(title = "Breakeven Analysis", status = "warning", 
                    solidHeader = TRUE, width = 12,
                    h4(textOutput("breakeven_text"))))
      ))
))

traceback()

server <- function(input, output, session) {
  
  # Epidemic Code
  # Population & initial conditions
  N   <- 100000L
  I0  <- 20
  E0  <- 80
  Q0  <- 0
  R0  <- 0
  S0  <- N - I0 - E0 - Q0 - R0
  
  # Core disease parameters
  R0_basic <- 2.5
  kappa    <- 1/7
  beta     <- R0_basic * kappa
  sigma    <- 1/3
  
  # Manual quarantine parameters
  gamma_m  <- 1/5
  rho_m    <- 0.02
  tau_max <- 1/5
  
  # Electronic quarantine parameters
  gamma_e <- 1/4
  rho_e   <- 0.01
  tau_max_e <- 1/4
  
  # Time grid
  Tmax  <- 200
  Times <- seq(0, Tmax, by = 0.5)
  
  # Helper functions
  auc_trapz <- function(x, y) {
    sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2)
  }
  
  to_per100k <- function(df, N, cols = c("S","E","I","Qm","R")) {
    present <- intersect(cols, names(df))
    df[present] <- lapply(df[present], function(v) (v * 1e5) / N)
    df
  }
  
  # Manual tracing ODE
  seiQRm_rhs <- function(t, state, par) {
    with(as.list(c(state, par)), {
      Npop   <- S + E + I + Qm + R
      lambda <- beta * (I + rho_m * Qm) / Npop
      tau    <- pi * tau_max
      
      dS  <- -lambda * S
      dE  <-  lambda * S - sigma * E
      dI  <-  sigma * E - (kappa + tau) * I
      dQm <-  tau * I - gamma_m * Qm
      dR  <-  kappa * I + gamma_m * Qm
      
      list(c(dS, dE, dI, dQm, dR))
    })
  }
  
  run_qm <- function(pi, y0 = c(S = S0, E = E0, I = I0, Qm = Q0, R = R0), times = Times) {
    pars <- c(beta = beta, sigma = sigma, kappa = kappa,
              gamma_m = gamma_m, rho_m = rho_m, tau_max = tau_max, pi = pi)
    out <- deSolve::ode(y = y0, times = times, func = seiQRm_rhs, parms = pars, method = "lsoda")
    as.data.frame(out)
  }
  
  # Electronic tracing ODE
  seiQRe_rhs <- function(t, state, par) {
    with(as.list(c(state, par)), {
      Npop   <- S + E + I + Qe + R
      lambda <- beta * (I + rho_e * Qe) / Npop
      tau    <- pi * tau_max_e
      
      dS  <- -lambda * S
      dE  <-  lambda * S - sigma * E
      dI  <-  sigma * E - (kappa + tau) * I
      dQe <-  tau * I - gamma_e * Qe
      dR  <-  kappa * I + gamma_e * Qe
      
      list(c(dS, dE, dI, dQe, dR))
    })
  }
  
  run_qe <- function(pi, y0 = c(S = S0, E = E0, I = I0, Qe = Q0, R = R0), times = Times) {
    pars <- c(beta = beta, sigma = sigma, kappa = kappa,
              gamma_e = gamma_e, rho_e = rho_e, tau_max_e = tau_max_e, pi = pi)
    out <- deSolve::ode(y = y0, times = times, func = seiQRe_rhs, parms = pars, method = "lsoda")
    as.data.frame(out)
  }
  
  # Main comparison plot
  output$epidemic_comparison_plot <- renderPlot({
    qm_sim <- run_qm(pi = input$pi_m)
    qe_sim <- run_qe(pi = input$pi_e)
    
    qm_per100k <- to_per100k(qm_sim, N, cols = c("I")) %>%
      select(time, I) %>%
      mutate(Model = paste0("Manual Qm (π = ", input$pi_m, ")"))
    
    qe_per100k <- to_per100k(qe_sim, N, cols = c("I")) %>%
      select(time, I) %>%
      mutate(Model = paste0("Electronic Qe (π = ", input$pi_e, ")"))
    
    both <- bind_rows(qm_per100k, qe_per100k)
    
    ggplot(both, aes(x = time, y = I, color = Model)) +
      geom_line(linewidth = 1.2) +
      labs(title = "Comparison of I(t): Manual vs Electronic Tracing",
           x = "Time (days)", 
           y = "Infectious individuals per 100,000",
           color = "Model") +
      theme_minimal() +
      scale_color_brewer(palette = "Set1") +
      theme(legend.position = "top", text = element_text(size = 12))
  })
  
  # Qm compartments plot
  output$qm_compartments_plot <- renderPlot({
    qm_sim <- run_qm(pi = input$pi_m)
    qm_data <- to_per100k(qm_sim, N, cols = c("E","I","Qm"))
    qm_long <- qm_data %>%
      select(time, E, I, Qm) %>%
      pivot_longer(-time, names_to = "Compartment", values_to = "Per100k")
    
    ggplot(qm_long, aes(x = time, y = Per100k, color = Compartment)) +
      geom_line(linewidth = 1) +
      labs(title = paste0("Manual Tracing: Active compartments (π = ", input$pi_m, ")"),
           x = "Time (days)", y = "Individuals per 100,000") +
      theme_minimal() +
      scale_color_brewer(palette = "Dark2")
  })
  
  # Qe compartments plot
  output$qe_compartments_plot <- renderPlot({
    qe_sim <- run_qe(pi = input$pi_e)
    qe_data <- to_per100k(qe_sim, N, cols = c("E","I","Qe"))
    qe_long <- qe_data %>%
      select(time, E, I, Qe) %>%
      pivot_longer(-time, names_to = "Compartment", values_to = "Per100k")
    
    ggplot(qe_long, aes(x = time, y = Per100k, color = Compartment)) +
      geom_line(linewidth = 1) +
      labs(title = paste0("Electronic Tracing: Active compartments (π = ", input$pi_e, ")"),
           x = "Time (days)", y = "Individuals per 100,000") +
      theme_minimal() +
      scale_color_brewer(palette = "Dark2")
  })
  
  # Sweep analysis plots
  output$qm_sweep_auc <- renderPlot({
    pi_grid <- seq(0, 1, by = 0.05)
    qm_sweep <- map_dfr(pi_grid, function(p) {
      sim <- run_qm(pi = p)
      sim_p <- to_per100k(sim, N, cols = c("I"))
      tibble(pi = p, auc_I = auc_trapz(sim$time, sim_p$I))
    })
    
    ggplot(qm_sweep, aes(x = pi, y = auc_I)) +
      geom_line(linewidth = 1, color = "steelblue") +
      geom_point(color = "steelblue") +
      scale_x_continuous(labels = percent_format(accuracy = 1)) +
      labs(title = "Manual Qm: AUC(I) vs π",
           x = "π (share I → Qm)", y = "AUC per 100,000") +
      theme_minimal()
  })
  
  output$qe_sweep_auc <- renderPlot({
    pi_grid <- seq(0, 1, by = 0.05)
    qe_sweep <- map_dfr(pi_grid, function(p) {
      sim <- run_qe(pi = p)
      sim_p <- to_per100k(sim, N, cols = c("I"))
      tibble(pi = p, auc_I = auc_trapz(sim$time, sim_p$I))
    })
    
    ggplot(qe_sweep, aes(x = pi, y = auc_I)) +
      geom_line(linewidth = 1, color = "steelblue") +
      geom_point(color = "steelblue") +
      scale_x_continuous(labels = percent_format(accuracy = 1)) +
      labs(title = "Electronic Qe: AUC(I) vs π",
           x = "π (share I → Qe)", y = "AUC per 100,000") +
      theme_minimal()
  })
  
  output$qm_sweep_peak <- renderPlot({
    pi_grid <- seq(0, 1, by = 0.05)
    qm_sweep <- map_dfr(pi_grid, function(p) {
      sim <- run_qm(pi = p)
      tibble(pi = p, peak_day = sim$time[which.max(sim$I)])
    })
    
    ggplot(qm_sweep, aes(x = pi, y = peak_day)) +
      geom_line(linewidth = 1, color = "firebrick") +
      geom_point(color = "firebrick") +
      scale_x_continuous(labels = percent_format(accuracy = 1)) +
      labs(title = "Manual Qm: Peak day vs π",
           x = "π (share I → Qm)", y = "Peak day") +
      theme_minimal()
  })
  
  output$qe_sweep_peak <- renderPlot({
    pi_grid <- seq(0, 1, by = 0.05)
    qe_sweep <- map_dfr(pi_grid, function(p) {
      sim <- run_qe(pi = p)
      tibble(pi = p, peak_day = sim$time[which.max(sim$I)])
    })
    
    ggplot(qe_sweep, aes(x = pi, y = peak_day)) +
      geom_line(linewidth = 1, color = "firebrick") +
      geom_point(color = "firebrick") +
      scale_x_continuous(labels = percent_format(accuracy = 1)) +
      labs(title = "Electronic Qe: Peak day vs π",
           x = "π (share I → Qe)", y = "Peak day") +
      theme_minimal()
  })
  
  # Economic Dashboard
  # Demographic data
  demographics_data <- data.frame(
    Category = c(rep("Age", 8), rep("Race", 6), rep("Language", 3)),
    Group = c("0-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+",
              "Black/African American", "White", "Asian", "Other/2+ races", "American Indian/Alaska Native", "Native Hawaiian/Pacific Islander",
              "English", "Spanish", "Other"),
    Invitees_Pct = c(3, 30, 26, 16, 12, 8, 4, 1,
                     39, 47, 9, 4, 0.5, 0.5,
                     97, 2, 1),
    Completed_Pct = c(2, 34, 27, 15, 11, 7, 4, 0,
                      22, 62, 12, 3, 0, 0,
                      89, 4, 7)
  )
  
  # Reactive calculations
  calculations <- reactive({
    input$calculate
    
    # Manual Contact Tracing Calculations
    total_manual_staff_hours <- input$manual_staff_hours_per_day * input$analysis_period_days
    
    time_per_index_hours <- input$time_per_index / 60
    time_per_contact_hours <- input$time_per_contact / 60
    non_responder_time_hours <- input$non_responder_followup_time / 60
    
    successful_contacts_per_case <- input$contacts_per_user_manual * input$response_rate_manual
    time_for_successful_contacts <- successful_contacts_per_case * time_per_contact_hours
    time_per_index_case_total <- time_per_index_hours + time_for_successful_contacts + non_responder_time_hours
    
    index_cases <- floor(total_manual_staff_hours / time_per_index_case_total)
    total_successful_contacts <- floor(index_cases * successful_contacts_per_case)
    manual_total_reached <- index_cases + total_successful_contacts
    
    # Digital Contact Tracing Calculations
    total_analysis_capacity <- input$max_sent_per_day * input$analysis_period_days
    responding_index_cases <- total_analysis_capacity * input$response_rate_digital
    contacts_joining <- responding_index_cases * input$contacts_per_user_digital * input$recruitment_success
    digital_total_reached <- responding_index_cases + contacts_joining
    
    # Manual Cost Calculations
    manual_daily_staff_cost <- input$manual_staff_hours_per_day * input$staff_hourly_cost
    manual_total_cost <- (manual_daily_staff_cost * input$analysis_period_days) + input$fixed_costs_manual
    
    # Digital Cost Calculations
    # Calculate people reached per day
    digital_people_per_day <- digital_total_reached / input$analysis_period_days
    
    # Time for survey review (successful contacts)
    survey_review_hours_per_day <- (digital_people_per_day * input$survey_review_time) / 60
    
    # Time for unsuccessful contacts
    unsuccessful_per_day <- input$max_sent_per_day - digital_people_per_day
    unsuccessful_time_hours_per_day <- (unsuccessful_per_day * input$unsuccessful_contact_time) / 60
    
    # Total daily staff time
    digital_daily_staff_hours <- survey_review_hours_per_day + unsuccessful_time_hours_per_day
    digital_daily_staff_cost <- digital_daily_staff_hours * input$digital_staff_hourly
    
    # Maintenance costs
    digital_daily_maintenance_cost <- input$maintenance_hours_per_day * input$maintenance_staff_hourly
    
    # Total digital costs
    digital_operational_cost <- (digital_daily_staff_cost + digital_daily_maintenance_cost) * input$analysis_period_days
    digital_total_cost <- digital_operational_cost + input$fixed_costs_digital
    
    list(
      manual_reached = round(manual_total_reached, 0),
      digital_reached = round(digital_total_reached, 0),
      manual_cost_total = manual_total_cost,
      digital_cost_total = digital_total_cost,
      manual_cost_per_person = if(manual_total_reached > 0) manual_total_cost / manual_total_reached else 0,
      digital_cost_per_person = if(digital_total_reached > 0) digital_total_cost / digital_total_reached else 0,
      cost_components = list(
        manual = list(
          daily_staff = manual_daily_staff_cost * input$analysis_period_days,
          fixed = input$fixed_costs_manual
        ),
        digital = list(
          daily_staff = digital_daily_staff_cost * input$analysis_period_days,
          maintenance = digital_daily_maintenance_cost * input$analysis_period_days,
          fixed = input$fixed_costs_digital
        )
      )
    )
  })
  
  # Value Boxes - Reach Tab
  output$manual_reach_box <- renderValueBox({
    calc_data <- calculations()
    valueBox(
      value = format(calc_data$manual_reached, big.mark = ","),
      subtitle = paste("Manual CT Reach (", input$analysis_period_days, " days)")
    )
  })
  
  output$digital_reach_box <- renderValueBox({
    calc_data <- calculations()
    valueBox(
      value = format(calc_data$digital_reached, big.mark = ","),
      subtitle = paste("Digital CT Reach (", input$analysis_period_days, " days)")
    )
  })
  
  output$reach_advantage_box <- renderValueBox({
    calc_data <- calculations()
    better_method <- ifelse(calc_data$manual_reached > calc_data$digital_reached, 
                            "Manual", "Digital")
    advantage <- abs(calc_data$manual_reached - calc_data$digital_reached)
    valueBox(
      value = format(advantage, big.mark = ","),
      subtitle = paste(better_method, "reaches more people")
    )
  })
  
  # Value Boxes - Cost Tab
  output$manual_cost_box <- renderValueBox({
    calc_data <- calculations()
    valueBox(
      value = paste0("$", format(round(calc_data$manual_cost_total, 0), big.mark = ",")),
      subtitle = "Manual CT Total Cost",
      color = "blue"
    )
  })
  
  output$digital_cost_box <- renderValueBox({
    calc_data <- calculations()
    valueBox(
      value = paste0("$", format(round(calc_data$digital_cost_total, 0), big.mark = ",")),
      subtitle = "Digital CT Total Cost",
      color = "green"
    )
  })
  
  output$cost_advantage_box <- renderValueBox({
    calc_data <- calculations()
    cheaper_method <- ifelse(calc_data$manual_cost_per_person < calc_data$digital_cost_per_person, 
                             "Manual", "Digital")
    valueBox(
      value = paste0("$", round(min(calc_data$manual_cost_per_person, calc_data$digital_cost_per_person), 2)),
      subtitle = paste(cheaper_method, "is cheaper per person"),
      color = "yellow"
    )
  })
  
  # Reach comparison plot
  output$reach_comparison_plot <- renderPlotly({
    calc_data <- calculations()
    
    data <- data.frame(
      Method = c("Manual", "Digital"),
      People_Reached = c(calc_data$manual_reached, calc_data$digital_reached)
    )
    
    p <- ggplot(data, aes(x = Method, y = People_Reached, fill = Method)) +
      geom_col(alpha = 0.8, width = 0.6) +
      geom_text(aes(label = format(People_Reached, big.mark = ",")), 
                vjust = -0.5, size = 4, fontface = "bold") +
      theme_minimal() +
      theme(legend.position = "none", text = element_text(size = 12),
            plot.title = element_text(size = 16, hjust = 0.5)) +
      labs(title = "People Reached Comparison", x = "Method", y = "People Reached") +
      scale_fill_manual(values = c("Manual" = "#3498db", "Digital" = "#2ecc71")) +
      scale_y_continuous(labels = function(x) format(x, big.mark = ","))
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$age_demographics_plot <- renderPlotly({
    age_data <- demographics_data[demographics_data$Category == "Age", ]
    
    plot_data <- data.frame(
      Age_Group = rep(age_data$Group, 2),
      Percentage = c(age_data$Invitees_Pct, age_data$Completed_Pct),
      Type = rep(c("Invitees", "Completed Survey"), each = nrow(age_data)))
    
    p <- ggplot(plot_data, aes(x = Age_Group, y = Percentage, fill = Type)) +
      geom_col(position = "dodge", alpha = 0.8) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
      labs(title = "Age Distribution: Digital CT Participation",
           x = "Age Group", y = "Percentage") +
      scale_fill_manual(values = c("Invitees" = "#3498db", "Completed Survey" = "#2ecc71"))
    
    ggplotly(p)
  })
  
  output$race_demographics_plot <- renderPlotly({
    race_data <- demographics_data[demographics_data$Category == "Race", ]
    race_data <- race_data[race_data$Invitees_Pct >= 1, ]
    
    plot_data <- data.frame(
      Race = rep(race_data$Group, 2),
      Percentage = c(race_data$Invitees_Pct, race_data$Completed_Pct),
      Type = rep(c("Invitees", "Completed Survey"), each = nrow(race_data)))
    
    p <- ggplot(plot_data, aes(x = Race, y = Percentage, fill = Type)) +
      geom_col(position = "dodge", alpha = 0.8) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
      labs(title = "Race/Ethnicity: Digital CT Participation",
           x = "Race/Ethnicity", y = "Percentage") +
      scale_fill_manual(values = c("Invitees" = "#3498db", "Completed Survey" = "#2ecc71"))
    
    ggplotly(p)
  })
  
  output$language_demographics_plot <- renderPlotly({
    lang_data <- demographics_data[demographics_data$Category == "Language", ]
    
    plot_data <- data.frame(
      Language = rep(lang_data$Group, 2),
      Percentage = c(lang_data$Invitees_Pct, lang_data$Completed_Pct),
      Type = rep(c("Invitees", "Completed Survey"), each = nrow(lang_data)))
    
    p <- ggplot(plot_data, aes(x = Language, y = Percentage, fill = Type)) +
      geom_col(position = "dodge", alpha = 0.8) +
      theme_minimal() +
      theme(legend.position = "bottom") +
      labs(title = "Primary Language: Digital CT Participation",
           x = "Primary Language", y = "Percentage") +
      scale_fill_manual(values = c("Invitees" = "#3498db", "Completed Survey" = "#2ecc71"))
    
    ggplotly(p)
  })
  
  output$total_cost_plot <- renderPlotly({
    calc_data <- calculations()
    
    data <- data.frame(
      Method = c("Manual", "Digital"),
      Total_Cost = c(calc_data$manual_cost_total, calc_data$digital_cost_total))
    
    p <- ggplot(data, aes(x = Method, y = Total_Cost, fill = Method)) +
      geom_col(alpha = 0.8, width = 0.6) +
      geom_text(aes(label = paste0("$", format(round(Total_Cost, 0), big.mark = ","))), 
                vjust = -0.5, size = 4, fontface = "bold") +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(title = "Total Cost Comparison", x = "Method", y = "Total Cost ($)") +
      scale_fill_manual(values = c("Manual" = "#3498db", "Digital" = "#2ecc71")) +
      scale_y_continuous(labels = function(x) paste0("$", format(x, big.mark = ",")))
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$cost_per_person_plot <- renderPlotly({
    calc_data <- calculations()
    
    data <- data.frame(
      Method = c("Manual", "Digital"),
      Cost_Per_Person = c(calc_data$manual_cost_per_person, calc_data$digital_cost_per_person))
    
    p <- ggplot(data, aes(x = Method, y = Cost_Per_Person, fill = Method)) +
      geom_col(alpha = 0.8, width = 0.6) +
      geom_text(aes(label = paste0("$", round(Cost_Per_Person, 2))), 
                vjust = -0.5, size = 4, fontface = "bold") +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(title = "Cost per Person Reached", x = "Method", y = "Cost per Person ($)") +
      scale_fill_manual(values = c("Manual" = "#3498db", "Digital" = "#2ecc71")) +
      scale_y_continuous(labels = function(x) paste0("$", format(x, big.mark = ",")))
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$cost_breakdown_plot <- renderPlotly({
    calc_data <- calculations()
    
    combined_data <- data.frame(
      Method = c("Manual", "Manual", "Digital", "Digital", "Digital"),
      Component = c("Daily Staff", "Fixed Costs", "Daily Staff", "Maintenance", "Fixed Costs"),
      Cost = c(calc_data$cost_components$manual$daily_staff,
               calc_data$cost_components$manual$fixed,
               calc_data$cost_components$digital$daily_staff,
               calc_data$cost_components$digital$maintenance,
               calc_data$cost_components$digital$fixed)
    )
    
    combined_data <- combined_data[combined_data$Cost > 0, ]
    
    if(nrow(combined_data) > 0) {
      p <- ggplot(combined_data, aes(x = Method, y = Cost, fill = Component)) +
        geom_col(position = "stack", alpha = 0.8) +
        theme_minimal() +
        labs(title = "Cost Breakdown by Component", x = "Method", y = "Cost ($)") +
        scale_y_continuous(labels = function(x) paste0("$", format(x, big.mark = ","))) +
        theme(legend.position = "bottom")
      
      ggplotly(p)
    } else {
      ggplot() + theme_void()
    }
  })
  
  # Summary Metrics Outputs
  output$people_per_hour_text <- renderText({
    calc_data <- calculations()
    
    manual_hours <- input$manual_staff_hours_per_day * input$analysis_period_days
    manual_pph <- calc_data$manual_reached / manual_hours
    
    digital_hours <- (calc_data$digital_reached / input$analysis_period_days) * 
      ((input$survey_review_time + input$unsuccessful_contact_time) / 60)
    digital_pph <- if(digital_hours > 0) calc_data$digital_reached / (digital_hours * input$analysis_period_days) else 0
    
    paste0("Manual: ", round(manual_pph, 2), " people/hour  Digital: ", round(digital_pph, 2), " people/hour")
  })
  
  output$cost_efficiency_text <- renderText({
    calc_data <- calculations()
    paste0("Manual: $", round(calc_data$manual_cost_per_person, 2), " per person  Digital: $", 
           round(calc_data$digital_cost_per_person, 2), " per person")
  })
  
  output$breakeven_text <- renderText({
    calc_data <- calculations()
    
    manual_fixed <- input$fixed_costs_manual
    manual_var_cost_per_person <- (calc_data$manual_cost_total - manual_fixed) / calc_data$manual_reached
    
    digital_fixed <- input$fixed_costs_digital
    digital_var_cost_per_person <- (calc_data$digital_cost_total - digital_fixed) / calc_data$digital_reached
    
    if(abs(manual_var_cost_per_person - digital_var_cost_per_person) < 0.01) {
      "No clear breakeven point: Both methods have similar variable costs per person."
    } else {
      breakeven_people <- (manual_fixed - digital_fixed) / (digital_var_cost_per_person - manual_var_cost_per_person)
      
      if(breakeven_people > 0 && breakeven_people < 10000000) {
        paste0("Breakeven at approximately ", format(round(breakeven_people, 0), big.mark = ","), 
               " people reached (where both methods cost the same per person)")
      } else {
        "No practical breakeven point with current parameters - one method is more efficient across all scales."
      }
    }
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)