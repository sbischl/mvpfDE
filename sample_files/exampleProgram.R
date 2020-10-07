#----------------------------------------------------------------------------------------------------------------------#
# Example Program:
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Author & Author (2020)

# Each of the programName.R files must include a function with the same name. (Without the .R at the end)
# This function contains all the logic to calculate the MVPF.
# Furthermore, the function has to meet the following requirements:
 # 1) Take bootstrap_replication with default value 0 as input. Further inputs are generally possible to specifiy different
 # assumptions, but all inputs must have default values.
 # 2) Begin with
 # program_name <- toString(match.call()[1])
 # estimates <- getEstimates(program_name, bootstrap_replication)
 # These two lines of code load the estimates from the excel file into a dataframe called estimates.
 # A estimate called myEstimate from the excel file will be accessible through estimates$myEstimate
 # This strucutre automatically takes care of bootstrapping. For the bootstrap, the program function will be repeatedly called
 # with randomly drawn estimates in accordance with the distributions specified in the excel file.
 # 3) Returns a list which has to contain the keys: "willingness_to_pay" , "government_net_costs" and "prices_year".
 # willingness_to_pay & government_net_costs are the numerator and denominator of the MVPF respectively. prices_year
 # is the year whose price level is used for the calculation. Additional return values can be added to the list. These will
 # also be bootstrapped and output when running main.R

exampleProgram <- function (bootstrap_replication = 0, internalize_carbon_emissions = 1) {
  program_name <- toString(match.call()[1]) # Do not change this line
  estimates <- getEstimates(program_name, bootstrap_replication)  # Do not change this line

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions:
  #--------------------------------------------------------------------------------------------------------------------#


  #--------------------------------------------------------------------------------------------------------------------#
  # MVPF Calculation
  #--------------------------------------------------------------------------------------------------------------------#

  return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs,
                        prices_year = prices_year)

  return(return_values)
}