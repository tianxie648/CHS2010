MODULE GLOBVAR
  IMPLICIT NONE

    INTEGER, PARAMETER :: n_individuals = 2500	    ! Number of individuals in dataset 
    INTEGER, PARAMETER :: n_variables = 2			! Number of variables in dataset
    INTEGER, PARAMETER :: n_bins = 50					
    REAL(8) :: logratio(n_individuals,n_variables)
    REAL(8) :: dataset(n_individuals,n_variables)

END MODULE GLOBVAR
