SUBROUTINE INIT
  USE GLOBVAR
  USE STATISTICS
  IMPLICIT NONE

    INTEGER :: i
    REAL(8) :: aux(n_individuals)

    write(*,*) 'going to open datafile'
    OPEN(1,file='ratioeduc.out')
    DO i = 1, n_individuals
        READ(1,fmt=*) aux(i)
        dataset(i,1) = aux(i)
    END DO
    CLOSE(1)

    OPEN(1,file='ratiocrime.out')
    DO i = 1, n_individuals
        READ(1,fmt=*) aux(i)
        dataset(i,2) = aux(i)
    END DO
    CLOSE(1)

END SUBROUTINE INIT



