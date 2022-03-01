MODULE MW_UTILS

    CONTAINS

!*********************************************************************
    FUNCTION GET_BOX(filein, typ) RESULT(res)

        USE KINDS
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: filein
        CHARACTER(LEN=3), INTENT(IN) :: typ
        CHARACTER(LEN=200) :: crap
        INTEGER :: io
        REAL(KIND=DP) :: box_x, box_y, box_z
        REAL(KIND=DP), DIMENSION(3) :: res
        REAL(KIND=DP) :: bohr = 0.529177210903_dp 

        READ(filein,*, IOSTAT=io) crap
        READ(filein,*, IOSTAT=io) crap 
        READ(filein,*, IOSTAT=io) crap
        READ(filein,*, IOSTAT=io) crap
        READ(filein,*, IOSTAT=io) crap
        READ(filein,*, IOSTAT=io) box_x, box_y, box_z
        
        IF (typ == 'ang') THEN
            box_x = box_x * bohr
            box_y = box_y * bohr
            box_z = box_z * bohr
        END IF

        res = (/ box_x, box_y, box_z /)

    END FUNCTION GET_BOX
!*********************************************************************
    FUNCTION GET_NATOM(filein, choice) RESULT(res)

        USE KINDS
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: filein
        INTEGER :: io, n_tot, n_elec, n_bulk
        INTEGER :: res
        CHARACTER(LEN=200) :: crap
        CHARACTER(LEN=4), INTENT(IN) :: choice
        
        READ(filein,*, IOSTAT=io) crap
        READ(filein,*, IOSTAT=io) crap
        READ(filein,*, IOSTAT=io) crap, n_tot
        READ(filein,*, IOSTAT=io) crap, n_elec

        n_bulk = n_tot - n_elec 

        REWIND(filein)

        IF (choice == 'bulk') THEN
            res = n_bulk
        ELSE IF (choice == 'elec') THEN
            res = n_elec
        ELSE IF (choice == 'both') THEN
            res = n_tot
        END IF
     
    END FUNCTION GET_NATOM

!*********************************************************************

END MODULE MW_UTILS
