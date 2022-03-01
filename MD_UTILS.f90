MODULE MD_UTILS

    CONTAINS

!*********************************************************************
    FUNCTION DIST_w_PBC(coord_a, coord_b, cell_size) RESULT(res)

        USE KINDS
        IMPLICIT NONE
        REAL(KIND=DP), INTENT(IN) :: coord_a
        REAL(KIND=DP), INTENT(IN) :: coord_b
        REAL(KIND=DP), INTENT(IN) :: cell_size

        REAL(KIND=DP) :: d, halfbox

        halfbox = 2.0/cell_size

        d = coord_b - coord_a

        ! minimal distance convenction
        d = d - cell_size * int(d*halfbox)

        res = d

    END FUNCTION DIST_w_PBC
!*********************************************************************
    FUNCTION num_lines_file(filein) RESULT(res)

        IMPLICIT NONE
        INTEGER, INTENT(IN) :: filein
        INTEGER :: io
        CHARACTER*200 :: inputline
        INTEGER :: n_line
        n_line = 0
        DO
            READ(filein,*, IOSTAT=io) inputline
            IF (io > 0) THEN
                WRITE(*,*) 'Check input. Something was wrong'
                EXIT
            ELSE IF (io < 0) THEN
                EXIT
            ELSE
                n_line = n_line + 1
            ENDIF
        ENDDO

        REWIND(filein)

        res = n_line

    END FUNCTION num_lines_file
!*********************************************************************

END MODULE MD_UTILS
