! comment the #define to disable debug logging
#define DEBUG

! this module is a bunch of ugly stuff to print debug stuff to a file.
module debug

    IMPLICIT NONE

    INTEGER, parameter :: debugfile = 20        ! I/O unit for the debug file.
    LOGICAL :: debugIsOpen
    CHARACTER(8) :: date
    CHARACTER(10) :: time
    
    CONTAINS


    ! DEBUG LOG    
    subroutine debug_log(dbg)
        CHARACTER(len=*), intent(in) :: dbg

#ifdef DEBUG    
        CALL DATE_AND_TIME(date, time) 
        IF(debugIsOpen .EQV. .false.) THEN
            OPEN(unit=DEBUGFILE,file="debug.txt",action="write",status="replace")
            debugIsOpen = .true.
            WRITE(debugfile, '(a,a,a,a)') "** opening debug file : ", date, " / ", time
        END IF
        WRITE(debugfile, '(a, a)', advance="no") time, " : "
        WRITE(debugfile, '(a)') dbg

#endif

    END subroutine debug_log
    
    ! DEBUG CLOSE
    subroutine debug_close()
#ifdef debug
        CALL debug_log("closing debug file")
        CLOSE(debugfile)
#endif
    END subroutine debug_close
    
end module debug