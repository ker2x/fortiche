program fortiche

    !use section (if any)
    use ISO_FORTRAN_ENV
    use debug
    
    implicit none
    
    ! constants
    CHARACTER (len=8) :: name = "fortiche"
    CHARACTER (len=2) :: version = "1"
    CHARACTER (len=5) :: author = "ker2x"

    ! variables and stuff here
    CHARACTER (len= :), allocatable :: input
    CHARACTER (len=4096) :: inbuffer            ! frustrating but... well, fortran.
    
    !INTEGER (KIND=8) :: bitfield                ! a 64bit signed integer
    
    ! init stuff here
    CALL debug_log("Initializing Fortiche engine")
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! main loop (responding to UCI commands)
    CALL debug_log("Entering main loop")
    DO
    
        ! get input
        READ(*, '(a)') inbuffer     ! because apparently you can't have allocation on read so you can't just read "input". meh.
        input = TRIM(inbuffer)
        CALL debug_log(input)
        
        ! main parsing stuff

        !uci
        IF(input .EQ. 'uci') THEN
            CALL debug_log("   printing uci info")
            WRITE(*, '(a, a, a, a)') "id name ", name, " ", version
            WRITE(*, '(a, a)') "id author ", author
            !add more stuff before uciok if required
            WRITE(*, '(a)') "uciok"

        !isready
        ELSE IF(input .EQ. 'isready') THEN
            CALL debug_log("   isready -> readyok")
            WRITE(*, '(g0)') "readyok"
            
        !ucinewgame
        
        !position
        
        !go
        
            !ponder
            
            !searchmoves
            
            !movetime
            
            !depth
            
            !infinite
            
            !wtime/btime
            
        !ponderhit
        
        !stop
        
        !setoption
        
            !thread
            
            !hash
            
            !evalcache
            
            !ponder
            
            !multipv
            
            !buffertime
            
            !syzygypath
            
            !scalematerial
            
            !scalekingsafety
                        
        
        !quit -> exit main loop
        ELSE IF(input .EQ. 'quit') THEN
            CALL debug_log("   quit command issued, exiting main loop")
            EXIT
        
        !non uci command
            !nothing yet
            
        !unknown command
        ELSE
            CALL debug_log("   ignoring invalid command")
        END IF

    end do
    
    ! bye
    CALL debug_log("closing, bye")
    CALL debug_close()
    
end program fortiche
