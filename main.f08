program fortiche

    !use section (if any)
    use ISO_FORTRAN_ENV
    use debug
    
    implicit none
    
    ! constants
    CHARACTER (len=8) :: name = "fortiche"
    CHARACTER (len=2) :: version = "1"
    CHARACTER (len=5) :: author = "ker2x"

    ! parsing variable stuff here
    CHARACTER (len= :), allocatable :: input
    CHARACTER (len=4096) :: inbuffer            ! frustrating but... well, fortran.
    INTEGER :: input_length
    
    ! engine variable
    LOGICAL :: ponder
    
    
    !INTEGER (KIND=8) :: bitfield                ! a 64bit signed integer
    
    ! init stuff here
    CALL debug_log("Initializing Fortiche engine")
    ponder = .FALSE.
    
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! main loop (responding to UCI commands)
    CALL debug_log("Entering main loop")
    DO
    
        ! get input
        READ(*, '(a)') inbuffer     ! it bloack until a command is issued. computation must be done in another thread
        input = TRIM(inbuffer)
        input_length = LEN(input)
        CALL debug_log("<- " // input)
        
        ! main parsing stuff

        !uci
        IF(input .EQ. 'uci') THEN
            CALL debug_log("  -> printing uci info")
            WRITE(*, '(a, a, a, a)') "id name ", name, " ", version
            WRITE(*, '(a, a)') "id author ", author
            !add more stuff before uciok if required
            WRITE(*, '(a)') "uciok"

        !isready
        ELSE IF(input .EQ. 'isready') THEN
            CALL debug_log("  -> readyok")
            WRITE(*, '(g0)') "readyok"
            
        !ucinewgame
        ELSE IF(input .EQ. 'ucinewgame') THEN
            CALL debug_log("  -> not implemented : reset board and start a new game")
        
        !position
        ELSE IF((input_length >= 8) .AND. input(1:8) == 'position') THEN
            CALL debug_log("  -> not implemented : position")

        !go ...
        ELSE IF((LEN(input) >= 2) .AND. input(1:2) == 'go') THEN
        
            !go ponder
            IF(input == 'go ponder') THEN
                IF(ponder .EQV. .FALSE.) THEN
                    CALL debug_log("  -> not implemented : go ponder")
                    ponder = .TRUE.
                ELSE
                    CALL debug_log("  -> go ponder was called but 'ponder' was already set to true")
                END IF
                
            !go searchmoves ...
            ELSE IF((input_length >= 14) .AND. input(1:14) == 'go searchmoves') THEN
                CALL debug_log("  -> not implemented : go searchmoves")
            
            !movetime ... (in ms)
            ELSE IF((input_length >= 11) .AND. input(1:11) == 'go movetime') THEN
                CALL debug_log("  -> not implemented : set movetime in ms") 
            
            !depth ... (search x plies only)
            ELSE IF ((input_length >= 8) .AND. input(1:8) == "go depth") THEN
                CALL debug_log("  -> not implemented : search x plies only")
            
            !infinite
            ELSE IF(input == 'go infinite') THEN
                CALL debug_log("  -> not implemented : search intil stop command is issued")
            
            !wtime ... (in ms)
            ELSE IF((input_length >=  8) .AND. input(1:8) == "go wtime") THEN
                CALL debug_log("  -> not implemented : wtime")

            !btime
            ELSE IF((input_length >=  8) .AND. input(1:8) == "go btime") THEN
                CALL debug_log("  -> not implemented : btime")
            
            END IF
            
        !ponderhit
        ELSE IF(input == "ponderhit") THEN
            CALL debug_log("  -> not implemented : ponderhit")
        
        !stop
        ELSE IF(input == "stop") THEN
            CALL debug_log("  -> not implemented : stop")
            IF(ponder .EQV. .TRUE.) THEN
                CALL debug_log("  -> the engine was pondering, stop pondering")
                ponder = .FALSE.
            END IF
        
        !setoption
        ELSE IF((input_length >= 9) .AND. input(1:9) == 'setoption') THEN
            CALL debug_log("  -> not implemented : setoption")
        
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
            CALL debug_log("  -> quit command issued, exiting main loop")
            EXIT
        
        !non uci command
            !nothing yet
            
        !unknown command
        ELSE
            CALL debug_log("  -> ignoring invalid command : " // input)
        END IF

    end do
    
    ! bye
    CALL debug_log("closing, bye")
    CALL debug_close()
    
end program fortiche
