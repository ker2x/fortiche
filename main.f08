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
    LOGICAL :: isStopped
    
    
    !INTEGER (KIND=8) :: bitfield                ! a 64bit signed integer
    
    ! init stuff here
    CALL debug_log("Initializing " // name // " engine version " // version)
    ponder = .FALSE.
    isStopped = .FALSE.
    
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! main loop (responding to UCI commands)
    ! http://wbec-ridderkerk.nl/html/UCIProtocol.html
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    CALL debug_log("Entering main loop")
    DO
    
        ! get input
        READ(*, '(a)') inbuffer     ! it bloack until a command is issued. computation must be done in another thread
        input = TRIM(inbuffer)
        input_length = LEN(input)
        CALL debug_log("<- " // input)
        
        ! main parsing stuff
        
        ! ignore commands while running except : stop, quit, ponderhit
        IF((isStopped .EQV. .FALSE.) .AND. ((input /= "stop") .AND. (input /= "quit") .AND. (input /= "ponderhit"))) THEN
            CALL debug_log("  -> " // input // " was called but ignored because the engine is not stopped")
            CYCLE
        END IF

        !uci
        IF(input .EQ. 'uci') THEN
            CALL debug_log("  -> printing uci info")
            WRITE(*, '(a)') "id name " // name // " " // version
            WRITE(*, '(a)') "id author " // author
            !add more stuff before uciok if required
            WRITE(*, '(a)') "uciok"

        !isready
        ELSE IF(input .EQ. 'isready') THEN
            CALL debug_log("  -> readyok")
            WRITE(*, '(g0)') "readyok"
            
        !ucinewgame
        ELSE IF(input .EQ. 'ucinewgame') THEN
            CALL debug_log("  -> not implemented : reset board and start a new game")
        
        !position [fen | startops] ...
        ELSE IF((input_length >= 8) .AND. input(1:8) == 'position') THEN
            CALL debug_log("  -> not implemented : position")

        !go ...
        ELSE IF((input_length >= 2) .AND. (input(1:2) == 'go') .AND. (isStopped .EQV. .TRUE.)) THEN
        
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
            
            !movetime x (in ms)
            ELSE IF((input_length >= 11) .AND. input(1:11) == 'go movetime') THEN
                CALL debug_log("  -> not implemented : set movetime in ms") 
            
            !depth x (search x plies only)
            ELSE IF ((input_length >= 8) .AND. input(1:8) == "go depth") THEN
                CALL debug_log("  -> not implemented : search x plies only")
            
            !infinite
            ELSE IF(input == 'go infinite') THEN
                CALL debug_log("  -> not implemented : search intil stop command is issued")
            
            !wtime x (in ms)
            ELSE IF((input_length >=  8) .AND. input(1:8) == "go wtime") THEN
                CALL debug_log("  -> not implemented : go wtime")

            !btime x
            ELSE IF((input_length >=  8) .AND. input(1:8) == "go btime") THEN
                CALL debug_log("  -> not implemented : go btime")
                
            !movestogo x
            ELSE IF((input_length >= 12) .AND. input(1:12) == "go movestogo") THEN
                CALL debug_log("  -> not implemented : go movestogo")
            
            !nodes x
            ELSE IF((input_length >= 8) .AND. input(1:8) == "go nodes") THEN
                CALL debug_log("  -> not implemented : go nodes")
            
            !mate x
            ELSE IF((input_length >= 7) .AND. input(1:7) == "go mate") THEN
                CALL debug_log("  -> not implemented : go mate")
            
            !winc x
            ELSE IF((input_length >= 7) .AND. input(1:7) == "go winc") THEN
                CALL debug_log("  -> not implemented : go winc")
            
            !binc x
            ELSE IF((input_length >= 7) .AND. input(1:7) == "go binc") THEN
                CALL debug_log("  -> not implemented : go binc")
            
            !???
            ELSE
                CALL debug_log("  -> invalid go ... command")
            
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
            IF(isStopped .EQV. .FALSE.) THEN
                CALL debug_log("  -> set isStop to true")
                isStopped = .TRUE.
            ELSE
                CALL debug_log("  -> engine was already stopped")
            END IF
        
        !setoption name ... [value ...]
        ELSE IF((input_length >= 9) .AND. input(1:9) == 'setoption') THEN
            CALL debug_log("  -> not implemented : setoption")
        
            !thread ... (setup number of thread)
            
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
