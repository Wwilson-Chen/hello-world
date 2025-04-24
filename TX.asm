    PROCESSOR	10F322
    RADIX	DEC
    
    config FOSC = INTOSC    ; Oscillator Selection bits (INTOSC oscillator: CLKIN function disabled)
    config BOREN = OFF      ; Brown-out Reset Enable (Brown-out Reset disabled)
    config WDTE = OFF       ; Watchdog Timer Enable (WDT disabled)
    config PWRTE = ON       ; Power-up Timer Enable bit (PWRT enabled)
    config MCLRE = ON       ; MCLR Pin Function Select bit (MCLR pin function is RESET input)
    config CP = OFF         ; Code Protection bit (Program memory code protection is disabled)
    config LVP = ON         ; Low-Voltage Programming Enable (Low-voltage programming enabled)
    config LPBOR = OFF      ; Brown-out Reset Selection bits (BOR disabled)
    config BORV = LO        ; Brown-out Reset Voltage Selection (Brown-out Reset Voltage (Vbor), low trip point selected.)
    config WRT = OFF        ; Flash Memory Self-Write Protection (Write protection off)

; This include file pulls in the proper definitions for the registers for our processor   
#include <xc.inc>

; as an example of a macro definition, here is a reproduction of the pageselw
; macro that was included in the old assembler.
pageselw    macro	    Table
    movlw	    (Table>>8)
    movwf	    PCLATH
    endm
	
; This PSECT command specifies that we want to locate our variables in Bank0   
    PSECT   udata_bank0
; We make them GLOBAL to help the debugger find them, that's all
    GLOBAL  TX_TARGET_BYTE, TX_BYTE_REG, TX_COUNTER, TX_END_FLAG
TX_TARGET_BYTE: ds  1
TX_BYTE_REG:    ds  1
TX_COUNTER:     ds  1
TX_END_FLAG:    dS  1

;
;   Data space used by interrupt handler to save context is
;   placed at the top of bank 0 for upward compatibility
    PSECT   Isr_data,global,class=RAM,space=1,delta=1,noexec
;
    GLOBAL  WREG_save,STATUS_save
;
WREG_save:      DS  1
STATUS_save:    DS  1

; This PSECT command is used to locate our code. The actual address is specifed
; in the project properties box "pic-as Global Options -> Additional options"
; Be sure to add this line there : 
;   -Wa,-a -Wl,-DCODE=2,-pReset_Vec=00h,-pISR_Vec=04h,-pIsr_data=07eh,-Map=test.map    
    PSECT   Reset_Vec,class=CODE,delta=2
    
; The GLOBAL declaration is not strictly necessary in a single module program.    
    GLOBAL  ResetVector
ResetVector:
    goto    Main

; This PSECT places the ISR handler at the ISR_Vec address in the additional
; options line shown earlier. For our processor this needs to be at address 04h
    PSECT   ISR_Vec,class=CODE,delta=2

ISR_Vector:
; start off by saving the context
    movwf   WREG_save
    swapf   STATUS,W
    movwf   STATUS_save
;
IsrHandler:
    btfsc   PIR1, PIR1_NCO1IF_POSN ; Test if NCO1 interrupt flag is set, skip if clear
    goto    IsrNCO1 ; if flag is set, go to ISR of NCO1
    goto    IsrExit ; if flag is clear, go to ISR exit

;;;;;;;;;;;;;;;;;;;;;;;;;; NCO1 ISR Start Here ;;;;;;;;;;;;;;;;;;
IsrNCO1:
    bcf	    PIR1, PIR1_NCO1IF_POSN ;clear NCO1 interrupt flag
    
    ; check if TX_COUNTER is 9
    movlw   9                      ; move 9 into w
    subwf   TX_COUNTER, W          ; sub w from TX_COUNTER(f-w), result in w
    btfsc   STATUS, STATUS_Z_POSN  ; test if z bit is 1 (f=w), if so goto TX_COUNTER_Is9, skip if clear
    goto    TX_COUNTER_Is9
    
    ; check if TX_COUNTER is 8
    movlw   8                      ; move 8 into w
    subwf   TX_COUNTER, W          ; sub w from TX_COUNTER(f-w), result in w
    btfsc   STATUS, STATUS_Z_POSN  ; test if z bit is 1 (f=w), if so goto TX_COUNTER_Is9, skip if clear
    goto    TX_COUNTER_Is8
    
    ; if not 9 or 8, handle bit transmission
    goto    TX_BIT_TRANSMISSION
  
TX_COUNTER_Is9:
    bsf	    TX_END_FLAG, 0              ; Set TX end flag, indicate transmission is done 
    bcf     NCO1CON, NCO1CON_N1EN_POSN  ; disable NCO1
    bcf     PIE1, PIE1_NCO1IE_POSN      ; disable NCO1 interrupt
    goto    IsrExit                     ; end of transmission, exit ISR

TX_COUNTER_Is8:
    bsf     LATA, LATA_LATA1_POSN    ; set RA1 high for end bit
    incf    TX_COUNTER               ; increment counter
    goto    IsrExit                  ; finish then exit isr
    
TX_BIT_TRANSMISSION:
    incf    TX_COUNTER, f           ; increment counter
    btfsc   TX_BYTE_REG, 0          ; check current bit, skip if clear(0)
    bsf	    LATA, LATA_LATA1_POSN   ; set RA1 output high, since not skip means the bit is 1
    btfss   TX_BYTE_REG, 0          ; check current bit, skip if set(1)
    bcf	    LATA, LATA_LATA1_POSN   ; set RA1 output low, since skip means the bit is 0
    rrf	    TX_BYTE_REG, f          ; shift bits right 1 position to transmit next bit in line
    goto    IsrExit                 ; exit isr    
;;;;;;;;;;;;;;;;;;;;;;;;;; NCO1 ISR End Here ;;;;;;;;;;;;;;;;;;    

IsrExit:
; now we restore the context using a funny sequence to keep from modifying the
; status register after it has been restored.
    swapf   STATUS_save, w
    movwf   STATUS
    swapf   WREG_save, f
    swapf   WREG_save, w
    retfie                      ; Return from interrupt
    
; Space for lookup tables    
BeginTables:
; look up tables go here    
EndTables:

; After assembly, check the list file or map file to insure that BeginTables
; and EndTables are on the same 256 byte 'page'. Both 0x0?? or 0x1??
; you find the list file in YourProjectFolder.X\build\default\production
; you find the map file (test.map) in the root of the project folder.

TX_Init:
    ; initialize RA1 pin for TX
    bcf	    TRISA, TRISA_TRISA1_POSN  ; clear TRISA1 to make RA1 output pin
    bcf	    ANSELA, ANSELA_ANSA1_POSN ; clear ANSELA1 to disable analog
    bsf	    LATA, LATA_LATA1_POSN     ; set RA1 high, as TX idle high
    bcf     WPUA, WPUA_WPUA1_POSN     ; Disable weak pull-ups on RA1
    
    ; initializ NCO1
    movlw   157                            ; set increment 157
    movwf   NCO1INCL                       ; move 157 to NCO1 increment register, make approximate BAUD 2400
    clrf    NCO1INCH                       ; clear ICO1 high bit
    bcf	    NCO1CLK, NCO1CLK_N1CKS0_POSN   ; Select HFINTOSC as clock input, N1CKS =0b10
    bsf	    NCO1CLK, NCO1CLK_N1CKS1_POSN
    ;bsf     OSCCON, OSCCON_IRCF0_POSN      ; set clock freq as 16MHz, default 110, change to 111
    
    ; enable interrupt
    bsf     INTCON, INTCON_GIE_POSN   ; enable global interrupts
    bsf     INTCON, INTCON_PEIE_POSN  ; enable peripheral interrupts
   
    clrf    TX_TARGET_BYTE            ; clear the byte to transmit, let it start from zero
    clrf    TX_BYTE_REG               ; clear byte reg
    bsf     TX_END_FLAG, 0            ; initiate first TX
    return                            ; end TX_init

TX_Start:
    ; initialize the next target tx byte
    clrf    TX_COUNTER                ; clear counter to start from 0
    incf    TX_TARGET_BYTE, f         ; increment the byte to transmit
    movf    TX_TARGET_BYTE, w         ; load w register with byte to transmit 
    movwf   TX_BYTE_REG               ; load register with byte to transmit 
    bcf	    LATA, LATA_LATA1_POSN     ; Set output RA1 low for start bit 
    clrf    TX_END_FLAG               ; clear end flag
    
    ; clear NCO1 accumulator
    clrf    NCO1ACCL                  ; clear low byte
    clrf    NCO1ACCH                  ; clear high byte
    clrf    NCO1ACCU                  ; clear upper byte
    
    bcf     PIR1, PIR1_NCO1IF_POSN       ; clear the NCO1 interrupt flag
    bsf     NCO1CON, NCO1CON_N1EN_POSN   ; enable NCO1
    bsf     PIE1, PIE1_NCO1IE_POSN       ; enable NCO1 interrupts
    
    return
    
Main:
    call    TX_Init
    
Loop:
    btfsc   TX_END_FLAG, 0       ; Test end flag. If clear, restart loop
    goto    TX_Start
    goto    Loop
	
END	ResetVector
