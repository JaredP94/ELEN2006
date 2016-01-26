 
list	p = 16F690
#include    <p16F690.inc>

__CONFIG    _MCLRE_ON & _BOR_ON & _PWRTE_OFF & _FCMEN_OFF & _CP_OFF & _CPD_OFF & _IESO_OFF & _WDTE_OFF & _INTRC_OSC_NOCLKOUT


UDATA_SHR
    number      res 1
    tempStat	res 1
    tempW	res 1
    delayVar	res 1
  
  
ORG 0x00
goto initialise
  
ORG 0004h 
GOTO interruptServiceRoutine
  
initialise

    
;************GENERAL CONFIGS****************************
;Sets everything to digital
banksel	ANSEL
clrf    ANSEL
clrf    ANSELH

banksel OSCCON
movlw b'11010000'	;4MHz oscillator frequency
movwf OSCCON
	
Banksel PORTC
movlw b'00000000'	;Sets all of C as outputs
movwf PORTC

    
;************INTERRUPT CONFIGS**************************
;Enable Interrupt Configs
    BANKSEL INTCON
    movlw B'11001000' ;Enables internal interrupt flag
    movwf INTCON
    bsf INTCON, GIE
    bsf INTCON, PEIE
    bsf	INTCON,	INTE
    
    
;Inputs RA2 Configs
    BANKSEL OPTION_REG
    movlw B'01000000' ;Triggers on rising edge
    movwf OPTION_REG 
 
    
;************TRANSMISSION CONFIGS**************************
;Transmission Set-Up
    banksel BAUDCTL
    bcf BAUDCTL, BRG16	;Baud Rate in 8 bit mode
 
    banksel TXSTA
    bcf TXSTA, BRGH	;Low-Speed
    
    
    BANKSEL RCSTA
    bsf	RCSTA, SPEN
   
    banksel TXSTA
    bcf TXSTA, SYNC	;Set EUSART to Asynchronous Mode
    BSF TXSTA, TXEN	;Set the EUSART to Transmit Mode
    
    BANKSEL SPBRGH
    clrf    SPBRGH
    movlw D'12' ;Sets baud rate to 4800
    movwf SPBRG
    
    

 
movlw	b'00000000' ;Initialises number to be sent  and incremented as zero
movwf	number
goto	mainStuff   ;Start program
 
  
;**************MAIN***********************************
mainStuff
    CLRF    STATUS
 movf number, w    ;Move number to be sent into TXREG to transmit
 banksel TXREG
 movwf TXREG
 btfss	number, 3
 goto	display
 btfss number, 1
 goto	display
 movlw	b'00000000'
 movwf	number
 goto display
  

  
  
;*************INTERRUPT********************************
   interruptServiceRoutine
	
    movwf   tempW
    swapf   STATUS,W
    clrf    STATUS
    movwf   tempStat
    
	INCF number,F 
	BCF INTCON, INTF 
	
    swapf   tempStat,w
    movwf   STATUS
    swapf   tempW,f
    swapf   tempW,w
   RETFIE

  
  
;**************DISPLAY****************************************
display
  movfw number
  call lookUp	
  banksel TRISC
  movwf TRISC

goto mainStuff

lookUp
	addwf PCL, f
	retlw b'10000001'
	retlw b'11001111'
	retlw b'10010010'
	retlw b'10000110'
	retlw b'11001100'
	retlw b'10100100'
	retlw b'10100000'
	retlw b'10001111'
	retlw b'10000000'
	retlw b'10001100'
    return
   
    
;*************DELAY******************************
  DELAY
	movlw D'255'
	movwf delayVar
	
      loop
	    decfsz delayVar
      goto loop
   RETURN
END



