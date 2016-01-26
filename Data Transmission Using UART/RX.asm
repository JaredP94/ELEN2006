list	p = 16F690
#include    <p16F690.inc>

__CONFIG    _MCLRE_ON & _BOR_ON & _PWRTE_OFF & _FCMEN_OFF & _CP_OFF & _CPD_OFF & _IESO_OFF & _WDTE_OFF & _INTRC_OSC_NOCLKOUT

UDATA_SHR
whold	res 1       ; temperaray register variable
shold	res 1       ; temperary status variable
Display_var res 1   ; display variable

org 0
	goto start
		
org 0004h
	goto interupt
	
start

    banksel OSCCON     ; default 4Mhz
    movlw B'11010000' 
    movwf OSCCON  

    banksel ANSEL       ; setting pins to output
    clrf ANSEL
    banksel ANSELH
    clrf ANSELH

    banksel TRISC
    movlw B'00000000'    ; clearing display tris
    movwf TRISC 
    
    banksel INTCON      ; setting up interupt
    movlw B'11000000' 
    movwf INTCON

    bsf INTCON, GIE
    bsf INTCON, PEIE

    banksel SPBRGH
    clrf SPBRGH
    
    banksel SPBRG     ; setting up baud rate
    movlw d'12'	
    movwf SPBRG

    banksel BAUDCTL
    bcf BAUDCTL, BRG16
    
    banksel TXSTA      ; low speed baud
    bcf TXSTA, BRGH 
    
    banksel RCSTA
    bsf RCSTA, SPEN 
    
    banksel TXSTA
    bcf TXSTA, SYNC 
    
    banksel RCSTA       ; setting up interupt
    bsf RCSTA, CREN 
    
    banksel PIE1
    bsf PIE1, RCIE 
    
begin

    banksel Display_var
    movf Display_var,w    ; moving RCREG temp variable in register
    call lookUp
    banksel TRISC
    movwf TRISC

goto begin


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
   

interupt
   
    banksel whold
    movwf whold
    swapf STATUS,w
    clrf STATUS
    movwf shold
      
    banksel RCREG
    movf RCREG,w
    movwf Display_var
    bcf PIR1,RCIF

    swapf shold,w
    movwf STATUS     
    SWAPF whold,f
    SWAPF whold,w
   
    retfie

END