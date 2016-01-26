list      p=16F690    
#include "p16F690.inc"
errorlevel  -205, -207, -302	;Used to ignore common warning messages
__CONFIG _INTRC_OSC_NOCLKOUT & _WDTE_OFF & _PWRTE_OFF & _MCLRE_ON & _CP_OFF & _CPD_OFF & _BOREN_ON & _IESO_ON & _FCMEN_ON

cblock 	0x20			;define general purpose registers (stored temporarily in memory)
	d1 			;used in delay routine
	d2 			;used in delay routine 
	d3			;used in delay routine
endc
	
UDATA				;Variable definitions with assigned bytes
	adc_out	res 1
	adc_dec	res 2
	mpy_cnt	res 1
	tens	res 1
	ones	res 1
	temp	res 1
	led	res 1
    
	org	0x000		;Begin program at beginning of page memory
	
main   	
	banksel	TRISA
	clrf	TRISA		;set PortA to be outputs
	bsf	TRISA,2		;set RA2 to be an input - used in ADC
	clrf	TRISB		;set PortB to be outputs
	clrf	TRISC		;set PortC to be outputs
	
	banksel	ADCON1
	movlw	b'00010000'	;fosc/8
	movwf	ADCON1
	banksel	ANSEL
	clrf	ANSEL		;Disable all analogue pins (AN pins)
	bsf	ANSEL,2		;RA2 set as analogue pin
	banksel	ADCON0
	movlw	b'00001001'	;Enable ADC input on pin RA2, ADC to be left justified
	movwf	ADCON0
	
loop	bsf	ADCON0,1	    ;ADC routine to take in value outputted by thermistor
wait	btfsc	ADCON0,1	    ;Test ADC until value is acquired
	goto	wait
	movlw	h'002'		    ;Index page for ADC lookup table
	movwf	PCLATH		    ;Store index in PC latch high byte
	banksel	ADRESH		    
	movf	ADRESH,w	    ;Store MSB of ADC input (upper 8 bits of 10 bit value)
	call	getTemp		    ;Acquire value from lookup table based on ADC input
	banksel	adc_out
	movwf	adc_out		    ;Store value from lookup table in variable
	clrf	ADRESH		    ;Clear ADC input to avoid unwanted results for next input
	clrf	PCLATH		    ;Point index back to page 0 of memory
	
	clrf	adc_dec		    ;BCD routine to give decimal output on seven seg
	clrf	adc_dec+1	    ;Clearing variable for use in BCD
	movlw	d'8'
	movwf	mpy_cnt
	movlw	d'100'
	bcf	STATUS,C	    ;Ensure no carry flags are initially raised
	
l_mpy	rrf	adc_out,f	    ;Bit shift through ADC input (dividing)
	btfsc	STATUS,C	    ;Check if any values are carried
	addwf	adc_dec+1,f	    ;Increment variable (represents ones)
	rrf	adc_dec+1,f	    
	rrf	adc_dec,f
	decfsz	mpy_cnt,f	    ;Counter to run 8 times (shift through entire ADC byte)
	goto	l_mpy		    ;Loop until complete
	movf	adc_dec+1,w	    
	movwf	ones		    ;Store carried values into "ones" variable
	clrf	tens		    ;Ensure "tens" variable has no random values
	
l_bcd	movlw	d'10'		    ;Subtraction loop to check for tens from ADC value
	subwf	ones,w		    ;Subracts 10 from "ones" variable
	btfss	STATUS,C	    ;Check for any carry values
	goto	end_bcd		    ;Ends loop when no values left
	movwf	ones		    ;Store result in "ones"
	incf	tens,f		    ;Increment "tens" variable to indicate a tens unit
	goto	l_bcd
end_bcd				    ;End of BCD routine
	
	call	Delay		    ;Delay to ensure conversion is complete
	call	DISP_UPDATE_D1	    ;Display "tens" digit on left seven seg
	call	Delay		    ;Delay so that output is visible
	call	Delay
	call	DISP_UPDATE_D2	    ;Display "ones" digit on right seven seg
	call	Delay		    ;Delay so that output is visible
	call	Led_Display	    ;Sub-routine to change RGB led based on temperature
	goto	loop		    ;Loop indefinitely	
	 
DISP_UPDATE_D1			    ;Sub-routine to display "tens" digit on left seven seg
	bcf	PORTA,1		    ;Set transistor low for left seven seg to allow output display
	movf	tens,w		    ;Move "tens" value into working register
	call	set7seg_R	    ;Sub-routine to display value
	bsf	PORTA,0		    ;Set transistor high for right seven seg to prevent output display
	return			    ;Go back to main routine
    
DISP_UPDATE_D2			    ;Sub-routine to display "ones" digit on right seven seg
	bcf	PORTA,0		    ;Set transistor low for right seven seg to allow output display
	movf	ones,w		    ;Move "ones" value into working register
	call	set7seg_R	    ;Sub-routine to display value
	bsf	PORTA,1		    ;Set transistor high for left seven seg to prevent output display
	return			    ;Go back to main routine
	
set7seg_R			    ;Sub-routine to display a value on the seven seg
	banksel	temp		    ;Variable selection
	movwf	temp		    ;Value in working register stored in variable
	call	get7sC		    ;Code for output retrieved from lookup table
	nop			    ;No operation to ensure value correctly retrieved
	movwf	PORTC		    ;Output code (highs & lows) sent to PortC for display
	nop			    ;No operation to ensure values correctly outputted
	return			    ;Return back to previous routine

Led_Display			    ;Sub-routine to display set colour on RGB led
	movf	tens,w		    ;Value from "tens" variable moved into working register
	call	Led_result	    ;Sub-routine to define output colour based on "tens" value			    
	return			    ;Return to previous routine
	
Delay				    ;Setting up delay loop variables
	movlw	d'2'		    ;Defining count values for delay
	movwf	d1			
	movlw	d'2'			
	movwf	d2
	movlw	d'1'
	movwf	d3
Delay_0					;Running delay
	decfsz	d1,1			;decrement by one, then go to next line
	goto	$+2			;skips two lines
	decfsz	d2, 1
	goto	$+2
	decfsz	d3, 1
	goto	Delay_0
	return				;Return to previous routine

Correct					;Sub-routine for temp between 30-40
	banksel	TRISB			;Select bank to set pins as inputs or outputs in PortB
	bsf	TRISB,4			;RB4 set as input (0V output)
	bsf	TRISB,5			;RB5 set as input (0V output)
	bcf	TRISB,6			;RB6 set as output
	banksel	PORTB			;Select bank to set outputs as high or low in PortB
	bcf	PORTB,6			;RB6 set low (~0V output) - Turns on Green led
	return				;return to previous routine
	
Cold					;Sub-routine for temp below 30
	banksel	TRISB			;Select bank to set pins as inputs or outputs in PortB
	bsf	TRISB,4			;RB4 set as input (0V output)
	bcf	TRISB,5			;RB5 set as output
	bsf	TRISB,6			;RB6 set as input (0V output)
	banksel	PORTB			;Select bank to set outputs as high or low in PortB
	bcf	PORTB,5			;RB5 set low (~0V output) - Turns on Blue led
	return				;Return to previous routine
	
Hot					;Sub-routine for temp above 40
	banksel	TRISB			;Select bank to set pins as inputs or outputs in PortB
	bcf	TRISB,4			;RB4 set as output
	bsf	TRISB,5			;RB5 set as input (0V input)
  	bsf	TRISB,6			;RB6 set as input (0V input)
	banksel	PORTB			;Select bank to set outouts as high or low in PortB
	bcf	PORTB,4			;RB6 set low (~0V) - Turns on Red led
	return				;Return to previous routine
		
get7sC					;Lookup table for decimals outputs to seven segs
        addwf   PCL,F			;Adds contents to program counter register (i.e uses index to find value)
	retlw   b'01000000'		;0 into working register
	retlw   b'01111001'		;1 into working register 
	retlw   b'00100100'		;2 into working register
	retlw   b'00110000'		;3 into working register
	retlw   b'00011001'		;4 into working register
	retlw   b'00010010'		;5 into working register
	retlw   b'00000010'		;6 into working register
	retlw   b'01111000'		;7 into working register
	retlw   b'00000000'		;8 into working register
	retlw   b'00011000'		;9 into working register

Led_result				;Lookup table for temperature results to be displayed on Led
	addwf	PCL,F			;Adds contents to program counter register (i.e uses index to find value)
	goto	Cold			;0 degrees (all Celsius)
	goto	Cold			;10 degrees
	goto	Cold			;20 degrees
	goto	Correct			;30 degrees
	goto	Hot			;40 degrees
	goto	Hot			;50 degrees
	goto	Hot			;60 degrees
	goto	Hot			;70 degrees
    	goto	Hot			;80 degrees
	goto	Hot			;90 degrees
	
	org	h'200'			;Memory location at page 2
getTemp					;Lookup table for temperature values corresponding to ADC input
	addwf	PCL,F			;Adds contents to program counter register (i.e uses index to find value)
	retlw	d'0'
	retlw	d'0'
	retlw	d'0'
	retlw	d'0'
	retlw	d'0'
	retlw	d'0'
	retlw	d'0'
	retlw	d'0'
	retlw	d'0'
	retlw	d'0'
	retlw	d'0'
	retlw	d'0'
	retlw	d'0'
	retlw	d'0'
	retlw	d'0'
	retlw	d'0'
	retlw	d'3'
	retlw	d'7'
	retlw	d'10'
	retlw	d'12'
	retlw	d'15'
	retlw	d'17'
	retlw	d'20'
	retlw	d'23'
	retlw	d'25'
	retlw	d'28'
	retlw	d'30'
	retlw	d'33'
	retlw	d'35'
	retlw	d'35'
	retlw	d'38'
	retlw	d'40'
	retlw	d'43'
	retlw	d'43'
	retlw	d'43'
	retlw	d'46'
	retlw	d'48'
	retlw	d'51'
	retlw	d'53'
	retlw	d'53'
	retlw	d'56'
	retlw	d'58'
	retlw	d'61'
	retlw	d'61'
	retlw	d'64'
	retlw	d'64'
	retlw	d'65'
	retlw	d'65'
	retlw	d'69'
	retlw	d'71'
	retlw	d'71'
	retlw	d'74'
	retlw	d'74'
	retlw	d'76'
	retlw	d'76'
	retlw	d'76'
	retlw	d'76'
	retlw	d'76'
	retlw	d'76'
	retlw	d'76'
	retlw	d'76'
	retlw	d'79'
	retlw	d'79'
	retlw	d'79'
	retlw	d'79'
	retlw	d'79'
	retlw	d'79'
	retlw	d'79'
	retlw	d'81'
	retlw	d'81'
	retlw	d'84'
	retlw	d'84'
	retlw	d'87'
	retlw	d'87'
	retlw	d'89'
	retlw	d'89'
	retlw	d'92'
	retlw	d'97'
	retlw	d'99'
	retlw	d'102'
	retlw	d'104'
	retlw	d'107'
	retlw	d'107'
	retlw	d'110'
	retlw	d'110'
	retlw	d'112'
	retlw	d'112'
	retlw	d'112'
	retlw	d'115'
	retlw	d'115'
	retlw	d'117'
	retlw	d'117'
	retlw	d'120'
	retlw	d'120'
	retlw	d'120'
	retlw	d'122'
	retlw	d'122'
	retlw	d'122'
	retlw	d'125'
	retlw	d'128'
	retlw	d'130'
	retlw	d'130'
	retlw	d'133'
	retlw	d'135'
	retlw	d'138'
	retlw	d'138'
	retlw	d'138'
	retlw	d'138'
	retlw	d'138'
	retlw	d'140'
	retlw	d'140'
	retlw	d'143'
	retlw	d'143'
	retlw	d'145'
	retlw	d'145'
	retlw	d'148'
	retlw	d'148'
	retlw	d'148'
	retlw	d'151'
	retlw	d'153'
	retlw	d'153'
	retlw	d'156'
	retlw	d'158'
	retlw	d'158'
	retlw	d'161'
	retlw	d'161'
	retlw	d'163'
	retlw	d'163'
	retlw	d'163'
	retlw	d'166'
	retlw	d'166'
	retlw	d'168'
	retlw	d'168'
	retlw	d'171'
	retlw	d'171'
	retlw	d'174'
	retlw	d'174'
	retlw	d'176'
	retlw	d'176'
	retlw	d'176'
	retlw	d'179'
	retlw	d'179'
	retlw	d'181'
	retlw	d'181'
	retlw	d'184'
	retlw	d'184'
	retlw	d'186'
	retlw	d'186'
	retlw	d'189'
	retlw	d'189'
	retlw	d'192'
	retlw	d'194'
	retlw	d'197'
	retlw	d'197'
	retlw	d'199'
	retlw	d'199'
	retlw	d'199'
	retlw	d'202'
	retlw	d'202'
	retlw	d'204'
	retlw	d'204'
	retlw	d'207'
	retlw	d'207'
	retlw	d'209'
	retlw	d'209'
	retlw	d'212'
	retlw	d'212'
	retlw	d'215'
	retlw	d'215'
	retlw	d'217'
	retlw	d'217'
	retlw	d'220'
	retlw	d'220'
	retlw	d'222'
	retlw	d'222'
	retlw	d'225'
	retlw	d'225'
	retlw	d'227'
	retlw	d'227'
	retlw	d'230'
	retlw	d'230'
	retlw	d'232'
	retlw	d'232'
	retlw	d'235'
	retlw	d'235'
	retlw	d'238'
	retlw	d'238'
	retlw	d'240'
	retlw	d'240'
	retlw	d'243'
	retlw	d'243'
	retlw	d'245'
	retlw	d'245'
	retlw	d'248'
	retlw	d'248'
	retlw	d'250'
	retlw	d'250'
	retlw	d'252'
	retlw	d'252'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
	retlw	d'254'
end					;End of program