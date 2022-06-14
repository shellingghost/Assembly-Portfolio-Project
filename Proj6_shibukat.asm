TITLE Proj6_shibukat     (proj6_shibukat.asm)

; Author: Troy Shibukawa
; Last Modified: 6/5/2022
; OSU email address: shibukat@oregonstate.edu
; Course number/section:271   CS271 Section 400
; Project Number: 6                Due Date: 6/5/2022
; Description: Program asks user to input ten signed integers. Returns invalid error and reprompts of input is not to specification.
;		After program receives ten valid inputs, it returns back to the user what they input along with the sum and their truncated average.

INCLUDE Irvine32.inc

;-----------------------------------------------------------------------------------------------------------------------------
; Name: mGetString
;
; First macro:
; Display a prompt (input parameter, by reference), then get the user’s keyboard input into a memory location 
; (output parameter, by reference). You may also need to provide a count (input parameter, by value) for the length 
; of input string you can accommodate* and a provide a number of bytes read (output parameter, by reference) by the macro.
;
; Preconditions: Input cannot be nothing (must have symbol entered as input)
;
; Receives:
; inPrompt = offset for user reference
; userInput = address of input for readstring
; outLength = length of user input
; errorString = offset for error reply
;
; Returns: userInput= address of user string, outLength = number of characters entered
;-----------------------------------------------------------------------------------------------------------------------------

mGetString	MACRO	inPrompt, userInput, outLength, errorString
	push	edx								
	push	eax								
	push	ecx								

	_condOut:										; different strings, if 99- we write error
		cmp		eax, 0
		je		_normalOut
		cmp		eax, 99
		je		_errorOut

		_normalOut:
			mov		edx, inPrompt					; pushed as offset of prompt, reference of address
			call	WriteString						; Display a prompt (input parameter, by reference) 
			jmp		_getOutput	

		_errorOut:
			mov		edx, errorString
			call	WriteString
			jmp		_getOutput

		_getOutput:
			mov		edx, userInput
			mov		ecx, 13							; sets allowable length, truncates at twelve-10 for integers, eleventh for possible sign, and if twelve-invalid
			call	ReadString						; get user input- readstring- receives: edx = address of buffer, ecx = buffer size, 
													; returns: edx = address of user string, eax = number of characters entered
	
			cmp		eax, 0
			je		_errorOut						; trying to fix err of no input
			mov		[outLength], eax					; 

			pop		ecx
			pop		eax
			pop		edx
ENDM

;-----------------------------------------------------------------------------------------------------------------------------
; Name: mDisplayString
;
; Print string from stored location in memory location (input parameter, by reference)
;
; Preconditions: None
;
; Receives: 
; inString = offset of string to display
;
; Returns: None
;-----------------------------------------------------------------------------------------------------------------------------

mDisplayString	MACRO	inString
	push	eax
	push	ebx
	push	ecx
	push	edx

	; setting the source, esi
	mov		esi, offset	totalArr
	mov		edx, esi



	_loadLoop:
		; load lodsd into eax addressed by esi
		LODSD				; loads the first sdword
	
		mov		edx, eax
		call	WriteString
		loop	_loadLoop

	pop		edx
	pop		ecx
	pop		ebx
	pop		eax
ENDM

.data
; byte data
intro1			BYTE	"Project 6: Portfolio Project",10,13, "Written byte Troy Shibukawa",10,13,0
request1		BYTE	"Please enter ten decimal integers when prompted.",10,13, "Each integer entered has to bextr small enough to fit into a 32 bit register. ",0
request2		BYTE	"After you have entered ten decimal integers, they will be repeated back along with the sum and average offset them. ",0
return1			BYTE	"The following integers have been entered:",10,13,0
return2			BYTE	"The sum of the numbers entered is: ",0
return3			BYTE	"The truncated average of the numbers entered is: ",0
thanks1			BYTE	"Thank you for using my program!",0
prompt1			BYTE	"Please enter a decimal integer: ", 0
error1			BYTE	"There has been and error. Non valid input entered, either the character is  invalid or integer length too long: ",10,13, "Please re-enter: ",0
outInput		BYTE	22	DUP(0)						; user input
listEntered		BYTE	"you have entered: ",10,13,0
inBetween		BYTE	", ",0							; for between each of the listed inputs
forConversion	BYTE	11 DUP(0)						; for conversion to ask I
totalArr		BYTE	40 DUP(0)						; holds the list of sdward that right val reads from
totalSum		BYTE	11 DUP(0)						; holds the entire value, used to get truncated value
truncSum		BYTE	11 DUP(0)						; testing for holding truncated value
bytOut			BYTE	12 DUP(0)						; holds temporary bite out value for writevalue, may need to be (?)

; non-byte data
tempDest	SDWORD	?									; intermediary between read val and write val, used for conversion*
sumTotal	SDWORD	?
lengthInput	SDWORD	?									; length of user input
negOne		SDWORD	0									; check to see if value was negative, reset after every loop
incTotal	SDWORD	0									; what is added to totalArr and incremented to get appropriate sdword in WriteVal
incPointer	SDWORD	?									; holds pointer which is incremented after every loop of writeal

.code

;-----------------------------------------------------------------------------------------------------------------------------
; Name: main
; 
; Description: Procedure takes in ten signed integers from user, checks them for validity . If they were not valid, the user
;		is reprompted to reinput a valid signed integer. After the last integers entered, the users input is reiterated, 
;		the sum of the integers is displayed, and the truncated average is displayed as well.
;
; Preconditions: Each integer entered must be a valid signed integer.
;
; Postconditions: Displayed output to screen.
;
; Receives: Ten integers from user input.
;
; Returns: Returns  ten  valid integers from user, their sum, and their truncated average.
;-----------------------------------------------------------------------------------------------------------------------------

main PROC
	;---------------------------------------------------------------------------------------------------------------
	; 1) Get 10 valid integers from the user. Your ReadVal will be called within the loop in main. Do not put your counted loop within ReadVal
	; 2) Stores these numeric values in an array.
	; 3) Display the integers, their sum, and their truncated average.
	; display a introduction using WriteString and request one and two
	;
	;---------------------------------------------------------------------------------------------------------------
	mov		edx, offset	intro1
	call	WriteString
	call	Crlf

	mov		edx, offset	request1
	call	WriteString

	mov		edx, offset	request2
	call	WriteString

	call	Crlf
	call	Crlf

	mov		ecx, 10							; used for the loop
	mov		ebx, 0							; used for incrementing

	loop1:
		;------------------------------------------------------------------------------------------------------------------
		; integer from user loop
		; preparation to call ReadVal
		;
		;------------------------------------------------------------------------------------------------------------------
		push	offset	totalArr			; 36 sdword array that holds sdword
		push	offset	totalSum			; 32-holds the sum total of the entire
		push	offset	negOne				; 28---checked to see if the value is negative
		push	offset	tempDest			; 24- holds the value for conversion to sdword
		push	offset	lengthInput			; 20
		push	offset	error1				; 16
		push	offset	prompt1				; 12
		push	offset	outInput			; 8
		call	ReadVal

		mov		edx, offset	totalArr		; holds the address of array of sdwords
		add		edx, ebx					; iterates the array address appropriately

		mov		eax, offset	tempDest		; holds the address of a temporary destination	value
		mov		eax, [eax]					; hold the temporary value
		mov		[edx], eax					; temporary sdword value now in total array destination

		add		ebx, 4						; holds the value of the required iteration for array
		LOOP	loop1
		mov		ecx, 10						; used for the loop


	; to screen
	call	Crlf
	mov		edx, offset	return1
	call	WriteString


	; set incPointer held value to have same address as totalArr
	mov		edx, offset	totalArr				; address of total array in edx
	mov		ebx, edx

	mov		edx, offset	incPointer
	mov		[edx], ebx							; saving this to be dereference within to make it like an address pointer

	loop2:
		; stack prep for writval
		push	offset	incPointer				; 36, now has appropriate address to work from, just dereference and address
		push	offset	bytOut					; 32
		push	offset	incTotal				; 28		
		push	offset	totalArr				; 24
		push	offset	forConversion			; 20
		push	offset	forConversion			; 16
		push	offset	negOne					; 12
		push	offset	tempDest				; 8
		call	WriteVal

		cmp		ecx, 1
		jg		_inBetween
		jmp		_setUp

		_inBetween:
			mov		edx, offset	inBetween			
			call	WriteString

		_setUp:
			; setting up for incremention
			mov		edx, offset	incPointer

			; incrementing value held at offset incPointer
			mov		eax, 4
			add		[edx], eax

		LOOP	loop2

	call	Crlf

	mov		edx, offset return2
	call	WriteString

	mov		edx, offset	totalSum				; address oftotalSum in edx
	mov		ebx, edx

	mov		edx, offset	incPointer
	mov		[edx], ebx							; saving this to be dereference within to make it like an address pointer

	; stack prep for writval
	push	offset	incPointer					; 36, now has appropriate address to work from, just dereference and address
	push	offset	bytOut						; 32
	push	offset	incTotal					; 28		
	push	offset	totalArr					; 24
	push	offset	forConversion				; 20
	push	offset	forConversion				; 16
	push	offset	negOne						; 12
	push	offset	tempDest					; 8

	call	WriteVal							; uses mDisplayString to iterate over where we stored at our array

	call	Crlf


	; get the truncated average
	mov		eax, offset	totalSum
	mov		eax, [eax]
	mov		edx, 0
	mov		ebx, 10

	CDQ
	idiv		ebx							; truncated value is now in

	mov		edx, offset	truncSum			; TESTING! holds trunked value
	mov		[edx], eax						; at the value of the offset of total sum should be the truncated value

	; call the string collar
	mov		edx, offset	return3				; phrase: the truncated average is: 
	call	WriteString

	; setup pointer
	mov		edx, offset	truncSum			; address oftotalSum in edx
	mov		ebx, edx

	mov		edx, offset	incPointer
	mov		[edx], ebx						; saving this to be dereference within to make it like an address pointer

	; trying to reset bytOut
	mov		edx, offset	bytOut
	mov		[edx], edx
	mov		ebx, 0
	mov		[edx], ebx						; reset bytOut

	; set up writeval
	push	offset	incPointer		; 36, now has appropriate address to work from, just dereference and address
	push	offset	bytOut			; 32
	push	offset	incTotal		; 28		
	push	offset	totalArr		; 24
	push	offset	forConversion	; 20
	push	offset	forConversion	; 16
	push	offset	negOne			; 12
	push	offset	tempDest		; 8

	call	WriteVal				; called procedure writval


	; newline
	call	Crlf
	call	Crlf

	; thank you
	 mov	edx, offset	thanks1
	 call	WriteString

	Invoke ExitProcess,0			; exit to operating system
main ENDP

;-----------------------------------------------------------------------------------------------------------------------------
; Name: ReadVal
; 
; Description: Invokes mGetString macro for user input in the form of a string of digits, converts them into a SDWORD value,
;		validating the users input, and then stores the user input.
;
; Preconditions: User must enter ten valid signed integers.
;
; Postconditions: Changes array of sdwords where user input is stored.
;
; Receives:
;		[ebp+32]		= totalSum, address of total value of 10 digits
;		[ebp+28]		= negOne, global negative check	
;		[ebp+24]		= tempDest, address of temporary destination of input	
;		[ebp+20]		= lengthInput, length of input	
;		[ebp+16]		= error1, address of error message	
;		[ebp+12]		= prompt1, address of prompt one
;		[ebp+8]			= outInput, source/destination for primitive loop
;
; Returns:
;		totalSum- address of total value of 10 digits, lengthInput - length of input
;-----------------------------------------------------------------------------------------------------------------------------

ReadVal	PROC 
;------------------------------------------------------------------------------------------------------------------
;	 1) invoke the mGetString macro to get user input in the form of string digits
;	 2) convert using string primitives the string of ascii digits to its numeric value representation SDWORD,
;		 validating the users input is a valid number *no letter symbols etcetera)
;	 3) store this one value in a memory variable (output parameter, by reference)
;
;------------------------------------------------------------------------------------------------------------------
	push	ebp
	mov		ebp, esp				; set up STD calling 
	push	eax
	push	ebx
	push	ecx
	push	edx

	mov		eax,0					; for conditional control inside of mGetString
	
	_mainLoop:
		;------------------------------------------------------------------------------------------------------------------
		; input parameter, by reference, prompt, static input for length of input we can accommodate/used in comparison
		;	 output parameters, memory location, by reference, number of bites read, by reference(variable?)
		;
		; mGetString	MACRO	inPrompt, userInput, outLength, errorString
		;
		;------------------------------------------------------------------------------------------------------------------
		mov				edx, [ebp+24]
		mov				ebx, 0
		mov				[edx], ebx										; trying to reset tempDest

		mov				edx, 0
		mov				ecx, [ebp+28]
		mov				[ecx], edx										; used to check if the value is negative, reset



		mGetString		[ebp+12],[ebp+8] ,[ebp+20], [ebp+16]			; input parameters are prompt, edx, and out location, ebx

	;------------------------------------------------------------------------------------------------------------------
	; 2) convert using string primitives: the string of ascii digits to its numeric value representation SDWORD, 
	; validating the users input is a valid number
	; *no letter symbols etcetera)

	; outInput= address of user string***** ebp+8 needs to bedereference
	; outLength= count of length inputed
	;
	;------------------------------------------------------------------------------------------------------------------	
	_loopPrim:							
		cld								
		mov		ecx, [ebp+20]				; length of user input, used for loop
		mov		esi, [ebp+8]				; source is the same as destination
		mov		edi, [ebp+8]

		mov		ebx, [ebp+24]				; putting the address at this location into the register of ebx
		mov		edx,0						; resetting*****/ edx used as intermediary*****
		mov		[ebx], edx					; pushing value into the de referenced offset just like line. 50//resetting

	premInvalid:							; premature invalidtells us that there were too many characters in the initial input, precheck

		; check to see  if only +/-
		cmp		ecx, 1
		je		_singleSign
		jmp		_limSize					; jump straight to checking to see that the limit of length is okay

			_singleSign:
				LODSB						; be sure to jump appropriately after this
				cmp		AL,43				; check for +. if either a plus or minus, jumps straight to invalid. just a sign is inappropriate
				je		_invalid
				cmp		AL, 45				; negative sign check
				je		_invalid
				jmp		_afterLoad			; jump post byte load if the only byte is not a sign

		_limSize:
			cmp		ecx, 12
			je		_invalid

			cmp		ecx, 11					; needs to be a sign here or else invalid
			je		_lenCheck
			jmp		_convertLoop			; if not 12,default invalid, and if not eleven, which requires a sign check (10 max char)
											;	then jump to _convertLoop	


	_lenCheck:		
		; loads into the accumulator first byte to check for sign, skips the low sign from convert loop
		LODSB								; seems redundant but is necessary for check
		cmp		AL,43						; check for +/-. if either a plus or minus, jumps straight the valid
		je		_afterLoad
		cmp		AL, 45						; negative sign check
		je		_afterLoad
		jmp		_invalid					; if the length is eleven and neither is the character first a minus or plus, it isn't valid/fixes 11 int problem

	_convertLoop:							; make sure that every input is valid within range, maybe used local variable to help with +
		LODSB								; puts a bite in AL //multiple jumps*, conditional jump when local variable zero(check for sign)
											; have to loop to get multiple sign numbers, done in main
	
	_afterLoad:							
		cmp		ecx, 0						; exit loop if count is down to zero
		je		_finProcedure				; todo: I've loaded this, check this, but how do I turn it all in sdword?
											; I have a string of valid* character saved, how to convert into sdward?
		


		mov		ebx, [ebp+20]				; if the length of the initial input is the same of the ecx, we were at the first and we should check for a sign
		cmp		ebx, ecx					; conditional jump to check for a signed value upfront, counts are also used to compare to max length
		je		_signCheck
		jmp		_validCheck

	_signCheck:
		cmp		AL,43						; check for +/-. if either a plus or minus, jumps straight the valid
		je		_valid
		cmp		AL, 45						; negative branch of logic
		je		_negAlgo					; sets value check to not zero, 1, for a different logic loop to add up each individual byte value
		jmp		_addSDWORD					; if neither signed +/-, jump to adding it up

	_negAlgo:
		mov		edx, 1
		mov		ebx, [ebp+28]
		mov		[ebx], edx					; dereferencing to change value at actual location
		jmp		_valid

	_validCheck:							; checks that the integer inputted is within valid range: 48-57, inclusive
		cmp		AL, 48
		jl		_invalid					; if character is lower than forty eight, the entire input is invalidated
		cmp		AL, 57

		jle		_checkSDWORD				; if it is valid*, we add it to our sdword- after this, we jump to valid in & dec ecx

		
		jmp		_invalid					; if it's not within the range 48-57, it is invalid

	_checkSDWORD:							; check to see if the value was negative or not
		mov		edx, [ebp+28]				; edx used as intermediary, probably need to get the value at the address not the address
		mov		edx, [edx]					; actual value here
		cmp		edx, 0						; value is positive
		je		_addSDWORD
		cmp		edx, 1
		je		_subSDWORD

	_valid:
		; increments counter, all roads lead to- unless invalid-invalid comes from a jump in _validCheck
		dec		ecx
		jmp		_convertLoop					 

	_invalid:								; prince error message, push it to stack or maybe have another procedure called, or macro?
		mov		eax, 99						; if eax ninety nine then we know to go down the invalid logic in the request screen
		jmp		_mainLoop

	_addSDWORD:
		;----------------------------------------------------------------------------------------------------------------------
		; using algorithm of using getting the total sum and multiplying it by ten and the given bite value you get the total
		; 10 x (ebp+24) + (bytevalue-49)//one byte at a time
		; ebx eax edx---all open, just make sure to keep in mind al
		;
		;----------------------------------------------------------------------------------------------------------------------	

		sub		AL, 48						; might need to make it not a constant|||strange tough, should be 48 TODO:!
		mov		ebx, 0
		mov		BL, AL						; saving from multiplication use of eax/ BL now has translated bite value

		mov		eax, [ebp+24]				; putting the address into eax
		mov		eax, [eax]					; getting the value out of the address* and putting it into the register to work with
		mov		edx,10
		mul		edx

		add		eax, ebx					; adding it altogether before moving it into the address
		mov		edx, [ebp+24]				; getting actual address
		mov		[edx], eax					; putting the value into the address
		jmp		_valid


	_subSDWORD:	
		;------------------------------------------------------------------------------------------------------------------
		; subtracting instead of adding, global variable is one
		; ebx eax edx---all open, just make sure to keep in mind al
		;
		;------------------------------------------------------------------------------------------------------------------	

		sub		AL, 48						; might need to make it not a constant|||strange tough, should be 48 TODO:!
		mov		ebx, 0
		mov		BL, AL						; saving from multiplication use of eax/ BL now has translated bite value

		mov		eax, [ebp+24]				; putting the address into eax
		mov		eax, [eax]					; getting the value out of the address* and putting it into the register to work with
		mov		edx,10
		mul		edx

		sub		eax, ebx					;subtracting before moving it into the address

		mov		edx, [ebp+24]				; getting actual address
		mov		[edx], eax					; putting the value into the address
		jmp		_valid



	_finProcedure:
		mov		edx, [ebp+32]				; the total some value
		mov		edx, [edx]					; holds total sum value
		mov		ebx, [ebp+24]				; hold the value, the address
		mov		ebx, [ebx]					; value at the address in ebx register -> user input

		; decision tree for subtraction or addition
		mov		eax, [ebp+28]				; edx used as intermediary, probably need to get the value at the address not the address
		mov		eax, [eax]					; actual value here
		cmp		eax, 0						; value is positive
		je		_addVal
		cmp		eax, 1
		je		_subVal

		_addVal:
			add		edx, ebx				; hold the new value, total sum value
			jmp		_afterMath

		_subVal:
			neg		ebx
			sub		edx, ebx
			jmp		_afterMath

		_afterMath:
			; putting the new value into the location value place
			mov		ebx, [ebp+32]
			mov		[ebx], edx

			pop		edx
			pop		ecx
			pop		ebx
			pop		eax
			pop		ebp
			ret		36		

ReadVal ENDP

;-----------------------------------------------------------------------------------------------------------------------------
; Name: WriteVal
; 
; Description: Converts sdword value to an ascii string of digits and invokes macro mDisplayString to print it in the form
;		 of ascii.
;
; Preconditions: An appropriate address with the sdword value to convert.
;
; Postconditions: None
;
; Receives:
;		called procedure writval
;		[ebp+36]		= incPointer, now has appropriate address to work from, just dereference and address
;		[ebp+32]		= bytOut, bites to output
;		[ebp+28]		= incTotal, increment for array address
;		[ebp+24]		= totalArr, total some value of numbers entered
;		[ebp+20]		= forConversion, first array for conversion
;		[ebb+16]		= forConversion, for conversion
;		[ebp+12]		= negOne, global for checking negative
;		[ebp+8]			= tempDest, temporary destination		
;
; Returns: Displays to screen ascii characters
;-----------------------------------------------------------------------------------------------------------------------------
WriteVal PROC

	; 1) Convert a numeric SDWORD value (input parameter, by value)-stack to a string of ASCII digits[subtraction].
	push	ebp
	mov		ebp, esp
	push	eax
	push	ebx
	push	ecx
	push	edx


	mov		edx, [ebp+32]						; resetting bytOut
	mov		ebx, 0
	mov		[edx], ebx

	_toAscii:
		mov		edx, [ebp+36]					; takes in address with value within that has actual address
		mov		edx, [edx]						; should have appropriate address within edx+ register

		; you dereference this address to get the four bites within
		mov		eax, [edx]
		mov		ecx, 0							; prep counter value
		mov		edx, 0							; resetting value at [ebp+12]
		mov		[ebp+12],edx
		_asciiLoop:
			cmp		ecx, 0
			je		_negCheck						; if it's the very first you must check if its negative
			jmp		_idivMath

			_negCheck:
				cmp		eax, 0
				jl		_pushNeg
				jmp		_idivMath

			_pushNeg:
				neg		eax							; flipping value from negative
				mov		ebx, 1
				mov		[ebp+12], ebx				; setting global value marker to negative
				jmp		_asciiLoop



			_idivMath:
				; start dividing
				; edx holds the value
				mov		ebx, 10
				cdq				
				idiv	ebx							; eax now has remainder, edx now has total


				; check eax

				inc		ecx
				push	edx
				cmp		eax, 0
				je		_checkMin					; changed to new jump, check to see if we push -checkMin
				jmp		_asciiLoop

				;---------------------------------------------------------------------------------------------------------------
				; divide and take dividend, if dividend is zero, eax, jump out
				; idiv to maintain flag/sign
				; logic block where we push if global is more than zero/have to reset before the procedure is called again
				;
				;---------------------------------------------------------------------------------------------------------------
				_checkMin:
					; compare line
					mov		ebx, [ebp+12]
					cmp		ebx, 1
					je		_pushMin
					jmp		_toString

				_pushMin:
					push	45
					inc		ecx
					mov		edx, ecx
					jmp		_toString

				; jump back to start of loop
				jmp		_asciiLoop

				_toString:
					;---------------------------------------------------------------------------------------------------------------
					; start popping everything off from the stack, use ecx determined how much the pop
					; start primitive, utilization of the stack
					; ecx decrement
					;
					;---------------------------------------------------------------------------------------------------------------
					cld

					; set destination
					mov		edi, [ebp+32]
					_primLoop:
						pop		eax
						
						cmp		ecx, edx
						je		_minusCheck
						jmp		_nonMinus

						_minusCheck:
							mov		ebx, [ebp+12]
							cmp		ebx, 1									
							je		_storeAs
							jmp		_nonMinus

						_nonMinus:
							add		eax, 48
							jmp		_storeAs					

						_storeAs:
							STOSB
							dec		ecx
							cmp		ecx, 0
							je		_toScreen
							jmp		_primLoop

				_toScreen:
					mov		edx, [ebp+32]
					call	WriteString
					pop		edx
					pop		ecx
					pop		ebx
					pop		eax
					pop		ebp
					ret		36			
WriteVal ENDP

END main