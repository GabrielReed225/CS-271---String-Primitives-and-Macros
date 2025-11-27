; String Primatives and macros      (Proj6_reedgab.asm)

; Author: Gabriel Reed
; Last Modified: 03/17/24
; OSU email address: reedgab@oregonstate.edu
; Course number/section:   CS271 Section 400
; Project Number:     6             Due Date:  03/17/24
; Description: This program will introduce itself then give the user directions on what to do. The program will then prompt the user to enter 10 signed integers that can fit in a 32-bit registers, displying an error message if
;				the users input is outside that parameter. Finally, the program will display all entered digits as well as their sum and average.

INCLUDE Irvine32.inc

; --------------------------------------------------------------------------------- 
; Name: mGetString 
; 
; Description: displays a prompt for the user and stores a user-inputted string
; 
; Receives: userPrompt = prompt for the user
;			userInput = variable to store the user-inputted sting
;			inputLength	= variable to store the length of the user-inputted string]
;			INTEGER_NUMBER = the number of integers to be entered
;			currentNumber = the current integer number being entered
;			userOutput = where the output string is stored
;
;
;  Returns: inputLength = length of string inputted
;			userInput = user-inputted string
; ---------------------------------------------------------------------------------
mGetString	MACRO userPrompt, userInput, stringLength, userStringLength, outputString, currentNumber


  ; Save registers
  PUSH	EAX
  PUSH	ECX
  PUSH	EDX

  mDisplayString userPrompt										; prints the user prompt to screen

  ; get user input
  PUSH	outputString
  PUSH	currentNumber
  CALL	WriteVal

  MOV	EDX, userInput
  MOV	ECX, stringLength
  call	ReadString
  MOV	userStringLength, EAX									; saves the number of characters entered by the user

  ; restore registers
  POP	EDX
  POP	ECX
  POP	EAX

ENDM

; ----------------------------------------------------------------------------------------------------------------
; Name: mGetString
;
; Displays a given input string
;
;
;	Receives:
;		outputString = the string to be printed
;
;	returns: none
; -----------------------------------------------------------------------------------------------------------------
mDisplayString MACRO outputString

  PUSH	EDX
  MOV	EDX, outputString
  CALL	WriteString
  POP	EDX

ENDM

INTEGER_NUMBER = 10											; the number of integers to be entered
INPUT_MAXIMUM = 12											; maximum string count entered

.data
NameAndTitle			BYTE		"Project 6: Designing low-level I/O procedures",10,"by Gabriel Reed",13, 10, 10, 0					
userIntsructions1		BYTE		"Please provide 10 signed decimal numbers",13, 10,10, 0	
programDescription		BYTE		"Each number needs to have a value that will fit inside a 32 bit register. After all 10 numbers have been collected, a list of integers will be displayed, as well as their sum and average value ",13, 10,10, 0
integerArray			SDWORD		INTEGER_NUMBER DUP(?)
userDirections			BYTE		"Please enter a signed value: ", 0
errorMessage			BYTE		"ERROR: Input not within parameters", 10, "Please try again: ", 0
userInput				BYTE		INPUT_MAXIMUM DUP(?)
inputLength				DWORD		0
inputError				DWORD		0
inputSign				SDWORD		1
integerSum				SDWORD		?
integerAverage			SDWORD		?
userOutput				BYTE		INPUT_MAXIMUM DUP(?)
integersDescription		BYTE		"You entered the following: ", 0
addedDescription		BYTE		10, "The sum of your numbers is: ", 0
averageDescription		BYTE		10, "The truncated average of your numbers is: ", 0
seperator				BYTE		", ", 0
goodbye					BYTE		"Thanks for swinging by, until next time! Bye!", 0

.code
; ----------------------------------------------------------------------------------------------------------------
; Name: main
;
; call all other procedures to execute the profram
;
; Postconditions: changes register ecx, edi
;
;
;	returns: integerArray
; -----------------------------------------------------------------------------------------------------------------
main PROC

  ; Display the introduction and instructions
  mDisplayString  OFFSET NameAndTitle
  mDisplayString  OFFSET userIntsructions1
  mDisplayString  OFFSET programDescription

  ; Prepare the necessary registers for integerArray
  MOV	EDI, OFFSET integerArray
  MOV	ECX, INTEGER_NUMBER

  ;  Push necessary statementd onto the stack
_InputLoop:
  PUSH	ECX
  PUSH	EDI
  PUSH	OFFSET integerArray
  PUSH	OFFSET errorMessage
  PUSH	OFFSET userOutput
  PUSH	OFFSET userDirections	
  PUSH	OFFSET inputSign
  PUSH	OFFSET inputError
  PUSH	OFFSET userInput				
  PUSH	OFFSET inputLength
  CALL	 ReadVal

  POP	ECX
  ADD	EDI, 4
  LOOP	_InputLoop
  CALL	CrLf

  ; display all input strings
  mDisplayString OFFSET integersDescription
  MOV	ESI, OFFSET	integerArray
  MOV	ECX, INTEGER_NUMBER

_Numbers:																		; perform a loop to inerate and display all members of the array
  LODSD
  PUSH  ECX
  PUSH	OFFSET userOutput
  PUSH	EAX
  CALL	WriteVal
  POP	ECX
  CMP	ECX, 1
  JE	_Skip																	; bypass the seperator insert if last number has been reached
  mDisplayString OFFSET seperator
  LOOP	_Numbers

_Skip:

  ; Display sum
  mDisplayString OFFSET addedDescription
  MOV	EBX, 0
  MOV	ESI, OFFSET	integerArray
  MOV	ECX, 10

_Sum:																			; perform a loop to get the sums of the integers
  CLD
  LODSD
  ADD	EBX, EAX
  LOOP _Sum
  MOV	EAX, EBX
  PUSH	OFFSET userOutput
  PUSH	EAX
  call WriteVal

  ; display average
  mDisplayString OFFSET averageDescription
  MOV	EBX, INTEGER_NUMBER
  MOV	EDX, 0
  DIV	EBX
  PUSH	OFFSET userOutput
  PUSH	EAX
  call WriteVal
  CALL	CrLf

  CALL	CrLf
  mDisplayString OFFSET goodbye

	Invoke ExitProcess,0	; exit to operating system
main ENDP

; ----------------------------------------------------------------------------------------------------------------
; Name: ReadVal
;
; Uses the mGetString macro to get user input in the form of a string of digits, converts the inputted string to an integer and stores this integer in integerArray 
;
; Postconditions: changes register eax and ebp register and changes to integerArray, inputSign, inputError and userOutput
;
; Receives: integerArray = storage for the strings converted to integers
;			inputSign = determinor of whether or not a number is positive or negative
;			inputError = variable used to dtermine if a string is valid
;			userOutput = where the user integer to string conversion is held
;			errorMessage = a message displayed when an invalid entry is input
;			inputLength = length of user inputted string
;			userDirections - prompt for the user to enter a numerical string
;			userInput = where the original user input is stored
;
;	returns: integerArray 
; -----------------------------------------------------------------------------------------------------------------
ReadVal PROC

  ; get a Stack reference point
  PUSH  EBP
  MOV   EBP, ESP

_userInput:
  mGetString [EBP+24], [EBP+12], INPUT_MAXIMUM, [EBP+8], [EBP+28], EAX


  ; Determine whether or not the string is valid
  PUSH	[EBP+20]									
  PUSH	[EBP+16]																; push the flag variables to the stack
  PUSH	[EBP+8]										
  PUSH	[EBP+12]																; push the user input values and string length to the stack			
  call	validateString

  ; Determine if error flag is set
  MOV	EAX, [EBP+16]
  MOV	EAX, [EAX]
  CMP	EAX, 0
  JNE	_Error																	; jump to _Error if an input error has occurred

  ; Convert user-inputted string into a signed integer
  PUSH	[EBP+40]																; push the integerArray onto the stack
  PUSH	[EBP+20]																; push the user input values and string length to the stack		
  PUSH	[EBP+16]				
  PUSH	[EBP+8]		
  PUSH	[EBP+12]																; push the user input values and string length to the stack		
  call	stringToInteger		

  ;Determine if there is Overflow
  MOV	EAX, [EBP+16]
  MOV	EAX, [EAX]
  CMP	EAX, 0
  JE	_Return																	; jumps to return if value can fit within a 32-bit register, otherwise returns an error

_Error:																			; prints an error message to the screen and resets inputError back to 0
  mDisplayString [EBP+32]
  MOV	EAX, [EBP+16]
  MOV	DWORD PTR [EAX], 0

  JMP	_userInput

_Return:																		; restore used registers and return to calling function
  POP	EBP
  RET	36

ReadVal ENDP

; --------------------------------------------------------------------------------- 
; Name: validateString
;  
; Description: determines if the user-inputted string is valid
; 
; Postconditions: changes to eax, ecx, edx,m ebp, esi registers and changes to inputError and inputSign
; 
; Receives: inputLength = length of user inputted string
;			inputSign = determinor of whether or not a number is positive or negative
;			inputError = variable used to dtermine if a string is valid
;			userInput = where the original user input is stored
;
;	returns: none
; ---------------------------------------------------------------------------------
validateString PROC
  ; preserve registers
  PUSH	EAX
  PUSH	ECX
  PUSH	EDX
  PUSH	ESI
  PUSH	EBP

  MOV	EBP, ESP																	; get a stack point reference

 ; Move user input and user input length into registers
  MOV	ESI, [EBP + 24]						
  MOV	ECX, [EBP + 28]				

  ; Input out of bounds
  CMP	ECX, 0
  JLE	_Error																		; jump to _Error if input is nonexistent
  CMP	ECX, 12
  JGE	_Error																		; jump to _Error if input has too many digits	

  ; Clear EAX and prepare the first digit
  MOV	EAX, 0
  CLD																				; set direction for string primative
  LODSB

  ; Check first character for sign
  PUSH	[EBP + 36]														
  PUSH	[EBP + 32]														
  PUSH	EAX
  CALL	FirstDigitCheck
	
  ; Update ECX and verify whether or not there are succeeding digits
  SUB	ECX, 1
  CMP	ECX, 0
  JLE	_Return

_NextDigit:
	; Clear EAX abd prepare another ASCII character
  MOV	EAX, 0
  CLD
  LODSB																				; load ASCII character

  ; Validate ASCII character
  PUSH	[EBP + 32]							
  PUSH	EAX
  CALL	ValidDigit
	
  ; Break loop if invalid character is entered
  MOV	EAX, 0
  MOV	EDX, [EBP + 32]
  CMP	EAX, [EDX]																	; compare EAX to the value of inputError, jump if inputError is 1
  JNE	_Return

  LOOP	_NextDigit

  JMP	_Return																		; returns to calling procedure once loop has finished

_Error:																				; sets the error flag
  MOV	EAX, [EBP + 32]
  MOV	DWORD ptr [EAX], 1

_Return:																			 ; restore used registers and return to calling function
  POP	EBP
  POP	ESI
  POP	EDX
  POP	ECX
  POP	EAX
  RET	16
validateString ENDP

; --------------------------------------------------------------------------------- 
; Name: FirstDigitCheck
;  
; Description: determines whether or not the first digit is valid
;
;Postconditions: changes to ebp, eax and ebp and changes to inputSign andf inputError
; 
; Receives: an ASCII character to validate
;			inputSign = determiner of whether or not the character is positive or negative
;			inputError = determiner of whether or not the input is valid
;
; Returns: none
; ---------------------------------------------------------------------------------
FirstDigitCheck PROC
  ; preserve registers
  PUSH	EAX
  PUSH	EDX
  PUSH	EBP

  MOV	EBP, ESP																	; get a stack point reference
  MOV	EAX, [EBP + 16]																; move ASCII character to EAX

  ; Determine whether character is supposed to be positive or negative
  CMP	EAX, 43d
  JE	_Return																		; proceed to return portion if character is positive
  CMP	EAX, 45d
  JE	_Negative																	; proceed to _Negative if value is supposed to be negative

  ; Check if character is numerical
  CMP	EAX, 48d
  JB	_Invalid																	; proceed to invalid if character is supposed to be below 0	
  CMP	EAX, 57d
  JA	_Invalid																	; proceed to invalid if character is supposed to be above 0
  JMP	_Return

_Invalid:																			; sets inputError to 1 if input is invalid
  MOV	EAX, [EBP + 20]
  MOV	DWORD ptr [EAX], 1

_Negative:																			; sets inputSign to -1 if input is supposed to be negative
  MOV	EAX, [EBP + 24]
  MOV	EDX, -1
  MOV	[EAX], EDX

_Return:																			; restore used registers and return to calling function
  POP	EBP
  POP	EDX
  POP EAX
  RET 12
FirstDigitCheck ENDP

; --------------------------------------------------------------------------------- 
; Name: ValidDigit 
;  
; Description: determnines whether or not all succeeding digits are valid
; 
; Postconditions: changes to ebp and eax registers and changes to inputError
; 
; Receives: an ASCII character to validate
;			inputError = determiner of whether or not the input is valid
;
; Returns: none
; ---------------------------------------------------------------------------------
ValidDigit PROC
  ; preserve registers
  PUSH	EAX
  PUSH	EBP

  MOV	EBP, ESP											; get a stack point reference
  MOV	EAX, [EBP + 12]										; move ASCII character to EAX

  ; Check if character is numerical
  CMP	EAX, 48d
  JB	_Invalid											; proceed to invalid if character is supposed to be below 0	
  CMP	EAX, 57d
  JA	_Invalid											; proceed to invalid if character is supposed to be above 0
  JMP	_Return

_Invalid:													; sets inputError to 1 if input is invalid
  MOV	EAX, [EBP + 20]
  MOV	DWORD ptr [EAX], 1

_Return:													; restore used registers and return to calling function
  POP	EBP
  POP	EAX
  RET	8
ValidDigit ENDP

; --------------------------------------------------------------------------------- 
; Name: stringToInteger
;  
; Description: converts the user-inputted string into a signed integer
; 
; Postconditions: changes to eax, ebx, ecx, edx, esi and edi registers and changes to inputError and integerArray
; 
; Receives: inputLength = length of user inputted string
;			inputSign = determinor of whether or not a number is positive or negative
;			inputError = variable used to dtermine if a string is valid
;			userInput = where the original user input is stored
;			integerArray = storage for the strings converted to integers
;			
;	returns: none
; ---------------------------------------------------------------------------------
stringToInteger PROC

  ; Preserve registers
  PUSH	EAX
  PUSH	EBX
  PUSH	ECX
  PUSH	EDX
  PUSH	ESI
  PUSH	EDI
  PUSH	EBP
	
  MOV	EBP, ESP											; get a stack point reference

  ; Prepare registers
  MOV	ESI, [EBP + 32]				
  MOV	EDI, [EBP + 48]						
  MOV	ECX, [EBP + 36]						
  MOV	EBX, 0												; Starting integer partial sum
  MOV	EDX, 1												; starting multiplier

  ; Move ESI to array endpoint 
  MOV	EAX, ECX
  SUB	EAX, 1												; subtract since ESI is already at 1
  ADD ESI, EAX

_StringConversion:
  ; Clear EAX and get next ACSII character
  MOV	EAX, 0
  STD
  LODSB

  ; Determine whether or not the last digit is the sign
  CMP	EAX, 43d		
  JE	_SignInteger										; jump to _SignInteger if there is a sign and its positive
  CMP	EAX, 2Dh	
  JE	_SignInteger										; jump to _SignInteger if there is a sign and its negative

  ; Convert ASCII to integer
  SUB	EAX, 48d											; subtract 48 since 48d is ASCII for 0

  ; Multiply digit to correct place
  PUSH	EDX
  MUL	EDX									
  ADD	EBX, EAX											; multiply by the correct multiple of 10 and add to existing integer partial sum

  JO	_Overflow											; jumps to Overflow if there is an overflow error

  ; Update multiplier and start next loop
  POP	EDX
  MOV	EAX, EDX
  MOV	EDX, 10
  MUL	EDX
  MOV	EDX, EAX

  LOOP	_StringConversion

_SignInteger:												; Affix correct sign to final integer
  MOV	EAX, EBX
  MOV	EBX, [EBP + 44]				
  MOV	EBX, [EBX]
  MUL	EBX	

  ; Reset sign flag
  MOV	EBX, [EBP + 44]	
  MOV	SDWORD PTR [EBX], 1
	
  MOV	[EDI], EAX											; Save integer value to array

  JMP	_Return

_Overflow:
  POP	EDX													; preserve the stack since the procedure is returning
  
  ; Set Error Flag
  MOV	EAX, [EBP + 40]
  MOV DWORD ptr [EAX], 1

_Return:													 ; restore used registers and return to calling function
  POP	EBP
  POP	EDI
  POP	ESI
  POP	EDX
  POP	ECX
  POP	EBX
  POP	EAX
  RET	20
stringToInteger ENDP

; --------------------------------------------------------------------------------- 
; Name: WriteVal 
;  
; Description: takes the saved integers and turns them into strings
; 
; 
; Receives: integerArray = saved integers to be converted to strings
;			userOutput = location where the converted strings are stored
;
;
; Returns: userOutput
; ---------------------------------------------------------------------------------
WriteVal PROC

  ; preserve used registers
  PUSH	EAX
  PUSH  EBX
  PUSH	ECX
  PUSH  EDX
  PUSH	EDI
  PUSH	EBP
	
  MOV	EBP, ESP											; sets up a stack point reference

  ; Prepare registers
  MOV	EDX, [EBP + 28]						
  MOV	EDI, [EBP + 32]		
  MOV	ECX, 10	
  MOV	EBX, 1000000000

  ; determine if value is positive or negative
  CMP	EDX, 0
  JL	_Negative
  JMP	_GetDigit

_Negative:													; convert to a positive integer and attach a minus in front
  MOV	EAX, -1
  MUL	EDX
  MOV	EDX, EAX
  MOV	EAX, '-'
  STOSB			

_GetDigit:													;get a digit from the integer being evaluated
  MOV	EAX, EDX
  MOV	EDX, 0
  DIV	EBX					
	
  ; preserve remainder
  PUSH	EDX

 ; Determine if digit is leading zero
  CMP	EAX, 0
  JNE	_SaveDigit											; saves the digit if it is not zero
  CMP	ECX, 1
  JE	_SaveDigit											; saves the digit if it is the last digit
  CMP	EAX, 0
  JE	_Divisor


_SaveDigit:													; send the digit to the string array and increment pointer					
  ADD	EAX, 48d
  STOSB

_Divisor:													; update divisor to previous multiple of 10
  MOV	EAX, EBX
  MOV	EDX, 0
  MOV	EBX, 10
  DIV	EBX
  MOV	EBX, EAX

  ; restore remainder
  POP	EDX

  LOOP	_GetDigit	

  ; null terminate string									; this terminates the string
  MOV	EAX, 0
  STOSB

  ; Display output 
  MOV	EAX, EBP
  CMP	EAX, 1703700
  JE	_Reset
  mDisplayString	[EBP + 32]

_Reset:													 ; clear output string for next entry 
  MOV	ECX, INPUT_MAXIMUM
  MOV	EDI, [EBP + 32]
  MOV	EAX, 0
  REP	STOSB

_Return:												 ; restore used registers and return to calling function
  POP	EBP
  POP	EDI
  POP	EDX
  POP	ECX
  POP	EBX
  POP	EAX
  RET	8

WriteVal ENDP

END main
