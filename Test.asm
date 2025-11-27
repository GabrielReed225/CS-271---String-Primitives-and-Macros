; String Primatives and macros      (Proj6_reedgab.asm)

; Author: Gabriel Reed
; Last Modified: 03/17/24
; OSU email address: reedgab@oregonstate.edu
; Course number/section:   CS271 Section 400
; Project Number:     6             Due Date:  03/17/24
; Description: 
;              

INCLUDE Irvine32.inc

; ----------------------------------------------------------------------------------------------------------------
; Name: mGetString
;
; Obtains user input in the foirm of a string
;
; Postconditions: changes register edx, ecx
;
; Receives:
;	userDirections = tells the user what to do
;	errorMessage = tells the user to enter a different input
;
;	returns: none
; -----------------------------------------------------------------------------------------------------------------
mGetString	MACRO userPrompt, userInput, stringLength, userStringLength, outputString, currentNumber

  ; Save registers
  PUSH	EAX
  PUSH	ECX
  PUSH	EDX

  mDisplayString userPrompt										; prints the user prompt to screen

  ; get user input
  PUSH	outputString
  PUSH	currentNumber
  call WriteVal
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

INTEGER_NUMBER = 10
INPUT_MAXIMUM = 12

.data
NameAndTitle			BYTE		"Project 6: Designing low-level I/O procedures",10,"by Gabriel Reed",13, 10, 10, 0									; this is the program title and my name
userIntsructions1		BYTE		"Please provide 10 signed decimal numbers",13, 10,10, 0	
programDescription		BYTE		"Each number needs to have a value that will fit inside a 32 bit register. After all 10 numbers have been collected, a list of integers will be displayed, as well as their sum and average value ",13, 10,10, 0
integerArray			SDWORD		INTEGER_NUMBER DUP(?)
userDirections			BYTE		"Please enter a signed number: ", 0
errorMessage			BYTE		"ERROR: Input not within parameters", 10, "Please try again: ", 0
userInteger				SDWORD		?
userInput				BYTE		INPUT_MAXIMUM DUP(?)
inputLength				DWORD		0
inputError				DWORD		0
inputSign				SDWORD		1
integerSum				SDWORD		?
integerAverage			SDWORD		?
userOutput				BYTE		INPUT_MAXIMUM DUP(?)
integersDescription		BYTE		"You entered the following: ", 0
addedDescription		BYTE		10, "The sum of your numbers is:", 0
averageDescription		BYTE		"The truncated average of your numbers is: ", 13, 10, 0
seperator				BYTE		", "


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
  PUSH	OFFSET userDirections		; 24
  PUSH	OFFSET inputSign
  PUSH	OFFSET inputError
  PUSH	OFFSET userInput				
  PUSH	OFFSET inputLength
  CALL	 ReadVal

  POP	EDI
  POP	ECX
  ADD	EDI, 4
  LOOP	_InputLoop

  ;Display outputs
  mDisplayString OFFSET integersDescription
  MOV	ESI, OFFSET	integerArray
  MOV	ECX, INTEGER_NUMBER
_Numbers:
  LODSD
  PUSH  ECX
  PUSH	OFFSET userOutput
  PUSH	EAX
  CALL	WriteVal
  POP	ECX
  CMP	ECX, 1
  JE	_Skip
  mDisplayString OFFSET seperator
  LOOP	_Numbers

_Skip:

; Display sum
  mDisplayString OFFSET addedDescription
  MOV	EBX, 0
  MOV	ESI, OFFSET	integerArray
  MOV	ECX, INTEGER_NUMBER
_Sum:
  LODSB
  MOV	EBX, [ESI]
  ADD	EBX, EAX
  ADD	ESI, 4
  LOOP _Sum

  

  

	Invoke ExitProcess,0	; exit to operating system
main ENDP

; ----------------------------------------------------------------------------------------------------------------
; Name: ReadVal
;
; Uses the mGetString macro to get user input in the form of a string of digits, converts the inputted string to an integer and stores this integer in userInteger 
;
; Postconditions: changes register eax, ebc, ecx, edx, esi, edi, ebp and changes to userIntegers
;
; Receives:
;	userInteger = storage for converted user input
;	userInstructions1 = the instructions given to the user
;	programDescription = tells what trhe program will do
;
;	returns: userIntegers
; -----------------------------------------------------------------------------------------------------------------
ReadVal PROC

  ; get a Stack reference point
  PUSH  EBP
  MOV   EBP, ESP

_getInput:

  mGetString [EBP+24], [EBP+12], INPUT_MAXIMUM, [EBP+8], [EBP+28], EAX
  

  ; validate
  PUSH	[EBP+20]									; the sign of the input
  PUSH	[EBP+16]									; error sign
  PUSH	[EBP+8]										; length of input
  PUSH	[EBP+12]									; where the input is stored
  call	validateString

  ; else
  MOV	EAX, [EBP+16]
  MOV	EAX, [EAX]
  CMP	EAX, 0
  JNE	_errorMessage

  ;save as integer
  PUSH	[EBP+40]				; edi 
  PUSH	[EBP+20]				; sign
  PUSH	[EBP+16]				; error
  PUSH	[EBP+8]					; input length
  PUSH	[EBP+12]				; user input
  call	stringToInteger		

  ;check for overflow
  MOV	EAX, [EBP+16]
  MOV	EAX, [EAX]
  CMP	EAX, 0
  JE	_errorMessageEnd

_errorMessage:
  mDisplayString [EBP+32]

  ;reset flag
  MOV	EAX, [EBP+16]
  MOV	DWORD PTR [EAX], 0

  JMP	_getInput

_errorMessageEnd:

  POP	EBP
  RET 32

ReadVal ENDP

; --------------------------------------------------------------------------------- 
; Name: validateString
;  
; Description: This procedure checks each character in the argument string to ensure
;				it is valid. If it is not, the error flag is set.
; 
; Preconditions: Argument addresses should be valid. Data at addresses should be initialized.
; 
; Postconditions: Registers are restored after procedure call.
; 
; Receives: 
;			[EBP + 24] -> Address offset of where user string input is saved.
;			[EBP + 28] -> Address offset of where the user input string length is saved.
;			[EBP + 32] -> Address offset of where the input error flag should be saved.
;			[EBP + 36] -> Address offset of where the sign of user input should be saved.
;
; Returns: The following data may be changed after this procedure:
;			[EBP + 32] -> Address offset of where the input error flag is saved.
;			[EBP + 36] -> Address offset of where the sign of user input is saved.
; ---------------------------------------------------------------------------------
validateString PROC
  ; get a Stack reference point
  PUSH  EBP
  MOV   EBP, ESP

  ; set up loop
  MOV	ESI, [EBP+8]
  MOV	ECX, [EBP+12]

  ; if invalid
  CMP	ECX, 0
  JLE	_inputLengthError
  CMP	ECX, 11
  JGE	_inputLengthError

  MOV	EAX, 0
  CLD
  LODSB

  ; check for sign
  PUSH	[EBP+20]
  PUSH	[EBP+16]
  PUSH	EAX
  CALL	validateFirstCharacter

  ;decrement ECX and verify
  DEC	ECX
  CMP	ECX, 0
  JLE	_validateEnd

_nextCharacter:

  ; clear EAX and startt next digit
  MOV	EAX, 0
  CLD
  LODSB

  ; validate
  PUSH	[EBP+16]
  PUSH	EAX
  CALL	validateCharacter

  ; break loop if invalid
  MOV	EAX, 0
  MOV	EDX, [EBP+16]
  CMP	EAX, [EDX]
  JNE	_validateEnd

  LOOP	_nextCharacter

  JMP	_validateEnd

_inputLengthError:
  MOV	EAX, [EBP+16]
  MOV	DWORD PTR [EAX], 1

_validateEnd:
  POP	EBP
  RET	16

validateString ENDP

; --------------------------------------------------------------------------------- 
; Name: validateFirstCharacter 
;  
; Description: This procedure validates the first character of a user string input.
;				It allows characters that are +, -, or numerical inputs (in ASCII).
;				If the character is not valid, the error flag is set.
;				If a negative sign is found, the sign flag is set to -1.
; 
; Preconditions: Argument addresses should be valid. Data at addresses should be initialized.
; 
; Postconditions: Registers are restored after procedure call.
; 
; Receives: [EBP + 16] -> ASCII character byte (hexadecimal)
;			[EBP + 20] -> Address offset of where the input error flag should be saved.
;			[EBP + 24] -> Address offset of where the sign of the user input should be saved.
;
; Returns: The following data may be changed after this procedure:
;			[EBP + 20] -> Address offset of where the input error flag is saved.
;			[EBP + 24] -> Address offset of where the sign of the user input is saved.
; ---------------------------------------------------------------------------------
validateFirstCharacter PROC

  ; get a Stack reference point
  PUSH  EBP
  MOV   EBP, ESP

  MOV	EAX, [EBP+8]


  CMP	EAX, 43d
  JB	_errorFirstCharEnd
  CMP	EAX, 45d
  JE	_minusSign

  ; check if first value is number
  CMP	EAX, 48d
  JB	_errorFirstChar
  CMP	EAX, 57d
  JA	_errorFirstChar
  JMP	_errorFirstCharEnd

_minusSign:
  ; set sign flag to neg 1
  MOV	EAX, [EBP+16]
  MOV	EDX, -1
  MOV	[EAX], EDX
  JMP	_errorFirstCharEnd

_errorFirstChar:
  MOV	EAX, [EBP+12]
  MOV	DWORD PTR [EAX], 1

_errorFirstCharEnd:
  POP	EBP
  RET 12

validateFirstCharacter ENDP

; --------------------------------------------------------------------------------- 
; Name: validateCharacter 
;  
; Description: This procedure validates a character of a user string input.
;				It allows characters that are numerical inputs (in ASCII).
;				If the character is not valid, the error flag is set.
; 
; Preconditions: Argument addresses should be valid. Data at addresses should be initialized.
; 
; Postconditions: Registers are restored after procedure call.
; 
; Receives: [EBP + 12] -> ASCII character byte (hexadecimal)
;			[EBP + 16] -> Address offset of where the input error flag should be saved.
;
; Returns: The following data may be changed after this procedure:
;			[EBP + 16] -> Address offset of where the input error flag is saved.
; ---------------------------------------------------------------------------------
validateCharacter PROC
  ; get a Stack reference point
  PUSH  EBP
  MOV   EBP, ESP

  MOV	EAX, [EBP+8]

  ; check if character is numeric
  CMP	EAX, 48d
  JB	_error
  CMP	EAX, 57d
  JA	_error
  JMP	_errorEnd

_error:
  MOV	EAX, [EBP+12]
  MOV	DWORD PTR [EAX], 1

_errorEnd:
  POP	EBP
  RET	8

validateCharacter ENDP

; --------------------------------------------------------------------------------- 
; Name: stringToSDWORD 
;  
; Description: This procedure converts a string to a signed double word (SDWORD) value.
;				If the value is too large, the error flag is set.
; 
; Preconditions: Argument addresses should be valid. Data at addresses should be initialized.
; 
; Postconditions: Registers are restored after procedure call.
; 
; Receives:
;			[EBP + 32] -> Address offset of where user string input should be saved.
;			[EBP + 36] -> Address offset of where the user input string length should be saved.
;			[EBP + 40] -> Address offset of where the input error flag should be saved.
;			[EBP + 44] -> Address offset of where the sign of user input should be saved.
;			[EBP + 48] -> Address offset of where the converted SDWORD value should be saved.
;
; Returns: The following data may be changed after this procedure:
;			[EBP + 40] -> Address offset of where the input error flag is saved.
;			[EBP + 48] -> Address offset of where the converted SDWORD value is saved.
; ---------------------------------------------------------------------------------
stringToInteger PROC
  ; get a Stack reference point
  PUSH  EBP
  MOV   EBP, ESP

  ; prepare registers	
  MOV	ESI, [EBP+8]
  MOV	EDI, [EBP+24]
  MOV	ECX, [EBP+12]
  MOV	EBX, 0
  MOV	EDX, 1

  ; ESI to last
  MOV	EAX, ECX
  DEC	EAX
  ADD	ESI, EAX

_addInteger:
  MOV	EAX, 0
  STD
  LODSB

  CMP	EAX, 43d
  JE	_multiplyBySignFlag
  CMP	EAX, 45d
  JE	_multiplyBySignFlag

  SUB	EAX, 48d

  PUSH	EDX
  IMUL	EDX
  ADD	EBX, EAX

  JO	_overflowError

  ; next cycle
  POP	EDX
  MOV	EAX, EDX
  MOV	EDX, 10
  IMUL	EDX
  MOV	EDX, EAX

  LOOP	_addInteger

_multiplyBySignFlag:
  MOV	EAX, EBX
  MOV	EBX, [EBP+20]
  MOV	EBX, [EBX]
  IMUL	EBX

  ;reset sign
  MOV	EBX, [EBP+20]
  MOV	SDWORD PTR [EBX], 1

  ;save to array
  MOV	[EDI], EAX

  JMP	stringToIntegerEnd

_overflowError:
  POP	EDX
  MOV	EAX, [EBP+16]
  MOV	DWORD PTR [EAX], 1

stringToIntegerEnd:
  POP	EBP
  RET	20

stringToInteger ENDP

; --------------------------------------------------------------------------------- 
; Name: WriteVal 
;  
; Description: This procedure converts signed double word (SDWORD) integer values to
;				strings and prints them to the console.
; 
; Preconditions: Argument addresses should be valid. Data at addresses should be initialized.
; 
; Postconditions: Registers are restored after procedure call.
; 
; Receives: [EBP + 28] -> SDWORD value to convert to string
;			[EBP + 32] -> Address offset of where output string should be saved.
;
; Returns: The following data may be changed after this procedure:
;			[EBP + 32] -> Address offset of where output string is saved.
; ---------------------------------------------------------------------------------
WriteVal PROC
  ; get a Stack reference point
  PUSH  EBP
  MOV   EBP, ESP

  ; prepare registers
  MOV	EDX, [EBP+8]
  MOV	EDI, [EBP+12]
  MOV	ECX, 10
  MOV	EBX, 1000000000

  CMP	EDX, 0
  JL	_negativeNumber
  JMP	_getChar

_negativeNumber:
  NEG	EDX

  MOV	EAX, "-"
  STOSB

_getChar:

 ; leading digit
 MOV	EAX, EDX
 CDQ
 DIV	EBX

 PUSH	EDX

 ; if leading digit not 0, save
 CMP	EAX, 0
 JNE	_saveChar
 CMP	ECX, 1
 JE		_saveChar

 PUSH	EAX
 PUSH	EBX
 MOV	EAX, [EBP+12]

_checkNextChar:
  MOV	BL, BYTE PTR [EAX]
  CMP	BL, 49d
  JGE	_nonLeadingZero

  INC	EAX
  CMP	EDI, EAX
  JLE	_leadingZero
  JMP	_checkNextChar

_leadingZero:
  POP EBX
  POP EAX
  JMP	_saveCharEnd

_nonLeadingZero:
  POP EBX
  POP EAX

_saveChar:
  ADD	EAX, 48d
  STOSB

_saveCharEnd:
  MOV	EAX, EBX
  CDQ
  MOV	EBX, 10
  DIV	EBX
  MOV	EBX, EAX

  POP	EDX

  LOOP	_getChar

  MOV	EAX, 0
  STOSB

  ; Display output string
	MOV	EAX, EBP
	CMP	EAX, 1703720
	JE	_FirstRound
	mDisplayString	[EBP + 12]

_FirstRound:

  ;clear output string
  MOV	ECX, INPUT_MAXIMUM
  MOV	EDI, [EBP+12]
  MOV	EAX, 0
  REP	STOSB
  POP	EBP
  RET	8

WriteVal ENDP

; --------------------------------------------------------------------------------- 
; Name: printOutput 
;  
; Description: This procedure prints each of the numbers in the argument array. It
;				then calculates and prints the sum of the numbers, as well as the
;				rounded average of the numbers.
; 
; Preconditions: Argument addresses should be valid.
; 
; Postconditions: Registers are restored after procedure call. Data at addresses should be initialized.
; 
; Receives: [EBP + 28] -> Address offset of title for entered numbers ("You entered these numbers: ").
;			[EBP + 32] -> Address offset of title for sum of numbers ("The sum is: ").
;			[EBP + 36] -> Address offset of title for average of numbers ("The average is: ").
;			[EBP + 40] -> Address offset of the array of numbers to be displayed, summed, and averaged.
;			[EBP + 44] -> Address offset of where each output string should be saved.
;
; Returns: The following data may be changed after this procedure:
;			[EBP + 44] -> Address offset of where each output string should be saved.
; ---------------------------------------------------------------------------------
printOutput PROC
  ; get a Stack reference point
  PUSH  EBP
  MOV   EBP, ESP

  ;prepare registers
  MOV	ECX, INTEGER_NUMBER
  MOV	ESI, [EBP+8]
  
  
  ; entered numbers
  CALL	CrLF
  mDisplayString  [EBP+24]

_printNumber:

  LODSD

  ; write as string
  PUSH	[EBP+12]
  PUSH	EAX
  CALL	WriteVal

  ; at last number
  CMP	ECX, 1
  JE	_calculateSum

  LOOP	_printNumber

_calculateSum:

  ; prepare registers
  MOV	ECX, INTEGER_NUMBER
  MOV	ESI, [EBP+8]
  MOV	EBX, 0

_sumNext:

  LODSD
  add	EBX, EAX
  LOOP	_sumNext

  CALL CrLf
  CALL CrLf
  mDisplayString [EBP+20]

  MOV	EAX, EBX
  PUSH	[EBP+12]
  PUSH	EAX
  CALL	WriteVal

_calculateAverage:
  MOV	EBX, INTEGER_NUMBER
  CDQ
  IDIV	EBX
  
  CALL CrLf
  CALL CrLf
  mDisplayString [EBP+16]

  PUSH	[EBP+12]
  PUSH	EAX
  CALL	WriteVal

  POP	EBP
  RET	16

printOutput ENDP

END main
