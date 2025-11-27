A MASM program that perform the following tasks:

Implements and tests two macros for string processing. These macros use Irvine’s ReadString to get input from the user, and WriteString procedures to display output.
  mGetString:  Displays a prompt (input parameter, by reference), then get the user’s keyboard input into a memory location (output parameter, by reference). Also provides a count for the length of input string that can be accommodated.
  mDisplayString:  Prints the string which is stored in a specified memory location (input parameter, by reference).

Implements and tests two procedures for signed integers which use string primitive instructions
  ReadVal: 
    Invokes the mGetString macro to get user input in the form of a string of digits.
    Converts the string of ascii digits to its numeric value representation (SDWORD), validating the user’s input is a valid number (no letters, symbols, etc).
    Stores this one value in a memory variable (output parameter, by reference). 
  WriteVal: 
  Converts a numeric SDWORD value (input parameter, by value) to a string of ASCII digits.
  Invokes the mDisplayString macro to print the ASCII representation of the SDWORD value to the output.
  
Writes a test program which uses the ReadVal and WriteVal procedures above to:
  Get 10 valid integers from the user. 
  Stores these numeric values in an array.
  Display the integers, their sum, and their truncated average.
