# FMacro

FMacro is a macro-focused preprocessor for Fortran. 

*Why?*
Modern Fortran is a very nice language, but one of it's shortcomings is the lack of generic programming support. 
C gets around this issue through it's very capable preprocessor. 
Fortran does not have a standard preprocessor. 
Many compilers permit the use of the C preprocessor in Fortran code, but unfortunately the implementation is not
very consistent across different compilers, once you start looking at the details like macro argument concatenation
and stringification. 
Many other preprocessors have been written, but the ones I'm aware of either don't have clean support for macros,
or require external dependencies like Python to be installed.
The only thing that we can be sure a Fortran programmer already has installed is a Fortran compiler, 
so FMacro is written Fortran.

## Specification

FMacro is designed to be immediately recognizable for anyone familiar with the C preprocessor,
but not be *so* similar in syntax as to be ambiguous about whether the C preprocessor or FMacro is being used.
With that in mind...

Any line in a Fortran source file on which the first two nonwhitespace characters are `!%` is treated as an FMacro command.

### Macros

A macro definition begins with the sequence `!%define`, followed by the macro name. 
If the macro takes arguments, the macro name may be followed by the argument list, in parentheses.
For example:
```
    !%define LINUX 1
    !%define FREEBSD 2 
    !%define unless(condition) if(.not. condition)	
``` 

At any point in the file after a macro has been defined, it can be used, simply by writing the macro name.
For example, with the above macro definitions,
```
unless(os == LINUX) then
```
would become
```
if(.not. os == 1) then
```

### Multiline Macros
A macro can also be a *multiline* macro, in which case `!%define_ml` is used instead of `!%define`.
A multiline macro definition includes all subsequent lines, until the first noncomment line is encountered.
Newline characters are preserved.
A multiline macro may contain single-line macro definitions, but may not contain a multi-line macro.
For example:
```
!%define_ml X(TYPE)
!
! subroutine mysub(a)
!     TYPE :: a
!     ...
! end subroutine

X(real)
X(integer)
```

Would become:
```
 subroutine mysub(a)
     real :: a
     ...
 end subroutine
 subroutine mysub(a)
     integer :: a
     ...
 end subroutine
```
Of course, this won't compile because the two subroutines have the same name (the solution to this will be explained shortly),
but the example illustrates the point.

### Stringification

Inside a macro definition, the character # preceding a macro argument will be rendered as the macro argument, enclosed in quotation marks.
Whitespace is permitted after the # symbol.
For example,
```
!%define assert(condition) if(.not. condition) error stop "ASSERTION FAILED: " // #condition

assert(x > 10)
```
Would be rendered as
```
if(.not. x > 10) error stop "ASSERTION FAILED: " // "x > 10"
```

### Concatenation

Inside a macro definition, tokens on either side of the operator ## will be concatenated, after expanding their values as macros, if applicable.
Returning to the multiline macro example:
```
!%define_ml X(TYPE)
!
! subroutine mysub_ ## TYPE (a)
!     TYPE :: a
!     ...
! end subroutine

X(real)
X(integer)
```
Would become:
```
 subroutine mysub_real(a)
     real :: a
     ...
 end subroutine
 subroutine mysub_integer(a)
     integer :: a
     ...
 end subroutine
```

### Built-in macros

The macros `$LINE`, `$OUTLINE` and `$COUNTER` are built in to FMacro. 
`$LINE` evaluates to the line number in the input file (before preprocessing).
`$OUTLINE` evaluates to the line number in the output file (after preprocessing).
`$COUNTER` evaluates to an integer number, starting at 1 and increasing by 1 each time `$COUNTER` is used.

The command `!%resetcounter` can be used to reset the value of `$COUNTER` to 1.

### Undefining macros

Any user-defined macro can be undefined with `!%undef` folled by the macro name.

### Including macros from another file

The command `!%includemacros`, followed by a file name includes all the macro definitions present in the specified
file in the current file.
Nothing other than macro definitions is included.
The specified file must be located in the same directory as the current file, or in a specified include directory
(see command line interface section, below).


### Command line interface

FMacro acceps the following command line arguments:

`-I`, followed immediately by a directory path (no whitespace between I and the path) will cause the specified directory to 
be searched by `%includemacros` commands.

`-Da=b`, defines a macro called `a`, with the value of `b`.
Macros with parameters cannot be defined on the command line.
