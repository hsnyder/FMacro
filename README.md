# FMacro

FMacro is a macro preprocessor for Fortran. 
Modern Fortran is an excellent language for scientific computing, 
but one of its shortcomings is the lack of strong support for generic programming. 
FMacro aims to solve this through the use of a simple preprocessor,
modeled after some of the language committee's early ideas for a 
template language feature, to be included in a future Fortran standard. 

*Warning:* FMacro is currently alpha software.

## Feature Summary

Command line usage: `fmacro [typefile] inputfile`, where `inputfile` is a fortran source file containing 
FMacro directives, and `typefile` is an optional argument that specifies the location of the type list file 
(described below), if different from the default (`./Typefile`). 
The preprocessor's output is written to standard output. 

FMacro requires a "type file", which is a plain text file with a list of types, one type per line. 
Each macro in the input source file will be instantiated once per type in the typefile. 

An example typefile:

```
integer(int32)
real(real32)
```

Macro blocks are one or more lines of code, situated between `!$macro(...)` and `!$end macro`.
A macro block should enclose exactly one subroutine or function. 
Inside the parentheses following `!$macro` is a symbol that stands in for a type. 
For example, `!$macro(T)` defines `T` to be a type parameter. 
During preprocessing, the macro will be duplicated once for each type in the type file, and 
any occurrences of `type(T)` inside the block will be replaced by the corresponding type.
The name of the output subroutine or function will be suffixed with a unique string that identifies the type.

For example:

```
!$macro(T)
	subroutine whatever(a,x,b,o)
		type(T), intent(in)  :: a, x(:), b
		type(T), intent(out) :: o(:)
		o(:) = a*x(:) + b
	end subroutine
!$end macro
```

The above macro, combined with the example type file from above, would be processed into:

```
	subroutine whatever_integer_int32_(a,x,b,o)
		integer(int32), intent(in)  :: a, x(:), b
		integer(int32), intent(out) :: o(:)
		o(:) = a*x(:) + b
	end subroutine
	subroutine whatever_real_real32_(a,x,b,o)
		real(real32), intent(in)  :: a, x(:), b
		real(real32), intent(out) :: o(:)
		o(:) = a*x(:) + b
	end subroutine
```

You can do the same thing in interface blocks

```
interface whatever
!$macro(T)
	subroutine whatever
!$end macro
end interface
```

which would be rendered out to the following.

```
interface whatever
	subroutine whatever_integer_int32_
	subroutine whatever_real_real32_
end interface
```

The use of the `procedure` keyword is supported for this purpose.

## Limitations

- Source file lines must be shorter than 256 characters (note this is well in excess of what the Fortran standard allows).
- Maximum number of types in a type file: 10,000.
- Maximum number of lines in a single macro block: 100,000.
- Source files containing ASCII character codes 26 or 30 will badly confuse the parser (these are invalid, nonprinting characters anyway, so this will probbaly never matter). 
- Function / subroutine names, as well as type symbols, must be drawn from the following set of characters: `abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_`
- The name of a function or subroutine to be macrod needs to be on the same line as the "function", "subroutine" or "procedure" keyword (it cannot be on the next line, even if '&' is used).


## Building

FMacro is a single source file, just point your compiler at `fmacro.f90`. 
