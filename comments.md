# Comments

In this file we have placed the comments that have been made regarding the Haskell code in the CAS.hs file. The comments are very detailed and were taking too much space in the code file so they have been moved here. They have been given unique labels so they can be searched very quickly.

## A. Module declaration

**A.1**

* We start by declaring this .hs file to be module. The module must have the same name as the file (CAS.hs).hs
* In the module identifier after the module's name and delimited in parentheses we declare the classes, types and functions we want to export from the module.
* The parentheses are followed by the keyword where and then the rest of the file is dedicated to defining the various objects that are being exported.


## B. Expr type

**B.1** - We define the new data typeclass 'Expr' which corresponds to a general algebraic expression. This is achieved by using recursion in the definition of the constructors. By defining these carefully one can use patterm-matching to implement various functions for the 'Expr' typeclass.

**B.2** - We declare 'Expr a' to be a type class where 'a' can be any concrete type

**B.3** - A constant (basically a number) can be an expression by itself (the most basic kind there is)

**B.4** - This is a recursive constructor with the Constructor name 'Sum' and which takes a list of 'Expr a' objects as its constructor parameters

**B.5** - The reciprocal of an expression (1 / Expr)

**B.6** - Expression raised to an INTEGER power.

**B.7** - The algebraic variables in the expression: x, y, z, .etc

**B.8** - We declare Expr to be an instance of the Eq class since we will want to compare expressions for equality