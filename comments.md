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


## C. Expr instance of Show

**C.1**

* We declare 'Expr' to be an instance of the 'Show' typeclass. Since 'Expr a' is a typeclass with a type-parameter 'a' we declare that in our declaration of the instance we limit ourselves to types of class 'Show' i.e. this instance only refers to types 'Expr a' where 'a' itself is an instance of the Show class. This is the '(Show a) =>' part.
* Then comes the actual instance declaration 'Show (Expr a)'. We need the parentheses when a type-parameter is included.
* Lastly comes the 'where' keyword and after it on the following lines comes the functions that must be defined for a type to be an instance of the class. In the case of the 'Show' typeclass this only one function 'show'

**C.2** -  We pattern match on the 'Const a' constructor. In the case of constant we simply show the number. The 'a' in this line is NOT the same as the 'a' in the instance declaration line above it. Here 'a' matches the value inside the 'Const a' constructor. Since the instance declaration limits 'Expr a' to type-parameters 'a' that are an instance of 'Show' so we can run the 'show' method directly on the value 'a' inside the 'Const a' parameter

**C.3** - We use the utility function showList to print the expression with its parts separated by the " + " symbol

**C.4** - The negation of an expression is simply appending '-' in front of it. We use the concatenation operator ':' to preprend '-' in front of the String representation of the expression

**C.5** - Since 's' is a String (from the definition of the 'Symbol' constucor) we don't need to use 'show' here. Had we done so it would have printed the strings surrounded by quotation marks

**C.6**

* This is a utility function (NOT exported) for showing a list of expressions. The separator (+ or *) is specified.
* The empty list should not occur but if it does we simply print (0)
* For a non-empty list we print the surrounding parentheses and use another related utility function to print the meat of the expression.

**C.7** - In this utility function for a single element we simply print the expression within. For a larger list we use a head:tail pattern-match to extract the head show it, add the separator and then use recursion on the rest.


## D. Expr instance of Num

**D.1**

* We define Expr to be an instance of the Num class which gives us access to the standard arithematic operations. It is rather evident that we expect the type-parameter 'a' to be an instance of the Num class itself.
* The really interesting thing is how we will map the arithematic operators on to the constructors of the Expr class. Basically we use the operators to recursivley construct every more complex expressions. This is why we will have to define simplify to carry out obvious simplification of the expression later on.

**D.2** - We want ONLY constant integers in our expressions so we limit the type-constraint from the usual 'Num a' to 'Integral a'. This means that any calls to the methods defined in this class which uses non-integer Constants will raise an exception.

**D.3** - We now use a function (sum') to carry out the actual addition. This allows us to write a specialized function to this effect.

**D.4**

* This definition is very clear. It however obfuscates the fact that this is a curried function. To see that one can define it equivalently as: ``(-) a b = sum' a (Neg b)``
* And now one can curry it as follows: ``let d = (-) (Const 4)`` then one can apply 'd' to other expressions: ``d x``  --->  ``(4 - x) = sum' (Const 4) (Neg (Symbol "x"))``

**D.5** - The Num typeclass defines a number of functions that we are not interested in implementing since they do not make sense for algebraic expressions. For these we simply define them to be equal to the 'undefined' function which will raise en exception if these are used.

**D.6**

* We define the fromInteger method which gives us the ability to detect integers in expressions from context and convert them in to expressions of type Const. An example will clarify.
* With fromInteger so defined we can write '2 * x' and it will be interpreted and converted in to 'Const 2 * Symbol "x"'. This will save us from having to write 'Const 2' all the time.
