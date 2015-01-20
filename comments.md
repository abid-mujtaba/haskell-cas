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



## E. Expr instance of Fractional

**E.1**

* The instance declaration has a minor subtlety in the type-constraint 'Integral a'. Usually in such an overloading scenario the type-constraing would be (Fractional a). Note in the definition of the type Expr that the only constructor that (without recursion) uses the type-parameter 'a' is 'Const a'. For our application we are ONLY interested in expressions that contain integer constants so we restrict the type parameter 'a' here to 'Integral a'.
* If a non-integer constant is found see E.2

**E.2** - We define the fromRational method to be an 'error' so that when this method is called the specified error message is printed. This ensures that the constants in our expressions are only allowed to be integers. We will deal with rational fractions by using Prod and Rec.



## F. Functions for arithematic operations

**F.1** - We are carrying out pattern-matching where the first pattern that matches is the one that is used. So our first pattern matches the addition of two Sum expressions with xs and ys matching the lists encapsulated by both. The result is simply another Sum with the two lists concatenated.

**F.2** - The next two patterns match the possibility of a single Sum expression being added to a non-sum expression (since the case of two Sum-s being added corresponds to the pattern at the top). In such a case we simply add the expression to the list within the Sum.

**F.3** - The final pattern which is the catch-all corresponds by elimination (via the upper patterns) to the case of two non-Sum expression being added. In this case we simply construct a list of two elements and put it inside the Sum.



## G. Simplification functions for expressions

**G.1**

* We define the simplification for a Sum expression that encapsulates a list of expressions.
* We use pattern matching on the RHS to get the list of expressions 'xs'
* On the RHS we first use simplify_sum on the list of expressions. Then we pass the result on to 'empty_sum' which tests for the possibility of an empty list.



## H. Collection of Const terms inside a Sum

**H.1**

* We use a let expression to bind ``(c, es)`` to the result of the ``foldr``. We use this binding in the if statement that follows. If after the ``foldr`` the collected constant value is 0 we simply return the collected list ``es``.
* If ``c != 0`` then we simply append a ``Const`` expression corresponding to ``c`` at the end of the list of expressions ``es``
* The ``foldr`` takes a binary function, an initial accumulator which in this case is the tuple ``(0, [])`` and then folds the function over the list of expressions.
* The idea is that the current accumulator and one element of the list is fed to the binary function ``fold_constants``. The function analyzes the element. If the element is a ``Const`` then its value is added to the first member of the accumulator which keeps track of the sum of constant values.
* If the element passed in to ``fold_constants`` is NOT a ``Const`` then we leave the sum value unchanged and append the element to the list of expressions which forms the second part of the accumulator.
* With this in mind it is obvious that the initial accumulator which appears in ``foldr`` must be ``(0, [])`` because we start with a constants sum value of 0 (and add to it element by element) and we start with an empty list to which we append non-Const expressions as we fold over the list ``xs``
* By using a ``foldr`` here we get to keep the order of elements from the original list



## I. fold_constants

*Binary function for collecting terms inside a Sum*

**I.1**

* The desired functionality for this binary function is defined in comment H.1.
* Because it is to be used in a ``foldr`` (as contrasted with a ``foldl``) the element of the folded list is the first argument to ``foldr`` and the accumulator is the second.
* Consequently the signature starts with an ``Expr a`` for the element and then ``(a, [Expr a])`` for the accumulator and returns an object of the same type as the accumulator
* Note: By using a type-constraint of ``Integral a`` we can refer to integers inside ``Const n``. We use ``(a, [Expr a])`` because we want the first element of the accumulator to have the same type as that encapsulated by the ``Const``. It should be further noted that the definition of ``Expr`` shows that the only value constructor of ``Expr`` that uses the type-parameter ``a`` in its definition is ``Const``.

**I.2**
* We use pattern matching to access the element 'e', accumulator sum value 'm' and the collected list of expressions 'xs'
* On the RHS we use a case expression to pattern-match on the element 'e' which is of type 'Expr a'
* The first pattern we match is for 'e' being a Const with value n. In this case we simply add n to the current accumulator value m and leave the list of expressions unchanged (basically removing the Const from the list and placing its value inside the sum integer)

**I.3** The second pattern matches for a negative constant and behave analogous to the first pattern.

**I.4** If the element 'e' is not a (negative) constant we leave the sum value 'm' unchanged and prepend the element to the collected list. By virtue of prepending and not appending in a function that is used in a ``foldr`` we retain the ordering of the original list.



## J. empty_sum

*Function that deals with the possibility of the list inside the Sum being empty*

**J.1** - If the list of expressions destined to be encapsulated by Sum is empty (tested using the ``null`` function) we return a ``Const 0`` which is the logical equivalent of an empty sum (e.g. 3 + -3 = 0).

**J.2** - If the list contains a single expression then we return just that expression (e.g. x + 0 = x). Otherwise we return the list encapsulated by a ``Sum``