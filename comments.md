# Comments

In this file we have placed the comments that have been made regarding the Haskell code in the CAS.hs file. The comments are very detailed and were taking too much space in the code file so they have been moved here. They have been given unique labels so they can be searched very quickly.

At the bottom of the file are general comments about the development process and debugging.


## A. Module declaration

**A.1**

* We start by declaring this .hs file to be module. The module must have the same name as the file (CAS.hs).hs
* In the module identifier after the module's name and delimited in parentheses we declare the classes, types and functions we want to export from the module.
* The parentheses are followed by the keyword where and then the rest of the file is dedicated to defining the various objects that are being exported.

**A.2** - ``Expr(Symbol)`` signifies that ONLY its ``Symbol`` value constructor is to be exported. All of the other constructors are hidden. Our intention is to allow a user to build up complex expressions by starting with ``Symbol``s and ``Const`` values as building blocks and tying them together with arithmetic operations without having to explicitly use the rest of the value constructors. To access ``Const`` values we use the ``const'`` function described in *A.3*

**A.3** - We export the ``const'`` function as it provides the only mechanism available outside the module for creating ``Const`` objects (since we did NOT export ``Const``).

**A.4** - We export the ``^`` infix function which we have defined in the module (overriding the Prelude definition of the same)



## B. Expr type

**B.1** - We define the new data typeclass 'Expr' which corresponds to a general algebraic expression. This is achieved by using recursion in the definition of the constructors. By defining these carefully one can use pattern-matching to implement various functions for the 'Expr' typeclass.

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

**C.4** - The negation of an expression is simply appending '-' in front of it. We use the concatenation operator ':' to prepend '-' in front of the String representation of the expression

**C.5** - Since 's' is a String (from the definition of the 'Symbol' constructor) we don't need to use 'show' here. Had we done so it would have printed the strings surrounded by quotation marks

**C.6**

* This is a utility function (NOT exported) for showing a list of expressions. The separator (+ or *) is specified.
* The empty list should not occur but if it does we simply print (0)
* For a non-empty list we print the surrounding parentheses and use another related utility function to print the meat of the expression.

**C.7** - In this utility function for a single element we simply print the expression within. For a larger list we use a head:tail pattern-match to extract the head show it, add the separator and then use recursion on the rest.



## D. Expr instance of Num

**D.1**

* We define Expr to be an instance of the Num class which gives us access to the standard arithmetic operations. It is rather evident that we expect the type-parameter 'a' to be an instance of the Num class itself.
* The really interesting thing is how we will map the arithmetic operators on to the constructors of the Expr class. Basically we use the operators to recursively construct every more complex expressions. This is why we will have to define simplify to carry out obvious simplification of the expression later on.

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



## F. Functions for arithmetic operations

**F.1**
 
* We are carrying out pattern-matching where the first pattern that matches is the one that is used. So our first pattern matches the addition of two Sum expressions with xs and ys matching the lists encapsulated by both. The result is simply another Sum with the two lists concatenated.
* NOTE the use of the simplification method ``s``. This means that every arithmetic operation ``+``, ``*``, etc., concludes with a simplification to ensure that the expressions remain as compact as possible. 

**F.2** - The next two patterns match the possibility of a single Sum expression being added to a non-sum expression (since the case of two Sum-s being added corresponds to the pattern at the top). In such a case we simply add the expression to the list within the Sum.

**F.3**

* The final pattern which is the catch-all corresponds by elimination (via the upper patterns) to the case of two non-Sum expression being added.
* We use guards to differentiate between two possible scenarios. If the two expressions being added are equal we convert the Sum in to a Prod by multiplying the expression by ``2``; otherwise we simply place both inside a Sum expression.



## G. Simplification functions for expressions

**G.1**

* The purpose of the simplification of a ``Const`` is to ensure that NO ``Const`` object encapsulates a negative integer.
* We use guards to check if the polarity of ``c`` is negative in which case we transform the ``Const`` in to a ``Neg (Const _)``.
* When ``c`` is NOT negative the original ``Const`` should be returned.
* We use the ``@`` symbol to generate an *as-pattern* with which we bind the name ``o`` to the pattern that has been successfully matched against on this line.
* We use the name ``o`` to refer to the original ``Const`` when we return it with the last ``otherwise`` guard.


**G.2**

* We don't want the exponentiation of a ``Const`` to be stored inside an ``Exp`` object so we use pattern-matching to detect this possibility and construct the corresponding ``Const`` object.
* To do so we use the ``^`` function from ``Prelude`` using the qualified import we made earlier. It raises the integer value inside the ``Const`` to the relevant power and then we store the result in a ``Const`` value to get back an ``Expr a`` as the function expects.

**G.3** - Separate pattern for negative constant raised to a power since that isn't matched by the first pattern. Note the use of ``s`` to simplify the final ``Const`` in case the integer inside is negative. ``s`` will change that to a ``Neg (Const _)``

**G.4**

* We define the simplification for a Sum expression that encapsulates a list of expressions.
* We use pattern matching on the RHS to get the list of expressions 'xs'
* The actual simplification is carried out by the ``simplify_sum`` utility function (this is analogous to how the action of ``+`` is carried out by ``sum'``).

**G.5** - When we run out of patterns to match for simplification it means the expression cannot be simplified and so we return it as is.



## H. Collection of Const terms inside a Sum

**H.1** 

* We use a let expression to bind ``(c, es)`` to the result of the ``foldr``. We use this binding in the function evaluation that follows. 
* The ``foldr`` takes a binary function, an initial accumulator which in this case is the tuple ``(0, [])`` and then folds the function over the list of expressions.
* The idea is that the current accumulator and one element of the list is fed to the binary function ``fold_constants``. The function analyzes the element. If the element is a ``Const`` then its value is added to the first member of the accumulator which keeps track of the sum of constant values.
* If the element passed in to ``fold_constants`` is NOT a ``Const`` then we leave the sum value unchanged and append the element to the list of expressions which forms the second part of the accumulator.
* With this in mind it is obvious that the initial accumulator which appears in ``foldr`` must be ``(0, [])`` because we start with a constants sum value of 0 (and add to it element by element) and we start with an empty list to which we append non-Const expressions as we fold over the list ``xs``
* By using a ``foldr`` here we get to keep the order of elements from the original list

**H.2** - The collected constant value ``c`` along with the simplified list of expressions ``es`` returned by the ``foldr`` is passed to the function ``append_constant`` which is defined using the ``where`` keyboard. We structure it this way because using guards in a function is the cleanest way of explaining what the code does.

**H.3.**

* If after the ``foldr`` the collected constant value ``c`` is 0 we simply return the collected list of expressions ``es``.
* If ``c > 0`` then we simply append a ``Const`` expression corresponding to ``c`` at the end ``es``.
* The remaining case corresponds to ``c < 0`` and we append it as a positive constant encapsulated in a ``Neg``. The idea is to keep *all constants in all expressions strictly positive* and use ``Neg`` where necessary.



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



## K. Simplification of a Product Expression

**K.1**

* This is analogous to ``collect_sum_const`` with one major difference. We want to keep the numerator (``n``) and denominator (``d``) constants separate.
* To that end the accumulator consists of two integers ``n`` and ``d`` and the simplified list of expressions ``es``.
* ``fold_prod_constants`` takes an element of the list and checks (pattern-matches) agains a constant in either the numerator or denominator.
* The initial accumulator consists of ``(1, 1, [])`` since the identity element for multiplication is ``1``.
* NOTE: This is monoid behaviour. Eventually one can implement Sum and Prod as monoids along with the functions ``+`` and ``*`` respectively.

**K.2** - We use the case expression to pattern match against the final values of ``n`` and ``d`` (which we get from the ``foldr``). Since the list of expressions is for a ``Prod`` a constant value of ``1`` is redundant (just as ``0`` is redundant in a sum), so we pattern match to deal with these possibilities.

**K.3** 

* Again we use the case expression to pattern match against the element of the expression list (an arbitrary expression). If it is a ``Const`` we multiply it with the accumulator's numerator value.
* If it is ``Rec (Const m)`` it is a constant in the denominator so we multiply it with the accumulator's denominator value.
* Otherwise we simply append the expression to the accumulator's expression list ``es``

**K.4** - A careful study of ``collect_prod_const`` will reveal that there exists a special case where both ``n`` and ``d`` are equal to ``1`` and ``es`` is null (``[]``). In this case the function returns an empty list (a Product with no elements). This actually corresponds to a value of ``1`` which is what the corresponding pattern match returns.



## L. Expr instance of Ord

**L.1** - We only need to define the ``compare`` function while making Expr an instance of the Ord class. Since it takes two objects, compares them and generates the resulting ``Ordering`` object (with values of ``LT``, ``EQ`` or ``GT``) we get the remaining comparison operators (``<``, ``<=``, ``==``, ``>``, ``>=``) for free. In addition the Data.List.sort function will also work automatically once ``Expr`` is made an instance of the ``Ord`` type-class.

**L.2** - We pattern-matched to look at the scenario where we are comparing two ``Const`` objects. We extracted the integers within and then set the result to be equal to the comparison of the encapsulated integers. This is a classic technique in Haskell when dealing with encapsulated objects (also knows an objects with context).

**L.3** - In this pattern we are assuming that all ``Const`` objects ONLY encapsulate **positive** integers. This is guaranteed to work because of the way we handle expression construction in which we are careful to keep ``Const`` objects positive.

**L.4** - We specify that a constant has lower sorting order than a symbol.

**L.5** - We define the ``Neg`` of an expression to have the same order as the expression itself.

**L.6** - A reciprocal of an expression has a degree which is the negative of the expression itself. Thus in general Reciprocal expressions have lower order.

**L.7** - Any pair of expressions that don't fit any of the patterns above are caught by this pattern and passed on to the ``compareDegree`` function.

**L.8** 

* When comparing two exponents we use pattern matching to access both the inner expression and the power (index) by which they are raised.
* We use guards to deal with the various cases: The powers match, the various comparisons of the total degree of each expression, etc.
* Note the use of the ``where`` keyboard to calculate and define the constants ``da`` and ``db`` which are used in the guards.

**L.9**

* Catch-all pattern for comparing expressions which are NOT both exponents.
* We compare the degree of both expressions to calculate an order.
* Note that when the degrees are equal we pass on the arguments to the ``compare'`` function.

**L.10** - We leave this function undefined for now because we are unsure how to proceed at this depth. A possible refactor using recursion may be in order.S
 



## M. showActual

*Method for showing the actual structure of an ``Expr`` by printing out how it is made up of the constituent Value Constructors. It is used for debugging.*

**M.1** - The simplest pattern to deal with. If an expression is simply a ``Const`` we simply print out ``Const <constant>``. Note the use of string concatenation using ``++`` and the use of the ``show`` function to convert the integer constant ``c`` in to its String representation.

**M.2** - For ``Symbol`` we simply return the String it encapsulates. Note that we don't use ``show`` here because using it on Strings adds additional quotes around the object which we don't want.

**M.3** - For ``Neg`` we use a parenthesis to indicate its bounds and we use recursion by calling ``showActual`` on the expression inside the ``Neg``.

**M.4**

* We output is bounded on either side by ``"Sum ["`` and ``"]"``. 
* The text in the middle is created by applying a ``foldl`` over the list of expressions inside the ``Sum``.
* The ``foldl`` uses the ``foldListElement`` over the elements and starts with an empty String as the accumulator.
* After the ``foldl`` we have a String which corresponds to a comma-separated list of String representations of elements.
* Because of the way the ``foldListElement`` is implemented this String has an extraneous ``", "`` at the beginning. We get rid of it by using the ``drop 2`` function which removes the first two elements of the String.

**M.5**

* The purpose of this function is to be used in a ``foldl``. It is supposed to be mapped over a list of expressions converting them in to ``showActual`` Strings and then concatenating them separated by ``", "``.
* To the accumulator we add first the String ", " and then the String representation of the current element (expression) by recursively calling ``showActual``.
* Note: By adding ``", "`` before each element we end up with a String that has ``", "`` at the start but NOT at the end. We like it this way because removing elements from the start of a list is much easier (and more efficient) than from the end of a list (simply use ``drop 2``).


## N. ``Const`` Constructor

*A function for creating and returning a ``Const`` object.

**N.1** - The purpose of this function is to act as a constructor. We need a special function for this because we want to ensure that **ALL** ``Const`` objects **ONLY** contain positive integers.
 
**N.2**

* Note the use of ``fromIntegral`` to convert the ``Integral`` ``c`` to an ``Int`` which is what is required by the ``Const`` constructor by definition.
* When using function composition the argument to the composition needs to be separated because composition has lower precedence than function application, hence the ``$`` before the argument ``c``.
* Once again we need a ``$`` to separate ``abs.fromIntegral`` from ``Const`` so that everything on the RHS of ``Const`` is calculated first, before it is used as an argument by ``Const``. An equivalent construction would be ``Const . abs . fromIntegral $ c`` where we include the ``Const`` constructor in the function composition because the constructor is after all like any other function. The choice of which construct to use is a preference, in this case the readability of the code.



## O. Degree of (Polynomial) Expression

**O.1**
 
* This is classic Haskell. We write down what something is NOT how to calculate it.
* In this case we use pattern-matching to define the degree for each Value Constructor.
* Some of them are obvious such as the degree of a constant or a symbol by itself.
* Others use recursion such as the degree of the negative of an expression is equal to that of the expression itself, hence the definition for ``Neg e``

**O.2** - A product of polynomial expressions has degree equal to the sum of the degrees of its constituent parts (by definition). So we ``map`` ``degree`` (recursively) over the list to get a corresponding list comprising of the degree of each element of ``xs`` and then we use ``sum`` to add all of these number up to get the final result.

**O.3** 

* The degree of the sum of polynomials is the highest (maximum) of the degree of its parts. To calculate this we ``map`` ``degree`` over ``xs`` and then ``foldl1`` the ``max`` function over it.
* ``max`` is a binary function that returns the larger of its two arguments.
* We want to implement ``max`` over the whole list so we naturally use a fold. In this case ``fold11`` because with it the first element of the list serves as an accumulator. At first I was mistakenly using ``foldl max 0`` with ``0`` as the initial accumulator but that would have failed for a list with entirely negative degree polynomials. With ``foldl1`` we are guaranteed to get the correct maximum value. ``foldl1`` applies ``max`` successively over the elements of ``xs`` and keeps track of the maximum value to date.



## P. *import* statements

**P.1**

* The exponentiation function ``^`` that normally raises a ``Num`` object by an ``Integer`` to produce an ``Integer`` i.e. it has signature: ``(^) :: (Integral b, Num a) => a -> b -> a``.
* We want to use ``^`` to implement exponentiation of ``Expr`` objects by integers.
* The problem is that ``^`` is implemented in the ``Prelude`` and not in any type-class. Thus we can't make ``Expr`` and instance of some type-class to override ``^``.
* Therefore to override ``^`` we must suppress the definition in the ``Prelude`` to which end we import ``Prelude`` while hiding the function ``(^)``.
* With this in place we can now define ``^`` ourselves.

**P.2** - We make a qualified import of Prelude which will allow us to access it by name and access its members using the ``.`` terminology. It will allow us to refer to the hidden/suppressed ``^`` as ``Prelude.^``.



## Q. Exponentiation using ^

**Q.1** - Since we have hidden/suppressed ``Prelude.^`` we can give our construction of ``^`` any signature we want. In this case we keep it in line with the ``Exp`` value constructor since that is what we want the output of the function to be. So ``^`` takes an ``Expr`` and an ``Integral`` and returns an ``Expr``.

**Q.2** - We use the ``exp_`` utility function to construct the exponent. This allows us to use pattern-matching to carry out the proper construction. 



## R. Multiplying expressions (using ``prod_``)

**R.1**

* ``prod_`` is a utility function for multiplying two expressions. It is analogous to ``exp_``. It in turn calls ``prod``` which implements the meat of the multiplication functionality.
* The purpose of ``prod_`` is to implement the more abstract rules of multiplication before ``prod'`` gets in to the specifics. Since it has to call ``proc'`` eventually we can implement commutation here and so any rules defined here must have their symmetric counter parts defined explicitly here as well.

**R.2**

* When two exponents are multiplied, if they have the same base we get an exponent with their powers added. If the bases are unequal we simply hand off computation to ``prod'``.
* Note that because equality of the bases of the exponents has been checked here ``prod'`` doesn't need to worry about it.
* This means that for all the expressions that can be inside an ``Exp`` (all the value constructors) we don't need patterns to match for equality of the base with the expression. By being abstract here, at the cost of explicit commutation patterns, we save on a larger number of pattern-matches later.

**R.3** - Checks if an exponent is being multiplied by an expression which is equal to its base.

**R.4** - Implements the commutation of (R.3) by switching the arguments and calling ``prod_`` recursively. Saves us from having to implement the same logic all over again.

**R.5**

* This rule really shows the power of Haskell, in particular recursion.
* When multiplying two ``Prod``s together simply treat the first one as a list of expressions and multiply them successively to the second ``Prod`` building up the solution recursively.
* This lets all of the above-defined rules do all the heavy-lifting and ensures that all of the necessary rules are met.
* The first pattern matches the base case of the list of expressions containing a single element.
* By construction a ``Prod`` can never be empty. Even a singleton ``Prod`` is nonsensical but we take it to be the same as the element itself.
* The second pattern extracts the first element from the first ``Prod``, multiplies it with the second product ``p2`` using ``prod_``.
* We then multiply this new ``Prod`` with a ``Prod`` consisting of the remaining list of elements ``es`` by calling ``prod_`` recursively.

**R.6**

* This pattern matches for an arbitrary expression multiplied by a product.
* It implements the two general rules obeyed by multiplication.
* One, if the expression exists inside the product the result is the product with that expression squared.
* Two, if there exists an exponent inside the product which has the expression as a base then the result is the product with the exponent with an incremented power.

**R.6a**

* The result of ``match a ps`` is of type ``Maybe [Expr a]``.
* When an element inside ``ps`` matches the expression ``a`` or an exponent of ``a`` ``match`` returns the new product list wrapped inside a Maybe.
* If no match occurs it returns ``Nothing`` to indicate that the calculation failed.
* Inside ``branch`` we use pattern-matching to deal with these two possibilities.
* If the result is ``Nothing`` we simply pass the element and ``Prod`` to ``prod'`` to carry out the multiplication.
* If the result is an ``[Expr a]`` wrapped inside a ``Just`` we use pattern-matching to extract the list and put it inside ``Prod``.

**R.6b** - This is the base case of the recursion. It indicates that no matching element (expression itself or its exponent) was found so the calculation is a failure and we return ``Nothing`` to indicate this.

**R.6c**

* We use pattern-matching to extract the ``Exp`` element from the head of the list.
* If the base of the exponent matches the expression ``c`` we replace the exponent with one with incremented power at the head of the list.
* We wrap this updated product list inside a ``Just`` and return it.
* If there is not match we use recursion. We want the current element ``ea`` at the head followed by the recursive result of ``match`` applied to the rest of the list ``es``. This is not straight-forward because the result of ``match`` is either a ``Just`` or a ``Nothing``.
* We are in classic **functor** territory.
* If the result of the recursive ``match`` is ``Nothing`` it means the recursion has failed and so the result from this append should also be ``Nothing``
* If the result of the recursive ``match`` is successful it will contain a list of expressions *inside* a ``Just``. So we have to concatenate ``ea`` with a list that is inside a context (``Just``).
* This begs for the use of ``fmap``. We construct a concatenation function by currying ``:`` by making it ``(ea:)``. Normally applying ``(ea:)`` to a list of expressions will add ``ea`` at the head of the list. Using ``fmap`` we apply this function to the content of the ``Maybe``.
* So ``fmap (ea:) Just [...]`` will add ``ea`` to the head of the list inside the ``Just``
* Additionally, since ``Maybe`` is a functor, ``fmap`` is aware of its context and so by construction ``fmap`` of any function over a ``Nothing`` gives a ``Nothing``.
* Both are requirements are met.

**R.6d** - The match for the expression is more general than the exponent so it has be placed lower in the list. This proceeds in complete analogy to (R.1k) 

**R.7** - Implements the abstract rule that when the same expression is multiplied by itself it is equivalent to raising the expression by the power 2. We use the ``exp_`` method to achieve this which in turn implements its own set of rules for constructing exponents, thereby guaranteeing proper construction.




**R.2a** 

* Our aim is to ensure that all the components of ``Prod`` (all the elements inside the ``Prod``) are **always** positive.
* To represent an expression which consists of the product of simple terms (``Const`` and ``Symbol``) which is negative overall we encapsulate the ``Prod`` inside a ``Neg`` which is the outermost expression. There will be no ``Neg`` *inside* a ``Prod``.
* To that end we define the behaviour of ``Neg`` expressions in multiplication first keeping the above requirement/assumption in mind.
* The product of two ``Neg`` expressions is simply the positive product of the content of the ``Neg``s.
* Multiplying a negative expression with a positive expression gives an overall negative product.
* Note how this will automatically deal with the possibility of the multiplication of ``Const`` and ``Symbol`` objects.
* Note that with the ``Neg`` conditions implemented at the top none of the following patterns will concern themselves with ``Neg``.
* Note also that the case of ``prod' a (Neg b)`` is taken care of the commutativity relation implemented by the very last pattern (at the bottom).

**R.2b**

* We implement the most basic identities for multiplication.
* Multiplying **anything** by zero results in zero. Note the use of ``_`` to represent an arbitrary expression we do not wish to use in the following definition.
* Multiplying **anything** by one results in the same thing being returned. We use pattern matching to name the matched expression ``e`` and simply return it as is.

**R.3** -  This pattern matches two positive constants being multiplied and the result is a ``Const`` and not a ``Prod``. This ensures that ``Const`` multiplication is automatically simplified.



**R.5**

* Note the use of both as-patterns and pattern-matching within the ``Symbol`` construct to gain access to both the ``Symbol``s as a whole and their contents. 
* We use ``a`` and ``b``, the ``String``s inside the ``Symbol``s to determine whether they match or not and if not the order between them.
* We use guards to look at the various scenarios.
* We explicitly implement the ordering of symbols inside the product.
* Note that by accessing the ``String`` inside each ``Symbol`` we carry out our comparison not on the ``Symbol`` as a whole but on its contents. Thus NO reference is made to the ``compare`` function defined when ``Expr`` was made an instance of the ``Ord`` class.
* We did not explicitly mention the scenario ``a == b`` since that is taken care of in ``prod_``



**R.7**

* Deals with the explicit case of a ``Symbol`` being multiplied by an ``Exp`` whose base is a ``Symbol`` and NOT a general ``Expr``.
* We have left the ``a == b`` case where the base is equal to the ``Symbol`` being multiplied undefined since we want to use the future implementation of ``exp'`` here.
* For the other cases we implement the expected order of variables (alphabetic).

**R.8** - This is a place-holder of sorts which deals with the possibility that a general expression can form the base of an exponent, e.g. ``x * (y + 2)^3``. In this case we have implemented no ordering other than the ``Symbol`` always appearing before the ``Exp``.

**R.9**

* This deals with the multiplication of two exponents both of which have pure ``Symbol``s as their base.
* When the symbols are different we use their lexical order to order the exponents in the Product

**R.10a**

* We implement ordering for when an exp with a symbol base is multiplied with a general exp.
* We have to provide the reversed pattern explicitly because underneath it is a catch-all pattern which will not allow the reverse to go the commutation pattern at the very end.
* Catch-all pattern for two exponents being multiplied with each other. No ordering has been implemented so far.

**R.10b** - We specify a sorting order here where a simple exponent of a single symbol raised to a power precedes a ``Sum`` while a non-symbol (general) expression raised to a power comes after a ``Sum``. The ordering is rather arbitrary.

**R.10c** - The possibility of the sum in the exponent being the same as the sum being multiplied with is handled earlier in ``prod_`` so is ignored here.


**R.15** - Analogous to (R.11) we use ``where`` to specify a function with pattern-matching to handle the insertion of a ``Symbol`` in to an existing ``Product``.

**R.16** - Any symbol must be inserted after the ``Const`` part. We place the ``Const`` in the front and then use recursion to insert the ``Symbol`` in the remaining list of expressions.

**R.17** 

* An excellent example of mixing guards and pattern-matching.
* We use pattern-matching to identify the scenario where the first element in the list is ``Symbol``. Then we use guards to compare the two Symbols, the one passed in and the one at the head of the list.
* The first guard deals with the possibility of the incoming ``Symbol`` String being lexically smaller than the head ``Symbol`` String. In this case we simply place the incoming (smaller) ``Symbol`` first, followed by the head ``Symbol``, followed by the rest of the list and the recursion terminates.
* Note that we tested ``b < c`` first and left ``b == c`` for later. This is based on the assumption that with a number of ``Symbol``s in an expression, equality is the least likely outcome so we use more likely matches first to make the algorithm faster.
* When ``b > c`` it means the incoming ``Symbol`` is lexically higher than the current symbol and so we place ``sc`` first and then use recursion to insert ``sa`` in the remaining list.
* When ``b == c`` we replace ``sc`` with an exponent power 2 to represent the multiplication of the two symbols.

**R.18** - Analogous to (R.17) but with a focus on multiplying symbols with possibly their own exponents.

**R.19**

* Catch-all pattern. If none of the patterns above match this catch-all assumes that the list of expressions is bizarre enough to warrant simply plopping the symbol ahead of it.
* Note that this pattern will also match the scenario where ``es`` is ``[]`` (empty, possible in a recursion scenario) which correponds to the symbol ending up at the end of the list.

**R.20**

* Rules for multiplying an exponent of a symbol with a general product.
* This is completely analogous to the rules for multiplying a symbol with a general product.
* Note how we pass in the String inside the ``Symbol`` and the associated power from the exponent to the ``mult_exp`` function defined using ``where`` syntax.

w ``Prod`` just created. The recursion will ensure that the result is built up by multiplying one element at a time till they are all exhausted.

**R.22**

* This is the piece de resistance. We first have to guarantee that we explicitly exhaust all possible combinations of constructors without ever writing a pattern which has the arguments (constructors) switched.
* Since multiplication is commutative we define the last catch-all pattern to declare that the ``prod'`` function is commutative which corresponds to calling ``prod'`` recursively with the arguments switched.
* So if we call ``prod' (Symbol _) (Const _)`` it matches none of the patterns since only ``prod' (Const _) (Symbol _)`` is defined. So it falls through to the last pattern and is set equal to the latter and is sent on its way to match the pattern with the arguments reversed. Thus we don't have to write the inverted pattern expression for all asymmetric patterns.

**R.23** - We multiply the two ``Rec`` together and then multiply it with the rest of the elements using ``multiply`` which builds the product back up piece by piece using recursion.

**R.24** - By construction the **single** ``Rec`` expression is always the first element of the ``Prod`` (if it exists).

**R.25** - The possibility of the two sums being equal is handled earlier by ``prod_``.

**R.26** - We use pattern-matching to handle the possibility that the sum exists in the product already and therefore needs to be handled using exponentiation.



## S. Exponentiation

**S.1**

* We define an intermediary function between ``(^)`` and ``exp'``.
* Its purpose it to deal with the possibility of negative powers in an exponent without falling in to an infinite recursion.
* When the power is negative we place the exponent inside a ``Rec`` and send the absolute value of ``p`` to the ``exp'`` call.
* When the power is zero we return ``1``. This will in all likelihood shadow the rule ``exp` _ 0 = Const 1`` but we leave the latter in place for the sake of completeness.

**S.2**

* The first pattern we match is for a negative base.
* We extract the expression within the ``Neg``.
* If the power is even we realise that the negative part is turned to a positive ``(-1)^(2n) = 1`` so we simply return the expression inside the ``Neg`` exponentiated using a recursive call.
* If the power is odd the result must be negative overall because ``(-1)^(2n+1) = -1`` and so we surround the recursive exponent with a final ``Neg``.

**S.3** - The exponent of an inverse expression is the inverse of the exponent of the expression.

**S.4**

* Note the use of ``Prelude.^`` to refer to the ``^`` inside ``Prelude`` which we hid when we imported ``Prelude``. Here is where the qualified import of ``Prelude`` comes in handy.
* We overrode ``^`` to only work with the first argument being ``Expr a`` while here we want to raise in Int to an Int power for which we must use the function defined inside ``Prelude``.

**S.5** - When our patterns are exhausted we simply exponent the expression (this can be ``Symbol``, ``Sum`` or ``Prod``).



## T. Multiplication using ``prod'``

**T.1**

* ``prod'`` is a utility function for multiplying two expressions. It is called by ``prod_``. It is completely analogous to ``sum'`` with the same kind of pattern-matching implemented.
* We use pattern matching to define the common multiplication scenarios. This will save us time because we won't have to simplify terms afterwards (and repeatedly).
* ``prod'`` uses pattern matching to implement the rules for multiplying any expression with any other expression.
* Multiplication is commutative ``A * B = B * A``. This is implemented by the catch-all pattern at the very bottom ``prod' a b = prod' b a``.
* This means all possible combinations of expressions (value constructors) must be explicitly stated.
* For very combination the symmetric reversed combination is covered by commutation and need not be specified.
* We start with ``Const`` and specify rules for multiplying it with each kind of value constructor (including ``Const`` itself).
* This means that when we move on to the rules for ``Symbol`` we won't need to deal with the case of ``Symbol * Const`` because that will be covered here.
* As we work our way through all the constructors we will be left with less and less rules to write for each.
* Every value constructor must have a rule for multiplying with its own data type.
* Note how we use exhaustive rules for ``Neg`` in ``prod_`` to take care of all of them in a simple fashion.


*Multiplying a ``Const`` with another expressions*

**T.2a**

* Note that the ``Prod`` is constructed with the ``Const`` first and the ``Symbol`` afterwards. This saves on simplification.
* The case of ``prod' (Symbol _) (Const _)`` is taken care of by the commutation implemented in the last pattern which sends that case to this pattern by switching the arguments around and once again ensuring that the ``Const`` comes first in the function arguments.

**T.2b** - This is based on the assumption that no exponent will ever have a ``Const`` in its base because such exponents will already have been converted in to the resulting ``Const`` values.

**T.2c** - ``Rec`` is **always** the first element in a ``Prod`` (when it is present)


*Multiplying a ``Const`` with a ``Prod``

**T.3**

* This entire cluster of patterns is based on the assumption that every ``Prod`` has the same construction format: ``[<Rec>, <Const>, ...]`` where the first element is optionally a ``Rec`` (the ONLY one in the ``Prod`` - they are all collected in to one). The next element is a ``Const`` (if there is no ``Rec`` element this will be the first, otherwise it will be the second element). ``Const`` is also optional.
* Note that the case of the product and/or the constant being negative is handled by the rules defined for abstract ``Neg`` objects being multiplied which use recursion.

**T.3a** - This pattern corresponds to the first element (of the list of elements inside the ``Prod``) being a ``Const``. We multiply the constant values together and append it to the rest of the list of expressions ``es`` which we got by pattern-matching.

**T.3b**

* If the first element is a ``Rec`` we reorder the multiplication by multiplying ``c`` with the rest of the elements ``es`` first and then multiplying the ``Rec`` to the result. This uses all of the carefully constructed rules for multiplication and ensures that the ``Rec`` appears first in the list.
* Note how we used as-patterns to keep a handle on the ``Const`` and ``Rec`` objects which are used on the RHS.
* Note how we used ``_`` inside the ``Const`` and ``Rec`` patterns because the rule we are defining doesn't require knowledge of these values.

**T.3c** - If the first element of the list is neither a ``Const`` nor a ``Rec`` we simply append the ``Const`` in front of the entire list.
 
 
 

## Debugging

### ``trace``

Source: *https://wiki.haskell.org/Debugging*

``trace`` is an extremely useful function for debugging Haskell code and is invaluable during development. It should however never be left in production code.

The signature of ``trace`` is ``String -> a -> a``. You pass it a ``String`` which is usually a debugging message (sort of a ``printf``) and the actual value you want the function to return and ``trace`` will print the message and then return the value.

Basically it allows us to print a message (as a side-effect) without altering the actual purpose of our code. Invaluable.

To use ``trace`` one must import it as follows: ``import Debug.Trace(trace)`` which only imports this one function from the ``Debug.Trace`` module.

A point to note is that any function that uses ``trace`` will probably need access to the ``Show`` type-class so ``Show a`` may have to be added as a type constraint to the calling function.