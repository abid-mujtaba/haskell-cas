Copyright 2015 Abid Hasan Mujtaba

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.


# Comments

In this file we have placed the comments that have been made regarding the Haskell code in the CAS.hs file. The comments are very detailed and were taking too much space in the code file so they have been moved here. They have been given unique labels so they can be searched very quickly.

At the bottom of the file are general comments about the development process and debugging.


## A. Module declaration

**A.1**

* We start by declaring this .hs file to be module. The module must have the same name as the file (CAS.hs).hs
* In the module identifier after the module's name and delimited in parentheses we declare the classes, types and functions we want to export from the module.
* The parentheses are followed by the keyword where and then the rest of the file is dedicated to defining the various objects that are being exported.

**A.2** - ``Expr(Symbol)`` signifies that ONLY its ``Symbol`` value constructor is to be exported. All of the other constructors are hidden. Our intention is to allow a user to build up complex expressions by starting with ``Symbol``s and ``Const`` values as building blocks and tying them together with arithmetic operations without having to explicitly use the rest of the value constructors. To access ``Const`` values we use the ``const'`` function described in *A.3*

**A.4** - We export the ``^`` infix function which we have defined in the module (overriding the Prelude definition of the same)



## B. Expr type

**B.1** - We define the new data typeclass 'Expr' which corresponds to a general algebraic expression. This is achieved by using recursion in the definition of the constructors. By defining these carefully one can use pattern-matching to implement various functions for the 'Expr' typeclass.

**B.2** - We declare 'Expr a' to be a type class where 'a' can be any concrete type

**B.3** - A constant (basically a number) can be an expression by itself (the most basic kind there is)

**B.4** - This is a recursive constructor with the Constructor name 'Sum' and which takes a list of 'Expr a' objects as its constructor parameters

**B.5** - A fraction. Consists of a numerator and a denominator.

**B.6** - Expression raised to an INTEGER power.

**B.7** - The algebraic variables in the expression: x, y, z, .etc

**B.8** - We declare Expr to be an instance of the Eq class since we will want to compare expressions for equality



## C. Expr instance of Show

**C.1**

* We declare 'Expr' to be an instance of the 'Show' typeclass. Since 'Expr a' is a typeclass with a type-parameter 'a' we declare that in our declaration of the instance we limit ourselves to types of class 'Show' i.e. this instance only refers to types 'Expr a' where 'a' itself is an instance of the Show class. This is the '(Show a) =>' part.
* Then comes the actual instance declaration 'Show (Expr a)'. We need the parentheses when a type-parameter is included.
* Lastly comes the 'where' keyword and after it on the following lines comes the functions that must be defined for a type to be an instance of the class. In the case of the 'Show' typeclass this only one function 'show'

**C.2** -  We pattern match on the 'Const a' constructor. In the case of constant we simply show the number. The 'a' in this line is NOT the same as the 'a' in the instance declaration line above it. Here 'a' matches the value inside the 'Const a' constructor. Since the instance declaration limits 'Expr a' to type-parameters 'a' that are an instance of 'Show' so we can run the 'show' method directly on the value 'a' inside the 'Const a' parameter

**C.4** - The negation of an expression is simply appending '-' in front of it. We use the concatenation operator ':' to prepend '-' in front of the String representation of the expression

**C.5** - Since 's' is a String (from the definition of the 'Symbol' constructor) we don't need to use 'show' here. Had we done so it would have printed the strings surrounded by quotation marks

**C.6**

* This is a utility function (NOT exported) for showing a list of expressions. The separator (+ or *) is specified.
* The empty list should not occur but if it does we simply print (0)
* For a non-empty list we print the surrounding parentheses and use another related utility function to print the meat of the expression.

**C.7** - In this utility function for a single element we simply print the expression within. For a larger list we use a head:tail pattern-match to extract the head show it, add the separator and then use recursion on the rest.

**C.8** - We analyze the elements of the list, in particular the first two elements. If the second element is negative we print a minus "-" and then recursively call showSum with the second element changed from negative to positive so that when it is printed another negative sign isn't printed.



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
* Note the use of the ``const'`` smart constructor which is able to detect a negative integer and convert it in to a ``Neg (Const _)``.



## E. Expr instance of Fractional

**E.1**

* The instance declaration has a minor subtlety in the type-constraint 'Integral a'. Usually in such an overloading scenario the type-constraint would be (Fractional a). Note in the definition of the type Expr that the only constructor that (without recursion) uses the type-parameter 'a' is 'Const a'. For our application we are ONLY interested in expressions that contain integer constants so we restrict the type parameter 'a' here to 'Integral a'.
* If a non-integer constant is found see (E.3)

**E.2** - To construct the reciprocal part we use the ``frac`` function instead of ``Frac`` because the former is aware of how to properly construct fractions. It automatically simplifies the expression as we construct it.

**E.3** - We define the fromRational method to be an 'error' so that when this method is called the specified error message is printed. This ensures that the constants in our expressions are only allowed to be integers. We will deal with rational fractions by using Prod and Rec.



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

**L.1**

* We only need to define the ``compare`` function while making Expr an instance of the Ord class. Since it takes two objects, compares them and generates the resulting ``Ordering`` object (with values of ``LT``, ``EQ`` or ``GT``) we get the remaining comparison operators (``<``, ``<=``, ``==``, ``>``, ``>=``) for free. 
* In addition the Data.List.sort function will also work automatically once ``Expr`` is made an instance of the ``Ord`` type-class.
* In this instantiation we are basically implementing the lexical order of expressions especially as used to place them inside a ``Sum``.
* The lexical order is defined to be "Graded/Degree reverse lexicographic order"
* This ``compare`` function starts by comparing the degree of expressions and if those match hands over to compare'

**L.3**

* We define the comparison of two ``Neg`` expressions to have the same ordering as the expressions itself.
* When only one expression is negative then two possibilities exist.
* If the expression inside the negative is the same as the other expression then the positive expression is greater.
* If the absolute value of the two expressions are not equal then we return the ordering  of the absolute value of the two.

**L.4**

* Catch-all pattern for comparing expressions which are NOT both exponents.
* We compare the degree of both expressions to calculate an order.
* Note that when the degrees are equal we pass on the arguments to the ``compare'`` function.

**L.5**

- We are comparing two Frac with equal degree.
- First we compare the numerators. If it is not EQ we return the result.
- Otherwise we compare the denominators.
- Note the use of ``mappend`` to perform this lazily and only compare the denominators if the comparision for the numerators turns out to be equal.
- We want a Frac to have higher precedence (is written first) whenever it is compared with a non-Frac expression.
 
**L.6** - We pattern-matched to look at the scenario where we are comparing two ``Symbol`` objects. We want ``x`` to appear before ``y`` so ``compare' x y`` should be equal to ``GT`` as required by reverse lexical order. To achieve that we must reverse the alphabetic ordering of the ``String`` inside the ``Symbol``. 

**L.7**

* In compare' we are handling exponents whose total degree matches (inner degree times exponent). We use pattern-matching to deal with several possibilities.
* If the power of the two exponents match as well then the lexical order is given by the lexical order between the bases of the two exponents.
* If the power doesn't much then in the system we are defining here we want the expression with the higher power (lower degree in the base) to come first which is why when ``pa < pb`` we return ``LT``.

**L.8**

* Now that we have compared an exponent with another exponent we deal with the possibility of an exponent being compared with a non-exponent expression with the same degree (so a Sum or Prod).
* In such a case an exponent has higher order according to the rule defined in ``L.7``. We want a pure exponent to appear first inside a sum followed by a more complex expression with equal degree.

**L.9** - When comparing two ``Sum``s we perform a comparison of the list of expressions encapsulated.

**L.10** - This is the base case. If both lists are exhausted simultaneously it means the lists must be equal and so the ``Sum``s must be equal.

**L.11** - The first list whose arguments are exhausted takes higher precedence.

**L.12**

* The recursion occurs by comparing the heads of both lists. If the comparison is ``EQ`` we move on to the remaining part (tail) of both lists. If the comparison is not ``EQ`` then we simply return the value.
* We use ``mappend`` from ``Data.Monoid`` which treats ``Ordering`` as a monoid. ``mappend`` is defined such that if the first argument is ``LT`` or ``GT`` then that is the result of ``mappend`` **regardless** of the value of the second argument.
* If the first argument is ``EQ`` then the result is the value of the second argument. This is exactly what we want since we place the recursive call in the second argument so that it is only called if the first argument is ``EQ``.

**L.13** - Compare a ``Sum`` with a general expression (non-``Sum`` since that match occurs earlier). The comparison is carried out analogous to the two ``Sum`` comparison but with only the single other expression compared successively with elements of the list inside the ``Sum``.

**L.14** - Comparison is an inherently anti-symmetric operation. We use the ``flipCompare`` method to flip the resulting ``Ordering`` after you have flipped the arguments.

**L.15** - We are implementing graded reverse lexicographic order in which when comparing monomials one starts looking at the components from right to left, hence we reverse both lists before passing them to the ``cList`` function.

**L.16**

* There are three base cases for the recursive function ``cList``.
* If both lists are empty that means all corresponding elements of both lists gave ``EQ`` result in comparison (that is they were equal) in which case the two lists must be equal and so the result must clearly by ``EQ``. **Alternately** from a theoretical point where we don't need to concern ourselves with how we get there, in lexical order the comparison of two empty lists should always result in ``EQ``.
* If the first list becomes empty while the second one is not it means that the second list has an entry which means that the first list is lexically lower and vice versa.

**L.17**

* For the case of two non-empty lists we compare the head of both lists using `cmp`.
* If the result of the comparison of heads is unequal to `EQ` than the rules that goves the monoid `mappend` means that the second argument of `mappend` is NOT evaluated and the first argument is returned as the result.
* If `cmp` evalautes to `EQ` then the second argument is evaluated which is simply a recursive call to cList.

**L.18** - The `cmp` function is defined to compare elements which are inside a product. These have slightly different rules than for sums and this is why we need a dedicated function for this. Note that this uses ``compare`` recursively.

**L.19**

* We compare two exponents with the same degree.
* The first step is to compare the outer degrees of the exponents, since the base of each could have a degree higher than one. Analogous to total degree the element with lower outer degree is lexically lower.
* If the outer degrees of the two expressions is equals, and since we are in compare' whether the total degree is equal it means that the inner degrees are also equal.
* Consequently we simply pass both inner expressions to compare' to get the comparison result.


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

**O.3** - QuickCheck tests revealed that degree was being called with an empty sum causing an exception. We explicitly set the degree of an empty sum to be zero to avoid the exception. 

**O.4** 

* The degree of the sum of polynomials is the highest (maximum) of the degree of its parts. To calculate this we ``map`` ``degree`` over ``xs`` and then ``foldl1'`` the ``max`` function over it.
* ``max`` is a binary function that returns the larger of its two arguments.
* We want to implement ``max`` over the whole list so we naturally use a fold. In this case ``fold11`` because with it the first element of the list serves as an accumulator. At first I was mistakenly using ``foldl max 0`` with ``0`` as the initial accumulator but that would have failed for a list with entirely negative degree polynomials. With ``foldl1`` we are guaranteed to get the correct maximum value. ``foldl1'`` applies ``max`` successively over the elements of ``xs`` and keeps track of the maximum value to date.



## P. *import* statements

**P.1**

* The exponentiation function ``^`` that normally raises a ``Num`` object by an ``Integer`` to produce an ``Integer`` i.e. it has signature: ``(^) :: (Integral b, Num a) => a -> b -> a``.
* We want to use ``^`` to implement exponentiation of ``Expr`` objects by integers.
* The problem is that ``^`` is implemented in the ``Prelude`` and not in any type-class. Thus we can't make ``Expr`` and instance of some type-class to override ``^``.
* Therefore to override ``^`` we must suppress the definition in the ``Prelude`` to which end we import ``Prelude`` while hiding the function ``(^)``.
* With this in place we can now define ``^`` ourselves.

**P.2** - We make a qualified import of Prelude which will allow us to access it by name and access its members using the ``.`` terminology. It will allow us to refer to the hidden/suppressed ``^`` as ``Prelude.^``.

**P.3** - We import the ``mappend`` function so that we can combine ``Ordering`` (``LT``, ``GT`` and ``EQ``) in a specific fashion.



## Q. Exponentiation using ^

**Q.1** - Since we have hidden/suppressed ``Prelude.^`` we can give our construction of ``^`` any signature we want. In this case we keep it in line with the ``Exp`` value constructor since that is what we want the output of the function to be. So ``^`` takes an ``Expr`` and an ``Integral`` and returns an ``Expr``.

**Q.2** - We use the ``exp_`` utility function to construct the exponent. This allows us to use pattern-matching to carry out the proper construction. 



## R. Multiplying expressions (using ``prod_``)

**R.1a**

* ``prod_`` is a utility function for multiplying two expressions. It is analogous to ``exp_``. It in turn calls ``prod``` which implements the meat of the multiplication functionality.
* The purpose of ``prod_`` is to implement the more abstract rules of multiplication before ``prod'`` gets in to the specifics. Since it has to call ``proc'`` eventually we can implement commutation here and so any rules defined here must have their symmetric counter parts defined explicitly here as well.

**R.1b** 

* Our aim is to ensure that all the components of ``Prod`` (all the elements inside the ``Prod``) are **always** positive.
* To represent an expression which consists of the product of simple terms (``Const`` and ``Symbol``) which is negative overall we encapsulate the ``Prod`` inside a ``Neg`` which is the outermost expression. There will be no ``Neg`` *inside* a ``Prod``.
* To that end we define the behaviour of ``Neg`` expressions in multiplication first keeping the above requirement/assumption in mind.
* The product of two ``Neg`` expressions is simply the positive product of the content of the ``Neg``s.
* Multiplying a negative expression with a positive expression gives an overall negative product.
* Note how this will automatically deal with the possibility of the multiplication of ``Const`` and ``Symbol`` objects.
* Note that with the ``Neg`` conditions implemented at the top none of the following patterns will concern themselves with ``Neg``.

**R.1c**

* We implement the most basic identities for multiplication.
* Multiplying **anything** by zero results in zero. Note the use of ``_`` to represent an arbitrary expression we do not wish to use in the following definition.
* Multiplying **anything** by one results in the same thing being returned. We use pattern matching to name the matched expression ``e`` and simply return it as is.

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

**R.7** - Implements commutation for a ``Prod`` being multiplied with an arbitrary expression. We simply switch the arguments around because the patterns look for an arbitrary expression multiplied with a ``Prod``.

**R.8** - Implements the abstract rule that when the same expression is multiplied by itself it is equivalent to raising the expression by the power 2. We use the ``exp_`` method to achieve this which in turn implements its own set of rules for constructing exponents, thereby guaranteeing proper construction.



## S. Exponentiation

**S.1**

* We define an intermediary function between ``(^)`` and ``exp'``.
* Its purpose it to deal with the possibility of negative powers in an exponent without falling in to an infinite recursion.
* When the power is negative we invert the exponent by creating a ``Frac`` with ``Const 1`` in the numerator and place the exponent in the denominator but with a positive exponent by sending the absolute value of ``p`` to the ``exp'`` call.
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

**T.2**

* We do a pattern match for a ``Const`` being multiplied by any other expression.
* If the pattern matches we pass the arguments to the ``prod_c`` utility function which is dedicated only to the multiplication of ``Const`` with other expressions.
* This has two advantages. One, it makes the code in ``prod'`` more concise because we don't place all the ``Const`` patterns here.
* Two, the code becomes more efficient because if the first expression in ``prod'`` is NOT a ``Const`` then it fails this one pattern here and moves on, without having to fail against every pattern that is described in ``prod_c``. Thus branching is implemented within the patterns.
 
 
 
## U. Multiplying by ``Const``
 
**U.1a** - This is a utility function for multiplying a ``Const`` expression with other expressions.

**U.1b** -  This pattern matches two positive constants being multiplied and the result is a ``Const`` and not a ``Prod``. This ensures that ``Const`` multiplication is automatically simplified.
 
**U.2**

* Note that the ``Prod`` is constructed with the ``Const`` first and the ``Symbol`` afterwards. This saves on simplification.
* The case of ``prod' (Symbol _) (Const _)`` is taken care of by the commutation implemented in the last pattern which sends that case to this pattern by switching the arguments around and once again ensuring that the ``Const`` comes first in the function arguments.

**U.3** - This is based on the assumption that no exponent will ever have a ``Const`` in its base because such exponents will already have been converted in to the resulting ``Const`` values.

**U.4** - ``Rec`` is **always** the first element in a ``Prod`` (when it is present)


*Multiplying a ``Const`` with a ``Prod``

**U.5**

* This pattern is based on the assumption that every ``Prod`` has the same construction format: ``[<Rec>, <Const>, ...]`` where the first element is optionally a ``Rec`` (the ONLY one in the ``Prod`` - they are all collected in to one). The next element is a ``Const`` (if there is no ``Rec`` element this will be the first, otherwise it will be the second element). ``Const`` is also optional.
* Note that the case of the product and/or the constant being negative is handled by the rules defined for abstract ``Neg`` objects being multiplied which use recursion.
* Instead of the where-pattern we could have gone with multiple patterns but they would have looked cumbersome. This way not only is the logic cleaner but the whole category of ``Const * Prod`` can be rejected in one pattern test rather than having multiple pattern tests. 

**U.5a** - This pattern corresponds to the first element (of the list of elements inside the ``Prod``) being a ``Const``. We multiply the constant values together and append it to the rest of the list of expressions ``es`` which we got by pattern-matching.

**U.5b**

* If the first element is a ``Rec`` we place it at the head of the list. The rest of the list is created by recursively calling ``mul`` on the value of the ``Const`` i.e. ``a`` and ``es`` the rest of the list inside the ``Prod`` (after the ``Rec`` has been removed).
* Note how we used an as-pattern to keep a handle on the ``Rec`` objects which are used on the RHS.
* Note how we used ``_`` inside the ``Rec`` pattern because the rule we are defining doesn't require knowledge of its internals (for now).

**U.5c** - If the first element of the list is neither a ``Const`` nor a ``Rec`` we simply append the ``Const`` in front of the entire list.

**U.6** - ``prod_c`` is **only** intended for ``Const`` patterns. So the catch-all pattern at the bottom raises an error which will inform us if ``prod_c`` is every used incorrectly.
 
 
 
## V. Multiplying by ``Symbol``

**V.1**

* By performing ``Symbol`` pattern matching in another function altogether all calls where ``Symbol`` comes first, even if it is before ``Const``, is captured by the ``prod_s`` function and execution never falls to the catch-all pattern at the end of ``prod'`` which implements commutation.
* Therefore we must explicitly provide the rule for ``Symbol x Const``. We simply pass this on to the ``prod_c`` where the rule has already been defined. We are using commutation to simplify our task.

**V.2**

* Note the use of both as-patterns and pattern-matching within the ``Symbol`` construct to gain access to both the ``Symbol``s as a whole and their contents. 
* We use ``a`` and ``b``, the ``String``s inside the ``Symbol``s to determine whether they match or not and if not the order between them.
* We use guards to look at the various scenarios.
* We explicitly implement the ordering of symbols inside the product.
* Note that by accessing the ``String`` inside each ``Symbol`` we carry out our comparison not on the ``Symbol`` as a whole but on its contents. Thus NO reference is made to the ``compare`` function defined when ``Expr`` was made an instance of the ``Ord`` class.
* We did not explicitly mention the scenario ``a == b`` since that is taken care of in ``prod_`` 

**V.3**

* Deals with the explicit case of a ``Symbol`` being multiplied by an ``Exp`` whose base is a ``Symbol`` and NOT a general ``Expr``.
* We order the ``Symbol`` and the ``Exp (Symbol _)`` based on the lexical order of the two symbols. 

**V.4** - This pattern deals with the possibility that a general expression can form the base of an exponent, e.g. ``x * (y + 2)^3``. In this case we have implemented the ordering that the ``Symbol`` always appearing before the ``Exp``.

**V.5** - Analogous to (U.5) we use ``where`` to specify a function with pattern-matching to handle the insertion of a ``Symbol`` in to an existing ``Product``.

**V.6** - Any symbol must be inserted after the ``Const`` part. We place the ``Const`` in the front and then use recursion to insert the ``Symbol`` in the remaining list of expressions.

**V.7** 

* An excellent example of mixing guards and pattern-matching.
* We use pattern-matching to identify the scenario where the first element in the list is ``Symbol``. Then we use guards to compare the two Symbols, the one passed in and the one at the head of the list.
* The first guard deals with the possibility of the incoming ``Symbol`` String being lexically smaller than the head ``Symbol`` String. In this case we simply place the incoming (smaller) ``Symbol`` first, followed by the head ``Symbol``, followed by the rest of the list and the recursion terminates.
* Otherwise technically corresponds only to ``b > c`` it means the incoming ``Symbol`` is lexically higher than the current symbol and so we place ``sc`` first and then use recursion to insert ``sa`` in the remaining list.
* The ``b == c`` case is handled generally in ``prod_``.

**V.8** - Analogous to (V.6) but with a focus on multiplying symbols with possibly their own exponents.

**V.9**

* Catch-all pattern. If none of the patterns above match this catch-all assumes that the list of expressions is bizarre enough to warrant simply plopping the symbol ahead of it.
* Note that this pattern will also match the scenario where ``es`` is ``[]`` (empty, possible in a recursion scenario) which corresponds to the symbol ending up at the end of the list.



## W. Multiplying by ``Exp``


**W.1** - In analogy to (V.1) we provide the explicit rules for ``Exp`` multiplied by ``Const`` and ``Symbol`` by punting the calculation to the rules defined there. We have to explicit here because we moved all ``Exp`` matches to a separate function and so the pattern is matched even when we are multiplying by ``Const`` and ``Symbol``.

**W.2**

* This deals with the multiplication of two exponents both of which have pure ``Symbol``s as their base.
* ``prod_`` deals with the possibility of both symbols being the same.
* Since the symbols are guaranteed to be different here, we use their lexical order to order the exponents in the Product

**W.3**

* We implement ordering for when an exp with a symbol base is multiplied with a general exp.
* We have to provide the reversed pattern explicitly because both constructors are ``Exp``. There is no reason for this to go all the way down to the reversal pattern. It is caught by the patttern in ``prod_`` and assigned to ``prod_e`` where we have to deal with it.

**W.4** - We specify a sorting order here where a simple exponent of a single symbol raised to a power precedes a ``Sum`` while a non-symbol (general) expression raised to a power comes after a ``Sum``. The ordering is rather arbitrary.

**W.5** - The possibility of there being a sum in the exponent which is the same as the sum being multiplied with is handled earlier in ``prod_`` so is ignored here. For a general expression inside the ``Exp`` we place the ``Sum`` first. Once again the ordering is rather arbitrary.

**W.6**

* Rules for multiplying an exponent of a symbol with a general product.
* This is completely analogous to the rules for multiplying a symbol with a general product.
* Note how we pass in the String inside the ``Symbol`` and the associated power from the exponent to the ``mul`` function defined using ``where`` syntax.
* Note the catch-all pattern at the bottom. It declares that an exponent of a symbol has higher precedence than all other expressions except for the ones for which rules/patterns are defined above.

**W.7**

* Rules for multiplying an exponent with a general (non-symbol) base with a general product.
* Note that there is no catch-all at the bottom so we have a base case at the top dealing with the possibility of any empty Product list (due to recursion) in which case we simply return the exponent as a singleton list.
* We match for the head being exponent first so that it isn't hidden by the pattern that follows (which is more general - as a rule patterns move from specific to general).
* Note the *otherwise* case where the exponent is always sent to ``mul`` recursively when ``b != c``. This opens up the possibility of the list ``es`` becoming empty which is why we need the base case at the top.
* These patterns allow for general expressions inside the exponent to be multiplied with the same expression or an exponent of it in the Product. This is a desired simplification we want during expression construction (when we are multiplying expressions).



## X. Multiplying by ``Rec``

**X.1** - Explicit rules/patterns for implementing commutation with expressions whose rules have been defined already (above ``prod_r`` inside ``prod'``)

**X.2** - We multiply the two ``Rec`` together and then multiply it with the rest of the elements using ``mul`` which builds the product back up piece by piece using recursion.

**X.3** - By construction the **single** ``Rec`` expression is always the first element of the ``Prod`` (if it exists).



## Y. Multiplying by ``Sum``

**Y.1** - The order of two sum expressions inside a product is based on their lexical order given by ``compare``. The possibility of the two sums being equal is handled earlier by ``prod_``.

**Y.2** - The purpose of ``mul`` is to place the Sum ``sa`` inside the list of expressions that comprise the product.

**Y.3** - Recursive base case. If we arrive at the end of the list then this is where the Sum must be placed.

**Y.4** - The rules we are implementing require that a Sum be placed after all Symbols and Exponent of Symbols.

**Y.5**

* When comparing with a non-symbol(-exponent) element we find out whether the Sum is lexically "greater" than the element in question. This is done according to rules that are slightly different from that used for constructing Sums and so we use the ``cmp_prod`` function. If the Sum is "greater" than the element it appears first in the list.
* Equality is considered an error since that possibility should have been taken care off by an earlier pattern match.

**Y.6**

* The special rules for constructing Products require that (when not comparing with symbols or exponent there-of - handled earlier) if the degree of the Sum is lower than that of the second element it appears first and vice versa. This is the opposite of how ``compare`` (used in construction of Sums) behaves.
* If the degrees are equal we fall back to using ``compare``.


## Z. Adding expressions

**Z.1**

* A smart constructor from creating a ``Sum`` from a list of expressions. A never of utility function used for defining addition relations create a list of elements. These can end up being empty or singleton so we need a smart constructor to handle these possibilities.
* An empty list inside a ``Sum`` can be created when negative expressions cancel positive ones. In this case it is obvious that the result should be zero.
* Similarly cancellation can create lists with only a single expression in which case the sum should be equal to just that simple expression.

**Z.2** - Implement the basic rules of addition. Adding 0 to any expression leaves it unchanged. We also implement the commutation.

**Z.3**

* Since `Neg` is intrinsically bound up with the concept of addition we deal with some of the ``Neg`` relations up front.
* When one adds to expressions both inside ``Neg`` we add the inner expressions and place the result in a ``Neg`` context.
* For adding an expression with a ``Neg`` expression (the former is guaranteed to not be a ``Neg`` because of the pattern above) we implement commutation on the pattern.
* This means that all subsequent patterns **only** need to match with ``Neg`` in the **first** argument (the first one is handled using commutation here).

**Z.4**

* When adding something with a ``Neg`` ``Sum`` we first test if the expression withing the ``Neg`` is equal to the second expression in which case the answer should be zero.
* Otherwise we map ``Neg`` over all of the elements converting it in to a ``Sum`` over ``Neg`` elements before we carry out the addition.

**Z.5**

* The first pattern is the base case of recursion when we end up with a sum containing a singleton list. By the logic from ``sum_list`` this is equal to the element/expression itself. So we extract the expression from inside the singleton and pass it recursively to ``sum_``.
* When adding two ``Sum`` we first check for the possibility of the two sums being equal in which case we replace them by twice the expression.
* Otherwise we add one element at a time from the first list in to the second list.
* Note how we use recursion to add the remaining first list with the now extended second list until the first list becomes empty and the second list contains the result of the completed addition.

**Z.6**

* This is analogous to how we multiply an arbitrary expression with a ``Prod``.
* We are looking for the possibility that the expression or its negative exists in the ``Sum`` already in which case we have to cancel or combine the two together.
* We iterate over the list looking for a possible match and use a ``Maybe`` to deal with the possibilit of failure (i.e. no match is found).

**Z.7** - Commutation pattern corresponding to (Z.6).

**Z.8** - We test fot the possibility of a general expression being added to its own negative which should result in a zero.  

**Z.9** - When handling two expressions which are not sums we test for equality. If they are equal we replace them with twice their value otherwise we pass the arguments to the ``sum'`` utility function for further processing.

**Z.10** - When adding a ``Const`` or ``Neg (Const _)`` with another expression we simply pass the arguments to the utility function ``sum_c`` which is dedicated to the addition of (negative) ``Const`` with any general expression.

**Z.11** - With the special case of Const (because of compacting), Prod because of factoring and Sum (because the operation is addition itself) taken care of we can lump all the rest in to one function ``sum_x`` which uses the ``compare`` function to order the elements in the desired lexical order.



## AA. Adding ``Const`` using ``sum_c``

**AA.1**

* When adding a constant with a ``Sum`` our aim is to find the possible (negative) constant inside the sum and combine the two inside the sum.
* Note that we used a general pattern for the first argument and did not specify ``(Const _)``. This is because had we done so we would have had to define an exactly analogous pattern for ``Neg (Const`` which would be wasteful.
* The down-side to our desire to not have repetition in the patterns and functions is that we must ensure that this utility function ``sum_c`` is **only** called with a (negative) constant and nothing else i.e. it is only called when pattern-matching against these constructors. Since ``sum_c`` is only called from within ``sum'`` for just such a case this is guaranteed.

**AA.2** - Base case of the recursion. Note that if this singleton is the final result of ``add` then the function ``sum_list`` will unpack it in to the single expression inside.

**AA.3**

* If the constant is lexically higher than the head we simply place the constant in that location.
* If the constant is lexically lower than the head we place the head in the position and recursively call ``add`` again with the tail of the list since the constant may need to be located further down the list.
* When we have lexical equality it is guaranteed, from the implementation of the ``compare`` function, that the head element is also a (negative) constant (has degree zero). In this case we simply add the two elements to get a unified ``Const``.
* We make a recursive call to ``add`` with the newly created ``Const`` to deal with the possibility of the addition resulting in a ``Const 0``. The recursive call to ``add`` will remove the zero if that is the case.

**AA.4** - If the second element is neither a constant nor a sum we simply create a ``Sum`` from just the second argument (singleton) and then recursively call ``sum_c`` on it. This works because ``sum_c`` with ``Sum`` as the second argument implements all of the rules for constructing a ``Sum`` with the correct lexical order.



## AB. Adding non-``Const/Sum/Prod`` expressions using ``sum_x``

**AB.1** - If the second argument is a ``Const`` we simply pass the arguments to ``sum_c``.

**AB.2**

* This is becoming a common pattern when dealing with adding expressions to ``Sum`` or multiplying expressions to ``Prod``.
* The first pattern is the base case for recursion.
* In the second pattern we extract the head of the list for further processing.
* We use the ``compare`` function to perform lexical ordering.
* We use a ``case`` expression to read the result of the ``compare`` and branch execution.
* Note that ``compare`` via the result ``EQ`` provides a mechanism for collecting equal symbols together.
* Note the recursive call to ``add`` when the incoming expression is lexically higher than the head. We pass over the head and continue the process.

**AB.3**

* The case of the two expressions being equal is dealt with by ``sum_`` when it deals with the equality of general expressions.
* For unequal expressions we simply place them in graded reverse lexical order inside the ``Sum``.
* The ``compare`` function was written for the express purpose of placing general expressions in lexical order (firstly by using their degree).
* ``compare`` returns an ``Ordering`` so we use ``case`` on the result to branch execution.
* Note that ``EQ`` here corresponds to the result of ``compare``. Two expressions which are unequal should never have ``EQ`` for the result of ``compare`` thus this is explicitly declared to be an error.




## AC. Adding ``Prod`` using ``sum_p``

**AC.1** - We are adding two (possibly negative) ``Prod`` expressions together. We are looking for the possibility of them both being a constant (possibly 1) multiplied by the **same** remaining expression.

**AC.2** - We use pattern-matching to ensure that the second expression is also a (negative) product in which case we hand over the calculation to `sum_pc`. Note that the first argument is guaranteed to be a (negative) product based on how ``sum_p`` is called from ``sum_`` only after such a pattern-match has occurred.

**AC.3** - If the second argument is not a product we simply pass calculation on to ``sum_x`` which is designed for general (non-special) addition of expressions.

**AC.4** - The purpose of ``split`` is to take a (negative) product and return a tuple consisting of a constant (which can be negative or even ``1``) and the rest of the list of expressions inside the product. The output is used by the `add` function defined below it.
 
**AC.5**

* First we check that the two lists ``as`` and ``bs`` match.
* If they do not the entire exercise is futile so we simply send the reconstituted products to ``sum_x``.
* If they do match we add the constants together and multiply it with the rest of the product. By using ``*`` here we guarantee that all the edge cases of ``0``, ``1`` and negative constants will be looked after automatically.



## AD. Constructing ``Frac``

**AD.1** - High level matching of numerator and denominator. If the two match we simply cancel them out and return 1. Otherwise we use ``frac'`` to further probe the possibility of cancellation.

**AD.2** 

- We are dividing a ``Prod`` by a non-``Prod`` expression and we are looking for the possibility of cancellation.
- To check for cancellation we recurse over the list of expressions inside the ``Prod`` and look for a possible cancellation using ``frac_`` function which is designed for this purpose. It tries all possible cancellations and returns a ``Nothing`` if unsuccessful.
- To that end we use the ``divide`` function defined after ``where``.
- It uses two lists and the denominator.
- The first list contains all of the elements that have already been tried and for which no cancellation occured.
- The second list contains all of the elements that remain.

**AD.3**

- This is the base case where the second list is empty.
- If the second list becomes empty it means no cancellation was possible and so we construct the uncancelled fraction.
- We use ``reverse`` on ``es`` because the way the elements are collected in ``es`` is in reverse order. The first element from ``xs`` is put in ``es``, then the second one is added using ``:`` so that it is now in the order 2:1:[] rather than 1:2:[].

**AD.4**

- This is the general (non-base case) and it in turn uses the ``branch`` function to carry out its task.
- We pass to branch the processed and unprocessed list of expressions ``es`` and ``xs`` respectively along with the current element ``x`` and the denominator ``d``.
- In addition the last argument is constructed by calling ``frac_`` on ``x`` and ``d``.
- If the call to ``frac_`` returns ``Nothing`` it means no simplification was possible and so we append ``x`` to ``es`` (note how the element is added in reverse order that is to the start of ``es`` and not its end) and pass the remaining list ``xs`` in a recursive call to ``divide``
- If ``frac_`` returns a ``Just e`` then the simplification was successful and we return a ``Prod`` consisting of the simplified term multipled by ``es`` and ``xs``.
- Since ``es`` was constructed in a reverse fashion we use ``reverse`` to put it right end up before we stitch it with ``xs``.
 
**AD.5** - For the case of a non-``Prod`` expression being divided by a ``Prod`` we use the ``rec`` function to flip it around and use the ``Prod`` divided by non-``Prod`` function defined above.

**AD.6** - This is the catch-all at the bottom which is used for all patterns that do not match the ones defined above for ``frac'``. For these we use ``frac_`` to attempt a simplification. If it works the simplified expression is returned otherwise an explicit ``Frac`` is constructed.

**AD.7**

* ``frac_`` expects two non-``Prod`` elements and attempts to simplify them during division.
* It uses pattern-matching to handle different cases.
* If a simplification is performed the result is returned wrapped inside a ``Just`` context.
* Otherwise a ``Nothing`` is returned to indicate that no simplification was possible.
* It checks for one or both expressions being exponents and finally equality  between elements.
* ``Prod`` in either numerator and/or denominator is handled in an element wise fashion where ``frac_`` is called for each element in a recursive fashion.

## Debugging

### ``trace``

Source: *https://wiki.haskell.org/Debugging*

``trace`` is an extremely useful function for debugging Haskell code and is invaluable during development. It should however never be left in production code.

The signature of ``trace`` is ``String -> a -> a``. You pass it a ``String`` which is usually a debugging message (sort of a ``printf``) and the actual value you want the function to return and ``trace`` will print the message and then return the value.

Basically it allows us to print a message (as a side-effect) without altering the actual purpose of our code. Invaluable.

To use ``trace`` one must import it as follows: ``import Debug.Trace(trace)`` which only imports this one function from the ``Debug.Trace`` module.

A point to note is that any function that uses ``trace`` will probably need access to the ``Show`` type-class so ``Show a`` may have to be added as a type constraint to the calling function.
