code snippets of the book《the seasoned schemer》

this README is inspired by Peter Krumins's [the seasoned schemer](<https://github.com/pkrumins/the-seasoned-schemer>).

### Contents
=====================================================================================

- [chapter 11. Welcome Back to the Show](#chapter-11-welcome-back-to-the-show)
- [chapter 12. Take Cover](#chapter-12-take-cover)
- [chapter 13. Hop ,Skip and Jump](#chapter-13-hop-skip-and-jump)
- [chapter 14. Let There Be Names](#chapter-14-let-there-be-names)
- [chapter 15. The Difference Between Men and Boys](#chapter-15-the-difference-between-men-and-boys)
- [chapter 16. Ready, Set Bang!](#chapter-16-ready-set-bang)
- [chapter 17. We Change Therefore We Are](#chapter-17-we-change-therefore-we-are)
- [chapter 18. We Change Therefore We Are the Same](#chapter-18-we-change-therefore-we-are-the-same)
- [chapter 19. Absconding with the Jewels](#chapter-19-absconding-with-the-jewels)
- [chapter 20. What is in Store?](#chapter-20-what-is-in-store)

=====================================================================================

### chapter 11. Welcome Back to the Show

Welcome back to the Show!

This chapter first introduces functions two-in-a-row, sum-of-prefixes, both of which use helper functions in their definitions.

also, the eleventh commandment is stated:

**the eleventh commandment**

> Use additional arguments when a function needs to know what other arguments to the function have been like so far.

then, function named scramble is defined, the definition of this function is a little hard to understand.

Peter Krumins said the explanation in the book was not CORRECT. He thought that "The result at each position is found by counting backward from the NEXT position to this index." not from the current position to this index.

Yes, he's right. can't agree more.

=====================================================================================

### chapter 12. Take Cover

You will see Y combinator in this chapter, but be relax.

This chapter is about (letrec …), a new primitive function.

After using (letrec …) to redefine multirember, you'll be fine with it.

and also the twelfth commandment is presented.

**the twelfth commandment**

> Use (letrec ...) to remove arguments that do not change for recursive applications.

In the later part, More examples about (letrec …) are given.

After rewriting the union function, the thirteenth commandment is presented.

**the thirteenth commandment**

> Use (letrec ...) to hide and to protect functions.

Rewrite the functions we have defined in chapter 11 with (letrec …), you'll be familiar with it.

After that, take a rest and make yourself a great dessert.

=====================================================================================

### chapter 13. Hop Skip and Jump

Just as it is titled, this chapter is going to tell you how to hop, skip and jump with (letcc ….).

With the definition of intersectall using (letcc …...), we get the fourteenth commandment.

**the fourteenth commandment**

> Use (letcc ...) to return values abruptly and promptly.

Be careful when using (letcc ….),  using (call-with-current-continuation …..) instead if letcc is undefined.

More examples are given to show the power of (letcc …..) and make you familiar with it.

 You'll see it again in the later chapters.

=====================================================================================

### chapter 14. Let There Be Names

Introduces (let ….), which is similar to (define …..).

After using (let ….) to simplify functions like leftmost, rember1\*, the preliminary version of the fifteenth commandment is stated:

**the fifteenth commandment [preliminary version]**

> Use (let ...) to name the values of repeated expressions.

with the definition of depth\*, here goes a revised version:

**the fifteenth commandment [revised version]**

> Use (let ...) to name the values of repeated expressions in a function definition if they may be evaluated twice for one and the same use of the function.

In the end of this chapter, (try….) is applied to simplify the definition of rm. be careful when you try it, cause I found it not working.

=====================================================================================

### chapter 15. The Difference Between Men and Boys

This chapter and the next one are all about (set! …).

Using (set! …),  you can change the value of variables defined in (let ….).

here, the sixteenth,  the seventeenth[preliminary version] and the eighteenth commandments are stated:

**the sixteenth commandment**

> Use (set! ...) only with names defined in (let...)s.



**the seventeenth commandment[preliminary version]**

> Use (set! ...) for (let ((x ...))) only if there is at least one (lambda ..) between it and the (let ((x ...))).



**the eighteenth commandment**

> Use (set! ...) only when the value that x refers to is no longer needed.

=====================================================================================

### chapter 16. Ready Set Bang

Using  (set! ….) to define deep function. Collect the results that deep have been returned and the numbers it has applied to. This seems to be a kind of tail call.

and, also the nineteenth commandment is stated.

**the nineteenth commandment**:

> Use (set! ...) to remember valuable things between two distinct uses of a function.

later, we meet Y-combinator again. and also a new friend Y-bang.

After the derivation of Y-bang, we get the final version of the seventeenth commandment.

**the seventeenth commandment[final version]**

> Use (set! ...) for (let ((x ...))) only if there is at least one (lambda ..) between it and the  (let ((x ...))) or if the new value for x is a function refers to x.

Enjoy the difference between  Y and Y-bang.

=====================================================================================

### chapter 17. We Change Therefore We Are

Continues with the function deep, later, defines a new function called consC to show how to calculate the usage of  (cons …..) .

More examples of (set! …..) are given to make you understand how (set! ….) works.

=====================================================================================

### chapter 18. We Change Therefore We Are the Same

Defines functions like kar, kdr, kons,  which acts similar to primitive car, cdr , cons.

Introduces (set-cdr! …..) function, which modify the cdr of a list. also defines a function same? to show how to check if two list are the same.

At the end of this chapter, function named finite-lenkth is used to return its argument's length.

=====================================================================================

### chapter 19. Absconding with the Jewels

This chapter gives some examples of continuations and collector. Collector is sometimes called continuation, remember?

After you go with the author to define the function two-in-a-row\* step by step, you'll see the power of letcc.

=====================================================================================

### chapter 20. What is in Store

Just like the chapter 10, this chapter also writes a compiler to parse scheme in scheme.

You can treat chapter 10 as a preliminary version, and this chapter, a final version.


OK. Time for 🍮 !
