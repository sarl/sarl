# Function and subprogram definitions in SARL

<script>
function exerciseToggle(id) {
    var x = document.getElementById('exercise' + id.toString());
    if (x.style.display === "none") {
        x.style.display = "block";
    } else {
        x.style.display = "none";
    }
}
</script>

> **_Note:_** If you don't know how to solve an problem, or what is the function to be used, you could search on Internet for the answer using the API of the Java programming language. Indeed, since SARL is fully compatible with the Java API, you could use all the types or functions that are defined in this Java API.

## Exercise 1

* Write a SARL function to find the maximum of three numbers.

<a href="javascript:exerciseToggle(1)">Answer</a>
<div id="exercise1" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.fctexercises
	[:OnHtml]
	class Solution {
		static def max(a : double, b : double, c : double) {
			if (a > b && a > c) {
				return a
			}
			if (b > c) {
				return b
			}
			return c
		}
	}
[:End:]
</div>

## Exercise 2

* Write a SARL function to sum all the numbers in a list.
* Example of input list: `(8, 2, 3, 0, 7)`
* Expected output: `20`

<a href="javascript:exerciseToggle(2)">Answer</a>
<div id="exercise2" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.fctexercises
	[:OnHtml]
	import java.util.List
	class Solution {
		static def sum(list : List<Double>) : double {
			var sum = 0.0
			for (num : list) {
				sum += num
			}
			return sum
		}
	}
[:End:]
</div>

## Exercise 3

* Write a SARL function to multiply all the numbers in a list.
* Sample List: `(8, 2, 3, -1, 7)`
* Expected Output: `-336`

<a href="javascript:exerciseToggle(3)">Answer</a>
<div id="exercise3" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.fctexercises
	[:OnHtml]
	import java.util.List
	class Solution {
		static def mul(list : List<Double>) : double {
			var mul = 0.0
			for (num : list) {
				mul *= num
			}
			return mul
		}
	}
[:End:]
</div>

## Exercise 4

* Write a SARL program to reverse a string.
* Sample String: `1234abcd`
* Expected Output: `dcba4321`

<a href="javascript:exerciseToggle(4)">Answer</a>
<div id="exercise4" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.fctexercises
	[:OnHtml]
	class Solution {
		static def reverse(value : String) : String {
			var rev = new StringBuilder
			for (c : value.toCharArray) {
				rev.insert(0, c)
			}
			return rev.toString
		}
	}
[:End:]
</div>

## Exercise 5

* Write a SARL function to calculate the factorial of a number (a non-negative integer). The function accepts the number as an argument.

<a href="javascript:exerciseToggle(5)">Answer</a>
<div id="exercise5" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.fctexercises
	[:OnHtml]
	class Solution {
		static def fact(n : int) : int {
			if (n > 0) {
				return fact(n - 1) * n
			}
			return 1
		}
	}
[:End:]
</div>

## Exercise 6

* Write a SARL function to check whether a number falls within a given range.

<a href="javascript:exerciseToggle(6)">Answer</a>
<div id="exercise6" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.fctexercises
	[:OnHtml]
	class Solution1 {
		static def inRange(n : int, start : int, end : int) : boolean {
			start <= n && n <= end
		}
	}

	class Solution2 {
		static def inRange(n : int, start : int, end : int) : boolean {
			(start .. end).contains(n)
		}
	}
[:End:]
</div>

## Exercise 7

* Write a SARL function that accepts a string and counts the number of upper and lower case letters.
* Sample String: `The quick Brow Fox`
* Expected Output:

```text
No. of Upper case characters : 3
No. of Lower case Characters : 12
```

<a href="javascript:exerciseToggle(7)">Answer</a>
<div id="exercise7" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.fctexercises
	[:OnHtml]
	import static extension java.lang.Character.*
	class Solution {
		static def caseCount(value : String) : void {
			var lcount = 0
			var ucount = 0
			for (c : value.toCharArray) {
				if (c.isUpperCase) {
					ucount++
				} else if (c.isLowerCase) {
					lcount++
				}
			}
			println("No. of Upper case characters : " + ucount)
			println("No. of Lower case Characters : " + lcount)
		}
	}
[:End:]
</div>

## Exercise 8

* Write a SARL function that takes a list and returns a new list with distinct elements from the first list.
* Sample List: `[1,2,3,3,3,3,4,5]`
* Unique List: `[1, 2, 3, 4, 5]`

<a href="javascript:exerciseToggle(8)">Answer</a>
<div id="exercise8" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.fctexercises
	[:OnHtml]
	import java.util.List
	class Solution1 {
		static def disctinct(list : List<Integer>) : List<Integer> {
			var uniq = newArrayList
			for (c : list) {
				if (!uniq.contains(c)) {
					uniq += c
				}
			}
			return uniq
		}
	}

	class Solution2 {
		static def disctinct(list : List<Integer>) : List<Integer> {
			var uniq = newTreeSet(null)
			uniq.addAll(list)
			return newArrayList(uniq)
		}
	}
[:End:]
</div>

## Exercise 9

* Write a SARL function that takes a number as a parameter and checks whether the number is prime or not.
* A prime number (or a prime) is a natural number greater than 1 and that has no positive divisors other than 1 and itself.

<a href="javascript:exerciseToggle(9)">Answer</a>
<div id="exercise9" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.fctexercises
	[:OnHtml]
	class Solution {
		static def isPrime(n : int) : boolean {
			if (n == 1) {
				return false
			} else if (n == 2) {
				return true
			}
			for (x : 2..<n) {
				if (n % x == 0) {
					return false
				}
			}
			return true
		}
	}
[:End:]
</div>

## Exercise 10

* Write a SARL program to print the even numbers from a given list.
* Sample List: `[1, 2, 3, 4, 5, 6, 7, 8, 9]`
* Expected Result: `[2, 4, 6, 8]`

<a href="javascript:exerciseToggle(10)">Answer</a>
<div id="exercise10" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.fctexercises
	[:OnHtml]
	import java.util.List
	class Solution {
		static def showEvenNumberList(list : List<Integer>) : void {
			var evenNums = newArrayList
			for (num : list) {
				if (num % 2 == 0) {
					evenNums += num
				}
			}
			println(evenNums)
		}
	}
[:End:]
</div>

## Exercise 11

* Write a SARL function to check whether a number is "Perfect" or not.
* According to Wikipedia: In number theory, a perfect number is a positive integer that is equal to the sum of its proper positive divisors, that is, the sum of its positive divisors excluding the number itself (also known as its aliquot sum). Equivalently, a perfect number is a number that is half the sum of all of its positive divisors (including itself).
* Example: The first perfect number is 6, because 1, 2, and 3 are its proper positive divisors, and 1 + 2 + 3 = 6. Equivalently, the number 6 is equal to half the sum of all its positive divisors: ( 1 + 2 + 3 + 6 ) / 2 = 6. The next perfect number is 28 = 1 + 2 + 4 + 7 + 14. This is followed by the perfect numbers 496 and 8128.

<a href="javascript:exerciseToggle(11)">Answer</a>
<div id="exercise11" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.fctexercises
	[:OnHtml]
	class Solution {
		static def isPerfect(n : int) : boolean {
			var sum = 0
			for (x : 1..<n) {
				if (n % x == 0) {
					sum += x
				}
			}
			sum == n
		}
	}
[:End:]
</div>

## Exercise 12

* Write a SARL function that checks whether a passed string is a palindrome or not.
* A palindrome is a word, phrase, or sequence that reads the same backward as forward, e.g., `madam` run.

<a href="javascript:exerciseToggle(12)">Answer</a>
<div id="exercise12" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.fctexercises
	[:OnHtml]
	class Solution {
		static def isPalindrome(value : String) : boolean {
			var i = 0
			var j = value.length - 1
			while (i < j) {
				if (value.charAt(i) != value.charAt(j)) {
					return false
				}
			}
			return true
		}
	}
[:End:]
</div>

## Exercise 13

* Write a SARL function to check whether a string is a pangram or not.
* Pangrams are words or sentences containing every letter of the alphabet at least once.
* For example: `The quick brown fox jumps over the lazy dog` is a pangram.

<a href="javascript:exerciseToggle(13)">Answer</a>
<div id="exercise13" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.fctexercises
	[:OnHtml]
	import static extension java.lang.Character.*
	class Solution {
		static def isPangram(value : String) : boolean {
			if (value.length < 26) {
				return false
			}
			var found = newTreeSet(null)
			for (c : value.toCharArray) {
				if (found.size == 26) {
					return true
				}
				if (c.isLetter) {
					found += c.toLowerCase
				}
			}
			return found.size == 26
    	}
	}
[:End:]
</div>

## Exercise 14

* Write a SARL program that accepts a hyphen-separated sequence of words as input and prints the words in a hyphen-separated sequence after sorting them alphabetically.
* Sample Items: `green-red-yellow-black-white`
* Expected Result: `black-green-red-white-yellow`

<a href="javascript:exerciseToggle(14)">Answer</a>
<div id="exercise14" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.fctexercises
	[:OnHtml]
	import static extension java.util.Collections.sort;
	class Solution {
		static def sortWords(value : String) {
			var components = newArrayList(value.split("\\-"))
			components.sort
			var result = String.join('-', components)
			println(result)
    	}
	}
[:End:]
</div>

## Exercise 15

* Write a SARL function to create and print a list where the values are the squares of numbers between 1 and 30 (both included).

<a href="javascript:exerciseToggle(15)">Answer</a>
<div id="exercise15" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.fctexercises
	[:OnHtml]
	class Solution {
		static def showSquareNumbers {
			var numbers = newArrayList
			for (num : 1..30) {
				numbers += num**2
			}
			println(numbers)
    	}
	}
[:End:]
</div>

[:Include:](../../includes/legal.inc)
