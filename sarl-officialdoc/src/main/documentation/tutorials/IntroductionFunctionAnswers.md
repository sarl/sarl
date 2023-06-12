# Introduction to Functions with SARL - Answers

[:Outline:]

## Exercise 1

[:Success:]
	package io.sarl.docs.tutorials.fctexercises
	[:On]
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

## Exercise 2

[:Success:]
	package io.sarl.docs.tutorials.fctexercises
	[:On]
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

## Exercise 3

[:Success:]
	package io.sarl.docs.tutorials.fctexercises
	[:On]
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

## Exercise 4

[:Success:]
	package io.sarl.docs.tutorials.fctexercises
	[:On]
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

## Exercise 5

[:Success:]
	package io.sarl.docs.tutorials.fctexercises
	[:On]
	class Solution {
		static def fact(n : int) : int {
			if (n > 0) {
				return fact(n - 1) * n
			}
			return 1
		}
	}
[:End:]

## Exercise 6

[:Success:]
	package io.sarl.docs.tutorials.fctexercises
	[:On]
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

## Exercise 7

[:Success:]
	package io.sarl.docs.tutorials.fctexercises
	[:On]
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

## Exercise 8

[:Success:]
	package io.sarl.docs.tutorials.fctexercises
	[:On]
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

## Exercise 9

[:Success:]
	package io.sarl.docs.tutorials.fctexercises
	[:On]
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

## Exercise 10

[:Success:]
	package io.sarl.docs.tutorials.fctexercises
	[:On]
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

## Exercise 11

[:Success:]
	package io.sarl.docs.tutorials.fctexercises
	[:On]
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

## Exercise 12

[:Success:]
	package io.sarl.docs.tutorials.fctexercises
	[:On]
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

## Exercise 13

[:Success:]
	package io.sarl.docs.tutorials.fctexercises
	[:On]
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

## Exercise 14

[:Success:]
	package io.sarl.docs.tutorials.fctexercises
	[:On]
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

## Exercise 15

[:Success:]
	package io.sarl.docs.tutorials.fctexercises
	[:On]
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



[:Include:](../legal.inc)
