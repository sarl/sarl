# Introduction to Lists with SARL - Answers

[:Outline:]

## Exercise 1

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List	
	class Solution1 {
		static def sum(list : List<? extends Number>) : Number {
			var s = 0.0
			for (e : list) {
				s += e
			}
			return s
		}
	}

	class Solution2 {
		static def sum(list : List<? extends Number>) : Number {
			list.reduce[accumulator, current | accumulator.doubleValue + current.doubleValue]
		}
	}
[:End:]

## Exercise 2

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List	
	class Solution1 {
		static def mul(list : List<? extends Number>) : Number {
			var s = 0.0
			for (e : list) {
				s *= e
			}
			return s
		}
	}

	class Solution2 {
		static def mul(list : List<? extends Number>) : Number {
			list.reduce[accumulator, current | accumulator.doubleValue * current.doubleValue]
		}
	}
[:End:]

## Exercise 3

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List	
	class Solution1 {
		static def maxValue(list : List<? extends Number>) : Number {
			var m : Number = null
			for (e : list) {
				if (m === null || m < e) {
					m = e
				}
			}
			return m
		}
	}

	class Solution2 {
		static def maxValue(list : List<? extends Number>) : Number {
			list.max[a, b | a <=> b]
		}
	}
[:End:]

## Exercise 4

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List	
	class Solution1 {
		static var original = #['abc', 'xyz', 'aba', '1221']

		static def countStrings(list : List<String>) : int {
			var n = 0
			for (s : list) {
				if (s.length >= 2 && s.charAt(0) == s.charAt(s.length - 1)) {
					n++
				}
			}
			return n
		}

		static def main : void {
			println(countStrings(original))
		}
	}

	class Solution2 {
		static var original = #['abc', 'xyz', 'aba', '1221']

		static def countStrings(list : List<String>) : int {
			list.filter[it.length >= 2 && it.charAt(0) == it.charAt(it.length - 1)].size
		}

		static def main : void {
			println(countStrings(original))
		}
	}
[:End:]

## Exercise 5

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List	
	class Solution {
		static var original = #[#[2, 5], #[1, 2], #[4, 4], #[2, 3], #[2, 1]]

		static def main : void {
			var result = original.sortWith[a, b | 
				a.get(1) <=> b.get(1)
			]
			println(result)
		}
	}
[:End:]

## Exercise 6

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List	
	class Solution {
		static def remove_duplicates(list : List<?>) {
			var result = newArrayList
			for (e : list) {
				if (!result.contains(e)) {
					result += e
				}
			}
			return result
		}
	}
[:End:]

## Exercise 7

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List	
	class Solution {
		static def enquiry(list : List<?>) {
			list.size === 0
		}
	}
[:End:]

## Exercise 8

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List	
	class Solution {
		static def copy(list : List<?>) {
			var cp = newArrayList
			cp.addAll(list)
			return cp
		}
	}
[:End:]

## Exercise 9

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List	
	class Solution1 {
		static def words(list : List<String>, n : int) {
			var cp = newArrayList
			for (e : list) {
				if (e.length > n) {
					cp += e
				}
			}
			return cp
		}
	}

	class Solution2 {
		static def words(list : List<String>, n : int) {
			list.filter[it.length > n].toList
		}
	}
[:End:]

## Exercise 10

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List	
	class Solution1 {
		static def common(list1 : List<?>, list2 : List<?>) {
			for (e : list1) {
				if (list2.contains(e)) {
					return true
				}
			}
			return false
		}
	}

	class Solution2 {
		static def words(list1 : List<String>, list2 : List<?>) {
			list1.exists[list2.contains(it)]
		}
	}
[:End:]

## Exercise 11

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	class Solution {
		static var original = #['Red', 'Green', 'White', 'Black', 'Pink', 'Yellow']

		static def main : void {
			original.remove(5)
			original.remove(4)
			original.remove(0)
			println(original)
		}
	}
[:End:]

## Exercise 12

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	class Solution1 {
		static def main : void {
			var result = newArrayList
			for (a : 1..3) {
				var dim2 = newArrayList
				for (b : 1..4) {
					var dim3 = newArrayList
					for (c : 1..6) {
						dim3 += '*'
					}
					dim2 += dim3
				}
				result += dim2
			}
			println(result)
		}
	}

	class Solution2 {
		static def main : void {
			var result = (1..3).map[
				(1..4).map[
					(1..6).map['*'].toList
				].toList
			].toList
			println(result)
		}
	}
[:End:]

## Exercise 13

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List
	class Solution1 {
		static def remove_even(list : List<Integer>) : List<Integer> {
			var nlist = newArrayList
			for (e : list) {
				if ((e % 2) != 0) {
					nlist += e
				}
			}
			return nlist
		}
	}

	class Solution2 {
		static def remove_even(list : List<Integer>) : List<Integer> {
			list.filter[(it % 2) != 0].toList
		}
	}
[:End:]

## Exercise 14

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import static extension java.util.Collections.shuffle
	class Solution {

		static var original = #['Red', 'Green', 'White', 'Black', 'Pink', 'Yellow']

		static def main : void {
			original.shuffle
			println(original)
		}
	}
[:End:]

## Exercise 15

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List
	class Solution {

		static def swap(elements : Object[], index0 : int, index1 : int) : void {
			var tmp = elements.get(index0)
			elements.set(index0, elements.get(index1))
			elements.set(index1, tmp);
		}

		static def permuts(list : List<Object>) : List<List<Object>> {
			var permuts = newArrayList
			permuts.add(list.immutableCopy)
			if (list.size > 1) {
				var elements = list.toArray
				var indexes = newIntArrayOfSize(list.size)
				var i = 0
				while (i < list.size) {
				    if (indexes.get(i) < i) {
				       swap(elements, if (i % 2 == 0) 0 else indexes.get(i), i)
						permuts.add(newArrayList(elements))
				       indexes.set(i, indexes.get(i) + 1);
				       i = 0
				    } else {
				       indexes.set(i, 0)
				       i++
				    }
				}
			}
			return permuts
		}
	}
[:End:]

## Exercise 16

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List
	class Solution {
		static def difference(a : List<Object>, b : List<Object>) : List<Object> {
			var diff = newArrayList
			for (element : a) {
				if (!b.contains(element)) {
					diff += element
				}
			}
			return diff
		}
	}
[:End:]

## Exercise 17

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	class Solution1 {
		static var original = #[5, 15, 35, 8, 98]

		static def main : void {
			var i = 0
			for (element : original) {
				println(i + " " + element)
				i++
			}
		}
	}

	class Solution2 {
		static var original = #[5, 15, 35, 8, 98]

		static def main : void {
			for (var i = 0; i < original.size; i++) {
				println(i + " " + original.get(i))
			}
		}
	}

	class Solution3 {
		static var original = #[5, 15, 35, 8, 98]

		static def main : void {
			original.forEach [element, index |
				println(index + " " + element)
			]
		}
	}
[:End:]

## Exercise 18

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List
	class Solution1 {
		static var original : List<Character> = #['h', 'e', 'l', 'l', 'o']

		static def main : void {
			var str = ""
			for (c : original) {
				str += c
			}
			println(str)
		}
	}

	class Solution2 {
		static var original : List<Character> = #['h', 'e', 'l', 'l', 'o']

		static def main : void {
			var str_buffer = new StringBuilder
			for (c : original) {
				str_buffer.append(c)
			}
			var str = str_buffer.toString
			println(str)
		}
	}

	class Solution3 {
		static var original : List<Character> = #['h', 'e', 'l', 'l', 'o']

		static def main : void {
			var str = original.join("")
			println(str)
		}
	}
[:End:]

## Exercise 19

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List
	class Solution1 {
		static var original : List<Character> = #['h', 'e', 'l', 'l', 'o']

		static def findIndexOf(list : List<?>, element : Object) : int {
			var i = 0
			for (elt : list) {
				if (elt == element) {
					return i
				}
				i++
			}
			return -1
		}

		static def main : void {
			println(original.findIndexOf('h'))
			println(original.findIndexOf('e'))
			println(original.findIndexOf('l'))
			println(original.findIndexOf('o'))
		}
	}

	class Solution2 {
		static var original : List<Character> = #['h', 'e', 'l', 'l', 'o']

		static def main : void {
			println(original.indexOf('h'))
			println(original.indexOf('e'))
			println(original.indexOf('l'))
			println(original.indexOf('o'))
		}
	}
[:End:]

## Exercise 20

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List
	class Solution1 {
		static var original = #[#[4], #[0, 658, 4, 6], #[1, 2, 3]]

		static def flattenList(list : List<List<Integer>>) : List<Integer> {
			var output = newArrayList
			for (sublist : list) {
				for (element : sublist) {
					output += element
				}
			}
			return output
		}

		static def main : void {
			println(original.flattenList)
		}
	}

	class Solution2 {
		static var original = #[#[4], #[0, 658, 4, 6], #[1, 2, 3]]

		static def flattenList(list : List<List<Integer>>) : List<Integer> {
			var output = newArrayList
			for (sublist : list) {
				output += sublist
			}
			return output
		}

		static def main : void {
			println(original.flattenList)
		}
	}

	class Solution3 {
		static var original = #[#[4], #[0, 658, 4, 6], #[1, 2, 3]]

		static def main : void {
			println(original.flatten.toList)
		}
	}
[:End:]

## Exercise 21

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	class Solution1 {
		static var original1 = #[1, 45, 8, 6, 1]
		static var original2 = #[457, -1, 5]

		static def main : void {
			for (c : original2) {
				original1 += c
			}
			println(original1)
		}
	}

	class Solution2 {
		static var original1 = #[1, 45, 8, 6, 1]
		static var original2 = #[457, -1, 5]

		static def main : void {
			original1 += original2
			println(original1)
		}
	}
[:End:]

## Exercise 22

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.Random
	class Solution {
		static var original = #[1, 45, 8, 6, 1]

		static def main : void {
			var random = new Random
			var element = original.get(random.nextInt(original.size))
			println(element)
		}
	}
[:End:]

## Exercise 23

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List
	class Solution {
		static var list1 = #[10, 10, 0, 0, 10]
		static var list2 = #[10, 10, 10, 0, 0]

		static def circularly_identical(list1 : List<Integer>, list2 : List<Integer>) : boolean {
			// doubling list
			var list3 = newArrayList(list1)
		    list3.addAll(list1)
			// traversal in twice of list1
			for (x : 0..<list1.size) {
				var z = 0
				// check if list2 == list1 circularly
		        for (y : x..<(x + list1.size)) {
		            if (list2.get(z) == list3.get(y)) {
		            	z += 1
		            } else {
		            	break
		            }
		        }
		             
				// if all n elements are same circularly
				if (z == list1.size) {
					return true
				}
			}
		    return false
		}
   
		static def main : void {
			println(circularly_identical(list1, list2))
		}
	}
[:End:]

## Exercise 24

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	class Solution1 {
		static var original = #[1, 45, -45, 2, 987]

		static def main : void {
			var smaller : Integer = null
			for (c : original) {
				if (smaller === null || c < smaller) {
					smaller = c
				}
			}
			var secondSmaller : Integer = null
			for (c : original) {
				if (c > smaller && (smaller === null || c < secondSmaller)) {
					secondSmaller = c
				}
			}
			println(secondSmaller)
		}
	}

	class Solution2 {
		static var original = #[1, 45, -45, 2, 987]

		static def main : void {
			var smaller = original.min
			var secondSmaller = original.filter[it > smaller].min
			println(secondSmaller)
		}
	}
[:End:]

## Exercise 25

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List
	class Solution1 {
		static var original = #[1, 45, -45, 1, 2, 987, 45]

		static def get_uniq(list : List<Integer>) : List<Integer> {
			var uniq = newArrayList
			for (c : list) {
				if (!uniq.contains(c)) {
					uniq += c
				}
			}
			return uniq
		}

		static def main : void {
			println(get_uniq(original))
		}
	}

	class Solution2 {
		static var original = #[1, 45, -45, 1, 2, 987, 45]

		static def get_uniq(list : List<Integer>) : List<Integer> {
			var uniq = newTreeSet(null)
			uniq.addAll(list)
			return uniq.toList
		}

		static def main : void {
			println(get_uniq(original))
		}
	}
[:End:]

## Exercise 26

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List
	import java.util.Map
	class Solution {
		static var original = #[1, 45, -45, 1, 2, 987, 45]

		static def frequency(list : List<T>) : Map<T, Integer> with T {
			var map = newHashMap
			for (c : list) {
				var n = map.getOrDefault(c, 0)
				map.put(c, n + 1)
			}
			return map
		}

		static def main : void {
			println(frequency(original))
		}
	}
[:End:]

## Exercise 27

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List
	class Solution {
		static var original = #[1, 45, -45, 1, 2, 987, 45]

		static def is_sublist(list : List<Integer>, search_for : List<Integer>) : boolean {
			if (search_for.isEmpty) {
				return true
			}
			if (search_for.size > list.size) {
				return false
			}
			for (i : 0..<list.size) {
				if (list.get(i) == search_for.get(0)) {
					var n = 1
					while ((n < search_for.size) && (list.get(i + n) == search_for.get(n))) {
						n++
					}
					if (n == search_for.size) {
						return true
					}
				}
			}
			return false
		}

		static def main : void {
			println(is_sublist(original, #[1, 2, 987]))
			println(is_sublist(original, #[1, 987, 2]))
		}
	}
[:End:]

## Exercise 28

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.BitSet
	import java.util.List
	class Solution1 {
		static def sieve_of_eratosthenes(num : int) {
			var prime = newArrayList
			for (i : 0..num) {
				prime += true
			}
			var p = 2
			while (p * p <= num) {
				// If prime[p] is not changed, then it is a prime
				if (prime.get(p)) {
					// Updating all multiples of p
					for (var i = p * p; i <= num; i += p) {
						prime.set(i, false)
					}
				}
				p += 1
			}
			// Print all prime numbers
			for (p2 : 2..num) {
				if (prime.get(p2)) {
	    			println(p2)
	    		}
	    	}
		}

		static def main : void {
			sieve_of_eratosthenes(10)
			sieve_of_eratosthenes(20)
		}
	}

	class Solution2 {
		static def sieve_of_eratosthenes(num : int) {
			var prime = new BitSet(num)
			var p = 2
			while (p * p <= num) {
				// If prime[p] is not changed, then it is a prime
				if (!prime.get(p)) {
					// Updating all multiples of p
					for (var i = p * p; i <= num; i += p) {
						prime.set(i)
					}
				}
				p += 1
			}
			// Print all prime numbers
			for (i : 2..num) {
				if (!prime.get(i)) {
					println(i)
				}
			}
		}

		static def main : void {
			sieve_of_eratosthenes(10)
			sieve_of_eratosthenes(20)
		}
	}
[:End:]

## Exercise 29

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List
	class Solution {
		static def concat(list : List<String>, num : int) : List<String> {
			var new_list = newArrayList
			for (element : list) {
				for (i : 1..num) {
					new_list += element + i
				}
			}
			return new_list
		}

		static def main : void {
			println(concat(#['p', 'q'], 5))
		}
	}
[:End:]

## Exercise 30

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List
	class Solution {
		static def common(list1 : List<Integer>, list2 : List<Integer>) : List<Integer> {
			var common_list = newArrayList
			for (element : list1) {
				if (list2.contains(element)) {
					common_list += element
				}
			}
			return common_list
		}

		static def main : void {
			println(common(#[1, 2, 4, 6], #[2, 34, 6, 122]))
		}
	}
[:End:]

## Exercise 31

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List
	class Solution {
		static def change(list : List<Integer>) : List<Integer> {
			var new_list = newArrayList
			for (var i = 0; i < list.size; i += 2) {
				var tmp = list.get(i)
				list.set(i, list.get(i + 1))
				list.set(i + 1, tmp)
			}
			return new_list
		}

		static def main : void {
			println(change(#[0,1,2,3,4,5]))
		}
	}
[:End:]

## Exercise 32

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List
	class Solution {
		static def diz(n : int) : int {
			var d = 1
			while (n > d) {
				d *= 10
			}
			return d
		}

		static def merge(list : List<Integer>) : Integer {
			var result = 0
			for (element : list) {
				result *= diz(element)
				result += element
			}
			return result
		}

		static def main : void {
			println(merge(#[11, 33, 50]))
		}
	}
[:End:]

## Exercise 33

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List
	import java.util.Map
	class Solution1 {
		static var original = #[
			'be', 'have', 'do', 'say', 'get', 'make',
			'go', 'know', 'take', 'see', 'come', 'think',
			'look', 'want', 'give', 'use', 'find',
			'tell', 'ask', 'work', 'seem', 'feel',
			'leave', 'call']

		static def split_by_letter(words : List<String>) : Map<Character, List<String>> {
			var map : Map<Character, List<String>> = newHashMap
			for (word : words) {
				var letter = word.charAt(0)
				var list = map.computeIfAbsent(letter, [newArrayList])
				list += word
			}
			return map
		} 

		static def main : void {
			println(split_by_letter(original))
		}
	}

	class Solution2 {
		static var original = #[
			'be', 'have', 'do', 'say', 'get', 'make',
			'go', 'know', 'take', 'see', 'come', 'think',
			'look', 'want', 'give', 'use', 'find',
			'tell', 'ask', 'work', 'seem', 'feel',
			'leave', 'call']

		static def split_by_letter(words : List<String>) : Map<Character, List<String>> {
			return words.groupBy[it.charAt(0)]
		} 

		static def main : void {
			println(split_by_letter(original))
		}
	}
[:End:]

## Exercise 34

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	class Solution1 {
		static def main : void {
			var result = newHashMap
			for (i : 1..10) {
				result.computeIfAbsent(i) [newArrayList]
			}
			println(result)
		}
	}

	class Solution2 {
		static def main : void {
			var result = (1..10).toInvertedMap[newArrayList]
			println(result)
		}
	}
[:End:]

## Exercise 35

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	class Solution1 {
		static var original = #[
			#[1, 2], #[3, 4], #[1, 2], #[5, 6], #[7, 8],
			#[1, 2], #[3, 4], #[3, 4], #[7, 8], #[9, 10]
		]

		static def main : void {
			var result = newArrayList
			for (tuple : original) {
				for (element : tuple) {
					if (!result.contains(element)) {
						result += element
					}
				}
			}
			println(result)
		}
	}

	class Solution2 {
		static var original = #[
			#[1, 2], #[3, 4], #[1, 2], #[5, 6], #[7, 8],
			#[1, 2], #[3, 4], #[3, 4], #[7, 8], #[9, 10]
		]

		static def main : void {
			var result = newTreeSet(null)
			for (tuple : original) {
				for (element : tuple) {
					result += element
				}
			}
			println(result)
		}
	}

	class Solution3 {
		static var original = #[
			#[1, 2], #[3, 4], #[1, 2], #[5, 6], #[7, 8],
			#[1, 2], #[3, 4], #[3, 4], #[7, 8], #[9, 10]
		]

		static def main : void {
			var result = newTreeSet(null)
			for (tuple : original) {
				result += tuple
			}
			println(result)
		}
	}
[:End:]

## Exercise 36

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	class Solution1 {
		static var original = #[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

		static def main : void {
			var result = newArrayList
			for (value : original) {
				if ((value % 2) != 0) {
					result += value
				}
			}
			println(result)
		}
	}

	class Solution2 {
		static var original = #[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

		static def main : void {
			var result = original.filter[(it % 2) != 0].toList
			println(result)
		}
	}
[:End:]

## Exercise 37

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List
	class Solution {
		static var original = #[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

		static def appendBefore(list : List<Integer>, prefix : Integer) : List<Integer> {
			var result = newArrayList
			for (element : list) {
				result += prefix
				result += element
			}
			return result
		}

		static def main : void {
			println(appendBefore(original, 34))
		}
	}
[:End:]

## Exercise 38

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List
	class Solution {
		static var original = #[#['assign1', 'assign2'], #['final','assign4'], #['exam','study']]

		static def main : void {
			for (list : original) {
				if (!list.isEmpty) {
					var iter = list.iterator
					print(iter.next)
					while (iter.hasNext) {
						print(" " + iter.next)
					}
				}
				println
			}
		}
	}
[:End:]

## Exercise 39

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	class Solution {
		static var originalNames = #["Black", "Red", "Maroon", "Yellow"]
		static var originalColors = #["#000000", "#FF0000", "#800000", "#FFFF00"]

		static def main : void {
			var result = newArrayList
			var iter1 = originalNames.iterator
			var iter2 = originalColors.iterator
			while (iter1.hasNext && iter2.hasNext) {
				var map = newHashMap
				map.put('color_name', iter1.next)
				map.put('color_code', iter2.next)
				result += map
			}
			println(result)
		}
	}
[:End:]

## Exercise 40

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List
	class Solution {
		static var original = #['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n']

		static def split_at_n(list : List<String>, n : int) : List<List<String>> {
			var result = newArrayList
			var clist = newArrayList
			for (element : list) {
				if (clist.size == n) {
					result.add(clist)
					clist = newArrayList
				}
				clist += element
			}
			if (!clist.isEmpty) {
				result.add(clist)
			}
			return result
		}

		static def main : void {
			println(split_at_n(original, 5))
		}
	}
[:End:]

## Exercise 41

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List
	class Solution {
		static var a = #["red", "orange", "green", "blue", "white"]
		static var b = #["black", "yellow", "green", "blue"]

		static def difference(list1 : List<String>, list2 : List<String>) : List<String> {
			var result = newArrayList
			for (element : list1) {
				if (!list2.contains(element)) {
					result += element
				}
			}
			return result
		}

		static def main : void {
			println(difference(a, b))
			println(difference(b, a))
		}
	}
[:End:]

## Exercise 42

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List
	class Solution {
		static def replace_last(list1 : List<Integer>, list2 : List<Integer>) : List<Integer> {
			var result = newArrayList
			result += list1.subList(0, list1.size - 1)
			result += list2
			return result
		}

		static def main : void {
			println(replace_last(#[1, 3, 5, 7, 9, 10], #[2, 4, 6, 8]))
		}
	}
[:End:]

## Exercise 43

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List
	class Solution {
		static def exist_in(list : List<Integer>, position : int) : boolean {
			0 <= position && position < list.size
		}

		static def main : void {
			println(exist_in(#[1, 3, 5, 7, 9, 10], 3))
			println(exist_in(#[1, 3, 5, 7, 9, 10], 8))
		}
	}
[:End:]

## Exercise 44

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List
	class Solution1 {
		static var original = #[1, 2, 3, 4]

		static def append_prefix(list : List<Integer>, prefix : String) : List<String> {
			var result = newArrayList
			for (element : list) {
				result += prefix + element
			}
			return result
		}

		static def main : void {
			println(append_prefix(original, "emp"))
		}
	}

	class Solution2 {
		static var original = #[1, 2, 3, 4]

		static def append_prefix(list : List<Integer>, prefix : String) : List<String> {
			list.map[prefix + it].toList
		}

		static def main : void {
			println(append_prefix(original, "emp"))
		}
	}
[:End:]

## Exercise 45

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List
	import java.util.Iterator
	class Solution1 {
		static var original1 = #[1, 2, 3]
		static var original2 = #['red', 'white', 'black']

		static def zip(list1 : List<?>, list2 : List<?>) : List<Object[]> {
			var result : List<Object[]> = newArrayList
			val iter1 = list1.iterator
			val iter2 = list2.iterator
			while (iter1.hasNext && iter2.hasNext) {
				result.add(#[iter1.next, iter2.next])
			}
			return result
		}

		static def main : void {
			for (tuple : zip(original1, original2)) {
				println(tuple)
			}
		}
	}

	class Solution2 {
		static var original1 = #[1, 2, 3]
		static var original2 = #['red', 'white', 'black']

		static def zip(list1 : List<?>, list2 : List<?>) : Iterable<Object[]> {
			[
				val iter1 = list1.iterator
				val iter2 = list2.iterator
				new Iterator<Object[]> {
					def hasNext : boolean {
						iter1.hasNext && iter2.hasNext
					}
					def next : Object[] {
						var v1 = iter1.next
						var v2 = iter2.next
						return #[v1, v2]
					}
				}
			]
		}

		static def main : void {
			for (tuple : zip(original1, original2)) {
				println(tuple)
			}
		}
	}
[:End:]

## Exercise 46

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List
	class Solution {
		static var original = #[0, 10, #[20, 30], 40, 50, #[60, 70, 80], #[90, 100, 110, 120]]

		static def flatten_list(list : List<?>) : List<?> {
			var result = newArrayList
			for (element : list) {
				if (element instanceof Iterable) {
					result.addAll(element)
				} else {
					result += element
				}
			}
			return result
		}

		static def main : void {
			println(flatten_list(original))
		}
	}
[:End:]


## Exercise 47

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	class Solution {
		static var original = #[0, 0, 1, 2, 3, 4, 4, 5, 6, 6, 6, 7, 8, 9, 4, 4]

		static def main : void {
			var result = newArrayList
			for (element : original) {
				if (result.isEmpty || element != result.get(result.size - 1)) {
					result += element
				}
			}
			println(result)
		}
	}
[:End:]

## Exercise 48

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List
	class Solution {
		static var original = #[0, 0, 1, 2, 3, 4, 4, 5, 6, 6, 6, 7, 8, 9, 4, 4]

		static def main : void {
			var result = newArrayList
			var pack : List<Integer> = null
			var prev : Integer = null
			for (element : original) {
				if (element != prev) {
					if (!pack.isNullOrEmpty) {
						result.add(pack)
					}
					pack = newArrayList
				}
				pack += element
				prev = element
			}
			if (!pack.isNullOrEmpty) {
				result.add(pack)
			}
			println(result)
		}
	}
[:End:]

## Exercise 49

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List
	class Solution {
		static var original = #[1, 1, 2, 3, 4, 4, 5, 1]

		static def insert_at(list : List<Integer>, position : int, element : Integer) : List<Integer> {
			var new_list = newArrayList
			new_list.addAll(list)
			if (position >= new_list.size) {
				new_list += element
			} else {
				new_list.add(position, element)
			}
			return new_list
		}

		static def main : void {
			println(insert_at(original, 3, 12))
		}
	}
[:End:]

## Exercise 50

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List
	import java.util.Random
	class Solution {
		static var original = #[1, 1, 2, 3, 4, 4, 5, 1]

		static def extract_random(list : List<Integer>, n : int) : List<Integer> {
			var new_list = newArrayList
			if (n <= list.size) {
				new_list.addAll(list)
			} else {
				var random = new Random
				var selected = newHashSet
				for (i : 1..n) {
					var rnd = random.nextInt(list.size)
					while (!selected.add(rnd)) {
						rnd = random.nextInt(list.size)
					}
					new_list.add(list.get(rnd))
				}
			}
			return new_list
		}

		static def main : void {
			println(extract_random(original, 3))
		}
	}
[:End:]

## Exercise 51

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	class Solution {
		static var original = #[1, 2, 3, 4, 5, 6, 7, 8, 9]

		static def main : void {
			var result = newArrayList
			for (var i = 0; i < original.size - 1; i++) {
				var elt0 = original.get(i)
				for (var j = i + 1; j < original.size; j++) {
					var elt1 = original.get(j)
					result.add(#[elt0, elt1])
				}
			}
			println(result)
		}
	}
[:End:]

## Exercise 52

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	class Solution1 {
		static var original = #[22.4, 4.0, -16.22, -9.1, 11.0, -12.22, 14.2, -5.2, 17.5]

		static def main : void {
			var sum = 0l
			for (element : original) {
				sum += Math::round(element)
			}
			println(sum * original.size)
		}
	}

	class Solution2 {
		static var original = #[22.4, 4.0, -16.22, -9.1, 11.0, -12.22, 14.2, -5.2, 17.5]

		static def main : void {
			var result = original.size * original.map[Math::floor(it)].reduce[accumulator, current| accumulator + current]
			println(result)
		}
	}
[:End:]

## Exercise 53

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List
	class Solution {
		static def create_multidimensional_list(a : int, b : int) : List<List<Integer>> {
			var list = newArrayList
			for (i : 1..a) {
				var sublist = newArrayList
				for (j : 1..b) {
					sublist += 0
				}
				list.add(sublist)
			}
			return list
		}

		static def main : void {
			println(create_multidimensional_list(3, 2))
		}
	}
[:End:]

## Exercise 54

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List
	class Solution {
		static def input_matrix(args : String[]) : List<List<Integer>> {
			var n = args.get(0) as int
			var m = newArrayList
			var k = 1
			for (i : 1..n) {
				var row = newArrayList
				for (j : 1..n) {
					row.add(args.get(k) as int)
					k++
				}
				m.add(row)
			}
			return m
		}

		static def sum_diag(matrix : List<List<Integer>>) : int {
			var sum = 0
			for (i : 0..<matrix.size) {
				sum += matrix.get(i).get(i)
			}
			return sum
		}

		static def main(args : String*) : void {
			var matrix = input_matrix(args)
			var result = sum_diag(matrix)
			println(result)
		}
	}
[:End:]

## Exercise 55

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List
	import java.util.Iterator
	class Solution {
		static var original1 = #[#[1, 3], #[5, 7], #[9, 11]]
		static var original2 = #[#[2, 4], #[6, 8], #[10, 12, 14]]

		static def zip(list1 : List<List<Integer>>, list2 : List<List<Integer>>) : List<List<Integer>> {
			var iter1 = list1.iterator
			var iter2 = list2.iterator
			var result = newArrayList
			while (iter1.hasNext && iter2.hasNext) {
				var sublist = newArrayList
				sublist.addAll(iter1.next)
				sublist.addAll(iter2.next)
				result.add(sublist)
			}
			if (iter1.hasNext) {
				do {
					result.add(iter1.next)
				} while (iter1.hasNext)
			} else if (iter2.hasNext) {
				do {
					result.add(iter2.next)
				} while (iter2.hasNext)
			}
			return result
		}

		static def main : void {
			var result = zip(original1, original2)
			println(result)
		}
	}
[:End:]

## Exercise 56

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List
	class Solution {
		static var original = #[1, 1, 3, 4, 4, 5, 6, 7]

		static def isseq(list : List<Integer>, i : int, n : int) : boolean {
			var elt = list.get(i)
			for (var j = i + 1; j < n; j++) {
				if (list.get(j) != elt) {
					return false
				}
			}
			return true
		}

		static def extract(list : List<Integer>, n : int) : List<Integer> {
			var result = newArrayList
			for (var i = 0; i <= list.size - n; i++) {
				if (isseq(list, i, n)) {
					result += list.get(i)
				}
			}
			return result
		}

		static def main : void {
			var result = extract(original, 2)
			println(result)
		}
	}
[:End:]

## Exercise 57

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List
	class Solution {
		static var original1 = #[1, 1, 3, 4, 4, 5, 6, 7]
		static var original2 = #[0, 1, 2, 3, 4, 4, 5, 7, 8]

		static def average(lists : List<Integer>*) : double {
			var sum = 0.0
			var count = 0
			for (list : lists) {
				count += list.size
				for (elt : list) {
					sum += elt
				}
			}
			return sum / count
		}

		static def main : void {
			var result = average(original1, original2)
			println(result)
		}
	}
[:End:]

## Exercise 58

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List
	import java.math.BigInteger
	class Solution1 {
		static var original = #[1, 'abcd', 3, 1.2, 4, 'xyz', 5, 'pqr', 7, -5, -12.22]

		static def isInteger(value : Object) : boolean {
			value instanceof Integer || value instanceof Long || value instanceof BigInteger
		}

		static def count_integers(list : List<?>) : int {
			var count = 0
			for (element : list) {
				if (element.isInteger) {
					count++
				}
			}
			return count
		}

		static def main : void {
			println(count_integers(original))
		}
	}

	class Solution2 {
		static var original = #[1, 'abcd', 3, 1.2, 4, 'xyz', 5, 'pqr', 7, -5, -12.22]

		static def count_integers(list : List<?>) : int {
			list.filter[it instanceof Integer || it instanceof Long || it instanceof BigInteger].size
		}

		static def main : void {
			println(count_integers(original))
		}
	}
[:End:]

## Exercise 59

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List
	class Solution {
		static var original = #[#[1, 2, 3], #[2, 4, 5], #[1, 1, 1]]

		static def remove_column(list : List<List<Integer>>, col : int) : List<List<Integer>> {
			var rows = newArrayList
			for (row : list) {
				var ncols = newArrayList
				ncols.addAll(row)
				ncols.remove(col)
				rows.add(ncols)
			}
			return rows
		}

		static def main : void {
			println(remove_column(original, 1))
		}
	}
[:End:]

## Exercise 60

[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:On]
	import java.util.List
	class Solution {
		static var original = #[#[1, 2, 3], #[2, 4, 5], #[1, 1, 1]]

		static def get_column(list : List<List<Integer>>, col : int) : List<Integer> {
			var column = newArrayList
			for (row : list) {
				column += row.get(col)
			}
			return column
		}

		static def main : void {
			println(get_column(original, 1))
		}
	}
[:End:]


[:Include:](../legal.inc)
