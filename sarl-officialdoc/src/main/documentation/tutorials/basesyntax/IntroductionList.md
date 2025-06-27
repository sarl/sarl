# Introduction to List-based Data Structures with SARL

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

* Write a SARL program to sum all the items in a list.

<a href="javascript:exerciseToggle(1)">Answer</a>
<div id="exercise1" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
	import java.util.List
	class Solution2 {
		static def sum(list : List<? extends Number>) : Number {
			list.reduce[accumulator, current | accumulator.doubleValue + current.doubleValue]
		}
	}
[:End:]
</div>

## Exercise 2

* Write a SARL program to multiply all the items in a list.

<a href="javascript:exerciseToggle(2)">Answer</a>
<div id="exercise2" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
	import java.util.List	
	class Solution2 {
		static def mul(list : List<? extends Number>) : Number {
			list.reduce[accumulator, current | accumulator.doubleValue * current.doubleValue]
		}
	}
[:End:]
</div>

## Exercise 3

* Write a SARL program to get the largest number from a list.

<a href="javascript:exerciseToggle(3)">Answer</a>
<div id="exercise3" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
	import java.util.List
	class Solution2 {
		static def maxValue(list : List<? extends Number>) : Number {
			list.max[a, b | a <=> b]
		}
	}
[:End:]
</div>

## Exercise 4

* Write a SARL program to count the number of strings from a given list of strings. The string length is 2 or more and the first and last characters are the same.
* Sample List : `['abc', 'xyz', 'aba', '1221']`
* Expected Result : `2`

<a href="javascript:exerciseToggle(4)">Answer</a>
<div id="exercise4" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
	import java.util.List
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
</div>

## Exercise 5

* Write a SARL program to get a list, sorted in increasing order by the last element in each tuple from a given list of non-empty tuples.
* Sample List : `[(2, 5), (1, 2), (4, 4), (2, 3), (2, 1)]`
* Expected Result : `[(2, 1), (1, 2), (2, 3), (4, 4), (2, 5)]`

<a href="javascript:exerciseToggle(5)">Answer</a>
<div id="exercise5" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
</div>

## Exercise 6

* Write a SARL program to remove duplicates from a list.

<a href="javascript:exerciseToggle(6)">Answer</a>
<div id="exercise6" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
</div>

## Exercise 7

* Write a SARL program to check if a list is empty or not.

<a href="javascript:exerciseToggle(7)">Answer</a>
<div id="exercise7" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
	import java.util.List	
	class Solution {
		static def enquiry(list : List<?>) {
			list.size === 0
		}
	}
[:End:]
</div>

## Exercise 8

* Write a SARL program to clone or copy a list.

<a href="javascript:exerciseToggle(8)">Answer</a>
<div id="exercise8" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
	import java.util.List	
	class Solution {
		static def copy(list : List<?>) {
			var cp = newArrayList
			cp.addAll(list)
			return cp
		}
	}
[:End:]
</div>

## Exercise 9

* Write a SARL program to find the list of words that are longer than n from a given list of words.

<a href="javascript:exerciseToggle(9)">Answer</a>
<div id="exercise9" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
	import java.util.List
	class Solution2 {
		static def words(list : List<String>, n : int) {
			list.filter[it.length > n].toList
		}
	}
[:End:]
</div>

## Exercise 10

* Write a SARL function that takes two lists and returns `true` if they have at least one common member.

<a href="javascript:exerciseToggle(10)">Answer</a>
<div id="exercise10" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
	import java.util.List
	class Solution2 {
		static def words(list1 : List<String>, list2 : List<?>) {
			list1.exists[list2.contains(it)]
		}
	}
[:End:]
</div>

## Exercise 11

* Write a SARL program to print a specified list after removing the 0th, 4th and 5th elements.
* Sample List: `['Red', 'Green', 'White', 'Black', 'Pink', 'Yellow']`
* Expected Output : `['Green', 'White', 'Black']`

<a href="javascript:exerciseToggle(11)">Answer</a>
<div id="exercise11" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
</div>

## Exercise 12

* Write a SARL program to generate a 3*4*6 3D array whose each element is `*`.

<a href="javascript:exerciseToggle(12)">Answer</a>
<div id="exercise12" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
	import java.util.List
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
</div>

## Exercise 13

* Write a SARL program to print the numbers of a specified list after removing even numbers from it.

<a href="javascript:exerciseToggle(13)">Answer</a>
<div id="exercise13" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
	import java.util.List
	class Solution2 {
		static def remove_even(list : List<Integer>) : List<Integer> {
			list.filter[(it % 2) != 0].toList
		}
	}
[:End:]
</div>

## Exercise 14

* Write a SARL program to shuffle and print a specified list.

<a href="javascript:exerciseToggle(14)">Answer</a>
<div id="exercise14" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
	import static extension java.util.Collections.shuffle
	class Solution {

		static var original = #['Red', 'Green', 'White', 'Black', 'Pink', 'Yellow']

		static def main : void {
			original.shuffle
			println(original)
		}
	}
[:End:]
</div>

## Exercise 15

* Write a SARL program to generate all permutations of a list in SARL.  

<a href="javascript:exerciseToggle(15)">Answer</a>
<div id="exercise15" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
</div>

## Exercise 16

* Write a SARL program to calculate the difference between the two lists.  

<a href="javascript:exerciseToggle(16)">Answer</a>
<div id="exercise16" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
</div>

## Exercise 17

* Write a SARL program to access the index of a list.
* Sample Input: `[5, 15, 35, 8, 98]`
* Sample Output:

```text
0 5
1 15
2 35
3 8
4 98
```

<a href="javascript:exerciseToggle(17)">Answer</a>
<div id="exercise17" style="display:none;">
Three answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
	class Solution2 {
		static var original = #[5, 15, 35, 8, 98]

		static def main : void {
			for (var i = 0; i < original.size; i++) {
				println(i + " " + original.get(i))
			}
		}
	}
[:End:]
Answer #3 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
	class Solution3 {
		static var original = #[5, 15, 35, 8, 98]

		static def main : void {
			original.forEach [element, index |
				println(index + " " + element)
			]
		}
	}
[:End:]
</div>


## Exercise 18

* Write a SARL program to convert a list of characters into a string.  

<a href="javascript:exerciseToggle(18)">Answer</a>
<div id="exercise18" style="display:none;">
Three answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
	import java.util.List
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
[:End:]
Answer #3 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
	import java.util.List
	class Solution3 {
		static var original : List<Character> = #['h', 'e', 'l', 'l', 'o']

		static def main : void {
			var str = original.join("")
			println(str)
		}
	}
[:End:]
</div>

## Exercise 19

* Write a SARL program to find the index of an item in a specified list.  

<a href="javascript:exerciseToggle(19)">Answer</a>
<div id="exercise19" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
	import java.util.List
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
</div>


## Exercise 20

* Write a SARL program to flatten a shallow list.
* Sample Input: `[[4], [0, 658, 4, 6], [1, 2, 3]]`
* Expected Output: `[4, 0, 658, 4, 6, 1, 2, 3]`

<a href="javascript:exerciseToggle(20)">Answer</a>
<div id="exercise20" style="display:none;">
Three answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
	import java.util.List
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
[:End:]
Answer #3 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
	import java.util.List
	class Solution3 {
		static var original = #[#[4], #[0, 658, 4, 6], #[1, 2, 3]]

		static def main : void {
			println(original.flatten.toList)
		}
	}
[:End:]
</div>

## Exercise 21

* Write a SARL program to append a list to the second list.  

<a href="javascript:exerciseToggle(21)">Answer</a>
<div id="exercise21" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
	class Solution2 {
		static var original1 = #[1, 45, 8, 6, 1]
		static var original2 = #[457, -1, 5]

		static def main : void {
			original1 += original2
			println(original1)
		}
	}
[:End:]
</div>

## Exercise 22

* Write a SARL program to select an item randomly from a list.  

<a href="javascript:exerciseToggle(22)">Answer</a>
<div id="exercise22" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
</div>

## Exercise 23

* Write a SARL program to check whether two lists are circularly identical.

* Input 1:

```text
list1 = [10, 10, 0, 0, 10]
list2 = [10, 10, 10, 0, 0]
```

* Output 1: `true`
* Input 2:

```text
list1 = [10, 10, 0, 10, 0]
list2 = [10, 10, 10, 0, 0]
```

* Output 2: `false`

<a href="javascript:exerciseToggle(23)">Answer</a>
<div id="exercise23" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
</div>

## Exercise 24

* Write a SARL program to find the second smallest number in a list.  

<a href="javascript:exerciseToggle(24)">Answer</a>
<div id="exercise24" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
	class Solution2 {
		static var original = #[1, 45, -45, 2, 987]

		static def main : void {
			var smaller = original.min
			var secondSmaller = original.filter[it > smaller].min
			println(secondSmaller)
		}
	}
[:End:]
</div>

## Exercise 25

* Write a SARL program to get unique values from a list.  

<a href="javascript:exerciseToggle(25)">Answer</a>
<div id="exercise25" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
	import java.util.List
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
</div>

## Exercise 26

* Write a SARL program to get the frequency of elements in a list.  

<a href="javascript:exerciseToggle(26)">Answer</a>
<div id="exercise26" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
</div>

## Exercise 27

* Write a SARL program to check whether a list contains a sublist.  

<a href="javascript:exerciseToggle(27)">Answer</a>
<div id="exercise27" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
</div>

## Exercise 28

* Write a SARL program that uses the Sieve of Eratosthenes method to compute prime numbers up to a specified number.

> **_Note:_** In mathematics, the sieve of Eratosthenes, one of a number of prime number sieves, is a simple, ancient algorithm for finding all prime numbers up to any given limit.

<a href="javascript:exerciseToggle(28)">Answer</a>
<div id="exercise28" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
	import java.util.BitSet
	import java.util.List
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
</div>

## Exercise 29

* Write a SARL program to create a list by concatenating a given list with a range from 1 to n.  
* Sample list: `['p', 'q']`
* n = 5
* Sample Output: `['p1', 'q1', 'p2', 'q2', 'p3', 'q3', 'p4', 'q4', 'p5', 'q5']`

<a href="javascript:exerciseToggle(29)">Answer</a>
<div id="exercise29" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
</div>

## Exercise 30

* Write a SARL program to find common items in two lists.  

<a href="javascript:exerciseToggle(30)">Answer</a>
<div id="exercise30" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
</div>

## Exercise 31

* Write a SARL program to change the position of every n-th value to the (n+1)th in a list.  
* Sample list: `[0,1,2,3,4,5]`
* Expected Output: `[1, 0, 3, 2, 5, 4]`

<a href="javascript:exerciseToggle(31)">Answer</a>
<div id="exercise31" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
</div>

## Exercise 32

* Write a SARL program to convert a list of multiple integers into a single integer.  
* Sample list: `[11, 33, 50]`
* Expected Output: `113350`

<a href="javascript:exerciseToggle(32)">Answer</a>
<div id="exercise32" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
</div>

## Exercise 33

* Write a SARL program to split a list based on the first character of a word.
* Sample Input:

```text
['be', 'have', 'do', 'say', 'get', 'make', 'go', 'know', 'take', 'see', 'come', 'think',
 'look', 'want', 'give', 'use', 'find', 'tell', 'ask', 'work', 'seem', 'feel',
 'leave', 'call']
```

* Expected Output:

```text
{ 'a': ['ask'],
  'b': ['be'],
  'c': ['come', 'call'],
  'd': ['do'],
  'f': ['find', 'feel'],
  'g': ['get', 'go', 'give'],
  'h': ['have'],
  'k': ['know'],
  'l': ['look', 'leave'],
  'm': ['make'],
  's': ['say', 'see', 'seem'],
  't': ['take', 'think', 'tell'],
  'u': ['use'],
  'w': ['want', 'work']
}
```

<a href="javascript:exerciseToggle(33)">Answer</a>
<div id="exercise33" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
	import java.util.List
	import java.util.Map
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
</div>

## Exercise 34

* Write a SARL program to create multiple lists.  
* Sample Input: `1..10`
* Expected Output: `{'1': [], '8': [], '14': [], '5': [], '17': [], '9': [], '2': [], '7': [], '16': [], '19': [], '4': [], '18': 
[], '13': [], '3': [], '15': [], '11': [], '20': [], '6': [], '12': [], '10': []}`

<a href="javascript:exerciseToggle(34)">Answer</a>
<div id="exercise34" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
	class Solution1 {
		static def main : void {
			var result = newHashMap
			for (i : 1..10) {
				result.computeIfAbsent(i) [newArrayList]
			}
			println(result)
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
	class Solution2 {
		static def main : void {
			var result = (1..10).toInvertedMap[newArrayList]
			println(result)
		}
	}
[:End:]
</div>

## Exercise 35

* Write a SARL program to convert a pair of values into a sorted unique array.  
* Original List:  `[(1, 2), (3, 4), (1, 2), (5, 6), (7, 8), (1, 2), (3, 4), (3, 4), (7, 8), (9, 10)]`
* Sorted Unique Data: `[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]`

<a href="javascript:exerciseToggle(35)">Answer</a>
<div id="exercise35" style="display:none;">
Three answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
[:End:]
Answer #3 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
</div>

## Exercise 36

* Write a SARL program to select the odd items from a list.  

<a href="javascript:exerciseToggle(36)">Answer</a>
<div id="exercise36" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
	class Solution2 {
		static var original = #[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

		static def main : void {
			var result = original.filter[(it % 2) != 0].toList
			println(result)
		}
	}
[:End:]
</div>

## Exercise 37

* Write a SARL program to insert an element before each element of a list.  

<a href="javascript:exerciseToggle(37)">Answer</a>
<div id="exercise37" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
</div>

## Exercise 38

* Write a SARL program to print nested lists (each list on a new line) using the `println()` function.  
* Sample Input: `[['assign1', 'assign2'], ['final', 'assign4'], ['exam', 'study']]`
* Expected Output:

```text
assign1 assign2
final assign4
exam study
```

<a href="javascript:exerciseToggle(38)">Answer</a>
<div id="exercise38" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
</div>

## Exercise 39

* Write a SARL program to convert a list to a list of maps.  
* Sample lists: `["Black", "Red", "Maroon", "Yellow"], ["#000000", "#FF0000", "#800000", "#FFFF00"]`
* Expected Output: `[{'color_name': 'Black', 'color_code': '#000000'}, {'color_name': 'Red', 'color_code': '#FF0000'}, {'color_name': 'Maroon', 'color_code': '#800000'}, {'color_name': 'Yellow', 'color_code': '#FFFF00'}]`

<a href="javascript:exerciseToggle(39)">Answer</a>
<div id="exercise39" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
</div>

## Exercise 40

* Write a SARL program to split a list every Nth element.
* Sample list: `['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n']`
* n = 5
* Expected Output: `[['a', 'd', 'g', 'j', 'm'], ['b', 'e', 'h', 'k', 'n'], ['c', 'f', 'i', 'l']]`

<a href="javascript:exerciseToggle(40)">Answer</a>
<div id="exercise40" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
</div>

## Exercise 41

* Write a SARL program to compute the difference between two lists.  
* Sample data: `a = ["red", "orange", "green", "blue", "white"]`, `b = ["black", "yellow", "green", "blue"]`
* Expected Output when `a` is passed before `b`: `['white', 'orange', 'red']`
* Expected Output when `b` is passed before `a`: `['black', 'yellow']`

<a href="javascript:exerciseToggle(41)">Answer</a>
<div id="exercise41" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
</div>

## Exercise 42

* Write a SARL program to replace the last element in a list with another list.  
* Sample data: `[1, 3, 5, 7, 9, 10]` and `[2, 4, 6, 8]`
* Expected Output: `[1, 3, 5, 7, 9, 2, 4, 6, 8]`

<a href="javascript:exerciseToggle(42)">Answer</a>
<div id="exercise42" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
</div>

## Exercise 43

* Write a SARL program to check whether the n-th element exists in a given list.  

<a href="javascript:exerciseToggle(43)">Answer</a>
<div id="exercise43" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
</div>

## Exercise 44

* Write a SARL program to insert a given string at the beginning of all items in a list.  
* Sample list: `[1,2,3,4]`
* Input string: `emp`
* Expected output: `['emp1', 'emp2', 'emp3', 'emp4']`

<a href="javascript:exerciseToggle(44)">Answer</a>
<div id="exercise44" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
	import java.util.List
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
</div>

## Exercise 45

* Write a SARL program to iterate over two lists simultaneously.  

<a href="javascript:exerciseToggle(45)">Answer</a>
<div id="exercise45" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
	import java.util.List
	import java.util.Iterator
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
</div>

## Exercise 46

* Write a SARL program to flatten a given nested list structure.  
* Original list: `[0, 10, [20, 30], 40, 50, [60, 70, 80], [90, 100, 110, 120]]`
* Flatten list: `[0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120]`

<a href="javascript:exerciseToggle(46)">Answer</a>
<div id="exercise46" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
</div>

## Exercise 47

* Write a SARL program to remove consecutive (following each other continuously) duplicates (elements) from a given list.  
* Original list: `[0, 0, 1, 2, 3, 4, 4, 5, 6, 6, 6, 7, 8, 9, 4, 4]`
* After removing consecutive duplicates: `[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 4]`

<a href="javascript:exerciseToggle(47)">Answer</a>
<div id="exercise47" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
</div>

## Exercise 48

* Write a SARL program to pack consecutive duplicates of a given list of elements into sublists.  
* Original list: `[0, 0, 1, 2, 3, 4, 4, 5, 6, 6, 6, 7, 8, 9, 4, 4]`
* After packing consecutive duplicates of the said list elements into sublists: `[[0, 0], [1], [2], [3], [4, 4], [5], [6, 6, 6], [7], [8], [9], [4, 4]]`

<a href="javascript:exerciseToggle(48)">Answer</a>
<div id="exercise48" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
</div>

## Exercise 49

* Write a SARL program to insert an element at a specified position into a given list.  
* Original list: `[1, 1, 2, 3, 4, 4, 5, 1]`
* After inserting an element at kth position in the said list: `[1, 1, 12, 2, 3, 4, 4, 5, 1]`

<a href="javascript:exerciseToggle(48)">Answer</a>
<div id="exercise49" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
</div>

## Exercise 50

* Write a SARL program to extract a given number of randomly selected elements from a given list.  
* Original list: `[1, 1, 2, 3, 4, 4, 5, 1]`
* Selected 3 random numbers of the above list: `[4, 4, 1]`

<a href="javascript:exerciseToggle(50)">Answer</a>
<div id="exercise50" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
</div>

## Exercise 51

* Write a SARL program to generate combinations of n distinct objects taken from the elements of a given list.
* Original list: `[1, 2, 3, 4, 5, 6, 7, 8, 9]`
* Combinations of 2 distinct objects: `[1, 2] [1, 3] [1, 4] [1, 5] .... [7, 8] [7, 9] [8, 9]`

<a href="javascript:exerciseToggle(51)">Answer</a>
<div id="exercise51" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
</div>


## Exercise 52

* Write a SARL program to round every number in a given list of numbers and print the total sum multiplied by the length of the list.  
* Original list: `[22.4, 4.0, -16.22, -9.1, 11.0, -12.22, 14.2, -5.2, 17.5]`
* Result: `243`

<a href="javascript:exerciseToggle(52)">Answer</a>
<div id="exercise52" style="display:none;">
Two answers are posible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
	class Solution2 {
		static var original = #[22.4, 4.0, -16.22, -9.1, 11.0, -12.22, 14.2, -5.2, 17.5]

		static def main : void {
			var result = original.size * original.map[Math::floor(it)].reduce[accumulator, current| accumulator + current]
			println(result)
		}
	}
[:End:]
</div>

## Exercise 53

* Write a SARL program to create a multidimensional list (lists of lists) with zeros. Multidimensional list: `[[0, 0], [0, 0], [0, 0]]`.

<a href="javascript:exerciseToggle(53)">Answer</a>
<div id="exercise53" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
</div>

## Exercise 54

* Write a SARL program to read a square matrix from the command line and print the sum of the matrix's primary diagonal. Accept the size of the square matrix and elements for each column separated with a space (for every row) as input from the user.  
* Input the size of the matrix: 3

```text
2 3 4
4 5 6
3 4 7
```

* Sum of matrix primary diagonal: `14`

<a href="javascript:exerciseToggle(54)">Answer</a>
<div id="exercise54" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
</div>

## Exercise 55

* Write a SARL program to Zip two given lists of lists.  
* Original lists:

```text
[[1, 3], [5, 7], [9, 11]]
[[2, 4], [6, 8], [10, 12, 14]]
```

* Zipped list: `[[1, 3, 2, 4], [5, 7, 6, 8], [9, 11, 10, 12, 14]]`

<a href="javascript:exerciseToggle(55)">Answer</a>
<div id="exercise55" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
</div>

## Exercise 56

* Write a SARL program to extract specified number of elements from a given list, which follows each other continuously.  
* Original list: `[1, 1, 3, 4, 4, 5, 6, 7]`
* Extract 2 number of elements from the said list which follows each other continuously: `[1, 4]`
* Original list: `[0, 1, 2, 3, 4, 4, 4, 4, 5, 7]`
* Extract 4 number of elements from the said list which follows each other continuously: `[4]`

<a href="javascript:exerciseToggle(56)">Answer</a>
<div id="exercise56" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
</div>

## Exercise 57

* Write a SARL program to compute average of two given lists.  
* Original list:

```text
[1, 1, 3, 4, 4, 5, 6, 7]
[0, 1, 2, 3, 4, 4, 5, 7, 8]
```

* Average of two lists: `3.823529411764706`

<a href="javascript:exerciseToggle(57)">Answer</a>
<div id="exercise57" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
</div>


## Exercise 58

* Write a SARL program to count integers in a given mixed list.  
* Original list: `[1, 'abcd', 3, 1.2, 4, 'xyz', 5, 'pqr', 7, -5, -12.22]`
* Number of integers in the said mixed list: 6

<a href="javascript:exerciseToggle(58)">Answer</a>
<div id="exercise58" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
[:End:]
Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
	import java.util.List
	import java.math.BigInteger
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
</div>

## Exercise 59

* Write a SARL program to remove a specified column from a given nested list.  
* Original Nested list: `[[1, 2, 3], [2, 4, 5], [1, 1, 1]]`
* After removing 1st column: `[[2, 3], [4, 5], [1, 1]]`
* Original Nested list: `[[1, 2, 3], [-2, 4, -5], [1, -1, 1]]`
* After removing 3rd column: `[[1, 2], [-2, 4], [1, -1]]`

<a href="javascript:exerciseToggle(59)">Answer</a>
<div id="exercise59" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
</div>

## Exercise 60

* Write a SARL program to extract a specified column from a given nested list.  
* Original Nested list: `[[1, 2, 3], [2, 4, 5], [1, 1, 1]]`
* Extract 1st column: `[1, 2, 1]`
* Original Nested list: `[[1, 2, 3], [-2, 4, -5], [1, -1, 1]]`
* Extract 3rd column: `[3, -5, 1]`

<a href="javascript:exerciseToggle(60)">Answer</a>
<div id="exercise60" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.listexercises
	[:OnHtml]
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
</div>


[:Include:](../../includes/legal.inc)
