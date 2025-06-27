# Introduction to Map-based or Dictionary Data Structures with SARL

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

> **_Note 1:_** In SARL, a map is a data structure that is also known as dictionary in other programming languages. This type of data structure maps keys to values.

> **_Note 2:_** If you don't know how to solve an problem, or what is the function to be used, you could search on Internet for the answer using the API of the Java programming language. Indeed, since SARL is fully compatible with the Java API, you could use all the types or functions that are defined in this Java API.


## Exercise 1

* Write a SARL script to sort (ascending and descending) a map by value.

<a href="javascript:exerciseToggle(1)">Answer</a>
<div id="exercise1" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.List
	import java.util.Map
	import java.util.Map.Entry
	class Solution {
		static var original = #{1 -> 2, 3 -> 4, 4 -> 3, 2 -> 1, 0 -> 0}

		static def ascending_sort(map : Map<Integer, Integer>) : List<Entry<Integer, Integer>> {
			original.entrySet.sortWith[a, b | a.value <=> b.value]
		}

		static def descending_sort(map : Map<Integer, Integer>) : List<Entry<Integer, Integer>> {
			original.entrySet.sortWith[a, b | b.value <=> a.value]
		}

		static def main : void {
			println(ascending_sort(original))
			println(descending_sort(original))
		}
	}
[:End:]
</div>

## Exercise 2

* Write a SARL script to add a key to a map.
* Sample Map: `{0: 10, 1: 20}`
* Expected Result: `{0: 10, 1: 20, 2: 30}`

<a href="javascript:exerciseToggle(2)">Answer</a>
<div id="exercise2" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution {
		static var original = #{0 -> 10, 1 -> 20}

		static def main : void {
			original.put(2, 30)
			println(original)
		}
	}
[:End:]
</div>

## Exercise 3

* Write a SARL script to concatenate the following maps to create a new one.
* Sample Maps:

```text
dic1={1:10, 2:20}
dic2={3:30, 4:40}
dic3={5:50,6:60}
```

* Expected Result : `{1: 10, 2: 20, 3: 30, 4: 40, 5: 50, 6: 60}`

<a href="javascript:exerciseToggle(3)">Answer</a>
<div id="exercise3" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution1 {
		static var dic1 = #{1 -> 10, 2 -> 20}
		static var dic2 = #{3 -> 30, 4 -> 40}
		static var dic3 = #{5 -> 50, 6-> 60}

		static def main : void {
			var newmap = newHashMap
			for (e : dic1.entrySet) {
				newmap.put(e.key, e.value)
			}
			for (e : dic2.entrySet) {
				newmap.put(e.key, e.value)
			}
			for (e : dic3.entrySet) {
				newmap.put(e.key, e.value)
			}
			println(newmap)
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution2 {
		static var dic1 = #{1 -> 10, 2 -> 20}
		static var dic2 = #{3 -> 30, 4 -> 40}
		static var dic3 = #{5 -> 50, 6-> 60}

		static def main : void {
			var newmap = newHashMap
			newmap.putAll(dic1)
			newmap.putAll(dic2)
			newmap.putAll(dic3)
			println(newmap)
		}
	}
[:End:]
</div>


## Exercise 4

* Write a SARL script to check whether a given key already exists in a map.

<a href="javascript:exerciseToggle(4)">Answer</a>
<div id="exercise4" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution {
		static var dic = #{1 -> 10, 2 -> 20}
		static def main : void {
			println(dic.containsKey(1))
			println(dic.containsKey(5))
		}
	}
[:End:]
</div>

## Exercise 5

* Write a SARL program to iterate over maps using for loops.

<a href="javascript:exerciseToggle(5)">Answer</a>
<div id="exercise5" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution {
		static var dic = #{1 -> 10, 2 -> 20}

		static def main : void {
			for (e : dic.entrySet) {
				println(e.key + " -> " + e.value)
			}
		}
	}
[:End:]
</div>

## Exercise 6

* Write a SARL script to generate and print a map that contains a number (between 1 and n) in the form (x, x*x).
* n = 5
* Expected Output: `{1: 1, 2: 4, 3: 9, 4: 16, 5: 25}`

<a href="javascript:exerciseToggle(6)">Answer</a>
<div id="exercise6" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	class Solution1 {
		static def generate(n : int) : Map<Integer, Double> {
			var map = newHashMap
			for (i : 1..n) {
				map.put(i, i**2)
			}
			return map
		}

		static def main : void {
			println(generate(5))
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	class Solution2 {
		static def generate(n : int) : Map<Integer, Double> {
			(1..n).toInvertedMap[it**2]
		}

		static def main : void {
			println(generate(5))
		}
	}
[:End:]
</div>

## Exercise 7

* Write a SARL script to print a map where the keys are numbers between 1 and 15 (both included) and the values are the square of the keys.
* Sample map: `{1: 1, 2: 4, 3: 9, 4: 16, 5: 25, 6: 36, 7: 49, 8: 64, 9: 81, 10: 100, 11: 121, 12: 144, 13: 169, 14: 196, 15: 225}`

<a href="javascript:exerciseToggle(7)">Answer</a>
<div id="exercise7" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution {
		static def main : void {
			println((1..15).toInvertedMap[it**2 as int])
		}
	}
[:End:]
</div>

## Exercise 8

* Write a SARL script to merge two SARL maps.

<a href="javascript:exerciseToggle(8)">Answer</a>
<div id="exercise8" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	class Solution1 {
		static var original1 = #{1 -> 154, 2 -> 44, 3 -> 9}
		static var original2 = #{4 -> 16, 5 -> 25}

		static def merge(m1 : Map<? extends K, ? extends V>, m2 : Map<? extends K, ? extends V>) : Map<K, V> with K, V {
			var newmap = newHashMap
			newmap.putAll(m1)
			newmap.putAll(m2)
			return newmap
		}

		static def main : void {
			println(merge(original1, original2))
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	class Solution2 {
		static var original1 = #{1 -> 154, 2 -> 44, 3 -> 9}
		static var original2 = #{4 -> 16, 5 -> 25}

		static def main : void {
			println(original1.union(original2))
		}
	}
[:End:]
</div>

## Exercise 9

* Write a SARL program to sum all the values in a maps.

<a href="javascript:exerciseToggle(9)">Answer</a>
<div id="exercise9" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	class Solution1 {
		static var original = #{1 -> 154, 2 -> 44, 3 -> 9}

		static def sum(m : Map<Integer, Integer>) : int {
			var s = 0
			for (v : m.values) {
				s += v
			}
			return s
		}

		static def main : void {
			println(sum(original))
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	class Solution2 {
		static var original = #{1 -> 154, 2 -> 44, 3 -> 9}

		static def sum(m : Map<Integer, Integer>) : int {
			m.values.reduce[accumulator, current | accumulator + current]
		}

		static def main : void {
			println(sum(original))
		}
	}
[:End:]
</div>

## Exercise 10

* Write a SARL program to multiply all the values in a map.

<a href="javascript:exerciseToggle(10)">Answer</a>
<div id="exercise10" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	class Solution1 {
		static var original = #{1 -> 154, 2 -> 44, 3 -> 9}

		static def mul(m : Map<Integer, Integer>) : int {
			var s = 1
			for (v : m.values) {
				s *= v
			}
			return s
		}

		static def main : void {
			println(mul(original))
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	class Solution2 {
		static var original = #{1 -> 154, 2 -> 44, 3 -> 9}

		static def mul(m : Map<Integer, Integer>) : int {
			m.values.reduce[accumulator, current | accumulator * current]
		}

		static def main : void {
			println(mul(original))
		}
	}
[:End:]
</div>

## Exercise 11

* Write a SARL program to remove a key from a map.

<a href="javascript:exerciseToggle(11)">Answer</a>
<div id="exercise11" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	class Solution {
		static var original = #{1 -> 154, 2 -> 44, 3 -> 9}

		static def main : void {
			original.remove(2)
			println(original)
		}
	}
[:End:]
</div>

## Exercise 12

* Write a SARL program to map two lists into a map.

<a href="javascript:exerciseToggle(10)">Answer</a>
<div id="exercise10" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	class Solution1 {
		static var original1 = #[1, 2, 9]
		static var original2 = #[154, 44, 9]

		static def main : void {
			var map = newHashMap
			var iter1 = original1.iterator
			var iter2 = original2.iterator
			while (iter1.hasNext && iter2.hasNext) {
				map.put(iter1.next, iter2.next)
			}
			println(map)
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	class Solution2 {
		static var original1 = #[1, 2, 9]
		static var original2 = #[154, 44, 9]

		static def main : void {
			var iter1 = original1.iterator
			var map = original2.toMap[iter1.next]
			println(map)
		}
	}
[:End:]
</div>

## Exercise 13

* Write a SARL program to sort a given map by key.

<a href="javascript:exerciseToggle(13)">Answer</a>
<div id="exercise13" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	class Solution {
		static var original = #{1 -> 154, 3 -> 9, 2 -> 44}

		static def main : void {
			var sortedMap = newTreeSet(null)
			sortedMap.addAll(original)
			println(sortedMap)
		}
	}
[:End:]
</div>

## Exercise 14

* Write a SARL program to get the maximum and minimum values of a map.

<a href="javascript:exerciseToggle(14)">Answer</a>
<div id="exercise14" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution1 {
		static var original = #{1 -> 154, 3 -> 9, 2 -> 44}

		static def main : void {
			var min = Integer::MAX_VALUE
			var max = Integer::MIN_VALUE
			for (e : original.entrySet) {
				if (e.value < min) {
					min = e.value
				}
				if (e.value > max) {
					max = e.value
				}
			}
			println("Min = " + min)
			println("Max = " + max)
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution2 {
		static var original = #{1 -> 154, 3 -> 9, 2 -> 44}

		static def main : void {
			var min = original.values.min
			var max = original.values.max
			println("Min = " + min)
			println("Max = " + max)
		}
	}
[:End:]
</div>

## Exercise 15

* Write a SARL program to remove duplicated values from the map.

<a href="javascript:exerciseToggle(15)">Answer</a>
<div id="exercise15" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution {
		static var original = #{1 -> 154, 3 -> 9, 2 -> 44, 6 -> 9}

		static def main : void {
			var newmap = newHashMap
			for (e : original.entrySet) {
				if (!newmap.containsValue(e.value)) {
					newmap.put(e.key, e.value)
				}
			}
			println(newmap)
		}
	}
[:End:]
</div>

## Exercise 16

* Write a SARL program to check if a map is empty or not.

<a href="javascript:exerciseToggle(16)">Answer</a>
<div id="exercise16" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution1 {
		static var original = #{1 -> 154, 3 -> 9, 2 -> 44, 6 -> 9}

		static def main : void {
			println(original.isEmpty)
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution2 {
		static var original = #{1 -> 154, 3 -> 9, 2 -> 44, 6 -> 9}

		static def main : void {
			println(original.size == 0)
		}
	}
[:End:]
</div>

## Exercise 17

* Write a SARL program to combine two maps by adding values for common keys.
* Inputs:

```text
d1 = {'a': 100, 'b': 200, 'c':300}
d2 = {'a': 300, 'b': 200, 'd':400}
```

* Sample output: `{'a': 400, 'b': 400, 'd': 400, 'c': 300}`

<a href="javascript:exerciseToggle(13)">Answer</a>
<div id="exercise13" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution {
		static var d1 = #{'a' -> 100, 'b' -> 200, 'c' -> 300}
		static var d2 = #{'a' -> 300, 'b' -> 200, 'd' -> 400}

		static def main : void {
			var m = newHashMap
			for (e : d1.entrySet) {
				var v = d2.get(e.key)
				if (v === null) {
					m.put(e.key, e.value)
				} else {
					m.put(e.key, e.value + v)
				}
			}
			for (e : d2.entrySet) {
				m.putIfAbsent(e.key, e.value)
			}			
			println(m)
		}
	}
[:End:]
</div>

## Exercise 18

* Write a SARL program to print all distinct values in a map.
* Sample Data : `[{"V":"S001"}, {"V": "S002"}, {"VI": "S001"}, {"VI": "S005"}, {"VII":"S005"}, {"V":"S009"},{"VIII":"S007"}]`
* Expected Output : `{'S005', 'S002', 'S007', 'S001', 'S009'}`

<a href="javascript:exerciseToggle(18)">Answer</a>
<div id="exercise18" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution1 {
		static var original = #[
			#{"V" -> "S001"}, #{"V" -> "S002"},
			#{"VI" -> "S001"}, #{"VI" -> "S005"},
			#{"VII" -> "S005"}, #{"V" -> "S009"},
			#{"VIII" -> "S007"}
		]

		static def main : void {
			var m = newTreeSet(null)
			for (e : original) {
				m.addAll(e.values)
			}
			println(m)
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution2 {
		static var original = #[
			#{"V" -> "S001"}, #{"V" -> "S002"},
			#{"VI" -> "S001"}, #{"VI" -> "S005"},
			#{"VII" -> "S005"}, #{"V" -> "S009"},
			#{"VIII" -> "S007"}
		]

		static def main : void {
			var m = newTreeSet(null)
			original.forEach[m.addAll(it.values)]
			println(m)
		}
	}
[:End:]
</div>

## Exercise 19

* Write a SARL program to create and display all combinations of letters, selecting each letter from a different key in a map.
* Sample data: `{'1':['a','b'], '2':['c','d']}`
* Expected Output:

```text
ac
ad
bc
bd
```

<a href="javascript:exerciseToggle(19)">Answer</a>
<div id="exercise19" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution {
		static var original = #{'1' -> #['a','b'], '2' -> #['c','d']}

		static def main : void {
			var m = newArrayList
			var candidates = original.get(1)
			if (candidates.isNullOrEmpty) {
				m.addAll(candidates)
				var i = 2
				candidates = original.get(i)
				while (candidates !== null) {
					var m2 = newArrayList
					for (cand0 : m) {
						for (cand1 : candidates) {
							m2 += cand0 + cand1
						}
					}
					m = m2
					i++
					candidates = original.get(i)
				}
			}
			println(m)
		}
	}
[:End:]
</div>

## Exercise 20

* Write a SARL program to find the highest 3 values of corresponding keys in a map.

<a href="javascript:exerciseToggle(20)">Answer</a>
<div id="exercise20" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution1 {
		static var original = #{'A' -> 67, 'B' -> 23, 'C' -> 45,
                   'D' -> 56, 'E' -> 12, 'F' -> 69} 

		static def main : void {
			var iter = original.entrySet.iterator
			var en = iter.next
			var max0 = en.value
			var key0 = en.key
			var max1 : int
			var key1 : String
			var max2 : int
			var key2 : String
			while (iter.hasNext) {
				en = iter.next
				val m = en.value
				val k = en.key
				if (m > max0) {
					max2 = max1
					key2 = key1
					max1 = max0
					key1 = key0
					max0 = m
					key0 = k
				} else if (m > max1) {
					max2 = max1
					key2 = key1
					max1 = m
					key1 = k
				} else if (m > max0) {
					max2 = m
					key2 = k
				}
			}
			var newmap = #{key0 -> max0, key1 -> max1, key2 -> max2}
			println(newmap)
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution2 {
		static var original = #[
			#{"V" -> "S001"}, #{"V" -> "S002"},
			#{"VI" -> "S001"}, #{"VI" -> "S005"},
			#{"VII" -> "S005"}, #{"V" -> "S009"},
			#{"VIII" -> "S007"}
		]

		static def main : void {
			var m = newTreeSet(null)
			original.forEach[m.addAll(it.values)]
			println(m)
		}
	}
[:End:]
</div>

## Exercise 21

* Write a SARL program to combine values in a list of maps.
* Sample data: `[{'item': 'item1', 'amount': 400}, {'item': 'item2', 'amount': 300}, {'item': 'item1', 'amount': 750}]`
* Expected Output: `{'item1': 1150, 'item2': 300}`

<a href="javascript:exerciseToggle(21)">Answer</a>
<div id="exercise21" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution {
		static var original = #[
			#{'item' -> 'item1', 'amount' -> 400},
			#{'item' -> 'item2', 'amount' -> 300},
			#{'item' -> 'item1', 'amount' -> 750}
		] 

		static def main : void {
			var newmap = newHashMap
			for (m : original) {
				newmap.put(m.get("item"), m.get("amount"))
			}
			println(newmap)
		}
	}
[:End:]
</div>

## Exercise 22

* Write a SARL program to create a map from a string. Track the count of the letters from the string.
* Sample string: `w3resource`
* Expected output: `{'w': 1, '3': 1, 'r': 2, 'e': 2, 's': 1, 'o': 1, 'u': 1, 'c': 1}`

<a href="javascript:exerciseToggle(22)">Answer</a>
<div id="exercise22" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	class Solution {
		static def letters(value : String) : Map<Character, Integer> {
			var map = newHashMap
			for (b : value.bytes) {
				var c = b as char
				var n = map.getOrDefault(c, 0)
				map.put(c, n + 1)
			}
			return map
		}

		static def main : void {
			println(letters("w3resource"))
		}
	}
[:End:]
</div>

## Exercise 23

* Write a SARL program to print a map in table format.
* Sample Input:

```text
{1: ["Samuel", 21, 'Data Structures'],
 2: ["Richie", 20, 'Machine Learning'],
 3: ["Lauren", 21, 'OOPS with java'],
}
```

* Expected Output:

```text
Samuel	21	Data Structures
Richie	20	Machine Learning
Lauren	21	OOPS with java
```

<a href="javascript:exerciseToggle(23)">Answer</a>
<div id="exercise23" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution {
		static var original = #{
			1 -> #["Samuel", 21, 'Data Structures'],
 			2 -> #["Richie", 20, 'Machine Learning'],
 			3 -> #["Lauren", 21, 'OOPS with java']
		}
		
		static def main : void {
			var i = 1
			var row = original.get(i)
			while (row !== null) {
				if (!row.isEmpty) {
					var iter = row.iterator
					print(iter.next)
					while (iter.hasNext) {
						print(" " + iter.next)
					}
				}
				println
				i++
				row = original.get(i)
			}
		}
	}
[:End:]
</div>


## Exercise 24

* Write a SARL program to sort a list alphabetically in a map.
* Sample Input:  `{'n1': [2, 3, 1], 'n2': [5, 1, 2], 'n3': [3, 2, 4]}`
* Expected Output: `{'n1': [1, 2, 3], 'n2': [1, 2, 5], 'n3': [2, 3, 4]}`

<a href="javascript:exerciseToggle(24)">Answer</a>
<div id="exercise24" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution1 {
		static var original = #{'n1' -> #[2, 3, 1], 'n2' -> #[5, 1, 2], 'n3' -> #[3, 2, 4]}
		
		static def main : void {
			var newmap = newHashMap
			for (e : original.entrySet) {
				var nv = e.value.sort
				newmap.put(e.key, nv)
			}
			println(newmap)
		}
	}
[:End:]
</div>

## Exercise 25

* Write a SARL program to remove spaces from map keys.

<a href="javascript:exerciseToggle(25)">Answer</a>
<div id="exercise25" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution {
		static var original = #{'P 01' -> 'DBMS', 'P 02' -> 'OS', 'P 0 3 ' -> 'Soft Computing'}
		
		static def main : void {
			var newmap = newHashMap
			for (e : original.entrySet) {
				var nk = e.key.replaceAll("\\s+", "")
				newmap.put(nk, e.value)
			}
			println(newmap)
		}
	}
[:End:]
</div>

## Exercise 26

* Write a SARL program to get the top three items in a shop.
* Sample data: `{'item1': 45.50, 'item2':35, 'item3': 41.30, 'item4':55, 'item5': 24}`
* Expected Output:

```text
item4 55
item1 45.5
item3 41.3
```

<a href="javascript:exerciseToggle(26)">Answer</a>
<div id="exercise26" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution1 {
		static var original = #{'item1' -> 45.50, 'item2' -> 35.0, 'item3' -> 41.30, 'item4' -> 55.0, 'item5' -> 24.0}
		
		static def main : void {
			var iter = original.entrySet.iterator
			var en = iter.next
			var max0 = en.value
			var key0 = en.key
			var max1 : double
			var key1 : String
			var max2 : double
			var key2 : String
			while (iter.hasNext) {
				en = iter.next
				val m = en.value
				val k = en.key
				if (m > max0) {
					max2 = max1
					key2 = key1
					max1 = max0
					key1 = key0
					max0 = m
					key0 = k
				} else if (m > max1) {
					max2 = max1
					key2 = key1
					max1 = m
					key1 = k
				} else if (m > max0) {
					max2 = m
					key2 = k
				}
			}
			println(key0 + " " + max0)
			println(key1 + " " + max1)
			println(key2 + " " + max2)
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution2 {
		static var original = #{'item1' -> 45.50, 'item2' -> 35.0, 'item3' -> 41.30, 'item4' -> 55.0, 'item5' -> 24.0}
		
		static def main : void {
			var max0 = original.values.max
			var max1 = original.values.filter[it < max0].max
			var max2 = original.values.filter[it < max1].max
			var newmap = original.filter[k, v | v >= max2]
			for (e : newmap.entrySet) {
				println(e.key + " " + e.value)
			}
		}
	}
[:End:]
</div>

## Exercise 27

*Write a SARL program to get the key, value and item in a map.

<a href="javascript:exerciseToggle(27)">Answer</a>
<div id="exercise27" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution {
		static var original = #{1 -> 10, 2 -> 20, 3 -> 30, 4 -> 40, 5 -> 50, 6 -> 60}
		
		static def main : void {
			for (e : original.entrySet) {
				println(e.key + "\t" + e.value + "\t" + e)
			}
		}
	}
[:End:]
</div>

## Exercise 28

* Write a SARL program to print a map line by line.

<a href="javascript:exerciseToggle(28)">Answer</a>
<div id="exercise28" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution {
		static var original = #{1 -> 10, 2 -> 20, 3 -> 30, 4 -> 40, 5 -> 50, 6 -> 60}
		
		static def main : void {
			for (e : original.entrySet) {
				println(e.key + "\t" + e.value)
			}
		}
	}
[:End:]
</div>

## Exercise 29

* Write a SARL program to count the number of items in a map value that is a list.

<a href="javascript:exerciseToggle(29)">Answer</a>
<div id="exercise29" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution1 {
		static var original = #{
			'Alex' -> #['subj1', 'subj2', 'subj3'],
			'David' -> #['subj1', 'subj2']
		}
		
		static def main : void {
			var sum = 0
			for (v : original.values) {
				sum += v.size
			}
			println(sum)
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution2 {
		static var original = #{
			'Alex' -> #['subj1', 'subj2', 'subj3'],
			'David' -> #['subj1', 'subj2']
		}
		
		static def main : void {
			var sum = original.values.map[it.size].reduce(accumulator, current | accumulator + current)
			println(sum)
		}
	}
[:End:]
</div>

## Exercise 30

* Write a SARL program to sort items by value in reverse order.
* Sample data: `{'Math':81, 'Physics':83, 'Chemistry':87}`
* Expected data: `[('Chemistry', 87), ('Physics', 83), ('Math', 81)]`

<a href="javascript:exerciseToggle(30)">Answer</a>
<div id="exercise30" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution {
		static var original = #{
			'Math' -> 81, 'Physics' -> 83, 'Chemistry' -> 87
		}

		static def main : void {
			var result = original.entrySet.sortWith[a, b | b.value <=> a.value].map[#[it.key, it.value]].toList
			println(result)
		}
	}
[:End:]
</div>

## Exercise 31

* Write a SARL program to create a map from two lists without losing duplicate values.
* Sample lists: `['Class-V', 'Class-VI', 'Class-VII', 'Class-VIII']` and `[1, 2, 2, 3]`
* Expected Output: `{'Class-V': {1}, 'Class-VI': {2}, 'Class-VII': {2}, 'Class-VIII': {3}}`

<a href="javascript:exerciseToggle(31)">Answer</a>
<div id="exercise31" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution {
		static var original1 = #['Class-V', 'Class-VI', 'Class-VII', 'Class-VIII']
		static var original2 = #[1, 2, 2, 3]
		
		static def main : void {
			var iter1 = original1.iterator
			var iter2 = original2.iterator
			var newmap = newHashMap
			while (iter1.hasNext && iter2.hasNext) {
				newmap.put(iter1.next, iter2.next)
			}
			println(newmap)
		}
	}
[:End:]
</div>

## Exercise 32

* Write a SARL program to match key values in two dictionaries.
* Sample maps: `{'key1': 1, 'key2': 3, 'key3': 2}` and `{'key1': 1, 'key2': 2}`
* Expected output: `{'key1': 1}` is present in both input maps

<a href="javascript:exerciseToggle(32)">Answer</a>
<div id="exercise32" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution {
		static var original1 = #{'key1' -> 1, 'key2' -> 3, 'key3' -> 2}
		static var original2 = #{'key1' -> 1, 'key2' -> 2}
		
		static def main : void {
			var newmap = newHashMap
			for (e1 : original1.entrySet) {
				var v2 = original2.get(e1.key)
				if (v2 == e1.value) {
					newmap.put(e1.key, e1.value)
				}
			}
			println(newmap)
		}
	}
[:End:]
</div>


## Exercise 33

* Write a SARL program to store map data in a JSON file.
* Original map:

```json
{'students': [{'firstName': 'Nikki', 'lastName': 'Roysden'}, {'firstName': 'Mervin', 'lastName': 'Friedland'}, {'firstName': 'Aron ', 'lastName': 'Wilkins'}], 'teachers': [{'firstName': 'Amberly', 'lastName': 'Calico'}, {'firstName': 'Regine', 'lastName': 'Agtarap'}]}
```

* Json file:

```json
{'students': [{'firstName': 'Nikki', 'lastName': 'Roysden'}, {'firstName': 'Mervin', 'lastName': 'Friedland'}, {'firstName': 'Aron ', 'lastName': 'Wilkins'}], 'teachers': [{'firstName': 'Amberly', 'lastName': 'Calico'}, {'firstName': 'Regine', 'lastName': 'Agtarap'}]}
```

<a href="javascript:exerciseToggle(33)">Answer</a>
<div id="exercise33" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	class Solution {
		static var original = #{'students' -> #[
				#{'firstName' -> 'Nikki', 'lastName' -> 'Roysden'},
				#{'firstName' -> 'Mervin', 'lastName' -> 'Friedland'},
				#{'firstName' -> 'Aron ', 'lastName' -> 'Wilkins'}
			],
			'teachers' -> #[
				#{'firstName' -> 'Amberly', 'lastName' -> 'Calico'},
				#{'firstName' -> 'Regine', 'lastName' -> 'Agtarap'}
			]
		}

		static def toJson(data : Object, buffer : StringBuilder) {
			if (data instanceof Map) {
				buffer.append("{")
				var iter = (data as Map<String, ?>).entrySet.iterator
				if (iter.hasNext) {
					var me = iter.next
					buffer.append(me.key).append(":")
					toJson(me.value, buffer)
					while (iter.hasNext) {
						buffer.append(", ")
						me = iter.next
						buffer.append(me.key).append(":")
						toJson(me.value, buffer)
					}
				}
				buffer.append("}")
			} else if (data instanceof Iterable) {
				buffer.append("[")
				var iter = data.iterator
				if (iter.hasNext) {
					toJson(iter.next, buffer)
					while (iter.hasNext) {
						buffer.append(", ")
						toJson(iter.next, buffer)
					}
				}
				buffer.append("]")
			} else {
				buffer.append(data)
			}
		}
		
		static def main : void {
			var json = new StringBuilder
			original.toJson(json)
			println(json.toString)
		}
	}
[:End:]
</div>


## Exercise 34

* Write a SARL program to create a map of keys x, y, and z where each key has as value a list from 11-20, 21-30, and 31-40 respectively. Access the fifth value of each key from the map.

```text
{'x': [11, 12, 13, 14, 15, 16, 17, 18, 19, 20],
'y': [21, 22, 23, 24, 25, 26, 27, 28, 29, 30],
'z': [31, 32, 33, 34, 35, 36, 37, 38, 39, 40]}
15
25
35
x has value [11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
y has value [21, 22, 23, 24, 25, 26, 27, 28, 29, 30]
z has value [31, 32, 33, 34, 35, 36, 37, 38, 39, 40]
```

<a href="javascript:exerciseToggle(34)">Answer</a>
<div id="exercise34" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution {
		static def main : void {
			var map = #{
				"x" -> (11..20).toList,
				"y" -> (21..30).toList,
				"z" -> (31..40).toList
			}
			println(map)
			#["x", "y", "z"].forEach[
				println(map.get(it).get(4))
			]
			#["x", "y", "z"].forEach[
				println(it + " has the value " + map.get(it))
			]
		}
	}
[:End:]
</div>

## Exercise 35

* Write a SARL program to drop empty items from a given map.
* Original Map: `{'c1': 'Red', 'c2': 'Green', 'c3': null}`
* New map after dropping empty items: `{'c1': 'Red', 'c2': 'Green'}`

<a href="javascript:exerciseToggle(35)">Answer</a>
<div id="exercise35" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution {
		static var original = #{'c1' -> 'Red', 'c2' -> 'Green', 'c3' -> null}
		
		static def main : void {
			var newmap = original.filter[k, v | v.isNullOrEmpty]
			println(newmap)
		}
	}
[:End:]
</div>

## Exercise 36

* Write a SARL program to filter a map based on values.
* Original Map: `{'Cierra Vega': 175, 'Alden Cantrell': 180, 'Kierra Gentry': 165, 'Pierre Cox': 190}`
* Marks greater than 170: `{'Cierra Vega': 175, 'Alden Cantrell': 180, 'Pierre Cox': 190}`

<a href="javascript:exerciseToggle(36)">Answer</a>
<div id="exercise36" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution {
		static var original = #{'Cierra Vega' -> 175, 'Alden Cantrell' -> 180, 'Kierra Gentry' -> 165, 'Pierre Cox' -> 190}

		static def main : void {
			var newmap = original.filter[k, v | v > 170]
			println(newmap)
		}
	}
[:End:]
</div>

## Exercise 37

* Write a SARL program to convert more than one list to a nested map.
* Original strings: `['S001', 'S002', 'S003', 'S004']`, and `['Adina Park', 'Leyton Marsh', 'Duncan Boyle', 'Saim Richards']`, and `[85, 98, 89, 92]`
* Nested map: `[{'S001': {'Adina Park': 85}}, {'S002': {'Leyton Marsh': 98}}, {'S003': {'Duncan Boyle': 89}}, {'S004': {'Saim Richards': 92}}]`

<a href="javascript:exerciseToggle(37)">Answer</a>
<div id="exercise37" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution {
		static var original1 = #['S001', 'S002', 'S003', 'S004']
		static var original2 = #['Adina Park', 'Leyton Marsh', 'Duncan Boyle', 'Saim Richards']
		static var original3 = #[85, 98, 89, 92]

		static def main : void {
			var iter1 = original1.iterator
			var iter2 = original2.iterator
			var iter3 = original3.iterator
			var newmap = newHashMap
			while (iter1.hasNext && iter2.hasNext && iter3.hasNext) {
				var newmap1 = newHashMap
				newmap1.put(iter2.next, iter3.next)
				newmap.put(iter1.next, newmap1)
			}
			println(newmap)
		}
	}
[:End:]
</div>

## Exercise 38

* Write a SARL program to filter the height and width of students, which are stored in a map.
* Original Map: `{'Cierra Vega': (6.2, 70), 'Alden Cantrell': (5.9, 65), 'Kierra Gentry': (6.0, 68), 'Pierre Cox': (5.8, 66)}`
* Height > 6ft and Weight> 70kg: `{'Cierra Vega': (6.2, 70)}`

<a href="javascript:exerciseToggle(38)">Answer</a>
<div id="exercise38" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution {
		static var original = #{
			'Cierra Vega' -> #[6.2, 70],
			'Alden Cantrell' -> #[5.9, 65],
			'Kierra Gentry' -> #[6.0, 68],
			'Pierre Cox' -> #[5.8, 66]}

		static def main : void {
			val height = 6.0
			val weight = 70.0
			var newmap = original.filter[k, v | v.get(0) > height && v.get(1) > weight]
			println(newmap)
		}
	}
[:End:]
</div>

## Exercise 39

* Write a SARL program to verify that all values in a map are the same.
* Original Map: `{'Cierra Vega': 12, 'Alden Cantrell': 12, 'Kierra Gentry': 12, 'Pierre Cox': 12}`
* Check all are 12 in the map: `true`
* Check all are 10 in the map: `false`

<a href="javascript:exerciseToggle(39)">Answer</a>
<div id="exercise39" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	class Solution1 {
		static var original = #{
			'Cierra Vega' -> 12,
			'Alden Cantrell' -> 12,
			'Kierra Gentry' -> 12,
			'Pierre Cox' -> 12}

		static def check(map : Map<String, Integer>, n : int) : boolean {
			for (v : map.values) {
				if (v != n) {
					return false
				}
			}
			return true
		}

		static def main : void {
			println(original.check(12))
			println(original.check(10))
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	class Solution2 {
		static var original = #{
			'Cierra Vega' -> 12,
			'Alden Cantrell' -> 12,
			'Kierra Gentry' -> 12,
			'Pierre Cox' -> 12}

		static def check(map : Map<String, Integer>, n : int) : boolean {
			map.values.forall[it == n]
		}

		static def main : void {
			println(original.check(12))
			println(original.check(10))
		}
	}
[:End:]
</div>

## Exercise 40

* Write a SARL program to create a map  grouping a sequence of key-value pairs into a map of lists.
* Original list: `[('yellow', 1), ('blue', 2), ('yellow', 3), ('blue', 4), ('red', 1)]`
* Grouping a sequence of key-value pairs into a map of lists: `{'yellow': [1, 3], 'blue': [2, 4], 'red': [1]}`

<a href="javascript:exerciseToggle(40)">Answer</a>
<div id="exercise40" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution {
		static var original = #[
			#['yellow', 1], #['blue', 2], #['yellow', 3], #['blue', 4], #['red', 1]
		]

		static def main : void {
			var map = newHashMap
			for (e : original) {
				var list = map.computeIfAbsent(e.get(0)) [newArrayList]
				list += e.get(1)
			}
			println(map)
		}
	}
[:End:]
</div>

## Exercise 41

* Write a SARL program to split a given map of lists into lists of maps.
* Original map of lists: `{'Science': [88, 89, 62, 95], 'Language': [77, 78, 84, 80]}`
* Split said map of lists into list of dictionaries: `[{'Science': 88, 'Language': 77}, {'Science': 89, 'Language': 78}, {'Science': 62, 'Language': 84}, {'Science': 95, 'Language': 80}]`

<a href="javascript:exerciseToggle(41)">Answer</a>
<div id="exercise41" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	import java.util.List
	class Solution {
		static var original = #{'Science' -> #[88, 89, 62, 95], 'Language' -> #[77, 78, 84, 80]}

		static def list_of_dicts(marks : Map<String, List<Integer>>) : List<Map<String, Integer>> {
			var list = newArrayList
			for (value : marks.entrySet) {
				var i = 0
				for (value0 : value.value) {
					var m : Map<String, Integer>
					if (i >= list.size) {
						m = newHashMap
						list.add(m)
					} else {
						m = list.get(i)
					}
					m.put(value.key, value0)
					i++
				}
			}
		    return list
		}

   		static def main : void {
			println(list_of_dicts(original))
		}
	}
[:End:]
</div>

## Exercise 42

* Write a SARL program to remove a specified map from a given list.
* Original list of map: `[{'id': '#FF0000', 'color': 'Red'}, {'id': '#800000', 'color': 'Maroon'}, {'id': '#FFFF00', 'color': 'Yellow'}, {'id': '#808000', 'color': 'Olive'}]`
* Remove id `#FF0000` from the said list of map: `[{'id': '#800000', 'color': 'Maroon'}, {'id': '#FFFF00', 'color': 'Yellow'}, {'id': '#808000', 'color': 'Olive'}]`

<a href="javascript:exerciseToggle(42)">Answer</a>
<div id="exercise42" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	import java.util.List
	class Solution1 {
		static var original = #[
			#{'id' -> '#FF0000', 'color' -> 'Red'},
			#{'id' -> '#800000', 'color' -> 'Maroon'},
			#{'id' -> '#FFFF00', 'color' -> 'Yellow'},
			#{'id' -> '#808000', 'color' -> 'Olive'}
		]

		static def remove_color(colors : List<Map<String, String>>, id : String) : List<Map<String, String>> {
			var newlist = newArrayList
			for (c : colors) {
				if (c.get("id") != id) {
					newlist.add(c)
				}
			}
			return newlist
		}

   		static def main : void {
			println(remove_color(original, '#FF0000'))
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	import java.util.List
	class Solution2 {
		static var original = #[
			#{'id' -> '#FF0000', 'color' -> 'Red'},
			#{'id' -> '#800000', 'color' -> 'Maroon'},
			#{'id' -> '#FFFF00', 'color' -> 'Yellow'},
			#{'id' -> '#808000', 'color' -> 'Olive'}
		]

		static def remove_color(colors : List<Map<String, String>>, id : String) : List<Map<String, String>> {
			colors.filter[it.get("id") != id].toList
		}

   		static def main : void {
			println(remove_color(original, '#FF0000'))
		}
	}
[:End:]
</div>

## Exercise 43

* Write a SARL program to convert string values of a given map into integer/float datatypes.
* Original list: `[{'x': '10', 'y': '20', 'z': '30'}, {'p': '40', 'q': '50', 'r': '60'}]`
* String values of a given map, into integer types: `[{'x': 10, 'y': 20, 'z': 30}, {'p': 40, 'q': 50, 'r': 60}]`
* Original list: `[{'x': '10.12', 'y': '20.23', 'z': '30'}, {'p': '40.00', 'q': '50.19', 'r': '60.99'}]`
* String values of a given map, into float types: `[{'x': 10.12, 'y': 20.23, 'z': 30.0}, {'p': 40.0, 'q': 50.19, 'r': 60.99}]`

<a href="javascript:exerciseToggle(43)">Answer</a>
<div id="exercise43" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	import java.util.List
	class Solution1 {
		static var original = #[
			#{'x' -> '10', 'y' -> '20', 'z' -> '30'},
			#{'p' -> '40', 'q' -> '50', 'r' -> '60'}
		]

		static def isInteger(value : String) : boolean {
			try {
				Long::parseLong(value)
				return true
			} catch (ex : Throwable) {
				return false
			}
		}

		static def numbers(list : List<Map<String, String>>) : List<Map<String, Number>> {
			var newlist = newArrayList
			for (l : list) {
				var map = newHashMap
				for (e : l.entrySet) {
					var value = if (e.value.isInteger) (e.value as Integer) else (e.value as Float)
					map.put(e.key, value)
				}
			}
			return newlist
		}

   		static def main : void {
			println(numbers(original))
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	import java.util.List
	class Solution2 {
		static var original = #[
			#{'x' -> '10', 'y' -> '20', 'z' -> '30'},
			#{'p' -> '40', 'q' -> '50', 'r' -> '60'}
		]

		static def isInteger(value : String) : boolean {
			try {
				Long::parseLong(value)
				return true
			} catch (ex : Throwable) {
				return false
			}
		}

		static def numbers(list : List<Map<String, String>>) : List<Map<String, Number>> {
			list.map[
				it.mapValues[
					if (it.isInteger) (it as Integer) else (it as Float)
				]
			]
		}

   		static def main : void {
			println(numbers(original))
		}
	}
[:End:]
</div>

## Exercise 44

* A SARL map contains List as a value. Write a SARL program to clear the list values in the said map.
* Original Map: `{'C1': [10, 20, 30], 'C2': [20, 30, 40], 'C3': [12, 34]}`
* Clear the list values in the said map: `{'C1': [], 'C2': [], 'C3': []}`

<a href="javascript:exerciseToggle(44)">Answer</a>
<div id="exercise44" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	import java.util.List
	class Solution1 {
		static var original = #{'C1' -> #[10, 20, 30], 'C2' -> #[20, 30, 40], 'C3' -> #[12, 34]}

		static def clear(map : Map<String, List<Integer>>) : Map<String, List<Integer>> {
			var newmap = newHashMap
			for (e : map.entrySet) {
				newmap.put(e.key, <Integer>newArrayList)
			}
			return newmap
		}

   		static def main : void {
			println(clear(original))
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	import java.util.List
	class Solution2 {
		static var original = #{'C1' -> #[10, 20, 30], 'C2' -> #[20, 30, 40], 'C3' -> #[12, 34]}

		static def clear(map : Map<String, List<Integer>>) : Map<String, List<Integer>> {
			map.mapValues[newArrayList]
		}

   		static def main : void {
			println(clear(original))
		}
	}
[:End:]
</div>

## Exercise 45

* A SARL map contains List as a value. Write a SARL program to update the list values in the said map by adding 1 to the scores in Math and substracting 2 to the scores in Physics
* Original Map: `{'Math': [88, 89, 90], 'Physics': [92, 94, 89], 'Chemistry': [90, 87, 93]}`
* Update the list values of the said map: `{'Math': [89, 90, 91], 'Physics': [90, 92, 87], 'Chemistry': [90, 87, 93]}`

<a href="javascript:exerciseToggle(45)">Answer</a>
<div id="exercise45" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	import java.util.List
	class Solution1 {
		static var original = #{'Math' -> #[88, 89, 90], 'Physics' -> #[92, 94, 89], 'Chemistry' -> #[90, 87, 93]}

		static def update(map : Map<String, List<Integer>>, name : String, delta : int) : Map<String, List<Integer>> {
			var newmap = newHashMap
			for (e : map.entrySet) {
				if (e.key == name) {
					var ns = newArrayList
					for (s : e.value) {
						ns += s + delta
					}
					newmap.put(e.key, ns)
				} else {
					newmap.put(e.key, e.value)
				}
			}
			return newmap
		}

   		static def main : void {
   			var m = original.update("Math", 1)
   			m = m.update("Physics", -2)
			println(m)
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	import java.util.List
	class Solution2 {
		static var original = #{'Math' -> #[88, 89, 90], 'Physics' -> #[92, 94, 89], 'Chemistry' -> #[90, 87, 93]}

		static def update(map : Map<String, List<Integer>>, name : String, delta : int) : Map<String, List<Integer>> {
			map.entrySet.toMap(
				[it.key],
				[
					if (it.key == name) {
						it.value.map[it + delta]
					} else {
						it.value
					}
				]
			)
		}

   		static def main : void {
   			var m = original.update("Math", 1)
   			m = m.update("Physics", -2)
			println(m)
		}
	}
[:End:]
</div>

## Exercise 46

* Write a SARL program to extract a list of values from a given list of maps.
* Original Map: `[{'Math': 90, 'Science': 92}, {'Math': 89, 'Science': 94}, {'Math': 92, 'Science': 88}]`
* Extract a list of values from said list of maps where subject = Science: `[92, 94, 88]`
* Original Map: `[{'Math': 90, 'Science': 92}, {'Math': 89, 'Science': 94}, {'Math': 92, 'Science': 88}]`
* Extract a list of values from said list of maps where subject = Math: `[90, 89, 92]`

<a href="javascript:exerciseToggle(46)">Answer</a>
<div id="exercise46" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	import java.util.List
	class Solution1 {
		static var original = #[
			#{'Math' -> 90, 'Science' -> 92},
			#{'Math' -> 89, 'Science' -> 94},
			#{'Math' -> 92, 'Science' -> 88}
		]

		static def extract(list : List<Map<String, Integer>>, name : String) : List<Integer> {
			var scores = newArrayList
			for (map : list) {
				var score = map.get(name)
				if (score !== null) {
					scores += score
				}
			}
			return scores
		}

   		static def main : void {
   			println(original.extract("Science"))
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	import java.util.List
	class Solution2 {
		static var original = #[
			#{'Math' -> 90, 'Science' -> 92},
			#{'Math' -> 89, 'Science' -> 94},
			#{'Math' -> 92, 'Science' -> 88}
		]

		static def extract(list : List<Map<String, Integer>>, name : String) : List<Integer> {
			list.map[it.get(name)]
		}

   		static def main : void {
   			println(original.extract("Science"))
		}
	}
[:End:]
</div>

## Exercise 47

* Write a SARL program to find the length of a map of values.
* Original Map: `{1: 'red', 2: 'green', 3: 'black', 4: 'white', 5: 'black'}`
* Length of map values: `{'red': 3, 'green': 5, 'black': 5, 'white': 5}`
* Original Map: `{'1': 'Austin Little', '2': 'Natasha Howard', '3': 'Alfred Mullins', '4': 'Jamie Rowe'}`
* Length of map values: `{'Austin Little': 13, 'Natasha Howard': 14, 'Alfred Mullins': 14, 'Jamie Rowe': 10}`

<a href="javascript:exerciseToggle(47)">Answer</a>
<div id="exercise47" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.List
	import java.util.Map
	class Solution1 {
		static var original = #[
			#{'Math' -> 90, 'Science' -> 92},
			#{'Math' -> 89, 'Science' -> 94},
			#{'Math' -> 92, 'Science' -> 88}
		]

		static def extract(list : List<Map<String, Integer>>, name : String) : List<Integer> {
			var scores = newArrayList
			for (map : list) {
				var score = map.get(name)
				if (score !== null) {
					scores += score
				}
			}
			return scores
		}

   		static def main : void {
   			println(original.extract("Science"))
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.List
	import java.util.Map
	class Solution2 {
		static var original = #{1 -> 'red', 2 -> 'green', 3 -> 'black', 4 -> 'white', 5 -> 'black'}

		static def sizes(map : Map<Integer, String>) : Map<String, Integer> {
			map.values.toInvertedMap[it.length]
		}

   		static def main : void {
   			println(original.sizes)
		}
	}
[:End:]
</div>

## Exercise 48

* Write a SARL program to get the depth of a map.

<a href="javascript:exerciseToggle(48)">Answer</a>
<div id="exercise48" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	class Solution1 {
		static var original = #{'a' -> 1, 'b' -> #{'c' -> #{'d' -> #{}}}}

		static def depth(map : Object) : int {
			if (map instanceof Map) {
				var iter = map.values.iterator
				if (iter.hasNext) {
					var max = depth(iter.next)
					while (iter.hasNext) {
						var m = depth(iter.next)
						if (m > max) {
							max = m
						}
					}
					return max + 1
				}
			}
			return 0
		}

   		static def main : void {
   			println(original.depth)
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	class Solution2 {
		static var original = #{'a' -> 1, 'b' -> #{'c' -> #{'d' -> #{}}}}

		static def depth(map : Object) : int {
			if (map instanceof Map) {
				return map.values.map[depth(it)].max + 1
			}
			return 0
		}

   		static def main : void {
   			println(original.depth)
		}
	}
[:End:]
</div>

## Exercise 49

* Write a SARL program to access map key's element by index.
* Sample Input: `{'physics': 80, 'math': 90, 'chemistry': 86}`
* Expected Output:

```text
0 = physics
1 = math
2 = chemistry
```

<a href="javascript:exerciseToggle(49)">Answer</a>
<div id="exercise49" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution {
		static var original = #{'physics' -> 80, 'math' -> 90, 'chemistry' -> 86}

   		static def main : void {
   			for (i : 0..<3) {
	   			println(i + " = " + original.keySet.get(i))
   			}
		}
	}
[:End:]
</div>

## Exercise 50

* Write a SARL program to convert a map into a list of lists.
* Original Map: `{1: 'red', 2: 'green', 3: 'black', 4: 'white', 5: 'black'}`
* Convert the said map into a list of lists: `[[1, 'red'], [2, 'green'], [3, 'black'], [4, 'white'], [5, 'black']]`
* Original Map: `{'1': 'Austin Little', '2': 'Natasha Howard', '3': 'Alfred Mullins', '4': 'Jamie Rowe'}`
* Convert the said map into a list of lists: `[['1', 'Austin Little'], ['2', 'Natasha Howard'], ['3', 'Alfred Mullins'], ['4', 'Jamie Rowe']]`

<a href="javascript:exerciseToggle(50)">Answer</a>
<div id="exercise50" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	import java.util.List
	class Solution1 {
		static var original = #{1 -> 'red', 2 -> 'green', 3 -> 'black', 4 -> 'white', 5 -> 'black'}

		static def make_list(map : Map<Integer, String>) : List<List<Object>> {
			var list = newArrayList
			for (e : map.entrySet) {
				var tab = <Object>newArrayList
				tab += e.key
				tab += e.value
				list.add(tab)
			}
			return list
		}

   		static def main : void {
   			println(original.make_list)
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	import java.util.List
	class Solution2 {
		static var original = #{1 -> 'red', 2 -> 'green', 3 -> 'black', 4 -> 'white', 5 -> 'black'}

		static def make_list(map : Map<Integer, String>) : List<List<Object>> {
			map.entrySet.map[#[it.key, it.value] as List<Object>].toList
		}

   		static def main : void {
   			println(original.make_list)
		}
	}
[:End:]
</div>

## Exercise 51

* Write a SARL program to filter even numbers from a map of values.
* Original Map: `{'V': [1, 4, 6, 10], 'VI': [1, 4, 12], 'VII': [1, 3, 8]}`
* Filter even numbers from said map values: `{'V': [4, 6, 10], 'VI': [4, 12], 'VII': [8]}`
* Original Map: `{'V': [1, 3, 5], 'VI': [1, 5], 'VII': [2, 7, 9]}`
* Filter even numbers from said map values: `{'V': [], 'VI': [], 'VII': [2]}`

<a href="javascript:exerciseToggle(51)">Answer</a>
<div id="exercise51" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	import java.util.List
	class Solution1 {
		static var original = #{
			'V' -> #[1, 4, 6, 10],
			'VI' -> #[1, 4, 12],
			'VII' -> #[1, 3, 8]}

		static def even_numbers(map : Map<String, List<Integer>>) : Map<String, List<Integer>> {
			var newmap = newHashMap
			for (e : map.entrySet) {
				var newlist = newArrayList
				for (v : e.value) {
					if ((v % 2) == 0) {
						newlist += v
					}
				}
				newmap.put(e.key, newlist)
			}
			return newmap
		}

   		static def main : void {
   			println(original.even_numbers)
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	import java.util.List
	class Solution2 {
		static var original = #{
			'V' -> #[1, 4, 6, 10],
			'VI' -> #[1, 4, 12],
			'VII' -> #[1, 3, 8]}

		static def even_numbers(map : Map<String, List<Integer>>) : Map<String, List<Integer>> {
			map.mapValues[it.filter[(it % 2) == 0].toList]
		}

   		static def main : void {
   			println(original.even_numbers)
		}
	}
[:End:]
</div>

## Exercise 52

* Write a SARL program to get all combinations of key-value pairs in a given map.
* Original Map: `{'V': [1, 4, 6, 10], 'VI': [1, 4, 12], 'VII': [1, 3, 8]}`
* Combinations of key-value pairs of the said map: `[{'V': [1, 4, 6, 10], 'VI': [1, 4, 12]}, {'V': [1, 4, 6, 10], 'VII': [1, 3, 8]}, {'VI': [1, 4, 12], 'VII': [1, 3, 8]}]`
* Original Map: `{'V': [1, 3, 5], 'VI': [1, 5]}`
* Combinations of key-value pairs of the said map: `[{'V': [1, 3, 5], 'VI': [1, 5]}]`

<a href="javascript:exerciseToggle(52)">Answer</a>
<div id="exercise52" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	import java.util.List
	class Solution {
		static var original = #{
			'V' -> #[1, 4, 6, 10],
			'VI' -> #[1, 4, 12],
			'VII' -> #[1, 3, 8]}

		static def combinations(keys : List<String>) : List<List<String>> {
			var list = newArrayList()
			for (var i = 0; i < keys.size - 1; i++) {
				for (var j = i + 1; j < keys.size; j++) {
					list.add(#[keys.get(i), keys.get(j)])
				}
			}
			return list
		}

		static def combinations(map : Map<String, List<Integer>>) : List<Map<String, List<Integer>>> {
			var list = newArrayList
			for (keys : map.keySet.toList.combinations) {
				var newmap = newHashMap
				for (key : keys) {
					newmap.put(key, map.get(key))
				}
				list.add(newmap)
			}
			return list
		}

   		static def main : void {
   			println(original.combinations)
		}
	}
[:End:]
</div>

## Exercise 53

* Write a SARL program to find the specified number of maximum values in a given map.
* Original Map: `{'a': 5, 'b': 14, 'c': 32, 'd': 35, 'e': 24, 'f': 100, 'g': 57, 'h': 8, 'i': 100}`
* 1 maximum value(s) in the said map: `['f']`
* 2 maximum value(s) in the said map: `['f', 'i']`
* 5 maximum value(s) in the said map: `['f', 'i', 'g', 'd', 'c']`

<a href="javascript:exerciseToggle(53)">Answer</a>
<div id="exercise53" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	class Solution {
		static var original = #{
			'a' -> 5, 'b' -> 14, 'c' -> 32, 'd' -> 35, 'e' -> 24,
			'f' -> 100, 'g' -> 57, 'h' -> 8, 'i' -> 100}

   		static def max_values(map : Map<String, Integer>, n : int) : Iterable<String> {
   			map.entrySet.sortWith[a, b | b.value <=> a.value].map[it.key].take(n)
		}

   		static def main : void {
   			println(original.max_values(1))
   			println(original.max_values(2))
   			println(original.max_values(5))
		}
	}
[:End:]
</div>

## Exercise 54

* Write a SARL program to find the shortest list of values for the keys in a given map.
* Original Map: `{'V': [10, 12], 'VI': [10], 'VII': [10, 20, 30, 40], 'VIII': [20], 'IX': [10, 30, 50, 70], 'X': [80]}`
* Shortest list of values with the keys of the said map: `['VI', 'VIII', 'X']`

<a href="javascript:exerciseToggle(54)">Answer</a>
<div id="exercise54" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	import java.util.List
	class Solution {
		static var original = #{
			'V' -> #[10, 12], 'VI' -> #[10], 'VII' -> #[10, 20, 30, 40],
			'VIII' -> #[20], 'IX' -> #[10, 30, 50, 70], 'X' -> #[80]}

   		static def shorted_list(map : Map<String, List<Integer>>) : List<String> {
   			val min = map.values.map[it.size].min
   			map.entrySet.filter[it.value.size == min].map[it.key].toList
		}

   		static def main : void {
   			println(original.shorted_list)
		}
	}
[:End:]
</div>

## Exercise 55

* Write a SARL program to extract values from a given map and create a list of lists from those values.
* Original Map: `[{'student_id': 1, 'name': 'Jean Castro', 'class': 'V'}, {'student_id': 2, 'name': 'Lula Powell', 'class': 'V'}, {'student_id': 3, 'name': 'Brian Howell', 'class': 'VI'}, {'student_id': 4, 'name': 'Lynne Foster', 'class': 'VI'}, {'student_id': 5, 'name': 'Zachary Simon', 'class': 'VII'}]`
* Extract values from the said map and create a list of lists using those values:
  * For `['student_id', 'name', 'class']`: `[[1, 'Jean Castro', 'V'], [2, 'Lula Powell', 'V'], [3, 'Brian Howell', 'VI'], [4, 'Lynne Foster', 'VI'], [5, 'Zachary Simon', 'VII']]`
  * For `['student_id', 'name']`: `[[1, 'Jean Castro'], [2, 'Lula Powell'], [3, 'Brian Howell'], [4, 'Lynne Foster'], [5, 'Zachary Simon']]`
  * For `['name', 'class']`: `[['Jean Castro', 'V'], ['Lula Powell', 'V'], ['Brian Howell', 'VI'], ['Lynne Foster', 'VI'], ['Zachary Simon', 'VII']]`

<a href="javascript:exerciseToggle(55)">Answer</a>
<div id="exercise55" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	import java.util.List
	class Solution {
		static var original = #[
			#{'student_id' -> "1", 'name' -> 'Jean Castro', 'class' -> 'V'},
			#{'student_id' -> "2", 'name' -> 'Lula Powell', 'class' -> 'V'},
			#{'student_id' -> "3", 'name' -> 'Brian Howell', 'class' -> 'VI'},
			#{'student_id' -> "4", 'name' -> 'Lynne Foster', 'class' -> 'VI'},
			#{'student_id' -> "5", 'name' -> 'Zachary Simon', 'class' -> 'VII'}
		]

   		static def extract(list : List<Map<String, String>>, keys : String*) : List<List<String>> {
   			var result = newArrayList
   			for (person : list) {
   				var personList = newArrayList
   				for (k : keys) {
   					personList += person.get(k)
   				}
   				result.add(personList)
   			}
   			return result
		}

   		static def main : void {
   			println(original.extract("student_id", "name", "class"))
   			println(original.extract("student_id", "name"))
   			println(original.extract("name", "class"))
		}
	}
[:End:]
</div>

## Exercise 56

* Write a SARL program to convert a given list of lists to a map.
* Original list of lists: `[[1, 'Jean Castro', 'V'], [2, 'Lula Powell', 'V'], [3, 'Brian Howell', 'VI'], [4, 'Lynne Foster', 'VI'], [5, 'Zachary Simon', 'VII']]`
* Convert the said list of lists to a map: `{1: ['Jean Castro', 'V'], 2: ['Lula Powell', 'V'], 3: ['Brian Howell', 'VI'], 4: ['Lynne Foster', 'VI'], 5: ['Zachary Simon', 'VII']}`

<a href="javascript:exerciseToggle(56)">Answer</a>
<div id="exercise56" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution {
		static var original = #[
			#[1, 'Jean Castro', 'V'], #[2, 'Lula Powell', 'V'],
			#[3, 'Brian Howell', 'VI'], #[4, 'Lynne Foster', 'VI'],
			#[5, 'Zachary Simon', 'VII']
		]

   		static def main : void {
   			var result = original.toMap(
   				[it.get(0) as Integer],
   				[#[it.get(1) as String, it.get(2) as String]]
   			)
   			println(result)
		}
	}
[:End:]
</div>

## Exercise 57

* Write a SARL program that creates key-value list pairings within a map.
* Original map: `{1: ['Jean Castro'], 2: ['Lula Powell'], 3: ['Brian Howell'], 4: ['Lynne Foster'], 5: ['Zachary Simon']}`
* A key-value list pairings of the said map: `[{1: 'Jean Castro', 2: 'Lula Powell', 3: 'Brian Howell', 4: 'Lynne Foster', 5: 'Zachary Simon'}]`

<a href="javascript:exerciseToggle(57)">Answer</a>
<div id="exercise57" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution {
		static var original = #{1 -> #['Jean Castro'], 2 -> #['Lula Powell'],
			3 -> #['Brian Howell'], 4 -> #['Lynne Foster'],
			5 -> #['Zachary Simon']
		}

   		static def main : void {
   			var result = #[original.mapValues[it.get(0)]]
   			println(result)
		}
	}
[:End:]
</div>

## Exercise 58

* Write a SARL program to get the total length of all values in a given map with string values.
* Original map: `{'#FF0000': 'Red', '#800000': 'Maroon', '#FFFF00': 'Yellow', '#808000': 'Olive'}`
* Total length of all values of the said map with string values: `20`

<a href="javascript:exerciseToggle(58)">Answer</a>
<div id="exercise58" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution {
		static var original = #{'#FF0000' -> 'Red', '#800000' -> 'Maroon',
			'#FFFF00' -> 'Yellow', '#808000' -> 'Olive'}

   		static def main : void {
   			var result = original.values.map[it.length]
   				.reduce[accumulator, current | accumulator + current]
   			println(result)
		}
	}
[:End:]
</div>

## Exercise 59

* Write a SARL program to check if a specific key and a value exist in a map.
* Original Map: `[{'student_id': 1, 'name': 'Jean Castro', 'class': 'V'}, {'student_id': 2, 'name': 'Lula Powell', 'class': 'V'}, {'student_id': 3, 'name': 'Brian Howell', 'class': 'VI'}, {'student_id': 4, 'name': 'Lynne Foster', 'class': 'VI'}, {'student_id': 5, 'name': 'Zachary Simon', 'class': 'VII'}]`

<a href="javascript:exerciseToggle(59)">Answer</a>
<div id="exercise59" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	import java.util.List
	class Solution1 {
		static var original = #[
			#{'student_id' -> '1', 'name' -> 'Jean Castro', 'class' -> 'V'},
			#{'student_id' -> '2', 'name' -> 'Lula Powell', 'class' -> 'V'},
			#{'student_id' -> '3', 'name' -> 'Brian Howell', 'class' -> 'VI'},
			#{'student_id' -> '4', 'name' -> 'Lynne Foster', 'class' -> 'VI'},
			#{'student_id' -> '5', 'name' -> 'Zachary Simon', 'class' -> 'VII'}
		]

		static def check(persons : List<Map<String, String>>, key : String, value : String) : boolean {
			for (person : persons) {
				if (person.get(key) == value) {
					return true
				}
			}
			return false
		}

   		static def main : void {
   			println(original.check("name", "Lynne Foster"))
   			println(original.check("class", "VI"))
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	import java.util.Map
	import java.util.List
	class Solution2 {
		static var original = #[
			#{'student_id' -> '1', 'name' -> 'Jean Castro', 'class' -> 'V'},
			#{'student_id' -> '2', 'name' -> 'Lula Powell', 'class' -> 'V'},
			#{'student_id' -> '3', 'name' -> 'Brian Howell', 'class' -> 'VI'},
			#{'student_id' -> '4', 'name' -> 'Lynne Foster', 'class' -> 'VI'},
			#{'student_id' -> '5', 'name' -> 'Zachary Simon', 'class' -> 'VII'}
		]

		static def check(persons : List<Map<String, String>>, key : String, value : String) : boolean {
			persons.exists[it.get(key) == value]
		}

   		static def main : void {
   			println(original.check("name", "Lynne Foster"))
   			println(original.check("class", "VI"))
		}
	}
[:End:]
</div>

## Exercise 60

* Write a SARL program to invert a given map with non-unique hashable values.
* Sample Input: `{'Ora Mckinney': 8, 'Theodore Hollandl': 7, 'Mae Fleming': 7, 'Mathew Gilbert': 8, 'Ivan Little': 7}`
* Sample Output: `{8: ['Ora Mckinney', 'Mathew Gilbert'], 7: ['Theodore Hollandl', 'Mae Fleming', 'Ivan Little']}`

<a href="javascript:exerciseToggle(60)">Answer</a>
<div id="exercise60" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:OnHtml]
	class Solution {
		static var original = #{'Ora Mckinney' -> 8, 'Theodore Hollandl' -> 7,
			'Mae Fleming' -> 7, 'Mathew Gilbert' -> 8, 'Ivan Little' -> 7}

   		static def main : void {
   			var map = newHashMap
   			for (e : original.entrySet) {
   				var list = map.computeIfAbsent(e.value) [newArrayList]
   				list += e.value
   			}
   			println(map)
		}
	}
[:End:]
</div>

[:Include:](../../includes/legal.inc)
