# Introduction to Maps with SARL - Answers

[:Outline:]

## Exercise 1

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 2

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
	class Solution {
		static var original = #{0 -> 10, 1 -> 20}

		static def main : void {
			original.put(2, 30)
			println(original)
		}
	}
[:End:]


## Exercise 3

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 4

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
	class Solution {
		static var dic = #{1 -> 10, 2 -> 20}
		static def main : void {
			println(dic.containsKey(1))
			println(dic.containsKey(5))
		}
	}
[:End:]


## Exercise 5

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
	class Solution {
		static var dic = #{1 -> 10, 2 -> 20}

		static def main : void {
			for (e : dic.entrySet) {
				println(e.key + " -> " + e.value)
			}
		}
	}
[:End:]


## Exercise 6

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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

	class Solution2 {
		static def generate(n : int) : Map<Integer, Double> {
			(1..n).toInvertedMap[it**2]
		}

		static def main : void {
			println(generate(5))
		}
	}
[:End:]


## Exercise 7

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
	class Solution {
		static def main : void {
			println((1..15).toInvertedMap[it**2 as int])
		}
	}
[:End:]


## Exercise 8

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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

	class Solution2 {
		static var original1 = #{1 -> 154, 2 -> 44, 3 -> 9}
		static var original2 = #{4 -> 16, 5 -> 25}

		static def main : void {
			println(original1.union(original2))
		}
	}
[:End:]


## Exercise 9

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 10

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 11

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
	import java.util.Map
	class Solution {
		static var original = #{1 -> 154, 2 -> 44, 3 -> 9}

		static def main : void {
			original.remove(2)
			println(original)
		}
	}
[:End:]


## Exercise 12

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 13

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 14

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 15

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 16

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
	class Solution1 {
		static var original = #{1 -> 154, 3 -> 9, 2 -> 44, 6 -> 9}

		static def main : void {
			println(original.isEmpty)
		}
	}

	class Solution2 {
		static var original = #{1 -> 154, 3 -> 9, 2 -> 44, 6 -> 9}

		static def main : void {
			println(original.size == 0)
		}
	}
[:End:]


## Exercise 17

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 18

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 19

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 20

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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

	class Solution2 {
		static var original = #{'A' -> 67, 'B' -> 23, 'C' -> 45,
                   'D' -> 56, 'E' -> 12, 'F' -> 69} 

		static def main : void {
			var max0 = original.values.max
			var max1 = original.values.filter[it < max0].max
			var max2 = original.values.filter[it < max1].max
			var newmap = original.filter[k, v | v >= max2]
			println(newmap)
		}
	}
[:End:]


## Exercise 21

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 22

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 23

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 24

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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

	class Solution2 {
		static var original = #{'n1' -> #[2, 3, 1], 'n2' -> #[5, 1, 2], 'n3' -> #[3, 2, 4]}
		
		static def main : void {
			var newmap = original.mapValues[it.sort]
			println(newmap)
		}
	}
[:End:]


## Exercise 25

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 26

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 27

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
	class Solution {
		static var original = #{1 -> 10, 2 -> 20, 3 -> 30, 4 -> 40, 5 -> 50, 6 -> 60}
		
		static def main : void {
			for (e : original.entrySet) {
				println(e.key + "\t" + e.value + "\t" + e)
			}
		}
	}
[:End:]


## Exercise 28

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
	class Solution {
		static var original = #{1 -> 10, 2 -> 20, 3 -> 30, 4 -> 40, 5 -> 50, 6 -> 60}
		
		static def main : void {
			for (e : original.entrySet) {
				println(e.key + "\t" + e.value)
			}
		}
	}
[:End:]


## Exercise 29

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 30

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 31

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 32

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 33

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 34

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 35

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
	class Solution {
		static var original = #{'c1' -> 'Red', 'c2' -> 'Green', 'c3' -> null}
		
		static def main : void {
			var newmap = original.filter[k, v | v.isNullOrEmpty]
			println(newmap)
		}
	}
[:End:]


## Exercise 36

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
	class Solution {
		static var original = #{'Cierra Vega' -> 175, 'Alden Cantrell' -> 180, 'Kierra Gentry' -> 165, 'Pierre Cox' -> 190}

		static def main : void {
			var newmap = original.filter[k, v | v > 170]
			println(newmap)
		}
	}
[:End:]


## Exercise 37

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 38

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 39

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 40

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 41

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 42

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 43

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 44

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 45

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 46

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 47

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 48

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 49

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
	class Solution {
		static var original = #{'physics' -> 80, 'math' -> 90, 'chemistry' -> 86}

   		static def main : void {
   			for (i : 0..<3) {
	   			println(i + " = " + original.keySet.get(i))
   			}
		}
	}
[:End:]


## Exercise 50

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 51

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 52

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 53

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 54

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 55

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 56

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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

## Exercise 57

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 58

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 59

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


## Exercise 60

[:Success:]
	package io.sarl.docs.tutorials.mapexercises
	[:On]
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


[:Include:](../legal.inc)
