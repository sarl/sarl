# Introduction to Strings of Characters with SARL

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

* Write a SARL program to calculate the length of a string, providing on the command line.

<a href="javascript:exerciseToggle(1)">Answer</a>
<div id="exercise1" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution {
		static def main(args : String*) : void {
			println(args.get(0).length)
		}
	}
[:End:]
</div>

## Exercise 2

* Write a SARL program to count the number of characters (character frequency) in a string.
* Sample String: `google.com`
* Expected Result: `{'g': 2, 'o': 3, 'l': 1, 'e': 1, '.': 1, 'c': 1, 'm': 1}`

<a href="javascript:exerciseToggle(2)">Answer</a>
<div id="exercise2" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution {
		static def main : void {
			var map = newHashMap
			for (b : "google.com".bytes) {
				var c = b as char
				var n = map.getOrDefault(c, 0)
				map.put(c, n + 1)
			}
			println(map)
		}
	}
[:End:]
</div>

## Exercise 3

* Write a SARL program to get a string made of the first 2 and last 2 characters of a given string (from the command line). If the string length is less than 2, return the empty string instead.
* Sample String: `w3resource`
* Expected Result: `w3ce`
* Sample String: `w3`
* Expected Result: `w3w3`
* Sample String: ` w`
* Expected Result: Empty String

<a href="javascript:exerciseToggle(3)">Answer</a>
<div id="exercise3" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution {
		static def main(args : String*) : void {
			var input = args.get(0)
			var output = ""
			if (input.length >= 2) {
				output = input.substring(0, 2) + input.substring(input.length - 2, input.length)
			}
			println(output)
		}
	}
[:End:]
</div>

## Exercise 4

* Write a SARL program to get a string from a given string (from the command line) where all occurrences of its first char have been changed to `*`, except the first char itself.
* Sample String: `restart`
* Expected Result: `resta*t`

<a href="javascript:exerciseToggle(4)">Answer</a>
<div id="exercise4" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution {
		static def main(args : String*) : void {
			var input = args.get(0)
			var output = ""
			if (input.length >= 1) {
				var firstChar = input.charAt(0)
				output = firstChar + input.substring(1, input.length).replace(firstChar, '*')
			}
			println(output)
		}
	}
[:End:]
</div>

## Exercise 5

* Write a SARL program to get a single string from two given strings (from the command line), separated by a space and swap the first two characters of each string.
* Sample String: `abc`, `xyz`
* Expected Result: `xyc abz`

<a href="javascript:exerciseToggle(5)">Answer</a>
<div id="exercise5" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution {
		static def start(value : String) : String {
			if (value.length >= 2) value.substring(0, 2) else value
		}
		
		static def end(value : String) : String {
			if (value.length >= 2) value.substring(2, value.length) else ""
		}

		static def main(args : String*) : void {
			if (args.length >= 2) {
				var in0 = args.get(0)
				var in1 = args.get(1)
				var output = start(in1) + end(in0) + start(in0) + end(in1)
				println(output)
			}
		}
	}
[:End:]
</div>

## Exercise 6

* Write a SARL program to add `ing` at the end of a given string (length should be at least 3). If the given string already ends with `ing`, add `ly` instead. If the string length of the given string is less than 3, leave it unchanged.
* Sample String: `abc`
* Expected Result: `abcing`
* Sample String: `string`
* Expected Result: `stringly`

<a href="javascript:exerciseToggle(6)">Answer</a>
<div id="exercise6" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution {
		static def add_ing(value : String) : String {
			if (value.length >= 3) {
				if (value.endsWith("ing")) value + "ly" else value + "ing"
			} else {
				value
			}
		}

		static def main(args : String*) : void {
			for (value : args) {
				println(value.add_ing)
			}
		}
	}
[:End:]
</div>

## Exercise 7

* Write a SARL program to find the first occurrence of the substrings `not` and `poor` in a given string. If `not` follows `poor`, replace the whole `not`...`poor` substring with `good`. Return the resulting string.
* Sample String: `The lyrics is not that poor!` and `The lyrics is poor!`
* Expected Result: `The lyrics is good!` and `The lyrics is poor!`

<a href="javascript:exerciseToggle(7)">Answer</a>
<div id="exercise7" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	import java.util.regex.Pattern
	class Solution {
		static def main(args : String*) : void {
			var pattern = Pattern::compile("not.+?poor") 
			for (value : args) {
				var matcher = pattern.matcher(value)
				var nvalue = matcher.replaceAll("good")
				println(nvalue)
			}
		}
	}
[:End:]
</div>

## Exercise 8

* Write a SARL function that takes a list of words and return the longest word and the length of the longest one.

<a href="javascript:exerciseToggle(8)">Answer</a>
<div id="exercise8" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution1 {
		static def longuest_word(words : String[]) : Object[] {
			if (words.length > 0) {
				var wd = words.get(0)
				var max = wd.length
				for (var i = 1; i < words.length; i++) {
					var w = words.get(i)
					if (w.length > max) {
						max = w.length
						wd = w
					}
				}
				return #[wd, max]
			}
			return null
		}
		static def main(args : String*) : void {
			println(args.longuest_word)
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution2 {
		static def longuest_word(words : String[]) : Object[] {
			words.sortWith[a, b | b.length <=> a.length].map[#[it, it.length]].get(0)
		}
		static def main(args : String*) : void {
			println(args.longuest_word)
		}
	}
[:End:]
</div>

## Exercise 9

* Write a SARL program to remove the nth index character from a nonempty string.

<a href="javascript:exerciseToggle(9)">Answer</a>
<div id="exercise9" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution {
		static def remove_char(word : String, index : int) : String {
			word.substring(0, index) + word.substring(index + 1, word.length)
		}
		static def main(args : String*) : void {
			for (arg : args) {
				println(arg.remove_char(arg.length / 2))
			}
		}
	}
[:End:]
</div>

## Exercise 10

* Write a SARL program to change a given string to a newly string where the first and last chars have been exchanged.

<a href="javascript:exerciseToggle(10)">Answer</a>
<div id="exercise10" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution {
		static def swap_bounds(word : String) : String {
			if (word.length >= 2) {
				var firstChar = word.charAt(0)
				var lastChar = word.charAt(word.length - 1)
				if (word.length > 2) {
					lastChar + word.substring(1, word.length - 2) + firstChar
				} else {
					(lastChar + firstChar) as String
				}
			} else {
				word
			}
		}
		static def main(args : String*) : void {
			for (arg : args) {
				println(arg.swap_bounds)
			}
		}
	}
[:End:]
</div>

## Exercise 11

* Write a SARL program to remove characters that have odd index values in a given string.

<a href="javascript:exerciseToggle(11)">Answer</a>
<div id="exercise11" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution1 {
		static def remove_odd_chars(word : String) : String {
			var str = ""
			for (var i = 0; i < word.length; i += 2) {
				str += word.charAt(i)
			}
			return str
		}
		static def main(args : String*) : void {
			for (arg : args) {
				println(arg.remove_odd_chars)
			}
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution2 {
		static def remove_odd_chars(word : String) : String {
			var str = new StringBuilder
			for (var i = 0; i < word.length; i += 2) {
				str.append(word.charAt(i))
			}
			return str.toString
		}
		static def main(args : String*) : void {
			for (arg : args) {
				println(arg.remove_odd_chars)
			}
		}
	}
[:End:]
</div>

## Exercise 12

* Write a SARL program to count the occurrences of each word in a given sentence.

<a href="javascript:exerciseToggle(12)">Answer</a>
<div id="exercise12" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	import java.util.Map
	class Solution {
		static def count_words(sentence : String) : Map<String, Integer> {
			var map = newHashMap
			for (word : sentence.split("\\s+")) {
				var n = map.getOrDefault(word, 0)
				map.put(word, n + 1)
			}
			return map
		}
		static def main(args : String*) : void {
			for (arg : args) {
				println(arg.count_words)
			}
		}
	}
[:End:]
</div>

## Exercise 13

* Write a SARL script that takes input from the command line and displays that input back in upper and lower cases.

<a href="javascript:exerciseToggle(13)">Answer</a>
<div id="exercise13" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution {
		static def main(args : String*) : void {
			for (arg : args) {
				println(arg.toUpperCase)
				println(arg.toLowerCase)
			}
		}
	}
[:End:]
</div>

## Exercise 14

* Write a SARL program that accepts a comma-separated sequence of words as command line input and prints the distinct words in sorted form (alphanumerically).
* Sample Words: `red, white, black, red, green, black`
* Expected Result: `black`, `green`, `red`, `white`, `red`

<a href="javascript:exerciseToggle(14)">Answer</a>
<div id="exercise14" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	import java.util.List
	class Solution {
		static def main(args : String*) : void {
			var words = args.map[newArrayList(it.split("\\s*,\\s*"))]
				.reduce[accumulator : List<String>, current : List<String> |
					(accumulator + current) as List<String>
				]
			println(words)
		}
	}
[:End:]
</div>

## Exercise 15

* Write a SARL function to create an HTML string with tags around the word(s).
* Sample function and result:

* `['i', 'SARL']` gives `<i>SARL</i>`
* `['b', 'SARL Tutorial']` gives `<b>SARL Tutorial</b>`

<a href="javascript:exerciseToggle(15)">Answer</a>
<div id="exercise15" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	import java.util.List
	class Solution1 {
		static def to_html(words : List<String>) : String {
			var str = ""
			var action : String = null
			for (w : words) {
				if (action !== null) {
					str += "<" + action + ">" + w + "</" + action + ">" 
				} else if (w == "b" || w == "i") {
					action = w
				} else {
					str += w
				}
			}
			return str
		}
		static def main(args : String*) : void {
			println(args.to_html)
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	import java.util.List
	class Solution2 {
		static def to_html(words : List<String>) : String {
			var str = new StringBuilder
			var action : String = null
			for (w : words) {
				if (action !== null) {
					str.append("<").append(action).append(">")
					str.append(w).append("</").append(action).append(">") 
				} else if (w == "b" || w == "i") {
					action = w
				} else {
					str.append(w)
				}
			}
			return str.toString
		}
		static def main(args : String*) : void {
			println(args.to_html)
		}
	}
[:End:]
</div>

## Exercise 16

* Write a SARL function to insert a string in the middle of a string.
Sample function and result :
* arguments `('[[]]', 'SARL')` gives `[[SARL]]`
* arguments `('{{}}', 'PHP')` gives `{{PHP}}`

<a href="javascript:exerciseToggle(16)">Answer</a>
<div id="exercise16" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution {
		static def insert_middle(str : String, inside : String) : String {
			var m = str.length / 2
			str.substring(0, m) + inside + str.substring(m, str.length)
		}
		static def main : void {
			println("{{}}".insert_middle("SARL"))
			println("[[]]".insert_middle("SARL"))
		}
	}
[:End:]
</div>

## Exercise 17

* Write a SARL function to get a string made of 4 copies of the last two characters of a specified string (length must be at least 2).
Sample function and result :
* `SARL` gives `RLRLRL`
* `Exercises` -> `eseseses`

<a href="javascript:exerciseToggle(17)">Answer</a>
<div id="exercise17" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution1 {
		static def duplicate_last(str : String) : String {
			if (str.length >= 2) {
				var substr = str.substring(str.length - 2, str.length)
				return substr + substr + substr + substr
			}
			return str
		}
		static def main : void {
			println("SARL".duplicate_last)
			println("Exercices".duplicate_last)
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution2 {
		static def operator_power(value : String, n : int) : String {
			var buffer = new StringBuilder
			for (i : 1..n) {
				buffer.append(value)
			}
			return buffer.toString
		}

		static def duplicate_last(str : String) : String {
			if (str.length >= 2) {
				return str.substring(str.length - 2, str.length) ** 4
			}
			return str
		}
		static def main : void {
			println("SARL".duplicate_last)
			println("Exercices".duplicate_last)
		}
	}
[:End:]
</div>

## Exercise 18

* Write a SARL function to get a string made of the first three characters of a specified string. If the length of the string is less than 3, return the original string.
* `ipy` gives `ipy`
* `SARL` gives `SAR`

<a href="javascript:exerciseToggle(18)">Answer</a>
<div id="exercise18" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution {
		static def begin(str : String) : String {
			if (str.length <= 3) str else str.substring(0, 3)
		}
		static def main : void {
			println("SARL".begin)
			println("Exercices".begin)
		}
	}
[:End:]
</div>

## Exercise 19

* Write a SARL function to reverse a string if its length is a multiple of 4.

<a href="javascript:exerciseToggle(19)">Answer</a>
<div id="exercise19" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution {
		static def reverse(str : String) : String {
			if ((str.length%4) != 0) str else new String(str.bytes.reverse)
		}
		static def main : void {
			println("SARL".reverse)
			println("Exercices".reverse)
		}
	}
[:End:]
</div>

## Exercise 20

* Write a SARL function to convert a given string to all uppercase if it contains at least 2 uppercase characters in the first 4 characters.

<a href="javascript:exerciseToggle(20)">Answer</a>
<div id="exercise20" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution {
		static def selective_uppercase(str : String) : String {
			var num_upper = 0 str.bytes.take(4).filter[Character::isUpperCase(it)].size
			if (num_upper >= 2) str.toUpperCase else str
		}
		static def main : void {
			println("SARL".selective_uppercase)
			println("Exercices".selective_uppercase)
		}
	}
[:End:]
</div>

## Exercise 21

* Write a SARL program to sort a string lexicographically.

<a href="javascript:exerciseToggle(21)">Answer</a>
<div id="exercise21" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution {
		static def sort(str : String) : String {
			new String(str.toCharArray.sort)
		}
		static def main : void {
			println("SARL".sort)
			println("Exercices".sort)
		}
	}
[:End:]
</div>

## Exercise 22

* Write a SARL program to remove a newline in a string.

<a href="javascript:exerciseToggle(22)">Answer</a>
<div id="exercise22" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution {
		static def remove_nl(str : String) : String {
			str.replaceAll("[\n\r]+", "")
		}
		static def main : void {
			println("SARL".remove_nl)
			println("Exercices".remove_nl)
		}
	}
[:End:]
</div>

## Exercise 23

* Write a SARL program to check whether a string starts with specified characters.

<a href="javascript:exerciseToggle(23)">Answer</a>
<div id="exercise23" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution {
		static def starts_with(str : String, prefix : String) : boolean {
			if (prefix.length > str.length) {
				return false
			}
			for (var i = 0; i < prefix.length; i++) {
				if (str.charAt(i) != prefix.charAt(i)) {
					return false
				}
			}
			return true
		}
		static def main : void {
			println("SARL".starts_with("SA"))
			println("SARL".starts_with("Sa"))
		}
	}
[:End:]
</div>

## Exercise 24

* Write a SARL program to create a Caesar encryption.

> **_Note:_** In cryptography, a Caesar cipher, also known as Caesar's cipher, the shift cipher, Caesar's code or Caesar shift, is one of the simplest and most widely known encryption techniques. It is a type of substitution cipher in which each letter in the plaintext is replaced by a letter some fixed number of positions down the alphabet. For example, with a left shift of 3, D would be replaced by A, E would become B, and so on. The method is named after Julius Caesar, who used it in his private correspondence.

<a href="javascript:exerciseToggle(24)">Answer</a>
<div id="exercise24" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution1 {
		static def caesarCipher(str : String, delta : int) : String {
			var tab = newCharArrayOfSize(str.length)
			for (var i = 0; i < str.length; i++) {
				tab.set(i, (tab.get(i)+ delta) as char)
			}
			new String(tab)
		}
		static def main : void {
			println("SARL".caesarCipher(-3))
			println("SARL".caesarCipher(4))
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution2 {
		static def caesarCipher(str : String, delta : int) : String {
			new String(str.toCharArray.map[(it + delta) as char])
		}
		static def main : void {
			println("SARL".caesarCipher(-3))
			println("SARL".caesarCipher(4))
		}
	}
[:End:]
</div>

## Exercise 25

* Write a SARL program to remove existing indentation from all of the lines in a given text.

<a href="javascript:exerciseToggle(25)">Answer</a>
<div id="exercise25" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution1 {
		static def remove_indent(str : String) : String {
			var out = new StringBuilder
			var line_start = true
			for (c : str.toCharArray) {
				if (line_start) {
					if (!Character::isWhitespace(c)) {
						line_start = false
						out.append(c)
					}
				} else if (c == "\n" || c == "\r") {
					line_start = true
				} else {
					out.append(c)
				}
			}
			return out.toString
		}
		static def main : void {
			println("SARL\n\tline2\n    line3".remove_indent)
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution2 {
		static def remove_indent(str : String) : String {
			str.replaceFirst("^\\s+", "").replaceAll("[\n\r]\\s*", "\n")
		}
		static def main : void {
			println("SARL\n\tline2\n    line3".remove_indent)
		}
	}
[:End:]
</div>

## Exercise 26

* Write a SARL program to add prefix text to all of the lines in a string.

<a href="javascript:exerciseToggle(26)">Answer</a>
<div id="exercise26" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution1 {
		static def add_prefix(str : String, prefix : String) : String {
			var out = new StringBuilder
			var line_start = true
			out.append(prefix)
			for (c : str.toCharArray) {
				if (line_start) {
					if (!Character::isWhitespace(c)) {
						line_start = false
						out.append(c)
					}
				} else if (c == "\n" || c == "\r") {
					line_start = true
					out.append(prefix)
				} else {
					out.append(c)
				}
			}
			return out.toString
		}
		static def main : void {
			println("SARL\n\tline2\n    line3".add_prefix("---"))
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution2 {
		static def add_prefix(str : String, prefix : String) : String {
			str.replaceFirst("^\\s+", prefix).replaceAll("[\n\r]\\s*", "\n" + prefix)
		}
		static def main : void {
			println("SARL\n\tline2\n    line3".add_prefix("---"))
		}
	}
[:End:]
</div>

## Exercise 27

* Write a SARL program to print the real numbers up to 2 decimal places.

<a href="javascript:exerciseToggle(27)">Answer</a>
<div id="exercise27" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	import java.text.MessageFormat
	import static extension java.lang.Math.abs
	class Solution1 {
		static def format_number(value : double) : String {
			var str = new StringBuilder
			var lvalue = (value as long).abs
			str.append(lvalue)
			var dec = value.abs - lvalue
			if (dec != 0.0) {
				for (i : 1..2) {
					dec *= 10
					str.append(dec as int)
				}
			}
			return str.toString
		}
		static def main : void {
			println(123.456789.format_number)
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	import java.text.MessageFormat
	class Solution2 {
		static def format_number(value : double) : String {
			MessageFormat::format("#0.00", value)
		}
		static def main : void {
			println(123.456789.format_number)
		}
	}
[:End:]
</div>

## Exercise 28

* Write a SARL program to print the real numbers up to 2 decimal places with a sign.

<a href="javascript:exerciseToggle(28)">Answer</a>
<div id="exercise28" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	import java.text.MessageFormat
	class Solution1 {
		static def format_number(value : double) : String {
			var str = new StringBuilder
			var lvalue = value as long
			str.append(lvalue)
			var dec = value - lvalue
			if (dec != 0.0) {
				for (i : 1..2) {
					dec *= 10
					str.append(dec as int)
				}
			}
			return str.toString
		}
		static def main : void {
			println(123.456789.format_number)
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	import java.text.MessageFormat
	class Solution2 {
		static def format_number(value : double) : String {
			MessageFormat::format("+#0.00", value)
		}
		static def main : void {
			println(123.456789.format_number)
		}
	}
[:End:]
</div>

## Exercise 29

* Write a SARL program to print the real positive and negative numbers with no decimal places.

<a href="javascript:exerciseToggle(29)">Answer</a>
<div id="exercise29" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	import java.text.MessageFormat
	class Solution1 {
		static def format_number(value : double) : String {
			var str = new StringBuilder
			str.append(value as long)
			return str.toString
		}
		static def main : void {
			println(123.456789.format_number)
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	import java.text.MessageFormat
	class Solution2 {
		static def format_number(value : double) : String {
			MessageFormat::format("+#0", value)
		}
		static def main : void {
			println(123.456789.format_number)
		}
	}
[:End:]
</div>

## Exercise 30

* Write a SARL program to print the integers with zeros to the left of the specified width.

<a href="javascript:exerciseToggle(30)">Answer</a>
<div id="exercise30" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution1 {
		static def format_number(value : int, width : int) : String {
			var buffer = new StringBuilder
			buffer.append(value)
			while (buffer.length < width) {
				buffer.insert(0, '0')
			}
			return buffer.toString
		}
		static def main : void {
			println(123.format_number(10))
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	class Solution2 {
		static def format_number(value : int, width : int) : String {
			String::format("%0" + width + "d", value)
		}
		static def main : void {
			println(123.format_number(10))
		}
	}
[:End:]
</div>

## Exercise 31

* Write a SARL program to print the integers with '*' to the right of the specified width.

<a href="javascript:exerciseToggle(31)">Answer</a>
<div id="exercise31" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution {
		static def format_number(value : int, width : int, leadingchar : char = '*') : String {
			var buffer = new StringBuilder
			buffer.append(value)
			while (buffer.length < width) {
				buffer.insert(0, leadingchar)
			}
			return buffer.toString
		}
		static def main : void {
			println(123.format_number(10))
		}
	}
[:End:]
</div>

## Exercise 32

* Write a SARL program to count occurrences of a substring in a string, both provided on the command line.

<a href="javascript:exerciseToggle(32)">Answer</a>
<div id="exercise32" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution {
		static def count_sub(str : String, sub : String) : int {
			if (sub.length == 0 || sub.length > str.length) {
				return 0
			}
			var sub0 = sub.charAt(0)
			var count = 0
			for (var i = 0; i < str.length - sub.length; i++) {
				var base = str.charAt(i)
				if (base == sub0) {
					var found = true
					for (var j = i + 1, var k = 1; k < sub.length; j++, k++) {
						var c0 = str.charAt(j)
						var c1 = sub.charAt(k)
						if (c0 != c1) {
							found = false
							break
						}
					}
					if (found) {
						count++
					}
				}
			}
			return count
		}
		static def main(args : String*) : void {
			println(count_sub(args.get(0), args.get(1)))
		}
	}
[:End:]
</div>

## Exercise 33

* Write a SARL program to reverse a string that is provided on the command line.

<a href="javascript:exerciseToggle(33)">Answer</a>
<div id="exercise33" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution1 {
		static def reverse(str : String) : String {
			var chars = newCharArrayOfSize(str.length)
			for (var i = 0, var j = str.length - 1; i < str.length; i++, j--) {
				chars.set(i, str.charAt(j))
			}
			return new String(chars)
		}
		static def main(args : String*) : void {
			for (arg : args) {
				println(arg.reverse)
			}
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution2 {
		static def reverse(str : String) : String {
			new String(str.toCharArray.reverseView)
		}
		static def main(args : String*) : void {
			for (arg : args) {
				println(arg.reverse)
			}
		}
	}
[:End:]
</div>

## Exercise 34

* Write a SARL program to reverse words in a string that is provided on the command line.

<a href="javascript:exerciseToggle(34)">Answer</a>
<div id="exercise34" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution1 {
		static def reverse_words(str : String) : String {
			var words = str.split("\\s+")
			var nwords = new StringBuilder
			for (word : words) {
				if (nwords.length > 0) {
					nwords.insert(0, " ")
				}
				nwords.insert(0, word)
			}
			return nwords.toString
		}
		static def main(args : String*) : void {
			for (arg : args) {
				println(arg.reverse_words)
			}
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution2 {
		static def reverse_words(str : String) : String {
			str.split("\\s+").reverseView.join(" ")
		}
		static def main(args : String*) : void {
			for (arg : args) {
				println(arg.reverse_words)
			}
		}
	}
[:End:]
</div>

## Exercise 35

* Write a SARL program to print the square and cube symbols in the area of a rectangle and the volume of a cylinder, using the string formatting tool of the API.
* Input variables:

```text
area = 1256.66
volume = 1254.725
decimals = 2
```

* Sample output:

```text
The area of the rectangle is 1256.66cm2
The volume of the cylinder is 1254.725cm3
```

<a href="javascript:exerciseToggle(35)">Answer</a>
<div id="exercise35" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	import static extension com.google.common.base.Strings.repeat
	class Solution {
		static def main : void {
			var area = 1256.66
			var volume = 1254.725
			var decimals = 2
			println(String::format(
				"The area of the rectangle is {0,number,#0." + "0".repeat(decimals) + "}cm\u00b2", area))
			decimals = 3
			println(String::format(
				"The volume of the cylinder is {0,number,#0." + "0".repeat(decimals) + "}cm\u00b3", volume))
		}
	}
[:End:]
</div>

## Exercise 36

* Write a SARL program to print the index of a character in a string.
* Sample string: `w3resource`
* Expected output:

```text
Current character w position at 0
Current character 3 position at 1
Current character r position at 2
....
Current character c position at 8
Current character e position at 9
```

<a href="javascript:exerciseToggle(36)">Answer</a>
<div id="exercise36" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution1 {
		static def index_of(str : String, c : char) : int {
			for (var i = 0; i < str.length; i++) {
				if (str.charAt(i) == c) {
					return i
				}
			}
			return -1
		}
		static def main : void {
			println("w3resource".index_of('w'))
			println("w3resource".index_of('3'))
			println("w3resource".index_of('r'))
			println("w3resource".index_of('e'))
			println("w3resource".index_of('s'))
			println("w3resource".index_of('o'))
			println("w3resource".index_of('u'))
			println("w3resource".index_of('c'))
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution2 {
		static def main : void {
			println("w3resource".indexOf('w'))
			println("w3resource".indexOf('3'))
			println("w3resource".indexOf('r'))
			println("w3resource".indexOf('e'))
			println("w3resource".indexOf('s'))
			println("w3resource".indexOf('o'))
			println("w3resource".indexOf('u'))
			println("w3resource".indexOf('c'))
		}
	}
[:End:]
</div>

## Exercise 37

* Write a SARL program to check whether a string contains all letters of the alphabet.

<a href="javascript:exerciseToggle(37)">Answer</a>
<div id="exercise37" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution {
		static def is_alphabet(str : String) : boolean {
			if (str.length < 26) {
				var alpha = newTreeSet(null)
				str.chars.forEach[
					var c = Character::toLowerCase(it as char)
					if (Character::isLetter(c)) {
						alpha += c
					}
				]
				return alpha.size == 26
			} else {
				for (var c = ('a' as char); c <= ('z' as char); c++) {
					if (str.chars.noneMatch[Character::toLowerCase(it as char) == c]) {
						return false
					}
				}
			}
			return true
		}
		static def main : void {
			println("w3resource".is_alphabet)
		}
	}
[:End:]
</div>

## Exercise 38

* Write a SARL program to convert a given string into a list of words.
* Input: `The quick brown fox jumps over the lazy dog.`
* Sample Output:

```text
['The', 'quick', 'brown', 'fox', 'jumps', 'over', 'the', 'lazy', 'dog.']
```

<a href="javascript:exerciseToggle(38)">Answer</a>
<div id="exercise38" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	import java.util.List
	class Solution1 {
		static def split_words(str : String) : List<String> {
			var list = newArrayList
			var word = new StringBuilder
			str.chars.forEach[
				var c = it as char
				if (!Character::isWhitespace(c)) {
					word.append(c)
				} else if (word.length > 0) {
					list.add(word.toString)
					word = new StringBuilder
				}
			]
			return list
		}

		static def main(args : String*) : void {
			for (sentence : args) {
				println(sentence.split_words)
			}
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	import java.util.List
	class Solution2 {
		static def main(args : String*) : void {
			for (sentence : args) {
				println(sentence.split("\\s+"))
			}
		}
	}
[:End:]
</div>

## Exercise 39

* Write a SARL program to lowercase the first n characters in a string.

<a href="javascript:exerciseToggle(39)">Answer</a>
<div id="exercise39" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution1 {
		static def lowercase(str : String, n : int) : String {
			if (str.length <= n) {
				return str.toLowerCase
			}
			var word = new StringBuilder
			for (var i = 0; i < n; i++) {
				word.append(Character::toLowerCase(str.charAt(i)))
			}
			for (var i = n; i < str.length; i++) {
				word.append(str.charAt(i))
			}
			return word.toString
		}

		static def main : void {
			println("W3RESOURCE".lowercase(3))
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution2 {
		static def lowercase(str : String, n : int) : String {
			if (str.length <= n) {
				return str.toLowerCase
			}
			return str.substring(0, n).toLowerCase + str.substring(n, str.length)
		}

		static def main : void {
			println("W3RESOURCE".lowercase(3))
		}
	}
[:End:]
</div>

## Exercise 40

* Write a SARL program to swap commas and dots in a string.
* Sample string: `32.054,23`
* Expected Output: `32,054.23`

<a href="javascript:exerciseToggle(40)">Answer</a>
<div id="exercise40" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution1 {
		static def swap(str : String) : String {
			var out = new StringBuilder
			for (var i = 0; i < str.length; i++) {
				var c = str.charAt(i)
				switch (c) {
					case '.': {
						out.append(",")
					}
					case ',': {
						out.append(".")
					}
					default: {
						out.append(c)
					}
				}
			}
			return out.toString
		}
		static def main : void {
			println("W3RESOURCE".swap)
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution2 {
		static def swap(str : String) : String {
			str.chars.map[
				var c = it as char
				switch (c) {
					case '.': {
						return ',' as int
					}
					case ',': {
						return '.' as int
					}
					default: {
						return c as int
					}
				}
			].toArray.join
		}
		static def main : void {
			println("10,153.2568".swap)
		}
	}
[:End:]
</div>

## Exercise 41

* Write a SARL program to count and display vowels in text.

<a href="javascript:exerciseToggle(41)">Answer</a>
<div id="exercise41" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution1 {
		static var VOWELS = <Character>newTreeSet(null, "a", "e", "i", "o", "u", "y")

		static def vowels(str : String) : int {
			var count = 0
			for (var i = 0; i < str.length; i++) {
				if (VOWELS.contains(str.charAt(i))) {
					count++
				}
			}
			return count
		}

		static def main : void {
			println("W3RESOURCE".vowels)
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution2 {
		static var VOWELS = <Character>newTreeSet(null, "a", "e", "i", "o", "u", "y")

		static def vowels(str : String) : int {
			str.toCharArray.filter[VOWELS.contains(it)].size
		}

		static def main : void {
			println("W3RESOURCE".vowels)
		}
	}
[:End:]
</div>

## Exercise 42

* Write a SARL program to split a string on the last occurrence of the delimiter.

<a href="javascript:exerciseToggle(42)">Answer</a>
<div id="exercise42" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution1 {
		static def split_last(str : String, delimiter : char) : String[] {
			var part1 = new StringBuilder
			var i = str.length - 1
			for (; i >= 0; i--) {
				var c = str.charAt(i)
				if (delimiter == c) {
					break
				}
				part1.insert(0, c)
			}
			var part0 = new StringBuilder
			for (i--; i >= 0; i--) {
				var c = str.charAt(i)
				part0.insert(0, c)
			}
			return #[part0.toString, part1.toString]
		}

		static def main : void {
			println("W3,RESOURCE,id".split_last(","))
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution2 {
		static def split_last(str : String, delimiter : char) : String[] {
			var idx = str.lastIndexOf(delimiter)
			if (idx >= 0) {
				return #[
					str.substring(0, idx),
					str.substring(idx + 1, str.length)
				]
			}
			return #[str]
		}

		static def main : void {
			println("W3,RESOURCE,id".split_last(","))
		}
	}
[:End:]
</div>

## Exercise 43

* Write a SARL program to find the first non-repeating character in a given string.

<a href="javascript:exerciseToggle(43)">Answer</a>
<div id="exercise43" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution {
		static def first_non_repeating_character(str : String) : Character {
			var char_order = newArrayList
			var ctr = newHashMap
			str.chars.forEach[
				if (ctr.containsKey(it)) {
					var n = ctr.getOrDefault(it, 0)
					ctr.put(it, n + 1)
				} else {
					ctr.put(it, 1)
					char_order += it
				}
			]
			return char_order.findFirst[ctr.get(it) == 1] as Character
		}

		static def main : void {
			println(first_non_repeating_character('abcdef'))
			println(first_non_repeating_character('abcabcdef'))
			println(first_non_repeating_character('aabbcc'))
		}
	}
[:End:]
</div>

## Exercise 44

* Write a SARL program to print all permutations with a given repetition number of characters of a given string.

<a href="javascript:exerciseToggle(44)">Answer</a>
<div id="exercise44" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	import java.util.List
	class Solution {
		static def all_repeat(str : String, n : int) : List<String> {
			var chars = str.toCharArray
			var result = newArrayList
			if (n > 0) {
				var nn = Math::min(n, chars.length)
				while (nn > 0) {
					var nresult = newArrayList
					for (base : result) {
						for (c : chars) {
							nresult += base + (c as String)
						}
					}
					result = nresult
					nn--
				}
			}
			return result
		}

		static def main : void {
			println(all_repeat('xyz', 3))
			println(all_repeat('xyz', 2))
			println(all_repeat('abcd', 4))
		}
	}
[:End:]
</div>

## Exercise 45

* Write a SARL program to find the first repeated character in a given string.

<a href="javascript:exerciseToggle(45)">Answer</a>
<div id="exercise45" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution {
		static def first_repeating_character(str : String) : Character {
			var char_order = newArrayList
			var ctr = newHashMap
			str.chars.forEach[
				if (ctr.containsKey(it)) {
					var n = ctr.getOrDefault(it, 0)
					ctr.put(it, n + 1)
				} else {
					ctr.put(it, 1)
					char_order += it
				}
			]
			return char_order.findFirst[ctr.get(it) >= 1] as Character
		}

		static def main : void {
			println(first_repeating_character('abcdef'))
			println(first_repeating_character('abcabcdef'))
			println(first_repeating_character('aabbcc'))
		}
	}
[:End:]
</div>

## Exercise 46

* Write a SARL program to find the first repeated character in a given string where the index of the first occurrence is smallest.

<a href="javascript:exerciseToggle(46)">Answer</a>
<div id="exercise46" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution1 {
		static def first_second(str : String, c : char) : int {
			var i = 0
			var index = -1
			while (i < str.length && index == -1) {
				var cc = str.charAt(i)
				if (cc == c) {
					index = i
				}
				i++
			}
			if (index >= 0) {
				while (i < str.length) {
					var cc = str.charAt(i)
					if (cc == c) {
						return i
					}
					i++
				}
			}
			return -1
		}

		static def main : void {
			println(first_second('abcabcdef', 'a'))
			println(first_second('abcabcdef', 'b'))
			println(first_second('abcabcdef', 'c'))
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution2 {
		static def first_second(str : String, c : char) : int {
			var index = str.indexOf(c)
			if (index >= 0) {
				return str.indexOf(c, index + 1)
			}
			return -1
		}

		static def main : void {
			println(first_second('abcabcdef', 'a'))
			println(first_second('abcabcdef', 'b'))
			println(first_second('abcabcdef', 'c'))
		}
	}
[:End:]
</div>

## Exercise 47

* Write a SARL program to find the first repeated word in a given string.

<a href="javascript:exerciseToggle(47)">Answer</a>
<div id="exercise47" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution {
		static def first_repeated_word(str : String) : String {
			var found = newHashSet
			var words = str.split("\\s+")
			for (word : words) {
				if (!found.add(word)) {
					return word
				}
			}
			return null
		}

		static def main : void {
			println(first_repeated_word("ab ca bc ab"))
			println(first_repeated_word("ab ca bc ab ca ab bc"))
			println(first_repeated_word("ab ca bc ca ab bc"))
			println(first_repeated_word("ab ca bc"))
		}
	}
[:End:]
</div>

## Exercise 48

* Write a SARL program to find the second most repeated word in a given string.

<a href="javascript:exerciseToggle(48)">Answer</a>
<div id="exercise48" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution {
		static def word_count(str : String) : String {
			var counts = newHashMap
			var words = str.split("\\s+")
			for (word : words) {
				var n = counts.getOrDefault(word, 0)
				counts.put(word, n + 1)
			}
			var e = counts.entrySet.sortWith[a, b | b.value <=> a.value].get(1)
			return e.key
		}
    
		static def main : void {
			println(word_count("Both of these issues are fixed by postponing the evaluation of annotations. Instead of compiling code which executes expressions in annotations at their definition time, the compiler stores the annotation in a string form equivalent to the AST of the expression in question. If needed, annotations can be resolved at runtime using typing.get_type_hints(). In the common case where this is not required, the annotations are cheaper to store (since short strings are interned by the interpreter) and make startup time faster."))
		}
	}
[:End:]
</div>

## Exercise 49

* Write a SARL program to remove spaces from a given string.

<a href="javascript:exerciseToggle(49)">Answer</a>
<div id="exercise49" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution1 {
		static def remove_spaces(str : String) : String {
			var out = new StringBuilder
			for (var i = 0; i < str.length; i++) {
				var c = str.charAt(i)
				if (!Character::isWhitespace(c)) {
					out.append(c)
				}
			}
			return out.toString
		}
    
		static def main : void {
			println(remove_spaces("Both of these issues are fixed by postponing the evaluation of annotations."))
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution2 {
		static def remove_spaces(str : String) : String {
			str.replaceAll("\\s+", "")
		}
    
		static def main : void {
			println(remove_spaces("Both of these issues are fixed by postponing the evaluation of annotations."))
		}
	}
[:End:]
</div>

## Exercise 50

* Write a SARL program to find all the common characters in lexicographical order from two given lower case strings. If there are no similar letters print `No common characters`.

<a href="javascript:exerciseToggle(50)">Answer</a>
<div id="exercise50" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	import java.util.Map
	class Solution {
		static def nb_chars(str : String) : Map<Character, Integer> {
			var map = newHashMap
			str.chars.forEach[
				var c = it as char
				var n = map.getOrDefault(c, 0)
				map.put(c, n + 1)
			]
			return map
		}

		static def common_chars(str1 : String, str2 : String) : String {
			var d1 = str1.nb_chars
			var d2 = str2.nb_chars
			var common_dict = newArrayList
			for (k : d1.keySet) {
				if (d2.containsKey(k)) {
					common_dict += k
				}
			}
			return common_dict.sort.join
		}

		static def main : void {
			var str1 = 'Python'
			var str2 = 'PHP'
			println("Two strings: " + str1 + ' : ' + str2)
			println(common_chars(str1, str2))
			str1 = 'Java'
			str2 = 'PHP'
			println("Two strings: " + str1 + ' : ' + str2)
			println(common_chars(str1, str2))
		}
	}
[:End:]
</div>

## Exercise 51

* Write a SARL program to make two given strings (lower case, may or may not be of the same length) anagrams without removing any characters from any of the strings.

<a href="javascript:exerciseToggle(51)">Answer</a>
<div id="exercise51" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	import java.util.Map
	class Solution {
		static def nb_chars(str : String) : Map<Character, Integer> {
			var map = newHashMap
			str.chars.forEach[
				var c = it as char
				var n = map.getOrDefault(c, 0)
				map.put(c, n + 1)
			]
			return map
		}

		static def make_anagram(str1 : String, str2 : String) : int {
			var str1_map = str1.nb_chars
			var str2_map = str2.nb_chars
			var ctr = 0
			for (e : str2_map.entrySet) {
				if (str1_map.containsKey(e.key)) {
					ctr += Math::max(0, e.value - str1_map.get(e.key))
				} else {
					ctr += e.value
				}
			}
			for (e : str1_map.entrySet) {
				if (str2_map.containsKey(e.key)) {
					ctr += Math::max(0, e.value - str2_map.get(e.key))
				} else {
					ctr += e.value
				}
			}
			return ctr
		}

		static def main(args : String*) : void {
			var str1 = args.get(0)
			var str2 = args.get(1)
			println(make_anagram(str1, str2))
		}
	}
[:End:]
</div>

## Exercise 52

* Write a SARL program to remove all consecutive duplicates of a given string.

<a href="javascript:exerciseToggle(52)">Answer</a>
<div id="exercise52" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	import java.util.Map
	class Solution {
		static def remove_duplicates(str : String) : String {
    		var i = 0
			var j = 0
			var newElements = new StringBuilder
			while (j < str.length) {
				if (str.charAt(i) == str.charAt(j)) {
					j++
				} else if (str.charAt(j) != str.charAt(i) || j == str.length() - 1) {
					newElements.append(str.charAt(i))
					i = j
					j++
				}
			}
			newElements.append(str.charAt(j - 1))
			return newElements.toString
		}

		static def main : void {
			println("aaaaabbbbbb".remove_duplicates)
			println("geeksforgeeks".remove_duplicates)
			println("aabccba".remove_duplicates)
		}
	}
[:End:]
</div>

## Exercise 53

* Write a SARL program to find the longest common sub-string from two given strings.

<a href="javascript:exerciseToggle(53)">Answer</a>
<div id="exercise53" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	import java.util.Set
	class Solution {
		static def longest_substring(s : String, t : String) : Set<String> {
			var m = s.length
			var n = t.length
			var counter = newArrayList
			for (x : 0..m) {
				var lst = newArrayList
				for (i : 1..n) {
					lst += 0
				}
				counter.add(lst)
			}
			var longest = 0
			var lcs_set = newHashSet
			for (i : 0..<m) {
				for (j : 0..<n) {
					if (s.charAt(i) == t.charAt(j)) {
						var c = counter.get(i).get(j) + 1
						counter.get(i + 1).set(j, c)
						if (c >= longest) {
							if (c > longest) {
							lcs_set = newHashSet
							longest = c
							}
							lcs_set += s.substring(i - c + 1, i+2)
						}
					}
				}
			}
			return lcs_set
		}

		static def main : void {
			println(longest_substring('abcdefgh', 'xswerabcdwd'))
		}
	}
[:End:]
</div>

## Exercise 54

* Write a SARL program to count Uppercase, Lowercase, special characters and numeric values in a given string.

<a href="javascript:exerciseToggle(54)">Answer</a>
<div id="exercise54" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	import java.util.Set
	class Solution {
		static def count_chars(str : String) : int[] {
			var upper_ctr = 0
			var lower_ctr = 0
			var number_ctr = 0
			var special_ctr = 0
			for (i : 0..<str.length) {
				if (str.charAt(i) >= 'A' && str.charAt(i) <= 'Z') {
					upper_ctr++
				} else if (str.charAt(i) >= 'a' && str.charAt(i) <= 'z') {
					lower_ctr++
				} else if (str.charAt(i) >= '0' && str.charAt(i) <= '9') {
					number_ctr++
				}  else {
					special_ctr++
				}
			}
			return #[upper_ctr, lower_ctr, number_ctr, special_ctr]
		}

		static def main : void {
			var str = "@W3Resource.Com"
			println("Original Substrings:" + str)
			var t = count_chars(str)
			println('Upper case characters: ' + t.get(0))
			println('Lower case characters: ' + t.get(1))
			println('Number case: ' + t.get(2))
			println('Special case characters: ' + t.get(3))
		}
	}
[:End:]
</div>

## Exercise 55

* Write a SARL program to wrap a given string into a paragraph with a given width.
* Input a string: `The quick brown fox.`
* Input the width of the paragraph: `10`
* Result:

```text
The quick
brown fox.
```

<a href="javascript:exerciseToggle(55)">Answer</a>
<div id="exercise55" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	class Solution {
		static def wrap(str : String, width : int) : String {
			var words = str.split("\\s+")
			var buffer = new StringBuilder
			var line = new StringBuilder
			for (word : words) {
				line.append(word)
				if (line.length >= width) {
					if (buffer.length > 0) {
						buffer.append("\n")
					}
					buffer.append(line)
					line.length = 0
				}
			}
			if (line.length >= 0) {
				if (buffer.length > 0) {
					buffer.append("\n")
				}
				buffer.append(line)
			}
			return buffer.toString
		}

		static def main : void {
			println("The quick brown fox.".wrap(10))
		}
	}
[:End:]
</div>

## Exercise 56

* Write a SARL program to swap cases in a given string.
* Input: `SARL eXERiCISES`
* Output: `sarl ExerIcises`

<a href="javascript:exerciseToggle(56)">Answer</a>
<div id="exercise56" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	import static extension java.lang.Character.*
	class Solution {
		static def swap(str : String) : String {
			var out = new StringBuilder
			for (c : str.toCharArray) {
				if (c.isLowerCase) {
					out.append(c.toUpperCase)
				} else if (c.isUpperCase) {
					out.append(c.toLowerCase)
				} else {
					out.append(c)
				}
			}
			return out.toString
		}

		static def main : void {
			println("SARL eXERiCISES".swap)
		}
	}
[:End:]
</div>

## Exercise 57

* Write a SARL program to check whether a given string contains a capital letter, a lower case letter, a number and a minimum length.

<a href="javascript:exerciseToggle(57)">Answer</a>
<div id="exercise57" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	import static extension java.lang.Character.*
	import java.util.List
	class Solution {
		static var messages : List<(String) => String> = #[
			[if (it.chars.anyMatch[c|c.isUpperCase]) null else 'String must have 1 upper case character.'],
			[if (it.chars.anyMatch[c|c.isLowerCase]) null else 'String must have 1 lower case character.'],
			[if (it.chars.anyMatch[c|c.isDigit]) null else 'String must have 1 number.'],
			[if (it.length >= 8) null else 'String length should be at least 8.']
		]

		static def check_string(str : String) : String {
			var r = messages.findFirst[it.apply(str) !== null].apply(str)
			if (r === null) {
				r = "Valid string."
			}
			return r
		}

		static def main(args : String*) : void {
			println(args.get(0).check_string)
		}
	}
[:End:]
</div>

## Exercise 58

* Write a SARL program to convert a given heterogeneous list of scalars into a string.
* Original list: `['Red', 100, -50, 'green', 'w,3,r', 12.12, false]`
* Convert the heterogeneous list of scalars into a string: `Red,100,-50,green,w,3,r,12.12,false`

<a href="javascript:exerciseToggle(58)">Answer</a>
<div id="exercise58" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	import java.util.List
	class Solution1 {
		static var original= #['Red', 100, -50, 'green', 'w,3,r', 12.12, false]

		static def convert(list : List<?>) : String {
			var out = new StringBuilder
			for (element : list) {
				if (out.length > 0) {
					out.append(",")
				}
				out.append(element)
			}
			return out.toString
		}
		static def main : void {
			println(original.convert)
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	import java.util.List
	class Solution2 {
		static var original= #['Red', 100, -50, 'green', 'w,3,r', 12.12, false]

		static def convert(list : List<?>) : String {
			list.join(',')
		}
		static def main : void {
			println(original.convert)
		}
	}
[:End:]
</div>


## Exercise 59

* Write a SARL program to extract numbers from a given string.
* Original string: `red 12 black 45 green`
* Extract numbers from the said string: `[12, 45]`

<a href="javascript:exerciseToggle(58)">Answer</a>
<div id="exercise58" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	import java.util.List
	class Solution1 {
		static def toNumber(str : String) : Number {
			try {
				return Long::valueOf(str)
			} catch (ex : Throwable) {
			}
			try {
				return Double::valueOf(str)
			} catch (ex : Throwable) {
			}
			return null
		}
		static def extract(str : String) : List<? extends Number> {
			var words = str.split("\\s+")
			var out = newArrayList
			for (word : words) {
				var n = word as Number
				if (n !== null) {
					out += n
				}
			}
			return out
		}
		static def main : void {
			println("red 12 black 45 green".extract)
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	import java.util.List
	class Solution2 {
		static def toNumber(str : String) : Number {
			try {
				return Long::valueOf(str)
			} catch (ex : Throwable) {
			}
			try {
				return Double::valueOf(str)
			} catch (ex : Throwable) {
			}
			return null
		}
		static def extract(str : String) : List<? extends Number> {
			str.split("\\s+").map[it as Number].filter[it !== null].toList
		}
		static def main : void {
			println("red 12 black 45 green".extract)
		}
	}
[:End:]
</div>

## Exercise 60

* Write a SARL program to replace each character of a word of length five and more with a hash character (`#`).
* Original string: `Count the lowercase letters in the said list of words:`
* Replace words (length five or more) with hash characters in the said string: `##### the ######### ####### in the said list of ######`

<a href="javascript:exerciseToggle(58)">Answer</a>
<div id="exercise58" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	import com.google.common.base.Strings
	class Solution1 {
		static def hide(str : String) : String {
			var words = str.split("\\s+")
			var out = new StringBuilder
			for (word : words) {
				if (out.length > 0) {
					out.append(" ")
				}
				if (word.length >= 5) {
					for (i : 1..word.length) {
						out.append("#")
					}
				} else {
					out.append(word)
				}
			}
			return out.toString
		}
		static def main : void {
			println("Count the lowercase letters in the said list of words:".hide)
		}
	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.strexercises
	[:OnHtml]
	import com.google.common.base.Strings
	class Solution2 {
		static def hide(str : String) : String {
			str.split("\\s+").map[
				if (it.length >= 5) {
					Strings.repeat("#", it.length)
				} else {
					it
				}
			].join(" ")
		}
		static def main : void {
			println("Count the lowercase letters in the said list of words:".hide)
		}
	}
[:End:]
</div>


[:Include:](../../includes/legal.inc)
