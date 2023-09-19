# Introduction to Maps with SARL

[:Outline:]

> **_Note 1:_** In SARL, a map is a data structure that is also known as dictionary in other programming languages. This type of data structure maps keys to values.

> **_Note 2:_** If you don't know how to solve an problem, or what is the function to be used, you could search on Internet for the answer using the API of the Java programming language. Indeed, since SARL is fully compatible with the Java API, you could use all the types or functions that are defined in this Java API.


## Exercise 1

Write a SARL script to sort (ascending and descending) a map by value.

> [Solution](IntroductionMapAnswers.md#exercise-1)


## Exercise 2

Write a SARL script to add a key to a map.

* Sample Map: `{0: 10, 1: 20}`
* Expected Result: `{0: 10, 1: 20, 2: 30}`

> [Solution](IntroductionMapAnswers.md#exercise-2)


## Exercise 3

Write a SARL script to concatenate the following maps to create a new one.

* Sample Maps:

```
dic1={1:10, 2:20}
dic2={3:30, 4:40}
dic3={5:50,6:60}
```

* Expected Result : `{1: 10, 2: 20, 3: 30, 4: 40, 5: 50, 6: 60}`

> [Solution](IntroductionMapAnswers.md#exercise-3)


## Exercise 4

Write a SARL script to check whether a given key already exists in a map.

> [Solution](IntroductionMapAnswers.md#exercise-4)


## Exercise 5

Write a SARL program to iterate over maps using for loops.

> [Solution](IntroductionMapAnswers.md#exercise-5)


## Exercise 6

Write a SARL script to generate and print a map that contains a number (between 1 and n) in the form (x, x*x).

* n = 5
* Expected Output: `{1: 1, 2: 4, 3: 9, 4: 16, 5: 25}`

> [Solution](IntroductionMapAnswers.md#exercise-6)


## Exercise 7

Write a SARL script to print a map where the keys are numbers between 1 and 15 (both included) and the values are the square of the keys.

* Sample map: `{1: 1, 2: 4, 3: 9, 4: 16, 5: 25, 6: 36, 7: 49, 8: 64, 9: 81, 10: 100, 11: 121, 12: 144, 13: 169, 14: 196, 15: 225}`

> [Solution](IntroductionMapAnswers.md#exercise-7)


## Exercise 8

Write a SARL script to merge two SARL maps.

> [Solution](IntroductionMapAnswers.md#exercise-8)


## Exercise 9

Write a SARL program to sum all the values in a maps.

> [Solution](IntroductionMapAnswers.md#exercise-9)


## Exercise 10

Write a SARL program to multiply all the values in a map.

> [Solution](IntroductionMapAnswers.md#exercise-10)


## Exercise 11

Write a SARL program to remove a key from a map.

> [Solution](IntroductionMapAnswers.md#exercise-11)


## Exercise 12

Write a SARL program to map two lists into a map.

> [Solution](IntroductionMapAnswers.md#exercise-12)


## Exercise 13

Write a SARL program to sort a given map by key.

> [Solution](IntroductionMapAnswers.md#exercise-13)


## Exercise 14

Write a SARL program to get the maximum and minimum values of a map.

> [Solution](IntroductionMapAnswers.md#exercise-14)


## Exercise 15

Write a SARL program to remove duplicated values from the map.

> [Solution](IntroductionMapAnswers.md#exercise-15)


## Exercise 16

Write a SARL program to check if a map is empty or not.

> [Solution](IntroductionMapAnswers.md#exercise-16)


## Exercise 17

Write a SARL program to combine two maps by adding values for common keys.

* Inputs:

```
d1 = {'a': 100, 'b': 200, 'c':300}
d2 = {'a': 300, 'b': 200, 'd':400}
```

* Sample output: `{'a': 400, 'b': 400, 'd': 400, 'c': 300}`

> [Solution](IntroductionMapAnswers.md#exercise-17)


## Exercise 18

Write a SARL program to print all distinct values in a map.

* Sample Data : `[{"V":"S001"}, {"V": "S002"}, {"VI": "S001"}, {"VI": "S005"}, {"VII":"S005"}, {"V":"S009"},{"VIII":"S007"}]`
* Expected Output : `{'S005', 'S002', 'S007', 'S001', 'S009'}`

> [Solution](IntroductionMapAnswers.md#exercise-18)


## Exercise 19

Write a SARL program to create and display all combinations of letters, selecting each letter from a different key in a map.

* Sample data: `{'1':['a','b'], '2':['c','d']}`
* Expected Output:

```
ac
ad
bc
bd
```

> [Solution](IntroductionMapAnswers.md#exercise-19)


## Exercise 20

Write a SARL program to find the highest 3 values of corresponding keys in a map.

> [Solution](IntroductionMapAnswers.md#exercise-20)


## Exercise 21

Write a SARL program to combine values in a list of maps.

* Sample data: `[{'item': 'item1', 'amount': 400}, {'item': 'item2', 'amount': 300}, {'item': 'item1', 'amount': 750}]`
* Expected Output: `{'item1': 1150, 'item2': 300}`

> [Solution](IntroductionMapAnswers.md#exercise-21)


## Exercise 22

Write a SARL program to create a map from a string. Track the count of the letters from the string.

* Sample string: `w3resource`
* Expected output: `{'w': 1, '3': 1, 'r': 2, 'e': 2, 's': 1, 'o': 1, 'u': 1, 'c': 1}`

> [Solution](IntroductionMapAnswers.md#exercise-22)


## Exercise 23

Write a SARL program to print a map in table format.

* Sample Input:

```
{1: ["Samuel", 21, 'Data Structures'],
 2: ["Richie", 20, 'Machine Learning'],
 3: ["Lauren", 21, 'OOPS with java'],
}
```

* Expected Output:

```
Samuel	21	Data Structures
Richie	20	Machine Learning
Lauren	21	OOPS with java
```

> [Solution](IntroductionMapAnswers.md#exercise-23)


## Exercise 24

Write a SARL program to sort a list alphabetically in a map.

* Sample Input:  `{'n1': [2, 3, 1], 'n2': [5, 1, 2], 'n3': [3, 2, 4]}`
* Expected Output: `{'n1': [1, 2, 3], 'n2': [1, 2, 5], 'n3': [2, 3, 4]}`

> [Solution](IntroductionMapAnswers.md#exercise-24)


## Exercise 25

Write a SARL program to remove spaces from map keys.

> [Solution](IntroductionMapAnswers.md#exercise-25)


## Exercise 26

Write a SARL program to get the top three items in a shop.

* Sample data: `{'item1': 45.50, 'item2':35, 'item3': 41.30, 'item4':55, 'item5': 24}`
* Expected Output:

```
item4 55
item1 45.5
item3 41.3
```

> [Solution](IntroductionMapAnswers.md#exercise-26)


## Exercise 27

Write a SARL program to get the key, value and item in a map.

> [Solution](IntroductionMapAnswers.md#exercise-27)


## Exercise 28

Write a SARL program to print a map line by line.

> [Solution](IntroductionMapAnswers.md#exercise-28)


## Exercise 29

Write a SARL program to count the number of items in a map value that is a list.

> [Solution](IntroductionMapAnswers.md#exercise-29)


## Exercise 30

Write a SARL program to sort items by value in reverse order.

* Sample data: `{'Math':81, 'Physics':83, 'Chemistry':87}`
* Expected data: `[('Chemistry', 87), ('Physics', 83), ('Math', 81)]`

> [Solution](IntroductionMapAnswers.md#exercise-30)


## Exercise 31

Write a SARL program to create a map from two lists without losing duplicate values.

* Sample lists: `['Class-V', 'Class-VI', 'Class-VII', 'Class-VIII']` and `[1, 2, 2, 3]`
* Expected Output: `{'Class-V': {1}, 'Class-VI': {2}, 'Class-VII': {2}, 'Class-VIII': {3}}`

> [Solution](IntroductionMapAnswers.md#exercise-31)


## Exercise 32

Write a SARL program to match key values in two dictionaries.

* Sample maps: `{'key1': 1, 'key2': 3, 'key3': 2}` and `{'key1': 1, 'key2': 2}`
* Expected output: `{'key1': 1}` is present in both input maps

> [Solution](IntroductionMapAnswers.md#exercise-32)


## Exercise 33

Write a SARL program to store map data in a JSON file.

* Original map:

```
{'students': [{'firstName': 'Nikki', 'lastName': 'Roysden'}, {'firstName': 'Mervin', 'lastName': 'Friedland'}, {'firstName': 'Aron ', 'lastName': 'Wilkins'}], 'teachers': [{'firstName': 'Amberly', 'lastName': 'Calico'}, {'firstName': 'Regine', 'lastName': 'Agtarap'}]}
```

* Json file:

```
{'students': [{'firstName': 'Nikki', 'lastName': 'Roysden'}, {'firstName': 'Mervin', 'lastName': 'Friedland'}, {'firstName': 'Aron ', 'lastName': 'Wilkins'}], 'teachers': [{'firstName': 'Amberly', 'lastName': 'Calico'}, {'firstName': 'Regine', 'lastName': 'Agtarap'}]}
```

> [Solution](IntroductionMapAnswers.md#exercise-33)


## Exercise 34

Write a SARL program to create a map of keys x, y, and z where each key has as value a list from 11-20, 21-30, and 31-40 respectively. Access the fifth value of each key from the map.

```
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

> [Solution](IntroductionMapAnswers.md#exercise-34)


## Exercise 35

Write a SARL program to drop empty items from a given map.

* Original Map: `{'c1': 'Red', 'c2': 'Green', 'c3': null}`
* New map after dropping empty items: `{'c1': 'Red', 'c2': 'Green'}`

> [Solution](IntroductionMapAnswers.md#exercise-35)


## Exercise 36

Write a SARL program to filter a map based on values.

* Original Map: `{'Cierra Vega': 175, 'Alden Cantrell': 180, 'Kierra Gentry': 165, 'Pierre Cox': 190}`
* Marks greater than 170: `{'Cierra Vega': 175, 'Alden Cantrell': 180, 'Pierre Cox': 190}`

> [Solution](IntroductionMapAnswers.md#exercise-36)


## Exercise 37

Write a SARL program to convert more than one list to a nested map.

* Original strings: `['S001', 'S002', 'S003', 'S004']`, and `['Adina Park', 'Leyton Marsh', 'Duncan Boyle', 'Saim Richards']`, and `[85, 98, 89, 92]`
* Nested map: `[{'S001': {'Adina Park': 85}}, {'S002': {'Leyton Marsh': 98}}, {'S003': {'Duncan Boyle': 89}}, {'S004': {'Saim Richards': 92}}]`

> [Solution](IntroductionMapAnswers.md#exercise-37)


## Exercise 38

Write a SARL program to filter the height and width of students, which are stored in a map.

* Original Map: `{'Cierra Vega': (6.2, 70), 'Alden Cantrell': (5.9, 65), 'Kierra Gentry': (6.0, 68), 'Pierre Cox': (5.8, 66)}`
* Height > 6ft and Weight> 70kg: `{'Cierra Vega': (6.2, 70)}`

> [Solution](IntroductionMapAnswers.md#exercise-38)


## Exercise 39

Write a SARL program to verify that all values in a map are the same.

* Original Map: `{'Cierra Vega': 12, 'Alden Cantrell': 12, 'Kierra Gentry': 12, 'Pierre Cox': 12}`
* Check all are 12 in the map: `true`
* Check all are 10 in the map: `false`

> [Solution](IntroductionMapAnswers.md#exercise-39)


## Exercise 40

Write a SARL program to create a map  grouping a sequence of key-value pairs into a map of lists.

* Original list: `[('yellow', 1), ('blue', 2), ('yellow', 3), ('blue', 4), ('red', 1)]`
* Grouping a sequence of key-value pairs into a map of lists: `{'yellow': [1, 3], 'blue': [2, 4], 'red': [1]}`

> [Solution](IntroductionMapAnswers.md#exercise-40)


## Exercise 41

Write a SARL program to split a given map of lists into lists of maps.

* Original map of lists: `{'Science': [88, 89, 62, 95], 'Language': [77, 78, 84, 80]}`
* Split said map of lists into list of dictionaries: `[{'Science': 88, 'Language': 77}, {'Science': 89, 'Language': 78}, {'Science': 62, 'Language': 84}, {'Science': 95, 'Language': 80}]`

> [Solution](IntroductionMapAnswers.md#exercise-41)


## Exercise 42

Write a SARL program to remove a specified map from a given list.

* Original list of map: `[{'id': '#FF0000', 'color': 'Red'}, {'id': '#800000', 'color': 'Maroon'}, {'id': '#FFFF00', 'color': 'Yellow'}, {'id': '#808000', 'color': 'Olive'}]`
* Remove id `#FF0000` from the said list of map: `[{'id': '#800000', 'color': 'Maroon'}, {'id': '#FFFF00', 'color': 'Yellow'}, {'id': '#808000', 'color': 'Olive'}]`

> [Solution](IntroductionMapAnswers.md#exercise-42)


## Exercise 43

Write a SARL program to convert string values of a given map into integer/float datatypes.

* Original list: `[{'x': '10', 'y': '20', 'z': '30'}, {'p': '40', 'q': '50', 'r': '60'}]`
* String values of a given map, into integer types: `[{'x': 10, 'y': 20, 'z': 30}, {'p': 40, 'q': 50, 'r': 60}]`
* Original list: `[{'x': '10.12', 'y': '20.23', 'z': '30'}, {'p': '40.00', 'q': '50.19', 'r': '60.99'}]`
* String values of a given map, into float types: `[{'x': 10.12, 'y': 20.23, 'z': 30.0}, {'p': 40.0, 'q': 50.19, 'r': 60.99}]`

> [Solution](IntroductionMapAnswers.md#exercise-43)


## Exercise 44

A SARL map contains List as a value. Write a SARL program to clear the list values in the said map.

* Original Map: `{'C1': [10, 20, 30], 'C2': [20, 30, 40], 'C3': [12, 34]}`
* Clear the list values in the said map: `{'C1': [], 'C2': [], 'C3': []}`

> [Solution](IntroductionMapAnswers.md#exercise-44)


## Exercise 45

A SARL map contains List as a value. Write a SARL program to update the list values in the said map by adding 1 to the scores in Math and substracting 2 to the scores in Physics

* Original Map: `{'Math': [88, 89, 90], 'Physics': [92, 94, 89], 'Chemistry': [90, 87, 93]}`
* Update the list values of the said map: `{'Math': [89, 90, 91], 'Physics': [90, 92, 87], 'Chemistry': [90, 87, 93]}`

> [Solution](IntroductionMapAnswers.md#exercise-45)


## Exercise 46

Write a SARL program to extract a list of values from a given list of maps.

* Original Map: `[{'Math': 90, 'Science': 92}, {'Math': 89, 'Science': 94}, {'Math': 92, 'Science': 88}]`
* Extract a list of values from said list of maps where subject = Science: `[92, 94, 88]`
* Original Map: `[{'Math': 90, 'Science': 92}, {'Math': 89, 'Science': 94}, {'Math': 92, 'Science': 88}]`
* Extract a list of values from said list of maps where subject = Math: `[90, 89, 92]`

> [Solution](IntroductionMapAnswers.md#exercise-46)


## Exercise 47

Write a SARL program to find the length of a map of values.

* Original Map: `{1: 'red', 2: 'green', 3: 'black', 4: 'white', 5: 'black'}`
* Length of map values: `{'red': 3, 'green': 5, 'black': 5, 'white': 5}`
* Original Map: `{'1': 'Austin Little', '2': 'Natasha Howard', '3': 'Alfred Mullins', '4': 'Jamie Rowe'}`
* Length of map values: `{'Austin Little': 13, 'Natasha Howard': 14, 'Alfred Mullins': 14, 'Jamie Rowe': 10}`

> [Solution](IntroductionMapAnswers.md#exercise-47)


## Exercise 48

Write a SARL program to get the depth of a map.

> [Solution](IntroductionMapAnswers.md#exercise-48)


## Exercise 49

Write a SARL program to access map key's element by index.

* Sample Input: `{'physics': 80, 'math': 90, 'chemistry': 86}`
* Expected Output:

```
0 = physics
1 = math
2 = chemistry
```

> [Solution](IntroductionMapAnswers.md#exercise-49)


## Exercise 50

Write a SARL program to convert a map into a list of lists.

* Original Map: `{1: 'red', 2: 'green', 3: 'black', 4: 'white', 5: 'black'}`
* Convert the said map into a list of lists: `[[1, 'red'], [2, 'green'], [3, 'black'], [4, 'white'], [5, 'black']]`
* Original Map: `{'1': 'Austin Little', '2': 'Natasha Howard', '3': 'Alfred Mullins', '4': 'Jamie Rowe'}`
* Convert the said map into a list of lists: `[['1', 'Austin Little'], ['2', 'Natasha Howard'], ['3', 'Alfred Mullins'], ['4', 'Jamie Rowe']]`

> [Solution](IntroductionMapAnswers.md#exercise-50)


## Exercise 51

Write a SARL program to filter even numbers from a map of values.

* Original Map: `{'V': [1, 4, 6, 10], 'VI': [1, 4, 12], 'VII': [1, 3, 8]}`
* Filter even numbers from said map values: `{'V': [4, 6, 10], 'VI': [4, 12], 'VII': [8]}`
* Original Map: `{'V': [1, 3, 5], 'VI': [1, 5], 'VII': [2, 7, 9]}`
* Filter even numbers from said map values: `{'V': [], 'VI': [], 'VII': [2]}`

> [Solution](IntroductionMapAnswers.md#exercise-51)


## Exercise 52

Write a SARL program to get all combinations of key-value pairs in a given map.

* Original Map: `{'V': [1, 4, 6, 10], 'VI': [1, 4, 12], 'VII': [1, 3, 8]}`
* Combinations of key-value pairs of the said map: `[{'V': [1, 4, 6, 10], 'VI': [1, 4, 12]}, {'V': [1, 4, 6, 10], 'VII': [1, 3, 8]}, {'VI': [1, 4, 12], 'VII': [1, 3, 8]}]`
* Original Map: `{'V': [1, 3, 5], 'VI': [1, 5]}`
* Combinations of key-value pairs of the said map: `[{'V': [1, 3, 5], 'VI': [1, 5]}]`

> [Solution](IntroductionMapAnswers.md#exercise-52)


## Exercise 53

Write a SARL program to find the specified number of maximum values in a given map.

* Original Map: `{'a': 5, 'b': 14, 'c': 32, 'd': 35, 'e': 24, 'f': 100, 'g': 57, 'h': 8, 'i': 100}`
* 1 maximum value(s) in the said map: `['f']`
* 2 maximum value(s) in the said map: `['f', 'i']`
* 5 maximum value(s) in the said map: `['f', 'i', 'g', 'd', 'c']`

> [Solution](IntroductionMapAnswers.md#exercise-53)


## Exercise 54

Write a SARL program to find the shortest list of values for the keys in a given map.

* Original Map: `{'V': [10, 12], 'VI': [10], 'VII': [10, 20, 30, 40], 'VIII': [20], 'IX': [10, 30, 50, 70], 'X': [80]}`
* Shortest list of values with the keys of the said map: `['VI', 'VIII', 'X']`

> [Solution](IntroductionMapAnswers.md#exercise-54)


## Exercise 55

Write a SARL program to extract values from a given map and create a list of lists from those values.

* Original Map: `[{'student_id': 1, 'name': 'Jean Castro', 'class': 'V'}, {'student_id': 2, 'name': 'Lula Powell', 'class': 'V'}, {'student_id': 3, 'name': 'Brian Howell', 'class': 'VI'}, {'student_id': 4, 'name': 'Lynne Foster', 'class': 'VI'}, {'student_id': 5, 'name': 'Zachary Simon', 'class': 'VII'}]`
* Extract values from the said map and create a list of lists using those values:
  * For `['student_id', 'name', 'class']`: `[[1, 'Jean Castro', 'V'], [2, 'Lula Powell', 'V'], [3, 'Brian Howell', 'VI'], [4, 'Lynne Foster', 'VI'], [5, 'Zachary Simon', 'VII']]`
  * For `['student_id', 'name']`: `[[1, 'Jean Castro'], [2, 'Lula Powell'], [3, 'Brian Howell'], [4, 'Lynne Foster'], [5, 'Zachary Simon']]`
  * For `['name', 'class']`: `[['Jean Castro', 'V'], ['Lula Powell', 'V'], ['Brian Howell', 'VI'], ['Lynne Foster', 'VI'], ['Zachary Simon', 'VII']]`

> [Solution](IntroductionMapAnswers.md#exercise-55)


## Exercise 56

Write a SARL program to convert a given list of lists to a map.

* Original list of lists: `[[1, 'Jean Castro', 'V'], [2, 'Lula Powell', 'V'], [3, 'Brian Howell', 'VI'], [4, 'Lynne Foster', 'VI'], [5, 'Zachary Simon', 'VII']]`
* Convert the said list of lists to a map: `{1: ['Jean Castro', 'V'], 2: ['Lula Powell', 'V'], 3: ['Brian Howell', 'VI'], 4: ['Lynne Foster', 'VI'], 5: ['Zachary Simon', 'VII']}`

> [Solution](IntroductionMapAnswers.md#exercise-56)


## Exercise 57

Write a SARL program that creates key-value list pairings within a map.

* Original map: `{1: ['Jean Castro'], 2: ['Lula Powell'], 3: ['Brian Howell'], 4: ['Lynne Foster'], 5: ['Zachary Simon']}`
* A key-value list pairings of the said map: `[{1: 'Jean Castro', 2: 'Lula Powell', 3: 'Brian Howell', 4: 'Lynne Foster', 5: 'Zachary Simon'}]`

> [Solution](IntroductionMapAnswers.md#exercise-57)


## Exercise 58

Write a SARL program to get the total length of all values in a given map with string values.

* Original map: `{'#FF0000': 'Red', '#800000': 'Maroon', '#FFFF00': 'Yellow', '#808000': 'Olive'}`
* Total length of all values of the said map with string values: `20`

> [Solution](IntroductionMapAnswers.md#exercise-58)


## Exercise 59

Write a SARL program to check if a specific key and a value exist in a map.

* Original Map: `[{'student_id': 1, 'name': 'Jean Castro', 'class': 'V'}, {'student_id': 2, 'name': 'Lula Powell', 'class': 'V'}, {'student_id': 3, 'name': 'Brian Howell', 'class': 'VI'}, {'student_id': 4, 'name': 'Lynne Foster', 'class': 'VI'}, {'student_id': 5, 'name': 'Zachary Simon', 'class': 'VII'}]`

> [Solution](IntroductionMapAnswers.md#exercise-59)


## Exercise 60

Write a SARL program to invert a given map with non-unique hashable values.

* Sample Input: `{'Ora Mckinney': 8, 'Theodore Hollandl': 7, 'Mae Fleming': 7, 'Mathew Gilbert': 8, 'Ivan Little': 7}`
* Sample Output: `{8: ['Ora Mckinney', 'Mathew Gilbert'], 7: ['Theodore Hollandl', 'Mae Fleming', 'Ivan Little']}`

> [Solution](IntroductionMapAnswers.md#exercise-60)


[:Include:](../legal.inc)
