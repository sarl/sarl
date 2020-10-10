# SARL Documentation Generator

[:Outline:]

[:Fact:]{(io.sarl.sarldoc.Main)::getDefaultProgramName == '[:name](sarldoc)'}

[:name!] is a documentation generator for the SARL language for generating API documentation in HTML format from SARL source code.
The HTML format is used for adding the convenience of being able to hyperlink related documents together.

The "doc comments" format used by [:name!] is the same as the one used by the [Javadoc](https://en.wikipedia.org/wiki/Javadoc) 
tool. Indeed, [:name!] delegates the generation of the documentation to Javadoc with a specific 
documentation pattern for SARL code.

[:name!] does not affect performance in SARL as all comments are removed at compilation time. Writing comments and
[:name!] is for better understanding the code and thus better maintaining it.

## Usage

The [:name:] tool takes arguments:

```text
[:name!] [OPTIONS] <[:srcfolder](source folder)>...
```


The [:name:] tool takes at least one [:srcfolder:] from which the SARL files are read.

You could change the behavior of the [:name:] compiler with the command-line options.
For obtaining a list of the options, type:

```text
[:name!] -h
```


## Command-Line Options

The complete list of the options is:



| Option | Description |
| ------ | ----------- |
[:Dynamic:]{
	runShellSilently(
		makeExecName("..", "..", "..", "..", "..", "..", "products", "sarldoc", "target", "sarldoc"),
		"generatemarkdownhelp".makeCliOption)
}

## Documentation Comments

This section describes source code comments and comment inheritance.

### Source Code Comments

You can include documentation comments in the source code, ahead of declarations for SARL 
construct such as agent, behavior, capacity, skill, event, class, interface, method, constructor, or field.
A documentation comment consists of the characters between `/**` and `*/` that end it. Leading asterisks are
allowed on each line and are described further in the following section. The text in a comment can continue
onto multiple lines.

[:Success:]
    [:On]
    /**
	 * This is the typical format of a simple documentation comment
	 * that spans two lines.
	 */
    [:Off]
    class X {}
[:End:]

#### Placement of Comments

Documentation comments are recognized only when placed immediately before SARl constructs, 
such as agent, behavior, capacity, skill, event, class, interface, constructor, method, or field declarations.
Documentation comments placed in the body of a method are ignored. The [:name:] command recognizes only one
documentation comment per declaration statement.

#### Parts of Comments

A documentation comment has a main description followed by a tag section. The main description begins after the starting
delimiter `/**` and continues until the tag section. The tag section starts with the first block tag, which is defined
by the first `@` character that begins a line (ignoring leading asterisks, white space, and leading separator `/**`).
It is possible to have a comment with only a tag section and no main description. The main description cannot continue
after the tag section begins. The argument to a tag can span multiple lines. There can be any number of tags, and some
types of tags can be repeated while others cannot. For example, this [:seetag:] tag starts the tag section:

[:Success:]
    [:On]
    /**
	 * This sentence holds the main description for this documentation comment.
	 * [:seetag](@see) java.lang.Object
	 */
    [:Off]
    class X {}
[:End:]


#### Block and Inline Tags

A tag is a special keyword within a documentation comment that the [:name:] command processes. There are two kinds of
tags: block tags, which appear as an `@tag` tag (also known as standalone tags), and inline tags, which appear within
braces, as an `{@tag}` tag.

To be interpreted, a block tag must appear at the beginning of a line, ignoring leading
asterisks, white space, and the separator (`/**`). This means you can use the `@` character elsewhere in the text and
it will not be interpreted as the start of a tag. If you want to start a line with the `@` character and not have it
be interpreted, then use the HTML entity `&#064;`. Each block tag has associated text, which includes any text
following the tag up to, but not including, either the next tag, or the end of the documentation comment. This associated
text can span multiple lines.

An inline tag is allowed and interpreted anywhere that text is allowed. The following example contains the
[:deprecatedtag:] block tag and the `[:startlinktag!]}" inline tag.

[:Success:]
    [:On]
    /**
     * [:deprecatedtag](@deprecated) As of SARL 1.1, replaced by [:startlinktag]({@link) #setBounds(int,int,int,int)}
     */
    [:Off]
    class X {}
[:End:]


#### Write comments in HTML

The text must be written in HTML with HTML entities and HTML tags. You can use whichever version of HTML your
browser supports. The standard [:name:] generates HTML 3.2-compliant code elsewhere (outside of the documentation
comments) with the inclusion of cascading style sheets and frames. HTML 4.0 is preferred for generated files
because of the frame sets.

For example, entities for the less than symbol (`<`) and the greater than symbol (`>`) should be written as `&lt;`
and `&gt;`. Similarly, the ampersand (`&`) should be written as `&amp;`. The bold HTML tag [:boldhtmltag:] is shown
in the following example.

[:Success:]
    [:On]
    /**
     * This is a [:boldhtmltag](<b>)doc</b> comment.
     * @see java.lang.Object
     */
    [:Off]
    class X {}
[:End:]


#### Leading Asterisks

When the [:name:] command parses a documentation comment, leading asterisks (`*`) on each line are discarded, and blanks
and tabs that precede the initial asterisks (`*`) are also discarded. If you omit the leading asterisk on a line, then
the leading white space is no longer removed so that you can paste code examples directly into a documentation comment
inside a `<PRE>` tag with its indentation preserved. Spaces are interpreted by browsers more uniformly than
tabs. Indentation is relative to the left margin (rather than the separator `/**` or `<PRE>` tag).

#### First Sentence

The first sentence of each documentation comment should be a summary sentence that contains a concise but complete
description of the declared entity. This sentence ends at the first period that is followed by a blank, tab, or line
terminator, or at the first block tag. The [:name:] command copies this first sentence to the member summary at
the top of the HTML page.

### Method Comment Inheritance

The [:name:] command allows method comment inheritance in type declarations to fill in missing text or to explicitly
inherit method comments. Constructors, fields, and nested types do not inherit documentation comments.

> **_Caution:_** The source file for an inherited method must be on the path for the documentation comment to be available to
> copy. Neither the class nor its package needs to be passed in on the command line.

When a main description, or `@return`, `@param`, or `@throws` tag is missing from a method comment, the [:name:] command
copies the corresponding main description or tag comment from the method it overrides or implements (if any).
When an `@param` tag for a particular parameter is missing, the comment for that parameter is copied from the method
further up the inheritance hierarchy. When an `@throws` tag for a particular exception is missing, the `@throws` tag
is copied only when that exception is declared.

Insert the `{@inheritDoc}` inline tag in a method main description or `@return`, `@param`, or `@throws` tag comment.
The corresponding inherited main description or tag comment is copied into that spot.

### Type Declaration Comment Inheritance

Comment inheritance occurs in all possible cases of inheritance from type declarations:

* When a method in a type overrides a method in a super-type
* When a method in a type implements a method in an interface

In the first case, the [:name:] command generates the subheading "Overrides" in the documentation for the
overriding method. A link to the method being overridden is included, whether or not the comment is inherited.

In the second case, when a method in a specified type implements a method in an interface, the [:name:] command
generates the subheading "Specified by" in the documentation for the overriding method. A link to the method being
implemented is included, whether or not the comment is inherited.

## Tags

The [:name:] command parses special tags when they are embedded within a SARL documentation comment. The
[:name:] tags let you autogenerate a complete, well-formatted API from your source code. The tags start with
an "at" sign (`@`) and are case-sensitive. They must be typed with the uppercase and lowercase letters as shown.
A tag must start at the beginning of a line (after any leading spaces and an optional asterisk), or it is treated
as text. By convention, tags with the same name are grouped together. For example, put all [:seetag:] tags together.

Tags have the following types:

* Bock tags: Place block tags only in the tag section that follows the description. Block tags have the form: `@tag`.
* Inline tags: Place inline tags anywhere in the main description or in the comments for block tags. Inline tags are
  enclosed within braces: `{@tag}`.

| Tag                  | Description                                        |
| -------------------- | -------------------------------------------------- |
| `@author text`       | Adds an author entry with the specified name text. |
| `{@code text}`       | Displays text in code font without interpreting the text as HTML markup or nested tags. |
| `@deprecated text`   | Adds a comment indicating that this API should no longer be used (even though it may continue to work). |
| `{@docRoot}`         | Represents the relative path to the generated document's (destination) root directory from any generated page. |
| `@exception class-name text` | See `@throws`. |
| `{@inheritDoc}`      | Copies documentation from the nearest inheritable type into the current documentation comment at this tag's location. |
| `{@link ref label}`  | Inserts an inline link with a visible text label that points to the documentation for the specified element. |
| `{@linkplain ref label}` | Behaves the same as the `{@link}` tag, except the link label is displayed in plain text rather than code font. |
| `{@literal text}`    | Displays text without interpreting the text as HTML markup or nested tags. |
| `@param name text`   | Adds a parameter with the specified name followed by the specified description to the Parameters section. |
| `@return text`       | Adds a Returns section with the description text. |
| `@see ref`           | Adds a See Also heading with a link or text entry that points to a reference. |
| `@since text`        | Adds a Since heading with the specified text value to the generated documentation. |
| `@throws class-name text` | Adds a Throws subheading to the generated documentation, with the class-name and description text. |
| `{@value ref}`       | Adds a Throws subheading to the generated documentation, with the class-name and description text. |
| `@version text`      | Adds a Version subheading with the specified text value to the generated documents. |


## Acknowledgements

This documentation is inspired by the documentation from the
[Oracle Javadoc](https://docs.oracle.com/javase/8/docs/technotes/tools/windows/javadoc.html) project.

[:Include:](../legal.inc)

