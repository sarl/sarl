# SARL Syntax highlighting with other tools

[:Outline:]

Syntax highlighting is a feature of text editors that are used for programming, scripting, or markup languages, such as SARL.
The feature displays text, especially source code, in different colors and fonts according to the category of terms.
This feature facilitates writing in a structured language such as a programming language or a markup language as both structures
and syntax errors are visually distinct. Highlighting does not affect the meaning of the text itself; it is intended only for human readers.

Several style specifications are provided for syntax highlighting in other tools than the SARL product.


## Atom Editor

[Atom Editor](https://atom.io/) is is a highly configurable text editor.
This editor uses the TextMate language syntax.

The SARL style is available on [GitHub](https://raw.githubusercontent.com/sarl/sarl/master/formatting-styles/textmate/sarl.tmLanguage)


## GNU source-highlight

[GNU source-highlight](https://www.gnu.org/software/src-highlite/) is the GNU source code highlightner.

The SARL style is available on [GitHub](https://raw.githubusercontent.com/sarl/sarl/master/formatting-styles/source-highlight/sarl.lang)

For installing the SARL style, you should:

* download all the style file;
* copy the file into the GNU source-lighlight folder.


## Google Code Prettify

[Google Code Prettify](https://github.com/google/code-prettify) is a Javascript library that enables syntax highlightning for several
different languages on Internet pages.

The SARL style is available on [GitHub](https://raw.githubusercontent.com/sarl/sarl/master/formatting-styles/gtk/sarl.lang)

For using the SARL style, you should:

* copy the SARL style into the same folder as the Google Code Prettify script; and
* Use the CSS classes `prettyprint lang-sarl` each time you want to render a SARL code into your HTML pages.


## Gtk Source View

[Gtk Source View](https://wiki.gnome.org/Projects/GtkSourceView) is the core library that is used by the Gnome tools, including Gedit,
for doing syntax highlightning.

The SARL style is available on [GitHub](https://raw.githubusercontent.com/sarl/sarl/master/formatting-styles/prettify/lang-sarl.js)

For using the SARL style, you should:

* copy the SARL style into the folder `$HOME/.local/share/gtksourceview-3.0/language-specs/`.


## LaTeX

LaTeX is a powerfull wordprocessor that is able to create documents such as books, presentations...


### Standard LaTeX Style

LaTeX may uses different extensions (packages) for rnedering algorithms and source code.
The SARL styles for LaTeX are based on the LaTeX [listings package](https://www.ctan.org/pkg/listings).

Two versions of the SARL style for LaTeX are available:

* [Monochrom](https://raw.githubusercontent.com/sarl/sarl/master/formatting-styles/latex/sarl-listing.sty) (no coloring), and
* [Colorized](https://raw.githubusercontent.com/sarl/sarl/master/formatting-styles/latex/sarl-colorized-listing.sty).

For using the SARL styles, you should:

* download the SARL style;
* copy the file into `$HOME/texmf/`.


### LaTeX Beamer

The [LaTeX Beamer](https://www.ctan.org/tex-archive/macros/latex/contrib/beamer) can be used for pro­duc­ing slides.
The SARL style for LaTeX Beamer is based on the LaTeX [listings package](https://www.ctan.org/pkg/listings) and one the predefined color templates from Beamer.

The SARL style is available on [GitHub](https://raw.githubusercontent.com/sarl/sarl/master/formatting-styles/latex/sarl-beamer-listing.sty)

For using the SARL style, you should:

* download the SARL style;
* copy the file into `$HOME/texmf/`.


## Pygments

[Pygments](http://pygments.org) is a Python generic syntax highlighter suitable for use in code hosting, forums, wikis or other applications that need to prettify source code.

The SARL style is available on [GitHub](https://github.com/sarl/sarl/tree/master/formatting-styles/pygments/)

For installing the SARL style, you should:

* download all the files from the previous URL;
* open a command-line shell;
* go into the folder where your have downloaded the `setup.py` script;
* call: `python setup.py install`


## Sublime Text Editor

[Sublime Text Editor](https://www.sublimetext.com/) is a sophisticated text editor for code and markup.
This editor uses the TextMate language syntax.

The SARL style is available on [GitHub](https://raw.githubusercontent.com/sarl/sarl/master/formatting-styles/textmate/sarl.tmLanguage)

For using the SARL style, you should:

* copy the SARL style into the folder `$HOME/.config/sublime-text-3/Packages/User/SARL/`.


## TextMate Editor

[TextMate Editor](https://macromates.com/) is a sophisticated text editor for MacOS platforms.

The SARL style is available on [GitHub](https://raw.githubusercontent.com/sarl/sarl/master/formatting-styles/textmate/sarl.tmLanguage)


## Vim Editor

[Vim Editor](http://www.vim.org/) is is a highly configurable text editor built to make creating and
changing any kind of text very efficient.

The SARL style is available on [GitHub](https://raw.githubusercontent.com/sarl/sarl/master/formatting-styles/vim/sarl.vim)

For using the SARL style, you should:

* copy the SARL style into the folder `$HOME/.vim/ftdetect`.



[:Include:](../legal.inc)

