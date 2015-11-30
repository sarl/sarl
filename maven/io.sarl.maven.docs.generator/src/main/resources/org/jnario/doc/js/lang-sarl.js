PR['registerLangHandler'](
   PR['createSimpleLexer'](
      [
         [PR['PR_PLAIN'], /^[\t\n\r \xA0]+/, null, '\t\n\r \xA0'],
         [PR['PR_PUNCTUATION'], /^[!#%&()*+,\-./:;<=>?@\[\\\]^{|}]+/, null, '!#%&()*+,-./:;<=>?@[\\]^{|}'],
      ],
      [
         [PR['PR_STRING'], /^(?:"(?:[^\"\\]|\\.)*"|'(?!\'\')(?:[^\'\\]|\\.)*')/],
         [PR['PR_STRING'], /^'(?:[^\r\n\\']|\\(?:'|[^\r\n']+))'/],
         [PR['PR_LITERAL'], /^'[a-zA-Z_$][\w$]*(?!['$\w])/],
         [PR['PR_KEYWORD'], /^(?:abstract|agent|annotation|as|behavior|boolean|byte|capacity|case|catch|char|class|def|default|dispatch|do|double|else|enum|event|extends|extension|final|finally|fires|float|for|if|implements|import|instanceof|int|interface|long|native|new|on|override|package|private|protected|public|requires|return|short|skill|static|strictfp|super|switch|synchronized|throw|throws|transient|try|typeof|uses|val|var|void|volatile|while|with)\b/],
         [PR['PR_LITERAL'], /^(?:false|it|null|occurrence|this|true)\b/],
         [PR['PR_LITERAL'], /^(?:(?:0(?:[0-7]+|X[0-9A-F]+))L?|(?:(?:0|[1-9][0-9]*)(?:(?:\.[0-9]+)?(?:E[+\-]?[0-9]+)?F?|L?))|\\.[0-9]+(?:E[+\-]?[0-9]+)?F?)/i],
         [PR['PR_TYPE'], /^[$_]*[A-Z][_$A-Z0-9]*[a-z][\w$]*/],
         [PR['PR_PLAIN'], /^[$a-zA-Z_][\w$]*/],
         [PR['PR_COMMENT'], /^\/(?:\/.*|\*(?:\/|\**[^*/])*(?:\*+\/?)?)/],
         [PR['PR_PUNCTUATION'], /^(?:\.+|\/)/]
      ]),
   ['sarl']);
