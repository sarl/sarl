/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.lang.mwe2.codebuilder.fragments;

import java.io.IOException;

import javax.inject.Inject;
import javax.inject.Singleton;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.formatting.impl.AbstractTokenStream;
import org.eclipse.xtext.resource.SaveOptions;
import org.eclipse.xtext.serializer.impl.Serializer;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.compiler.ISourceAppender;
import org.eclipse.xtext.xtext.generator.model.JavaFileAccess;
import org.eclipse.xtext.xtext.generator.model.TypeReference;

/** Generator of the abstract code builder.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class AbstractAppenderBuilderFragment extends AbstractSubCodeBuilderFragment {

	@Override
	public void generate() {
		if (getCodeBuilderConfig().isISourceAppendableEnable()) {
			generateAbstractAppender();
		}
	}

	/** Generate the abstract appender.
	 */
	protected void generateAbstractAppender() {
		final TypeReference abstractAppender = getAbstractAppenderImpl();
		StringConcatenationClient content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** Abstract implementation of an appender for the " //$NON-NLS-1$
						+ getLanguageName() + " language."); //$NON-NLS-1$
				it.newLine();
				it.append(" */"); //$NON-NLS-1$
				it.newLine();
				it.append("@SuppressWarnings(\"all\")"); //$NON-NLS-1$
				it.newLine();
				it.append("public abstract class "); //$NON-NLS-1$
				it.append(abstractAppender.getSimpleName());
				it.append(" {"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Inject.class);
				it.newLine();
				it.append("\tprivate AppenderSerializer serializer;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Build the source code and put it into the given appender."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param appender the object that permits to create the source code."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\tpublic abstract void build("); //$NON-NLS-1$
				it.append(ISourceAppender.class);
				it.append(" appender) throws "); //$NON-NLS-1$
				it.append(IOException.class);
				it.append(";"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Build the source code and put it into the given appender."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param the object to serialize"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param appender the object that permits to create the source code."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\tprotected void build("); //$NON-NLS-1$
				it.append(EObject.class);
				it.append(" object, "); //$NON-NLS-1$
				it.append(ISourceAppender.class);
				it.append(" appender) throws "); //$NON-NLS-1$
				it.append(IOException.class);
				it.append(" {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tthis.serializer.serialize(object, appender);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Singleton.class);
				it.newLine();
				it.append("\tpublic static class AppenderSerializer extends "); //$NON-NLS-1$
				it.append(Serializer.class);
				it.append(" {"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic void serialize("); //$NON-NLS-1$
				it.append(EObject.class);
				it.append(" object, "); //$NON-NLS-1$
				it.append(ISourceAppender.class);
				it.append(" appender) throws "); //$NON-NLS-1$
				it.append(IOException.class);
				it.append(" {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tserialize(object, new TokenStream(appender), "); //$NON-NLS-1$
				it.append(SaveOptions.class);
				it.append(".defaultOptions());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate static class TokenStream extends "); //$NON-NLS-1$
				it.append(AbstractTokenStream.class);
				it.append(" {"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tprivate final "); //$NON-NLS-1$
				it.append(ISourceAppender.class);
				it.append(" appender;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic TokenStream("); //$NON-NLS-1$
				it.append(ISourceAppender.class);
				it.append(" appender) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tthis.appender = appender;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic String toString() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn this.appender.toString();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic void writeHidden("); //$NON-NLS-1$
				it.append(EObject.class);
				it.append(" grammarElement, String value) throws "); //$NON-NLS-1$
				it.append(IOException.class);
				it.append(" {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (!"); //$NON-NLS-1$
				it.append(Strings.class);
				it.append(".isEmpty(value)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tSystem.out.println(value);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic void writeSemantic("); //$NON-NLS-1$
				it.append(EObject.class);
				it.append(" grammarElement, String value) throws "); //$NON-NLS-1$
				it.append(IOException.class);
				it.append(" {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (!"); //$NON-NLS-1$
				it.append(Strings.class);
				it.append(".isEmpty(value)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tSystem.out.println(value);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
			}

		};
		JavaFileAccess javaFile = getFileAccessFactory().createJavaFile(abstractAppender, content);
		javaFile.writeTo(getSrcGen());
	}

}
