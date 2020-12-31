/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2021 the original authors or authors.
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

package io.sarl.lang.mwe2.externalspec;

import java.text.MessageFormat;

import org.eclipse.xtext.xbase.compiler.ISourceAppender;

/** Appendable that provides high level methods.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public interface IStyleAppendable extends ISourceAppender {

	/** Append a comment.
	 *
	 * @param text the text.
	 * @param parameters the parameters to pass to {@link MessageFormat}
	 */
	void appendComment(String text, Object... parameters);

	/** Compute a text formatted with {@link MessageFormat}.
	 *
	 * @param text the text.
	 * @param parameters the parameters to pass to {@link MessageFormat}.
	 * @return the formatted text.
	 */
	default String applyFormat(String text, Object... parameters) {
		if (parameters.length > 0) {
			return MessageFormat.format(text, parameters);
		}
		return text;
	}

	/** Append a text formatted with {@link MessageFormat}.
	 *
	 * @param text the text.
	 * @param parameters the parameters to pass to {@link MessageFormat}
	 */
	default void append(String text, Object... parameters) {
		append(applyFormat(text, parameters));
	}

	/** Append a text formatted with {@link MessageFormat} followed by a newline.
	 *
	 * @param text the text.
	 * @param parameters the parameters to pass to {@link MessageFormat}
	 */
	default void appendNl(String text, Object... parameters) {
		append(text, parameters);
		newLine();
	}

	/** Append a text followed by a newline.
	 *
	 * @param text the text.
	 */
	default void appendNl(String text) {
		append(text);
		newLine();
	}

	/** Append a standard header.
	 */
	void appendHeader();

}
