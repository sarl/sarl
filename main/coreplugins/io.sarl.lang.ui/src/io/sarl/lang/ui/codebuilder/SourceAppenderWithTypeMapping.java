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

package io.sarl.lang.ui.codebuilder;

import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.xbase.compiler.ISourceAppender;
import org.eclipse.xtext.xbase.typesystem.references.FunctionTypeReference;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;

import io.sarl.lang.services.SARLGrammarKeywordAccess;

/** A source appender that is mapping the Java function types (Function0...) to the equivalent SARL notation.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
class SourceAppenderWithTypeMapping implements ISourceAppender {

	private final ISourceAppender source;

	private final SARLGrammarKeywordAccess keywords;

	/** Constructor.
	 *
	 * @param source the source appender.
	 * @param keywords the keyword accessor.
	 */
	SourceAppenderWithTypeMapping(ISourceAppender source, SARLGrammarKeywordAccess keywords) {
		assert source != null;
		assert keywords != null;
		this.source = source;
		this.keywords = keywords;
	}

	@Override
	public ISourceAppender append(CharSequence string) {
		this.source.append(string);
		return this;
	}

	@Override
	public ISourceAppender append(JvmType type) {
		this.source.append(type);
		return this;
	}

	@Override
	public ISourceAppender append(LightweightTypeReference typeRef) {
		if (typeRef.isFunctionType()) {
			final FunctionTypeReference functionReference = typeRef.getAsFunctionTypeReference();
			this.source.append(this.keywords.getLeftParenthesisKeyword());
			boolean first = true;
			for (final LightweightTypeReference parameter : functionReference.getParameterTypes()) {
				if (first) {
					first = false;
				} else {
					this.source.append(","); //$NON-NLS-1$
				}
				append(parameter);
			}
			this.source.append(this.keywords.getRightParenthesisKeyword());
			this.source.append(this.keywords.getEqualsSignGreaterThanSignKeyword());
			append(functionReference.getReturnType());
			return this;
		}
		this.source.append(typeRef);
		return this;
	}

	@Override
	public ISourceAppender newLine() {
		this.source.newLine();
		return this;
	}

	@Override
	public ISourceAppender increaseIndentation() {
		this.source.increaseIndentation();
		return this;
	}

	@Override
	public ISourceAppender decreaseIndentation() {
		this.source.decreaseIndentation();
		return this;
	}

	@Override
	public boolean isJava() {
		return this.source.isJava();
	}

	@Override
	public String toString() {
		return this.source.toString();
	}

}
