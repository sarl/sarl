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

package io.sarl.lang.sarl.actionprototype;

import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.google.common.base.Strings;

/**
 * Provides additional function signatures according the semantic
 * associated to the parameter's default values.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class DefaultInferredPrototype implements InferredPrototype {

	private final List<InferredStandardParameter> originalParameters;

	private final Map<ActionParameterTypes, List<InferredStandardParameter>> inferredParameters;

	private final FormalParameterProvider parameters;

	private final ActionParameterTypes parameterKey;

	private final QualifiedActionName key;

	/** Constructor.
	 * @param key key used to store this signature into a {@link IActionPrototypeProvider}.
	 * @param parameters list of formal parameters.
	 * @param parameterKey key for the formal parameters.
	 * @param originalParameters original parameters.
	 * @param inferredParameters list of inferred parameters.
	 */
	protected DefaultInferredPrototype(
			QualifiedActionName key,
			FormalParameterProvider parameters,
			ActionParameterTypes parameterKey,
			List<InferredStandardParameter> originalParameters,
			Map<ActionParameterTypes, List<InferredStandardParameter>> inferredParameters) {
		this.key = key;
		this.parameters = parameters;
		this.parameterKey = parameterKey;
		this.originalParameters = originalParameters;
		this.inferredParameters = inferredParameters;
	}

	@Override
	public QualifiedActionName getActionName() {
		return this.key;
	}

	@Override
	public Map<ActionParameterTypes, List<InferredStandardParameter>> getInferredParameterTypes() {
		return this.inferredParameters;
	}

	@Override
	public List<InferredStandardParameter> getOriginalParameterTypes() {
		return this.originalParameters;
	}

	@Override
	public FormalParameterProvider getFormalParameters() {
		return this.parameters;
	}

	@Override
	public ActionParameterTypes getFormalParameterTypes() {
		return this.parameterKey;
	}

	@Override
	public boolean isVarargs() {
		return this.parameterKey.isVarArg();
	}

	@Override
	public Iterable<ActionParameterTypes> getParameterTypeAlternatives() {
		return new Iterable<ActionParameterTypes>() {
			@Override
			public Iterator<ActionParameterTypes> iterator() {
				return new SignatureKeyIterator(
						getFormalParameterTypes(),
						getInferredParameterTypes().keySet().iterator());
			}
		};
	}

	private void toParameterString(StringBuilder buffer, int param, boolean isVarArg) {
		final String name = this.parameters.getFormalParameterName(param);
		buffer.append((!Strings.isNullOrEmpty(name)) ? name : null);
		buffer.append(" : "); //$NON-NLS-1$
		final String type = this.parameters.getFormalParameterType(param, isVarArg);
		buffer.append((!Strings.isNullOrEmpty(type)) ? type : null);
	}

	private void toParameterString(StringBuilder buffer) {
		if (this.parameters.getFormalParameterCount() > 0) {
			final int lastParamIndex = this.parameters.getFormalParameterCount() - 1;
			for (int i = 0; i < lastParamIndex; ++i) {
				toParameterString(buffer, i, false);
				buffer.append(", "); //$NON-NLS-1$
			}
			toParameterString(buffer, lastParamIndex, isVarargs());
		}
	}

	@Override
	public String toString() {
		final StringBuilder b = new StringBuilder();
		toParameterString(b);
		return b.toString();
	}

	@Override
	public String toString(String functionName) {
		final StringBuilder b = new StringBuilder();
		b.append(functionName);
		if (this.parameters.getFormalParameterCount() > 0) {
			b.append("("); //$NON-NLS-1$
			toParameterString(b);
			b.append(")"); //$NON-NLS-1$
		}
		return b.toString();
	}

	/** Iterator of the types of the action parameters.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class SignatureKeyIterator implements Iterator<ActionParameterTypes> {

		private ActionParameterTypes first;

		private final Iterator<ActionParameterTypes> it;

		SignatureKeyIterator(ActionParameterTypes first, Iterator<ActionParameterTypes> it) {
			this.first = first;
			this.it = it;
		}

		@Override
		public boolean hasNext() {
			return this.first != null || this.it.hasNext();
		}

		@Override
		public ActionParameterTypes next() {
			if (this.first != null) {
				final ActionParameterTypes first = this.first;
				this.first = null;
				return first;
			}
			return this.it.next();
		}

		@Override
		public void remove() {
			throw new UnsupportedOperationException();
		}

	}

}
