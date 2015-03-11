/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.lang.signature;

import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.xtend.core.xtend.XtendParameter;

/**
 * Provides additional function signatures according the semantic
 * associated to the parameter's default values.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class DefaultInferredActionSignature implements InferredActionSignature {

	private final Map<SignatureKey, List<InferredStandardParameter>> inferredParameters;
	private final List<? extends XtendParameter> parameters;
	private final SignatureKey parameterKey;
	private final ActionNameKey key;

	/**
	 * @param key - key used to store this signature into a {@link ActionSignatureProvider}.
	 * @param parameters - list of formal parameters.
	 * @param parameterKey - key for the formal parameters.
	 * @param inferredParameters - list of inferred parameters.
	 */
	protected DefaultInferredActionSignature(
			ActionNameKey key,
			List<? extends XtendParameter> parameters,
			SignatureKey parameterKey,
			Map<SignatureKey, List<InferredStandardParameter>> inferredParameters) {
		this.key = key;
		this.parameters = parameters;
		this.parameterKey = parameterKey;
		this.inferredParameters = inferredParameters;
	}

	@Override
	public ActionNameKey getKey() {
		return this.key;
	}

	@Override
	public Map<SignatureKey, List<InferredStandardParameter>> getInferredSignatures() {
		return this.inferredParameters;
	}

	@Override
	public List<? extends XtendParameter> getFormalParameters() {
		return this.parameters;
	}

	@Override
	public SignatureKey getFormalParameterKey() {
		return this.parameterKey;
	}

	@Override
	public boolean isVarargs() {
		return this.parameterKey.isVarargs();
	}

	@Override
	public Iterator<List<InferredStandardParameter>> iterator() {
		return this.inferredParameters.values().iterator();
	}

	@Override
	public Iterable<SignatureKey> signatureKeys() {
		return new Iterable<SignatureKey>() {
			@Override
			public Iterator<SignatureKey> iterator() {
				return new SignatureKeyIterator(
						getFormalParameterKey(),
						getInferredSignatures().keySet().iterator());
			}
		};
	}

	@Override
	public String toString() {
		if (!this.parameters.isEmpty()) {
			StringBuilder b = new StringBuilder();
			Iterator<? extends XtendParameter> it = this.parameters.iterator();
			XtendParameter p = it.next();
			b.append((p.getName() != null) ? p.getName() : null);
			b.append(" : "); //$NON-NLS-1$
			b.append((p.getParameterType() != null) ? p.getParameterType().getIdentifier() : null);
			while (it.hasNext()) {
				p = it.next();
				b.append(", "); //$NON-NLS-1$
				b.append((p.getName() != null) ? p.getName() : null);
				b.append(" : "); //$NON-NLS-1$
				b.append((p.getParameterType() != null) ? p.getParameterType().getIdentifier() : null);
			}
			return b.toString();
		}
		return ""; //$NON-NLS-1$
	}

	@Override
	public String toString(String functionName) {
		if (!this.parameters.isEmpty()) {
			StringBuilder b = new StringBuilder();
			b.append(functionName);
			b.append("("); //$NON-NLS-1$
			Iterator<? extends XtendParameter> it = this.parameters.iterator();
			XtendParameter p = it.next();
			b.append((p.getName() != null) ? p.getName() : null);
			b.append(" : "); //$NON-NLS-1$
			b.append((p.getParameterType() != null) ? p.getParameterType().getIdentifier() : null);
			while (it.hasNext()) {
				p = it.next();
				b.append(", "); //$NON-NLS-1$
				b.append((p.getName() != null) ? p.getName() : null);
				b.append(" : "); //$NON-NLS-1$
				b.append((p.getParameterType() != null) ? p.getParameterType().getIdentifier() : null);
			}
			b.append(")"); //$NON-NLS-1$
			return b.toString();
		}
		return ""; //$NON-NLS-1$
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class SignatureKeyIterator implements Iterator<SignatureKey> {

		private SignatureKey first;
		private final Iterator<SignatureKey> it;

		public SignatureKeyIterator(SignatureKey first, Iterator<SignatureKey> it) {
			this.first = first;
			this.it = it;
		}

		@Override
		public boolean hasNext() {
			return this.first != null || this.it.hasNext();
		}

		@Override
		public SignatureKey next() {
			if (this.first != null) {
				SignatureKey n = this.first;
				this.first = null;
				return n;
			}
			return this.it.next();
		}

		@Override
		public void remove() {
			throw new UnsupportedOperationException();
		}

	}

}
