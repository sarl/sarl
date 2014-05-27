/*
 * Copyright 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.lang.signature;

import io.sarl.lang.sarl.FormalParameter;

import java.util.Iterator;
import java.util.Map;

import org.eclipse.emf.common.util.EList;

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

	private final Map<SignatureKey,EList<InferredStandardParameter>> inferredParameters; 
	private final EList<FormalParameter> parameters;
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
			EList<FormalParameter> parameters,
			SignatureKey parameterKey,
			Map<SignatureKey,EList<InferredStandardParameter>> inferredParameters) {
		this.key = key;
		this.parameters = parameters;
		this.parameterKey = parameterKey;
		this.inferredParameters = inferredParameters;
	}

	/** {@inheritDoc}
	 */
	public ActionNameKey getKey() {
		return this.key;
	}

	/**
	 * {@inheritDoc}
	 */
	public Map<SignatureKey,EList<InferredStandardParameter>> getInferredSignatures() {
		return this.inferredParameters;
	}

	/** {@inheritDoc}
	 */
	public EList<FormalParameter> getFormalParameters() {
		return this.parameters;
	}
	
	/** {@inheritDoc}
	 */
	public SignatureKey getFormalParameterKey() {
		return this.parameterKey;
	}

	/** {@inheritDoc}
	 */
	public boolean isVarargs() {
		return this.parameterKey.isVarargs();
	}

	/** {@inheritDoc}
	 */
	public Iterator<EList<InferredStandardParameter>> iterator() {
		return this.inferredParameters.values().iterator();
	}

	/** {@inheritDoc}
	 */
	public Iterable<SignatureKey> signatureKeys() {
		return new Iterable<SignatureKey>() {
			public Iterator<SignatureKey> iterator() {
				return new SignatureKeyIterator(
						getFormalParameterKey(),
						getInferredSignatures().keySet().iterator());
			}
		};
	}
	
	/** {@inheritDoc}
	 */
	@Override
	public String toString() {
		if (!this.parameters.isEmpty()) {
			StringBuilder b = new StringBuilder();
			Iterator<FormalParameter> it = this.parameters.iterator();
			FormalParameter p = it.next();
			b.append(p.getName());
			b.append(" : "); //$NON-NLS-1$
			b.append(p.getParameterType().getIdentifier());
			while (it.hasNext()) {
				p = it.next();
				b.append(", "); //$NON-NLS-1$
				b.append(p.getName());
				b.append(" : "); //$NON-NLS-1$
				b.append(p.getParameterType().getIdentifier());
			}
			return b.toString();
		}
		return ""; //$NON-NLS-1$
	}

	/** {@inheritDoc}
	 */
	public String toString(String functionName) {
		if (!this.parameters.isEmpty()) {
			StringBuilder b = new StringBuilder();
			b.append(functionName);
			b.append("("); //$NON-NLS-1$
			Iterator<FormalParameter> it = this.parameters.iterator();
			FormalParameter p = it.next();
			b.append(p.getName());
			b.append(" : "); //$NON-NLS-1$
			b.append(p.getParameterType().getIdentifier());
			while (it.hasNext()) {
				p = it.next();
				b.append(", "); //$NON-NLS-1$
				b.append(p.getName());
				b.append(" : "); //$NON-NLS-1$
				b.append(p.getParameterType().getIdentifier());
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
		
		/** {@inheritDoc}
		 */
		public boolean hasNext() {
			return this.first!=null
					|| this.it.hasNext();
		}
		
		/** {@inheritDoc}
		 */
		public SignatureKey next() {
			if (this.first!=null) {
				SignatureKey n = this.first;
				this.first = null;
				return n;
			}
			return this.it.next();
		}
		
		/** {@inheritDoc}
		 */
		public void remove() {
			throw new UnsupportedOperationException();
		}
		
	}

}
