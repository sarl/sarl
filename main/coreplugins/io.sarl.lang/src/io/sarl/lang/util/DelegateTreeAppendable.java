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

package io.sarl.lang.util;

import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.generator.trace.AbstractTraceRegion;
import org.eclipse.xtext.generator.trace.ILocationData;
import org.eclipse.xtext.generator.trace.TraceNotFoundException;
import org.eclipse.xtext.xbase.compiler.GeneratorConfig;
import org.eclipse.xtext.xbase.compiler.output.ErrorTreeAppendable;
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;

/**
 * A delegating tree appendable.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8.6
 */
public class DelegateTreeAppendable implements ITreeAppendable {

	private final ITreeAppendable delegate;

	/** Constructor.
	 *
	 * @param delegate the appendable to delegate to.
	 */
	public DelegateTreeAppendable(ITreeAppendable delegate) {
		assert delegate != null;
		this.delegate = delegate;
	}

	/** {@inheritDoc}
	 * @deprecated imports are handled by external components.
	 */
	@Override
	@Deprecated
	public List<String> getImports() {
		return this.delegate.getImports();
	}

	@Override
	public void openScope() {
		this.delegate.openScope();
	}

	@Override
	public void openPseudoScope() {
		this.delegate.openPseudoScope();
	}

	@Override
	public String declareVariable(Object key, String proposedName) {
		return this.delegate.declareVariable(key, proposedName);
	}

	@Override
	public String declareSyntheticVariable(Object key, String proposedName) {
		return this.delegate.declareSyntheticVariable(key, proposedName);
	}

	@Override
	public String declareUniqueNameVariable(Object key, String proposedName) {
		return this.delegate.declareUniqueNameVariable(key, proposedName);
	}

	@Override
	public String getName(Object key) {
		return this.delegate.getName(key);
	}

	@Override
	public String removeName(Object key) throws IllegalStateException {
		return this.delegate.removeName(key);
	}

	@Override
	public boolean hasName(Object key) {
		return this.delegate.hasName(key);
	}

	@Override
	public Object getObject(String name) {
		return this.delegate.getObject(name);
	}

	@Override
	public boolean hasObject(String name) {
		return this.delegate.hasObject(name);
	}

	@Override
	public void closeScope() {
		this.delegate.closeScope();
	}

	@Override
	public int length() {
		return this.delegate.length();
	}

	@Override
	public String getContent() {
		return this.delegate.getContent();
	}

	@Override
	public GeneratorConfig getGeneratorConfig() {
		return this.delegate.getGeneratorConfig();
	}

	@Override
	public boolean isJava() {
		return this.delegate.isJava();
	}

	@Override
	public AbstractTraceRegion getTraceRegion() throws TraceNotFoundException {
		return this.delegate.getTraceRegion();
	}

	@Override
	public ITreeAppendable trace(EObject object, boolean useForDebugging) {
		return createDelegateToChild(this.delegate.trace(object, useForDebugging));
	}

	@Override
	public ITreeAppendable trace(EObject object) {
		return createDelegateToChild(this.delegate.trace(object));
	}

	@Override
	public ITreeAppendable trace(Iterable<? extends EObject> objects) {
		return createDelegateToChild(this.delegate.trace(objects));
	}

	@Override
	public ITreeAppendable trace(EObject object, EStructuralFeature feature, int indexInList) {
		return createDelegateToChild(this.delegate.trace(object, feature, indexInList));
	}

	@Override
	public ITreeAppendable trace(ILocationData location) {
		return createDelegateToChild(this.delegate.trace(location));
	}

	@Override
	public ITreeAppendable trace(ILocationData location, boolean useForDebugging) {
		return createDelegateToChild(this.delegate.trace(location, useForDebugging));
	}

	/** Create a child appendable.
	 *
	 * @param child the child to delegate to.
	 * @return the child.
	 */
	@SuppressWarnings("static-method")
	protected ITreeAppendable createDelegateToChild(ITreeAppendable child) {
		return new DelegateTreeAppendable(child);
	}

	@Override
	public ErrorTreeAppendable errorChild() {
		return this.delegate.errorChild();
	}

	@Override
	public ITreeAppendable append(JvmType type) {
		this.delegate.append(type);
		return this;
	}

	@Override
	public ITreeAppendable append(Class<?> type) {
		this.delegate.append(type);
		return this;
	}

	@Override
	public ITreeAppendable append(LightweightTypeReference typeRef) {
		this.delegate.append(typeRef);
		return this;
	}

	@Override
	public ITreeAppendable append(CharSequence content) {
		this.delegate.append(content);
		return this;
	}

	@Override
	public ITreeAppendable decreaseIndentation() {
		this.delegate.decreaseIndentation();
		return this;
	}

	@Override
	public ITreeAppendable increaseIndentation() {
		this.delegate.increaseIndentation();
		return this;
	}

	@Override
	public ITreeAppendable newLine() {
		this.delegate.newLine();
		return this;
	}

	@Override
	public String toString() {
		return this.delegate.toString();
	}
}
