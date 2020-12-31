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

package io.sarl.maven.docs;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.generator.trace.AbstractTraceRegion;
import org.eclipse.xtext.generator.trace.ILocationData;
import org.eclipse.xtext.xbase.compiler.ImportManager;
import org.eclipse.xtext.xbase.compiler.StringBuilderBasedAppendable;
import org.eclipse.xtext.xbase.compiler.output.ErrorTreeAppendable;
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;

/** A tree-based appendable that is able to trace.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 */
class TraceableTreeAppendable extends StringBuilderBasedAppendable implements ITreeAppendable {

	private final List<TraceableTreeAppendable> children = new ArrayList<>();

	private ILocationData location;

	private boolean useForDebugging = true;

	private WeakReference<TraceableTreeAppendable> parent;

	/** Constructor.
	 */
	TraceableTreeAppendable() {
		super();
	}

	/** Constructor.
	 *
	 * @param importManager the manager of the imported types.
	 */
	TraceableTreeAppendable(ImportManager importManager) {
		super(importManager);
	}

	/** Constructor.
	 *
	 * @param parent the parent appendable.
	 * @param useForDebugging indicates if the appendable is used for debugging or not.
	 * @param location the location associated to this appendable.
	 */
	protected TraceableTreeAppendable(TraceableTreeAppendable parent, boolean useForDebugging, ILocationData location) {
		super(parent.getImportManager());
		this.parent = new WeakReference<>(parent);
		this.location = location;
		this.useForDebugging = useForDebugging;
	}

	private TraceableTreeAppendable getParent() {
		return this.parent == null ? null : this.parent.get();
	}

	@Override
	public String toString() {
		final TraceableTreeAppendable p = getParent();
		if (p != null) {
			return p.toString();
		}
		return super.toString();
	}

	@Override
	public ITreeAppendable append(JvmType type) {
		final TraceableTreeAppendable p = getParent();
		if (p != null) {
			p.append(type);
		} else {
			super.append(type);
		}
		return this;
	}

	@Override
	public ITreeAppendable append(Class<?> type) {
		final TraceableTreeAppendable p = getParent();
		if (p != null) {
			p.append(type);
		} else {
			super.append(type);
		}
		return this;
	}

	@Override
	public ITreeAppendable append(CharSequence string) {
		final TraceableTreeAppendable p = getParent();
		if (p != null) {
			p.append(string);
		} else {
			super.append(string);
		}
		return this;
	}

	@Override
	public ITreeAppendable append(LightweightTypeReference typeRef) {
		final TraceableTreeAppendable p = getParent();
		if (p != null) {
			p.append(typeRef);
		} else {
			super.append(typeRef);
		}
		return this;
	}

	@Override
	public ITreeAppendable decreaseIndentation() {
		final TraceableTreeAppendable p = getParent();
		if (p != null) {
			p.decreaseIndentation();
		} else {
			super.decreaseIndentation();
		}
		return this;
	}

	@Override
	public ITreeAppendable increaseIndentation() {
		final TraceableTreeAppendable p = getParent();
		if (p != null) {
			p.increaseIndentation();
		} else {
			super.increaseIndentation();
		}
		return this;
	}

	@Override
	public ITreeAppendable newLine() {
		final TraceableTreeAppendable p = getParent();
		if (p != null) {
			p.newLine();
		} else {
			super.newLine();
		}
		return this;
	}

	@Override
	public AbstractTraceRegion getTraceRegion() {
		if (this.location == null) {
			throw new IllegalStateException("appendable was used without tracing");
		}
		return new AppendableBasedTraceRegion(this);
	}

	private void getTraceRegions(List<AbstractTraceRegion> regions) {
		if (this.location != null) {
			regions.add(getTraceRegion());
		}
		for (final TraceableTreeAppendable child : this.children) {
			child.getTraceRegions(regions);
		}
	}

	public List<AbstractTraceRegion> getTraceRegions() {
		final List<AbstractTraceRegion> regions = new ArrayList<>();
		getTraceRegions(regions);
		return regions;
	}

	/**
	 * Access the children of the {@link TraceableTreeAppendable}.
	 *
	 * @return the children of this appendable.
	 */
	public List<? extends ITreeAppendable> getChildren() {
		return this.children;
	}

	@Override
	public ITreeAppendable trace(ILocationData location) {
		return trace(location, this.useForDebugging);
	}

	@Override
	public ITreeAppendable trace(ILocationData location, boolean useForDebugging) {
		if (useForDebugging) {
			final TraceableTreeAppendable child = new TraceableTreeAppendable(
					this, useForDebugging, location);
			this.children.add(child);
			return child;
		}
		return this;
	}

	@Override
	public ITreeAppendable trace(EObject object, boolean useForDebugging) {
		throw new UnsupportedOperationException();
	}

	@Override
	public ITreeAppendable trace(Iterable<? extends EObject> objects) {
		throw new UnsupportedOperationException();
	}

	@Override
	public ITreeAppendable trace(EObject object, EStructuralFeature feature, int indexInList) {
		throw new UnsupportedOperationException();
	}

	@Override
	public ITreeAppendable trace(EObject object) {
		throw new UnsupportedOperationException();
	}

	@Override
	public ErrorTreeAppendable errorChild() {
		throw new UnsupportedOperationException();
	}

	/** REgion for a tree-based appendable that is able to trace.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.11
	 */
	protected static class AppendableBasedTraceRegion extends AbstractTraceRegion {

		private final int offset;

		private final int lineNumber;

		private final int length;

		private final int endLineNumber;

		private final boolean useForDebugging;

		private final ILocationData location;

		public AppendableBasedTraceRegion(TraceableTreeAppendable delegate) {
			super(null);
			this.location = delegate.location;
			this.offset = this.location.getOffset();
			this.length = this.location.getLength();
			this.lineNumber = this.location.getLineNumber();
			this.endLineNumber = this.location.getEndLineNumber();
			this.useForDebugging = delegate.useForDebugging;
		}

		@Override
		public boolean isUseForDebugging() {
			return this.useForDebugging;
		}

		@Override
		public int getMyLength() {
			return this.length;
		}

		@Override
		public int getMyOffset() {
			return this.offset;
		}

		@Override
		public int getMyLineNumber() {
			return this.lineNumber;
		}

		@Override
		public int getMyEndLineNumber() {
			return this.endLineNumber;
		}

		@Override
		public List<ILocationData> getAssociatedLocations() {
			return Collections.singletonList(this.location);
		}

		@Override
		public AppendableBasedTraceRegion getRoot() {
			return (AppendableBasedTraceRegion) super.getRoot();
		}

	}

}
