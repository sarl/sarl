/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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

package io.sarl.lang.generator.extra;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.generator.AbstractGenerator;
import org.eclipse.xtext.generator.IFileSystemAccess2;
import org.eclipse.xtext.generator.IGeneratorContext;
import org.eclipse.xtext.util.CancelIndicator;
import org.eclipse.xtext.util.PolymorphicDispatcher;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.compiler.AbstractStringBuilderBasedAppendable;
import org.eclipse.xtext.xbase.compiler.IAppendable;

/** Abstract implementation for the generator from SARL to an extra language.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public abstract class AbstractExtraGenerator extends AbstractGenerator {

	private final PolymorphicDispatcher<Void> beforeDispatcher;

	private final PolymorphicDispatcher<Void> generateDispatcher;

	private final PolymorphicDispatcher<Void> generateDispatcher2;

	private final PolymorphicDispatcher<Void> afterDispatcher;

	/** Construct the generator.
	 */
	public AbstractExtraGenerator() {
		this.beforeDispatcher = new PolymorphicDispatcher<Void>(
				"_before", 2, 2, //$NON-NLS-1$
				Collections.singletonList(this)) {
			@Override
			protected Void handleNoSuchMethod(Object... params) {
				return null;
			}
		};
		this.generateDispatcher = new PolymorphicDispatcher<>(
				"_generate", 2, 2, //$NON-NLS-1$
				Collections.singletonList(this));
		this.generateDispatcher2 = new PolymorphicDispatcher<>(
				"_generate", 3, 3, //$NON-NLS-1$
				Collections.singletonList(this));
		this.afterDispatcher = new PolymorphicDispatcher<Void>(
				"_after", 2, 2, //$NON-NLS-1$
				Collections.singletonList(this)) {
			@Override
			protected Void handleNoSuchMethod(Object... params) {
				return null;
			}
		};
	}

	@Override
	public void beforeGenerate(Resource input, IFileSystemAccess2 fsa, IGeneratorContext context) {
		final GeneratorContext generatorContext = createGeneratorContext(fsa, context);
		final EList<EObject> contents = input.getContents();
		for (final EObject obj : contents) {
			if (canGenerateFor(obj)) {
				before(obj, generatorContext);
			}
		}
	}

	@Override
	public void doGenerate(Resource input, IFileSystemAccess2 fsa, IGeneratorContext context) {
		final GeneratorContext generatorContext = createGeneratorContext(fsa, context);
		final EList<EObject> contents = input.getContents();
		for (final EObject obj : contents) {
			if (canGenerateFor(obj)) {
				generate(obj, generatorContext);
			}
		}
	}

	@Override
	public void afterGenerate(Resource input, IFileSystemAccess2 fsa, IGeneratorContext context) {
		final GeneratorContext generatorContext = createGeneratorContext(fsa, context);
		final EList<EObject> contents = input.getContents();
		for (final EObject obj : contents) {
			if (canGenerateFor(obj)) {
				after(obj, generatorContext);
			}
		}
	}

	/** Create the generator context for this generator.
	 *
	 * @param fsa the file system access.
	 * @param context the global context.
	 * @return the context.
	 */
	@SuppressWarnings("static-method")
	protected GeneratorContext createGeneratorContext(IFileSystemAccess2 fsa, IGeneratorContext context) {
		return new GeneratorContext(context, fsa);
	}

	/** Replies if this generator can generate resources from the given element.
	 *
	 * @param object the object for which the generation was queried.
	 * @return {@code true} for generating the resources, {@code false} for ignoring the object.
	 */
	@SuppressWarnings("static-method")
	protected boolean canGenerateFor(EObject object) {
		return !(object instanceof JvmIdentifiableElement);
	}

	/** Do something before the generation of the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 */
	protected void before(EObject object, GeneratorContext context) {
		this.beforeDispatcher.invoke(object, context);
	}

	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 */
	protected void generate(EObject object, GeneratorContext context) {
		this.generateDispatcher.invoke(object, context);
	}

	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 * @param appendable the target for the generated content.
	 */
	protected void generate(EObject object, GeneratorContext context, IAppendable appendable) {
		this.generateDispatcher2.invoke(object, context, appendable);
	}

	/** Do something after the generation of the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 */
	protected void after(EObject object, GeneratorContext context) {
		this.afterDispatcher.invoke(object, context);
	}

	/** The generator from SARL to the Python language.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	protected static class GeneratorContext implements IGeneratorContext {

		private final IGeneratorContext delegate;

		private IFileSystemAccess2 fileSystemAccess;

		private Map<String, Object> temporaryData;

		/** Create the context for the given delegate.
		 *
		 * @param delegate the delegate.
		 * @param fileSystemAccess the file system access.
		 */
		public GeneratorContext(IGeneratorContext delegate, IFileSystemAccess2 fileSystemAccess) {
			this.delegate = delegate;
			this.fileSystemAccess = fileSystemAccess;
		}

		@Override
		public CancelIndicator getCancelIndicator() {
			return this.delegate.getCancelIndicator();
		}

		/** Replies the delegate.
		 *
		 * @return the delegate.
		 */
		public IGeneratorContext getDelegate() {
			return this.delegate;
		}

		/** Replies the file system access.
		 *
		 * @return the file system access.
		 */
		public IFileSystemAccess2 getFileSystemAccess() {
			return this.fileSystemAccess;
		}

		/** Replies the stored data with the given identifier.
		 * If the data was not found, the default value is replied.
		 *
		 * @param <T> the type of the data.
		 * @param id the identifier.
		 * @param type the type of the data.
		 * @param defaultValue the default value.
		 * @return the data or the default value.
		 */
		public <T> T getData(String id, Class<T> type, T defaultValue) {
			if (Strings.isEmpty(id) || this.temporaryData == null) {
				return defaultValue;
			}
			final Object data = this.temporaryData.get(id);
			try {
				return type.cast(data);
			} catch (Throwable exception) {
				return defaultValue;
			}
		}

		/** Replies the stored data with the given identifier.
		 * If the data was not found, the default value is replied.
		 *
		 * @param <T> the type of the data.
		 * @param id the identifier.
		 * @param type the type of the data.
		 * @return the data or the default value.
		 */
		public <T> T getData(String id, Class<T> type) {
			return getData(id, type);
		}

		/** Store data with the given identifier.
		 *
		 * @param id the identifier.
		 * @param value the value.
		 */
		public void setData(String id, Object value) {
			if (Strings.isEmpty(id)) {
				return;
			}
			if (value == null) {
				if (this.temporaryData != null) {
					this.temporaryData.remove(id);
				}
				return;
			}
			if (this.temporaryData != null) {
				this.temporaryData = new TreeMap<>();
			}
			this.temporaryData.put(id, value);
		}

		/** Clear all stored data.
		 */
		public void clearData() {
			this.temporaryData = null;
		}

	}

	/** Appendable for extra languages.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	protected static class ExtraLanguageAppendable extends AbstractStringBuilderBasedAppendable {

		/** Constructor.
		 */
		public ExtraLanguageAppendable() {
			super(false);
		}

		/** Constructor.
		 *
		 * @param indentation the indentation string.
		 * @param lineSeparator the line separator string.
		 */
		public ExtraLanguageAppendable(String indentation, String lineSeparator) {
			super(indentation, lineSeparator, false);
		}

		@Override
		protected void appendType(JvmType type, StringBuilder builder) {
			builder.append(type.getQualifiedName());
		}

		@Override
		protected void appendType(Class<?> type, StringBuilder builder) {
			builder.append(type.getName());
		}

		/** {@inheritDoc}.
		 *
		 * @deprecated No replacement.
		 */
		@Override
		@Deprecated
		public List<String> getImports() {
			return Collections.emptyList();
		}

	}

}


