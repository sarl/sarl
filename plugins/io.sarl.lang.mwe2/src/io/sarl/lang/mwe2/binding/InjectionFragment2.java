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

package io.sarl.lang.mwe2.binding;

import java.lang.ref.WeakReference;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import com.ibm.icu.text.MessageFormat;
import org.apache.log4j.Logger;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xtext.generator.AbstractXtextGeneratorFragment;
import org.eclipse.xtext.xtext.generator.model.GuiceModuleAccess;
import org.eclipse.xtext.xtext.generator.model.GuiceModuleAccess.BindKey;
import org.eclipse.xtext.xtext.generator.model.GuiceModuleAccess.BindValue;
import org.eclipse.xtext.xtext.generator.model.GuiceModuleAccess.Binding;
import org.eclipse.xtext.xtext.generator.model.TypeReference;

/**
 * A {@link AbstractXtextGeneratorFragment} that enables to create user-defined
 * injections.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class InjectionFragment2 extends AbstractXtextGeneratorFragment {

	/** Logger.
	 */
	protected static final Logger LOG = Logger.getLogger(InjectionFragment2.class);

	private String name = getClass().getName();
	
	private String comment;
	
	private boolean overrideAll;

	private final List<BindingElement> rtBindingElements = new ArrayList<>();

	private final List<BindingElement> uiBindingElements = new ArrayList<>();

	/** Set if all the injection definitions are assumed to accept overriding of
	 * previously defined elements.
	 *
	 * @param override <code>true</code> for accepting overriding.
	 */
	public void setOverrideAll(boolean override) {
		this.overrideAll = override;
	}
	
	/** Replies if all the injection definitions are assumed to accept overriding of
	 * previously defined elements.
	 *
	 * @return <code>true</code> for accepting overriding.
	 */
	public boolean isOverrideAll() {
		return this.overrideAll;
	}

	/** Change the comment for the injection fragment.
	 *
	 * @param comment the comment for the fragment.
	 */
	public void setComment(String comment) {
		this.comment = comment;
		if (!Strings.isEmpty(comment)) {
			this.name = MessageFormat.format("{0} [{1}]", getClass().getName(), comment); //$NON-NLS-1$
		} else {
			this.name = getClass().getName();
		}
	}

	/** Replies the name of the injection fragment.
	 *
	 * @return the name of the fragment.
	 */
	@Pure
	public String getName() {
		return this.name;
	}

	/** Replies the comment for the injection fragment.
	 *
	 * @return the comment for the fragment.
	 */
	@Pure
	public String getComment() {
		return this.comment;
	}

	/** Add runtime binding element.
	 *
	 * @param element the runtime binding element.
	 */
	public void addRuntime(BindingElement element) {
		if (element != null) {
			this.rtBindingElements.add(element);
		}
	}

	/** Replies the runtime binding elements.
	 *
	 * @return the runtime binding elements.
	 */
	@Pure
	public List<BindingElement> getRuntimeBindings() {
		return this.rtBindingElements;
	}

	/** Add ui binding element.
	 *
	 * @param element the ui binding element.
	 */
	public void addUi(BindingElement element) {
		if (element != null) {
			this.uiBindingElements.add(element);
		}
	}

	/** Replies the ui binding elements.
	 *
	 * @return the ui binding elements.
	 */
	@Pure
	public List<BindingElement> getUiBindings() {
		return this.uiBindingElements;
	}

	/** Add runtime/ui binding element.
	 *
	 * @param element the runtime/ui binding element.
	 */
	public void addBoth(BindingElement element) {
		if (element != null) {
			this.rtBindingElements.add(element);
			this.uiBindingElements.add(element);
		}
	}
	
	private static TypeReference typeRef(String qualifiedName) {
		int index = qualifiedName.indexOf('$');
		if (index > 0) {
			String classname = qualifiedName.substring(0, index);
			String innerClasses = qualifiedName.substring(index + 1);
			index = classname.lastIndexOf('.');
			String packageName = classname.substring(0, index);
			classname = classname.substring(index + 1) + '.' + innerClasses;
			return new TypeReference(packageName, classname);
		}
		return TypeReference.typeRef(qualifiedName);
	}
	
	private void bind(GuiceModuleAccess module, List<BindingElement> bindings) {
		BindingFactory bindingFactory = new BindingFactory(module, getName());
		for (BindingElement element : bindings) {
			TypeReference typeReference = typeRef(element.getBind());
			String annotatedWith = element.getAnnotatedWith();
			String annotatedWithName = element.getAnnotatedWithName();	
			if (!Strings.isEmpty(annotatedWith)) {
				TypeReference annotationType = typeRef(annotatedWith);
				if (element.isInstance()) {
					bindingFactory.bindAnnotatedWithToInstance(typeReference, annotationType, element.getTo(),
							element.getFunctionName(), isOverrideAll() || element.isOverride());
				} else {
					TypeReference typeReference2 = typeRef(element.getTo());
					bindingFactory.bindAnnotatedWith(typeReference, annotationType, typeReference2,
							element.getFunctionName(), isOverrideAll() || element.isOverride());
				}
			} else if (!Strings.isEmpty(annotatedWithName)) {
				if (element.isInstance()) {
					bindingFactory.bindAnnotatedWithNameToInstance(typeReference, annotatedWithName,
							element.getTo(),
							element.getFunctionName(),
							isOverrideAll() || element.isOverride());
				} else {
					TypeReference typeReference2 = typeRef(element.getTo());
					bindingFactory.bindAnnotatedWithName(typeReference, annotatedWithName, typeReference2,
							element.getFunctionName(),
							isOverrideAll() || element.isOverride());
				}
			} else if (element.isInstance()) {
				bindingFactory.bindToInstance(typeReference,
						element.getFunctionName(),
						element.getTo(),
						element.isSingleton(),
						element.isEager(),
						isOverrideAll() || element.isOverride());
			} else {
				TypeReference typeReference2 = typeRef(element.getTo());
				bindingFactory.bindToType(
						typeReference,
						element.getFunctionName(),
						typeReference2,
						element.isSingleton(),
						element.isEager(),
						isOverrideAll() || element.isOverride());
			}
		}
		bindingFactory.contributeToModule();
	}
	
	@Override
	public void generate() {
		List<BindingElement> elements;
		
		elements = getRuntimeBindings();
		if (!elements.isEmpty()) {
			LOG.info(MessageFormat.format("Generating the user-defined bindings for runtime module: {0}", //$NON-NLS-1$
					Strings.emptyIfNull(getComment())));
			bind(getLanguage().getRuntimeGenModule(), elements);
		}
		
		elements = getUiBindings();
		if (!elements.isEmpty()) {
			LOG.info(MessageFormat.format("Generating the user-defined bindings for ui module: {0}", //$NON-NLS-1$
					Strings.emptyIfNull(getComment())));
			bind(getLanguage().getEclipsePluginGenModule(), elements);
		}
	}

	/** Factory of bindings.
	 *
	 * <p>This class is similar to {@link org.eclipse.xtext.xtext.generator.model.GuiceModuleAccess.BindingFactory},
	 * except that it supports the "override" feature.
	 * 
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class BindingFactory {
		
		private static final String CONFIGURE_PREFIX = "configure"; //$NON-NLS-1$

		private static final String BIND_PREFIX = "bind"; //$NON-NLS-1$

		private static final String REFERENCE_PREFIX = "ref "; //$NON-NLS-1$

		private final WeakReference<GuiceModuleAccess> module;

		private final String name;

		private final Set<Binding> bindings = new HashSet<>();

		private final List<Binding> removableBindings = new ArrayList<>();

		/**
		 * @param module the Guice module.
		 * @param name the name of the contributor.
		 */
		public BindingFactory(GuiceModuleAccess module, String name) {
			this.module = new WeakReference<>(module);
			this.name = name;
		}

		/** Bind an annotated element.
		 *
		 * @param bind the type to bind.
		 * @param annotatedWith the annotation to consider.
		 * @param to the target type.
		 * @param functionName the optional function name.
		 * @param override indicates if the binding could override an existing element.
		 */
		public void bindAnnotatedWith(TypeReference bind, TypeReference annotatedWith, TypeReference to,
				String functionName, boolean override) {
			StringConcatenationClient client = new StringConcatenationClient() {
				@Override
				protected void appendTo(TargetStringConcatenation builder) {
					builder.append("binder.bind("); //$NON-NLS-1$
					builder.append(bind);
					builder.append(".class).annotatedWith("); //$NON-NLS-1$
					builder.append(annotatedWith);
					builder.append(".class).to("); //$NON-NLS-1$
					builder.append(to);
					builder.append(".class);"); //$NON-NLS-1$
				}
			};
			LOG.debug(MessageFormat.format("\tbind {0} to {1} when annotated with {2}", //$NON-NLS-1$
					bind, to, annotatedWith));
			String fctname = functionName;
			if (Strings.isEmpty(fctname)) {
				fctname = bind.getSimpleName();
			}
			BindKey key = new GuiceModuleAccess.BindKey(formatFunctionName(fctname), null, false, false);
		    BindValue statements = new BindValue(null, null, false, Collections.singletonList(client));
		    Binding binding = new Binding(key, statements, true, this.name);
		    add(binding, override);
		}

		/** Bind an annotated element.
		 *
		 * @param bind the type to bind.
		 * @param annotatedWith the annotation to consider.
		 * @param to the instance.
		 * @param functionName the optional function name.
		 * @param override indicates if the binding could override an existing element.
		 */
		public void bindAnnotatedWithToInstance(TypeReference bind, TypeReference annotatedWith, String to,
				String functionName, boolean override) {
			StringConcatenationClient client = new StringConcatenationClient() {
				@Override
				protected void appendTo(TargetStringConcatenation builder) {
					builder.append("binder.bind("); //$NON-NLS-1$
					builder.append(bind);
					builder.append(".class).annotatedWith("); //$NON-NLS-1$
					builder.append(annotatedWith);
					builder.append(".class).toInstance("); //$NON-NLS-1$
					builder.append(to);
					builder.append(".class);"); //$NON-NLS-1$
				}
			};
			LOG.debug(MessageFormat.format("\tbind {0} to {1} when annotated with {2}", //$NON-NLS-1$
					bind, to, annotatedWith));
			String fctname = functionName;
			if (Strings.isEmpty(fctname)) {
				fctname = bind.getSimpleName();
			}
			BindKey key = new GuiceModuleAccess.BindKey(formatFunctionName(fctname), null, false, false);
		    BindValue statements = new BindValue(null, null, false, Collections.singletonList(client));
		    Binding binding = new Binding(key, statements, true, this.name);
		    add(binding, override);
		}

		/** Bind a type annotated with a name of the given value.
		 *
		 * @param bind the type to bind.
		 * @param name the name to consider.
		 * @param to the target type.
		 * @param functionName the optional function name.
		 * @param override indicates if the binding could override an existing element.
		 */
		public void bindAnnotatedWithName(TypeReference bind, String name, TypeReference to,
				String functionName, boolean override) {
			String tmpName = Strings.emptyIfNull(name);
			if (tmpName.startsWith(REFERENCE_PREFIX)) {
				tmpName = tmpName.substring(REFERENCE_PREFIX.length()).trim();
			} else {
				tmpName = "\"" + tmpName + "\""; //$NON-NLS-1$//$NON-NLS-2$
			}
			final String unferencedName = tmpName;
			StringConcatenationClient client = new StringConcatenationClient() {
				@Override
				protected void appendTo(TargetStringConcatenation builder) {
					builder.append("binder.bind("); //$NON-NLS-1$
					builder.append(bind);
					builder.append(".class).annotatedWith(Names.named("); //$NON-NLS-1$
					builder.append(unferencedName);
					builder.append(")).to("); //$NON-NLS-1$
					builder.append(to);
					builder.append(".class);"); //$NON-NLS-1$
				}
			};
			LOG.debug(MessageFormat.format("\tbind {0} to {1} when annotated with name {2}", //$NON-NLS-1$
					bind, to, unferencedName));
			String fctname = functionName;
			if (Strings.isEmpty(fctname)) {
				fctname = name;
			}
			BindKey key = new GuiceModuleAccess.BindKey(formatFunctionName(fctname), null, false, false);
		    BindValue statements = new BindValue(null, null, false, Collections.singletonList(client));
		    Binding binding = new Binding(key, statements, true, this.name);
		    add(binding, override);
		}
		
		/** Bind a type annotated with a name of the given value.
		 *
		 * @param bind the type to bind.
		 * @param name the name to consider.
		 * @param to the instance.
		 * @param functionName the optional function name.
		 * @param override indicates if the binding could override an existing element.
		 */
		public void bindAnnotatedWithNameToInstance(TypeReference bind, String name, String to,
				String functionName, boolean override) {
			String tmpName = Strings.emptyIfNull(name);
			if (tmpName.startsWith(REFERENCE_PREFIX)) {
				tmpName = tmpName.substring(REFERENCE_PREFIX.length()).trim();
			} else {
				tmpName = "\"" + tmpName + "\""; //$NON-NLS-1$//$NON-NLS-2$
			}
			final String unferencedName = tmpName;
			StringConcatenationClient client = new StringConcatenationClient() {
				@Override
				protected void appendTo(TargetStringConcatenation builder) {
					builder.append("binder.bind("); //$NON-NLS-1$
					builder.append(bind);
					builder.append(".class).annotatedWith(Names.named("); //$NON-NLS-1$
					builder.append(unferencedName);
					builder.append(")).toInstance("); //$NON-NLS-1$
					builder.append(to);
					builder.append(".class);"); //$NON-NLS-1$
				}
			};
			LOG.debug(MessageFormat.format("\tbind {0} to {1} when annotated with name \"{2}\"", //$NON-NLS-1$
					bind, to, name));
			String fctname = functionName;
			if (Strings.isEmpty(fctname)) {
				fctname = name;
			}
			BindKey key = new GuiceModuleAccess.BindKey(formatFunctionName(fctname), null, false, false);
		    BindValue statements = new BindValue(null, null, false, Collections.singletonList(client));
		    Binding binding = new Binding(key, statements, true, this.name);
		    add(binding, override);
		}

		/** Bind a type to an instance expression.
		 *
		 * @param bind the type to bind.
		 * @param functionName the name of the binding function. It may be <code>null</code> for the default name.
		 * @param instanceExpression the expression that represents the instance.
		 * @param isSingleton indicates if the instance is a singleton.
		 * @param isEager indicates if the instance is an eager singleton.
		 * @param override indicates if the binding could override an existing element.
		 */
		public void bindToInstance(TypeReference bind, String functionName, String instanceExpression,
				boolean isSingleton, boolean isEager, boolean override) {
			BindKey type;
			BindValue value;
			if (!Strings.isEmpty(functionName) && functionName.startsWith(CONFIGURE_PREFIX)) {
				String fname = functionName.substring(CONFIGURE_PREFIX.length());
				type = new BindKey(Strings.toFirstUpper(fname), null, isSingleton, isEager);
				StringConcatenationClient client = new StringConcatenationClient() {
					@Override
					protected void appendTo(TargetStringConcatenation builder) {
						builder.append("binder.bind("); //$NON-NLS-1$
						builder.append(bind);
						builder.append(".class).toInstance("); //$NON-NLS-1$
						builder.append(instanceExpression);
						builder.append(");"); //$NON-NLS-1$
					}
				};
				value = new BindValue(null, null, false, Collections.singletonList(client));
			} else {
				String fname = functionName;
				if (fname != null && fname.startsWith(BIND_PREFIX)) {
					fname = fname.substring(BIND_PREFIX.length());
				}
				type = new BindKey(Strings.toFirstUpper(fname), bind, isSingleton, isEager);
				StringConcatenationClient client = new StringConcatenationClient() {
					@Override
					protected void appendTo(TargetStringConcatenation builder) {
						builder.append(instanceExpression);
					}
				};
				value = new BindValue(client, null, false, Collections.emptyList());
			}
			LOG.debug(MessageFormat.format("\tbind {0} to instance", bind)); //$NON-NLS-1$
			Binding binding = new Binding(type, value, true, this.name);
			add(binding, override);
		}

		/** Bind a type to concrete type.
		 *
		 * @param bind the type to bind.
		 * @param functionName the name of the binding function. It may be <code>null</code> for the default name.
		 * @param to the concrete type.
		 * @param isSingleton indicates if the instance is a singleton.
		 * @param isEager indicates if the instance is an eager singleton.
		 * @param override indicates if the binding could override an existing element.
		 */
		public void bindToType(TypeReference bind, String functionName, TypeReference to,
				boolean isSingleton, boolean isEager, boolean override) {
			BindKey type;
			BindValue value;
			if (!Strings.isEmpty(functionName) && functionName.startsWith(CONFIGURE_PREFIX)) {
				String fname = functionName.substring(CONFIGURE_PREFIX.length());
				type = new GuiceModuleAccess.BindKey(Strings.toFirstUpper(fname), null, false, false);
				StringConcatenationClient client = new StringConcatenationClient() {
					@Override
					protected void appendTo(TargetStringConcatenation builder) {
						builder.append("binder.bind("); //$NON-NLS-1$
						builder.append(bind);
						builder.append(".class).to("); //$NON-NLS-1$
						builder.append(to);
						builder.append(".class);"); //$NON-NLS-1$
					}
				};
				value = new BindValue(null, null, false, Collections.singletonList(client));
			} else {
				String fname = functionName;
				if (fname != null && fname.startsWith(BIND_PREFIX)) {
					fname = fname.substring(BIND_PREFIX.length());
				}
				type = new BindKey(Strings.toFirstUpper(fname), bind, isSingleton, isEager);
				value = new BindValue(null, to, false, Collections.emptyList());
			}
			LOG.debug(MessageFormat.format("\tbind {0} to {1}", bind, to)); //$NON-NLS-1$
			Binding binding = new Binding(type, value, true, this.name);
			add(binding, override);
		}
		
		private static Binding findBinding(Set<Binding> bindings, Binding binding) {
			for (Binding bnd : bindings) {
				if (Objects.equals(bnd, binding)) {
					return bnd;
				}
			}
			return null;
		}

		private static String formatFunctionName(String name) {
			String formattedName = name;
			if (formattedName.startsWith(CONFIGURE_PREFIX)) {
				formattedName = formattedName.substring(CONFIGURE_PREFIX.length());
			} else if (formattedName.startsWith(BIND_PREFIX)) {
				formattedName = formattedName.substring(BIND_PREFIX.length());
			}
			return Strings.toFirstUpper(formattedName);
		}

		/** Add the binding.
		 *
		 * @param binding the binding to add.
		 * @param override indicates if the binding could override an existing element.
		 */
		protected void add(Binding binding, boolean override) {
			Set<Binding> moduleBindings = this.module.get().getBindings();
			Binding otherBinding = findBinding(moduleBindings, binding);
			if (!override) {
				if (otherBinding != null) {
					throw new IllegalArgumentException(MessageFormat.format(
							"Forbidden override of {0} by {1}.", //$NON-NLS-1$
							otherBinding, binding));
				}
			} else if (otherBinding != null) {
				this.removableBindings.add(otherBinding);
			}
			if (!this.bindings.add(binding)) {
				throw new IllegalArgumentException(
						MessageFormat.format("Duplicate binding for {0} in {1}", binding.getKey(), this.name)); //$NON-NLS-1$
			}
		}

		/** Put the bindings to the associated module.
		 */
		public void contributeToModule() {
			GuiceModuleAccess module = this.module.get();
			if (!this.removableBindings.isEmpty()) {
				// Ok, we are broking the Java secutiry manager.
				// But it's for having something working!
				try {
					Field field = module.getClass().getDeclaredField("bindings"); //$NON-NLS-1$
					boolean accessible = field.isAccessible();
					try {
						field.setAccessible(true);
						Collection<?> hiddenBindings = (Collection<?>) field.get(module);
						hiddenBindings.removeAll(this.removableBindings);
					} finally {
						field.setAccessible(accessible);
					}
				} catch (Exception exception) {
					throw new IllegalStateException(exception);
				}
			}
			module.addAll(this.bindings);
		}

	}
	
}
