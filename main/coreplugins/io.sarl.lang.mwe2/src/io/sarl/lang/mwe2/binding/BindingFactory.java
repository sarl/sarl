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
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xtext.generator.model.GuiceModuleAccess;
import org.eclipse.xtext.xtext.generator.model.GuiceModuleAccess.BindKey;
import org.eclipse.xtext.xtext.generator.model.GuiceModuleAccess.BindValue;
import org.eclipse.xtext.xtext.generator.model.GuiceModuleAccess.Binding;
import org.eclipse.xtext.xtext.generator.model.TypeReference;

/**
 * An injected element.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class BindingFactory {

	private static final String CONFIGURE_PREFIX = "configure"; //$NON-NLS-1$

	private static final String BIND_PREFIX = "bind"; //$NON-NLS-1$

	private static final String REFERENCE_PREFIX = "ref "; //$NON-NLS-1$

	private WeakReference<GuiceModuleAccess> module;

	private String name;

	private final Set<Binding> bindings = new HashSet<>();

	private final List<Binding> removableBindings = new ArrayList<>();

	/** Change the name of the factory.
	 *
	 * @param name the name.
	 */
	public void setName(String name) {
		this.name = name;
	}

	/** Change the associated Guice module.
	 *
	 * @param module the associated Guice module.
	 */
	public void setGuiceModule(GuiceModuleAccess module) {
		this.module = new WeakReference<>(module);
	}

	/** Bind an annotated element.
	 *
	 * @param bind the type to bind.
	 * @param annotatedWith the annotation to consider.
	 * @param to the target type.
	 * @param functionName the optional function name.
	 * @return the binding element.
	 */
	protected Binding bindAnnotatedWith(TypeReference bind, TypeReference annotatedWith, TypeReference to,
			String functionName) {
		final StringConcatenationClient client = new StringConcatenationClient() {
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
		String fctname = functionName;
		if (Strings.isEmpty(fctname)) {
			fctname = bind.getSimpleName();
		}
		final BindKey key = new GuiceModuleAccess.BindKey(formatFunctionName(fctname), null, false, false);
		final  BindValue statements = new BindValue(null, null, false, Collections.singletonList(client));
	    return new Binding(key, statements, true, this.name);
	}

	/** Bind an annotated element.
	 *
	 * @param bind the type to bind.
	 * @param annotatedWith the annotation to consider.
	 * @param to the instance.
	 * @param functionName the optional function name.
	 * @return the binding element.
	 */
	protected Binding bindAnnotatedWithToInstance(TypeReference bind, TypeReference annotatedWith, String to,
			String functionName) {
		final StringConcatenationClient client = new StringConcatenationClient() {
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
		String fctname = functionName;
		if (Strings.isEmpty(fctname)) {
			fctname = bind.getSimpleName();
		}
		final BindKey key = new GuiceModuleAccess.BindKey(formatFunctionName(fctname), null, false, false);
		final  BindValue statements = new BindValue(null, null, false, Collections.singletonList(client));
	    return new Binding(key, statements, true, this.name);
	}

	/** Bind a type annotated with a name of the given value.
	 *
	 * @param bind the type to bind.
	 * @param name the name to consider.
	 * @param to the target type.
	 * @param functionName the optional function name.
	 * @return the binding element.
	 */
	protected Binding bindAnnotatedWithName(TypeReference bind, String name, TypeReference to,
			String functionName) {
		String tmpName = Strings.emptyIfNull(name);
		if (tmpName.startsWith(REFERENCE_PREFIX)) {
			tmpName = tmpName.substring(REFERENCE_PREFIX.length()).trim();
		} else {
			tmpName = "\"" + tmpName + "\""; //$NON-NLS-1$//$NON-NLS-2$
		}
		final String unferencedName = tmpName;
		final StringConcatenationClient client = new StringConcatenationClient() {
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
		String fctname = functionName;
		if (Strings.isEmpty(fctname)) {
			fctname = name;
		}
		final BindKey key = new GuiceModuleAccess.BindKey(formatFunctionName(fctname), null, false, false);
		final BindValue statements = new BindValue(null, null, false, Collections.singletonList(client));
	    return new Binding(key, statements, true, this.name);
	}

	/** Bind a type annotated with a name of the given value.
	 *
	 * @param bind the type to bind.
	 * @param name the name to consider.
	 * @param to the instance.
	 * @param functionName the optional function name.
	 * @return the binding element.
	 */
	protected Binding bindAnnotatedWithNameToInstance(TypeReference bind, String name, String to,
			String functionName) {
		String tmpName = Strings.emptyIfNull(name);
		if (tmpName.startsWith(REFERENCE_PREFIX)) {
			tmpName = tmpName.substring(REFERENCE_PREFIX.length()).trim();
		} else {
			tmpName = "\"" + tmpName + "\""; //$NON-NLS-1$//$NON-NLS-2$
		}
		final String unferencedName = tmpName;
		final StringConcatenationClient client = new StringConcatenationClient() {
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
		String fctname = functionName;
		if (Strings.isEmpty(fctname)) {
			fctname = name;
		}
		final BindKey key = new GuiceModuleAccess.BindKey(formatFunctionName(fctname), null, false, false);
		final BindValue statements = new BindValue(null, null, false, Collections.singletonList(client));
	    return new Binding(key, statements, true, this.name);
	}

	/** Bind a type to an instance expression.
	 *
	 * @param bind the type to bind.
	 * @param functionName the name of the binding function. It may be {@code null} for the default name.
	 * @param instanceExpression the expression that represents the instance.
	 * @param isSingleton indicates if the instance is a singleton.
	 * @param isEager indicates if the instance is an eager singleton.
	 * @return the binding element.
	 */
	protected Binding bindToInstance(TypeReference bind, String functionName, String instanceExpression,
			boolean isSingleton, boolean isEager) {
		final BindKey type;
		final BindValue value;
		if (!Strings.isEmpty(functionName) && functionName.startsWith(CONFIGURE_PREFIX)) {
			final String fname = functionName.substring(CONFIGURE_PREFIX.length());
			type = new BindKey(Strings.toFirstUpper(fname), null, isSingleton, isEager);
			final StringConcatenationClient client = new StringConcatenationClient() {
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
			final StringConcatenationClient client = new StringConcatenationClient() {
				@Override
				protected void appendTo(TargetStringConcatenation builder) {
					builder.append(instanceExpression);
				}
			};
			value = new BindValue(client, null, false, Collections.emptyList());
		}
		return new Binding(type, value, true, this.name);
	}

	/** Bind a type to concrete type.
	 *
	 * @param bind the type to bind.
	 * @param functionName the name of the binding function. It may be {@code null} for the default name.
	 * @param to the concrete type.
	 * @param isSingleton indicates if the instance is a singleton.
	 * @param isEager indicates if the instance is an eager singleton.
	 * @param isProvider indicates if the binding is for a provider.
	 * @return the binding element.
	 */
	protected Binding bindToType(
			TypeReference bind, String functionName, TypeReference to,
			boolean isSingleton, boolean isEager, boolean isProvider) {
		final BindKey type;
		final BindValue value;
		if (!Strings.isEmpty(functionName) && functionName.startsWith(CONFIGURE_PREFIX)) {
			final String fname = functionName.substring(CONFIGURE_PREFIX.length());
			type = new GuiceModuleAccess.BindKey(Strings.toFirstUpper(fname), null, false, false);
			final StringConcatenationClient client;
			if (isProvider) {
				client = new StringConcatenationClient() {
					@Override
					protected void appendTo(TargetStringConcatenation builder) {
						builder.append("binder.bind("); //$NON-NLS-1$
						builder.append(bind);
						builder.append(".class).toProvider("); //$NON-NLS-1$
						builder.append(to);
						builder.append(".class);"); //$NON-NLS-1$
					}
				};
			} else {
				client = new StringConcatenationClient() {
					@Override
					protected void appendTo(TargetStringConcatenation builder) {
						builder.append("binder.bind("); //$NON-NLS-1$
						builder.append(bind);
						builder.append(".class).to("); //$NON-NLS-1$
						builder.append(to);
						builder.append(".class);"); //$NON-NLS-1$
					}
				};
			}
			value = new BindValue(null, null, false, Collections.singletonList(client));
		} else {
			String fname = functionName;
			if (fname != null && fname.startsWith(BIND_PREFIX)) {
				fname = fname.substring(BIND_PREFIX.length());
			}
			type = new BindKey(Strings.toFirstUpper(fname), bind, isSingleton, isEager);
			value = new BindValue(null, to, false, Collections.emptyList());
		}
		return new Binding(type, value, true, this.name);
	}

	private static Binding findBinding(Set<Binding> bindings, Binding binding) {
		for (final Binding bnd : bindings) {
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
	public void add(Binding binding, boolean override) {
		final Set<Binding> moduleBindings = this.module.get().getBindings();
		final Binding otherBinding = findBinding(moduleBindings, binding);
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
		final GuiceModuleAccess module = this.module.get();
		if (!this.removableBindings.isEmpty()) {
			// Ok, we are broking the Java secutiry manager.
			// But it's for having something working!
			try {
				final Field field = module.getClass().getDeclaredField("bindings"); //$NON-NLS-1$
				// TODO Is this compatible with Java 11?
				field.setAccessible(true);
				final Collection<?> hiddenBindings = (Collection<?>) field.get(module);
				hiddenBindings.removeAll(this.removableBindings);
			} catch (Exception exception) {
				throw new IllegalStateException(exception);
			}
		}
		module.addAll(this.bindings);
	}

	private static TypeReference typeRef(String qualifiedName) {
		int index = qualifiedName.indexOf('$');
		if (index > 0) {
			String classname = qualifiedName.substring(0, index);
			final String innerClasses = qualifiedName.substring(index + 1);
			index = classname.lastIndexOf('.');
			if (index >= 0) {
				final String packageName = classname.substring(0, index);
				classname = classname.substring(index + 1) + '.' + innerClasses;
				return new TypeReference(packageName, classname);
			}
		}
		return TypeReference.typeRef(qualifiedName);
	}

	/** Convert a binding element to a Guive binding.
	 *
	 * @param element the element to convert.
	 * @return the Guice binding.
	 */
	public Binding toBinding(BindingElement element) {
		final TypeReference typeReference = typeRef(element.getBind());
		final String annotatedWith = element.getAnnotatedWith();
		final String annotatedWithName = element.getAnnotatedWithName();
		if (!Strings.isEmpty(annotatedWith)) {
			final TypeReference annotationType = typeRef(annotatedWith);
			if (element.isInstance()) {
				return bindAnnotatedWithToInstance(typeReference, annotationType, element.getTo(),
						element.getFunctionName());
			}
			final TypeReference typeReference2 = typeRef(element.getTo());
			return bindAnnotatedWith(typeReference, annotationType, typeReference2,
						element.getFunctionName());
		}
		if (!Strings.isEmpty(annotatedWithName)) {
			if (element.isInstance()) {
				return bindAnnotatedWithNameToInstance(typeReference, annotatedWithName,
						element.getTo(),
						element.getFunctionName());
			}
			final TypeReference typeReference2 = typeRef(element.getTo());
			return bindAnnotatedWithName(typeReference, annotatedWithName, typeReference2,
						element.getFunctionName());
		}
		if (element.isInstance()) {
			return bindToInstance(typeReference,
					element.getFunctionName(),
					element.getTo(),
					element.isSingleton(),
					element.isEager());
		}
		final TypeReference typeReference2 = typeRef(element.getTo());
		return bindToType(
					typeReference,
					element.getFunctionName(),
					typeReference2,
					element.isSingleton(),
					element.isEager(),
					element.isProvider());
	}

}
