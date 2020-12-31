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

package io.sarl.docs.doclet.proxy;

import java.lang.ref.WeakReference;
import java.lang.reflect.Array;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.sun.javadoc.AnnotationTypeDoc;
import com.sun.javadoc.Doc;
import com.sun.javadoc.ProgramElementDoc;
import com.sun.javadoc.RootDoc;

import io.sarl.docs.doclet.SarlConfiguration;
import io.sarl.docs.doclet.exclude.ApidocExcluder;

/** Install the proxies for the {@code Doc}.
 * This object is filtering the arrays that are replied functions in {@link RootDoc} or {@link ProgramElementDoc}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 */
public class JreProxyInstaller implements ProxyInstaller {

	/** List of the functions' names that are not accepting a proxy as argument.
	 */
	private static final String[] SINGLE_ARGUMENT_UNPROTECTED_FUNCTION_NAMES = {
		"compareTo", //$NON-NLS-1$
		"equals", //$NON-NLS-1$
		"overrides", //$NON-NLS-1$
		"subclassOf", //$NON-NLS-1$
	};

	/** List of the functions' names that are not accepting a proxy as argument.
	 */
	static final Set<String> SINGLE_ARGUMENT_UNPROTECTED_FUNCTIONS = new HashSet<>(
			Arrays.asList(SINGLE_ARGUMENT_UNPROTECTED_FUNCTION_NAMES));

	private final WeakReference<SarlConfiguration> configuration;

	/** Constructor.
	 *
	 * @param configuration the configuration.
	 */
	public JreProxyInstaller(SarlConfiguration configuration) {
		this.configuration = new WeakReference<>(configuration);
	}

	@Override
	public RootDoc installProxies(RootDoc obj) {
		return (RootDoc) processElement(obj, null);
	}

	@Override
	public void installProxies(SarlConfiguration configuration) {
		configuration.root = (RootDoc) processElement(configuration.root, null);
	}

	@Override
	public void uninstallProxies(SarlConfiguration configuration) {
		configuration.root = unwrap(configuration.root);
	}

	/** Filter the given document.
	 *
	 * @param obj the document to filter.
	 * @param expectedType  the expected type of the {@code obj}.
	 * @return the filtered {@code obj}.
	 */
	protected Object processElement(Object obj, Class<?> expectedType) {
		if (obj == null || obj instanceof Proxy) {
			return obj;
		}
		if (obj instanceof Doc) {
			return wrap(obj);
		} else if (expectedType != null && expectedType.isArray()) {
			final Class<?> componentType = expectedType.getComponentType();
			if (Doc.class.isAssignableFrom(componentType)) {
				final int len = Array.getLength(obj);
				final List<Object> list = new ArrayList<>(len);
				final ApidocExcluder excluder = this.configuration.get().getApidocExcluder();
				for (int i = 0; i < len; ++i) {
					final Object entry = Array.get(obj, i);
					if (!(entry instanceof Doc)) {
						list.add(processElement(entry, componentType));
					} else if (excluder.isExcluded((Doc) entry)) {
						if (excluder.isTranslatableToTag((Doc) entry)) {
							//
						}
					} else {
						list.add(processElement(entry, componentType));
					}
				}
				final Object newArray = Array.newInstance(componentType, list.size());
				int i = 0;
				for (final Object element : list) {
					Array.set(newArray, i, element);
					++i;
				}
				return newArray;
			}
		}
		return obj;
	}

	@Override
	public AnnotationTypeDoc wrap(AnnotationTypeDoc obj) {
		return (AnnotationTypeDoc) wrap((Object) obj);
	}

	/** Unwrap the given object.
	 *
	 * @param object the object.
	 * @return the unwrapped object.
	 */
	protected Object wrap(Object object) {
		if (object == null || object instanceof Proxy) {
			return object;
		}
		final Class<?> type = object.getClass();
		return Proxy.newProxyInstance(type.getClassLoader(), type.getInterfaces(), new ProxyHandler(object));
	}

	@SuppressWarnings({ "synthetic-access", "unchecked" })
	@Override
	public <T> T unwrap(T proxy) {
		if (proxy != null && Proxy.isProxyClass(proxy.getClass())) {
			return (T) ((ProxyHandler) Proxy.getInvocationHandler(proxy)).target;
		}
		return proxy;
	}

	/** Excluder of documentation.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.7
	 */
	private class ProxyHandler implements InvocationHandler {

		private Object target;

		ProxyHandler(Object target) {
			this.target = target;
		}

		@Override
		public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
			if (args != null && args.length >= 1) {
				final String methodName = method.getName();
				if (SINGLE_ARGUMENT_UNPROTECTED_FUNCTIONS.contains(methodName)) {
					args[0] = unwrap(args[0]);
				}
			}
			try {
				return processElement(method.invoke(this.target, args), method.getReturnType());
			} catch (InvocationTargetException e) {
				throw e.getTargetException();
			}
		}

	}

}
