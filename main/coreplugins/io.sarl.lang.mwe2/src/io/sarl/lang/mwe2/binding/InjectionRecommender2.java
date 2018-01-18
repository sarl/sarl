/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2018 the original authors or authors.
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

import java.lang.reflect.Method;
import java.util.LinkedHashSet;
import java.util.Set;

import javax.inject.Inject;

import com.google.common.annotations.Beta;
import com.ibm.icu.text.MessageFormat;
import org.apache.log4j.Logger;
import org.eclipse.xtend.core.XtendRuntimeModule;
import org.eclipse.xtend.ide.XtendUiModule;
import org.eclipse.xtext.service.SingletonBinding;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xtext.generator.AbstractXtextGeneratorFragment;
import org.eclipse.xtext.xtext.generator.model.GuiceModuleAccess.Binding;

/**
 * A {@link AbstractXtextGeneratorFragment} that recommend injections for the SARL language.
 *
 * <p>The fragment compares the injections in the Xtend project and the SARL project, and gives
 * recommendations.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Beta
public class InjectionRecommender2 extends AbstractXtextGeneratorFragment {

	/** Logger.
	 */
	protected static final Logger LOG = Logger.getLogger(InjectionRecommender2.class);

	private static final String CONFIGURE_PREFIX = "configure"; //$NON-NLS-1$

	private static final String BIND_PREFIX = "bind"; //$NON-NLS-1$

	@Inject
	private BindingFactory bindingFactory;

	private String name = getClass().getName();

	private String comment;

	private boolean enable = true;

	/** Enable or disable the recommendations.
	 *
	 * @param enable <code>true</code> if enable.
	 */
	public void setEnable(boolean enable) {
		this.enable = enable;
	}

	/** Replies if the recommendations were enabled.
	 *
	 * @return <code>true</code> if enable.
	 */
	public boolean isEnable() {
		return this.enable;
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

	private static String getTypeName(Class<?> type) {
		if (type.getEnclosingClass() != null) {
			return type.getEnclosingClass().getName() + "$" + type.getSimpleName(); //$NON-NLS-1$
		}
		return type.getName();
	}

	private static void fillFrom(Set<BindingElement> bindings, Class<?> type) {
		for (final Method declaredMethod : type.getDeclaredMethods()) {
			final String methodName = declaredMethod.getName();
			if (!Strings.isEmpty(methodName)
					&& ((methodName.length() > CONFIGURE_PREFIX.length() && methodName.startsWith(CONFIGURE_PREFIX))
					|| (methodName.length() > BIND_PREFIX.length() && methodName.startsWith(BIND_PREFIX)))) {
				if (declaredMethod.getReturnType() != null && !Void.TYPE.equals(declaredMethod.getReturnType())) {
					// Binding function
					final Class<?> returnType = declaredMethod.getReturnType();
					String typeName;
					if (returnType.equals(Class.class)) {
						typeName = declaredMethod.getGenericReturnType().getTypeName();
						typeName = typeName.replaceFirst("^.*?Class<\\?\\s+extends\\s+", ""); //$NON-NLS-1$//$NON-NLS-2$
						typeName = typeName.replaceFirst(">$", ""); //$NON-NLS-1$//$NON-NLS-2$
					} else {
						typeName = getTypeName(returnType);
					}
					if (!Strings.isEmpty(typeName)) {
						final BindingElement element = new BindingElement();
						element.setBind(typeName);
						element.setTo(typeName);
						final SingletonBinding singleton = declaredMethod.getAnnotation(SingletonBinding.class);
						if (singleton != null) {
							element.setSingleton(true);
							element.setEager(singleton.eager());
						}
						bindings.add(element);
					}
				} else if (methodName.startsWith(CONFIGURE_PREFIX)) {
					final String typeName = methodName.substring(CONFIGURE_PREFIX.length());
					if (!Strings.isEmpty(typeName)) {
						final BindingElement element = new BindingElement();
						element.setBind(typeName);
						element.setTo(typeName);
						element.setFunctionName(methodName);
						bindings.add(element);
					}
				}
			}
		}
	}

	/** Provide the recommendations.
	 *
	 * @param label the source of the recommendation.
	 * @param source the source of recommendation.
	 * @param current the current bindings.
	 */
	protected void recommendFrom(String label, Set<BindingElement> source, Set<Binding> current) {
		this.bindingFactory.setName(getName());
		for (final BindingElement sourceElement : source) {
			final Binding wrapElement = this.bindingFactory.toBinding(sourceElement);
			if (!current.contains(wrapElement)) {
				LOG.info(MessageFormat.format("Recommended injection from {0}: {1}", //$NON-NLS-1$
						label, sourceElement.toString()));
			}
		}
	}

	@Override
	public void generate() {
		if (isEnable()) {
			final Set<BindingElement> xtendRtBindings = new LinkedHashSet<>();
			fillFrom(xtendRtBindings, XtendRuntimeModule.class.getSuperclass());
			fillFrom(xtendRtBindings, XtendRuntimeModule.class);

			final Set<Binding> currentRtBindings = getLanguage().getRuntimeGenModule().getBindings();

			recommendFrom("XtendRuntimeModule", xtendRtBindings, currentRtBindings); //$NON-NLS-1$

			final Set<BindingElement> xtendUiBindings = new LinkedHashSet<>();
			fillFrom(xtendUiBindings, XtendUiModule.class.getSuperclass());
			fillFrom(xtendUiBindings, XtendUiModule.class);

			final Set<Binding> currentUiBindings = getLanguage().getRuntimeGenModule().getBindings();

			recommendFrom("XtendUiModule", xtendUiBindings, currentUiBindings); //$NON-NLS-1$
		}
	}

}
