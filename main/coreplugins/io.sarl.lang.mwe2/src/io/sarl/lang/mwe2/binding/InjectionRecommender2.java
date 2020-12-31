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

import java.lang.reflect.Method;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.logging.Logger;
import javax.inject.Inject;

import com.google.common.annotations.Beta;
import com.ibm.icu.text.MessageFormat;
import org.eclipse.xtend.core.XtendRuntimeModule;
import org.eclipse.xtend.ide.XtendUiModule;
import org.eclipse.xtext.service.SingletonBinding;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xtext.generator.AbstractXtextGeneratorFragment;
import org.eclipse.xtext.xtext.generator.model.GuiceModuleAccess;
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
	protected static final Logger LOG = Logger.getLogger(InjectionRecommender2.class.getName());

	private static final String CONFIGURE_PREFIX = "configure"; //$NON-NLS-1$

	private static final String BIND_PREFIX = "bind"; //$NON-NLS-1$

	@Inject
	private BindingFactory bindingFactory;

	private String name = getClass().getName();

	private String comment;

	private boolean enable = true;

	private boolean runtimeEnable = true;

	private boolean uiEnable = true;

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

	/** Enable or disable the runtime recommendations.
	 *
	 * @param enable <code>true</code> if enable.
	 */
	public void setShowRuntimeRecommendations(boolean enable) {
		this.runtimeEnable = enable;
	}

	/** Replies if the runtime recommendations were enabled.
	 *
	 * @return <code>true</code> if enable.
	 */
	public boolean isShowRuntimeRecommendations() {
		return this.runtimeEnable;
	}

	/** Enable or disable the UI recommendations.
	 *
	 * @param enable <code>true</code> if enable.
	 */
	public void setShowUiRecommendations(boolean enable) {
		this.uiEnable = enable;
	}

	/** Replies if the UI recommendations were enabled.
	 *
	 * @return <code>true</code> if enable.
	 */
	public boolean isShowUiRecommendations() {
		return this.uiEnable;
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

	/** Replies if the type is ignorable within the recommendations.
	 *
	 * @param typeName the name of the type.
	 * @return {@code true} if the type could be ignored.
	 */
	protected static boolean isIgnorableType(String typeName) {
		return "FileExtensions".equals(typeName) || "LanguageName".equals(typeName); //$NON-NLS-1$ //$NON-NLS-2$
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
					if (!Strings.isEmpty(typeName) && !isIgnorableType(typeName)) {
						final BindingElement element = new BindingElement();
						element.setBind(typeName);
						element.setTo(Object.class.getName());
						final SingletonBinding singleton = declaredMethod.getAnnotation(SingletonBinding.class);
						if (singleton != null) {
							element.setSingleton(true);
							element.setEager(singleton.eager());
						}
						bindings.add(element);
					}
				} else if (methodName.startsWith(CONFIGURE_PREFIX)) {
					final String typeName = methodName.substring(CONFIGURE_PREFIX.length());
					if (!Strings.isEmpty(typeName) && !isIgnorableType(typeName)) {
						final BindingElement element = new BindingElement();
						element.setBind(typeName);
						element.setTo(Object.class.getName());
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
		boolean hasRecommend = false;
		for (final BindingElement sourceElement : source) {
			final Binding wrapElement = this.bindingFactory.toBinding(sourceElement);
			if (!current.contains(wrapElement)) {
				if (!hasRecommend) {
					LOG.info(MessageFormat.format("Begin recommendations for {0}", //$NON-NLS-1$
							label));
					hasRecommend = true;
				}
				LOG.warning(MessageFormat.format("\t{1}", //$NON-NLS-1$
						label, sourceElement.getKeyString()));
			}
		}
		if (hasRecommend) {
			LOG.info(MessageFormat.format("End recommendations for {0}", //$NON-NLS-1$
					label));
		} else {
			LOG.info(MessageFormat.format("No recommendation for {0}", //$NON-NLS-1$
					label));
		}
	}

	/** Provide the recommendations for the given module.
	 *
	 * @param superModule the super module to extract definitions from.
	 * @param currentModuleAccess the accessor to the the current module's definition.
	 */
	protected void recommend(Class<?> superModule, GuiceModuleAccess currentModuleAccess) {
		LOG.info(MessageFormat.format("Building injection configuration from {0}", //$NON-NLS-1$
				superModule.getName()));
		final Set<BindingElement> superBindings = new LinkedHashSet<>();
		fillFrom(superBindings, superModule.getSuperclass());
		fillFrom(superBindings, superModule);

		final Set<Binding> currentBindings = currentModuleAccess.getBindings();

		recommendFrom(superModule.getName(), superBindings, currentBindings);
	}

	@Override
	public void generate() {
		if (isEnable()) {
			if (isShowRuntimeRecommendations()) {
				recommend(XtendRuntimeModule.class, getLanguage().getRuntimeGenModule());
			}
			if (isShowUiRecommendations()) {
				recommend(XtendUiModule.class, getLanguage().getIdeGenModule());
			}
		}
	}

}
