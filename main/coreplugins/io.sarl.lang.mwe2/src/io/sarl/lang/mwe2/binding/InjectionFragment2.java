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

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import javax.inject.Inject;

import com.google.inject.Injector;
import com.ibm.icu.text.MessageFormat;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xtext.generator.AbstractXtextGeneratorFragment;
import org.eclipse.xtext.xtext.generator.model.GuiceModuleAccess;
import org.eclipse.xtext.xtext.generator.model.GuiceModuleAccess.Binding;

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
	protected static final Logger LOG = Logger.getLogger(InjectionFragment2.class.getName());

	@Inject
	private Injector injector;

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

	private void bind(GuiceModuleAccess module, List<BindingElement> bindings) {
		final BindingFactory bindingFactory = this.injector.getInstance(BindingFactory.class);
		bindingFactory.setName(this.name);
		bindingFactory.setGuiceModule(module);
		for (final BindingElement element : bindings) {
			final Binding guiceBinding = bindingFactory.toBinding(element);
			bindingFactory.add(guiceBinding, isOverrideAll() || element.isOverride());
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

}
