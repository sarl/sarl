/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

package io.sarl.lang.ui;

import com.google.inject.Binder;
import com.google.inject.Inject;
import com.google.inject.Injector;
import com.google.inject.Provider;
import com.google.inject.name.Names;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.eclipse.xtext.ui.editor.DirtyStateEditorSupport;
import org.eclipse.xtext.ui.editor.DocumentBasedDirtyResource;
import org.eclipse.xtext.ui.editor.XtextEditor;
import org.eclipse.xtext.ui.editor.autoedit.AbstractEditStrategy;
import org.eclipse.xtext.validation.IssueSeveritiesProvider;

import io.sarl.lang.ui.bugs.bug1115.FixedDirtyStateEditorSupport;
import io.sarl.lang.ui.bugs.bug1115.FixedPersistentDataAwareDirtyResource;
import io.sarl.lang.ui.validation.UIConfigurableIssueSeveritiesProvider;
import io.sarl.lang.validation.IConfigurableIssueSeveritiesProvider;

/**
 * Use this class to register components to be used within the IDE.
 *
 * <p>DOT NOT ADD BINDINGS IN THIS CLASS. PREFER TO UPDATE THE MWE2 SCRIPT.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLUiModule extends AbstractSARLUiModule {

	/** Provider of {@link UIConfigurableIssueSeveritiesProvider}.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.5
	 */
	private static class UIConfigurableIssueSeveritiesProviderProvider implements Provider<UIConfigurableIssueSeveritiesProvider> {

		private UIConfigurableIssueSeveritiesProvider severityProvider;

		@Inject
		private Injector injector;

		UIConfigurableIssueSeveritiesProviderProvider() {
			//
		}

		@Override
		public UIConfigurableIssueSeveritiesProvider get() {
			if (this.severityProvider == null) {
				this.severityProvider = new UIConfigurableIssueSeveritiesProvider();
				this.injector.injectMembers(this.severityProvider);
			}
			return this.severityProvider;
		}

	}

	/** Constructor.
	 *
	 * @param plugin the Eclipse plugin.
	 */
	public SARLUiModule(AbstractUIPlugin plugin) {
		super(plugin);
	}

	@Override
	public void configure(Binder binder) {
		super.configure(binder);
		// Configure the automatic auto-completion on specific characters: "." and ":"
		//  binder.bind(String.class).annotatedWith(com.google.inject.name.Names.named(
		//  XtextContentAssistProcessor.COMPLETION_AUTO_ACTIVATION_CHARS))
		//	.toInstance(".");

		// Configure a system singleton for issue severities provider
		final UIConfigurableIssueSeveritiesProviderProvider provider = new UIConfigurableIssueSeveritiesProviderProvider();
		binder.bind(UIConfigurableIssueSeveritiesProvider.class).toProvider(provider);
		binder.bind(IssueSeveritiesProvider.class).toProvider(provider);
		binder.bind(IConfigurableIssueSeveritiesProvider.class).toProvider(provider);
	}

	/** Configure the debug mode.
	 * 
	 * @param binder the binder.
	 */
	@SuppressWarnings("static-method")
	public void configureDebugMode(Binder binder) {
		if (Boolean.getBoolean("io.sarl.lang.debug") //$NON-NLS-1$
				|| Boolean.getBoolean("org.eclipse.xtext.xtend.debug")) { //$NON-NLS-1$
			binder.bindConstant().annotatedWith(Names.named(AbstractEditStrategy.DEBUG)).to(true);
		}
		// matches ID of org.eclipse.ui.contexts extension registered in plugin.xml
		binder.bindConstant().annotatedWith(Names.named(XtextEditor.KEY_BINDING_SCOPE))
		.to("io.sarl.lang.ui.scoping.SARLEditorScope"); //$NON-NLS-1$
	}

	@Override
	public Class<? extends DocumentBasedDirtyResource> bindDocumentBasedDirtyResource() {
		// TODO: Remove this function when the issue #1115 is fixed
		return FixedPersistentDataAwareDirtyResource.class;
	}

	@Override
	public Class<? extends DirtyStateEditorSupport> bindDirtyStateEditorSupport(){
		// TODO: Remove this function when the issue #1115 is fixed
		return FixedDirtyStateEditorSupport.class;
	}

}
