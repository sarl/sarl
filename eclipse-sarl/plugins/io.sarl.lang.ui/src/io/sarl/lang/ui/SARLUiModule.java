/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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

import javax.inject.Inject;
import javax.inject.Provider;

import com.google.inject.Binder;
import com.google.inject.name.Names;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.eclipse.xtext.common.types.xtext.ui.ITypesProposalProvider;
import org.eclipse.xtext.ui.editor.XtextEditor;
import org.eclipse.xtext.ui.editor.autoedit.AbstractEditStrategy;
import org.eclipse.xtext.ui.editor.contentassist.XtextContentAssistProcessor;
import org.eclipse.xtext.util.Strings;

import io.sarl.lang.services.SARLGrammarKeywordAccess;
import io.sarl.lang.ui.bugfixes.Bug406ImportingTypesProposalProvider;

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
@SuppressWarnings({"static-method", "javadoc", "checkstyle:javadocmethod"})
public class SARLUiModule extends AbstractSARLUiModule {

	public SARLUiModule(AbstractUIPlugin plugin) {
		super(plugin);
	}

	@Override
	public void configure(Binder binder) {
		super.configure(binder);
		// Configure the automatic auto-completion on specific characters: "." and ":"
		binder.bind(String.class).annotatedWith(com.google.inject.name.Names.named(XtextContentAssistProcessor.COMPLETION_AUTO_ACTIVATION_CHARS))
			.toProvider(new AutoCompletionKeywordProvider()).asEagerSingleton();
	}

	public void configureDebugMode(Binder binder) {
		if (Boolean.getBoolean("io.sarl.lang.debug") //$NON-NLS-1$
				|| Boolean.getBoolean("org.eclipse.xtext.xtend.debug")) { //$NON-NLS-1$
			binder.bindConstant().annotatedWith(Names.named(AbstractEditStrategy.DEBUG)).to(true);
		}
		// matches ID of org.eclipse.ui.contexts extension registered in plugin.xml
		binder.bindConstant().annotatedWith(Names.named(XtextEditor.KEY_BINDING_SCOPE))
		.to("io.sarl.lang.ui.scoping.SARLEditorScope"); //$NON-NLS-1$
	}

	// TODO: Remove when https://github.com/eclipse/xtext-eclipse/issues/28 is fixed.
	@Override
	public Class<? extends ITypesProposalProvider> bindITypesProposalProvider() {
		return Bug406ImportingTypesProposalProvider.class;
	}

	/** Provider of the keywords at which the proposals are automatically given.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class AutoCompletionKeywordProvider implements Provider<String> {

		@Inject
		private SARLGrammarKeywordAccess access;

		private String keywords;

		/** Constructor.
		 */
		AutoCompletionKeywordProvider() {
			//
		}

		@Override
		public String get() {
			if (Strings.isEmpty(this.keywords)) {
				this.keywords = this.access.getFullStopKeyword() + this.access.getColonKeyword();
			}
			return this.keywords;
		}

	}

}
