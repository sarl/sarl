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

package io.sarl.lang.ui;

import com.google.inject.Binder;
import com.google.inject.name.Names;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.eclipse.xtext.ui.editor.XtextEditor;
import org.eclipse.xtext.ui.editor.autoedit.AbstractEditStrategy;
import org.eclipse.xtext.ui.editor.contentassist.XtextContentAssistProcessor;
import org.eclipse.xtext.ui.editor.hover.IEObjectHover;

import io.sarl.lang.bugfixes.Bug187SARLDispatchingEObjectTextHover;

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

	private static final String AUTOMATIC_PROPOSAL_CHARACTERS = ".:"; //$NON-NLS-1$

	public SARLUiModule(AbstractUIPlugin plugin) {
		super(plugin);
	}

	@Override
	public void configure(Binder binder) {
		super.configure(binder);
		// Configure the automatic auto-completion on specific characters: "." and ":"
		binder.bind(String.class).annotatedWith(com.google.inject.name.Names.named(XtextContentAssistProcessor.COMPLETION_AUTO_ACTIVATION_CHARS))
			.toInstance(AUTOMATIC_PROPOSAL_CHARACTERS);
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

	// TODO: Remove when Xtext PR 187 is merged - https://github.com/eclipse/xtext-eclipse/pull/187
	@Override
	public Class<? extends IEObjectHover> bindIEObjectHover() {
		return Bug187SARLDispatchingEObjectTextHover.class;
	}

}
