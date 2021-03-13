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

package io.sarl.lang.ui.highlighting;

import org.eclipse.swt.SWT;
import org.eclipse.xtend.ide.highlighting.XtendHighlightingConfiguration;
import org.eclipse.xtext.ui.editor.syntaxcoloring.IHighlightingConfigurationAcceptor;
import org.eclipse.xtext.ui.editor.utils.TextStyle;

/**
 * The configuration for the SARL highlithing system.
 *
 * <p>This configuration supports the highlighting with a specific color the calls to the capacity methods.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public class SARLHighlightingConfiguration extends XtendHighlightingConfiguration {

	@Override
	public void configure(IHighlightingConfigurationAcceptor acceptor) {
		super.configure(acceptor);
		acceptor.acceptDefaultHighlighting(
				SARLHighlightingStyles.CAPACITY_METHOD_INVOCATION,
				"Capacity method invocations", //$NON-NLS-1$
				capacityMethodInvocation());
		acceptor.acceptDefaultHighlighting(
				SARLHighlightingStyles.ASYNCHRONOUS_METHOD_INVOCATION,
				"Asynchronous method invocations", //$NON-NLS-1$
				asynchronousMethodInvocation());
	}

	/** Style for the capacity method extension.
	 *
	 * @return the style.
	 */
	@SuppressWarnings("checkstyle:magicnumber")
	public TextStyle capacityMethodInvocation() {
		final TextStyle textStyle = extensionMethodInvocation().copy();
		textStyle.setStyle(SWT.ITALIC);
		return textStyle;
	}

	/** Style for the asynchronous method extension.
	 *
	 * @return the style.
	 */
	@SuppressWarnings("checkstyle:magicnumber")
	public TextStyle asynchronousMethodInvocation() {
		final TextStyle textStyle = extensionMethodInvocation().copy();
		textStyle.setStyle(SWT.BOLD);
		return textStyle;
	}

}
