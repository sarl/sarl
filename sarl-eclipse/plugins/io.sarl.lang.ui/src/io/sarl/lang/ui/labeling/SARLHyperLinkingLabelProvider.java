/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

package io.sarl.lang.ui.labeling;

import java.text.MessageFormat;

import com.google.inject.Inject;
import org.eclipse.emf.edit.ui.provider.AdapterFactoryLabelProvider;

/**
 * Label provider for hyperlinks.
 *
 * @author $Author: srodriguez$
 * @version io.sarl.lang.ui 0.15.0 20250909-115751
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.lang.ui
 * @since 0.5
 */
public class SARLHyperLinkingLabelProvider extends SARLLabelProvider {

	/** Constructor.
	 *
	 * @param delegate the delegate.
	 */
	@Inject
	public SARLHyperLinkingLabelProvider(AdapterFactoryLabelProvider delegate) {
		super(delegate);
	}

	@Override
	public String getText(Object element) {
		final var result = super.getText(element);
		if (result != null) {
			return MessageFormat.format(Messages.SARLHyperLinkingLabelProvider_0, result);
		}
		return null;
	}

}
