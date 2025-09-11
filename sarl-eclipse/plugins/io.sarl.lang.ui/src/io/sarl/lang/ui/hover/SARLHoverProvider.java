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

package io.sarl.lang.ui.hover;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtend.ide.hover.XtendHoverProvider;

import io.sarl.lang.sarl.SarlCastedExpression;

/**
 * Provider of hovers.
 *
 * <p>This provider enables the hovers on {@link SarlCastedExpression}.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.lang.ui 0.15.1 20250911-224827
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.lang.ui
 * @since 0.9
 */
@SuppressWarnings("restriction")
public class SARLHoverProvider extends XtendHoverProvider {

	@Override
	protected boolean hasHover(EObject object) {
		return super.hasHover(object) || object instanceof SarlCastedExpression;
	}

	@Override
	protected EObject getObjectToView(EObject object) {
		if (object instanceof SarlCastedExpression cvalue) {
			final var operation = cvalue.getFeature();
			if (operation != null) {
				return operation;
			}
		}
		return super.getObjectToView(object);
	}

}
