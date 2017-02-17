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

package io.sarl.lang.bugfixes;

import javax.inject.Inject;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.jface.text.IRegion;
import org.eclipse.xtext.resource.EObjectAtOffsetHelper;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.util.Pair;
import org.eclipse.xtext.util.Tuples;
import org.eclipse.xtext.xbase.XConstructorCall;
import org.eclipse.xtext.xbase.ui.hover.XbaseDispatchingEObjectTextHover;

/**
 * Dispatcher the building of the hover's content to different providers.
 *
 * <p>This class forces the use of the SARL hover provider when a constructor is called.
 * Indeed, in the default Xbase dispatcher, only XAbstractFeatureCall are treated, not XConstructorCall.
 * This class ass this support.
 *
 * <p>TODO: Remove when PR 187 is merged into Xtext (https://github.com/eclipse/xtext-eclipse/pull/187)
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.5
 * @see "https://github.com/eclipse/xtext-eclipse/pull/187"
 */
public class Bug187SARLDispatchingEObjectTextHover extends XbaseDispatchingEObjectTextHover {

	@Inject
	private EObjectAtOffsetHelper eobjectAtOffsetHelper;

	@Override
	protected Pair<EObject, IRegion> getXtextElementAt(XtextResource resource, int offset) {
		final Pair<EObject, IRegion> original = super.getXtextElementAt(resource, offset);
		if (original != null) {
			final EObject object = this.eobjectAtOffsetHelper.resolveContainedElementAt(resource, offset);
			if (object instanceof XConstructorCall) {
				return Tuples.create(object, original.getSecond());
			}
		}
		return original;
	}

}
