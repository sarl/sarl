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

package io.sarl.lang.macro;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtend.core.macro.ActiveAnnotationContextProvider;
import org.eclipse.xtext.common.types.JvmAnnotationType;
import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.xbase.annotations.xAnnotations.XAnnotation;
import org.eclipse.xtext.xbase.lib.Pair;

import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlSkill;

/** Provide the contexts of the active annotations.
 *
 * <p>This provider supports the SARL type declarations.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.9
 */
public class SarlActiveAnnotationContextProvider extends ActiveAnnotationContextProvider {

	@Override
	protected void searchAnnotatedElements(final EObject element, final IAcceptor<Pair<JvmAnnotationType, XAnnotation>> acceptor) {
		if (element instanceof SarlAgent) {
			final SarlAgent elt = (SarlAgent) element;
			registerMacroAnnotations(elt, acceptor);
			elt.getMembers().forEach(it -> searchAnnotatedElements(it, acceptor));
			return;
		}
		if (element instanceof SarlBehavior) {
			final SarlBehavior elt = (SarlBehavior) element;
			registerMacroAnnotations(elt, acceptor);
			elt.getMembers().forEach(it -> searchAnnotatedElements(it, acceptor));
			return;
		}
		if (element instanceof SarlSkill) {
			final SarlSkill elt = (SarlSkill) element;
			registerMacroAnnotations(elt, acceptor);
			elt.getMembers().forEach(it -> searchAnnotatedElements(it, acceptor));
			return;
		}
		super.searchAnnotatedElements(element, acceptor);
	}

}
