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

package io.sarl.lang.extralanguage.validator;

import java.util.ArrayList;
import java.util.List;

import com.google.inject.Inject;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.validation.AbstractDeclarativeValidator;
import org.eclipse.xtext.validation.Check;
import org.eclipse.xtext.validation.CheckType;
import org.eclipse.xtext.validation.EValidatorRegistrar;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.annotations.xAnnotations.XAnnotationsPackage;
import org.eclipse.xtext.xtype.XtypePackage;

/** The validator from SARL to the extra target languages.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version compiler 0.15.1 20250911-224823
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler
 * @since 0.6
 */
public class ExtraLanguageValidatorSupport extends AbstractDeclarativeValidator {

	@Inject
	private IExtraLanguageValidatorProvider validatorProvider;

	@Override
	protected List<EPackage> getEPackages() {
		final var result = new ArrayList<>(super.getEPackages());
		result.add(io.sarl.lang.sarl.SarlPackage.eINSTANCE);
		result.add(XtendPackage.eINSTANCE);
		result.add(XbasePackage.eINSTANCE);
		result.add(TypesPackage.eINSTANCE);
		result.add(XtypePackage.eINSTANCE);
		result.add(XAnnotationsPackage.eINSTANCE);
		return result;
	}

	@Override
	public void register(EValidatorRegistrar registrar) {
		//
	}

	/** Check the rules for the activated extra languages.
	 *
	 * @param currentObject the current object to test.
	 */
	@Check(CheckType.NORMAL)
	public void checkExtraLanguageRules(EObject currentObject) {
		final var validators = this.validatorProvider.getValidators(
				currentObject.eResource());
		if (!validators.isEmpty()) {
			for (final var validator : validators) {
				final var acceptor = getMessageAcceptor();
				final var stateAccess = setMessageAcceptor(acceptor);
				validator.validate(stateAccess, acceptor);
			}
		}
	}

}
