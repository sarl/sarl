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

package io.sarl.lang.ui.validation;

import java.text.MessageFormat;
import java.util.List;

import com.google.common.base.Objects;
import com.google.common.base.Strings;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.xtend.core.validation.IssueCodes;
import org.eclipse.xtend.core.xtend.XtendFile;
import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtend.ide.validator.XtendUIValidator;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.validation.Check;
import org.eclipse.xtext.validation.ValidationMessageAcceptor;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.annotations.xAnnotations.XAnnotationsPackage;
import org.eclipse.xtext.xtype.XtypePackage;

/** Validator based on the Eclipse UI.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLUIValidator extends XtendUIValidator {

	@Override
	protected List<EPackage> getEPackages() {
		final List<EPackage> packages = super.getEPackages();
		packages.add(io.sarl.lang.sarl.SarlPackage.eINSTANCE);
		packages.add(XtendPackage.eINSTANCE);
		packages.add(XbasePackage.eINSTANCE);
		packages.add(TypesPackage.eINSTANCE);
		packages.add(XtypePackage.eINSTANCE);
		packages.add(XAnnotationsPackage.eINSTANCE);
		return packages;
	}

	@Check
	@Override
	public void checkFileNamingConventions(XtendFile sarlFile) {
		//
		// The wrong package is a warning in SARL (an error in Xtend).
		//
		final String expectedPackage = Strings.nullToEmpty(getExpectedPackageName(sarlFile));
		final String declaredPackage = Strings.nullToEmpty(sarlFile.getPackage());
		if (!Objects.equal(expectedPackage, declaredPackage)) {
			if (expectedPackage.isEmpty()) {
				warning(Messages.SARLUIValidator_0,
						sarlFile,
						XtendPackage.Literals.XTEND_FILE__PACKAGE,
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						IssueCodes.WRONG_PACKAGE,
						expectedPackage);
			} else {
				warning(MessageFormat.format(Messages.SARLUIValidator_1, expectedPackage),
						sarlFile,
						XtendPackage.Literals.XTEND_FILE__PACKAGE,
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						IssueCodes.WRONG_PACKAGE,
						expectedPackage);
			}
		}
	}

}
