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

package io.sarl.lang.validation.subvalidators;

import static org.eclipse.xtend.core.validation.IssueCodes.JAVA_DOC_LINKING_DIAGNOSTIC;

import java.text.MessageFormat;

import com.google.inject.Inject;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.documentation.IEObjectDocumentationProvider;
import org.eclipse.xtext.documentation.IEObjectDocumentationProviderExtension;
import org.eclipse.xtext.documentation.IJavaDocTypeReferenceProvider;
import org.eclipse.xtext.naming.IQualifiedNameConverter;
import org.eclipse.xtext.scoping.IScopeProvider;
import org.eclipse.xtext.validation.Check;
import org.eclipse.xtext.validation.CheckType;

/**
 * A specialized validator to deal with SARL documentation (similar to Javadoc).
 *
 * @author $Author: sgalland$
 * @version compiler 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler
 * @since 0.14
 */
public class SARLDocumentationValidator extends AbstractSARLSubValidator {

	@Inject
	private IEObjectDocumentationProvider documentationProvider;

	@Inject
	private IJavaDocTypeReferenceProvider javaDocTypeReferenceProvider;

	@Inject
	private IScopeProvider scopeProvider;

	@Inject
	private IQualifiedNameConverter qualifiedNameConverter;

	/** Check the documentation reference links of the given member.
	 *
	 * @param member the member to check.
	 */
	@Check(CheckType.EXPENSIVE)
	public void checkJavaDocRefs(XtendMember member) {
		if (isIgnored(JAVA_DOC_LINKING_DIAGNOSTIC)) {
			return;
		}
		final var documentationNodes = ((IEObjectDocumentationProviderExtension) this.documentationProvider).getDocumentationNodes(member);
		for (final var node : documentationNodes){
			for (final var region : this.javaDocTypeReferenceProvider.computeTypeRefRegions(node)) {
				final var typeRefString = region.getText();
				if (typeRefString != null && typeRefString.length() > 0) {
					final var scope = this.scopeProvider.getScope(member, 
							TypesPackage.Literals.JVM_PARAMETERIZED_TYPE_REFERENCE__TYPE);
					final var candidate = scope.getSingleElement(this.qualifiedNameConverter.toQualifiedName(typeRefString));
					if (candidate == null) {
						final var severity = getIssueSeverities(getContext(), getCurrentObject()).getSeverity(JAVA_DOC_LINKING_DIAGNOSTIC);
						if (severity != null) {
							getChain().add(createDiagnostic(severity, 
									MessageFormat.format(Messages.SARLDocumentationValidator_1, typeRefString),
									member, region.getOffset(), region.getLength(), JAVA_DOC_LINKING_DIAGNOSTIC));
						}
					}
				}
			}
		}
	}

}
