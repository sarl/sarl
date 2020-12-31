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

package io.sarl.lang.validation;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import javax.inject.Singleton;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtend.core.xtend.XtendAnnotationTarget;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.validation.IssueSeverities;
import org.eclipse.xtext.xbase.XCollectionLiteral;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XStringLiteral;
import org.eclipse.xtext.xbase.annotations.xAnnotations.XAnnotation;

/** Suppress warnings programmatically with <code>@SuppressWarnings</code>.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Singleton
public class DefaultProgrammaticWarningSuppressor implements IProgrammaticWarningSuppressor {

	private static final String ALL_WARNINGS = "all"; //$NON-NLS-1$

	@Override
	public IssueSeverities getIssueSeverities(Map<Object, Object> context, EObject currentObject,
			IssueSeverities predefinedSeverities) {
		// Search for @SuppressWarnings annotations
		final Set<String> codes = new HashSet<>();
		XtendAnnotationTarget cObject = EcoreUtil2.getContainerOfType(currentObject, XtendAnnotationTarget.class);
		boolean ignoreAll = false;
		while (!ignoreAll && cObject != null) {
			for (final XAnnotation annotation : cObject.getAnnotations()) {
				if (SuppressWarnings.class.getName().equals(annotation.getAnnotationType().getIdentifier())) {
					final XExpression expr = annotation.getValue();
					if (expr instanceof XStringLiteral) {
						final String id = ((XStringLiteral) expr).getValue();
						if (ALL_WARNINGS.equalsIgnoreCase(id)) {
							ignoreAll = true;
							codes.clear();
							break;
						}
						codes.add(extractId(id));
					} else if (expr instanceof XCollectionLiteral) {
						final XCollectionLiteral collection = (XCollectionLiteral) expr;
						for (final XExpression idExpr : collection.getElements()) {
							final String id;
							if (idExpr instanceof XStringLiteral) {
								id = ((XStringLiteral) idExpr).getValue();
							} else {
								id = null;
							}
							if (ALL_WARNINGS.equalsIgnoreCase(id)) {
								ignoreAll = true;
								codes.clear();
								break;
							}
							codes.add(extractId(id));
						}
					}
				}
			}
			cObject = EcoreUtil2.getContainerOfType(cObject.eContainer(), XtendAnnotationTarget.class);
		}
		if (!ignoreAll && codes.isEmpty()) {
			return predefinedSeverities;
		}
		return new SuppressWarningIssueSeverities(predefinedSeverities, ignoreAll, codes);
	}

	private static String extractId(String code) {
		final int index = code.lastIndexOf('.');
		if (index >= 0) {
			return code.substring(index + 1);
		}
		return code;
	}

	/** The severity provider that supports <code>@SuppressWarnings</code>.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static final class SuppressWarningIssueSeverities extends IssueSeverities {

		private final IssueSeverities delegate;

		private final boolean ignoredAll;

		private final Set<String> ignoredWarnings;

		SuppressWarningIssueSeverities(IssueSeverities delegate, boolean ignoreAll, Set<String> codes) {
			super(null, null, null);
			this.delegate = delegate;
			this.ignoredAll = ignoreAll;
			this.ignoredWarnings = codes;
		}

		@SuppressWarnings("synthetic-access")
		@Override
		public Severity getSeverity(String code) {
			if (this.delegate == null || this.ignoredAll) {
				return Severity.IGNORE;
			}
			final String codeId = extractId(code);
			if (this.ignoredWarnings.contains(codeId)) {
				return Severity.IGNORE;
			}
			return this.delegate.getSeverity(code);
		}

	}

}
