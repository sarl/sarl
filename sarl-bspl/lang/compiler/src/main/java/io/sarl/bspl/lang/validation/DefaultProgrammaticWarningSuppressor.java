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

package io.sarl.bspl.lang.validation;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.validation.IssueSeverities;
import org.eclipse.xtext.xbase.XCollectionLiteral;
import org.eclipse.xtext.xbase.XStringLiteral;

import com.google.inject.Singleton;

import io.sarl.bspl.lang.bspl.BsplAnnotationTarget;

/** Suppress warnings programmatically with {@code @SuppressWarnings}.
 *
 * <p>Copied from SARL compiler code.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
@Singleton
public class DefaultProgrammaticWarningSuppressor implements IProgrammaticWarningSuppressor {

	private static final String ALL_WARNINGS = "all"; //$NON-NLS-1$

	@Override
	public IssueSeverities getIssueSeverities(Map<Object, Object> context, EObject currentObject,
			IssueSeverities predefinedSeverities) {
		// Search for @SuppressWarnings annotations
		final var codes = new HashSet<String>();
		var cObject = EcoreUtil2.getContainerOfType(currentObject, BsplAnnotationTarget.class);
		var ignoreAll = false;
		while (!ignoreAll && cObject != null) {
			for (final var annotation : cObject.getAnnotations()) {
				if (SuppressWarnings.class.getName().equals(annotation.getAnnotationType().getIdentifier())) {
					final var expr = annotation.getValue();
					if (expr instanceof XStringLiteral cvalue) {
						final var id = cvalue.getValue();
						if (ALL_WARNINGS.equalsIgnoreCase(id)) {
							ignoreAll = true;
							codes.clear();
							break;
						}
						codes.add(extractId(id));
					} else if (expr instanceof XCollectionLiteral collection) {
						for (final var idExpr : collection.getElements()) {
							final String id;
							if (idExpr instanceof XStringLiteral cvalue) {
								id = cvalue.getValue();
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
			cObject = EcoreUtil2.getContainerOfType(cObject.eContainer(), BsplAnnotationTarget.class);
		}
		if (!ignoreAll && codes.isEmpty()) {
			return predefinedSeverities;
		}
		return new SuppressWarningIssueSeverities(predefinedSeverities, ignoreAll, codes);
	}

	private static String extractId(String code) {
		final var index = code.lastIndexOf('.');
		if (index >= 0) {
			return code.substring(index + 1);
		}
		return code;
	}

	/** The severity provider that supports {@code @SuppressWarnings}.
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

		@Override
		public Severity getSeverity(String code) {
			if (this.delegate == null || this.ignoredAll) {
				return Severity.IGNORE;
			}
			final var codeId = extractId(code);
			if (this.ignoredWarnings.contains(codeId)) {
				return Severity.IGNORE;
			}
			return this.delegate.getSeverity(code);
		}

	}

}
