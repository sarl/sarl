/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

import java.util.Map;

import com.google.inject.Inject;
import org.eclipse.xtend.ide.common.highlighting.XtendHighlightingCalculator;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.ide.editor.syntaxcoloring.IHighlightedPositionAcceptor;
import org.eclipse.xtext.ui.editor.syntaxcoloring.DefaultHighlightingConfiguration;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;

import io.sarl.lang.core.annotation.SarlAsynchronousExecution;
import io.sarl.lang.services.SARLGrammarKeywordAccess;
import io.sarl.lang.typesystem.InheritanceHelper;

/**
 * A base implementation of the semantic highlighting calculation.
 *
 * <p>Uses syntax highlighting from {@link XtendHighlightingCalculator} and
 * adds SARL specific keywords, e.g. {@code occurrence}.
 *
 * <p>This calculator also highlight with a specific color the calls to the capacity methods.
 *
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("restriction")
public class SARLHighlightingCalculator extends XtendHighlightingCalculator {

	@Inject
	private SARLGrammarKeywordAccess grammarKeywordAccess;

	@Inject
	private InheritanceHelper inheritanceHelper;

	@Override
	protected Map<String, String> initializeHighlightedIdentifiers() {
		final var result = super.initializeHighlightedIdentifiers();
		result.put(this.grammarKeywordAccess.getOccurrenceKeyword(), DefaultHighlightingConfiguration.KEYWORD_ID);
		return result;
	}

	@Override
	protected void computeFeatureCallHighlighting(XAbstractFeatureCall featureCall, IHighlightedPositionAcceptor acceptor) {
		super.computeFeatureCallHighlighting(featureCall, acceptor);

		var feature = featureCall.getFeature();
		if (feature != null && !feature.eIsProxy() && feature instanceof JvmOperation cvalue && !featureCall.isOperation()) {
			final var code = getSarlSpecificHighlighting(cvalue);
			if (code != null) {
				highlightFeatureCall(featureCall, acceptor, code);
			}
		}
	}

	/** Replies the highlighting that is specific to SARL.
	 *
	 * @param feature the feature to test.
	 * @return the highlighting, or {@code null} if none.
	 * @since 0.12
	 */
	protected String getSarlSpecificHighlighting(JvmOperation feature) {
		if (feature != null) {
			if (this.annotationLookup.findAnnotation(feature, SarlAsynchronousExecution.class) != null) {
				return SARLHighlightingStyles.ASYNCHRONOUS_METHOD_INVOCATION;
			}
			final var container = feature.getDeclaringType();
			if (container instanceof JvmGenericType cvalue
				&& this.inheritanceHelper.isSarlCapacity(cvalue)) {
				return SARLHighlightingStyles.CAPACITY_METHOD_INVOCATION;
			}
		}
		return null;
	}

}
