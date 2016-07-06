/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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

package io.sarl.lang.ui.contentassist;

import org.eclipse.xtext.conversion.IValueConverter;
import org.eclipse.xtext.naming.IQualifiedNameConverter;
import org.eclipse.xtext.scoping.IScope;
import org.eclipse.xtext.ui.editor.contentassist.ConfigurableCompletionProposal.IReplacementTextApplier;
import org.eclipse.xtext.ui.editor.contentassist.ContentAssistContext;
import org.eclipse.xtext.xbase.ui.contentassist.ImportingTypesProposalProvider;

/** Provider of types to be imported.
 *
 * <p>This proposal does nothing special than the Xbase provider. But is is here allowing to filter types in SARL code.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLImportingTypesProposalProvider extends ImportingTypesProposalProvider {

	@Override
	protected IReplacementTextApplier createTextApplier(ContentAssistContext context, IScope typeScope,
			IQualifiedNameConverter qualifiedNameConverter, IValueConverter<String> valueConverter) {
		//final FilteringScope scope = new FilteringScope(typeScope,
		//		[name != XtendImportedNamespaceScopeProvider.OLD_DATA_ANNOTATION]);
		return super.createTextApplier(context, typeScope, qualifiedNameConverter, valueConverter);
	}

}
