/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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

package io.sarl.lang.formatting2;

import java.util.Collections;
import java.util.List;

import javax.inject.Named;

import com.google.inject.Inject;
import com.google.inject.Provider;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.Constants;
import org.eclipse.xtext.formatting2.FormatterPreferences;
import org.eclipse.xtext.formatting2.FormatterRequest;
import org.eclipse.xtext.formatting2.IFormatter2;
import org.eclipse.xtext.formatting2.regionaccess.ITextRegionAccess;
import org.eclipse.xtext.formatting2.regionaccess.ITextReplacement;
import org.eclipse.xtext.formatting2.regionaccess.TextRegionAccessBuilder;
import org.eclipse.xtext.preferences.IPreferenceValues;
import org.eclipse.xtext.preferences.IPreferenceValuesProvider;
import org.eclipse.xtext.preferences.TypedPreferenceValues;
import org.eclipse.xtext.resource.IResourceFactory;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.resource.XtextResourceSet;
import org.eclipse.xtext.util.StringInputStream;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.ObjectExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;

/**
 * Provide a facade for the SARL formatter.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class FormatterFacade {

	@Inject
	private IResourceFactory resourceFactory;

	@Inject
	@FormatterPreferences
	private IPreferenceValuesProvider configurationProvider;

	@Inject
	private IFormatter2 formatter;

	@Inject
	private Provider<TextRegionAccessBuilder> regionAccessBuilder;
	
	@Named(Constants.FILE_EXTENSIONS)
	private String fileExtension;

	/** Format the given code.
	 *
	 * @param sarlCode the code to format.
	 * @return the code to format.
	 */
	public String format(String sarlCode) {
		try {
			XtextResourceSet resourceSet = new XtextResourceSet();
			URI createURI = URI.createURI("synthetic://to-be-formatted." + this.fileExtension); //$NON-NLS-1$
			XtextResource resource = (XtextResource) this.resourceFactory.createResource(createURI);
			EList<Resource> resources = resourceSet.getResources();
			resources.add(resource);
			try (StringInputStream stringInputStream = new StringInputStream(sarlCode)) {
				resource.load(stringInputStream, Collections.emptyMap());
				ITextRegionAccess regionAccess = this.regionAccessBuilder.get().forNodeModel(resource).create();
				FormatterRequest formatterRequest = new FormatterRequest();
				final Procedure1<FormatterRequest> function = new Procedure1<FormatterRequest>() {
					@SuppressWarnings("synthetic-access")
					@Override
					public void apply(final FormatterRequest it) {
						it.setAllowIdentityEdits(false);
						it.setTextRegionAccess(regionAccess);
						IPreferenceValues preferenceValues = FormatterFacade.this.configurationProvider.getPreferenceValues(resource);
						it.setPreferences(TypedPreferenceValues.castOrWrap(preferenceValues));
					}
				};
				FormatterRequest request = ObjectExtensions.operator_doubleArrow(formatterRequest, function);
				List<ITextReplacement> replacements = this.formatter.format(request);
				return regionAccess.getRewriter().renderToString(replacements);
			}
		} catch (Throwable _e) {
			throw Exceptions.sneakyThrow(_e);
		}
	}
}