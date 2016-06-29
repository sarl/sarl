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

package io.sarl.lang.formatting2;

import java.util.Collections;
import java.util.List;

import javax.inject.Named;

import com.google.inject.Inject;
import com.google.inject.Provider;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
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
		return format(sarlCode, new XtextResourceSet());
	}

	/** Format the given code.
	 *
	 * @param sarlCode the code to format.
	 * @param resourceSet the resource set that sohuld contains the code. This resource set may be
	 *     used for resolving types by the underlying code.
	 * @return the code to format.
	 */
	public String format(String sarlCode, ResourceSet resourceSet) {
		try {
			final URI createURI = URI.createURI("synthetic://to-be-formatted." + this.fileExtension); //$NON-NLS-1$
			final Resource res = this.resourceFactory.createResource(createURI);
			if (res instanceof XtextResource) {
				final XtextResource resource = (XtextResource) res;
				final EList<Resource> resources = resourceSet.getResources();
				resources.add(resource);
				try (StringInputStream stringInputStream = new StringInputStream(sarlCode)) {
					resource.load(stringInputStream, Collections.emptyMap());
					final ITextRegionAccess regionAccess = this.regionAccessBuilder.get().forNodeModel(resource).create();
					final FormatterRequest formatterRequest = new FormatterRequest();
					formatterRequest.setAllowIdentityEdits(false);
					formatterRequest.setTextRegionAccess(regionAccess);
					final IPreferenceValues preferenceValues = FormatterFacade.this.configurationProvider
							.getPreferenceValues(resource);
					formatterRequest.setPreferences(TypedPreferenceValues.castOrWrap(preferenceValues));
					final List<ITextReplacement> replacements = this.formatter.format(formatterRequest);
					return regionAccess.getRewriter().renderToString(replacements);
				} finally {
					resources.remove(resource);
				}
			}
			return sarlCode;
		} catch (Throwable exception) {
			throw Exceptions.sneakyThrow(exception);
		}
	}
}
