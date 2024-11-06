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

package io.sarl.lang.formatting2;

import java.util.Collections;

import com.google.inject.Inject;
import com.google.inject.Provider;
import com.google.inject.name.Named;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtext.Constants;
import org.eclipse.xtext.formatting2.FormatterPreferences;
import org.eclipse.xtext.formatting2.FormatterRequest;
import org.eclipse.xtext.formatting2.IFormatter2;
import org.eclipse.xtext.formatting2.regionaccess.TextRegionAccessBuilder;
import org.eclipse.xtext.preferences.IPreferenceValuesProvider;
import org.eclipse.xtext.preferences.TypedPreferenceValues;
import org.eclipse.xtext.resource.IResourceFactory;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.resource.XtextResourceSet;
import org.eclipse.xtext.util.StringInputStream;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Pure;

/**
 * Provide a facade for the SARL formatter.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version compiler 0.14.0 20241106-161406
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler
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
	@Pure
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
	@Pure
	public String format(String sarlCode, ResourceSet resourceSet) {
		try {
			final var createURI = URI.createURI("synthetic://to-be-formatted." + this.fileExtension); //$NON-NLS-1$
			final var res = this.resourceFactory.createResource(createURI);
			if (res instanceof XtextResource resource) {
				final var resources = resourceSet.getResources();
				resources.add(resource);
				try (var stringInputStream = new StringInputStream(sarlCode)) {
					resource.load(stringInputStream, Collections.emptyMap());
					return formatResource(resource);
				} finally {
					resources.remove(resource);
				}
			}
			return sarlCode;
		} catch (Exception exception) {
			throw Exceptions.sneakyThrow(exception);
		}
	}

	/** Format the code in the given resource.
	 *
	 * @param resource the resource of the code to format.
	 */
	public void format(XtextResource resource) {
		assert resource != null;
		final var result = formatResource(resource);
		// Write back to the resource
		try (var stringInputStream = new StringInputStream(result)) {
			resource.load(stringInputStream, Collections.emptyMap());
		} catch (Exception exception) {
			throw Exceptions.sneakyThrow(exception);
		}
	}

	/** Format the code in the given resource.
	 *
	 * <p>This function does not change the resource content.
	 *
	 * @param resource the resource of the code to format.
	 * @return the result of the formatting.
	 */
	@Pure
	protected String formatResource(XtextResource resource) {
		assert resource != null;
		try {
			final var regionAccess = this.regionAccessBuilder.get().forNodeModel(resource).create();
			final var formatterRequest = new FormatterRequest();
			formatterRequest.setAllowIdentityEdits(false);
			formatterRequest.setTextRegionAccess(regionAccess);
			final var preferenceValues = FormatterFacade.this.configurationProvider
					.getPreferenceValues(resource);
			formatterRequest.setPreferences(TypedPreferenceValues.castOrWrap(preferenceValues));
			final var replacements = this.formatter.format(formatterRequest);
			return regionAccess.getRewriter().renderToString(replacements);
		} catch (Exception exception) {
			throw Exceptions.sneakyThrow(exception);
		}
	}

	/** Format the code in the given region.
	 *
	 * @param resource the resource to format.
	 * @param offset the offset of the text to format.
	 * @param length the length of the text.
	 */
	public void formatRegion(XtextResource resource, int offset, int length) {
		assert resource != null;
		assert offset >= 0;
		assert length >= 0;
		final var result = formatResourceRegion(resource, offset, length);
		// Write back to the resource
		try (var stringInputStream = new StringInputStream(result)) {
			resource.load(stringInputStream, Collections.emptyMap());
		} catch (Exception exception) {
			throw Exceptions.sneakyThrow(exception);
		}
	}

	/** Format the code in the given region.
	 *
	 * <p>This function does not change the resource content.
	 *
	 * @param resource the resource to format.
	 * @param offset the offset of the text to format.
	 * @param length the length of the text.
	 * @return the result of the formatting.
	 */
	@Pure
	public String formatResourceRegion(XtextResource resource, int offset, int length) {
		assert resource != null;
		assert offset >= 0;
		assert length >= 0;
		try {
			final var regionAccess = this.regionAccessBuilder.get().forNodeModel(resource).create();
			final var formatterRequest = new FormatterRequest();
			formatterRequest.setAllowIdentityEdits(false);
			formatterRequest.setRegions(Collections.singleton(regionAccess.regionForOffset(offset, length)));
			formatterRequest.setTextRegionAccess(regionAccess);
			final var replacements = this.formatter.format(formatterRequest);
			return regionAccess.getRewriter().renderToString(replacements);
		} catch (Exception exception) {
			throw Exceptions.sneakyThrow(exception);
		}
	}

}
