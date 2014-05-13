/*
 * Copyright 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.docs.utils;

import io.sarl.lang.sarl.Model;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtext.junit4.util.ParseHelper;
import org.eclipse.xtext.junit4.validation.ValidationTestHelper;
import org.eclipse.xtext.resource.ClassloaderClasspathUriResolver;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.resource.XtextResourceSet;

import com.google.inject.Inject;

/**
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLParser {
	@Inject
	private ParseHelper<Model> parser;
	@Inject
	private XtextResourceSet xtextResourceSet;
	
	@Inject private ValidationTestHelper validationTestHelper;

	/**
	 * @param text
	 * @return the model.
	 * @throws Exception
	 */
	public Model parse(CharSequence text) throws Exception {
		return this.parser.parse(text, getResourceSetWithDeafaultModels());
	}
	
	/**
	 * 
	 * @param text
	 * @return the model.
	 * @throws Exception
	 */
	public Model parsesSuccessfully(CharSequence text) throws Exception{
		Model model = parse(text);
		this.validationTestHelper.assertNoErrors(model);
		return model;
	}

	private ResourceSet getResourceSetWithDeafaultModels() {

		this.xtextResourceSet.setClasspathURIContext(getClass());
		this.xtextResourceSet.setClasspathUriResolver(new ClassloaderClasspathUriResolver());
		this.xtextResourceSet.addLoadOption(XtextResource.OPTION_RESOLVE_ALL, Boolean.TRUE);
		this.xtextResourceSet.createResource(URI.createURI("classpath:/io/sarl/core/events.sarl")); //$NON-NLS-1$
		this.xtextResourceSet.createResource(URI.createURI("classpath:/io/sarl/core/bic.sarl")); //$NON-NLS-1$
		return this.xtextResourceSet;
	}
}
