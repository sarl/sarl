/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.lang.tests.codebuilder.builders;

import com.google.inject.Inject;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.common.types.util.TypeReferences;
import org.eclipse.xtext.junit4.formatter.FormatterTestRequest;
import org.eclipse.xtext.junit4.formatter.FormatterTester;
import org.eclipse.xtext.junit4.util.ResourceHelper;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.serializer.ISerializer;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.Procedures;
import org.junit.ComparisonFailure;

import io.sarl.lang.codebuilder.CodeBuilderFactory;
import io.sarl.lang.formatting2.FormatterFacade;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public abstract class AbstractCodeBuilderTest extends AbstractSarlTest {

	@Inject
	protected ResourceHelper resourceHelper;

	@Inject
	protected CodeBuilderFactory factory;
	
	@Inject
	protected FormatterFacade facade;

	@Inject
	protected TypeReferences typeReferences;

	/** Assert formatting
	 *
	 * @param input the input.
	 * @param expected the expected input.
	 */
	protected void assertFormatted(EObject input, final String expected) {
		String actual = ((XtextResource) input.eResource()).getSerializer().serialize(input);
		if (!Strings.equal(actual, expected)) {
			throw new ComparisonFailure("invalid formatting", expected, actual);
		}
	}

}