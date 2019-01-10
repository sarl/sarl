/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

package io.sarl.lang.tests.bugs.to00699;

import static org.junit.Assert.*;
import static org.junit.Assert.assertTrue;

import com.google.inject.Inject;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.util.TypeReferences;
import org.eclipse.xtext.testing.util.ParseHelper;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.typesystem.references.StandardTypeReferenceOwner;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;
import org.junit.Rule;
import org.junit.Test;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.Repeat;
import io.sarl.tests.api.RepeatRule;

/** Testing class for issue: Invalid synchro of the editor.
 *
 * <p>https://github.com/sarl/sarl/issues/661
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/661"
 */
@SuppressWarnings("all")
public class Bug661 extends AbstractSarlTest {

	private static final String SNIPSET1 = multilineString(
			"package io.sarl.lang.tests.bug661",
			"class X {",
			"   var f : int",
			"}");

	@Rule
	public RepeatRule repeatRule = new RepeatRule();

	@Inject
	private TypeReferences typeReferences;

	@Inject
	private CommonTypeComputationServices services;

	@Test
	public void isSubtypeOf_00() throws Exception {
		SarlScript mas0 = file(SNIPSET1);
		SarlScript mas1 = file(SNIPSET1);

		assertNotSame(mas0.eResource().getResourceSet(), mas1.eResource().getResourceSet());
		
		JvmTypeReference reference0 = this.typeReferences.getTypeForName("boolean", mas0);
		
		StandardTypeReferenceOwner owner = new StandardTypeReferenceOwner(this.services, mas1);
		LightweightTypeReference reference1 = owner.newParameterizedTypeReference(this.typeReferences.findDeclaredType("boolean", mas1));
		
		assertFalse(reference1.isSubtypeOf(reference0.getType()));
	}

	@Test
	@Repeat(5)
	public void isSubtypeOf_01() throws Exception {
		ParseHelper<SarlScript> helper = getParseHelper();
		
		SarlScript mas0 = helper.parse(SNIPSET1);
		ResourceSet resourceSet = mas0.eResource().getResourceSet();
		SarlScript mas1 = helper.parse(SNIPSET1, resourceSet);

		assertSame(mas0.eResource().getResourceSet(), mas1.eResource().getResourceSet());
		
		JvmTypeReference reference0 = this.typeReferences.getTypeForName("boolean", mas0);
		
		StandardTypeReferenceOwner owner = new StandardTypeReferenceOwner(this.services, mas1);
		LightweightTypeReference reference1 = owner.newParameterizedTypeReference(this.typeReferences.findDeclaredType("boolean", mas1));
		
		assertTrue(reference1.isSubtypeOf(reference0.getType()));
	}

	@Test
	@Repeat(5)
	public void isAssignableFrom_00() throws Exception {
		SarlScript mas0 = file(SNIPSET1);
		SarlScript mas1 = file(SNIPSET1);

		assertNotSame(mas0.eResource().getResourceSet(), mas1.eResource().getResourceSet());

		JvmTypeReference reference0 = this.typeReferences.getTypeForName("boolean", mas0);
		
		StandardTypeReferenceOwner owner = new StandardTypeReferenceOwner(this.services, mas1);
		LightweightTypeReference reference1 = owner.newParameterizedTypeReference(this.typeReferences.findDeclaredType("boolean", mas1));
		
		assertFalse(reference1.isAssignableFrom(reference0.getType()));
	}

	@Test
	@Repeat(5)
	public void isAssignableFrom_01() throws Exception {
		ParseHelper<SarlScript> helper = getParseHelper();
		
		SarlScript mas0 = helper.parse(SNIPSET1);
		ResourceSet resourceSet = mas0.eResource().getResourceSet();
		SarlScript mas1 = helper.parse(SNIPSET1, resourceSet);

		assertSame(mas0.eResource().getResourceSet(), mas1.eResource().getResourceSet());

		JvmTypeReference reference0 = this.typeReferences.getTypeForName("boolean", mas0);
		
		StandardTypeReferenceOwner owner = new StandardTypeReferenceOwner(this.services, mas1);
		LightweightTypeReference reference1 = owner.newParameterizedTypeReference(this.typeReferences.findDeclaredType("boolean", mas1));
		
		assertTrue(reference1.isAssignableFrom(reference0.getType()));
	}

}
