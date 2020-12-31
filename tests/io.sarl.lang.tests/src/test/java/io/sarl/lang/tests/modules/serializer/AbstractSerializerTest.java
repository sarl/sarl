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
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.lang.tests.modules.serializer;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.google.inject.Inject;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.resource.SaveOptions;
import org.eclipse.xtext.serializer.ISerializer;

import io.sarl.tests.api.AbstractSarlTest;

/** Abstract implementation of a test for the serializer.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class AbstractSerializerTest extends AbstractSarlTest {

	@Inject
	protected ISerializer serializer;

	protected EObject object;

	protected void assertSerialize(String expected) {
		SaveOptions.Builder builder = SaveOptions.newBuilder();
		// No formatting
		//builder.format();
		String text = serializer.serialize(object, builder.getOptions());
		assertEquals(expected, text);
	}

}
