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

import io.sarl.lang.tests.api.AbstractSarlTest;

/** Abstract implementation of a test for the serializer.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version compiler.tests 0.14.0 20241106-161406
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler.tests
 */
public abstract class AbstractSerializerTest extends AbstractSarlTest {

	/** The serializer.
	 */
	@Inject
	protected ISerializer serializer;

	/** The Ecore object to serialize.
	 */
	protected EObject object;

	/** Assert the serialization generates the given text.
	 * 
	 * @param expected the expected result.
	 */
	protected void assertSerialize(String expected) {
		SaveOptions.Builder builder = SaveOptions.newBuilder();
		// No formatting
		//builder.format();
		String text = this.serializer.serialize(this.object, builder.getOptions());
		assertEquals(expected, text);
	}

}
