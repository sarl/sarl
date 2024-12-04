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

package io.sarl.lang.tests.modules.codebuilder;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import com.google.inject.Inject;
import com.google.inject.Injector;

import io.sarl.lang.codebuilder.CodeBuilderFactory;
import io.sarl.lang.tests.api.AbstractSarlTest;

/** Test the base function of {@code CodeBuilderFactory}.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
@DisplayName("Base of CodeBuilderFactory")
@SuppressWarnings("all")
@Tag("core")
@Tag("unit")
public class CodeBuilderFactoryTest extends AbstractSarlTest {

	@Inject
	private CodeBuilderFactory factory;
	
	@Inject
	private Injector injector;

	@Test
	@DisplayName("createOverridingInjector")
	public void createOverridingInjector() {
		final var module = mock(com.google.inject.Module.class);
		final var newInjector = this.factory.createOverridingInjector(this.injector, module);
		assertNotNull(newInjector);
	}

	@Test
	@DisplayName("getScriptFileExtension")
	public void getScriptFileExtension() {
		// By default, the SARL file extension is injected. See parameter of setFileExtensions().
		assertEquals("sarl", this.factory.getScriptFileExtension());
	}

	@Test
	@DisplayName("setFileExtensions(null)")
	public void setFileExtensions_null() {
		assertThrows(NullPointerException.class, () -> {
			this.factory.setFileExtensions(null);
		});
		// By default, the SARL file extension is injected. See parameter of setFileExtensions().
		assertEquals("sarl", this.factory.getScriptFileExtension());
	}

	@Test
	@DisplayName("setFileExtensions(\"\")")
	public void setFileExtensions_empty() {
		this.factory.setFileExtensions("");
		assertEquals("", this.factory.getScriptFileExtension());
	}

	@Test
	@DisplayName("setFileExtensions(\"a\")")
	public void setFileExtensions_1() {
		this.factory.setFileExtensions("a");
		assertEquals("a", this.factory.getScriptFileExtension());
	}

	@Test
	@DisplayName("setFileExtensions(\"a:b\")")
	public void setFileExtensions_2_unix() {
		this.factory.setFileExtensions("a:b");
		assertEquals("a", this.factory.getScriptFileExtension());
	}

	@Test
	@DisplayName("setFileExtensions(\"a;b\")")
	public void setFileExtensions_2_windows() {
		this.factory.setFileExtensions("a;b");
		assertEquals("a", this.factory.getScriptFileExtension());
	}

	@Test
	@DisplayName("setFileExtensions(\"a,b\")")
	public void setFileExtensions_2_conf() {
		this.factory.setFileExtensions("a,b");
		assertEquals("a", this.factory.getScriptFileExtension());
	}

	@Test
	@DisplayName("setFileExtensions(\"a:b:c\")")
	public void setFileExtensions_3_unix() {
		this.factory.setFileExtensions("a:b:c");
		assertEquals("a", this.factory.getScriptFileExtension());
	}

	@Test
	@DisplayName("setFileExtensions(\"a;b;c\")")
	public void setFileExtensions_3_windows() {
		this.factory.setFileExtensions("a;b;c");
		assertEquals("a", this.factory.getScriptFileExtension());
	}

	@Test
	@DisplayName("setFileExtensions(\"a,b,c\")")
	public void setFileExtensions_3_conf() {
		this.factory.setFileExtensions("a,b,c");
		assertEquals("a", this.factory.getScriptFileExtension());
	}

	@Test
	@DisplayName("setFileExtensions(\"a:b;c\")")
	public void setFileExtensions_3_mix_0() {
		this.factory.setFileExtensions("a:b;c");
		assertEquals("a", this.factory.getScriptFileExtension());
	}

	@Test
	@DisplayName("setFileExtensions(\"a:b,c\")")
	public void setFileExtensions_3_mix_1() {
		this.factory.setFileExtensions("a:b,c");
		assertEquals("a", this.factory.getScriptFileExtension());
	}

	@Test
	@DisplayName("setFileExtensions(\"a;b:c\")")
	public void setFileExtensions_3_mix_2() {
		this.factory.setFileExtensions("a;b:c");
		assertEquals("a", this.factory.getScriptFileExtension());
	}

	@Test
	@DisplayName("setFileExtensions(\"a;b,c\")")
	public void setFileExtensions_3_mix_3() {
		this.factory.setFileExtensions("a;b,c");
		assertEquals("a", this.factory.getScriptFileExtension());
	}

	@Test
	@DisplayName("setFileExtensions(\"a,b:c\")")
	public void setFileExtensions_3_mix_4() {
		this.factory.setFileExtensions("a,b:c");
		assertEquals("a", this.factory.getScriptFileExtension());
	}

	@Test
	@DisplayName("setFileExtensions(\"a,b;c\")")
	public void setFileExtensions_3_mix_5() {
		this.factory.setFileExtensions("a,b;c");
		assertEquals("a", this.factory.getScriptFileExtension());
	}

}
