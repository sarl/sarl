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
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.tests.api;

import javax.inject.Inject;
import javax.inject.Provider;

import com.google.inject.Injector;
import org.eclipse.xtext.testing.InjectWith;
import org.eclipse.xtext.testing.extensions.InjectionExtension;
import org.eclipse.xtext.testing.util.ParseHelper;
import org.eclipse.xtext.testing.validation.ValidationTestHelper;
import org.eclipse.xtext.xbase.lib.util.ReflectExtensions;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.extension.ExtensionContext;

import io.sarl.lang.jvmmodel.SarlJvmModelAssociations;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.extensions.ContextInitExtension;
import io.sarl.tests.api.extensions.IgnorableTestExtension;
import io.sarl.tests.api.extensions.JavaVersionCheckExtension;
import io.sarl.tests.api.extensions.MockInitializerExtension;
import io.sarl.tests.api.extensions.FieldResetExtension;
import io.sarl.tests.api.extensions.SarlInjectionExtension;
import io.sarl.tests.api.helper.SarlCompilationTestHelper;
import io.sarl.tests.api.tools.SarlValidationTestHelper;

/** Abstract class that is providing useful tools for unit tests.
 *
 * This class provides assertion functions, clear any property
 * related to Sarl, and reset the attributes of the unit test that
 * are marked <code>@Mock</code>, <code>@InjectMocks</code> or
 * <code>@Nullable</code>.
 *
 * @param <S> - the type of the service.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@ExtendWith({
	ContextInitExtension.class, JavaVersionCheckExtension.class, 
	SarlInjectionExtension.class, MockInitializerExtension.class, 
	IgnorableTestExtension.class, FieldResetExtension.class})
@InjectWith(ExtendedSARLInjectorProvider.class)
public abstract class AbstractSarlTest {

	@Inject
	private Injector injector;

	@Inject
	private SarlValidationTestHelper validationHelper;

	@Inject
	private ParseHelper<SarlScript> parser;

	@Inject
	private SarlCompilationTestHelper compiler;

	@Inject
	private Provider<SarlJvmModelAssociations> associations;

	@Override
	protected void finalize() throws Throwable {
		this.injector = null;
		this.validationHelper = null;
		this.parser = null;
		this.associations = null;
	}

	/** Replies the injector.
	 *
	 * @return the injector.
	 * @since 0.10
	 */
	protected Injector getInjector() {
		return this.injector;
	}

	/** Replies the parse helper.
	 *
	 * @return the parse helper.
	 * @since 0.7
	 */
	protected ParseHelper<SarlScript> getParseHelper() {
		return this.parser;
	}

	/** Replies the validation helper.
	 *
	 * @return the validation helper.
	 * @since 0.11
	 */
	protected SarlValidationTestHelper getValidationHelper() {
		return this.validationHelper;
	}

	/** Replies the compile helper.
	 *
	 * @return the compile helper.
	 * @since 0.9
	 */
	protected SarlCompilationTestHelper getCompileHelper() {
		return this.compiler;
	}

	/** Replies if the test could be ignored.
	 *
	 * @param context is the execution context.
	 * @return {@code true} if the test should be ignored.
	 */
	public boolean isIgnorable(ExtensionContext context) {
		return false;
	}

}
