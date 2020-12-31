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
package io.sarl.tests.api.extralang;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.util.Map;

import javax.inject.Inject;

import org.eclipse.xtext.generator.IFileSystemAccess;
import org.eclipse.xtext.generator.IOutputConfigurationProvider;
import org.eclipse.xtext.generator.OutputConfiguration;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper.Result;
import org.opentest4j.AssertionFailedError;

import io.sarl.tests.api.AbstractSarlTest;


/** Abstract implementation for testing extra language generators.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
@SuppressWarnings("all")
public abstract class AbstractExtraLanguageGeneratorTest extends AbstractSarlTest {

	@Inject
	private IOutputConfigurationProvider outputConfigurationProvider;

	protected abstract String getOutputConfigurationName();
	
	protected OutputConfiguration getOutputConfiguration() {
		Iterable<? extends OutputConfiguration> configurations = this.outputConfigurationProvider.getOutputConfigurations();
		OutputConfiguration defaultConfiguration = null;
		for (final OutputConfiguration configuration : configurations) {
			if (Strings.equal(IFileSystemAccess.DEFAULT_OUTPUT, configuration.getName())) {
				defaultConfiguration = configuration;
			} else if (Strings.equal(getOutputConfigurationName(), configuration.getName())) {
				return configuration;
			}
		}
		return defaultConfiguration;
	}
	
	protected GeneratorTest compile(String source) throws IOException {
		final Result[] result = new Result[1];
		getCompileHelper().compile(source, (it) -> {
			result[0] = it;
		});
		return new GeneratorTest(result[0]);
	}
	
	/** Generator test API.
	 * 
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public class GeneratorTest {

		private final Result result;

		GeneratorTest(Result result) {
			this.result = result;
		}

		public void assertTypeDefinition(String typeName, String expectedContent) {
			final OutputConfiguration configuration = getOutputConfiguration();
			if (configuration == null) {
				throw new AssertionFailedError("", expectedContent, "");
			}
			final String filename = typeName.replaceAll("\\.", "/") + ".py";
			final String key = "/" + CompilationTestHelper.PROJECT_NAME + "/" + configuration.getOutputDirectory() + "/" + filename;
			final Map<String, CharSequence> generatedResources = this.result.getAllGeneratedResources();
			CharSequence ocontent = generatedResources.get(key);
			final String content;
			if (ocontent == null) {
				content = ""; 
			} else {
				content = ocontent.toString();
			}
			assertEquals(expectedContent, ocontent);
		}

	}

}
