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

package io.sarl.lang.compiler;

import static com.google.common.collect.Sets.newHashSet;

import java.util.Set;

import com.google.inject.Singleton;
import org.eclipse.xtext.generator.IFileSystemAccess;
import org.eclipse.xtext.generator.OutputConfiguration;
import org.eclipse.xtext.generator.OutputConfigurationProvider;

import io.sarl.lang.SARLConfig;


/** Provide the output configuration from the SARL code to the Java code.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Singleton
public class SarlOutputConfigurationProvider extends OutputConfigurationProvider {

	@Override
	public Set<OutputConfiguration> getOutputConfigurations() {
		final OutputConfiguration defaultOutput = createStandardOutputConfiguration();
		final OutputConfiguration testOutput = createTestOutputConfiguration();
		if (defaultOutput != null) {
			if (testOutput != null) {
				return newHashSet(defaultOutput, testOutput);
			}
			return newHashSet(defaultOutput);
		}
		if (testOutput != null) {
			return newHashSet(testOutput);
		}
		return newHashSet();
	}

	/** Create the standard output configuration.
	 *
	 * @return the configuration, never {@code null}.
	 * @since 0.8
	 */
	@SuppressWarnings("static-method")
	protected OutputConfiguration createStandardOutputConfiguration() {
		final OutputConfiguration defaultOutput = new OutputConfiguration(IFileSystemAccess.DEFAULT_OUTPUT);
		defaultOutput.setDescription(Messages.SarlOutputConfigurationProvider_0);
		defaultOutput.setOutputDirectory(SARLConfig.FOLDER_SOURCE_GENERATED);
		defaultOutput.setOverrideExistingResources(true);
		defaultOutput.setCreateOutputDirectory(true);
		defaultOutput.setCanClearOutputDirectory(false);
		defaultOutput.setCleanUpDerivedResources(true);
		defaultOutput.setSetDerivedProperty(true);
		defaultOutput.setKeepLocalHistory(false);
		return defaultOutput;
	}

	/** Create the unit test output configuration.
	 *
	 * @return the configuration, never {@code null}.
	 * @since 0.8
	 */
	@SuppressWarnings("static-method")
	protected OutputConfiguration createTestOutputConfiguration() {
		final OutputConfiguration testOutput = new OutputConfiguration(SARLConfig.TEST_OUTPUT_CONFIGURATION);
		testOutput.setDescription(Messages.SarlOutputConfigurationProvider_1);
		testOutput.setOutputDirectory(SARLConfig.FOLDER_TEST_SOURCE_GENERATED);
		testOutput.setOverrideExistingResources(true);
		testOutput.setCreateOutputDirectory(true);
		testOutput.setCanClearOutputDirectory(false);
		testOutput.setCleanUpDerivedResources(true);
		testOutput.setSetDerivedProperty(true);
		testOutput.setKeepLocalHistory(false);
		return testOutput;
	}

}
