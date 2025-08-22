/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

package io.sarl.bspl.lang.compiler;

import static com.google.common.collect.Sets.newHashSet;

import java.util.Set;

import org.eclipse.xtext.generator.IFileSystemAccess;
import org.eclipse.xtext.generator.OutputConfiguration;
import org.eclipse.xtext.generator.OutputConfigurationProvider;

import com.google.inject.Singleton;

import io.sarl.bspl.lang.BSPLConfig;


/** Provide the output configuration from the SARL code to the Java code.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
@Singleton
public class BSPLOutputConfigurationProvider extends OutputConfigurationProvider {

	@Override
	public Set<OutputConfiguration> getOutputConfigurations() {
		final var defaultOutput = createStandardOutputConfiguration();
		final var testOutput = createTestOutputConfiguration();
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
	 */
	@SuppressWarnings("static-method")
	protected OutputConfiguration createStandardOutputConfiguration() {
		final var defaultOutput = new OutputConfiguration(IFileSystemAccess.DEFAULT_OUTPUT);
		defaultOutput.setDescription(Messages.BsplOutputConfigurationProvider_0);
		defaultOutput.setOutputDirectory(BSPLConfig.FOLDER_SOURCE_GENERATED);
		defaultOutput.setOverrideExistingResources(true);
		defaultOutput.setCreateOutputDirectory(true);
		defaultOutput.setCanClearOutputDirectory(false);
		defaultOutput.setCleanUpDerivedResources(true);
		defaultOutput.setSetDerivedProperty(true);
		defaultOutput.setKeepLocalHistory(Boolean.FALSE);
		return defaultOutput;
	}

	/** Create the unit test output configuration.
	 *
	 * @return the configuration, never {@code null}.
	 */
	@SuppressWarnings("static-method")
	protected OutputConfiguration createTestOutputConfiguration() {
		final var testOutput = new OutputConfiguration(BSPLConfig.TEST_OUTPUT_CONFIGURATION);
		testOutput.setDescription(Messages.BsplOutputConfigurationProvider_1);
		testOutput.setOutputDirectory(BSPLConfig.FOLDER_TEST_SOURCE_GENERATED);
		testOutput.setOverrideExistingResources(true);
		testOutput.setCreateOutputDirectory(true);
		testOutput.setCanClearOutputDirectory(false);
		testOutput.setCleanUpDerivedResources(true);
		testOutput.setSetDerivedProperty(true);
		testOutput.setKeepLocalHistory(Boolean.FALSE);
		return testOutput;
	}

}
