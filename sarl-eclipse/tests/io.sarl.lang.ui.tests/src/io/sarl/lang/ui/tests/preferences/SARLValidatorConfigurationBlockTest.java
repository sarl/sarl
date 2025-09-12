/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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
package io.sarl.lang.ui.tests.preferences;

import static io.sarl.tests.api.tools.TestAssertions.assertContainsCollection;
import static org.mockito.Mockito.mock;

import java.util.Set;
import java.util.TreeSet;

import com.google.inject.Inject;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.util.SarlUtils;
import io.sarl.lang.tests.api.AbstractSarlTest;
import io.sarl.lang.ui.preferences.SARLValidatorConfigurationBlock;
import io.sarl.lang.validation.StandardSarlConfigurableIssueCodesProvider;

/** This class tests the {@link SarlUtils} for SARL.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
@SuppressWarnings("all")
@DisplayName("SARLValidatorConfigurationBlock")
public class SARLValidatorConfigurationBlockTest extends AbstractSarlTest {

	@Inject
	private StandardSarlConfigurableIssueCodesProvider configuration;

	private final Set<String> expectedKeys = new TreeSet<>();

	private MockSARLValidatorConfigurationBlock block;
	
	@BeforeEach
	public void setUp() {
		this.expectedKeys.addAll(this.configuration.getConfigurableIssueCodes().keySet());
		this.block = new MockSARLValidatorConfigurationBlock();
	}
	
	private void assertConfigurableIssues() {
		assertContainsCollection(this.block.keys, this.expectedKeys);
	}

	private void removeImplicitlySupportedIssues() {
		this.block.keys.add("org.eclipse.xtext.builder.copyJavaProblems");
		this.block.keys.add("org.eclipse.xtext.xbase.validation.IssueCodes.unqualified_super_call");
	}
	
	@DisplayName("All configurable issues in UI")
	@Test
	public void configurableIssues() {
		this.block.fillActiveAnnotationSection();
		this.block.fillCodingStyleSection();
		this.block.fillDispatchSection();
		this.block.fillJavaDocSection();
		this.block.fillPotentialProgrammingProblemsSection();
		this.block.fillRestrictedApiSection();
		this.block.fillUnusedCodeSection();
		removeImplicitlySupportedIssues();
		assertConfigurableIssues();
	}

	/** Implementation of a subtype of {@code SARLValidatorConfigurationBlock}
	 * for aving a direct access to the tested functions.
	 *
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.15
	 */
	private static class MockSARLValidatorConfigurationBlock extends SARLValidatorConfigurationBlock {
		
		public final Set<String> keys = new TreeSet<>();

		private final ComboBoxBuilder builder;
		
		public MockSARLValidatorConfigurationBlock() {
			this.builder = createBuilder();
		}
		
		@Override
		protected Combo addJavaDelegatingComboBox(String prefKey, String label, Composite parent, int indent) {
			this.keys.add(prefKey);
			return mock(Combo.class);
		}
		
		@Override
		protected Combo addComboBox(Composite parent, String label, String key, int indent, String[] values,
				String[] valueLabels) {
			this.keys.add(key);
			return mock(Combo.class);
		}
		
		public void fillCodingStyleSection() {
			fillCodingStyleSection(this.builder);
		}

		public void fillPotentialProgrammingProblemsSection() {
			fillPotentialProgrammingProblemsSection(this.builder);
		}

		public void fillRestrictedApiSection() {
			fillRestrictedApiSection(this.builder);
		}
	
		public void fillUnusedCodeSection() {
			fillUnusedCodeSection(this.builder);
		}
		
		public void fillActiveAnnotationSection() {
			fillActiveAnnotationSection(this.builder);
		}

		public void fillDispatchSection() {
			fillDispatchSection(this.builder);
		}

		public void fillJavaDocSection() {
			fillJavaDocSection(this.builder);
		}

		public ComboBoxBuilder createBuilder() {
			return new ComboBoxBuilder(this, mock(Composite.class), 0);
		}

	}

}
