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
package io.sarl.lang.tests.modules.validation;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import com.google.inject.Inject;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.validation.IssueSeverities;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.validation.IConfigurableIssueSeveritiesProvider;
import io.sarl.lang.validation.IssueCodes;
import io.sarl.tests.api.AbstractSarlTest;

/** This class tests the not-ui implementation {@link IConfigurableIssueSeveritiesProvider}.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.5
 */
@SuppressWarnings("all")
@DisplayName("IConfigurableIssueSeveritiesProvider")
@Tag("core")
@Tag("unit")
public class IConfigurableIssueSeveritiesProviderTest extends AbstractSarlTest {

	private static final String SNIPSET = multilineString(
			"package io.sarl.lang.tests.modules.validation.snipset",
			"event MyEvent",
			"agent MyAgent {",
			"  on MyEvent [false] {",
			"  }",
			"}");

	@Inject
	private IConfigurableIssueSeveritiesProvider provider;

	@AfterEach
	public void tearDown() {
		this.provider.setAllSeverities(null);
	}

	@Test
	public void getIssueSeverities() throws Exception {
		SarlScript script = file(getParseHelper(), getValidationHelper(), SNIPSET);
		//
		IssueSeverities severities = this.provider.getIssueSeverities(script.eResource());
		//
		assertNotNull(severities);
		Severity severity = severities.getSeverity(IssueCodes.UNREACHABLE_BEHAVIOR_UNIT);
		assertNotNull(severity);
		assertSame(Severity.WARNING, severity);
	}

	@Test
	public void setSeverity() throws Exception {
		this.provider.setSeverity(IssueCodes.UNREACHABLE_BEHAVIOR_UNIT, Severity.INFO);
		//
		SarlScript script = file(getParseHelper(), getValidationHelper(), SNIPSET);
		//
		IssueSeverities severities = this.provider.getIssueSeverities(script.eResource());
		assertNotNull(severities);
		Severity severity = severities.getSeverity(IssueCodes.UNREACHABLE_BEHAVIOR_UNIT);
		assertNotNull(severity);
		assertSame(Severity.INFO, severity);
	}

	@Test
	public void setAllSeverities() throws Exception {
		this.provider.setAllSeverities(Severity.INFO);
		//
		SarlScript script = file(getParseHelper(), getValidationHelper(), SNIPSET);
		//
		IssueSeverities severities = this.provider.getIssueSeverities(script.eResource());
		assertNotNull(severities);
		Severity severity = severities.getSeverity(IssueCodes.UNREACHABLE_BEHAVIOR_UNIT);
		assertNotNull(severity);
		assertSame(Severity.INFO, severity);
	}

}
