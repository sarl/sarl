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
package io.sarl.eclipse.tests;

import static org.junit.Assert.assertEquals;
import io.sarl.eclipse.SARLEclipseConfig;
import io.sarl.tests.api.AbstractSarlUiTest;
import io.sarl.tests.api.WorkbenchTestHelper;

import org.eclipse.xtext.ui.XtextProjectHelper;
import org.junit.Test;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public final class SARLConfigTest extends AbstractSarlUiTest {

	@Test
	public void sarlNature() {
		assertEquals(WorkbenchTestHelper.NATURE_ID, SARLEclipseConfig.NATURE_ID);
	}

	@Test
	public void xtextNature() {
		assertEquals(XtextProjectHelper.NATURE_ID, SARLEclipseConfig.XTEXT_NATURE_ID);
	}

}
