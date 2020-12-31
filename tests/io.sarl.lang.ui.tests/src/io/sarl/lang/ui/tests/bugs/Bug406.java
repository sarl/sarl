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

package io.sarl.lang.ui.tests.bugs;

import org.eclipse.xtext.junit4.ui.ContentAssistProcessorTestBuilder;
import org.junit.Ignore;
import org.junit.Test;

import io.sarl.lang.ui.tests.contentassist.AbstractContentAssistTest;

/** Test for issue #406: Auto-completion does not work
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/406"
 */
@SuppressWarnings("all")
public class Bug406 extends AbstractContentAssistTest {
	
	@Test
	public void afterStaticTypeDotCharacterProposals() throws Exception {
		assertTextInsideProposals(newBuilder()
			.appendNl("class FooA1 {")
			.appendNl("    static def myfct ; void {}")
			.appendNl("}")
			.appendNl("class FooA2 {")
			.appendNl("    def myfct2 : void {")
			.append  ("        FooA1.")
			.appendSuffix("    }\n}"),
			"myfct");
	}

	@Test
	public void afterStaticTypeColumnCharactersProposals() throws Exception {
		assertTextInsideProposals(newBuilder()
				.appendNl("class FooA1 {")
				.appendNl("    static def myfct ; void {}")
				.appendNl("}")
				.appendNl("class FooA2 {")
				.appendNl("    def myfct2 : void {")
				.append  ("        FooA1::")
				.appendSuffix("    }\n}"),
				"myfct");
	}

	/**
	 * see https://github.com/eclipse/xtext-eclipse/issues/28
	 */
	@Test
	public void dirtyStateTypeModifiers_01() throws Exception {
		final ContentAssistProcessorTestBuilder builder = newBuilder().withDirtyState()
			.appendNl("final class FooA1 {}")
			.appendNl("class FooA2 {}")
			.append("class FooA3 extends FooA")
			.assertText("FooA2");
		assertNoText(builder, "FooA1");
	}
	
	/**
	 * see https://github.com/eclipse/xtext-eclipse/issues/28
	 */
	@Test
	public void dirtyStateTypeModifiers_02() throws Exception {
		newBuilder().withDirtyState()
			.appendNl("class FooA1 {}")
			.appendNl("class FooA2 {}")
			.append("class FooA3 extends FooA")
			.assertText("FooA1", "FooA2");
	}
	
}
