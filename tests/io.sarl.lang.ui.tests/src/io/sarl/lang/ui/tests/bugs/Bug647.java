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
package io.sarl.lang.ui.tests.bugs;

import java.awt.event.ActionEvent;

import org.eclipse.xtend.core.validation.IssueCodes;
import org.junit.Test;

import io.sarl.lang.ui.tests.quickfix.AbstractSARLQuickfixTest;

/** Issue: No generation of missed functions.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/647"
 */
@SuppressWarnings("all")
public class Bug647 extends AbstractSARLQuickfixTest {

	private static final String SNIPSET1 = multilineString(
			"package io.sarl.lang.ui.tests.bugs.bug647",
			"import java.util.Iterator",
			"class ShapedObject { }",
			"class FrustrumIterator<T extends ShapedObject> implements Iterator<T>{",
			"	",
			"}");

	private static final String FIXED1 = multilineString(
			"package io.sarl.lang.ui.tests.bugs.bug647",
			"import java.util.Iterator",
			"class ShapedObject { }",
			"class FrustrumIterator<T extends ShapedObject> implements Iterator<T>{",
			"	",
			"	override hasNext : boolean {",
			"		throw new UnsupportedOperationException(\"TODO: auto-generated method stub\")",
			"	}",
			"	",
			"	override next : T {",
			"		throw new UnsupportedOperationException(\"TODO: auto-generated method stub\")",
			"	}",
			"	",
			"}");

	private static final String SNIPSET2 = multilineString(
			"package io.sarl.lang.ui.tests.bugs.bug647",
			"import java.awt.^event.ActionListener",
			"class X implements ActionListener {",
			"	",
			"}");

	private static final String FIXED2 = multilineString(
			"package io.sarl.lang.ui.tests.bugs.bug647",
			"import java.awt.^event.ActionListener",
			"import java.awt.^event.ActionEvent",
			"",
			"class X implements ActionListener {",
			"	",
			"	override actionPerformed(e : ActionEvent) {",
			"		throw new UnsupportedOperationException(\"TODO: auto-generated method stub\")",
			"	}",
			"	",
			"}");

	@Test
	public void fixAddUnimplementedMethods1() {
		assertQuickFix(true,
				IssueCodes.CLASS_MUST_BE_ABSTRACT,
				//
				// Code to fix:
				//
				SNIPSET1,
				//
				// Label and description:
				//
				"Add unimplemented methods",
				//
				// Expected fixed code:
				//
				FIXED1);
	}

	@Test
	public void fixAddUnimplementedMethods2() throws Exception {
		assertQuickFix(true,
				IssueCodes.CLASS_MUST_BE_ABSTRACT,
				//
				// Code to fix:
				//
				SNIPSET2,
				//
				// Label and description:
				//
				"Add unimplemented methods",
				//
				// Expected fixed code:
				//
				FIXED2);
	}

}
