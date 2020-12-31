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

import org.eclipse.xtext.diagnostics.Diagnostic;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.Test;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.ui.tests.quickfix.AbstractSARLQuickfixTest;
import io.sarl.tests.api.AbstractSarlUiTest;
import io.sarl.tests.api.AbstractSarlTest.Validator;

/** Issue: Product Invalid Method Generation.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/640"
 */
@SuppressWarnings("all")
public class Bug640 extends AbstractSARLQuickfixTest {

	private static final String SNIPSET1 = multilineString(
			"package io.sarl.lang.ui.tests.bugs.bug640",
			"import java.lang.reflect.Array",
			"class ShapedObject { }",
			"class TreeNode<T extends ShapedObject> {",
			"	var children = Array.newInstance(TreeNode, 4) as TreeNode<T>[]",
			"	new (parent : TreeNode<T> = null, box : ShapedObject<?>) {",
			"	}",
			"	def addObj(obj : T) {",
			"		for (i : 0..4) {",
			"			var child = new TreeNode(this, getNewShape(i))",
			"			children.set(i, child)",
			"		}",
			"	}",
			"}");

	private static final String FIXED1 = multilineString(
			"package io.sarl.lang.ui.tests.bugs.bug640",
			"import java.lang.reflect.Array",
			"class ShapedObject { }",
			"class TreeNode<T extends ShapedObject> {",
			"	var children = Array.newInstance(TreeNode, 4) as TreeNode<T>[]",
			"	new (parent : TreeNode<T> = null, box : ShapedObject<?>) {",
			"	}",
			"	def addObj(obj : T) {",
			"		for (i : 0..4) {",
			"			var child = new TreeNode(this, getNewShape(i))",
			"			children.set(i, child)",
			"		}",
			"	}",
			"	",
			"	def getNewShape(integer : Integer) : ShapedObject<?> {",
			"		throw new UnsupportedOperationException(\"TODO: auto-generated method stub\")",
			"	}",
			"	",
			"}");

	@Test
	public void compilationFailure() throws Exception {
		final Validator validator = validate(file(SNIPSET1));
		validator.assertError(
				XbasePackage.eINSTANCE.getXFeatureCall(),
				Diagnostic.LINKING_DIAGNOSTIC,
				"The method getNewShape(Integer) is undefined");
	}

	@Test
	public void fix() {
		assertQuickFix(true,
				Diagnostic.LINKING_DIAGNOSTIC,
				//
				// Code to fix:
				//
				SNIPSET1,
				//
				// Label and description:
				//
				"Create method 'getNewShape(Integer)'",
				//
				// Expected fixed code:
				//
				FIXED1);
	}

}
