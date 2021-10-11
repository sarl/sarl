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

package io.sarl.lang.tests.bugs.to00699;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Invalid equals generation.
 *
 * <p>https://github.com/sarl/sarl/issues/646
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@DisplayName("Bug #646")
@SuppressWarnings("all")
@Tag("core")
public class Bug646Test extends AbstractSarlTest {

	private static final String SNIPSET1 = multilineString(
			"package io.sarl.lang.tests.bug646",
			"import java.util.Stack",
			"import java.util.List",
			"",
			"class ShapedObject { }",
			"",
			"class Shape2f<T> {",
			"  var inter : boolean",
			"  def intersects(s : Shape2f<?>) : boolean { inter }",
			"}",
			"",
			"class Tree<T extends ShapedObject> {",
			"  var root : TreeNode<T>",
			"  def getRoot : TreeNode<T> { root }",
			"}",
			"",
			"class TreeNode<T extends ShapedObject> {",
			"  var box : Shape2f<?>",
			"  var children : List<TreeNode<T>>",
			"  def getBox : Shape2f<?> { box }",
			"  def getChildren : Iterable<TreeNode<T>> { children }",
			"}",
			"",
			"class FrustumNodeIterator<T extends ShapedObject> {",
			"	var geometricFrustum : Shape2f<?>",
			"	var stack : Stack<TreeNode<T>>",
			"	",
			"	new (tree : Tree<T>, geomFrust : Shape2f<?>){",
			"		stack = new Stack",
			"		stack.push(tree.root)",
			"		this.geometricFrustum = geomFrust",
			"	}",
			"	",
			"	def hasNext(){",
			"		!stack.isEmpty",
			"	}",
			"	",
			"	def getNext(){",
			"		val node = stack.pop",
			"		if (node.box.intersects(geometricFrustum)) {",
			"			for(child : node.children){",
			"				stack.push(child)",
			"			}",
			"		}",
			"		node",	
			"	}",
			"}");

	private final String EXPECTED = multilineString(
			"package io.sarl.lang.tests.bug646;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import java.util.Stack;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class FrustumNodeIterator<T extends ShapedObject> {",
			"  private Shape2f<?> geometricFrustum;",
			"  ",
			"  private Stack<TreeNode<T>> stack;",
			"  ",
			"  public FrustumNodeIterator(final Tree<T> tree, final Shape2f<?> geomFrust) {",
			"    Stack<TreeNode<T>> _stack = new Stack<TreeNode<T>>();",
			"    this.stack = _stack;",
			"    this.stack.push(tree.getRoot());",
			"    this.geometricFrustum = geomFrust;",
			"  }",
			"  ",
			"  @Pure",
			"  public boolean hasNext() {",
			"    boolean _isEmpty = this.stack.isEmpty();",
			"    return (!_isEmpty);",
			"  }",
			"  ",
			"  @Pure",
			"  public TreeNode<T> getNext() {",
			"    TreeNode<T> _xblockexpression = null;",
			"    {",
			"      final TreeNode<T> node = this.stack.pop();",
			"      boolean _intersects = node.getBox().intersects(this.geometricFrustum);",
			"      if (_intersects) {",
			"        Iterable<TreeNode<T>> _children = node.getChildren();",
			"        for (final TreeNode<T> child : _children) {",
			"          this.stack.push(child);",
			"        }",
			"      }",
			"      _xblockexpression = node;",
			"    }",
			"    return _xblockexpression;",
			"  }",
			"  ",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public boolean equals(final Object obj) {",
			"    return super.equals(obj);",
			"  }",
			"  ",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public int hashCode() {",
			"    int result = super.hashCode();",
			"    return result;",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void parsing_01() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET1);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_01() throws Exception {
		getCompileHelper().compile(SNIPSET1, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug646.FrustumNodeIterator");
			assertEquals(EXPECTED, actual);
		});
	}

}
