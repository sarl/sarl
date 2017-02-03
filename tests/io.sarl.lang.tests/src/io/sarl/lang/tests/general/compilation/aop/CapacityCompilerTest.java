/*
 * Copyright (C) 2014-2017 the original authors or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http:"",www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.lang.tests.general.compilation.aop;

import static org.junit.Assert.assertEquals;

import com.google.inject.Inject;
import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper;
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper.Result;
import org.junit.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class CapacityCompilerTest extends AbstractSarlTest {

	@Inject
	private CompilationTestHelper compiler;

	@Test
	public void basicCapacityCompile() throws Exception {
		String source = "capacity C1 { }";
		String expected = multilineString(
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.core.Capacity;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public interface C1 extends Capacity {",
				"}",
				""
				);
		this.compiler.assertCompilesTo(source, expected);
	}

	@Test
	public void capacitymodifier_none() throws Exception {
		this.compiler.assertCompilesTo(
			multilineString(
				"capacity C1 { }"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.core.Capacity;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public interface C1 extends Capacity {",
				"}",
				""
			));
	}

	@Test
	public void capacitymodifier_public() throws Exception {
		this.compiler.assertCompilesTo(
			multilineString(
				"public capacity C1 { }"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.core.Capacity;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public interface C1 extends Capacity {",
				"}",
				""
			));
	}

	@Test
	public void capacitymodifier_private() throws Exception {
		this.compiler.assertCompilesTo(
			multilineString(
				"private capacity C1 { }"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.core.Capacity;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"interface C1 extends Capacity {",
				"}",
				""
			));
	}

	@Test
	public void actionmodifier_override() throws Exception {
		String source = multilineString(
				"capacity C1 {",
				"	def name",
				"}",
				"capacity C2 extends C1 {",
				"	override name",
				"}"
			);
		final String expectedC1 = multilineString(
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.core.Capacity;",
				"",
				"@FunctionalInterface",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public interface C1 extends Capacity {",
				"  public abstract void name();",
				"}",
				""
			);
		final String expectedC2 = multilineString(
				"import io.sarl.lang.annotation.SarlSpecification;",
				"",
				"@FunctionalInterface",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public interface C2 extends C1 {",
				"  @Override",
				"  public abstract void name();",
				"}",
				""
			);
		this.compiler.compile(source, (r) -> {
				assertEquals(expectedC1, r.getGeneratedCode("C1"));
				assertEquals(expectedC2, r.getGeneratedCode("C2"));
			});
	}

	@Test
	public void actionmodifier_none() throws Exception {
		this.compiler.assertCompilesTo(
			multilineString(
				"capacity C1 {",
				"	def name {}",
				"}"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.core.Capacity;",
				"",
				"@FunctionalInterface",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public interface C1 extends Capacity {",
				"  public abstract void name();",
				"}",
				""
			));
	}

}